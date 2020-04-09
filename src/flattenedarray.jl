struct FlattenedArray{V,L,M,P,InAx} <: AbstractArray{V,L}
    parent::P
    inneraxes::InAx
    @inline function FlattenedArray{V,L,M,P,InAx}(parent, inneraxes) where {V,L,M,P,InAx}
        new{V,L,M,P,InAx}(parent, inneraxes)
    end
end

@inline function FlattenedArray(
    parent::AbsSimilarNestedArr{V,M,N},
    inneraxes::NTuple{M,Any},
) where {V,M,N}
    FlattenedArray{V,M + N,M,typeof(parent),typeof(inneraxes)}(parent, inneraxes)
end

@inline function FlattenedArray(parent::AbsArr{<:Any,N}, inneraxes::NTuple{M,Any}) where {N,M}
    V = innereltype(parent)
    FlattenedArray{V,M + N,M,typeof(parent),typeof(inneraxes)}(parent, inneraxes)
end

@inline FlattenedArray(parent::AbsArr) = FlattenedArray(parent, inneraxes(parent))


####
#### Core Array Interface
####

@inline Base.axes(F::FlattenedArray) = (F.inneraxes..., axes(F.parent)...)

@inline Base.size(F::FlattenedArray) = map(Base.unsafe_length, axes(F))


# standard Cartesian indexing
@propagate_inbounds function Base.getindex(F::FlattenedArray{<:Any,L}, I::Vararg{Int,L}) where {L}
    F.parent[_outer_indices(F, I)...][_inner_indices(F, I)...]
end

@propagate_inbounds function Base.setindex!(
    F::FlattenedArray{<:Any,L},
    v,
    I::Vararg{Int,L},
) where {L}
    F.parent[_outer_indices(F, I)...][_inner_indices(F, I)...] = v
    return F
end


# In general, we can forward the indices corresponding to one of the inner arrays if
# length(index_dimsum(outI)) == 0 (so as to avoid materializing an intermediate array).
@inline function Base.getindex(F::FlattenedArray{<:Any,L}, I::Vararg{Idx,L}) where {L}
    J = Base.to_indices(F, I)
    @boundscheck checkbounds(F, J...)
    outI = _outer_indices(F, J)
    return _unsafe_getindex(F, J, outI, Base.index_dimsum(outI...))
end

# length(index_dimsum(outI)) != 0, reindex with cartesian indices
@inline function _unsafe_getindex(F::FlattenedArray, I::Tuple, outI::Tuple, ::Tuple)
    return @inbounds CartesianIndexer(F)[I...]
end
# length(index_dimsum(outI)) == 0: forward the inner indices
@inline function _unsafe_getindex(F::FlattenedArray, I::Tuple, outI::Tuple, ::Tuple{})
    inI = _inner_indices(F, I)
    return @inbounds F.parent[outI...][inI...]
end


# setindex! follows the same pattern as above
@inline function Base.setindex!(F::FlattenedArray{<:Any,L}, v, I::Vararg{Idx,L}) where {L}
    J = Base.to_indices(F, I)
    @boundscheck checkbounds(F, J...)
    outI = _outer_indices(F, J)
    return _unsafe_setindex!(F, J, v, outI, Base.index_dimsum(outI...))
end

@inline function _unsafe_setindex!(F::FlattenedArray, I::Tuple, v, outI::Tuple, ::Tuple)
    @inbounds CartesianIndexer(F)[I...] = v
    return F
end
@inline function _unsafe_setindex!(F::FlattenedArray, I::Tuple, v, outI::Tuple, ::Tuple{})
    inI = _inner_indices(F, I)
    @inbounds F.parent[outI...][inI...] = v
    return F
end


@inline function _inner_indices(::FlattenedArray{<:Any,L,M}, I::NTuple{L,Any}) where {L,M}
    front(I, Val(M))
end
@inline function _outer_indices(::FlattenedArray{<:Any,L,M}, I::NTuple{L,Any}) where {L,M}
    tail(I, Val(L - M))
end


####
#### Misc
####

Base.copy(F::FlattenedArray) = FlattenedArray(deepcopy(F.parent), F.inneraxes)

Base.parent(F::FlattenedArray) = F.parent

# Base.mightalias relies on dataids returning a Tuple, which could be insanely large
# depending on F.parent.
Base.mightalias(A::AbsArr, F::FlattenedArray) = _mightalias(Base.dataids(A), _dataids(F))
Base.mightalias(F::FlattenedArray, A::AbsArr) = _mightalias(_dataids(F), Base.dataids(A))
function Base.mightalias(F1::FlattenedArray, F2::FlattenedArray)
    _mightalias(_dataids(F1), _dataids(F2))
end

function _mightalias(Aids, Bids)
    for Aid in Aids
        Aid in Bids && return true
    end
    return false
end

function Base.unalias(dest::AbsArr, F::FlattenedArray)
    if Base.mightalias(dest, F.parent)
        return copy(F)
    else
        for I in eachindex(F.parent)
            F.parent[I] = Base.unalias(dest, F.parent[I])
        end
        return F
    end
end

Base.dataids(F::FlattenedArray) = Tuple(_dataids(F))

function _dataids(F::FlattenedArray)
    ids = UInt[Base.dataids(F.parent)...]
    for A in F.parent, id in Base.dataids(A)
        push!(ids, id)
    end
    ids
end


function Base.showarg(io::IO, A::FlattenedArray, toplevel)
    print(io, "flatview(")
    Base.showarg(io, parent(A), false)
    print(io, ')')
    toplevel && print(io, " with eltype ", eltype(A))
    return nothing
end


####
#### Extra
####

@inline inneraxes(F::FlattenedArray) = F.inneraxes

"""
    flatview(A::AbstractArray{<:AbstractArray{V,M},N})

Return a `M+N`-dimensional flattened view of `A`. Throws an error if the elements of `A` do not
have equal size. If `A` is not a nested array, the return value is `A` itself.

```jldoctest
julia> A = [reshape(Vector(1:6), (2, 3)), reshape(Vector(7:12), (2, 3))]
2-element Array{Array{Int64,2},1}:
 [1 3 5; 2 4 6]
 [7 9 11; 8 10 12]

julia> B = flatview(A)
2×3×2 flatview(::Array{Array{Int64,2},1}) with eltype Int64:
[:, :, 1] =
 1  3  5
 2  4  6

[:, :, 2] =
 7   9  11
 8  10  12

julia> B == reshape(hcat(B...), (2, 3, 2))
true
```
"""
flatview(A::AbsArr) = _flatview(A, inneraxes(A), eltype(A))
_flatview(A::AbsArr, inax::Tuple{}, ::Type{<:AbsArr{<:Any,0}}) = FlattenedArray(A, inax)
_flatview(A::AbsArr, inax::Tuple, ::Type) = FlattenedArray(A, inax)
_flatview(A::AbsArr, ::Tuple{}, ::Type) = A
