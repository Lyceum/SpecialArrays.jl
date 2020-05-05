struct FlattenedArray{V,L,M,N,P<:AbstractArray,InAx<:NTuple{M,Any}} <: AbstractArray{V,L}
    parent::P
    inneraxes::InAx
    @inline function FlattenedArray{V,L,M,N,P,InAx}(parent, inneraxes) where {V,L,M,N,P<:AbstractArray,InAx<:NTuple{M,Any}}
        new{V,L,M,N,P,InAx}(parent, inneraxes)
    end
end

function FlattenedArray(parent::AbstractArray, inneraxes::NTuple{M,Any}) where {M}
    V = innereltype(parent)
    N = ndims(parent)
    FlattenedArray{V,M+N,M,N,typeof(parent),typeof(inneraxes)}(parent, inneraxes)
end

FlattenedArray(parent::AbstractArray) = FlattenedArray(parent, inneraxes(parent))


####
#### Core Array Interface
####

@inline Base.axes(F::FlattenedArray) = (F.inneraxes..., axes(F.parent)...)

@inline Base.size(F::FlattenedArray) = map(Base.unsafe_length, axes(F))


# standard Cartesian indexing
@propagate_inbounds function Base.getindex(F::FlattenedArray{<:Any,L}, I::Vararg{Int,L}) where {L}
    Iin, Iout = splitindices(F, I)
    F.parent[Iout...][Iin...]
end

@propagate_inbounds function Base.setindex!(
    F::FlattenedArray{<:Any,L},
    v,
    I::Vararg{Int,L},
) where {L}
    Iin, Iout = splitindices(F, I)
    F.parent[Iout...][Iin...] = v
    return F
end

@inline function splitindices(F::FlattenedArray{<:Any,L,M,N}, I::NTuple{L,Int}) where {L,M,N}
    tuple_split(I, Val(M))
end


####
#### Misc
####

Base.copy(F::FlattenedArray) = FlattenedArray(deepcopy(F.parent), F.inneraxes)

Base.parent(F::FlattenedArray) = F.parent

function Base.showarg(io::IO, A::FlattenedArray, toplevel)
    print(io, "flatview(")
    Base.showarg(io, parent(A), false)
    print(io, ')')
    toplevel && print(io, " with eltype ", eltype(A))
    return nothing
end


# Because F::FlattenedArray is a flattened view of an AbstractArray{<:AbstractArray},
# Base.dataids should return the equivalent of union(Base.dataids(x), Base.dataids.(x)...).
# This works, but the default recursive implementation is slow when length(parent(F)) is large.
function Base.mightalias(A::AbstractArray, F::FlattenedArray)
    !isbits(A) && !isbits(F) && !isdisjoint(Base.dataids(A), _dataids(F))
end
function Base.mightalias(F::FlattenedArray, A::AbstractArray)
    !isbits(F) && !isbits(A) && !isdisjoint(_dataids(F), Base.dataids(A))
end
function Base.mightalias(F1::FlattenedArray, F2::FlattenedArray)
    !isbits(F1) && !isbits(F2) && !isdisjoint(_dataids(F1), _dataids(F2))
end

Base.dataids(F::FlattenedArray) = Tuple(_dataids(F))

function _dataids(F::FlattenedArray)
    ids = [Base.dataids(F.parent)...]
    for A in F.parent, id in Base.dataids(A)
        push!(ids, id)
    end
    ids
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
flatview(A::AbstractArrayOfArrays) = FlattenedArray(A)
flatview(A::AbstractArray) = A





"""
    $(TYPEDSIGNATURES)

Like [`flatview`](@ref) but returns a new array.
"""
flatten(A::AbstractArray) = copy(flatview(A))
