struct FlattenedArray{V,L,P<:AbsArr,A<:NTuple{L,TypedBool},Ax<:NTuple{L,Any}} <: AbstractArray{V,L}
    parent::P
    alongs::A
    axes::Ax
    @inline function FlattenedArray{V,L,P,A,Ax}(parent, alongs, axes) where {V,L,P,A,Ax}
        new{V,L,P,A,Ax}(parent, alongs, axes)
    end
end

function FlattenedArray(parent::AbsArr{<:AbsArr{V,M},N}, alongs::TupleN{TypedBool}, axes::Tuple) where {V,M,N}
    FlattenedArray{V,M+N,typeof(parent),typeof(alongs), typeof(axes)}(parent, alongs, axes)
end

function FlattenedArray(parent::AbsArr, alongs::TupleN{TypedBool})
    FlattenedArray(parent, alongs, _permute((inneraxes(parent)..., axes(parent)...), alongs))
end

FlattenedArray(parent::AbsArr, alongs::TypedBool...) = FlattenedArray(parent, alongs)


####
#### Core Array Interface
####

function _permute(t::NTuple{L,Any}, alongs::NTuple{L,TypedBool}) where {L}
    (t[alongs]..., t[tuple_map(!, alongs)]...)
end

@inline Base.axes(F::FlattenedArray) = F.axes

@inline Base.size(F::FlattenedArray) = map(Base.unsafe_length, axes(F))


# standard Cartesian indexing
@propagate_inbounds function Base.getindex(F::FlattenedArray{<:Any,L}, I::Vararg{Int,L}) where {L}
    Iout, Iin = _splitindices(F, I)
    @inbounds F.parent[Iout...][Iin...]
end

@propagate_inbounds function Base.setindex!(
    F::FlattenedArray{<:Any,L},
    v,
    I::Vararg{Int,L},
) where {L}
    Iout, Iin = _splitindices(F, I)
    @inbounds F.parent[Iout...][Iin...] = v
    return F
end

#@inline function _splitindices(F::FlattenedArray{<:Any,L}, I::NTuple{L,Int}) where {L}
#    I[F.alongs], I[tuple_map(!, F.alongs)]
#end
@inline function _splitindices(F::FlattenedArray{<:Any,L,<:AbsArr{<:AbsArr{<:Any,M},N}}, I::NTuple{L,Int}) where {L,M,N}
    #I[F.alongs], I[tuple_map(!, F.alongs)]
    tail(I, Val(N)), front(I, Val(M))
end


####
#### Misc
####

Base.copy(F::FlattenedArray) = FlattenedArray(deepcopy(F.parent), F.inneraxes)

Base.parent(F::FlattenedArray) = F.parent

## Base.mightalias relies on dataids returning a Tuple, which could be insanely large
## depending on F.parent.
#Base.mightalias(A::AbsArr, F::FlattenedArray) = _mightalias(Base.dataids(A), _dataids(F))
#Base.mightalias(F::FlattenedArray, A::AbsArr) = _mightalias(_dataids(F), Base.dataids(A))
#function Base.mightalias(F1::FlattenedArray, F2::FlattenedArray)
#    _mightalias(_dataids(F1), _dataids(F2))
#end

#function _mightalias(Aids, Bids)
#    for Aid in Aids
#        Aid in Bids && return true
#    end
#    return false
#end

#function Base.unalias(dest::AbsArr, F::FlattenedArray)
#    if Base.mightalias(dest, F.parent)
#        return copy(F)
#    else
#        for I in eachindex(F.parent)
#            F.parent[I] = Base.unalias(dest, F.parent[I])
#        end
#        return F
#    end
#end

#Base.dataids(F::FlattenedArray) = Tuple(_dataids(F))

#function _dataids(F::FlattenedArray)
#    ids = UInt[Base.dataids(F.parent)...]
#    for A in F.parent, id in Base.dataids(A)
#        push!(ids, id)
#    end
#    ids
#end


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

@inline inneraxes(F::FlattenedArray) = F.axes[F.alongs]

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
function flatview(A::AbsArr{<:AbsArr{<:Any,M},N}) where {M,N}
    alongs = (ntuple(_ -> True(), Val(M))..., ntuple(_ -> False(), Val(N))...)
    FlattenedArray(A, alongs)
end

"""
    $(TYPEDSIGNATURES)

Like [`flatview`](@ref) but returns a new array.
"""
flatten(A::AbsArr) = copy(flatview(A))
