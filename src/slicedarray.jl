struct SlicedArray{T<:AbstractArray,N,M,P<:AbstractArray,A<:TupleN{TypedBool},C} <: AbstractArray{T,N}
    parent::P
    alongs::A
    @inline function SlicedArray{T,N,M,P,A,C}(
        parent::P,
        alongs::A,
    ) where {T<:AbstractArray,N,M,P<:AbstractArray,A<:TupleN{TypedBool},C}
        # TODO check type parameters
        new(parent, alongs)
    end
end

@inline function SlicedArray(
    parent::P,
    alongs::A,
    paxes::Anys{L},
    outaxes::Anys{N},
    inaxes::Anys{M},
) where {L,P<:AbstractArray{<:Any,L},A<:NTuple{L,TypedBool},N,M}
    I = tuple_setindex(sliceaxes(parent), tuple_map(first, outaxes), invert(alongs))
    T = viewtype(parent, I)
    SlicedArray{T,N,M,P,A,iscontiguous(alongs)}(parent, alongs)
end

@inline function SlicedArray(parent::AbstractArray{<:Any,L}, alongs::NTuple{L,TypedBool}) where {L}
    paxes = sliceaxes(parent)
    inaxes = tuple_getindex(paxes, alongs)
    outaxes = tuple_getindex(paxes, tuple_map(!, alongs))
    SlicedArray(parent, alongs, paxes, outaxes, inaxes)
end


####
#### Core Array Interface
####

@inline Base.axes(S::SlicedArray) = tuple_getindex(axes(S.parent), invert(S.alongs))

@inline Base.size(S::SlicedArray) = tuple_getindex(size(S.parent), invert(S.alongs))


@propagate_inbounds function Base.getindex(S::SlicedArray{<:Any,N}, I::Vararg{Int,N}) where {N}
    view(S.parent, mergeindices(S, I)...)
end

@propagate_inbounds function Base.setindex!(S::SlicedArray{<:Any,N}, v, I::Vararg{Int,N}) where {N}
    S.parent[mergeindices(S, I)...] = v
    return S
end

# eltype(S) <: AbstractArray{T,0}, but eltype(S.parent) is just T, so we need to unwrap
# zero-dimensional elements.
@propagate_inbounds function Base.setindex!(
    S::SlicedArray{<:Any,N,0},
    v::AbstractArray{<:Any,0},
    I::Vararg{Int,N},
) where {N}
    S.parent[mergeindices(S, I)...] = v[]
    return S
end

Base.IndexStyle(::Type{<:SlicedArray}) = IndexCartesian()
Base.IndexStyle(::Type{<:SlicedArray{<:Any,1}}) = IndexLinear()

@inline function mergeindices(S::SlicedArray{<:Any,N}, I::Anys{N}) where {N}
    tuple_setindex(sliceaxes(S.parent), I, invert(S.alongs))
end


function Base.resize!(S::SlicedArray{<:Any,N}, dims::NTuple{N,Integer}) where {N}
    resize!(S.parent, tuple_setindex(size(S.parent), dims, invert(S.alongs)))
    return S
end
Base.resize!(S::SlicedArray{<:Any,N}, dims::Vararg{Integer,N}) where {N} = resize!(S, dims)

function Base.sizehint!(S::SlicedArray{<:Any,N}, dims::NTuple{N,Integer}) where {N}
    sizehint!(S.parent, tuple_setindex(size(S.parent), dims, invert(S.alongs)))
    return S
end
Base.sizehint!(S::SlicedArray{<:Any,N}, dims::Vararg{Integer,N}) where {N} = sizehint!(S, dims)


@inline function setindex_shape_check(dest::SlicedArray, szsrc::Dims)
    setindex_shape_check(inner_size(dest), szsrc)
end


####
#### ContiguousSlicedArray
####

# If only the leading dimensions of S.parent are sliced (i.e. S.alongs consists of any number of
# True's followed by any number of False's) then we can optimize some functions, and if N==1
# then functions like push! are well-defined, assuming the underlying parent supports those
# operations (e.g. ElasticArray).

const ContiguousSlicedArray{T,N,M,P,A} = SlicedArray{T,N,M,P,A,true}
const ContiguousSlicedVector{T,M,P,A} = ContiguousSlicedArray{T,1,M,P,A}


function Base.copyto!(
    dest::ContiguousSlicedArray{<:Any,N,M},
    doffs::Integer,
    src::ContiguousSlicedArray{<:Any,N,M},
    soffs::Integer,
    n::Integer,
) where {N,M}
    insize = inner_size(src)
    setindex_shape_check(dest, insize)
    stride = prod(insize)
    di1 = firstindex(dest.parent)
    si1 = firstindex(src.parent)
    doffs_parent = stride * (doffs - di1) + di1
    soffs_parent = stride * (soffs - si1) + si1
    copyto!(dest.parent, doffs_parent, src.parent, soffs_parent, stride * n)
    return dest
end

function Base.copyto!(
    dest::ContiguousSlicedArray{<:Any,N,M},
    src::ContiguousSlicedArray{<:Any,N,M},
) where {N,M}
    setindex_shape_check(dest, inner_size(src))
    copyto!(dest.parent, src.parent)
    return dest
end


function Base.append!(S::ContiguousSlicedVector, iter::ContiguousSlicedVector)
    setindex_shape_check(S, inner_size(iter))
    append!(S.parent, iter.parent)
    return S
end

# TODO can probably speed this up by doing resize + setindex like in Base
function Base.append!(S::ContiguousSlicedVector, iter)
    for el in iter
        push!(S, el)
    end
    return S
end


function Base.prepend!(S::ContiguousSlicedVector, iter::ContiguousSlicedVector)
    setindex_shape_check(S, inner_size(iter))
    prepend!(S.parent, iter.parent)
    return S
end

# TODO can probably speed this up by doing resize + setindex like in Base
function Base.prepend!(S::ContiguousSlicedVector, iter)
    for el in iter
        pushfirst!(S, el)
    end
    return S
end


function Base.pop!(S::ContiguousSlicedVector)
    isempty(S) && argerror("array must be non-empty")
    # We need to copy the last element as the view could be invalidated by resizing S.parent,
    # but Base.copy(::SubArray{T,0}) unpacks the 0-dimensional view and an element of type T,
    # so we wrap it with a 0-dimensional array
    item = _maybe_wrap(eltype(S.parent), copy(last(S)))
    resize!(S, length(S) - 1)
    return item
end

# TODO need something like resizebeg! for ElasticArray
#function Base.popfirst!(S::ContiguousSlicedVector)
#    isempty(S) && argerror("array must be non-empty"))
#    item = _maybe_wrap(eltype(S.parent), copy(first(S)))
#    resizebeg!(S.parent, length(S) - 1)
#    return item
#end

@inline _maybe_wrap(::Type{T}, x::AbstractArray{T}) where {T} = x
@inline _maybe_wrap(::Type{T}, x::T) where {T} = fill(x)


function Base.push!(S::ContiguousSlicedVector{<:Any,M}, item::AbstractArray{<:Any,M}) where {M}
    setindex_shape_check(S, size(item))
    append!(S.parent, item)
    return S
end

function Base.pushfirst!(S::ContiguousSlicedVector{<:Any,M}, item::AbstractArray{<:Any,M}) where {M}
    setindex_shape_check(S, size(item))
    prepend!(S.parent, item)
    return S
end


Base.empty!(S::ContiguousSlicedVector) = resize!(S, 0)

flatten(S::ContiguousSlicedArray) = S.parent


#####
##### Misc
#####

Base.parent(S::SlicedArray) = S.parent

Base.dataids(S::SlicedArray) = Base.dataids(S.parent)

Base.copy(S::SlicedArray) = SlicedArray(copy(S.parent), S.alongs)

function Base.showarg(io::IO, S::SlicedArray, toplevel)
    print(io, "slice(")
    Base.showarg(io, S.parent, false) # TODO try ture
    if length(S.alongs) > 0
        print(io, ", ", join(map(a -> a === True() ? ':' : '*', S.alongs), ", "))
    end
    print(io, ')')
    if toplevel
        print(io, " with ", Base.dims2string(inner_size(S)), " elements of ", inner_eltype(S))
    end
    return nothing
end


function mapslices(f, A::AbstractArray; dims)
    Asliced = slice(A, dims...)
    S = _alloc_mapslices(Asliced, f(_unsafe_getindex_unwrapped(Asliced, firstindex(Asliced))))
    for I in eachindex(Asliced, S) # TODO start from second
        _unsafe_setindex!(S, f(_unsafe_getindex_unwrapped(Asliced, I)), I)
    end
    return S.parent
end

@inline function _alloc_mapslices(S::SlicedArray{<:Any,N,M}, b1) where {N,M}
    insize = tuple_map(unsafe_length, _reshape_axes(axes(b1), Val(M)))
    psize = tuple_setindex(size(S.parent), insize, S.alongs)
    SlicedArray(Array{eltype(b1),M + N}(undef, psize...), S.alongs)
end
@inline _reshape_axes(axes::Tuple, ::Val{N}) where {N} = Base.rdims(Val(N), axes)
@inline _reshape_axes(axes::NTuple{N,Any}, ::Val{N}) where {N} = axes

@inline _unsafe_getindex_unwrapped(S::SlicedArray{<:Any,<:Any,0}, I) = @inbounds S[I][]
@inline _unsafe_getindex_unwrapped(S::SlicedArray, I) = @inbounds S[I]

@inline _unsafe_setindex!(S::SlicedArray, v, i::Int) = @inbounds S[i] .= v
@inline _unsafe_setindex!(S::SlicedArray, v, I::CartesianIndex) = @inbounds S[Tuple(I)...] .= v
@inline _unsafe_setindex!(S::SlicedArray, v::AbstractArray, I::CartesianIndex) = @inbounds S[I] = v
@inline _unsafe_setindex!(S::SlicedArray, v::AbstractArray, i::Int) = @inbounds S[i] = v


#####
##### Extra
#####

@inline inner_axes(S::SlicedArray) = tuple_getindex(axes(S.parent), S.alongs)

@inline inner_size(S::SlicedArray) = tuple_getindex(size(S.parent), S.alongs)

"""
    slice(A::AbstractArray, alongs...)
    slice(A::AbstractArray, alongs)

Return an array whose elements are views into `A` along the dimensions `alongs`.
`alongs` can be specified in the following ways:
- `:` or `*` representing sliced or indexed dimensions, respectively.
- Integers corresponding to the sliced dimensions.

See also: [`align`](@ref), [`flatten`](@ref).

# Examples

```jldoctest
julia> A = reshape(collect(1:8), (2,2,2))
2×2×2 Array{Int64,3}:
[:, :, 1] =
 1  3
 2  4

[:, :, 2] =
 5  7
 6  8

julia> S = slice(A, :, *, :)
2-element slice(::Array{Int64,3}, :, *, :) with 2×2 elements of Int64:
 [1 5; 2 6]
 [3 7; 4 8]

julia> S[1] == view(A, :, 1, :)
true

julia> slice(A, :, *, :) == slice(A, 1, 3)
true
```
"""
@inline function slice(A::AbstractArray{<:Any,L}, alongs...) where {L}
    _slice(A, to_alongs(Val(L), alongs...))
end
@inline function slice(A::AbstractArray{<:Any,L}, alongs::Tuple) where {L}
    _slice(A, to_alongs(Val(L), alongs))
end

@inline function _slice(A::AbstractArray{<:Any,L}, alongs::NTuple{L,TypedBool}) where {L}
    SlicedArray(A, alongs)
end
# When taking a slice of a slice we can avoid the extra indirection by merging alongs with S.alongs
@inline function _slice(S::SlicedArray{<:Any,N}, alongs::NTuple{N,TypedBool}) where {N}
    palongs = tuple_setindex(S.alongs, alongs, tuple_map(!, S.alongs))
    SlicedArray(S.parent, palongs)
end


"""
    align(A::AbstractArray{<:AbstractArray{V,M},N}, alongs...)
    align(A::AbstractArray{<:AbstractArray{V,M},N}, alongs)

The inverse of `slice`. Returns an `M+N`-dimension array where `alongs` specifies the dimensions
taken up by the inner arrays. If `A` is not a subtype of `AbstractArray{<:AbstractArray{V,M},N}`,
the return value is `A` itself.

See also: [`flatten`](@ref), [`slice`](@ref).

# Examples

```jldoctest
julia> A = [[1 2; 3 4], [5 6; 7 8]]
2-element Array{Array{Int64,2},1}:
 [1 2; 3 4]
 [5 6; 7 8]

julia> align(A, :, :, *)
2×2×2 PermutedDimsArray(flatten(::Array{Array{Int64,2},1}), (1, 2, 3)) with eltype Int64:
[:, :, 1] =
 1  2
 3  4

[:, :, 2] =
 5  6
 7  8

julia> align(A, :, *, :)
2×2×2 PermutedDimsArray(flatten(::Array{Array{Int64,2},1}), (1, 3, 2)) with eltype Int64:
[:, :, 1] =
 1  5
 3  7

[:, :, 2] =
 2  6
 4  8

julia> align(A, *, :, :) == align(A, 2, 3)
true

julia> align(slice(A, 2, 3), 2, 3) == A
true
```
"""
@inline function align(A::NestedArray{<:Any,M,N}, alongs...) where {M,N}
    _align(A, to_alongs(Val(M+N), alongs...))
end
@inline function align(A::NestedArray{<:Any,M,N}, alongs::Tuple) where {M,N}
    _align(A, to_alongs(Val(M+N), alongs))
end

# align(A, alongs) is the inverse of slice(A, alongs) i.e. align(slice(A, alongs), alongs) === A
function _align(S::SlicedArray{<:Any,<:Any,<:Any,<:Any,A}, alongs::A) where {A<:TupleN{TypedBool}}
    S.parent
end
@inline function _align(A::NestedArray{<:Any,M,N}, alongs::NTuple{L,TypedBool}) where {M,N,L}
    (M+N == L && M == length(tuple_getindex(alongs, alongs))) || throw_invalid_alongs(M, N, alongs)
    return iscontiguous(alongs) ? flatten(A) : PermutedDimsArray(flatten(A), _permdims(alongs))
end

@pure function _permdims(alongs::NTuple{L,TypedBool}) where {L}
    dims = ntuple(identity, Val(L))
    return (tuple_getindex(dims, alongs)..., tuple_getindex(dims, invert(alongs))...)
end


#####
##### 3rd Party
#####

@inline function UnsafeArrays.unsafe_uview(S::SlicedArray)
    SlicedArray(UnsafeArrays.unsafe_uview(S.parent), S.alongs)
end

function Adapt.adapt_structure(T, S::SlicedArray)
    SlicedArray(adapt(T, S.parent), S.alongs)
end
