struct SlicedArray{T,N,M,P,A} <: AbstractArray{T,N}
    parent::P
    alongs::A
    @inline function SlicedArray{T,N,M,P,A}(parent, alongs) where {T,N,M,P,A}
        check_slices_parameters(T::Type, Val(N::Int), Val(M::Int), P::Type, A::Type)
        new(parent, alongs)
    end
end

@inline function SlicedArray(
    parent::AbsArr{<:Any,L},
    alongs::NTuple{L,TypedBool},
    inaxes::NTuple{M,Any},
    outaxes::NTuple{N,Any},
) where {L,M,N}
    I = ntuple(i -> (@_inline_meta; first(outaxes[i])), Val(N))
    J = parentindices(axes(parent), alongs, I)
    T = viewtype(parent, J)
    SlicedArray{T,N,M,typeof(parent),typeof(alongs)}(parent, alongs)
end

@inline function SlicedArray(parent::AbsArr{<:Any,L}, alongs::NTuple{L,TypedBool}) where {L}
    paxes = axes(parent)
    inaxes = paxes[alongs]
    outaxes = paxes[tuple_map(!, alongs)]
    SlicedArray(parent, alongs, inaxes, outaxes)
end

@inline function SlicedArray(S::SlicedArray{<:Any,L}, alongs::NTuple{L,TypedBool}) where {L}
    SlicedArray(S.parent, Base.setindex(S.alongs, alongs, tuple_map(!, S.alongs)))
end

@generated function check_slices_parameters(
    ::Type{T},
    ::Val{N},
    ::Val{M},
    ::Type{P},
    ::Type{A},
) where {T,N,M,P,A}
    A <: NTuple{M + N,TypedBool} || return :(throw(ArgumentError("SlicedArray parameter A should <: NTuple{M+N,$TypedBool}")))
    ntrue = 0
    for AA in A.parameters
        AA === True && (ntrue += 1)
    end
    if !(P <: AbsArr{eltype(T),M+N} && N >= 0 && M >= 0 && M == ntrue)
        return :(throw(ArgumentError("SlicedArray parameter P should be <: AbstractArray{eltype(T),M+N}")))
    end
    return nothing
end


"""
    slice(A::AbstractArray, I::Union{Colon,typeof(*)}...)

Similar to `slice(A, alongs)` except that the sliced and indexed dimensions are specified
using `:` and the glob-like `*` syntax, respectively. If `length(I) != ndims(A)`, then
`A` will be reshaped accordingly.

# Examples

```jldoctest
julia> A = reshape(Vector(1:8), (2, 2, 2))
2×2×2 Array{Int64,3}:
[:, :, 1] =
 1  3
 2  4

[:, :, 2] =
 5  7
 6  8

julia> slice(A, 1, 3) === slice(A, :, *, :)
true

julia> B = slice(A, :, *)
4-element slice(::Array{Int64,2}, :, *) with 2-element eltype SubArray{Int64,1,Array{Int64,2},Tuple{Base.Slice{Base.OneTo{Int64}},Int64},true}:
 [1, 2]
 [3, 4]
 [5, 6]
 [7, 8]

julia> size(B)
(4,)

julia> innersize(B)
(2,)
```
"""
@inline function slice(A::AbsArr, I::Vararg{Union{Colon,typeof(*)},L}) where {L}
    alongs = ntuple(i -> (Base.@_inline_meta; I[i] === Colon() ? True() : False()), Val(L))
    SlicedArray(reshape(A, Val(L)), alongs)
end

"""
    slice(A::AbstractArray, alongs::Tuple)
    slice(A::AbstractArray, alongs...)

Return an array whose elements are views into `A` along the dimensions `alongs`.

# Examples

```jldoctest
julia> A = reshape(Vector(1:8), (2, 2, 2))
2×2×2 Array{Int64,3}:
[:, :, 1] =
 1  3
 2  4

[:, :, 2] =
 5  7
 6  8

julia> B = slice(A, 1, 3)
2-element slice(::Array{Int64,3}, :, *, :) with 2×2 eltype SubArray{Int64,2,Array{Int64,3},Tuple{Base.Slice{Base.OneTo{Int64}},Int64,Base.Slice{Base.OneTo{Int64}}},false}:
 [1 5; 2 6]
 [3 7; 4 8]

julia> view(A, :, 1, :) === B[1]
true
```

`alongs` can also be specified using `StaticInteger` from StaticNumbers.jl for
better type inference:

```jldoctest
julia> using StaticNumbers

julia> A = reshape(Vector(1:8), (2, 2, 2));

julia> slice(A, 1, 3) === slice(A, static(1), static(3))
true
```
"""
@inline function slice(A::AbsArr{<:Any,L}, alongs::TupleN{StaticOrInt}) where {L}
    SlicedArray(A, ntuple(dim -> (@_inline_meta; static_in(dim, alongs)), Val(L)))
end
@inline slice(A::AbsArr, alongs::Vararg{StaticOrInt}) = slice(A, alongs)

@inline function slice(A::AbsArr{<:Any,L}, ::Tuple{}) where {L}
    SlicedArray(A, ntuple(_ -> (@_inline_meta; False()), Val(L)))
end
@inline slice(A::AbsArr) = slice(A, ())


####
#### Core Array Interface
####

@inline Base.axes(S::SlicedArray) = axes(S.parent)[tuple_map(!, S.alongs)]

@inline Base.size(S::SlicedArray) = size(S.parent)[tuple_map(!, S.alongs)]


# standard Cartesian indexing
@propagate_inbounds function Base.getindex(S::SlicedArray{<:Any,N}, I::Vararg{Int,N}) where {N}
    view(S.parent, parentindices(S, I)...)
end

@propagate_inbounds function Base.setindex!(S::SlicedArray{<:Any,N}, v, I::Vararg{Int,N}) where {N}
    S.parent[parentindices(S, I)...] = v
    return S
end
@propagate_inbounds function Base.setindex!(
    S::SlicedArray{<:Any,N,0},
    v::AbsArr{<:Any,0},
    I::Vararg{Int,N},
) where {N}
    S.parent[parentindices(S, I)...] = v[]
    return S
end

# If `I isa Vararg{Idx,N} && length(Base.index_ndims(I...)) == N` We can just forward the
# indices to the parent array and drop the corresponding entries in `S.alongs`
# (that is, we can drop `S.alongs[i]` iff
# `S.alongs[i] === False() && and Base.index_shape(I[i]) === ())`) rather than allocating a new
# output array.

@inline function Base.getindex(S::SlicedArray{<:Any,N}, I::Vararg{Idx,N}) where {N}
    J = Base.to_indices(S, I)
    @boundscheck checkbounds(S, J...)
    _unsafe_getindex(S, J, Base.index_ndims(J...))
end

@inline function _unsafe_getindex(
    S::SlicedArray{<:Any,N},
    J::NTuple{N,Idx},
    ::NTuple{N,Bool},
) where {N}
    K = parentindices(S, J)
    return @inbounds _maybe_wrap(view(S.parent, K...), reslice(S.alongs, K))
end

# Fall back to Cartesian indexing.
@inline function _unsafe_getindex(S::SlicedArray, J::Tuple, ::Tuple)
    @inbounds CartesianIndexer(A)[J...]
end

@inline _maybe_wrap(A::AbstractArray, alongs::TupleN{TypedBool}) = SlicedArray(A, alongs)
# A single element, so no need to wrap with a SlicedArray
@inline _maybe_wrap(A::AbstractArray{<:Any,M}, ::NTuple{M,True}) where {M} = A

# add/drop non-sliced dimensions (i.e. alongs[dim] == False()) to match J
@inline function reslice(alongs::NTuple{L,TypedBool}, K::NTuple{L,Any}) where {L}
    (_reslice1(first(alongs), first(K))..., reslice(tail(alongs), tail(K))...)
end
reslice(::Tuple{}, ::Tuple{}) = ()
@inline _reslice1(::True, k) = (True(), ) # keep inner dimension
@inline _reslice1(::False, k) = _reslicefalse(k)
@inline _reslicefalse(::Real) = () # drop this dimension
@inline _reslicefalse(::Colon) = (False(), ) # keep this dimension
@inline function _reslicefalse(::AbstractArray{<:Any,N}) where {N}
    ntuple(_ -> False(), Val(N))
end


@inline function parentindices(S::SlicedArray{<:Any,N,M}, I::NTuple{N,Any}) where {N,M}
    parentindices(axes(S.parent), S.alongs, I)
end

@inline function parentindices(paxes::NTuple{N,Any}, alongs::NTuple{N,TypedBool}, I::Tuple) where {N}
    Base.setindex(tuple_map(ax->(@_inline_meta; Base.Slice(ax)), paxes), I, tuple_map(!, alongs))
end


#####
##### Misc
#####

Base.:(==)(A::SlicedArray, B::SlicedArray) = A.alongs == B.alongs && A.parent == B.parent

Base.parent(S::SlicedArray) = S.parent

Base.dataids(S::SlicedArray) = Base.dataids(S.parent)

Base.copy(S::SlicedArray) = SlicedArray(copy(S.parent), S.alongs)


Base.append!(S::SlicedArray, iter) = (append!(S.parent, iter); S)

Base.prepend!(S::SlicedArray, iter) = (prepend!(S.parent, iter); S)

function Base.resize!(S::SlicedArray{<:Any,N}, dims::NTuple{N,Integer}) where {N}
    indims = innersize(S)
    parentdims = static_merge(S.alongs, indims, dims)
    resize!(S.parent, parentdims)
    return S
end

Base.resize!(S::SlicedArray, dims::Integer...) = resize!(S, dims)


function Base.showarg(io::IO, A::SlicedArray, toplevel)
    print(io, "slice(")
    Base.showarg(io, parent(A), false)
    if length(A.alongs) > 0
        print(io, ", ", join(map(along2string, A.alongs), ", "))
    end
    print(io, ')')
    if toplevel
        print(io, " with ", Base.dims2string(innersize(A)), " eltype ", eltype(A))
    end
    return nothing
end
along2string(::True) = ':'
along2string(::False) = '*'


#####
##### Extra
#####

@inline innersize(S::SlicedArray) = size(S.parent)[S.alongs]
@inline inneraxes(S::SlicedArray) = axes(S.parent)[S.alongs]

flatview(S::SlicedArray) = S.parent


function mapslices(f, A::AbstractArray; dims::TupleN{StaticOrInt})
    S = slice(A, dims)
    B = _alloc_mapslices(S, f(first(S)))
    @assert axes(S) == axes(B)
    for I in eachindex(S, B) # TODO start from second
        _unsafe_copy_inner!(B, f(S[I]), I)
    end
    return B
end

@inline _unsafe_copy_inner!(B::AbsArr, b, I) = @inbounds B[I] = b
@inline function _unsafe_copy_inner!(B::AbsArr, b::AbsArr, I)
    error("Internal error. Please file a bug report.")
end

@inline function _unsafe_copy_inner!(B::SlicedArray, b, I)
    @inbounds Bv = B[I]
    @inbounds Bv .= b
end
@inline function _unsafe_copy_inner!(B::SlicedArray, b::AbsArr, I)
    Bv = @inbounds B[I]
    if length(Bv) != length(b)
        throw(DimensionMismatch("Expected of $(length(Bv)) for f(slice). Got: $(length(b))"))
    end
    copyto!(Bv, b)
    return B
end

function _alloc_mapslices(S::SlicedArray{<:Any,N,M}, b1) where {T,N,M}
    innerax = _reshape_axes(axes(b1), Val(M))
    innersz = ntuple(i -> (Base.@_inline_meta; length(innerax[i])), Val(M))
    parentsz = static_merge(S.alongs, innersz, size(S))
    SlicedArray(Array{eltype(b1),M + N}(undef, parentsz...), S.alongs)
end

_reshape_axes(axes::Tuple, ::Val{N}) where {N} = Base.rdims(Val(N), axes)
_reshape_axes(axes::NTuple{N,Any}, ::Val{N}) where {N} = axes



#####
##### 3rd Party
#####

@inline function UnsafeArrays.unsafe_uview(S::SlicedArray)
    SlicedArray(UnsafeArrays.unsafe_uview(S.parent), S.alongs)
end

function Adapt.adapt_structure(T, S::SlicedArray)
    SlicedArray(adapt(T, S.parent), S.alongs)
end
