struct SlicedArray{T,N,M,P,A} <: AbstractArray{T,N}
    parent::P
    alongs::A
    @inline function SlicedArray{T,N,M,P,A}(parent::AbstractArray{<:Any,L}, alongs::NTuple{L,TypedBool}) where {T,N,M,P,A,L}
        # TODO check parameters
        new(parent, alongs)
    end
end

@inline function SlicedArray(
    parent::AbstractArray{<:Any,L},
    alongs::NTuple{L,TypedBool},
    outaxes::NTuple{N,Any},
) where {L,N}
    I = tuple_map(first, outaxes)
    J = parentindices(axes(parent), alongs, I)
    T = viewtype(parent, J)
    SlicedArray{T,N,L-N,typeof(parent),typeof(alongs)}(parent, alongs)
end

@inline function SlicedArray(parent::AbstractArray{<:Any,L}, alongs::NTuple{L,TypedBool}) where {L}
    SlicedArray(parent, alongs, axes(parent)[tuple_map(!, alongs)])
end


"""
    slice(A::AbstractArray, alongs...)
    slice(A::AbstractArray, alongs)

Return an array whose elements are views into `A` along the dimensions `alongs`.
`alongs` can be specified in the following ways:
**TODO**
"""
@inline function slice(A::AbstractArray{<:Any,L}, alongs::NTuple{L,TypedBool}) where {L}
    SlicedArray(A, alongs)
end

@inline function slice(S::SlicedArray{<:Any,N}, alongs::NTuple{N,TypedBool}) where {N}
    SlicedArray(S.parent, setindex(S.alongs, alongs, tuple_map(!, S.alongs)))
end

@inline function slice(A::AbstractArray{<:Any,L}, alongs::NTuple{L,GlobBool}) where {L}
    SlicedArray(A, tuple_map(canonify, alongs))
end

@inline function slice(A::AbstractArray{<:Any,L}, alongs::TupleN{Integer}) where {L}
    SlicedArray(A, ntuple(dim -> (@_inline_meta; static_in(dim, alongs)), Val(L)))
end

@inline function slice(A::AbstractArray{<:Any,L}, alongs::Tuple{}) where {L}
    SlicedArray(A, ntuple(_ -> (@_inline_meta; False()), Val(L)))
end

@inline function slice(A::AbstractArray{<:Any,L}, alongs::Val{M}) where {L,M}
    SlicedArray(A, (ntuple(_ -> True(), Val(M))..., ntuple(_ -> False(), Val(L-M))...))
end

@inline slice(A::AbstractArray, alongs::TypedBool...) = slice(A, alongs)
@inline slice(A::AbstractArray, alongs::GlobBool...) = slice(A, alongs)
@inline slice(A::AbstractArray, alongs::Integer...) = slice(A, alongs)
@inline slice(A::AbstractArray) = slice(A, ())


####
#### Core Array Interface
####

@inline Base.axes(S::SlicedArray) = axes(S.parent)[tuple_map(!, S.alongs)]

@inline Base.size(S::SlicedArray) = tuple_map(Base.unsafe_length, axes(S))


@propagate_inbounds function Base.getindex(S::SlicedArray{<:Any,N}, I::Vararg{Int,N}) where {N}
    view(S.parent, parentindices(S, I)...)
end


@propagate_inbounds function Base.setindex!(S::SlicedArray{<:Any,N}, v, I::Vararg{Int,N}) where {N}
    S.parent[parentindices(S, I)...] = v
    return S
end

@propagate_inbounds function Base.setindex!(
    S::SlicedArray{<:Any,N,0},
    v::AbstractArray{<:Any,0},
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

#@inline function Base.getindex(S::SlicedArray{<:Any,N}, I::Vararg{Idx,N}) where {N}
#    J = Base.to_indices(S, I)
#    @boundscheck checkbounds(S, J...)
#    _unsafe_getindex(S, J, Base.index_ndims(J...))
#end

#@inline function _unsafe_getindex(
#    S::SlicedArray{<:Any,N},
#    J::NTuple{N,Idx},
#    ::NTuple{N,Bool},
#) where {N}
#    K = parentindices(S, J)
#    return @inbounds _maybe_wrap(view(S.parent, K...), reslice(S.alongs, K))
#end

## Fall back to Cartesian indexing.
#@inline function _unsafe_getindex(S::SlicedArray, J::Tuple, ::Tuple)
#    @inbounds CartesianIndexer(A)[J...]
#end

#@inline _maybe_wrap(A::AbstractArray, alongs::TupleN{TypedBool}) = SlicedArray(A, alongs)
## A single element, so no need to wrap with a SlicedArray
#@inline _maybe_wrap(A::AbstractArray{<:Any,M}, ::NTuple{M,True}) where {M} = A

## add/drop non-sliced dimensions (i.e. alongs[dim] == False()) to match J
#@inline function reslice(alongs::NTuple{L,TypedBool}, K::NTuple{L,Any}) where {L}
#    (_reslice1(first(alongs), first(K))..., reslice(tail(alongs), tail(K))...)
#end
#reslice(::Tuple{}, ::Tuple{}) = ()
#@inline _reslice1(::True, k) = (True(), ) # keep inner dimension
#@inline _reslice1(::False, k) = _reslicefalse(k)
#@inline _reslicefalse(::Real) = () # drop this dimension
#@inline _reslicefalse(::Colon) = (False(), ) # keep this dimension
#@inline function _reslicefalse(::AbstractArray{<:Any,N}) where {N}
#    ntuple(_ -> False(), Val(N))
#end


@inline function parentindices(S::SlicedArray{<:Any,N}, I::NTuple{N,Any}) where {N}
    parentindices(axes(S.parent), S.alongs, I)
end

@inline function parentindices(paxes::NTuple{L,Any}, alongs::NTuple{L,TypedBool}, I::Tuple) where {L}
    setindex(tuple_map(ax->(@_inline_meta; Base.Slice(ax)), paxes), I, tuple_map(!, alongs))
end


#####
##### Misc
#####

Base.:(==)(A::SlicedArray, B::SlicedArray) = A.alongs == B.alongs && A.parent == B.parent

Base.parent(S::SlicedArray) = S.parent

Base.dataids(S::SlicedArray) = Base.dataids(S.parent)

Base.copy(S::SlicedArray) = SlicedArray(copy(S.parent), S.alongs)

# TODO only iff elastic
#Base.append!(S::SlicedArray, iter) = (append!(S.parent, iter); S)

#Base.prepend!(S::SlicedArray, iter) = (prepend!(S.parent, iter); S)

#function Base.resize!(S::SlicedArray{<:Any,N}, dims::NTuple{N,Integer}) where {N}
#    indims = innersize(S)
#    parentdims = static_merge(S.alongs, indims, dims)
#    resize!(S.parent, parentdims)
#    return S
#end

#Base.resize!(S::SlicedArray, dims::Integer...) = resize!(S, dims)


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


# returns true iff any number of True's followed by any number of False's
iscontiguous(alongs::Tuple{True}) = true
iscontiguous(alongs::Tuple{False}) = true
iscontiguous(alongs::Tuple{True, Vararg{False}}) = true
iscontiguous(alongs::Tuple{True, Vararg{True}}) = true
iscontiguous(alongs::Tuple{False, Vararg{False}}) = true
iscontiguous(alongs::Tuple{False, Vararg{TypedBool}}) = false
iscontiguous(alongs::Tuple{True, Vararg{TypedBool}}) = iscontiguous(tail(alongs))


## returns true iff alongs[1:end-1] are True and alongs[end] is False
#iselastic(alongs::TupleN{TypedBool}) = _iselastic(front(alongs), last(alongs))
#_iselastic(alongs::TupleN{True}, ::False) = true
#_iselastic(alongs::TupleN{TypedBool}, ::TypedBool) = false


#####
##### Extra
#####

@inline inneraxes(S::SlicedArray) = axes(S.parent)[S.alongs]

flatview(S::SlicedArray) = iscontiguous(S.alongs) ? S.parent : FlattenedArray(S)

## align(slice(A, al), al) can just return the parent
#align(S::SlicedArray{<:Any,<:Any,<:Any,<:Any,A}, alongs::A) where {A<:TupleN{TypedBool}} = S.parent

#function align(A::AbstractArrayOfArrays, alongs::NTuple{L,TypedBool}) where {L}
#    if innerndims(A) != static_sum(alongs)
#        throw(ArgumentError("Must specify exactly M dimensions to be taken up by the inner arrays"))
#    end
#    dims = ntuple(identity, Val(L))
#    permuted_dims = (dims[alongs]..., dims[tuple_map(!, alongs)]...)
#    return PermutedDimsArray(flatview(A), permuted_dims)
#end
#align(A::AbstractArrayOfArrays, alongs::TypedBool...) = align(A, alongs)

#@inline function align(A::AbstractArrayOfArrays, alongs::NTuple{L,Union{Colon,typeof(*)}}) where {L}
#    align(A, ntuple(i -> (@_inline_meta; alongs[i] === Colon() ? True() : False()), Val(L)))
#end
#@inline align(A::AbstractArrayOfArrays, alongs::Vararg{Union{Colon,typeof(*)}}) = align(A, alongs)

#@inline function align(A::AbstractArrayOfArrays, alongs::NTuple{L,Integer}) where {L}
#    align(A, ntuple(dim -> (@_inline_meta; static_in(dim, alongs)), Val(L)))
#end
#@inline align(A::AbstractArrayOfArrays, alongs::Integer...) = align(A, alongs)

#function align(A::AbstractArrayOfArrays, ::Val{M}) where {M}
#    alongs = (ntuple(_ -> True(), Val(M))..., ntuple(_ -> False(), Val(ndims(A)))...)
#    align(A, alongs)
#end


function mapslices(f, A::AbstractArray; dims)
    S = slice(A, dims...)
    B = _alloc_mapslices(S, f(first(S)))
    for I in eachindex(S, B) # TODO start from second
        _unsafe_copy_inner!(B, f(S[I]), I)
        #@inbounds B[I] .= f(S[I]) # TODO check this
    end
    return B.parent
end

@inline function _unsafe_copy_inner!(B::SlicedArray, b, I)
    @inbounds Bv = B[I]
    @inbounds Bv .= b
end

@inline function _unsafe_copy_inner!(B::SlicedArray, b::AbstractArray, I)
    Bv = @inbounds B[I]
    if length(Bv) != length(b)
        throw(DimensionMismatch("Expected of $(length(Bv)) for f(slice). Got: $(length(b))"))
    end
    copyto!(Bv, b)
    return B
end

function _alloc_mapslices(S::SlicedArray{<:Any,N,M}, b1) where {N,M}
    innerax = _reshape_axes(axes(b1), Val(M))
    parentsz = static_merge(S.alongs, tuple_map(length, innerax), size(S))
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
