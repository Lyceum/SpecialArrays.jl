struct SlicedArray{T,N,M,P,A,Fast} <: AbstractArray{T,N}
    parent::P
    alongs::A
    elstride::Int
    @inline function SlicedArray{T,N,M,P,A,Fast}(parent::AbstractArray{<:Any,L}, alongs::NTuple{L,TypedBool}, elstride::Integer) where {T,N,M,P,A,Fast,L}
        # TODO check parameters
        new(parent, alongs, elstride)
    end
end

@inline function SlicedArray(
    parent::P,
    alongs::A,
    inaxes::NTuple{M,Any},
    outaxes::NTuple{N,Any},
) where {L,M,N,P<:AbstractArray{<:Any,L},A<:NTuple{L,TypedBool}}
    T = viewtype(parent, mergeindices(axes(parent), alongs, static_map(first, outaxes)))
    elstride = mapfoldl(unsafe_length, *, inaxes)
    SlicedArray{T,N,M,P,A,iscontiguous(alongs)}(parent, alongs, elstride)
end

@inline function SlicedArray(parent::AbstractArray{<:Any,L}, alongs::NTuple{L,TypedBool}) where {L}
    paxes = axes(parent)
    inaxes = static_getindex(paxes, alongs)
    outaxes = static_getindex(paxes, static_map(!, alongs))
    SlicedArray(parent, alongs, inaxes, outaxes)
end

@inline function SlicedArray(parent::P, alongs::A) where {L,P<:AbstractArray{<:Any,L},A<:NTuple{L,False}}
    T = viewtype(parent, static_map(first, axes(parent)))
    SlicedArray{T,L,0,P,A,iscontiguous(alongs)}(parent, alongs, 1)
end


#function Base.similar(S::SlicedArray, ::Type{<:AbstractArray{V}}, dims::Dims{N}) where {V,N}
#    slice(similar(S.parent, V, (innersize(S)..., dims...)), Val(N))
#end


####
#### Core Array Interface
####

@inline Base.axes(S::SlicedArray) = static_getindex(axes(S.parent), static_map(!, S.alongs))

@inline Base.size(S::SlicedArray) = static_map(unsafe_length, axes(S))


@propagate_inbounds function Base.getindex(S::SlicedArray{<:Any,N}, I::Vararg{Int,N}) where {N}
    view(S.parent, mergeindices(S, I)...)
end

@propagate_inbounds Base.getindex(S::SlicedArray{<:Any,0,0}) = zeroview(S.parent)


@propagate_inbounds function Base.setindex!(S::SlicedArray{<:Any,N}, v, I::Vararg{Int,N}) where {N}
    S.parent[mergeindices(S, I)...] = v
    return S
end

# while eltype(S) <: AbstractArray{T,0}, the eltype of the underlying storage is T
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

#const MyIdx = Union{Real, Colon, AbstractVector{Real}}

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
#    K = mergeindices(S, J)
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


@inline function mergeindices(S::SlicedArray{<:Any,N}, I::NTuple{N,Any}) where {N}
    mergeindices(axes(S.parent), S.alongs, I)
end
@inline function mergeindices(paxes::NTuple{L,Any}, alongs::NTuple{L,TypedBool}, I::Tuple) where {L}
    sliceaxes = static_map(ax->(@_inline_meta; Base.Slice(ax)), paxes)
    static_setindex(sliceaxes, I, static_map(!, alongs))
end

@inline function mergesize(S::SlicedArray{<:Any,N}, dims::NTuple{N,Integer}) where {N}
    static_setindex(size(S.parent), dims, static_map(!, S.alongs))
end

@inline function mergeaxes(S::SlicedArray{<:Any,N}, ax::NTuple{N,Any}) where {N}
    static_setindex(axes(S.parent), ax, static_map(!, S.alongs))
end

@inline Base.SubArray(parent::SlicedArray, I::Tuple{}) = zeroview(parent)


####
#### Broadcasting
####

Broadcast.BroadcastStyle(::Type{<:SlicedArray}) = ArrayStyle{SlicedArray}()

function Base.similar(bc::Broadcasted{ArrayStyle{SlicedArray}}, ::Type{T}) where {T}
    S2 = find_sa(bc)
    S = slice(rand(2,3),:,*)
    @info "YO" typeof(S2)
    @info typeof(S) == typeof(S2)
    ax = axes(bc)
    max = (Base.OneTo(2), Base.OneTo(3))
    #max = mergeaxes(S, ax)
    #slice(similar(typeof(S.parent), mergeaxes(S, ax)), Val(length(ax)))
    slice(similar(S.parent, map(length, max)), Val(length(ax)))
end

# find the first SlicedArray among bc.args
find_sa(bc::Broadcast.Broadcasted) = find_sa(bc.args)
find_sa(args::Tuple) = find_sa(find_sa(args[1]), tail(args))
find_sa(x) = x
find_sa(::Tuple{}) = nothing
find_sa(S::SlicedArray, rest) = S
find_sa(::Any, rest) = find_sa(rest)


####
#### FastSlicedArray
####

# If only the leading dimensions of S.parent are sliced (i.e. S.alongs consists of any number of
# True's followed by any number of False's) then we can optimize some operations on S as well as
# provide additional functionality (e.g. append!, resize!, etc.).

const FastSlicedArray{T,N,M,P,A} = SlicedArray{T,N,M,P,A,true}

#function Base.copyto!(dest::FastSlicedArray, doffs::Integer, src::FastSlicedArray, soffs::Integer, n::Integer)
#    setindex_shape_check(dest, innersize(src))
#    pn = dest.elstride * n # if innersize(dest) == innersize(src) matches then so will elstride
#    copyto!(dest.parent, parentoffset(dest, doffs), src.parent, parentoffset(src, soffs), pn)
#    return dest
#end

@inline function parentoffset(S::FastSlicedArray, i::Integer)
    i1 = firstindex(S.parent)
    S.elstride * (i - i1) + i1
end


function Base.append!(S::FastSlicedArray, iter::FastSlicedArray)
    setindex_shape_check(S, innersize(iter))
    append!(S.parent, iter.parent)
    return S
end

function Base.prepend!(S::FastSlicedArray, iter::FastSlicedArray)
    setindex_shape_check(S, innersize(iter))
    prepend!(S.parent, iter.parent)
    return S
end


function Base.resize!(S::FastSlicedArray{<:Any,N}, dims::NTuple{N,Integer}) where {N}
    resize!(S.parent, mergesize(S, dims))
    return S
end
Base.resize!(S::FastSlicedArray{<:Any,N}, dims::Vararg{Integer,N}) where {N} = resize!(S, dims)

function Base.sizehint!(S::FastSlicedArray{<:Any,N}, dims::NTuple{N,Integer}) where {N}
    sizehint!(S.parent, mergesize(S, dims))
    return S
end
Base.sizehint!(S::FastSlicedArray{<:Any,N}, dims::Vararg{Integer,N}) where {N} = sizehint!(S, dims)



const FastSlicedVector{T,M,P,A} = SlicedArray{T,1,M,P,A,true}

function Base.pop!(S::FastSlicedVector)
    isempty(S) && throw(ArgumentError("array must be non-empty"))
    item = last(S) # TODO convert to Array?
    resize!(S, length(S) - 1)
    return item
end

# TODO need something like resizebeg! for ElasticArray
#function Base.popfirst!(S::FastSlicedVector)
#    isempty(S) && throw(ArgumentError("array must be non-empty"))
#    item = first(S)
#    resizebeg!(S.parent, length(S) - 1)
#    return item
#end

function Base.push!(S::FastSlicedVector{<:Any,M}, item::AbstractArray{<:Any,M}) where {M}
    setindex_shape_check(S, size(item))
    append!(S.parent, item)
    return S
end

function Base.pushfirst!(S::FastSlicedVector{<:Any,M}, item::AbstractArray{<:Any,M}) where {M}
    setindex_shape_check(S, size(item))
    prepend!(S.parent, item)
    return S
end


Base.empty!(S::FastSlicedVector) = resize!(S.parent, mergesize(S, (0, )))
Base.empty(S::FastSlicedVector) = similar(S, (0, ))


@inline function setindex_shape_check(dest::SlicedArray, szsrc::Dims)
    setindex_shape_check(innersize(dest), szsrc)
end


#####
##### Misc
#####

Base.parent(S::SlicedArray) = S.parent

Base.dataids(S::SlicedArray) = Base.dataids(S.parent)

Base.copy(S::SlicedArray) = SlicedArray(copy(S.parent), S.alongs)

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


function mapslices(f, A::AbstractArray; dims)
    Asliced = slice(A, dims...)
    S = _alloc_mapslices(Asliced, f(_unsafe_getindex_unwrapped(Asliced, firstindex(Asliced))))
    for I in eachindex(Asliced, S) # TODO start from second
        _unsafe_setindex!(S, f(_unsafe_getindex_unwrapped(Asliced, I)), I)
    end
    return S.parent
end

@inline function _alloc_mapslices(S::SlicedArray{<:Any,N,M}, b1) where {N,M}
    sz_inner = static_map(unsafe_length, _reshape_axes(axes(b1), Val(M)))
    psize = static_setindex(size(S.parent), sz_inner, S.alongs)
    SlicedArray(Array{eltype(b1),M+N}(undef, psize...), S.alongs)
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

@inline inneraxes(S::SlicedArray) = static_getindex(axes(S.parent), S.alongs)


"""
    slice(A::AbstractArray, alongs...)
    slice(A::AbstractArray, alongs)

Return an array whose elements are views into `A` along the dimensions `alongs`.
`alongs` can be specified in the following ways:
**TODO**
"""
@inline slice(A::AbstractArray, alongs...) = SlicedArray(A, toalongs(axes(A), alongs...))

@inline function slice(S::SlicedArray, alongs...)
    mergedalongs = static_setindex(S.alongs, toalongs(axes(A), alongs...), static_map(!, S.alongs))
    SlicedArray(S.parent, mergedalongs)
end


flatview(S::SlicedArray) = FlattenedArray(S)
flatview(S::FastSlicedArray) = S.parent

## align(slice(A, al), al) can just return the parent
#align(S::SlicedArray{<:Any,<:Any,<:Any,<:Any,A}, alongs::A) where {A<:TupleN{TypedBool}} = S.parent

#function align(A::AbstractArrayOfArrays, alongs::NTuple{L,TypedBool}) where {L}
#    if innerndims(A) != static_sum(alongs)
#        throw(ArgumentError("Must specify exactly M dimensions to be taken up by the inner arrays"))
#    end
#    dims = ntuple(identity, Val(L))
#    permuted_dims = (dims[alongs]..., dims[static_map(!, alongs)]...)
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



#####
##### 3rd Party
#####

@inline function UnsafeArrays.unsafe_uview(S::SlicedArray)
    SlicedArray(UnsafeArrays.unsafe_uview(S.parent), S.alongs)
end

function Adapt.adapt_structure(T, S::SlicedArray)
    SlicedArray(adapt(T, S.parent), S.alongs)
end
