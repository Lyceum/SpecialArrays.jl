struct SlicedArray{T,N,M,alongs,P<:AbstractArray,C} <: AbstractArray{T,N}
    parent::P
    elstride::Int
    @inline function SlicedArray{T,N,M,alongs,P,C}(parent::P, elstride::Int) where {T,N,M,alongs,P<:AbstractArray,C}
        # TODO check parameters
        new(parent, elstride)
    end
end

#@inline function SlicedArray{T}(parent::P, elstride::Int, alongs::Bools, ::Anys{N}) where {T,P,N}
#    SlicedArray{T,N,ndims(T),alongs,P,iscontiguous(alongs)}(parent, elstride)
#end

#@inline function SlicedArray(parent::P, alongs::Bools) where {P<:AbstractArray}
#    mask = BoolIndex(alongs)
#    invmask = invert(mask)
#    paxes = sliceaxes(parent)
#    iaxes = paxes[mask]
#    oaxes = paxes[invmask]
#    T = typeof(view(parent, setindex(paxes, TupleTools.map(first, oaxes), invmask)...))
#    elstride = prod(TupleTools.map(unsafe_length, iaxes))
#    SlicedArray{T}(parent, elstride, alongs, iaxes)
#end

@inline function SlicedArray{T}(parent::P, elstride::Int, alongs::Bools, ::Anys{N}) where {T,P,N}
    SlicedArray{T,N,ndims(T),alongs,P,iscontiguous(alongs)}(parent, elstride)
end
alongs(::BoolIndex{N,M,mask}) where {N,M,mask} = mask
@inline function SlicedArray(parent::P, mask::BoolIndex) where {P<:AbstractArray}
    SlicedArray(parent, alongs(mask), mask, invert(mask))
end

@inline function SlicedArray(parent::P, alongs::Bools) where {P<:AbstractArray}
    mask = BoolIndex(alongs)
    invmask = invert(mask)
    SlicedArray(parent, alongs, mask, invmask)
end

@inline function SlicedArray(parent::P, alongs::Bools, mask::BoolIndex, invmask::BoolIndex) where {P<:AbstractArray}
    paxes = sliceaxes(parent)
    iaxes = paxes[mask]
    oaxes = paxes[invmask]
    T = typeof(view(parent, setindex(paxes, TupleTools.map(first, oaxes), invmask)...))
    #elstride = prod(TupleTools.map(unsafe_length, iaxes))
    #SlicedArray{T}(parent, elstride, alongs, iaxes)
end

#function Base.similar(S::SlicedArray, ::Type{<:AbstractArray{V}}, dims::Dims{N}) where {V,N}
#    slice(similar(S.parent, V, (innersize(S)..., dims...)), Val(N))
#end

alongs(S::SlicedArray{<:Any,<:Any,<:Any,_alongs}) where {_alongs} = _alongs
@inline mask(S::SlicedArray) = BoolIndex(alongs(S))
@inline invmask(S::SlicedArray) = invert(BoolIndex(alongs(S)))


####
#### Core Array Interface
####

@inline Base.axes(S::SlicedArray) = axes(S.parent)[invmask(S)]

@inline Base.size(S::SlicedArray) = size(S.parent)[invmask(S)]


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

@inline mergeindices(S::SlicedArray, I::Anys) = setindex(sliceaxes(S.parent), I, invmask(S))

@inline Base.SubArray(parent::SlicedArray, I::Tuple{}) = zeroview(parent)


####
#### Broadcasting
####

#Broadcast.BroadcastStyle(::Type{<:SlicedArray}) = ArrayStyle{SlicedArray}()

#function Base.similar(bc::Broadcasted{ArrayStyle{SlicedArray}}, ::Type{T}) where {T}
#    S2 = find_sa(bc)
#    S = slice(rand(2,3),:,*)
#    @info "YO" typeof(S2)
#    @info typeof(S) == typeof(S2)
#    ax = axes(bc)
#    max = (Base.OneTo(2), Base.OneTo(3))
#    #max = mergeaxes(S, ax)
#    #slice(similar(typeof(S.parent), mergeaxes(S, ax)), Val(length(ax)))
#    slice(similar(S.parent, map(length, max)), Val(length(ax)))
#end

# find the first SlicedArray among bc.args
find_sa(bc::Broadcast.Broadcasted) = find_sa(bc.args)
find_sa(args::Tuple) = find_sa(find_sa(args[1]), tail(args))
find_sa(x) = x
find_sa(::Tuple{}) = nothing
find_sa(S::SlicedArray, rest) = S
find_sa(::Any, rest) = find_sa(rest)


####
#### ContiguousSlicedArray
####

# If only the leading dimensions of S.parent are sliced (i.e. S.alongs consists of any number of
# True's followed by any number of False's) then we can optimize some operations on S as well as
# provide additional functionality (e.g. append!, resize!, etc.).

const ContiguousSlicedArray{T,N,M,P,A} = SlicedArray{T,N,M,P,A,true}

#function Base.copyto!(dest::ContiguousSlicedArray, doffs::Integer, src::ContiguousSlicedArray, soffs::Integer, n::Integer)
#    setindex_shape_check(dest, innersize(src))
#    pn = dest.elstride * n # if innersize(dest) == innersize(src) matches then so will elstride
#    copyto!(dest.parent, parentoffset(dest, doffs), src.parent, parentoffset(src, soffs), pn)
#    return dest
#end

@inline function parentoffset(S::ContiguousSlicedArray, i::Integer)
    i1 = firstindex(S.parent)
    S.elstride * (i - i1) + i1
end


function Base.append!(S::ContiguousSlicedArray, iter::ContiguousSlicedArray)
    setindex_shape_check(S, innersize(iter))
    append!(S.parent, iter.parent)
    return S
end

function Base.prepend!(S::ContiguousSlicedArray, iter::ContiguousSlicedArray)
    setindex_shape_check(S, innersize(iter))
    prepend!(S.parent, iter.parent)
    return S
end


function Base.resize!(S::ContiguousSlicedArray{<:Any,N}, dims::NTuple{N,Integer}) where {N}
    resize!(S.parent, setindex(size(S.parent), dims, mask(S)))
    return S
end
Base.resize!(S::ContiguousSlicedArray{<:Any,N}, dims::Vararg{Integer,N}) where {N} = resize!(S, dims)

function Base.sizehint!(S::ContiguousSlicedArray{<:Any,N}, dims::NTuple{N,Integer}) where {N}
    sizehint!(S.parent, setindex(size(S.parent), dims, mask(S)))
    return S
end
Base.sizehint!(S::ContiguousSlicedArray{<:Any,N}, dims::Vararg{Integer,N}) where {N} = sizehint!(S, dims)



const ContiguousSlicedVector{T,M,P,A} = SlicedArray{T,1,M,P,A,true}

function Base.pop!(S::ContiguousSlicedVector)
    isempty(S) && throw(ArgumentError("array must be non-empty"))
    item = last(S) # TODO convert to Array?
    resize!(S, length(S) - 1)
    return item
end

# TODO need something like resizebeg! for ElasticArray
#function Base.popfirst!(S::ContiguousSlicedVector)
#    isempty(S) && throw(ArgumentError("array must be non-empty"))
#    item = first(S)
#    resizebeg!(S.parent, length(S) - 1)
#    return item
#end

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


Base.empty!(S::ContiguousSlicedVector) = resize!(S.parent, setindex(size(S.parent), (0, ), mask(S)))
#Base.empty(S::ContiguousSlicedVector) = similar(S, (0, ))


@inline function setindex_shape_check(dest::SlicedArray, szsrc::Dims)
    setindex_shape_check(innersize(dest), szsrc)
end


#####
##### Misc
#####

Base.parent(S::SlicedArray) = S.parent

Base.dataids(S::SlicedArray) = Base.dataids(S.parent)

Base.copy(S::SlicedArray) = SlicedArray(copy(S.parent), S.alongs)

function Base.showarg(io::IO, S::SlicedArray, toplevel)
    print(io, "slice(")
    Base.showarg(io, S.parent, false) # TODO try ture
    if length(alongs(S)) > 0
        print(io, ", ", join(map(a -> a ? ':' : '*', alongs(S)), ", "))
    end
    print(io, ')')
    if toplevel
        print(io, " with ", Base.dims2string(innersize(S)), " eltype ", eltype(S)) # TODO innereltype? innerndims? T is a complicted subarray tytpe
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
    isize = TupleTools.map(unsafe_length, _reshape_axes(axes(b1), Val(M)))
    psize = setindex(size(S.parent), isize, mask(S))
    SlicedArray(Array{eltype(b1),M+N}(undef, psize...), alongs(S))
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

@inline inneraxes(S::SlicedArray) = axes(S.parent)[mask(S)]
@inline innersize(S::SlicedArray) = size(S.parent)[mask(S)]


"""
    slice(A::AbstractArray, alongs...)
    slice(A::AbstractArray, alongs)

Return an array whose elements are views into `A` along the dimensions `alongs`.
`alongs` can be specified in the following ways:
**TODO**
"""
@inline slice(A::AbstractArray{<:Any,L}, alongs...) where {L} = SlicedArray(A, to_alongs(alongs, Val(L)))
@inline slice(A::AbstractArray{<:Any,L}, alongs::Tuple) where {L} = SlicedArray(A, to_alongs(alongs, Val(L)))

#@inline function slice(S::SlicedArray{<:Any,N,M}, idims...) where {N,M}
#    pdims = ntuple(identity, Val(N+M))
#    mergedidims= setindices(S.alongs, to_alongs(axes(A), alongs...), static_map(!, S.alongs))
#    SlicedArray(S.parent, mergedalongs)
#end


flatview(S::SlicedArray) = FlattenedArray(S)
flatview(S::ContiguousSlicedArray) = S.parent

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
    SlicedArray(UnsafeArrays.unsafe_uview(S.parent), alongs(S))
end

function Adapt.adapt_structure(T, S::SlicedArray)
    SlicedArray(adapt(T, S.parent), alongs(S))
end
