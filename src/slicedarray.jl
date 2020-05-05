struct SlicedArray{T<:AbstractArray,N,M,P<:AbstractArray,A<:TypedBools,C} <: AbstractArray{T,N}
    parent::P
    alongs::A
    @inline function SlicedArray{T,N,M,P,A,C}(parent::P, alongs::A) where {T<:AbstractArray,N,M,P<:AbstractArray,A<:TypedBools,C}
        # TODO check parameters
        new(parent, alongs)
    end
end

@inline function SlicedArray(
    parent::P,
    alongs::A,
    paxes::Anys{L},
    outaxes::Anys{N},
    inaxes::Anys{M}
) where {L,P<:AbstractArray{<:Any,L},A<:TypedBools{L},N,M}
    I = tuple_setindex(sliceaxes(parent), tuple_map(first, outaxes), invert(alongs))
    T = viewtype(parent, I)
    SlicedArray{T,N,M,P,A,iscontiguous(alongs)}(parent, alongs)
end

@inline function SlicedArray(parent::AbstractArray{<:Any,L}, alongs::TypedBools{L}) where {L}
    paxes = sliceaxes(parent)
    inaxes = tuple_getindex(paxes, alongs)
    outaxes = tuple_getindex(paxes, tuple_map(!, alongs))
    SlicedArray(parent, alongs, paxes, outaxes, inaxes)
end

@inline function SlicedArray(S::SlicedArray{<:Any,L}, alongs::TypedBools{L}) where {L}
    SlicedArray(S.parent, tuple_setindex(S.alongs, alongs, tuple_map(!, S.alongs)))
end


####
#### Core Array Interface
####

@inline Base.axes(S::SlicedArray) = tuple_getindex(axes(S.parent), invert(S.alongs))

@inline Base.size(S::SlicedArray) = tuple_getindex(size(S.parent), invert(S.alongs))


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

@inline Base.SubArray(parent::SlicedArray, I::Tuple{}) = zeroview(parent)

@inline function mergeindices(S::SlicedArray{<:Any,N}, I::Anys{N}) where {N}
    tuple_setindex(sliceaxes(S.parent), I, invert(S.alongs))
end


####
#### ContiguousSlicedArray
####

# If only the leading dimensions of S.parent are sliced (i.e. S.alongs consists of any number of
# True's followed by any number of False's) then we can optimize some operations on S as well as
# provide additional functionality (e.g. append!, resize!, etc.).

const ContiguousSlicedArray{T,N,M,P,A} = SlicedArray{T,N,M,P,A,true}

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
    resize!(S.parent, tuple_setindex(size(S.parent), dims, invert(S.alongs)))
    return S
end
function Base.resize!(S::ContiguousSlicedArray{<:Any,N}, dims::Vararg{Integer,N}) where {N}
    resize!(S, dims)
end

function Base.sizehint!(S::ContiguousSlicedArray{<:Any,N}, dims::NTuple{N,Integer}) where {N}
    sizehint!(S.parent, tuple_setindex(size(S.parent), dims, invert(S.alongs)))
    return S
end
function Base.sizehint!(S::ContiguousSlicedArray{<:Any,N}, dims::Vararg{Integer,N}) where {N}
    sizehint!(S, dims)
end


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

Base.empty!(S::ContiguousSlicedVector) = resize!(S.parent, 0)

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
    if length(S.alongs) > 0
        print(io, ", ", join(map(a -> a === True() ? ':' : '*', S.alongs), ", "))
    end
    print(io, ')')
    if toplevel
        print(io, " with ", Base.dims2string(innersize(S)), " eltype", innereltype(S))
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

@inline inneraxes(S::SlicedArray) = tuple_getindex(axes(S.parent), S.alongs)

@inline innersize(S::SlicedArray) = tuple_getindex(size(S.parent), S.alongs)


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
    SlicedArray(UnsafeArrays.unsafe_uview(S.parent), S.alongs)
end

function Adapt.adapt_structure(T, S::SlicedArray)
    SlicedArray(adapt(T, S.parent), S.alongs)
end
