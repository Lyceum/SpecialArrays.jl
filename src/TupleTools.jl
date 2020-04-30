module TupleTools

using Base: OneTo, @pure, @_inline_meta, setindex, tail

export TupleN, Some, Anys, Bools
export BoolIndex, index_length, invert


# For convenience
const TupleN{T,N} = NTuple{N,T}
const Some{T,N} = Tuple{T, Vararg{T,N}}
const Anys{N} = NTuple{N,Any}
const Bools{N} = NTuple{N,Bool}

#abstract type TypedBool end
#struct True <: TypedBool end
#struct False <: TypedBool end
#TypedBool(b::Bool) = b ? True() : False()


#Base.:(!)(::False) = True()
#Base.:(!)(::True) = False()

#getindex(xs::NTuple{N,Any}, I::NTuple{N,TypedBool}) where {N} = _getindex(xs, I)
#@inline function _getindex(xs::OneTuple{Any}, I::OneTuple{TypedBool})
#    rest  = _getindex(tail(xs), tail(I))
#    return first(I) === True() ? (first(xs), rest...) : rest
#end
#_getindex(::Tuple{}, ::Tuple{}) = ()


#@inline function setindex(t::NTuple{N,Any}, v::Tuple, I::NTuple{N,TypedBool}) where {N}
#    _setindex(t, v, I, getindex(I, I))
#end
#function _setindex(t::Tuple, v::Tuple, I::Tuple, ::Tuple)
#    throw(DimensionMismatch("Number of values provided does not match number of indices"))
#end
#function _setindex(t::Tuple, v::NTuple{M,Any}, I::Tuple, ::NTuple{M,Any}) where {M}
#    __setindex(t, v, I)
#end
#@inline function __setindex(t::OneTuple{Any}, v::OneTuple{Any}, I::OneTuple{TypedBool})
#    if first(I) === True()
#        (first(v), __setindex(tail(t), tail(v), tail(I))...)
#    else
#        (first(t), __setindex(tail(t), v, tail(I))...)
#    end
#end
## proper termination
#__setindex(t::Tuple{}, v::Tuple{}, I::Tuple{}) = ()
#__setindex(t::OneTuple{Any}, v::Tuple{}, I::OneTuple{False}) = t


#@inline static_sum(t::TupleN{TypedBool}) = Val(length(t[t]))

#@inline function static_map(@specialize(f), t::NTuple{N,Any}) where {N}
#    ntuple(i -> (@_inline_meta; f(t[i])), Val(N))
#end



@inline map(f, t::Anys{N}) where {N} = ntuple(i -> (@_inline_meta; f(t[i])), Val(N))


####
#### BoolIndex
####

struct BoolIndex{N,M,mask} <: AbstractVector{Int}
    @inline function BoolIndex{N,M,mask}() where {N,M,mask}
        #mask isa Bools{N::Int} && M::Int == index_length(mask) || error("Invalid type parameters")
        new{N,M,mask}()
    end
end

const EmptyBoolIndex = BoolIndex{0,0,()}

@pure BoolIndex(mask::Bools{N}) where {N} = BoolIndex{N,sum(mask),mask}()
BoolIndex(mask::Tuple{}) = BoolIndex{0,0,()}()
@inline BoolIndex(mask::Bool...) = BoolIndex(mask)

@pure index_length(I::Bools) = sum(I)
index_length(I::Tuple{}) = 0


Base.length(::BoolIndex{N,M}) where {N,M} = M
Base.size(B::BoolIndex) = (length(B), )

@inline function Base.getindex(B::BoolIndex{N,M}, i::Int) where {N,M}
    @boundscheck 1 <= i <= M || throw(BoundsError(B, i))
    _unsafe_getindex(B, i)::Int
end

@pure function _unsafe_getindex(B::BoolIndex{N,M,mask}, i::Int) where {N,M,mask}
    n = 0
    for j in OneTo(N)
        mask[j] && (n += 1)
        n == i && return j
    end
    nothing
end

IndexStyle(::Type{<:BoolIndex}) = IndexLinear()


@pure Base.in(i::Int, B::BoolIndex{N,M,mask}) where {N,M,mask} = 1 <= i <= N ? mask[i] : false

Base.showarg(io::IO, I::BoolIndex, toplevel) = show(io, I)
function Base.show(io::IO, I::BoolIndex{<:Any,M,mask}) where {M,mask}
    print(io, "BoolIndex", mask, )
    return nothing
end

@pure invert(B::BoolIndex{N,M,mask}) where {N,M,mask} = BoolIndex{N,N-M,invert(mask)}()
@pure invert(mask::Bools{N}) where {N} = map(!, mask)


####
#### Tuple indexing
####

@pure function Base.getindex(t::Anys{N}, I::BoolIndex{N,M}) where {N,M}
    ntuple(Val(M)) do i
        @_inline_meta
        t[getindex(I, i)]
    end
end

Base.getindex(t::Anys, I::BoolIndex{N,M,mask}) where {N,M,mask} = throw(BoundsError(t, [mask...]))


@pure function Base.setindex(t::Anys{N}, v::Anys{M}, I::BoolIndex{N,M,mask}) where {N,M,mask}
    j = Ref(0) # RefValue to avoid Core.Box
    ntuple(Val(N)) do i
        @_inline_meta
        i in I ? v[j[]+=1] : t[i]
    end
end

function Base.setindex(t::Anys, v::Anys, I::BoolIndex{N,M,mask}) where {N,M,mask}
    length(t) == N || throw(BoundsError(t, [mask...]))
    length(v) == M || throw(DimensionMismatch("tried to assign $(length(v)) elements to $M destinations"))
    error("This shouldn't happen. Please file a bug report")
end



end # module
