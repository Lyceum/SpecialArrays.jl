# For convenience
const Some{T,N} = Tuple{T, Vararg{T,N}}
const TupleN{T,N} = NTuple{N,T}
const Anys{N} = NTuple{N,Any}

struct True end
struct False end
const TypedBool = Union{True, False}
@pure typed(x::Bool) = x ? True() : False()
untyped(::True) = true
untyped(::False) = false

@pure Base.:(!)(::False) = True()
@pure Base.:(!)(::True) = False()


const TypedBools{N} = NTuple{N,TypedBool}
@pure invert(t::TypedBools) = tuple_map(!, t)

@pure tuple_getindex(xs::Anys{N}, I::TypedBools{N}) where {N} = _tuple_getindex(xs, I)
@inline function _tuple_getindex(xs::Some{Any}, I::Some{TypedBool})
    rest  = _tuple_getindex(tail(xs), tail(I))
    return first(I) === True() ? (first(xs), rest...) : rest
end
_tuple_getindex(::Tuple{}, ::Tuple{}) = ()


@inline function tuple_setindex(t::Anys{N}, v::Tuple, I::TypedBools{N}) where {N}
    _tuple_setindex(t, v, I, tuple_getindex(I, I))
end
function _tuple_setindex(t::Tuple, v::Tuple, I::Tuple, ::Tuple)
    throw(DimensionMismatch("Number of values provided does not match number of indices"))
end
@pure function _tuple_setindex(t::Tuple, v::Anys{M}, I::Tuple, ::Anys{M}) where {M}
    __tuple_setindex(t, v, I)
end
@inline function __tuple_setindex(t::Some{Any}, v::Some{Any}, I::Some{TypedBool})
    if first(I) === True()
        (first(v), __tuple_setindex(tail(t), tail(v), tail(I))...)
    else
        (first(t), __tuple_setindex(tail(t), v, tail(I))...)
    end
end
__tuple_setindex(t::Tuple{}, v::Tuple{}, I::Tuple{}) = ()
__tuple_setindex(t::Some{Any}, v::Tuple{}, I::Some{False}) = t


@inline static_sum(t::TypedBools) = Val(length(t[t]))

@inline tuple_map(f, t::Anys{N}) where {N} = ntuple(i -> (@_inline_meta; f(t[i])), Val(N))

function tuple_split(t::NTuple{L,Any}, ::Val{M}) where {L,M}
    ntuple(i -> t[i], Val(M)), ntuple(i -> t[M+i], Val(L-M))
end
