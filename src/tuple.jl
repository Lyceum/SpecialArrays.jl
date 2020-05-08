# For convenience
const Some{T,N} = Tuple{T, Vararg{T,N}}
const TupleN{T,N} = NTuple{N,T}
const Anys{N} = NTuple{N,Any}


struct True end
struct False end
const TypedBool = Union{True, False}


@pure Base.:(!)(::False) = True()
@pure Base.:(!)(::True) = False()

@pure invert(t::TupleN{TypedBool}) = tuple_map(!, t)


@pure tuple_getindex(xs::Anys{N}, I::NTuple{N,TypedBool}) where {N} = _tuple_getindex(xs, I)
@inline function _tuple_getindex(xs::Some{Any,N}, I::Some{TypedBool,N}) where {N}
    rest  = _tuple_getindex(tail(xs), tail(I))
    return first(I) === True() ? (first(xs), rest...) : rest
end
_tuple_getindex(::Tuple{}, ::Tuple{}) = ()


@inline function tuple_setindex(t::Anys{N}, v::Tuple, I::NTuple{N,TypedBool}) where {N}
    _tuple_setindex(t, v, I, tuple_getindex(I, I))
end
@noinline function _tuple_setindex(t::Tuple, v::Tuple, I::Tuple, ::Anys{M}) where {M}
    throw_setindex_mismatch(M, length(v))
end
@pure function _tuple_setindex(t::Tuple, v::Anys{M}, I::Tuple, ::Anys{M}) where {M}
    __tuple_setindex(t, v, I)
end
@inline function __tuple_setindex(t::Some{Any,N}, v::Some{Any}, I::Some{TypedBool,N}) where {N}
    if first(I) === True()
        (first(v), __tuple_setindex(tail(t), tail(v), tail(I))...)
    else
        (first(t), __tuple_setindex(tail(t), v, tail(I))...)
    end
end
__tuple_setindex(t::Tuple{}, v::Tuple{}, I::Tuple{}) = ()
__tuple_setindex(t::Some{Any,N}, v::Tuple{}, I::Some{False,N}) where {N} = t


@inline tuple_map(f, t::Anys{N}) where {N} = ntuple(i -> (@_inline_meta; f(t[i])), Val(N))

@inline function tuple_split(t::NTuple{L,Any}, ::Val{M}) where {L,M}
    ntuple(i -> t[i], Val(M)), ntuple(i -> t[M+i], Val(L-M))
end
