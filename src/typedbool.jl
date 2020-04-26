# Guidelines for the scary usage of @pure:
# Based this conversation:https://discourse.julialang.org/t/pure-macro/3871/2
# Two of the restrictions of @pure are
#   1. The function it’s used on cannot be further extended by other methods after it gets called.
#   2. It cannot recurse.
#   3. An error cannot be thrown in the @pure function or any function it calls
# Based on this, all usages of @pure here follow these general guidelines:
#   1. If function definition it is used on has concrete arguments, it's safe to use @pure on
#     that function (e.g. Base.:(!) below).
#   2. Otherwise, it is applied to non-exported function defined here, possibly with an additional
#     `__` prefixed function if the implementation is recursive.

# An NTuple with at least one element
const OneTuple{T,N} = Tuple{T, Vararg{T,N}}

struct True end
struct False end
const TypedBool = Union{True, False}
using Base: @pure, tail
const TupleN{T,N} = NTuple{N,T}

@pure static_getindex(xs::NTuple{N,Any}, I::NTuple{N,TypedBool}) where {N} = _static_getindex(xs, I)
@inline function _static_getindex(xs::OneTuple{Any}, I::OneTuple{TypedBool})
    rest  = _static_getindex(tail(xs), tail(I))
    return first(I) === True() ? (first(xs), rest...) : rest
end
_static_getindex(::Tuple{}, ::Tuple{}) = ()


@inline function static_setindex(t::NTuple{N,Any}, v::Tuple, I::NTuple{N,TypedBool}) where {N}
    _static_setindex(t, v, I, static_getindex(I, I))
end
function _static_setindex(t::Tuple, v::Tuple, I::Tuple, ::Tuple)
    throw(DimensionMismatch("Number of values provided does not match number of indices"))
end
@pure function _static_setindex(t::Tuple, v::NTuple{M,Any}, I::Tuple, ::NTuple{M,Any}) where {M}
    __static_setindex(t, v, I)
end
@inline function __static_setindex(t::OneTuple{Any}, v::OneTuple{Any}, I::OneTuple{TypedBool})
    if first(I) === True()
        (first(v), __static_setindex(tail(t), tail(v), tail(I))...)
    else
        (first(t), __static_setindex(tail(t), v, tail(I))...)
    end
end
# proper termination
__static_setindex(t::Tuple{}, v::Tuple{}, I::Tuple{}) = ()
__static_setindex(t::OneTuple{Any}, v::Tuple{}, I::OneTuple{False}) = t

@pure Base.:(!)(::False) = True()
@pure Base.:(!)(::True) = False()

@inline static_sum(t::TupleN{TypedBool}) = Val(length(t[t]))

@inline function static_map(@specialize(f), t::NTuple{N,Any}) where {N}
    ntuple(i -> (@_inline_meta; f(t[i])), Val(N))
end
