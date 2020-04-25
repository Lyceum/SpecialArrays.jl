const GlobBool = Union{Colon, typeof(*)}
const BoolLike = Union{TypedBool, GlobBool}
canonify(x::True) = x
canonify(x::False) = x
canonify(::Colon) = True()
canonify(::typeof(*)) = False()
#@pure canonify(::Colon) = True()
#@pure canonify(::typeof(*)) = False()

# convert `alongs` to the canonical TypedBool form
@inline function toalongs(paxes::NTuple{L,Any}, alongs::NTuple{L,BoolLike}) where {L}
    static_map(canonify, alongs)
end
@inline function toalongs(paxes::NTuple{L,Any}, alongs::TupleN{Integer}) where {L}
    ntuple(dim -> (@_inline_meta; static_in(dim, alongs)), Val(L))
end
@inline function toalongs(paxes::NTuple{L,Any}, ::Tuple{}) where {L}
    ntuple(_ -> (@_inline_meta; False()), Val(L))
end
@inline toalongs(paxes::Tuple) = toalongs(paxes, ())
@inline function toalongs(paxes::NTuple{L,Any}, alongs::Val{N}) where {L,N}
    (ntuple(_ -> True(), Val(L-N))..., ntuple(_ -> False(), Val(N))...)
end
toalongs(paxes::Tuple, alongs::Union{Integer,BoolLike}...) = toalongs(paxes, alongs)

# returns true iff any number of True's followed by any number of False's
iscontiguous(alongs::Tuple{}) = true
iscontiguous(alongs::Tuple{True}) = true
iscontiguous(alongs::Tuple{False}) = true
iscontiguous(alongs::Tuple{True, Vararg{False}}) = true
iscontiguous(alongs::Tuple{True, Vararg{True}}) = true
iscontiguous(alongs::Tuple{False, Vararg{False}}) = true
iscontiguous(alongs::Tuple{False, Vararg{TypedBool}}) = false
iscontiguous(alongs::Tuple{True, Vararg{TypedBool}}) = iscontiguous(tail(alongs))
