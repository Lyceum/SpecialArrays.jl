module Mod

using Base: OneTo, @pure, @_inline_meta

const TupleN{T,N} = NTuple{N,T}
const Some{T,N} = Tuple{T, Vararg{T,N}}
const Anys{N} = NTuple{N,Any}
const Bools{N} = NTuple{N,Bool}


end #module
