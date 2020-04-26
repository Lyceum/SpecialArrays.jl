module TupleToolsX

using Base: @_inline_meta, setindex, tail
using TupleTools: deleteat, getindices, prod

@inline function map(@specialize(f), t::NTuple{N,Any}) where {N}
    ntuple(i -> (@_inline_meta; f(t[i])), Val(N))
end




# setindex on a Tuple is type unstable if changing eltypes
@inline setindices(t::Tuple, v::Tuple, I::Dims) = setindices(t, v, Val(I))
function setindices(t::NTuple{N,Any}, v::NTuple{M,Any}, ::Val{I}) where {N,M,I}
    if @generated
        j = 0
        return Expr(:tuple, ntuple(i -> i in I ? :(v[$(j+=1)]) : :(t[$i]), N)...)
    else
        return _setindices(t, v, I)
    end
end
@inline function _setindices(t::Tuple, v::NTuple{N,Any}, I::Dims{N}) where {N}
    _setindices(setindex(t, first(v), first(I)), tail(v), tail(I))
end
_setindices(t::Tuple, v::Tuple{}, I::Tuple{}) = t

end # module
