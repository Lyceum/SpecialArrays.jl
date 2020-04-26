const Glob = Union{Colon, typeof(*)}

to_alongs(alongs::Dims) = alongs
to_alongs(alongs::NTuple{N,Glob}) where {N} = _to_alongs(ntuple(identity, Val(N)), alongs...)
@inline function _to_alongs(dims::Dims, ::Colon, rest::Glob...)
    (first(dims), _to_alongs(tail(dims), rest...)...)
end
@inline _to_alongs(dims::Dims, ::typeof(*), rest::Glob...) = (_to_alongs(tail(dims), rest...)..., )
_to_alongs(dims::Tuple{}) = ()

iscontiguous(dims::Dims{0}) = true
iscontiguous(dims::Dims{1}) = true
@pure function iscontiguous(dims::Dims{N}) where {N}
    x = ntuple(identity, Val(N))
    for i = 1:N
        dims[i] == x[i] || return false
    end
    return true
end


####
#### Tuple utils
####

# TODO just length(t)
@inline static_sum(t::TupleN{TypedBool}) = Val(length(t[t]))

# TODO
@inline function static_map(@specialize(f), t::NTuple{N,Any}) where {N}
    ntuple(i -> (@_inline_meta; f(t[i])), Val(N))
end

@inline function getindices(t::Tuple, ind::Tuple{Vararg{Int}})
    (t[ind[1]], getindices(t, tail(ind))...)
end
getindices(t::Tuple, ind::Tuple{}) = ()

@inline function setindices(t::Tuple, v::NTuple{N,Any}, I::NTuple{N,Any}) where {N}
    setindices(setindex(t, first(v), first(I)), tail(v), tail(I))
end
setindices(t::Tuple, v::Tuple{}, I::Tuple{}) = t
