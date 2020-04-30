const Maybe{T} = Union{T,Nothing}
const TupleN{T,N} = NTuple{N,T}

const AbsArr{T,N} = AbstractArray{T,N}
const AbsMat{T} = AbstractMatrix{T}
const AbsVec{T} = AbstractVector{T}

const AbsNestedArr{N} = AbstractArray{<:AbstractArray,N}
const AbsSimilarNestedArr{V,M,N} = AbstractArray{<:AbstractArray{V,M},N}

@inline front(t::Tuple, m::Integer) = ntuple(i -> t[i], m)
@inline front(t::Tuple, ::Val{M}) where {M} = ntuple(i -> t[i], Val(M))
@inline front(t::NTuple{L,Any}) where {L} = front(t, Val(L - 1))

@inline function tail(t::Tuple, n::Integer)
    m = length(t) - n
    ntuple(i -> t[i+m], n)
end
@inline function tail(t::NTuple{L,Any}, ::Val{N}) where {L,N}
    M = L - N
    ntuple(i -> t[i+M], Val(N))
end
@inline tail(t::NTuple{L,Any}) where {L} = tail(t, Val(L - 1))

@inline function tuple_split(t::NTuple{L,Any}, ::Val{M}) where {L,M}
    front(t, Val(M)), tail(t, Val(L - M))
end
@inline function tuple_split(t::NTuple{L,Any}, m::Integer) where {L}
    front(t, Val(m)), tail(t, L - m)
end
