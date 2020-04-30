module M

const TupleN{T,N} = NTuple{N,T}
const Some{T,N} = Tuple{T, Vararg{T,N}}
const Anys{N} = NTuple{N,Any}
const Bools{N} = NTuple{N,Bool}

const True = Val(true)
const False = Val(false)
const VBools{N} = NTuple{N,Union{typeof(True),typeof(False)}}

Base.getindex(t::Anys{N}, I::Bools{N}) where {N} = _getindex(t, I)
@inline function _getindex(t::Anys{N}, I::Bools{N}) where {N}
    rest  = _getindex(tail(t), tail(I))
    first(I) === True() ? (first(t), rest...) : rest
end
_getindex(t::Tuple{}, I::Bools{0}) = ()

@pure index_length(I::TupleN{Bool})

@inline function Base.setindex(t::Anys{N}, v::Tuple, I::Bools{N}) where {N}
    @boundscheck index_length(I) == length(v) || throw(DimensionMismatch("tried to assign $(length(v)) elements to $(index_length(I)) destinations"))
    _setindex(t, v, I)
end
@inline function _setindex(t::Some{Any}, v::Some{Any}, I::Some{TypedBool})
    if first(I) === True()
        (first(v), _setindex(tail(t), tail(v), tail(I))...)
    else
        (first(t), _setindex(tail(t), v, tail(I))...)
    end
end
_setindex(t::Tuple{}, v::Tuple{}, I::Tuple{}) = ()
_setindex(t::Some{Any}, v::Tuple{}, I::Some{False})


Base.getindex(t::Anys{N}, I::VBools{N}) where {N} = _getindex(t, I)
@inline function _getindex(t::Anys{N}, I::VBools{N}) where {N}
    rest  = _getindex(tail(t), tail(I))
    first(I) === True ? (first(t), rest...) : rest
end
_getindex(t::Tuple{}, I::VBools{0}) = ()

@inline function Base.setindex(t::Anys{N}, v::Tuple, I::VBools{N}) where {N}
    @boundscheck index_length(I) == length(v) || throw(DimensionMismatch("tried to assign $(length(v)) elements to $(index_length(I)) destinations"))
    _setindex(t, v, I)
end
@inline function _setindex(t::Some{Any}, v::Some{Any}, I::Some{TypedBool})
    if first(I) === True()
        (first(v), _setindex(tail(t), tail(v), tail(I))...)
    else
        (first(t), _setindex(tail(t), v, tail(I))...)
    end
end
_setindex(t::Tuple{}, v::Tuple{}, I::Tuple{}) = ()
_setindex(t::Some{Any}, v::Tuple{}, I::Some{False})
end # module
