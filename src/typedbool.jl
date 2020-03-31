struct True end

struct False end

const TypedBool = Union{True, False}

@pure Base.:(!)(::False) = True()
@pure Base.:(!)(::True) = False()


@generated function static_merge(::Bys, x::X, y::Y) where {Bys<:TupleN{TypedBool},X<:Tuple,Y<:Tuple}
    i = j = 0
    xy = []
    for By in Bys.parameters
        if By === True
            push!(xy, :(x[$(i += 1)]))
            i > tuple_length(X) && return :(throw(BoundsError(x, $i)))
        else
            push!(xy, :(y[$(j += 1)]))
            j > tuple_length(Y) && return :(throw(BoundsError(y, $j)))
        end
    end
    return :(@_inline_meta; $(Expr(:tuple, xy...)))
end

# See: https://github.com/JuliaLang/julia/issues/33126
static_in(x::StaticOrInt, itr::TupleN{StaticOrInt}) = _static_in(x, itr)
@pure function _static_in(x::StaticOrInt, itr::TupleN{StaticOrInt})
    for y in itr
        unstatic(y) === unstatic(x) && return True() # TODO ==? unwrap?
    end
    return False()
end


@inline function Base.getindex(xs::NTuple{N,Any}, I::NTuple{N,TypedBool}) where {N}
    _getindex(xs, I)
end

@inline function _getindex(xs::NTuple{N,Any}, I::NTuple{N,TypedBool}) where {N}
    rest  = _getindex(tail(xs), tail(I))
    return first(I) === True() ? (first(xs), rest...) : rest
end
_getindex(::Tuple{}, ::Tuple{}) = ()


@inline function Base.setindex(t::NTuple{N,Any}, v::Tuple, I::NTuple{N,TypedBool}) where {N}
    _setindex(t, v, I)
end

@inline function _setindex(t::NTuple{N,Any}, v::Tuple, I::NTuple{N,TypedBool}) where {N}
    if first(I) === True()
        (first(v), _setindex(tail(t), tail(v), tail(I))...)
    else
        (first(t), _setindex(tail(t), v, tail(I))...)
    end
end
_setindex(::Tuple{}, ::Tuple{}, ::Tuple{}) = ()
function _setindex(::Tuple{}, ::Tuple, ::Tuple{})
    throw(DimensionMismatch("Cannot assign more values than indices"))
end


@inline tuple_map(f::F, t::NTuple{N,Any}) where {F,N} = ntuple(i -> (@_inline_meta; f(t[i])), Val(N))

@inline Base.findall(t::NTuple{N,TypedBool}) where {N} = ntuple(identity, Val(N))[t]