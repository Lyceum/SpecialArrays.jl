const Glob = Union{Colon, typeof(*)}

@inline to_alongs(alongs::TypedBools{L}, ::Val{L}) where {L} = alongs
@inline function to_alongs(::Val{M}, ::Val{L}) where {M,L}
    (ntuple(_ -> True(), Val(M))..., ntuple(_ -> False(), Val(L-M))...)
end
@inline function to_alongs(dims::Dims, ::Val{L}) where {L}
    ntuple(dim -> (@_inline_meta; dim in dims ? True() : False()), Val(L))
end
@inline function to_alongs(alongs::NTuple{L,Glob}, ::Val{L}) where {L}
    ntuple(i -> (@_inline_meta; alongs[i] === Colon() ? True() : False()), Val(L))
end
to_alongs(alongs::Tuple{}, ::Val{L}) where {L} = ntuple(_ -> False(), Val(L))
to_alongs(alongs, ::Val) = argerror("Invalid alongs: $alongs")


# returns true iff any number of True's followed by any number of False's
iscontiguous(alongs::Tuple{}) = true
iscontiguous(alongs::Tuple{True}) = true
iscontiguous(alongs::Tuple{False}) = true
iscontiguous(alongs::Tuple{True, Vararg{False}}) = true
iscontiguous(alongs::Tuple{True, Vararg{True}}) = true
iscontiguous(alongs::Tuple{False, Vararg{False}}) = true
iscontiguous(alongs::Tuple{False, Vararg{TypedBool}}) = false
@inline iscontiguous(alongs::Tuple{True, Vararg{TypedBool}}) = iscontiguous(tail(alongs))

@pure sliceaxes(A::AbstractArray) = sliceaxes(axes(A))
@pure sliceaxes(axes::Tuple) = tuple_map(Base.Slice, axes)


####
#### Misc
####

argerror(msg::AbstractString) = throw(ArgumentError(msg))
dimerror(msg::AbstractString) = throw(DimensionMismatch(msg))

# modified from Base (https://github.com/JuliaLang/julia/blob/04cf2d5f26b7b1ce58ece7c076ff97561db01722/base/indices.jl#L189)
@inline function setindex_shape_check(szdest::Dims, szsrc::Dims)
    szdest === szsrc || throw_setindex_mismatch(szdest, szsrc)
end
function throw_setindex_mismatch(szdest::Dims, szsrc::Dims)
    if length(szdest) == 1
        throw(DimensionMismatch("tried to assign $(szsrc[1]) elements to $(szdest[1]) destinations"))
    else
        throw(DimensionMismatch("tried to assign $(Base.dims2string(szsrc)) array to $(Base.dims2string(szdest)) destination"))
    end
end

@inline function viewtype(A::AA, I::II) where {AA<:AbstractArray, II<:Tuple}
    T = Core.Compiler.return_type(view, Tuple{AA,II.parameters...})
    isconcretetype(T) ? T : _viewtype(A, I)
end

@noinline function _viewtype(A::AA, I::II) where {AA<:AbstractArray, II<:Tuple}
    try
        return @inbounds typeof(view(A, I...))
    catch e
        msg = """
        Unable to infer the return type of $(_call2str(view, AA, II.parameters...))
        and typeof($(_call2str(view, typeof(A), I...))) threw the below error.
        Try passing in valid indices or implement:
            $(_call2str(viewtype, typeof(A), typeof(I)))

        """
        printstyled(stderr, msg, color = :light_red)
        rethrow(e)
    end
end

function _call2str(f, args...)
    argstrings = map(a -> a isa Type ? "::$a" : string(a), args)
    return length(args) > 0 ? "$f($(join(argstrings, ", ")))" : string(f)
end
