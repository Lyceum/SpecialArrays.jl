# For AbstractArray A and indices I, compute T = typeof(view(A, I...)) by:
# 1) Try to infer T from typeof(A) and typeof(I) by calling viewtype(typeof(A), typeof(I))
# 2) If !isconcretetype(T), fall back to typeof(view(A, I...))
# Custom subtypes of AbstractArray (e.g. UnsafeArray) could provide customized implementations
# of viewtype(A::AbstractArray, I::Tuple) if required.

viewtype(A::AbstractArray, I::Tuple) = _viewtype(A, I)
viewtype(A::AbstractArray, I...) = _viewtype(A, I)

@inline _viewtype(A::AbstractArray, ::Tuple{}) = typeof(zeroview(A))

@generated function _viewtype(A::AA, I::II) where {AA<:AbstractArray,II<:Tuple}
    T = Core.Compiler.return_type(view, Tuple{AA,II.parameters...})
    if isconcretetype(T)
        return :($T)
    else
        return :(__viewtype(A, I))
    end
end

function call2str(f, args...)
    argstrings = map(a -> a isa Type ? "::$a" : string(a), args)
    return length(args) > 0 ? "$f($(join(argstrings, ", ")))" : string(f)
end

@inline function __viewtype(A::AA, I::II) where {AA<:AbstractArray,II<:Tuple}
    try
        typeof(view(A, I...))
    catch e
        msg = join((
            "Unable to infer the return type of $(call2str(view, AA, II.parameters...)). ",
            "Only other option is to try typeof($(call2str(view, typeof(A), I...))) but ",
            "that resulted in the below error. Try passing in valid indices or implement:\n",
            "   $(call2str(viewtype, typeof(A), typeof.(I)...))\n",
        ))
        printstyled(stderr, msg, color = :light_red)
        rethrow(e)
    end
end

# Workaround for https://github.com/JuliaArrays/StaticArrays.jl/issues/705
@inline function zeroview(A::AbstractArray)
    SubArray(IndexStyle(Base.viewindexing(()), IndexStyle(A)), A, Base.ensure_indexable(()), Base.index_dimsum())
end
