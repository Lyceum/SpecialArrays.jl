

argerror(msg::AbstractString) = throw(ArgumentError(msg))

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


# `in` doesn't appear to always constant propagate.
# See: https://github.com/JuliaLang/julia/issues/33126
static_in(item::Integer, t::TupleN{Integer}) = _static_in(item, t)
_static_in(item::Integer, t::TupleN{Integer}) = ifelse(item in t, True(), False())
#@pure _static_in(item::Integer, t::TupleN{Integer}) = ifelse(item in t, True(), False())
