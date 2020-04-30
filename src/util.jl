const Glob = Union{Colon, typeof(*)}

@inline to_alongs(alongs::Bools{L}, ::Val{L}) where {L} = alongs
@inline function to_alongs(::Val{M}, ::Val{L}) where {M,L}
    (ntuple(_ -> true, Val(M))..., ntuple(_ -> false, Val(L))...)
end
@inline function to_alongs(dims::Dims, ::Val{L}) where {L}
    ntuple(dim -> (@_inline_meta; dim in dims), Val(L)) # TODO TupleTools.in
end
@inline function to_alongs(alongs::NTuple{L,Glob}, ::Val{L}) where {L}
    ntuple(i -> (@_inline_meta; alongs[i] === Colon()), Val(L))
end


# returns true iff any number of True's followed by any number of False's
iscontiguous(::Bools) = false
#iscontiguous(alongs::BoolIndex) = false # TODO
#iscontiguous(alongs::Tuple{}) = true
#iscontiguous(alongs::Tuple{True}) = true
#iscontiguous(alongs::Tuple{False}) = true
#iscontiguous(alongs::Tuple{True, Vararg{False}}) = true
#iscontiguous(alongs::Tuple{True, Vararg{True}}) = true
#iscontiguous(alongs::Tuple{False, Vararg{False}}) = true
#iscontiguous(alongs::Tuple{False, Vararg{TypedBool}}) = false
#iscontiguous(alongs::Tuple{True, Vararg{TypedBool}}) = iscontiguous(tail(alongs))

@pure sliceaxes(A::AbstractArray) = sliceaxes(axes(A))
@pure sliceaxes(axes::Tuple) = TupleTools.map(Base.Slice, axes)


####
#### Misc
####

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
