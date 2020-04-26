const Glob = Union{Colon, typeof(*)}

to_idims(idims::Dims) = idims
to_idims(idims::TupleN{Integer}) = convert(Dims, idims)
to_idims(idims::Integer...) = to_idims(idims)
to_idims() = () # method ambiguity

to_idims(::Val{M}) where {M} = ntuple(identity, Val(M))

# without @pure this only infers up to length 5
@pure to_idims(idims::NTuple{N,Glob}) where {N} = _to_idims(ntuple(identity, Val(N)), idims)
to_idims(idims::Glob...) = to_idims(idims)
@inline function _to_idims(pdims::Dims, idims::Tuple{Colon, Vararg{Glob}})
    (first(pdims), _to_idims(tail(pdims), tail(idims))...)
end
@inline function _to_idims(pdims::Dims, idims::Tuple{typeof(*), Vararg{Glob}})
    _to_idims(tail(pdims), tail(idims))
end
_to_idims(pdims::Tuple{}, idims::Tuple{}) = ()


@pure invdims(dims::Dims, ::Val{L}) where {L} = TupleToolsX.deleteat(ntuple(identity, Val(L)), dims)
@pure invdims(dims::Dims{0}, ::Val{L}) where {L} = ntuple(identity, Val(L))

@pure iscontiguous(idims::Dims{N}) where {N} = idims == ntuple(identity, Val(N))

sliceaxes(A::AbstractArray) = sliceaxes(axes(A))
@pure sliceaxes(axes::Tuple) = TupleToolsX.map(ax->(@_inline_meta; Base.Slice(ax)), axes)

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
