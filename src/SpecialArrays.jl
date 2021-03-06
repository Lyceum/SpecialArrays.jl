module SpecialArrays

using Adapt

using Base: @propagate_inbounds, @pure, @_inline_meta
using Base: require_one_based_indexing, tail, unsafe_length

using Compat
using DocStringExtensions
using Requires: @require
using UnsafeArrays


const NestedArray{V,M,N} = AbstractArray{<:AbstractArray{V,M},N}


include("tuple.jl")
include("util.jl")

export inner_eltype, inner_ndims, inner_axes, inner_size, inner_length
include("functions.jl")

export SlicedArray, slice, align
include("slicedarray.jl")

export FlattenedArray, flatten
include("flattenedarray.jl")

export BatchedVector, batch, similarbatch
include("batchedvector.jl")

function __init__()
    @require Zygote = "e88e6eb3-aa80-5325-afca-941959d7151f" include("zygote.jl")
end

end # module
