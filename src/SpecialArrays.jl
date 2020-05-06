module SpecialArrays

using Adapt

using Base: @propagate_inbounds, @pure, @_inline_meta
using Base: require_one_based_indexing, tail, unsafe_length

using DocStringExtensions
using Requires: @require
using UnsafeArrays

const NestedArray{V,M,N} = AbstractArray{<:AbstractArray{V,M},N}
const AbstractArrayOfArrays{N} = AbstractArray{<:AbstractArray}


const Idx = Union{Colon,Real,AbstractArray} # TODO

include("tuple.jl")
include("util.jl")
include("compat.jl")
include("viewtype.jl")

export innereltype, innerndims, inneraxes, innersize, innerlength
include("functions.jl")

export SlicedArray, slice, align
include("slicedarray.jl")

export FlattenedArray, flatview, flatten
include("flattenedarray.jl")

export BatchedVector, batch, batchlike
include("batchedvector.jl")

function __init__()
    @require Zygote = "e88e6eb3-aa80-5325-afca-941959d7151f" include("zygote.jl")
end

end # module
