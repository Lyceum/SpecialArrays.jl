module SpecialArrays

using Adapt

using Base: @propagate_inbounds, @pure, @_inline_meta
using Base: require_one_based_indexing, setindex, unsafe_length
using Base.Broadcast: Broadcast, ArrayStyle, Broadcasted

using DocStringExtensions
using MacroTools: @forward
using Requires: @require
using Shapes
using StaticNumbers
using UnsafeArrays

const AbstractArrayOfSimilarArrays{V,M,N} = AbstractArray{<:AbstractArray{V,M},N}
const AbstractArrayOfArrays{N} = AbstractArray{<:AbstractArray}


const Idx = Union{Colon,Real,AbstractArray} # TODO

include("TupleTools.jl")
using .TupleTools
using .TupleTools: TupleN, Some, Anys # TODO

include("util.jl")
include("compat.jl")
include("viewtype.jl")

export innereltype, innerndims, inneraxes, innersize, innerlength
include("functions.jl")

export SlicedArray, slice
include("slicedarray.jl")

export FlattenedArray, flatview, flatten
include("flattenedarray.jl")

export BatchedVector, batch, batchlike
include("batchedvector.jl")

function __init__()
    @require Zygote = "e88e6eb3-aa80-5325-afca-941959d7151f" include("zygote.jl")
end

end # module
