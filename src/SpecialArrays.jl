module SpecialArrays

using Adapt

using Base: @propagate_inbounds, @pure, @_inline_meta, require_one_based_indexing
using Base.MultiplicativeInverses: SignedMultiplicativeInverse

using DocStringExtensions
using LyceumCore
using MacroTools: @forward
using Requires: @require
using Shapes
using StaticNumbers
using UnsafeArrays


const Idx = Union{Colon,Real,AbstractArray}


include("viewtype.jl")
include("cartesianindexer.jl")

export innereltype, innerndims, inneraxes, innersize, innerlength
include("functions.jl")

export SlicedArray, slice
include("slicedarray.jl")

export FlattenedArray, flatten
include("flattenedarray.jl")

export ElasticArray
include("elasticarray.jl")

function __init__()
    @require Zygote = "e88e6eb3-aa80-5325-afca-941959d7151f" include("zygote.jl")
end

end # module
