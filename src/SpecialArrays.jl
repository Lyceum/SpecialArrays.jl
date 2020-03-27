module SpecialArrays

using Adapt
using Base: @propagate_inbounds, @pure, @_inline_meta, require_one_based_indexing
using DocStringExtensions
using LyceumBase.LyceumCore
using MacroTools: @forward
using StaticNumbers
using UnsafeArrays


const Idx = Union{Colon, Real, AbstractArray}


include("viewtype.jl")
include("cartesianindexer.jl")

export innereltype, innerndims, inneraxes, innersize, innerlength
export flatten
include("functions.jl")

export SlicedArray, slice
include("slicedarray.jl")

export FlattenedArray, flatview
include("flattenedarray.jl")

end # module