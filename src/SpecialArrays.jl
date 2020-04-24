module SpecialArrays

using Adapt

using Base: @propagate_inbounds, @pure, @_inline_meta, require_one_based_indexing
using Base.MultiplicativeInverses: SignedMultiplicativeInverse
using Base: setindex, unsafe_length
using Base.Broadcast: Broadcast, ArrayStyle, Broadcasted

using DocStringExtensions
using MacroTools: @forward
using Requires: @require
using Shapes
using StaticNumbers
using UnsafeArrays

# TODO This is an abstract type in ArraysOfArrays
const AbstractArrayOfSimilarArrays{V,M,N} = AbstractArray{<:AbstractArray{V,M},N}
const AbstractArrayOfArrays{N} = AbstractArray{<:AbstractArray}

const TupleN{T,N} = NTuple{N,T}

const Idx = Union{Colon,Real,AbstractArray} # TODO

include("util.jl")
include("typedbool.jl")
include("indices.jl")
include("compat.jl")
include("viewtype.jl")
include("cartesianindexer.jl")

export innereltype, innerndims, inneraxes, innersize, innerlength
include("functions.jl")

export SlicedArray, slice
include("slicedarray.jl")

export FlattenedArray, flatview, flatten
include("flattenedarray.jl")

export ElasticArray
include("elasticarray.jl")

export BatchedVector, batch, batchlike
include("batchedvector.jl")

function __init__()
    @require Zygote = "e88e6eb3-aa80-5325-afca-941959d7151f" include("zygote.jl")
end

end # module
