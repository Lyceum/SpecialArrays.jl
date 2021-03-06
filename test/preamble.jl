using Adapt: Adapt, adapt
using Base: index_shape, index_dimsum, index_ndims, to_indices, front, tail
using Base: mightalias, unalias, dataids, unsafe_convert
using BenchmarkTools
using ElasticArrays
using MacroTools
using UnPack
using Random
using Test
using UnsafeArrays

using SpecialArrays
using SpecialArrays: TypedBool, True, False
using SpecialArrays: tuple_getindex, tuple_setindex, tuple_map, tuple_split, invert

include("util.jl")
