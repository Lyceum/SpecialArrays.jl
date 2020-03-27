using Adapt: Adapt, adapt
using AxisArrays: AxisArrays
using Base: index_shape, index_dimsum, index_ndims, to_indices
using Base: mightalias, unalias, dataids
using BenchmarkTools
using LyceumBase.LyceumCore
using LyceumBase.TestUtil
using Parameters
using Random
using StaticNumbers
using Test
using UnsafeArrays

using SpecialArrays
using SpecialArrays: CartesianIndexer


include("util.jl")


testdims(L::Integer) = ntuple(i -> 3 + i, Val(unstatic(L)))
