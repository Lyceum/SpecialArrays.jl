using SpecialArrays
using Base: index_shape, index_dimsum, index_ndims, to_indices
using Test
using Random

using AxisArrays: AxisArrays
using UnsafeArrays
using BenchmarkTools
using StaticNumbers
using Parameters

using LyceumBase.LyceumCore
using SpecialArrays: _maybe_unsqueeze
using LyceumBase.TestUtil

include("testutil.jl")

nones(N::Integer) = ntuple(_ -> 1, Val(unstatic(N)))

testdims(L::Integer) = ntuple(i -> 3 + i, Val(unstatic(L)))
