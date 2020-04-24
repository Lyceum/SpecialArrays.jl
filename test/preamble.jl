using Base: index_shape, index_dimsum, index_ndims, to_indices
using Base: mightalias, unalias, dataids, unsafe_convert

using Adapt: Adapt, adapt
using LyceumDevTools
using Parameters
using Random
using StaticNumbers
using Test
using UnsafeArrays

using SpecialArrays
using SpecialArrays: AbsArr, front, tuple_split, TupleN


include("util.jl")
