using SpecialArrays
using SpecialArrays: True, False, TypedBool, tuple_map, front, tail
using BenchmarkTools
using LyceumCore


flat = rand(2,3,4)
alongs = (True(), True(), False())
A = slice(flat, alongs)
B = [Array(a) for a in A]
B2 = deepcopy(B)
F = flatview(B)
F2 = flatview(B2)

x = B[end]

#@assert !iselastic((True(), True()))
#@assert iselastic((True(), False()))
#@assert !iselastic((False(), True()))
#@assert !iselastic((False(), False()))

#@assert !iselastic((True(), True(), True()))
#@assert !iselastic((False(), True(), True()))
#@assert !iselastic((True(), False(), True()))
#@assert iselastic((True(), True(), False()))
#@assert !iselastic((True(), False(), False()))
#@assert !iselastic((False(), True(), False()))
#@assert !iselastic((False(), False(), True()))
#@assert !iselastic((False(), False(), False()))


flat = rand(2,3,4)
al = (:,*,:)
S = slice(flat, al)
nested = [Array(el) for el in S]

nothing
