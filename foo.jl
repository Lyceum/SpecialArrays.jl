module M

include("src/SpecialArrays.jl")
using .SpecialArrays
using .SpecialArrays: True, False, TypedBool, tuple_map, front, tail
using BenchmarkTools
using LyceumCore
using Random

function comp(A, B, I, J)
    for (i, j) in zip(I, J)
        @assert A[i] == B[j] (i, j)
    end
end
iscont(S::SlicedArray{T,N,M,P,A,Fast}) where {T,N,M,P,A,Fast} = Fast

flat = reshape(collect(1:(1*2*3)), (1,2,3))
alongs = (:,:,*)
#alongs = (*,:,*)

S1 = slice(copy(flat), alongs)
S2 = slice(rand!(similar(flat)), alongs)
copyto!(S2, 1, S1, 1, length(S2))
comp(S2, S1, 1:length(S2), 1:length(S1))

S1 = slice(copy(flat), alongs)
S2 = slice(rand!(similar(flat)), alongs)
copyto!(S2, 1, S1, 2, 2)
comp(S2, S1, 1:2, 2:3)

#flat = rand(2,3,100)
#S1 = slice(copy(flat), alongs)
#S2 = slice(rand!(similar(flat)), alongs)
#@btime copyto!($S1, 1, $S2, 1, length($S1))
#@btime SpecialArrays.mycopyto!($S1, 1, $S2, 1, length($S1))

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


end # module
nothing
