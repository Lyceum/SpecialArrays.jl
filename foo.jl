module M

include("src/SpecialArrays.jl")
using .SpecialArrays
using BenchmarkTools
using LyceumCore
using .SpecialArrays: True, False, TypedBool, tuple_map, front, tail
using JuliennedArrays: JuliennedArrays, Align
const JA = JuliennedArrays


function bench(A)
    #for I in CartesianIndices(A)
    for I in eachindex(A)
    #for i=1:10, j=1:10
        @inbounds A[I] *= 2
    end
end

flat = rand(20, 30, 400)
#alongs = (True(), False(), True())
alongs = (True(), True(), False())
ja_alongs = Tuple(al === True() ? JA.True() : JA.False() for al in alongs)

A = slice(flat, alongs)
B = [Array(a) for a in A]

C = Align(B, ja_alongs...)
D = FlattenedArray(B, alongs)


@btime bench($C);
@btime bench($D);

function splitind(I::NTuple{L,Any}, alongs::NTuple{L,TypedBool}) where {L}
    I[alongs], I[tuple_map(!, alongs)]
end
function splitind2(I::Tuple, ::Val{M}, ::Val{N}) where {M,N}
    front(I, Val(M)), tail(I, Val(N))
end
@inline function splitind3(F::FlattenedArray{<:Any,<:Any,<:AbsArr{<:AbsArr{<:Any,M},N}}, I::Tuple) where {M,N}
    #I[F.alongs], I[tuple_map(!, F.alongs)]
    front(I, Val(M)), tail(I, Val(N))
end

#I = ntuple(identity, ndims(flat))
#@info "yo"
#@btime SpecialArrays._splitindices($D, $I)
##@btime splitind3($D, $I)
#@btime JA.split_indices($C, $I)

nothing
end #module

#@code_lowered M.C[1,2,3]
#@code_lowered M.D[1,2,3]
