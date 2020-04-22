module TestZygote

using Zygote

using SpecialArrays: along2string
using SpecialArrays: True, False, TypedBool, tuple_map

include("preamble.jl")

const TEST_ALONGS = [
    (True(), True(), False()),
    (True(), False(), False()),

    (False(), True(), True()),
    (False(), False(), True()),
]

function makedata(al::TupleN{TypedBool})
    L = length(al)
    pdims = testdims(L)
    sdims = findall(al)
    innersize = pdims[al]
    outersize = pdims[tuple_map(!, al)]
    M, N = length(innersize), length(outersize)

    flat = rand!(Array{Float64,L}(undef, pdims...))

    nested = Array{Array{Float64,M},N}(undef, outersize...)
    i = 0
    Base.mapslices(flat, dims = sdims) do el
        i += 1
        nested[i] = zeros(Float64, innersize...)
        nested[i] .= el
        el
    end

    return (nested = nested, flat = flat)
end

showalongs(al) = "($(join(map(SpecialArrays.along2string, al), ", ")))"
#@testset "al = $(showalongs(al))" for al in TEST_ALONGS
#    @testset "slice" begin
#        data = makedata(al)
#        gs1 = Zygote.gradient(n -> prod(sum(n)), data.nested)
#        gs2 = Zygote.gradient(f -> prod(sum(slice(f, al))), data.flat)
#        @test first(gs2) == reshape(mapreduce(vec, vcat, first(gs1)), size(data.flat))
#    end
#end

    #@testset "Zygote" begin
    #    data = makedata(Float64, (True(), False(), True()))
    #    x = rand!(zeros(last(data.innersize)))
    #    g1 = Zygote.gradient(x -> sum(sum(a -> a * x, data.nested)), x)
    #    g2 = Zygote.gradient(x -> sum(sum(a -> a * x, slice(data.flat, data.sdims))), x)
    #    @test g1 == g2
    #    @test_skip @test_inferred Zygote.gradient(
    #        x -> sum(sum(a -> a * x, slice(data.flat, data.static_sdims))),
    #        x,
    #    )
    #end

    #@testset "Zygote" begin
    #    data = makedata(V, M, N)
    #    g1 = Zygote.gradient(n -> prod(sum(reshape(hcat(n...), size(f)), dims=1)), data.nested)
    #    g2 = Zygote.gradient(n -> prod(sum(flatview(n), dims=1)), data.nested)
    #    g3 = Zygote.gradient(s -> prod(sum(flatview(s), dims=1)), s)
    #    @test g1 == g2
    #    @test_skip @test_inferred Zygote.gradient(A -> sum(flatview(A) * x), data.nested)
    #en

end # module

using Zygote
@warn "YOO"
using SpecialArrays: along2string
using SpecialArrays: True, False, TypedBool, tuple_map

include("preamble.jl")

al = (False(), True(), True())
data = TestZygote.makedata(al)
gs1 = Zygote.gradient(n -> prod(sum(n)), data.nested)
gs2 = Zygote.gradient(f -> prod(sum(slice(f, al))), data.flat)
@assert length(gs1) == length(gs2) == 1
@info "" size(first(gs1)) size(first(first(gs1))) size(first(gs2))


#@inline function unslice(A::AbsArr, I::NTuple{L,Union{Colon,typeof(*)}}) where {L}
#    alongs = ntuple(i -> (Base.@_inline_meta; I[i] === Colon() ? True() : False()), Val(L))
#    SlicedArray(reshape(A, Val(L)), alongs)
#end
#@inline slice(A::AbsArr, I::Vararg{Union{Colon,typeof(*)},L}) where {L} = slice(A, I)

function unslice(A::AbstractArray, alongs::NTuple{N,TypedBool}) where {N}
    dims = ntuple(identity, Val(N))
    permuted_dims = (dims[alongs]..., dims[tuple_map(!, alongs)]...)
    return PermutedDimsArray(flatview(A), permuted_dims)
end

flat = rand(2,3,4)
alongs = (True(), False(), True())
A = slice(flat, alongs)
B = [Array(a) for a in A]
@assert A == B
@assert flat == parent(A) == unslice(A, alongs) == unslice(B, alongs)

function bench(A)
    for I in CartesianIndices(A)
        @inbounds A[I] .*= 2
    end
end

flat = rand(20, 30, 400)
P = PermutedDimsArray(flat, (3, 2, 1))
#@btime bench($flat)
#@btime bench($P)
using JuliennedArrays
const JA = JuliennedArrays
alongs = (True(), False(), True())
ja_alongs = Tuple(al === True() ? JA.True() : JA.False() for al in alongs)
A = slice(flat, alongs)
B = [Array(a) for a in A]
C = Align(B, ja_alongs...)
D = unslice(B, alongs)
@btime bench($C); @btime bench($D);
nothing
