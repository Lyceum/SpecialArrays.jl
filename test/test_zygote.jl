module TestZygote

using Zygote

include("preamble.jl")

const TEST_ALONGS = [
    (True(), False()),
    (False(), True()),
]

function makedata(alongs::NTuple{L,TypedBool}) where {L}
    psize = testdims(L)
    insize = tuple_getindex(psize, alongs)
    outsize = tuple_getindex(psize, invert(alongs))
    M = length(insize)
    N = length(outsize)
    alongs_int = tuple_getindex(ntuple(identity, L), alongs)

    flat = Array{Float64,L}(undef, psize...)
    flat[:] .= 1:length(flat)
    nested = Array{Array{Float64,M},N}(undef, outsize...)

    i = 0
    Base.mapslices(flat, dims = alongs_int) do el
        i += 1
        nested[i] = zeros(Float64, insize...)
        nested[i] .= el
        el
    end

    return (nested = nested, flat = flat)
end

showalongs(alongs) = "($(join(map(a -> a === True() ? ':' : '*', alongs), ", ")))"

@testset "slice" begin
    flat = rand(2, 3)
    let
        gs1 = Zygote.gradient(x -> prod(sum(eachcol(x))), flat)
        gs2 = Zygote.gradient(x -> prod(sum(slice(x, :, *))), flat)
        @test first(gs1) == first(gs2)
    end
    let
        gs1 = Zygote.gradient(x -> prod(sum(eachrow(x))), flat)
        gs2 = Zygote.gradient(x -> prod(sum(slice(x, *, :))), flat)
        @test first(gs1) == first(gs2)
    end
end

@testset "align/flatview" begin
    nested = [rand(2) for _=1:3]
    # Because of the splat in the below hcat, gs1 is a tuple-of-vectors, while gs2 is a
    # vector-of-vectors, so we have to convert for the comparison.
    # See also: https://github.com/FluxML/Zygote.jl/pull/501
    let
        gs1 = Zygote.gradient(x -> prod(sum(eachcol(hcat(x...)))), nested)
        gs2 = Zygote.gradient(x -> prod(sum(eachcol(align(x, :, *)))), nested)
        gs3 = Zygote.gradient(x -> prod(sum(eachcol(flatview(x)))), nested)
        gs1 = Tuple(Array(el) for el in first(gs1))
        gs2 = Tuple(Array(el) for el in first(gs2))
        gs3 = Tuple(Array(el) for el in first(gs3))
        @test gs1 == gs2
        @test gs1 == gs3
    end
    let
        # align(A, :, *) == align(A, *, :)
        gs1 = Zygote.gradient(x -> prod(sum(eachcol(hcat(x...)'))), nested)
        gs2 = Zygote.gradient(x -> prod(sum(eachcol(align(x, *, :)))), nested)
        gs1 = Tuple(Array(el) for el in first(gs1))
        gs2 = Tuple(Array(el) for el in first(gs2))
        @test gs1 == gs2
    end
end

end # module
