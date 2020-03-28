module TestFunctions

include("preamble.jl")

function makedata(V::Type, M::Integer, N::Integer)
    dims = testdims(M + N)
    innersize, outersize = tuple_split(dims, M)

    nested = Array{Array{V,M},N}(undef, outersize...)
    for I in eachindex(nested)
        nested[I] = rand!(Array{V,M}(undef, innersize...))
    end

    anynested = []
    append!(anynested, nested)

    return (
        nested = nested,
        anynested = anynested,
        flat = reshape(reduce(hcat, nested), dims),
        dims = dims,
        innersize = innersize,
        outersize = outersize,
        inneraxes = axes(first(nested)),
        outeraxes = axes(nested),
    )
end

@testset "M = $M, N = $N, V = $V" for M = 0:2, N = 0:2, V in (Float64,)
    @testset "inner_*" begin
        data = makedata(V, M, N)
        A = data.nested
        B = data.anynested

        @test innereltype(A) === V
        @test_inferred innereltype(A)
        @test innereltype(typeof(A)) === V
        @test_inferred innereltype(typeof(A))
        @test innereltype(B) === Any

        @test innerndims(A) == M
        @test_inferred innerndims(A)
        @test innerndims(typeof(A)) == M
        @test_inferred innerndims(typeof(A))
        @test innerndims(B) == M

        @test inneraxes(A) == data.inneraxes
        @test_inferred inneraxes(A)
        @test inneraxes(B) == data.inneraxes

        @test innersize(A) == data.innersize
        @test_inferred innersize(A)
        @test innersize(B) == data.innersize

        @test innerlength(A) == prod(data.innersize)
        @test_inferred innerlength(A)
        @test innerlength(B) == prod(data.innersize)
    end
end

@testset "inner_* errors" begin
    A = [zeros(2), zeros(3)]
    @test_throws ErrorException inneraxes(A)
    @test_throws ErrorException innersize(A)
    @test_throws ErrorException innerlength(A)
end

end # module
