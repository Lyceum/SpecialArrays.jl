module TestFunctions

include("preamble.jl")

function makedata(V::Type, M::Integer, N::Integer)
    dims = testdims(M + N)
    innersize, outersize = tuplesplit(dims, M)

    nested = Array{Array{V,M},N}(undef, outersize...)
    for I in eachindex(nested)
        nested[I] = rand!(Array{V,M}(undef, innersize...))
    end

    return (
        nested = nested,
        flat = reshape(reduce(hcat, nested), dims),
        dims = dims,
        innersize = innersize,
        outersize = outersize,
        inneraxes = axes(first(nested)),
        outeraxes = axes(nested),
    )
end

@testset "M = $M, N = $N, V = $V" for M=1:2, N=1:2, V in (Float64, Int)
    @testset "inner_*" begin
        data = makedata(V, M, N)
        A = data.nested

        @test innereltype(A) === V
        @test_inferred innereltype(A)

        @test innerndims(A) == M
        @test_inferred innerndims(A)

        @test inneraxes(A) == data.inneraxes
        @test_inferred inneraxes(A)

        @test innersize(A) == data.innersize
        @test_inferred innersize(A)

        @test innerlength(A) == prod(data.innersize)
        @test_inferred innerlength(A)
    end

    @testset "flatten" begin
        data = makedata(V, M, N)
        @test flatten(data.flat) === data.flat
        @test flatten(data.nested) == data.flat
        @test_inferred flatten(data.nested)
    end
end

@testset "inner_* errors" begin
    let A = [zeros(2), zeros(4, 6)]
        # ndims(eltype(A)) throws a MethodError on A
        @test_throws MethodError innerndims(A)
    end

    let A = [zeros(2), zeros(3)]
        @test innereltype(A) == Float64
        @test_throws DimensionMismatch inneraxes(A)
        @test_throws DimensionMismatch innersize(A)
        @test_throws DimensionMismatch innerlength(A)
    end
end

end # module