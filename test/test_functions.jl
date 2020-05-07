module TestFunctions

include("preamble.jl")

function makedata(V::Type, M::Integer, N::Integer)
    dims = testdims(M + N)
    inner_size, outersize = tuple_split(dims, Val(M))

    nested = Array{Array{V,M},N}(undef, outersize...)
    for I in eachindex(nested)
        nested[I] = rand!(Array{V,M}(undef, inner_size...))
    end

    return (
        nested = nested,
        flat = reshape(reduce(hcat, nested), dims),
        dims = dims,
        inner_size = inner_size,
        outersize = outersize,
        inner_axes = axes(first(nested)),
        outeraxes = axes(nested),
    )
end

@testset "M = $M, N = $N, V = $V" for M = 0:2, N = 0:2, V in (Float64,)
    @testset "inner_*" begin
        data = makedata(V, M, N)
        A = data.nested

        @test inner_eltype(A) === V
        @test_inferred inner_eltype(A)
        @test inner_eltype(typeof(A)) === V
        @test_inferred inner_eltype(typeof(A))

        @test inner_ndims(A) == M
        @test_inferred inner_ndims(A)
        @test inner_ndims(typeof(A)) == M
        @test_inferred inner_ndims(typeof(A))

        @test inner_axes(A) == data.inner_axes
        @test_inferred inner_axes(A)

        @test inner_size(A) == data.inner_size
        @test_inferred inner_size(A)

        @test inner_length(A) == prod(data.inner_size)
        @test_inferred inner_length(A)
    end
end

@testset "inner_* errors" begin
    A = [zeros(2), zeros(3)]
    @test_throws ArgumentError inner_axes(A)
    @test_throws ArgumentError inner_size(A)
    @test_throws ArgumentError inner_length(A)
end

end # module
