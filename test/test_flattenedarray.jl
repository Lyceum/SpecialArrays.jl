module FlattenedArrayTest

include("preamble.jl")

function makedata(V::Type, M::Integer, N::Integer)
    dims = testdims(M + N)
    innerdims, outerdims = tuplesplit(dims, M)

    nested = Array{Array{V,M},N}(undef, outerdims...)
    for I in eachindex(nested)
        nested[I] = rand!(Array{V,M}(undef, innerdims...))
    end

    return (
        nested = nested,
        flat = reshape(reduce(vcat, nested), dims),
        dims = dims,
        innerdims = innerdims,
        outerdims = outerdims,
        inneraxes = axes(first(nested)),
        outeraxes = axes(nested),
    )
end

@testset "V = $V, M = $M, N = $N" for M=1:2, N=1:2, V in (Float64, )

    @testset "constructors" begin
        data = makedata(V, M, N)
        Expected = FlattenedArray{V,M+N,M,typeof(data.nested),typeof(data.inneraxes)}

        @test typeof(Expected(data.nested, data.inneraxes)) == Expected
        @test_inferred Expected(data.nested, data.inneraxes)

        @test typeof(FlattenedArray(data.nested)) == Expected
        @test_inferred FlattenedArray(data.nested)

        @test typeof(FlattenedArray(data.nested, data.inneraxes)) == Expected
        @test_inferred FlattenedArray(data.nested, data.inneraxes)
    end

    let V=V, M=M, N=N
        test_array_AB() do
            data = makedata(V, M, N)
            A = FlattenedArray(data.nested)
            B = data.flat
            return A, B
        end
    end

    #@testset "misc" begin
    #    S, _, _ = test_SNF()
    #    @test parent(S) === S.parent
    #    @test Base.dataids(S) === Base.dataids(S.parent)
    #end

    #@testset "Extra" begin
    #    S, nested, _ = test_SNF()

    #    @test begin
    #        flat = flatten(S)
    #        flat !== S.parent && flat == S.parent
    #    end
    #    @test_inferred flatten(S)

    #    @test flatview(S) === S.parent
    #    @test_inferred flatview(S)

    #    @test innersize(S) == size(first(S))
    #    @test_inferred innersize(S)
    #    @test_noalloc inneraxes($S)

    #    @test inneraxes(S) == axes(first(S))
    #    @test_inferred inneraxes(S)
    #    @test_noalloc innersize($S)
    #end

    #@testset "UnsafeArrays" begin
    #    S, _, _= test_SNF()
    #    Sv = uview(S)
    #    @test parent(Sv) isa UnsafeArray{eltype(S.parent), ndims(S.parent)}
    #    @test S == Sv
    #end
end

end # module