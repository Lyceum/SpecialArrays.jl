module FlattenedArrayTest

include("preamble.jl")

function makedata(V::Type, M::Integer, N::Integer)
    dims = testdims(M + N)
    innersize, outersize = tuple_split(dims, M)

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

@testset "M = $M, N = $N, V = $V" for M = 1:2, N = 1:2, V in (Float64,)
    @testset "constructors" begin
        data = makedata(V, M, N)
        Expected = FlattenedArray{V,M + N,M,typeof(data.nested),typeof(data.inneraxes)}

        @test typeof(Expected(data.nested, data.inneraxes)) == Expected
        @test_inferred Expected(data.nested, data.inneraxes)

        @test typeof(FlattenedArray(data.nested)) == Expected
        @test_inferred FlattenedArray(data.nested)

        @test typeof(FlattenedArray(data.nested, data.inneraxes)) == Expected
        @test_inferred FlattenedArray(data.nested, data.inneraxes)

        @test flatten(data.nested) === FlattenedArray(data.nested)
    end

    let V = V, M = M, N = N
        test_array_AB() do
            data = makedata(V, M, N)
            A = FlattenedArray(data.nested)
            B = data.flat
            return A, B
        end
    end

    @testset "extra" begin
        data = makedata(V, M, N)
        F = flatten(data.nested)
        @test F.inneraxes === inneraxes(F) === inneraxes(data.nested)
        @test map(length, F.inneraxes) === innersize(F) === innersize(data.nested)
    end
end

@testset "aliasing" begin
    x1 = rand(2, 3)
    x2 = rand(2, 3)
    x3 = rand(2, 3)
    F = flatten([x1, x2])

    @test dataids(F) === (dataids(F.parent)..., Iterators.flatten(dataids.(F.parent))...)

    @test mightalias(F, x1)
    @test !mightalias(F, rand(2, 3))
    @test mightalias(x1, F)
    @test !mightalias(rand(2, 3), F)
    @test mightalias(F, flatten([x1, x3]))
    @test !mightalias(F, flatten([x3, x3]))

    let F2 = unalias(x1, F)
        @test F2.parent[1] !== x1 && F2.parent[1] == x1
        @test F2.parent[2] === x2
        @test F2 == F
        @test !(mightalias(x1, F2) && mightalias(F2, x1))
    end
end

@testset "Zygote" begin
    data = makedata(Float64, 1, 1)
    x = rand!(zeros(last(data.outersize)))
    g1 = Zygote.gradient(x -> sum(hcat(data.nested...) * x), x)
    g2 = Zygote.gradient(x -> sum(flatten(data.nested) * x), x)
    @test g1 == g2
    @test_skip @test_inferred Zygote.gradient(A -> sum(flatten(A) * x), data.nested)
end

end # module
