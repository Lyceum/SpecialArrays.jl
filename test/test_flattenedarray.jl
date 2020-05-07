module TestFlattenedArray

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

@testset "M = $M, N = $N, V = $V" for M in (0, 2), N = 1:2, V in (Float64,)
    @testset "constructors" begin
        data = makedata(V, M, N)
        Expected = FlattenedArray{V,M+N,M,N,typeof(data.nested),typeof(data.inner_axes)}
        @test flatview(data.nested) isa Expected
        @test_inferred flatview(data.nested)

        @test flatview(data.flat) === data.flat
        @test flatview(data.nested) == data.flat
    end

    @testset "basic" begin
        data = makedata(V, M, N)
        F = flatview(data.nested)
        F[:] .= 1:length(F)
        len = prod(data.inner_size)
        for i = 1:length(data.nested)
            a = data.nested[i]
            offs = (i - 1) * len + 1
            @test F[offs:offs+len-1] == vec(data.nested[i])
        end
    end

    @testset "flatten" begin
        data = makedata(V, M, N)
        @test flatten(data.flat) === data.flat
        @test flatten(data.nested) == data.flat
        nested = copy(data.nested)
        flat = flatten(nested)
        rand!(flat)
        @test nested == data.nested
    end

    let V = V, M = M, N = N
        test_array_AB() do
            data = makedata(V, M, N)
            A = FlattenedArray(data.nested)
            B = data.flat
            return A, B
        end
    end
end

@testset "aliasing" begin
    x1 = rand(2, 3)
    x2 = rand(2, 3)
    x3 = rand(2, 3)
    F = flatview([x1, x2])

    @test dataids(F) === (dataids(F.parent)..., Iterators.flatten(dataids.(F.parent))...)

    @test mightalias(F, x1)
    @test !mightalias(F, rand(2, 3))

    @test mightalias(x1, F)
    @test !mightalias(rand(2, 3), F)

    @test mightalias(F, flatview([x1, x3]))
    @test !mightalias(F, flatview([x3, x3]))

    let F2 = unalias(x1, F)
        @test !mightalias(x1, F2)
        @test !mightalias(F2, x1)
        @test F2.parent == F.parent
        @test F2.parent !== F.parent
        @test all(zip(F2.parent, F.parent)) do (f2, f)
            f2 !== f
        end
    end
end


end # module
