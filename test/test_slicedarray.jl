module TestSlices

using SpecialArrays: along2string

include("preamble.jl")

const TEST_ALONGS = [
    (static(true), ),
    (static(false), ),

    (static(true), static(true)),
    (static(false), static(true)),
    (static(false), static(false)),
]

slicedims(al::TupleN{SBool}) = Tuple(i for i=1:length(al) if unstatic(al[i]))

function makedata(V::Type, al::TupleN{SBool})
    L = length(al)
    pdims = testdims(L)
    sdims = slicedims(al)
    innersize = Tuple(pdims[i] for i in 1:L if unstatic(al[i]))
    outersize = Tuple(pdims[i] for i in 1:L if !unstatic(al[i]))
    M, N = length(innersize), length(outersize)

    flat = rand!(Array{V,L}(undef, pdims...))

    nested = Array{Array{V,M},N}(undef, outersize...)
    i = 0
    Base.mapslices(flat, dims=sdims) do el
        i += 1
        nested[i] = zeros(V, innersize...)
        nested[i] .= el
        el
    end

    return (
        nested = nested,
        flat = flat,
        pdims = pdims,
        sdims = sdims,
        static_sdims = map(static, sdims),
        innersize = innersize,
        outersize = outersize,
        inneraxes = axes(first(nested)),
        outeraxes = axes(nested),
        M = M,
        N = N,
    )
end

showalongs(al) = "($(join(map(SpecialArrays.along2string, al), ',')),)"


@testset "al = $(showalongs(al)), V = $V" for al in TEST_ALONGS, V in (Float64, Int)
    @testset "constructors" begin
        @unpack flat, sdims, static_sdims, M, N = makedata(V, al)
        Expected = SlicedArray{<:AbsArr{V,M},N,M,Array{V,M+N},typeof(al)}

        @test typeof(SlicedArray(flat, al)) <: Expected
        @test_inferred SlicedArray(flat, al)

        @test typeof(slice(flat, static_sdims)) <: Expected
        @test_inferred slice(flat, static_sdims)
        @test typeof(slice(flat, static_sdims...)) <: Expected
        @test_inferred slice(flat, static_sdims...)

        @test typeof(slice(flat, sdims)) <: Expected
        @test typeof(slice(flat, sdims...)) <: Expected

        I = map(a -> a isa STrue ? Colon() : *, al)
        @test typeof(slice(flat, I...)) <: Expected
        @test_inferred slice(flat, I...)
    end

    let V=V, al=al
        test_array_AB() do
            data = makedata(V, al)
            A = SlicedArray(data.flat, al)
            B = data.nested
            return A, B
        end
    end

    @testset "extra" begin
        data = makedata(V, al)
        A = SlicedArray(data.flat, al)
        @test begin
            flattened = flatten(A)
            flattened !== A.parent && flattened == A.parent
        end
        @test_inferred flatten(A)

        @test flatview(A) === A.parent
        @test_inferred flatview(A)

        @test innersize(A) == size(first(A))
        @test_inferred innersize(A)
        @test_noalloc inneraxes($A)

        @test inneraxes(A) == axes(first(A))
        @test_inferred inneraxes(A)
        @test_noalloc innersize($A)
    end
end

@testset "slice w/ reshape" begin
    flat = rand(2, 3, 4)
    let I = (:, *, :, *, :)
        A = slice(flat, I...)
        @test A isa SlicedArray{<:AbsArr{Float64,3},2,3,Array{Float64,5},typeof(A.alongs)}
        @test_inferred slice(flat, I...)
    end
    let I = (:, *)
        A = slice(flat, I...)
        @test A isa SlicedArray{<:AbsArr{Float64,1},1,1,Array{Float64,2},typeof(A.alongs)}
        @test_inferred slice(flat, I...)
    end
end

@testset "misc" begin
    A = slice(rand(2, 3, 4), 1, 3)
    @test parent(A) === A.parent
    @test Base.dataids(A) === Base.dataids(A.parent)
end

@testset "UnsafeArrays" begin
    A = slice(rand(2, 3, 4), 1, 3)
    Av = uview(A)
    @test parent(Av) isa UnsafeArray{eltype(A.parent), ndims(A.parent)}
    @test A == Av
end

Adapt.adapt_storage(::Type{<:CartesianIndexer}, A) = CartesianIndexer(A)
@testset "Adapt" begin
    A = slice(rand(2, 3, 4), 1, 3)
    B = adapt(CartesianIndexer, A)
    @test A == B
    @test parent(B) isa CartesianIndexer
end

end # module