module TestSlicedArray

using SpecialArrays: along2string
using SpecialArrays: CartesianIndexer
using SpecialArrays: True, False, TypedBool, static_map, static_getindex, static_setindex


include("preamble.jl")


const TEST_ALONGS = [
    (True(), ),
    (False(), ),

    (True(), True()),
    (True(), False()),
    (False(), True()),
    (False(), False()),
]

function makedata(V::Type, al::TupleN{TypedBool})
    L = length(al)
    pdims = testdims(L)
    sdims = findall(al)
    innersize = static_getindex(pdims, al)
    outersize = static_getindex(pdims, static_map(!, al))
    M, N = length(innersize), length(outersize)

    flat = rand!(Array{V,L}(undef, pdims...))

    nested = Array{Array{V,M},N}(undef, outersize...)
    i = 0
    Base.mapslices(flat, dims = sdims) do el
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


showalongs(al) = "($(join(map(SpecialArrays.along2string, al), ", ")))"
@testset "al = $(showalongs(al)), V = $V" for al in TEST_ALONGS, V in (Float64,)
    @testset "constructors" begin
        @unpack flat, sdims, static_sdims, M, N = makedata(V, al)
        Expected = SlicedArray{<:AbsArr{V,M},N,M,Array{V,M + N},typeof(al)}

        @test typeof(SlicedArray(flat, al)) <: Expected
        @test_inferred SlicedArray(flat, al)

        @test typeof(slice(flat, static_sdims)) <: Expected
        @test_inferred slice(flat, static_sdims)
        @test typeof(slice(flat, static_sdims...)) <: Expected
        @test_inferred slice(flat, static_sdims...)

        @test typeof(slice(flat, sdims)) <: Expected
        @test typeof(slice(flat, sdims...)) <: Expected

        I = map(a -> a isa True ? Colon() : *, al)
        @test typeof(slice(flat, I...)) <: Expected
        @test_inferred slice(flat, I...)
    end

    let V = V, al = al
        test_array_AB() do
            data = makedata(V, al)
            A = SlicedArray(data.flat, al)
            B = data.nested
            return A, B
        end
    end

    @testset "mapslices ($name)" for (name, f) in (
        ("identity", identity),
        ("sum", el -> sum(el)),
        ("reshape(..., reverse(dims))", el -> el isa AbsArr ? reshape(el, reverse(size(el))) : el),
        ("reshape(..., Val(1))", el -> el isa AbsArr ? reshape(el, Val(1)) : el),
    )
        data = makedata(V, al)
        B1 = mapslices(f, data.flat, dims = data.sdims)
        B2 = SpecialArrays.mapslices(f, data.flat, dims = data.sdims)
        @test B1 == B2
        @test_inferred SpecialArrays.mapslices(f, data.flat, dims = data.static_sdims)
    end

    #@testset "align" begin
    #    data = makedata(V, al)
    #    @test SpecialArrays.align(slice(data.flat, al), al) === data.flat
    #    @test SpecialArrays.align(data.nested, al) == data.flat
    #end

    @testset "extra" begin
        data = makedata(V, al)
        A = SlicedArray(data.flat, al)

        @test_inferred flatview(A)

        @test innersize(A) == size(first(A))
        @test_inferred innersize(A)
        @test_noalloc inneraxes($A)

        @test inneraxes(A) == axes(first(A))
        @test_inferred inneraxes(A)
        @test_noalloc innersize($A)
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
    @test parent(Av) isa UnsafeArray{eltype(A.parent),ndims(A.parent)}
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
