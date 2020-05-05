module TestSlicedArray



include("preamble.jl")


const TEST_ALONGS = [
    (True(), ),
    (False(), ),

    (True(), True()),
    (True(), False()),
    (False(), True()),
    (False(), False()),
]

function makedata(V::Type, alongs::NTuple{L,TypedBool}) where {L}
    psize = testdims(L)
    insize = tuple_getindex(psize, alongs)
    outsize = tuple_getindex(psize, invert(alongs))
    M = length(insize)
    N = length(outsize)
    alongs_int = tuple_getindex(ntuple(identity, L), alongs)

    flat = rand!(Array{V,L}(undef, psize...))

    nested = Array{Array{V,M},N}(undef, outsize...)
    i = 0
    Base.mapslices(flat, dims = alongs_int) do el
        i += 1 # TODO rand!
        nested[i] = zeros(V, insize...)
        nested[i] .= el
        el
    end

    return (
        alongs_int = alongs_int,
        alongs_glob = map(a -> a === True() ? Colon() : *, alongs),
        insize = insize,
        outsize = outsize,
        M = M,
        N = N,
        nested = nested,
        flat = flat,
    )
end


showalongs(alongs) = "($(join(map(a -> a === True() ? ':' : '*', alongs), ", ")))"
@testset "alongs = $(showalongs(alongs)), V = $V" for alongs in TEST_ALONGS, V in (Float64,)
    @testset "constructors" begin
        @unpack alongs_int, alongs_glob, flat, M, N = makedata(V, alongs)
        Expected = SlicedArray{<:AbstractArray{V,M},N,M,Array{V,M+N},typeof(alongs)}

        @test slice(flat, alongs) isa Expected
        @test_inferred slice(flat, alongs)
        @test slice(flat, alongs...) isa Expected
        @test_inferred slice(flat, alongs...)

        @test slice(flat, alongs_glob) isa Expected
        @test_inferred slice(flat, alongs_glob)
        @test slice(flat, alongs_glob...) isa Expected
        @test_inferred slice(flat, alongs_glob...)

        @test slice(flat, alongs_int) isa Expected
        @test slice(flat, alongs_int...) isa Expected
    end


    let V = V, alongs = alongs
        test_array_AB() do
            data = makedata(V, alongs)
            A = SlicedArray(data.flat, alongs)
            B = data.nested
            return A, B
        end
    end


    @testset "mapslices ($name)" for (name, f) in (
        ("identity", identity),
        ("sum", el -> sum(el)),
        ("reshape(..., reverse(dims))", el -> el isa AbstractArray ? reshape(el, reverse(size(el))) : el),
        ("reshape(..., Val(1))", el -> el isa AbstractArray ? reshape(el, Val(1)) : el),
    )
        data = makedata(V, alongs)
        B1 = mapslices(f, data.flat, dims = data.alongs_int)
        B2 = SpecialArrays.mapslices(f, data.flat, dims = data.alongs_int)
        @test B1 == B2
        @test_inferred SpecialArrays.mapslices(f, data.flat, dims = alongs)
    end

    continue
    #@testset "align" begin
    #    data = makedata(V, alongs)
    #    @test SpecialArrays.align(slice(data.flat, alongs), alongs) === data.flat
    #    @test SpecialArrays.align(data.nested, alongs) == data.flat
    #end

    @testset "extra" begin
        data = makedata(V, alongs)
        A = SlicedArray(data.flat, alongs)

        @test_inferred flatview(A)

        @test innersize(A) == size(first(A))
        @test_inferred innersize(A)
        @test_noalloc inneraxes($A)

        @test inneraxes(A) == axes(first(A))
        @test_inferred inneraxes(A)
        @test_noalloc innersize($A)
    end
end

#@testset "misc" begin
#    A = slice(rand(2, 3, 4), 1, 3)
#    @test parent(A) === A.parent
#    @test Base.dataids(A) === Base.dataids(A.parent)
#end

#@testset "UnsafeArrays" begin
#    A = slice(rand(2, 3, 4), 1, 3)
#    Av = uview(A)
#    @test parent(Av) isa UnsafeArray{eltype(A.parent),ndims(A.parent)}
#    @test A == Av
#end

# TODO
#Adapt.adapt_storage(::Type{<:CartesianIndexer}, A) = CartesianIndexer(A)
#@testset "Adapt" begin
#    A = slice(rand(2, 3, 4), 1, 3)
#    B = adapt(CartesianIndexer, A)
#    @test A == B
#    @test parent(B) isa CartesianIndexer
#end


end # module
