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

    parent = rand!(Array{V,L}(undef, psize...))

    nested = Array{Array{V,M},N}(undef, outsize...)
    i = 0
    Base.mapslices(parent, dims = alongs_int) do el
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
        parent = parent,
    )
end


showalongs(alongs) = "($(join(map(a -> a === True() ? ':' : '*', alongs), ", ")))"

@testset "alongs = $(showalongs(alongs)), V = $V" for alongs in TEST_ALONGS, V in (Float64,)
    @testset "constructors" begin
        @unpack alongs_int, alongs_glob, parent, M, N = makedata(V, alongs)
        Expected = SlicedArray{<:AbstractArray{V,M},N,M,Array{V,M+N},typeof(alongs)}

        @test slice(parent, alongs) isa Expected
        @test_inferred slice(parent, alongs)
        @test slice(parent, alongs...) isa Expected
        @test_inferred slice(parent, alongs...)

        @test slice(parent, alongs_glob) isa Expected
        @test_inferred slice(parent, alongs_glob)
        @test slice(parent, alongs_glob...) isa Expected
        @test_inferred slice(parent, alongs_glob...)

        @test slice(parent, alongs_int) isa Expected
        @test slice(parent, alongs_int...) isa Expected
    end

    @testset "IndexStyle" begin
        S = slice(makedata(V, alongs).parent, alongs)
        if ndims(S) == 1
            @test IndexStyle(typeof(S)) === IndexLinear()
        else
            @test IndexStyle(typeof(S)) === IndexCartesian()
        end
    end

    let V = V, alongs = alongs
        test_array_AB() do
            data = makedata(V, alongs)
            A = SlicedArray(data.parent, alongs)
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
        B1 = mapslices(f, data.parent, dims = data.alongs_int)
        B2 = SpecialArrays.mapslices(f, data.parent, dims = data.alongs_int)
        @test B1 == B2
        @test_inferred SpecialArrays.mapslices(f, data.parent, dims = alongs)
    end

    @testset "align" begin
        data = makedata(V, alongs)
        @test SpecialArrays.align(slice(data.parent, alongs), alongs) === data.parent
        @test SpecialArrays.align(data.nested, alongs) == data.parent
    end

    @testset "flatview" begin
        @unpack parent, nested = makedata(V, alongs)
        S = slice(parent, alongs)
        if SpecialArrays.iscontiguous(alongs)
            @test flatview(S) === S.parent === parent
        else
            flat = reshape(reduce(hcat, nested), (inner_size(nested)..., size(nested)...))
            @test flatview(S) == flat
        end
    end

    @testset "inner_*" begin
        data = makedata(V, alongs)
        S = SlicedArray(data.parent, alongs)

        @test inner_size(S) == size(first(S))
        @test_inferred inner_size(S)
        @test_noalloc inner_size($S)

        @test inner_axes(S) == axes(first(S))
        @test_inferred inner_axes(S)
        @test_noalloc inner_axes($S)
    end
end

@testset "ContiguousSlicedVector ($(showalongs(alongs))" for alongs in (
    (False(),),
    (True(), False()),
    (True(), True(), False()),
)
    V = Float64

    # TODO we define resize!/sizehint! for all SlicedArrays, not just ContiguousSlicedVector,
    # but ElasticArray only supports resizing along the last dimension, so that's all we
    # (currently) test for.
    @testset "resize!/empty!" begin
        data = makedata(V, alongs)
        S = slice(ElasticArray(data.parent), alongs)
        len = length(S)
        @test empty!(S) === S
        @test length(S) == 0
        @test resize!(S, len + 1) === S
        @test length(S) == len + 1
    end
    @testset "sizehint!" begin
        data = makedata(V, alongs)
        S = slice(ElasticArray(data.parent), alongs)
        @test sizehint!(S, length(S) + 1) === S
    end

    @testset "append!" begin
        data = makedata(V, alongs)
        S = empty!(slice(ElasticArray(data.parent), alongs))
        src1 = slice(rand!(ElasticArray(copy(data.parent))), alongs)
        src2 = slice(rand!(ElasticArray(copy(data.parent))), alongs)
        @test append!(S, src1) === S
        @test append!(S, src2) === S
        l = length(src1)
        @test S[1:l] == src1
        @test S[l+1:end] == src2
    end
    @testset "prepend!" begin
        data = makedata(V, alongs)
        S = empty!(slice(ElasticArray(data.parent), alongs))
        src1 = slice(rand!(ElasticArray(copy(data.parent))), alongs)
        src2 = slice(rand!(ElasticArray(copy(data.parent))), alongs)
        @test prepend!(S, src1) === S
        @test prepend!(S, src2) === S
        l = length(src1)
        @test S[1:l] == src2
        @test S[l+1:end] == src1
    end
    @testset "pop!" begin
        data = makedata(V, alongs)
        S = slice(ElasticArray(data.parent), alongs)
        xs = Array{V,data.M}[]
        @test all(1:length(S)) do _
            x = pop!(S)
            pushfirst!(xs, x)
            !(x isa SubArray)
        end
        @test xs == data.nested
    end
    @testset "push!" begin
        data = makedata(V, alongs)
        S = slice(ElasticArray(data.parent), alongs)
        empty!(S)
        @test all(data.nested) do x
            push!(S, x) === S
        end
        @test S == data.nested
    end
    @testset "pushfirst!" begin
        data = makedata(V, alongs)
        data.parent[:] .= 1:length(data.parent)
        S = slice(ElasticArray(data.parent), alongs)
        empty!(S)
        @test all(data.nested) do x
            pushfirst!(S, x) === S
        end
        @test S == reverse(data.nested)
    end
end

@testset "parent/dataids" begin
    S = slice(rand(2, 3, 4), 1, 3)
    @test parent(S) === S.parent
    @test Base.dataids(S) === Base.dataids(S.parent)
end

@testset "UnsafeArrays" begin
    S = slice(rand(2, 3, 4), 1, 3)
    Sv = uview(S)
    @test parent(Sv) isa UnsafeArray{eltype(S.parent),ndims(S.parent)}
    @test S == Sv
end

@testset "slice(::SlicedArray, ...)" begin
    A = rand(2, 3, 4)
    S1 = slice(A, *, :, :)
    S2 = slice(slice(A, *, :, *), *, :)
    @test S1 == S2
    @test S1.parent === S2.parent
end


# TODO Adapt.adapt_storage

end # module
