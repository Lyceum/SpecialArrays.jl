module TestBatchedVector

include("preamble.jl")

function makedata(V::Type, nbatches::Integer)
    batch_lengths = testdims(nbatches)
    offsets = [0, cumsum([batch_lengths...])...]
    nested = Vector{V}[rand(V, bl) for bl in batch_lengths]
    flat = nbatches == 0 ? V[] : reduce(vcat, nested)
    return (batch_lengths = batch_lengths, offsets = offsets, nested=nested, flat=flat)
end


@testset "nbatches = $nbatches, V = $V" for nbatches in (1, 10), V in (Float64, )
    @testset "constructors" begin
        @unpack batch_lengths, offsets, nested, flat = makedata(V, nbatches)
        Expected = BatchedVector{<:AbstractArray{V,innerndims(nested)},typeof(flat)}
        @test BatchedVector(flat, offsets) isa Expected
        @test_inferred BatchedVector(flat, offsets)
    end

    @testset "Extra" begin
        @unpack batch_lengths, offsets, nested, flat = makedata(V, nbatches)
        Expected = BatchedVector{<:AbstractArray{V,innerndims(nested)},typeof(flat)}
        @test batch(flat, batch_lengths) isa Expected
        @test_inferred batch(flat, batch_lengths)
        let B1 = batch(flat, batch_lengths)
            B2 = batchlike(copy(flat), B1)
            @test B1 == B2
            @test B1.parent !== B2.parent
        end
    end

    let nbatches = nbatches, V = V
        test_array_AB() do
            @unpack batch_lengths, nested, flat = makedata(V, nbatches)
            A = batch(flat, batch_lengths)
            B = nested
            return A, B
        end
    end
end

@testset "Misc" begin
    d = makedata(Float64, 5)
    B = BatchedVector(d.flat, d.offsets)
    @test Base.dataids(B) === (Base.dataids(d.flat)..., Base.dataids(d.offsets)...)
    @test parent(B) === d.flat
end

@testset "UnsafeArrays" begin
    d = makedata(Float64, 5)
    B = BatchedVector(d.flat, d.offsets)
    U = uview(B)
    @test U == B
    @test U.parent isa UnsafeArray{eltype(d.flat),1}
    @test U.offsets isa Vector{Int}
    @test_inferred uview(B)
end


end # module
