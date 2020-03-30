module TestElasticArray

using Base.MultiplicativeInverses: SignedMultiplicativeInverse
using SpecialArrays: growlastdim!, shrinklastdim!, resizelastdim!

include("preamble.jl")

function makedata(T::Type, N::Integer)
    dims = testdims(N)
    kernel_size = front(dims)
    data = rand(prod(dims))::Vector
    return (
        M=N - 1,
        dims=dims,
        kernel_size=kernel_size,
        kernel_length = SignedMultiplicativeInverse(prod(kernel_size)),
        data = data,
        A = reshape(copy(data), dims),
    )
end


@testset "N = $N, T = $T" for N in 1:2, T in (Float64, )
    @testset "constructors" begin
        @unpack M, dims, kernel_size, kernel_length, data = makedata(T, N)

        @test ElasticArray{T,N,M}(kernel_size, data) isa ElasticArray{T,N,M}
        @test ElasticArray{T,N,M}(kernel_size, data).kernel_size === kernel_size
        @test ElasticArray{T,N,M}(kernel_size, data).kernel_length === kernel_length
        @test ElasticArray{T,N,M}(kernel_size, data).data === data

        @test ElasticArray{T,N}(kernel_size, data) isa ElasticArray{T,N,M}
        @test ElasticArray{T,N}(kernel_size, data).kernel_size === kernel_size
        @test ElasticArray{T,N}(kernel_size, data).kernel_length === kernel_length
        @test ElasticArray{T,N}(kernel_size, data).data === data

        @test ElasticArray{T}(kernel_size, data) isa ElasticArray{T,N,M}
        @test ElasticArray{T}(kernel_size, data).kernel_size === kernel_size
        @test ElasticArray{T}(kernel_size, data).kernel_length === kernel_length
        @test ElasticArray{T}(kernel_size, data).data === data
    end

    @testset "undef" begin
        @unpack M, dims, kernel_size, kernel_length, data = makedata(T, N)

        @test ElasticArray{T,N,M}(undef, dims) isa ElasticArray{T,N,M}
        @test ElasticArray{T,N,M}(undef, dims).kernel_size === kernel_size
        @test ElasticArray{T,N,M}(undef, dims).kernel_length === kernel_length

        @test ElasticArray{T,N}(undef, dims) isa ElasticArray{T,N,M}
        @test ElasticArray{T,N}(undef, dims).kernel_size === kernel_size
        @test ElasticArray{T,N}(undef, dims).kernel_length === kernel_length

        @test ElasticArray{T}(undef, dims) isa ElasticArray{T,N,M}
        @test ElasticArray{T}(undef, dims).kernel_size === kernel_size
        @test ElasticArray{T}(undef, dims).kernel_length === kernel_length


        @test ElasticArray{T,N,M}(undef, dims...) isa ElasticArray{T,N,M}
        @test ElasticArray{T,N,M}(undef, dims...).kernel_size === kernel_size
        @test ElasticArray{T,N,M}(undef, dims...).kernel_length === kernel_length

        @test ElasticArray{T,N}(undef, dims...) isa ElasticArray{T,N,M}
        @test ElasticArray{T,N}(undef, dims...).kernel_size === kernel_size
        @test ElasticArray{T,N}(undef, dims...).kernel_length === kernel_length

        @test ElasticArray{T}(undef, dims...) isa ElasticArray{T,N,M}
        @test ElasticArray{T}(undef, dims...).kernel_size === kernel_size
        @test ElasticArray{T}(undef, dims...).kernel_length === kernel_length
    end

    @testset "from array" begin
        @unpack M, dims, kernel_size, kernel_length, data, A = makedata(T, N)

        @test ElasticArray{T,N,M}(A) isa ElasticArray{T,N,M}
        @test ElasticArray{T,N,M}(A).kernel_size === kernel_size
        @test ElasticArray{T,N,M}(A).kernel_length === kernel_length
        @test ElasticArray{T,N,M}(A).data == vec(A)
        @test ElasticArray{T,N,M}(A).data !== A

        @test ElasticArray{T,N}(A) isa ElasticArray{T,N,M}
        @test ElasticArray{T,N}(A).kernel_size === kernel_size
        @test ElasticArray{T,N}(A).kernel_length === kernel_length
        @test ElasticArray{T,N}(A).data == vec(A)
        @test ElasticArray{T,N}(A).data !== A

        @test ElasticArray{T}(A) isa ElasticArray{T,N,M}
        @test ElasticArray{T}(A).kernel_size === kernel_size
        @test ElasticArray{T}(A).kernel_length === kernel_length
        @test ElasticArray{T}(A).data == vec(A)
        @test ElasticArray{T}(A).data !== A
    end

    @testset "convert" begin
        @unpack M, dims, kernel_size, kernel_length, data, A = makedata(T, N)

        @test convert(ElasticArray{T,N,M}, A) isa ElasticArray{T,N,M}
        @test convert(ElasticArray{T,N,M}, A).kernel_size === kernel_size
        @test convert(ElasticArray{T,N,M}, A).kernel_length === kernel_length
        @test convert(ElasticArray{T,N,M}, A).data == vec(A)
        @test convert(ElasticArray{T,N,M}, A).data !== A

        @test convert(ElasticArray{T,N}, A) isa ElasticArray{T,N,M}
        @test convert(ElasticArray{T,N}, A).kernel_size === kernel_size
        @test convert(ElasticArray{T,N}, A).kernel_length === kernel_length
        @test convert(ElasticArray{T,N}, A).data == vec(A)
        @test convert(ElasticArray{T,N}, A).data !== A

        @test convert(ElasticArray{T}, A) isa ElasticArray{T,N,M}
        @test convert(ElasticArray{T}, A).kernel_size === kernel_size
        @test convert(ElasticArray{T}, A).kernel_length === kernel_length
        @test convert(ElasticArray{T}, A).data == vec(A)
        @test convert(ElasticArray{T}, A).data !== A
    end

    @testset "misc" begin
        data = makedata(T, N)
        E = ElasticArray(data.kernel_size, data.data)
        @test dataids(E) === dataids(data.data)
    end

    @testset "resize!" begin
        function resize_test(delta::Integer)
            data = makedata(T, N)
            E = ElasticArray(data.kernel_size, data.data)

            new_size = (front(size(E))..., last(size(E)) + delta)
            cmp_idxs = (front(axes(E))..., 1:(last(size(E))+min(0, delta)))
            @test sizehint!(E, new_size...) === E
            @test resize!(E, new_size...) === E
            @test size(E) === new_size
            @test E[cmp_idxs...] == data.A[cmp_idxs...]
        end

        resize_test(0)
        resize_test(2)
        resize_test(-2)
    end

    @testset "append!/prepend!" begin
        let data = makedata(T, N)
            E = ElasticArray(data.kernel_size, data.data)

            dims = front(size(E))
            len_lastdim = last(size(E))
            V = Array{T,length(dims)}[]

            for i = 1:4
                push!(V, rand!(zeros(dims...)))
                append!(E, last(V))
            end

            @test size(E) == (dims..., len_lastdim + length(V))
            @test all(1:length(V)) do i
                selectdim(E, ndims(E), i + len_lastdim) == V[i]
            end
        end

        let data = makedata(T, N)
            E = ElasticArray(data.kernel_size, data.data)
            dims = front(size(E))
            len_lastdim = last(size(E))
            V = Array{T,length(dims)}[]

            for i = 1:4
                pushfirst!(V, rand!(zeros(dims...)))
                prepend!(E, first(V))
            end

            @test size(E) == (dims..., len_lastdim + length(V))
            @test all(1:length(V)) do i
                selectdim(E, ndims(E), i) == V[i]
            end
        end
    end

    let N = N, T = T
        test_array_AB() do
            data = makedata(T, N)
            A = ElasticArray(data.kernel_size, data.data)
            B = data.A
            return A, B
        end
    end

    @testset "growlastdim!, shrinklastdim!, resizelastdim!" begin
        let data = makedata(T, N)
            E = ElasticArray(data.kernel_size, data.data)
            dims, d = front(size(E)), last(size(E))
            growlastdim!(E, 2)
            @test size(E) == (dims..., d + 2)
        end
        let data = makedata(T, N)
            E = ElasticArray(data.kernel_size, data.data)
            dims, d = front(size(E)), last(size(E))
            shrinklastdim!(E, 2)
            @test size(E) == (dims..., d - 2)
        end
        let data = makedata(T, N)
            E = ElasticArray(data.kernel_size, data.data)
            dims, d = front(size(E)), last(size(E))
            resizelastdim!(E, 2)
            @test size(E) == (dims..., 2)
        end
    end
end

@testset "pointer/unsafe_convert" begin
    data = makedata(Float64, 2)
    E = ElasticArray(data.kernel_size, data.data)

    @test pointer(E) === pointer(parent(E))
    @test pointer(E, length(E)) === pointer(parent(E), length(E))
    @test unsafe_convert(Ptr{eltype(E)}, E) === unsafe_convert(Ptr{eltype(E)}, parent(E))
end

@testset "basic math" begin
    T = Float64

    E1 = rand!(ElasticArray{T}(undef, 9, 9))
    E2 = rand!(ElasticArray{T}(undef, 9, 9))
    E3 = rand!(ElasticArray{T}(undef, 9, 7))

    A1 = Array(E1)
    A2 = Array(E2)
    A3 = Array(E3)

    @test_inferred 2 * E1
    @test 2 * E1 isa ElasticArray{T,2,1}
    @test 2 * E1 == 2 * A1

    @test_inferred E1 .+ 2
    @test E1 .+ 2 isa ElasticArray{T,2,1}
    @test E1 .+ 2 == A1 .+ 2

    @test_inferred E1 + E2
    @test E1 + E2 isa ElasticArray{T,2,1}
    @test E1 + E2 == A1 + A2

    @test_inferred E1 * E2
    @test E1 * E2 isa ElasticArray{T,2,1}
    @test E1 * E2 == A1 * A2
    @test E1 * E3 == A1 * A3

    @test E1^3 == A1^3
    @test inv(E1) == inv(A1)
end

end # module