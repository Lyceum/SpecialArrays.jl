testdims(L::Integer) = ntuple(i -> 3 + i, Val(L))

randlike(x::Number) = rand(typeof(x))
randlike(A::AbstractArray{<:Number}) = rand(eltype(A), size(A)...)
randlike(A::AbstractArray{<:Number,0}) = rand!(zeros(eltype(A)))

function randlike(A::AbstractArray{<:AbstractArray{V,M},N}) where {V,M,N}
    B = Array{Array{V,M},N}(undef, size(A)...)
    for I in eachindex(B)
        B[I] = randlike(A[I])
    end
    return B
end

function randlike!(A::AbstractArray{<:AbstractArray})
    for I in eachindex(A)
        copyto!(A[I], randlike(A[I]))
    end
    return A
end

function randlike!(A::AbstractArray{<:Number})
    for I in eachindex(A)
        A[I] = randlike(A[I])
    end
    return A
end


macro qe(ex)
    Expr(:quote, MacroTools.striplines(esc(ex)))
end

macro test_inferred(ex)
    @qe begin
        $Test.@test (($Test.@inferred $ex); true)
    end
end

macro test_noalloc(ex)
    @qe begin
        local nbytes = $memory($BenchmarkTools.@benchmark $ex samples = 1 evals = 1)
        $iszero(nbytes) ? true : $error("Allocated $(BenchmarkTools.prettymemory(nbytes))")
    end
end

macro test_getindex(A, I)
    @qe begin
        if $I isa $Tuple
            @test_inferred $A[$I...]
            if $index_dimsum($I...) == ()
                $Test.@test $typeof($A[$I...]) == $eltype($A)
            else
                $Test.@test $size($A[$I...]) == $map($length, $index_shape($to_indices($A, $I)...))
                $Test.@test $eltype($A[$I...]) == $eltype($A)
            end
        else
            @test_inferred $A[$I]
            if $index_dimsum($I) == () && !($I isa AbstractArray{<:Any,0})
                $Test.@test $typeof($A[$I]) == $eltype($A)
            else
                $Test.@test $size($A[$I]) == $map($length, $index_shape($to_indices($A, ($I,))...))
                $Test.@test $eltype($A[$I]) == $eltype($A)
            end
        end
    end
end

macro test_setindex!(A, I)
    @qe begin
        if $I isa $Tuple
            local x = $randlike($A[$I...])
            @test_inferred $setindex!($A, x, $I...)
            $Test.@test $setindex!($A, x, $I...) === $A
            $Test.@test $A[$I...] == x
        else
            local x = $randlike($A[$I])
            @test_inferred $setindex!($A, x, $I)
            $Test.@test $setindex!($A, x, $I) === $A
            $Test.@test $A[$I] == x
        end
    end
end

macro test_getindices(A, Is)
    @qe begin
        $Test.@test $all($Is) do I
            if I isa $Tuple
                if $index_dimsum(I...) == ()
                    $typeof($A[I...]) == $eltype($A)
                else
                    $eltype($A[I...]) == $eltype($A) &&
                    $size($A[I...]) == $map($length, $index_shape($to_indices($A, I)...))
                end
            else
                #if $index_dimsum(I) == ()
                if $index_dimsum(I) == () && !(I isa AbstractArray{<:Any,0})
                    $typeof($A[I]) == $eltype($A)
                else
                    $eltype($A[I]) == $eltype($A) &&
                    $size($A[I]) == $map($length, $index_shape($to_indices($A, (I,))...))
                end
            end
        end
    end
end

macro test_setindices!(A, Is)
    @qe begin
        $Test.@test $all($Is) do I
            if I isa $Tuple
                x = $randlike($A[I...])
                $A[I...] != x && $setindex!($A, x, I...) === $A && $A[I...] == x
            else
                x = $randlike($A[I])
                $A[I] != x && $setindex!($A, x, I) === $A && $A[I] == x
            end
        end
    end
end

macro test_all_equal(A, B, Is)
    @qe begin
        $Test.@test all($Is) do I
            if I isa Tuple
                $A[I...] == $B[I...] #&& typeof($A[I...]) == typeof($B[I...])
            else
                $A[I] == $B[I] #&& typeof($A[I]) == typeof($B[I])
            end
        end
    end
end


macro test_copyto!(dest, src)
    @qe begin
        $randlike!($dest)
        @test_inferred $copyto!($dest, $src)
        $Test.@test $copyto!($dest, $src) === $dest
        $Test.@test $all(I -> $dest[I] == $src[I], $eachindex($dest, $src))
        $Test.@test $dest == $src

        if $length($dest) > 2
            $randlike!($dest)
            @test_inferred $copyto!($dest, 2, $src, 2, 1)
            $Test.@test $copyto!($dest, 2, $src, 2, 1) === $dest
            $Test.@test $dest[2] == $src[2] && $dest[1] != $src[1] && $dest[3:end] != $src[3:end]
        else
            $Test.@test_skip @test_inferred $copyto!($dest, 2, $src, 2, 1)
            $Test.@test_skip $copyto!($dest, 2, $src, 2, 1) === $dest
            $Test.@test_skip $dest[2] == $src[2] &&
                             $dest[1] != $src[1] &&
                             $dest[3:end] != $src[3:end]
        end
    end
end

macro test_array_attributes(A)
    @qe begin
        $Test.@test $eltype($A) === $typeof($first($A))
        $Test.@test $ndims($A) == $length($axes($A))
        $Test.@test $size($A) == $map($length, $axes($A))
        $Test.@test $length($A) == $prod($size($A))

        $Test.@test $all(1:$ndims($A)) do dim
            $size($A)[dim] == $size($A, dim)
        end
        $Test.@test $size($A, $ndims($A) + 1) == 1

        $Test.@test $all(1:$ndims($A)) do dim
            $axes($A)[dim] == $axes($A, dim)
        end
        $Test.@test $axes($A, $ndims($A) + 1) == $Base.OneTo(1)

        @test_inferred $eltype($A)
        @test_inferred $ndims($A)
        @test_inferred $axes($A)
        @test_inferred $size($A)
        @test_inferred $length($A)
    end
end

macro test_similar(A, T, dims)
    @qe begin
        @test_inferred $similar($A)
        $Test.@test $eltype($similar($A)) === $eltype($A)
        $Test.@test $size($similar($A)) == $size($A)

        @test_inferred $similar($A, $T)
        $Test.@test $eltype($similar($A, $T)) === $T
        $Test.@test $size($similar($A, $T)) == $size($A)

        @test_inferred $similar($A, $dims)
        $Test.@test eltype($similar($A, $dims)) === $eltype($A)
        $Test.@test size($similar($A, $dims)) == $dims

        @test_inferred $similar($A, $T, $dims)
        $Test.@test eltype($similar($A, $T, $dims)) === $T
        $Test.@test size($similar($A, $T, $dims)) == $dims
    end
end


function test_indexing_AB(AB::Function)
    @testset "LinearIndices" begin
        let (A, _) = AB()
            @test_getindex A first(LinearIndices(A))
            @test_setindex! A first(LinearIndices(A))
        end
        let (A, B) = AB()
            @test LinearIndices(A) === LinearIndices(B)
            @test_all_equal A B LinearIndices(A)
            @test_getindices A LinearIndices(A)
            @test_setindices! A LinearIndices(A)
        end
        let (A, B) = AB()
            @test A[LinearIndices(A)] == B[LinearIndices(A)]
            @test_getindex A LinearIndices(A)
            @test_setindex! A LinearIndices(A)
        end
        let (A, B) = AB()
            @test begin
                I = hcat(LinearIndices(A), LinearIndices(A))
                A[I] == B[I]
            end
            @test_getindex A hcat(LinearIndices(A), LinearIndices(A))
            #@test_setindex! A hcat(LinearIndices(A), LinearIndices(A))
        end
    end
    @testset "CartesianIndices" begin
        let (A, _) = AB()
            @test_getindex A first(CartesianIndices(A))
            @test_setindex! A first(CartesianIndices(A))
        end
        let (A, B) = AB()
            @test CartesianIndices(A) === CartesianIndices(B)
            @test_all_equal A B CartesianIndices(A)
            @test_getindices A CartesianIndices(A)
            @test_setindices! A CartesianIndices(A)
        end
        let (A, B) = AB()
            @test A[CartesianIndices(A)] == B[CartesianIndices(A)]
            @test_getindex A CartesianIndices(A)
            @test_setindex! A CartesianIndices(A)
        end
        let (A, B) = AB()
            @test begin
                I = hcat(CartesianIndices(A), CartesianIndices(A))
                A[I] == B[I]
            end
            @test_getindex A hcat(CartesianIndices(A), CartesianIndices(A))
            #@test_setindex! A CartesianIndices(A)
        end
    end
    @testset "trailing singleton" begin
        let (A, _) = AB()
            @test_getindex A (Tuple(first(CartesianIndices(A)))..., 1)
            @test_setindex! A (Tuple(first(CartesianIndices(A)))..., 1)
        end
        let (A, B) = AB()
            @test_all_equal A B [(Tuple(I)..., 1) for I in CartesianIndices(A)]
            @test_getindices A [(Tuple(I)..., 1) for I in CartesianIndices(A)]
            @test_setindices! A [(Tuple(I)..., 1) for I in CartesianIndices(A)]
        end
    end
    @testset "trailing colon" begin
        let (A, _) = AB()
            @test_getindex A (Tuple(first(CartesianIndices(A)))..., :)
            @test_setindex! A (Tuple(first(CartesianIndices(A)))..., :)
        end
        let (A, B) = AB()
            @test_all_equal A B [(Tuple(I)..., :) for I in CartesianIndices(A)]
            @test_getindices A [(Tuple(I)..., :) for I in CartesianIndices(A)]
            @test_setindices! A [(Tuple(I)..., :) for I in CartesianIndices(A)]
        end
    end
    @testset "dropped singleton" begin
        _, B = AB()
        if ndims(B) > 0 && size(B, ndims(B)) == 1
            let (A, _) = AB()
                @test_getindex A front(Tuple(first(CartesianIndices(A))))
                @test_setindex! A front(Tuple(first(CartesianIndices(A))))
            end
            let (A, B) = AB()
                @test_all_equal A B [front(Tuple(I)) for I in CartesianIndices(A)]
                @test_getindices A [front(Tuple(I)) for I in CartesianIndices(A)]
                @test_setindices! A [front(Tuple(I)) for I in CartesianIndices(A)]
            end
        end
    end
    @testset "logical" begin
        let (A, B) = AB()
            @test begin
                I = map(isodd, LinearIndices(A))
                A[I] == B[I]
            end
            @test_getindex A map(isodd, LinearIndices(A))
            @test_setindex! A map(isodd, LinearIndices(A))
        end
        let (A, B) = AB()
            @test begin
                I = Tuple(map(isodd, ax) for ax in axes(A))
                A[I...] == B[I...]
            end
            @test_getindex A Tuple(map(isodd, ax) for ax in axes(A))
            @test_setindex! A Tuple(map(isodd, ax) for ax in axes(A))
        end
    end
    @testset "single colon" begin
        A, B = AB()
        @test A[:] == B[:]
    end
end


function test_array_AB(AB::Function)
    @testset "array attributes" begin
        A, _ = AB()
        @test_array_attributes A

        @test_noalloc eltype($A)
        @test_noalloc ndims($A)
        @test_noalloc axes($A)
        @test_noalloc size($A)
        @test_noalloc length($A)
    end

    @testset "indexing" begin
        test_indexing_AB(AB)
    end

    @testset "copy/copyto!/equality" begin
        let (A1, _) = AB()
            A2 = copy(A1)
            @test A1 == A2
            randlike!(A1)
            @test A1 != A2
        end
        let (A, B) = AB()
            @test_copyto! A B
        end
        let (A, B) = AB()
            @test_copyto! B A
        end
        let A1 = first(AB()), A2 = first(AB())
            @test_copyto! A1 A2
        end
    end
end
