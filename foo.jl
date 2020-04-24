using ElasticArrays: ElasticArray

using SpecialArrays
using SpecialArrays: True, False, TypedBool, static_map, front, tail, iscontiguous
using BenchmarkTools
using LyceumCore
using Random
using InteractiveUtils

function comp(A, B, B0, I, J)
    for (i, j) in zip(I, J)
        @assert A[i] == B[j] (i, j)
    end
    for i in eachindex(A)
        i in I || @assert all(isequal(69), A[i]) i
    end
    for j in eachindex(B)
        j in J || @assert B[j] == B0[j]
    end
end
iscont(S::SlicedArray{T,N,M,P,A,Fast}) where {T,N,M,P,A,Fast} = Fast



if false
S = slice(copy(flat), alongs)
@info "Size: $(size(S))"

@info "----1----"
S1 = slice(fill!(similar(flat), 69), alongs)
S2 = slice(copy(flat), alongs)
copyto!(S1, 1, S2, 1, length(S2))
comp(S1, S2, S, 1:length(S2), 1:length(S1))

@info "----2----"
S1 = slice(fill!(similar(flat), 69), alongs)
S2 = slice(copy(flat), alongs)
copyto!(S1, 1, S2, 2, 2)
comp(S1, S2, S, 1:2, 2:3)

@info "----3----"
S1 = slice(fill!(similar(flat), 69), alongs)
S2 = slice(copy(flat), alongs)
copyto!(S1, 2, S2, 4, 4)
comp(S1, S2, S, 2:5, 4:7)

@info "----4----"
d=(2,3,8)
flat = reshape(collect(1:prod(d)), d)
alongs = (:,:,*)
S1 = slice(fill!(similar(flat), 69), alongs)

d=(2,3,8,2)
flat = reshape(collect(1:prod(d)), d)
alongs = (:,:,*,*)
S = slice(copy(flat), alongs)
S2 = slice(copy(flat), alongs)
copyto!(S1, 2, S2, 4, 4)
comp(S1, S2, S, 2:5, 4:7)
@info "END"


flat = rand(2,3,4)
S = slice(ElasticArray(copy(flat)), :, :, *)
@info size(S)
x = Array(first(S))
push!(S, x)
@info size(S)
append!(S, [x])
@info size(S)
append!(S, [x,x])
@info size(S)

@info "---1---"
S = slice(ElasticArray(rand(2,3)), :, *)
x = Array(first(S))
@info size(S)
push!(S, x)
@info size(S)

@info "---2---"
S = slice(ElasticArray(rand(2,3)), :, *)
S2 = slice(ElasticArray(rand(2,3)), :, *)
@info size(S)
append!(S, S2)
@info size(S)
@assert S[4:6] == S2

@info "---3---"
S = slice(ElasticArray(reshape(collect(1:6), (2, 3))), :, *)
S2 = slice(ElasticArray(reshape(collect(7:30), (2, 3, 4))), :, *, *)
@info size(S)
append!(S, S2)
@info size(S)
@assert S[4:15] == S2[:]

@info "---4---"
S = slice(ElasticArray(reshape(collect(1:6), (2, 3))), :, *)
S2 = slice(ElasticArray(reshape(collect(7:30), (2, 3, 4))), :, *, *)
@info size(S)
prepend!(S, S2)
@info size(S)
@assert S[1:12] == S2[:]

@info "---5---"
#S = slice(rand(10,20,30), :, :, *)
#S2 = slice(rand(10,20,30), :, :, *)
#@btime copyto!($S, 1, $S2, 1, $(length(S2)))
#@btime SpecialArrays.mycopyto!($S, 1, $S2, 1, $(length(S2)))

@info "---5---"
S = slice(ElasticArray(rand(2,3,4,5)), :,:,*,*)
S2 = slice(ElasticArray(rand(2,3,4,5)), :,:,*,*)
@info size(S)
prepend!(S, S2)
@info size(S)
@assert S[1:20] == S2[:]

@info "---6---"
S = slice(ElasticArray(rand(2,3,4,5)), :,:,*,*)
S2 = slice(ElasticArray(rand(2,3,4,5)), :,:,*,*)
@info size(S)
append!(S, S2)
@info size(S)
@assert S[21:end] == S2[:]

end
#flat = rand(2,3,100)
#S1 = slice(copy(flat), alongs)
#S2 = slice(rand!(similar(flat)), alongs)
#@btime copyto!($S1, 1, $S2, 1, length($S1))
#@btime SpecialArrays.mycopyto!($S1, 1, $S2, 1, length($S1))



#@assert iscontiguous(())
#@assert iscontiguous((True(),))
#@assert iscontiguous((False(),))
#@assert iscontiguous((True(), True()))
#@assert iscontiguous((True(), False()))
#@assert !iscontiguous((False(), True()))
#@assert iscontiguous((False(), False()))
#@assert iscontiguous((True(), True(), True()))
#@assert !iscontiguous((False(), True(), True()))
#@assert !iscontiguous((True(), False(), True()))
#@assert iscontiguous((True(), True(), False()))
#@assert iscontiguous((True(), False(), False()))
#@assert !iscontiguous((False(), True(), False()))
#@assert !iscontiguous((False(), False(), True()))
#@assert iscontiguous((False(), False(), False()))


nothing
