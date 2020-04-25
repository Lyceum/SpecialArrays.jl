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



module M

using Base: @pure, tail, front
using SpecialArrays
using SpecialArrays: True, False, TypedBool, static_map, front, tail, iscontiguous
using BenchmarkTools
using LyceumCore
using Random
using InteractiveUtils


struct BoolIndex{N,I}
    function BoolIndex{N,I}() where {N,I}
        new{N::Int,I::NTuple{N,Bool}}()
    end
end
@pure BoolIndex(I::NTuple{N,Bool}) where {N} = BoolIndex{N,I}()

Base.length(::Type{<:BoolIndex{N}}) where {N} = N
Base.Tuple(::Type{<:BoolIndex{N,I}}) where {N,I} = I

Base.getindex(I::BoolIndex, i::Integer) = getindex(Tuple(I), i)
Base.setindex(I::BoolIndex, v::Bool, i::Integer) = setindex(Tuple(I), v, i)

for f in (:first, :last)
    @eval $Base.$f(::Type{T}) where {T<:$BoolIndex} = $Base.$f($Tuple(T))
end
for f in (:tail, :front)
    @eval @pure $Base.$f(::Type{T}) where {T<:$BoolIndex} = $BoolIndex($Base.$f($Tuple(T)))
end
for f in (:length, :Tuple, :tail, :front, :first, :last)
    @eval @pure $Base.$f(I::$BoolIndex) = $Base.$f(typeof(I))
end

@inline function Base.iterate(I::BoolIndex, i::Int=1)
    iterate(typeof(I), i)
end
#@inline function Base.iterate(::Type{I}, i::Int=1) where {I<:BoolIndex}
@inline function Base.iterate(I::BoolIndex, i::Int=1)
    return (1 <= i <= length(I)) ? (@inbounds I[i], i + 1) : nothing
end


@pure Base.getindex(xs::NTuple{N,Any}, I::BoolIndex{N}) where {N} = _getindex(xs, I)
@inline function _getindex(xs::Tuple, I::BoolIndex)
    rest  = _getindex(tail(xs), tail(I))
    #return first(I) ? (first(xs), rest...) : rest
    return first(I) === true ? (first(xs), rest...) : rest
end
_getindex(::Tuple{}, ::BoolIndex{0}) = ()

@inline function Base.setindex(t::NTuple{N,Any}, v::Tuple, I::BoolIndex{N}) where {N}
    _setindex(t, v, I, I[I])
end
function _setindex(t::Tuple, v::Tuple, I::BoolIndex, ::Tuple)
    throw(DimensionMismatch("Number of values provided does not match number of indices"))
end
@pure function _setindex(t::Tuple, v::NTuple{M,Any}, I::BoolIndex, ::NTuple{M,Any}) where {M}
    __setindex(t, v, I)
end
@inline function __setindex(t::Tuple, v::Tuple, I::BoolIndex)
    if first(I) === True()
        (first(v), __setindex(tail(t), tail(v), tail(I))...)
    else
        (first(t), __setindex(tail(t), v, tail(I))...)
    end
end
# proper termination
__setindex(t::Tuple{}, v::Tuple{}, I::BoolIndex{0}) = ()
__setindex(t::Tuple, v::Tuple{}, I::BoolIndex) = t

@inline function bam(A::AbstractArray{<:Any,N}, alongs::NTuple{N,Bool}) where {N}
    bam(A, BoolIndex(alongs))
end

@inline function bam(A::AbstractArray{<:Any,N}, alongs::BoolIndex{N}) where {N}
    a = ntuple(i -> alongs[i] ? True() : False(), Val(N))
    slice(A, a)
end

end # module



#I = M.BoolIndex((true,false,true))
n = 15
I = M.BoolIndex(ntuple(i -> isodd(i), n))
t = ntuple(identity, n)
v = Tuple(i for i in t if isodd(i))
#@btime $t[$I]
function fuck()
    x = rand(ntuple(identity, Val(8))...)
    #I = (true,false)
    I = ntuple(isodd, Val(8))
    #I = M.BoolIndex((true,false))
    M.bam(x, I)
end

function fuck2()
    x = rand(ntuple(identity, Val(8))...)
    I = (True(),False(),True(),False(), True(),False(),True(),False())
    slice(x,I)
end



nothing
