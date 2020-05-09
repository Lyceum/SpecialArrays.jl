struct BatchedVector{T<:AbstractVector,P<:AbstractVector} <: AbstractVector{T}
    parent::P
    offsets::Vector{Int}
    @inline function BatchedVector{T,P}(parent, offsets) where {T<:AbstractVector,P<:AbstractVector}
        check_offsets(parent, offsets)
        new(parent, offsets)
    end
end

@inline function BatchedVector(parent::P, offsets::AbstractVector{<:Integer}) where {P<:AbstractVector}
    i1 = firstindex(parent)
    T = viewtype(parent, i1:i1)
    return BatchedVector{T,P}(parent, offsets)
end


####
#### Core Array Interface
####

Base.length(A::BatchedVector) = length(A.offsets) - 1

Base.size(A::BatchedVector) = (length(A), )

@propagate_inbounds Base.getindex(A::BatchedVector, i::Int) = view(A.parent, batchrange(A, i))

@propagate_inbounds function Base.setindex!(A::BatchedVector, x, i::Int)
    A.parent[batchrange(A, i)] = x
    return A
end

Base.IndexStyle(::Type{<:BatchedVector}) = IndexLinear()

@propagate_inbounds function batchrange(A::BatchedVector, i::Int)
    j = firstindex(A.parent)
    from = A.offsets[i] + j
    to = A.offsets[i + 1] - 1 + j
    return from:to
end


####
#### Misc
####

Base.copy(A::BatchedVector) = BatchedVector(copy(A.parent), copy(A.offsets))

Base.dataids(A::BatchedVector) = (Base.dataids(A.parent)..., Base.dataids(A.offsets)...)

Base.parent(A::BatchedVector) = A.parent


####
#### Extra
####

"""
    $(SIGNATURES)

View `A` as a vector of batches where `batch(A, batch_lengths)[i]` has
length `batch_lengths[i]`.

# Examples

```jldoctest
julia> A = collect(1:10)
10-element Array{Int64,1}:
  1
  2
  3
  4
  5
  6
  7
  8
  9
 10

julia> B = batch(A, [2, 3, 5])
3-element BatchedVector{SubArray{Int64,1,Array{Int64,1},Tuple{UnitRange{Int64}},true},Array{Int64,1}}:
 [1, 2]
 [3, 4, 5]
 [6, 7, 8, 9, 10]

julia> B[2] == view(A, 3:5)
true
```
"""
function batch(A::AbstractVector, batch_lengths::AbstractVector{<:Integer})
    offsets = Vector{Int}(undef, length(batch_lengths) + 1)
    offsets[1] = cumsum = 0
    for (i, l) in enumerate(batch_lengths)
        cumsum += l
        offsets[i + 1] = cumsum
    end
    return BatchedVector(A, offsets)
end

"""
    $(SIGNATURES)

View `A` as a vector of batches using the same batch lengths as `B`.

See also: [`batch`](@ref).
"""
@inline function similarbatch(A::AbstractVector, B::BatchedVector)
    length(A) == length(B.parent) || argerror("length(A) != length(parent(B))")
    return BatchedVector(A, copy(B.offsets))
end

"""
    $(SIGNATURES)

Returns the unbatched parent array wrapped by `B`.
"""
SpecialArrays.flatten(B::BatchedVector) = B.parent


####
#### 3rd Party
####

@inline function UnsafeArrays.unsafe_uview(A::BatchedVector)
    BatchedVector(uview(A.parent), A.offsets)
end


####
#### Util
####

check_offsets(A::BatchedVector) = check_offsets(A.parent, A.offsets)

function check_offsets(parent::AbstractVector, offsets::AbstractVector{<:Integer})
    length(offsets) >= 1 || argerror("offsets cannot be empty")
    first(offsets) == 0 || argerror("First offset is non-zero")
    for i in LinearIndices(offsets)[2:end]
        o1 = offsets[i-1]
        o2 = offsets[i]
        # TODO 0 length slice
        o2 > o1 || argerror("Overlapping indices found in offsets")
    end
    # TODO support batching < length(parent)
    offsets[end] == length(parent) || argerror("offsets[end] does not equal to length(parent)")
    return nothing
end
