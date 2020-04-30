struct BatchedVector{T,P<:AbstractVector} <: AbstractVector{T} # TODO T<:AbstractVector
    parent::P
    offsets::Vector{Int}
    @inline function BatchedVector{T,P}(parent, offsets) where {T,P<:AbstractVector}
        check_offsets(parent, offsets)
        new(parent, offsets)
    end
end

@inline function BatchedVector(parent::AbstractVector, offsets::AbstractVector{<:Integer})
    T = viewtype(parent, firstindex(parent):firstindex(parent))
    BatchedVector{T,typeof(parent)}(parent, offsets)
end


####
#### Core Array Interface
####

@inline Base.size(A::BatchedVector) = (length(A), )

@inline Base.length(A::BatchedVector) = length(A.offsets) - 1


@propagate_inbounds function Base.getindex(A::BatchedVector, i::Int)
    return view(A.parent, batchrange(A, i))
end

@propagate_inbounds function Base.setindex!(A::BatchedVector, x, i::Int)
    A.parent[batchrange(A, i)] = x
    return A
end

Base.IndexStyle(::Type{<:BatchedVector}) = IndexLinear()


####
#### Misc
####

@inline function Base.:(==)(A::BatchedVector, B::BatchedVector)
    A.offsets == B.offsets && A.parent == B.parent
end

Base.copy(A::BatchedVector) = BatchedVector(copy(A.parent), copy(A.offsets))

Base.dataids(A::BatchedVector) = (Base.dataids(A.parent)..., Base.dataids(A.offsets)...)

Base.parent(A::BatchedVector) = A.parent


####
#### Extra
####

"""
    $(SIGNATURES)

View `A` as a vector of batches where `batch(parent, batch_lengths)[i]` has
length `batch_lengths[i]`.
"""
@inline function batch(parent::AbstractVector, batch_lengths)
    offsets = Vector{Int}(undef, length(batch_lengths) + 1)
    offsets[1] = cumsum = 0
    for (i, l) in enumerate(batch_lengths)
        cumsum += l
        offsets[i + 1] = cumsum
    end
    BatchedVector(parent, offsets)
end

"""
    $(SIGNATURES)
View `A` as a vector of batches using the same batch lengths as `B`.
"""
@inline function batchlike(A::AbstractVector, B::BatchedVector)
    length(A) == length(B.parent) || throw(ArgumentError("length(A) != length(parent(B))"))
    BatchedVector(A, copy(B.offsets))
end


####
#### 3rd Party
####

@inline function UnsafeArrays.unsafe_uview(A::BatchedVector)
    BatchedVector(uview(A.parent), A.offsets)
end


####
#### Util
####

@propagate_inbounds function batchrange(A::BatchedVector, i::Int)
    j = firstindex(A.parent)
    from = A.offsets[i] + j
    to = A.offsets[i + 1] - 1 + j
    return from:to
end

check_offsets(A::BatchedVector) = check_offsets(A.parent, A.offsets)

function check_offsets(parent::AbstractVector, offsets::AbstractVector{<:Integer})
    length(offsets) >= 1 || throw(ArgumentError("offsets cannot be empty"))
    first(offsets) == 0 || throw(ArgumentError("First offset is non-zero"))
    len = 0
    for i in LinearIndices(offsets)[2:end]
        o1 = offsets[i-1]
        o2 = offsets[i]
        o2 > o1 || throw(ArgumentError("Overlapping indices found in offsets"))
        len += o2 - o1
    end
    if len != length(parent)
        throw(ArgumentError("Length computed from offsets is not equal to length(parent)"))
    end
    return nothing
end
