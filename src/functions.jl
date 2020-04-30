"""
    innereltype(A::Type{<:AbstractArray})
    innereltype(A::AbstractArray)

Returns the common `eltype` of the elements of `A`.
Throws an error if the elements of `A` do not have equal element types.
"""
innereltype(::Type{<:AbstractArrayOfSimilarArrays{V}}) where {V} = V
innereltype(A::AbstractArrayOfSimilarArrays) = innereltype(typeof(A))
innereltype(A::AbstractArray) = _scan_inner(eltype, A)


"""
    innerndims(A::Type{<:AbstractArray})
    innerndims(A::AbstractArray)

Returns the common dimensionality of the elements of `A`.
Throws an error if the elements of `A` do not have equal dimensionality.
"""
innerndims(::Type{<:AbstractArrayOfSimilarArrays{<:Any,M}}) where {M} = M
innerndims(A::AbstractArrayOfSimilarArrays) = innerndims(typeof(A))
innerndims(A::AbstractArray) = _scan_inner(ndims, A)


"""
    inneraxes(A::AbstractArray[, d])

Returns the common axes of the elements of `A`.
Throws an error if the elements of `A` do not have equal axes.
"""
function inneraxes(A::AbstractArray)
    if isempty(A)
        # TODO this would be wrong for offset arrays?
        return ntuple(_ -> Base.OneTo(0), innerndims(A))
    else
        return _scan_inner(axes, A)
    end
end

@inline function inneraxes(A::AbstractArray, d::Integer)
    d <= innerndims(A) ? inneraxes(A)[d] : Base.OneTo(1)
end


"""
    innersize(A::AbstractArray[, d])

Returns the common size of the elements of `A`.
Throws an error if the elements of `A` do not have equal axes.
"""
@inline innersize(A::AbstractArray) = map(Base.unsafe_length, inneraxes(A))
@inline innersize(A::AbstractArray, d::Integer) = Base.unsafe_length(inneraxes(A, d))


"""
    innerlength(A::AbstractArray)

Returns the common length of the elements of `A`.
Throws an error if the elements of `A` do not have equal length.
"""
@inline innerlength(A::AbstractArray) = _scan_inner(length, A)


function _scan_inner(f::F, A::AbstractArray) where {F}
    if isempty(A)
        throw(ArgumentError("Cannot apply $f to an empty array"))
    else
        x = f(first(A))
        for I in eachindex(A)[2:end]
            f(A[I]) == x || error("The element arrays of A do not have matching $f")
        end
        return x
    end
end
