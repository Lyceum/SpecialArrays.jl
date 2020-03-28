"""
    innereltype(A::Type{<:AbstractArray})
    innereltype(A::AbstractArray)

Returns the common `eltype` of the elements of `A`.
"""
innereltype(::Type{A}) where {A<:AbsArr} = eltype(eltype(A))
innereltype(A::AbsArr) = eltype(eltype(A))


"""
    innerndims(A::Type{<:AbstractArray})
    innerndims(A::AbstractArray)

Returns the common dimensionality of the elements of `A`.
Throws an error if the elements of `A` do not have equal dimensionality.
"""
innerndims(::Type{<:AbsArr{<:AbsArr{<:Any,N}}}) where {N} = N
innerndims(A::AbsArr{<:AbsArr{<:Any,N}}) where {N} = N
# Unlike innereltype which defaults to Any if eltype(A) isa UnionAll there is no good default for N
innerndims(A::AbsArr) = _scan_inner(ndims, A)


"""
    inneraxes(A::AbstractArray[, d])

Returns the common axes of the elements of `A`.
Throws an error if the elements of `A` do not have equal axes.
"""
function inneraxes(A::AbsArr)
    if isempty(A)
        # TODO this would be wrong for offset arrays?
        return ntuple(_ -> Base.OneTo(0), innerndims(A))
    else
        return _scan_inner(axes, A)
    end
end

@inline function inneraxes(A::AbsArr, d::Integer)
    d <= innerndims(A) ? inneraxes(A)[d] : Base.OneTo(1)
end


"""
    innersize(A::AbstractArray[, d])

Returns the common size of the elements of `A`.
Throws an error if the elements of `A` do not have equal axes.
"""
@inline innersize(A::AbsArr) = map(Base.unsafe_length, inneraxes(A))
@inline innersize(A::AbsArr, d::Integer) = Base.unsafe_length(inneraxes(A, d))


"""
    innerlength(A::AbstractArray)

Returns the common length of the elements of `A`.
Throws an error if the elements of `A` do not have equal length.
"""
@inline innerlength(A::AbsArr) = _scan_inner(length, A)


function _scan_inner(f::F, A::AbsArr) where {F}
    if isempty(A)
        throw(ArgumentError("Cannot apply $f to an empty array"))
    else
        x = f(first(A))
        for a in A
            f(a) == x || error("The element arrays of A do not have matching $f")
        end
        return x
    end
end
