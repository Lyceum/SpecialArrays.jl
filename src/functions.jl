"""
    innereltype(A::AbstractArray{<:AbstractArray})
    innereltype(A::Type{<:AbstractArray{<:AbstractArray}})

Returns the common element type of the element arrays of `A`.
Equivalent to eltype(eltype(A)).
"""
function innereltype end

innereltype(::Type{A}) where {A<:AbsNestedArr} = eltype(eltype(A))
innereltype(A::AbsNestedArr) = eltype(eltype(A))


"""
    innerndims(A::AbstractArray{<:AbstractArray})
    innerndims(A::Type{<:AbstractArray{<:AbstractArray}})

Returns the dimensionality of the element arrays of `A`.
Equivalent to ndims(eltype(A)).
"""
function innerndims end

innerndims(::Type{A}) where {A<:AbsNestedArr} = ndims(eltype(A))
innerndims(A::AbsNestedArr) = ndims(eltype(A))


"""
    inneraxes(A::AbstractArray{<:AbstractArray}[, d])

Returns the common length of the element arrays of `A`.
Throws an error if the element arrays of `A` do not have equal axes.
"""
function inneraxes end

function inneraxes(A::AbsNestedArr)
    M = innerndims(A)
    if isempty(A)
        # TODO this would be wrong for offset arrays?
        ax = ntuple(_ -> Base.OneTo(0), Val(M))
    else
        ax = axes(first(A))
        length(ax) == M || throw(DimensionMismatch("length(inneraxes(A)) != innerndims(A)"))
        for a in A
            if axes(a) != ax
                throw(DimensionMismatch("The elements of A do not have equal axes"))
            end
        end
    end
    return ax
end

@inline function inneraxes(A::AbsNestedArr, d::Integer)
    d <= innerndims(A) ? inneraxes(A)[d] : Base.OneTo(1)
end


"""
    innersize(A::AbstractArray{<:AbstractArray}[, d])

Returns the size of the element arrays of `A`.
Throws an error if the elements of `A` do not have equal axes.
"""
function innersize end

@inline innersize(A::AbsArr) = map(Base.unsafe_length, inneraxes(A))
@inline innersize(A::AbsArr, d::Integer) = Base.unsafe_length(inneraxes(A, d))


"""
    innerlength(A::AbstractArray{<:AbstractArray})

Returns the common length of the element arrays of `A`.
Throws an error if the element arrays of `A` do not have equal size.
"""
function innerlength end

@inline innerlength(A::AbsNestedArr) = prod(innersize(A))


"""
    flatten(A::AbstractArray{<:AbstractArray{V,M},N}

Flatten `A` into an AbstractArray{V,M+N}. Fails if the elements of `A` do not all
have the same size. If the `A` is not a nested array, the return value is `A` itself.
"""
function flatten end

flatten(A::AbsArr) = A
flatten(A::AbsNestedArr) = Array(flatview(A))