"""
    innereltype(A::Type{<:AbstractArray{<:AbstractArray{V,M},N}})
    innereltype(A::AbstractArray{<:AbstractArray{V,M},N})

Returns `T`, the common `eltype` of the elements of `A`.
"""
innereltype(::Type{<:NestedArray{V}}) where {V} = @isdefined(V) ? V : Any
innereltype(A::NestedArray) = innereltype(typeof(A))


"""
    innerndims(A::Type{<:AbstractArray{<:AbstractArray{V,M},N}})
    innerndims(A::AbstractArray{<:AbstractArray{V,M},N})

Returns `M`, the common dimensionality of the elements of `A`.
"""
innerndims(::Type{<:NestedArray{V,M}}) where {V,M} = M
innerndims(A::NestedArray) = innerndims(typeof(A))


"""
    inneraxes(A::AbstractArray{<:AbstractArray{V,M},N}, [dim])

Returns the common axes of the elements of `A`.
Throws an error if the elements of `A` do not have equal axes or if `A` is empty.
"""
@inline function inneraxes(A::NestedArray{V,M}) where {V,M}
    # TODO would this be wrong for offset arrays?
    return isempty(A) ? ntuple(_ -> Base.OneTo(0), M) : _scan_inner(axes, A)
end

@inline function inneraxes(A::NestedArray{V,M}, d::Integer) where {V,M}
    d <= M ? inneraxes(A)[d] : Base.OneTo(0)
end


"""
    innersize(A::AbstractArray{<:AbstractArray{V,M},N}, [dim])

Returns the common size of the elements of `A`.
Throws an error if the elements of `A` do not have equal axes or if `A` is empty.
"""
@inline innersize(A::NestedArray) = map(Base.unsafe_length, inneraxes(A))
@inline innersize(A::NestedArray, d::Integer) = Base.unsafe_length(inneraxes(A, d))


"""
    innerlength(A::AbstractArray{<:AbstractArray{V,M},N})

Returns the common length of the elements of `A`.
Throws an error if the elements of `A` do not have equal length or if `A` is empty.
"""
@inline innerlength(A::AbstractArray) = _scan_inner(length, A)


function _scan_inner(f::F, A::AbstractArray) where {F}
    if isempty(A)
        argerror("Cannot apply $f to an empty array")
    else
        x = f(first(A))
        for I in eachindex(A)[2:end]
            f(A[I]) == x || argerror("The element arrays of A do not have matching $f")
        end
        return x
    end
end
