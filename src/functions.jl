"""
    inner_eltype(A::Type{<:AbstractArray{<:AbstractArray{V,M},N}})
    inner_eltype(A::AbstractArray{<:AbstractArray{V,M},N})

Returns `T`, the common `eltype` of the elements of `A`.
"""
inner_eltype(::Type{<:NestedArray{V}}) where {V} = @isdefined(V) ? V : Any
inner_eltype(A::NestedArray) = inner_eltype(typeof(A))


"""
    inner_ndims(A::Type{<:AbstractArray{<:AbstractArray{V,M},N}})
    inner_ndims(A::AbstractArray{<:AbstractArray{V,M},N})

Returns `M`, the common dimensionality of the elements of `A`.
"""
inner_ndims(::Type{<:NestedArray{<:Any,M}}) where {M} = M
inner_ndims(A::NestedArray) = inner_ndims(typeof(A))


"""
    inner_axes(A::AbstractArray{<:AbstractArray{V,M},N}, [dim])

Returns the common axes of the elements of `A`.
Throws an error if the elements of `A` do not have equal axes or if `A` is empty.
"""
@inline function inner_axes(A::NestedArray{<:Any,M}) where {M}
    # TODO would this be wrong for offset arrays?
    return isempty(A) ? ntuple(_ -> Base.OneTo(0), M) : _scan_inner(axes, A)
end

@inline function inner_axes(A::NestedArray{M}, d::Integer) where {M}
    d <= M ? inner_axes(A)[d] : Base.OneTo(0)
end


"""
    inner_size(A::AbstractArray{<:AbstractArray{V,M},N}, [dim])

Returns the common size of the elements of `A`.
Throws an error if the elements of `A` do not have equal axes or if `A` is empty.
"""
@inline inner_size(A::NestedArray) = map(Base.unsafe_length, inner_axes(A))
@inline inner_size(A::NestedArray, d::Integer) = Base.unsafe_length(inner_axes(A, d))


"""
    inner_length(A::AbstractArray{<:AbstractArray{V,M},N})

Returns the common length of the elements of `A`.
Throws an error if the elements of `A` do not have equal length or if `A` is empty.
"""
@inline inner_length(A::AbstractArray) = _scan_inner(length, A) # faster than prod(inner_size(A))


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
