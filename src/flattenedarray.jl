struct FlattenedArray{V,L,M,N,P<:NestedArray{V,M,N},InAx<:Anys{M}} <: AbstractArray{V,L}
    parent::P
    inner_axes::InAx
    @inline function FlattenedArray{V,L,M,N,P,InAx}(parent, inner_axes) where {V,L,M,N,P<:NestedArray{V,M,N},InAx<:Anys{M}}
        new{V,L,M,N,P,InAx}(parent, inner_axes)
    end
end

@inline function FlattenedArray(parent::NestedArray{V,M,N}, inner_axes::Anys{M}) where {V,M,N}
    FlattenedArray{V,M+N,M,N,typeof(parent),typeof(inner_axes)}(parent, inner_axes)
end

FlattenedArray(parent::NestedArray) = FlattenedArray(parent, inner_axes(parent))


####
#### Core Array Interface
####

@inline Base.axes(F::FlattenedArray) = (F.inner_axes..., axes(F.parent)...)

@inline Base.size(F::FlattenedArray) = map(Base.unsafe_length, axes(F))


# standard Cartesian indexing
@propagate_inbounds function Base.getindex(F::FlattenedArray{<:Any,L,M}, I::Vararg{Int,L}) where {L,M}
    Iin, Iout = tuple_split(I, Val(M))
    F.parent[Iout...][Iin...]
end

@propagate_inbounds function Base.setindex!(
    F::FlattenedArray{<:Any,L,M},
    v,
    I::Vararg{Int,L},
) where {L,M}
    Iin, Iout = tuple_split(I, Val(M))
    F.parent[Iout...][Iin...] = v
    return F
end


####
#### Misc
####

Base.copy(F::FlattenedArray) = FlattenedArray(deepcopy(F.parent), F.inner_axes)

Base.parent(F::FlattenedArray) = F.parent

function Base.showarg(io::IO, A::FlattenedArray, toplevel)
    print(io, "flatview(")
    Base.showarg(io, parent(A), false)
    print(io, ')')
    toplevel && print(io, " with eltype ", eltype(A))
    return nothing
end


# TODO verify that dataids should be the union of the child arrays
# Because F::FlattenedArray is a flattened view of an AbstractArray{<:AbstractArray},
# Base.dataids should return the equivalent of union(Base.dataids(x), Base.dataids.(x)...).
# This works, but the default recursive implementation is slow when length(parent(F)) is large.
function Base.mightalias(A::AbstractArray, F::FlattenedArray)
    !isbits(A) && !isbits(F) && !isdisjoint(Base.dataids(A), _dataids(F))
end
function Base.mightalias(F::FlattenedArray, A::AbstractArray)
    !isbits(F) && !isbits(A) && !isdisjoint(_dataids(F), Base.dataids(A))
end
function Base.mightalias(F1::FlattenedArray, F2::FlattenedArray)
    !isbits(F1) && !isbits(F2) && !isdisjoint(_dataids(F1), _dataids(F2))
end

Base.dataids(F::FlattenedArray) = Tuple(_dataids(F))

function _dataids(F::FlattenedArray)
    ids = [Base.dataids(F.parent)...]
    for A in F.parent, id in Base.dataids(A)
        push!(ids, id)
    end
    ids
end


####
#### Extra
####

"""
    flatview(A::AbstractArray{<:AbstractArray{V,M},N})

Return a `M+N`-dimensional flattened view of `A`. If `A` is not a subtype of
`AbstractArray{<:AbstractArray{V,M},N}`, the return value is `A` itself.
Throws an error if the elements of `A` do not have equal size.
Equivalent to `align(A, 1:M...)`.

See also: [`align`](@ref).

# Examples

```jldoctest
julia> A = [[1 2; 3 4], [5 6; 7 8]]
2-element Array{Array{Int64,2},1}:
 [1 2; 3 4]
 [5 6; 7 8]

julia> flatview(A)
2×2×2 flatview(::Array{Array{Int64,2},1}) with eltype Int64:
[:, :, 1] =
 1  2
 3  4

[:, :, 2] =
 5  6
 7  8

julia> flatview(A) == reshape(reduce(hcat, A), 2, 2, 2)
true
```
"""
flatview(A::NestedArray) = FlattenedArray(A)
flatview(A::AbstractArray) = A

"""
    $(SIGNATURES)

A recursive version of [`flatview`](@ref).

# Examples

```jldoctest
julia> x = [ [[1,2], [3,4]], [[5,6], [7,8]]]
2-element Array{Array{Array{Int64,1},1},1}:
 [[1, 2], [3, 4]]
 [[5, 6], [7, 8]]

julia> deep_flatview(x)
2×2×2 flatview(flatview(::Array{Array{Array{Int64,1},1},1})) with eltype Int64:
[:, :, 1] =
 1  3
 2  4

[:, :, 2] =
 5  7
 6  8

julia> deep_flatview(x) == flatview(flatview(x))
true
```
"""
deep_flatview(A::NestedArray) = deep_flatview(flatview(A))
deep_flatview(A::AbstractArray) = A
