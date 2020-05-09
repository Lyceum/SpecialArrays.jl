Zygote.@nograd inner_eltype, inner_ndims, inner_axes, inner_size, inner_length

Zygote.@adjoint function slice(A::AbstractArray, alongs...)
    slice(A, alongs...), Δ -> (align(Δ, alongs...), map(_ -> nothing, alongs)...)
end

Zygote.@adjoint function align(A::NestedArray, alongs...)
    align(A, alongs...), Δ -> (slice(Δ, alongs...), map(_ -> nothing, alongs)...)
end

# flatten(A::NestedArray{V,M,N}) is equivalent to:
# align(A, [True() for _=1:M]..., [False() for _=1:N]...)
Zygote.@adjoint function flatten(A::NestedArray{V,M}) where {V,M}
    flatten(A), Δ -> (slice(Δ, Val(M)), )
end
Zygote.@adjoint flatten(A::AbstractArray) = A, Δ -> (Δ, )
