Zygote.@nograd inner_eltype, inner_ndims, inner_axes, inner_size, inner_length

Zygote.@adjoint function slice(A::AbstractArray, alongs...)
    slice(A, alongs...), Δ -> (align(Δ, alongs...), map(_ -> nothing, alongs)...)
end

Zygote.@adjoint function align(A::NestedArray, alongs...)
    align(A, alongs...), Δ -> (slice(Δ, alongs...), map(_ -> nothing, alongs)...)
end

# flatview(A::NestedArray{V,M,N}) is equivalent to:
# align(A, [True() for _=1:M]..., [False() for _=1:N]...)
Zygote.@adjoint function flatview(A::NestedArray{V,M,N}) where {V,M,N}
    alongs = (ntuple(_ -> True(), Val(M))..., ntuple(_ -> False(), Val(N))...)
    flatview(A), Δ -> (slice(Δ, alongs), )
end

Zygote.@adjoint flatview(A::AbstractArray) = A, identity
