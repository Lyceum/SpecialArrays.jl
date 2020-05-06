Zygote.@nograd innereltype, innerndims, inneraxes, innersize, innerlength

Zygote.@adjoint function slice(A::AbstractArray, alongs...)
    slice(A, alongs...), Δ -> (align(Δ, alongs...), map(_ -> nothing, alongs)...)
end

Zygote.@adjoint function align(A::NestedArray, alongs...)
    align(A, alongs...), Δ -> (slice(Δ, alongs...), map(_ -> nothing, alongs)...)
end

# flatview(A::NestedArray{V,M,N}) is equivalent to:
# align(A, [True() for _=1:M]..., [False() for _=1:N]...)
Zygote.@adjoint function flatview(A::NestedArray{V,M}) where {V,M}
    flatview(A), Δ -> (slice(Δ, Val(M)), )
end

Zygote.@adjoint flatview(A::AbstractArray) = A, identity
