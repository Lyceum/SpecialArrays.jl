Zygote.@nograd innereltype, innerndims, inneraxes, innersize, innerlength

Zygote.@adjoint function slice(A::AbstractArray, alongs...)
    slice(A, alongs...), Δ -> (align(Δ, alongs...), map(_ -> nothing, alongs)...)
end

Zygote.@adjoint function align(A::NestedArray, alongs...)
    align(A, alongs...), Δ -> (slice(Δ, alongs...), map(_ -> nothing, alongs)...)
end

# flatview(A) is equivalent to:
# align(A, [True() for _=1:innerndims(A)]..., [False() for _=1:ndims(A)]...)
Zygote.@adjoint function flatview(A::NestedArray)
    flatview(A), Δ -> (slice(Δ, Val(innerndims(A))), )
end

Zygote.@adjoint flatview(A::AbstractArray) = A, identity
