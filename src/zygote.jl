Zygote.@nograd innereltype, innerndims, inneraxes, innersize, innerlength

Zygote.@adjoint function slice(A::AbstractArray, alongs...)
    slice(A, alongs...), Δ -> (align(Δ, alongs...), map(_ -> nothing, alongs)...)
end

Zygote.@adjoint function align(A::AbstractArrayOfArrays, alongs...)
    align(A, alongs...), Δ -> (slice(Δ, alongs...), map(_ -> nothing, alongs)...)
end

Zygote.@adjoint function flatview(A::AbstractArrayOfArrays)
    flatview(A), Δ -> (slice(Δ, Val(innerndims(A))), )
end

Zygote.@adjoint flatview(A::AbsArr) = A, identity
