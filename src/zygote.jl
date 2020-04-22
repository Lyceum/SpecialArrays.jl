Zygote.@nograd innereltype, innerndims, inneraxes, innersize, innerlength

Zygote.@adjoint function slice(A::AbsArr, along...)
    slice(A, along...), Δ -> (@info size(Δ) size(first(Δ)), typeof(Δ); (flatview(Δ), map(_ -> nothing, along)...))
end

Zygote.@adjoint function flatview(A::AbsSimilarNestedArr{<:Any,M}) where {M}
    flatview(A), Δ -> (slice(Δ, ntuple(static, Val(M))),)
end
Zygote.@adjoint function flatview(A::AbsNestedArr)
    flatview(A), Δ -> (slice(Δ, ntuple(identity, innerndims(A))),)
end
Zygote.@adjoint flatview(A::AbsArr) = A, identity
