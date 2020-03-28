Zygote.@nograd innereltype, innerndims, inneraxes, innersize, innerlength

Zygote.@adjoint function slice(A::AbsArr, along...)
    slice(A, along...), Δ -> (flatten(Δ), map(_ -> nothing, along)...)
end

Zygote.@adjoint function flatten(A::AbsSimilarNestedArr{<:Any,M}) where {M}
    flatten(A), Δ -> (slice(Δ, ntuple(static, Val(M))),)
end
Zygote.@adjoint function flatten(A::AbsNestedArr)
    flatten(A), Δ -> (slice(Δ, ntuple(identity, innerndims(A))),)
end
Zygote.@adjoint flatten(A::AbsArr) = A, identity
