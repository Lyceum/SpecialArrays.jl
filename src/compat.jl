# Compat.jl definition for isdisjoint (https://github.com/JuliaLang/Compat.jl/pull/695)

# https://github.com/JuliaLang/julia/pull/32003
if VERSION < v"1.4.0-DEV.29"
    hasfastin(::Type) = false
    hasfastin(::Union{Type{<:AbstractSet},Type{<:AbstractDict},Type{<:AbstractRange}}) = true
    hasfastin(x) = hasfastin(typeof(x))
else
    const hasfastin = Base.hasfastin
end

# https://github.com/JuliaLang/julia/pull/34427
if VERSION < v"1.5.0-DEV.124"
    const FASTIN_SET_THRESHOLD = 70

    function isdisjoint(l, r)
        function _isdisjoint(l, r)
            hasfastin(r) && return !any(in(r), l)
            hasfastin(l) && return !any(in(l), r)
            Base.haslength(r) && length(r) < FASTIN_SET_THRESHOLD &&
                return !any(in(r), l)
            return !any(in(Set(r)), l)
        end
        if Base.haslength(l) && Base.haslength(r) && length(r) < length(l)
            return _isdisjoint(r, l)
        end
        _isdisjoint(l, r)
    end

    export isdisjoint
end
