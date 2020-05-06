#module Mod
#include("src/SpecialArrays.jl")
#using .SpecialArrays
#end

using SpecialArrays, ElasticArrays

dims=(2,3,4)
x = reshape(1:prod(dims), dims)
e = ElasticArray(x)
s1 = slice(copy(e), :, :, *)
s2 = slice(copy(e), :, :, *)



dims=(2,3)
x = reshape(1:prod(dims), dims)
e = ElasticArray(x)
s = slice(e, :, *)
n = [Array(el) for el in s]
ret = all(n) do x
    pushfirst!(s, x) === s
end
