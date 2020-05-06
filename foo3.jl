#module Mod
#include("src/SpecialArrays.jl")
#using .SpecialArrays
#end

using SpecialArrays, ElasticArrays

n = [[1 2; 3 4], [5 6; 7 8]]
F = flatview(n)
len = innerlength(n)
for i = 1:length(n)
    a = n[i]
    offs = (i - 1) * len + 1
    @info offs:(offs+len-1)
    @info F[offs:offs+len-1] == vec(n[i])
end
