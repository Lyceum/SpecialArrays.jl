#macro includetests(testname)
#    if __source__.file !== nothing
#        _dirname = dirname(String(__source__.file::Symbol))
#        root = isempty(_dirname) ? pwd() : abspath(_dirname)
#    else
#        root = pwd()
#    end
#    ex = quote
#        if $isempty(ARGS)
#            local testfiles = $filter(f -> $match(r"^test_.*\.jl$", $basename(f)) !== nothing, $flattendir($root, dirs=false, join=false))
#        else
#            local testfiles = $map(f -> $endswith(f, ".jl") ? f : "$f.jl", ARGS)
#        end
#        @includetests $testname testfiles
#    end
#    esc(ex)
#end

function tuple_split(t::NTuple{L,Any}, ::Val{M}) where {L,M}
    ntuple(i -> t[i], Val(M)), ntuple(i -> t[M+i], Val(L-M))
end
