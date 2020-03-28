module TestSpecialArrays

include("preamble.jl")

if isempty(ARGS)
    TEST_FILES =
        sort([file for file in readdir(@__DIR__) if match(r"^test_.*\.jl$", file) !== nothing])
else
    TEST_FILES = ARGS
end

@testset "$file" for file in TEST_FILES
    include(file)
end

end # module
