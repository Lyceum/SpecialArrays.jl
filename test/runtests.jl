module TestSpecialArrays

include("preamble.jl")

#TEST_FILES = ["test_flattenedarray.jl", "test_functions.jl"]
#TEST_FILES = ["test_functions.jl"]
#TEST_FILES = ["test_slicedarray.jl"]

TEST_FILES = sort([file for file in readdir(@__DIR__) if
                  match(r"^test_.*\.jl$", file) !== nothing])

@testset "$file" for file in TEST_FILES
    include(file)
end

end # module