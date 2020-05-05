module TestSpecialArrays

include("preamble.jl")

function testfiles()
    x = String[]
    for (root, dirs, files) in walkdir(@__DIR__), file in files
        if match(r"^test_.*\.jl$", file) !== nothing
            push!(x, relpath(joinpath(root, file), @__DIR__))
        end
    end
    return x
end

@testset "SpecialArrays" begin
    @testset "$f" for f in testfiles()
        include(f)
    end
end

end # module
