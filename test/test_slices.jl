module TestSlices

include("preamble.jl")

const TEST_ALONGS = [
    (static(true), ),
    (static(false), ),

    (static(true), static(true)),
    (static(false), static(true)),
    (static(false), static(false)),
]

slicedims(al::TupleN{SBool}) = Tuple(i for i=1:length(al) if unstatic(al[i]))

# TODO add Show for slices
function _show_alongs(io, alongs::TupleN{SBool})
    write(io, '(')
    if length(alongs) == 1
        write(io, unstatic(alongs[1]) ? ':' : "i1", ',')
    elseif length(alongs) > 1
        write(io, unstatic(alongs[1]) ? ':' : "i1")
        for dim = 2:length(alongs)
            write(io, ',', ' ', unstatic(alongs[dim]) ? ':' : "i$dim")
        end
    end
    write(io, ')')
    return nothing
end

function _show_alongs(alongs::TupleN{SBool})
    io = IOBuffer()
    _show_alongs(io, alongs)
    String(take!(io))
end

function makedata(V::Type, al::TupleN{SBool})
    L = length(al)
    pdims = testdims(L)
    sdims = slicedims(al)
    innerdims = Tuple(pdims[i] for i in 1:L if unstatic(al[i]))
    outerdims = Tuple(pdims[i] for i in 1:L if !unstatic(al[i]))
    M, N = length(innerdims), length(outerdims)

    flat = rand!(Array{V,L}(undef, pdims...))

    nested = Array{Array{V,M},N}(undef, outerdims...)
    i = 0
    Base.mapslices(flat, dims=sdims) do el
        i += 1
        nested[i] = zeros(V, innerdims...)
        nested[i] .= el
        el
    end

    return (
        nested = nested,
        flat = flat,
        pdims = pdims,
        sdims = sdims,
        static_sdims = map(static, sdims),
        innerdims = innerdims,
        outerdims = outerdims,
        inneraxes = axes(first(nested)),
        outeraxes = axes(nested),
        M = M,
        N = N,
    )
end

@testset "V = $V, al = $(_show_alongs(al))" for V in (Float64, ), al in TEST_ALONGS

    @testset "constructors" begin
        @unpack flat, sdims, static_sdims, M, N = makedata(V, al)
        Expected = Slices{<:AbsArr{V,M},N,M,Array{V,M+N},typeof(al)}

        @test typeof(Slices(flat, al)) <: Expected
        @test_inferred Slices(flat, al)

        @test typeof(slice(flat, al)) <: Expected
        @test_inferred slice(flat, al)
        @test typeof(slice(flat, al...)) <: Expected
        @test_inferred slice(flat, al...)

        @test_inferred slice(flat, static_sdims)
        @test typeof(slice(flat, static_sdims)) <: Expected
        @test_inferred slice(flat, static_sdims...)
        @test typeof(slice(flat, static_sdims...)) <: Expected

        @test typeof(slice(flat, sdims)) <: Expected
        @test typeof(slice(flat, sdims...)) <: Expected
    end

    let V=V, al=al
        test_array_AB() do
            data = makedata(V, al)
            A = Slices(data.flat, al)
            B = data.nested
            return A, B
        end
    end

    #@testset "similar" begin
    #    let d = makedata(V, al)
    #        A = slice(d.flat, al)
    #        T = Array{V === Float64 ? Int : Float64, ndims(A) + 1}
    #        dims = (size(A)..., 10)
    #        @test_similar A T dims
    #    end
    #end

    @testset "misc" begin
        data = makedata(V, al)
        A = Slices(data.flat, al)
        @test parent(A) === A.parent
        @test Base.dataids(A) === Base.dataids(A.parent)
    end

    @testset "extra" begin
        data = makedata(V, al)
        A = Slices(data.flat, al)
        @test begin
            flattened = flatten(A)
            flattened !== A.parent && flattened == A.parent
        end
        @test_inferred flatten(A)

        @test flatview(A) === A.parent
        @test_inferred flatview(A)

        @test innersize(A) == size(first(A))
        @test_inferred innersize(A)
        @test_noalloc inneraxes($A)

        @test inneraxes(A) == axes(first(A))
        @test_inferred inneraxes(A)
        @test_noalloc innersize($A)
    end

    @testset "UnsafeArrays" begin
        data = makedata(V, al)
        A = Slices(data.flat, al)
        Av = uview(A)
        @test parent(Av) isa UnsafeArray{eltype(A.parent), ndims(A.parent)}
        @test A == Av
    end

    @testset "mapslices" for f in (
        identity,
        el -> sum(el),
        el -> el isa AbsArr ? reshape(el, reverse(size(el))) : el,
        el -> el isa AbsArr ? reshape(el, Val(1)) : el,
    )
        # dropdims=false/Base.mapslices behavior
        let data = makedata(V, al)
            @test_inferred SpecialArrays.mapslices(f, data.flat, dims=al)
            B1 = mapslices(f, data.flat, dims=slicedims(al))
            B2 = flatview(SpecialArrays.mapslices(f, data.flat, dims=al))
            @test B1 == B2
        end

        # dropdims=true
        let data = makedata(V, al)
            @test_inferred SpecialArrays.mapslices(f, data.flat, dims=al, dropdims=static(true))
            B1 = map(f, slice(data.flat, slicedims(al)))
            B2 = SpecialArrays.mapslices(f, data.flat, dims=al, dropdims=static(true))
            @test B1 == B2
        end
    end
end

#@testset "non-standard indexing (AxisArrays)" begin
#    A = AxisArrays.AxisArray(rand(2, 3), [:a, :b], [:x, :y, :z])
#    S = Slices(A, static(true), static(false))
#    @test S[:x] == A[:, :x]
#end

end # module