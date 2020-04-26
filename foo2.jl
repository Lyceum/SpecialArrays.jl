module M
using Test
using InteractiveUtils

#using Base: tail
Base.@pure argtail(x, rest...) = rest
tail(x) = argtail(x...)

getindex_unrolled(into::Tuple{}, switch::Tuple{}) = ()
getindex_unrolled(into, switch::Tuple{}) = ()
getindex_unrolled(into::Tuple{}, switch) = ()

function getindex_unrolled(into, switch)
    next = getindex_unrolled(tail(into), tail(switch))
    if first(switch)
        (first(into), next...)
    else
        next
    end
end

test() = Val{getindex_unrolled((1, "b", 3.0), (true, false, true))}()
#@code_warntype test()
@test (@inferred test() == Val{(1, 3.0)}())

# Test that tail-like functions don't block constant propagation
my_tail_const_prop(i, tail...) = tail
function foo_tail_const_prop()
    Val{my_tail_const_prop(1,2,3,4)}()
end
@test (@inferred foo_tail_const_prop()) == Val{(2,3,4)}()

end


nothing
