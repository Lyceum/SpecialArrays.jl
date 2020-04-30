module M

using BenchmarkTools
using SpecialArrays
using Base: @_inline_meta
using InteractiveUtils
using SpecialArrays
using SpecialArrays.TupleTools
using Base: @pure

# TODO
#@inline Base.index_dimsum(::Base.Slice{Base.OneTo{Int}}, I...) = (true, Base.index_dimsum(I...)...)

L = 2
dims = ntuple(i -> mod(i, 3)+1, L)
A = reshape(collect(1:prod(dims)), dims)
I = ntuple(i -> isodd(i) ? Colon() : *, L)
I2 = ntuple(isodd, L)
I3 = filter(isodd, ntuple(identity, Val(6)))

#@code_warntype slice(A, I)

using BenchmarkTools
function test()
    dims = (1,2,3,4,5,6)
    A = Array{Int}(undef, dims)
    I = BoolIndex(true,false,true,false,true,false)
    SlicedArray(A,I)
end

struct MyBoolIndex{N,M,mask}
    @inline function MyBoolIndex{N,M,mask}() where {N,M,mask}
        #mask isa Bools{N::Int} && M::Int == index_length(mask) || error("Invalid type parameters")
        new{N,M,mask}()
    end
end
@pure MyBoolIndex(mask::NTuple{N,Bool}) where {N} = MyBoolIndex{N,isum(mask),Tuple{mask...}}()
@pure MyBoolIndex(::Type{mask}) where {mask} = MyBoolIndex{length(mask.parameters),+(mask.parameters...),mask}()
@pure MyBoolIndex(::Type{Tuple{}}) where {mask} = MyBoolIndex{0,0,Tuple{}}()

@pure isum(I::NTuple{N,Bool}) where {N} = +(I...)
@pure isum(I::Tuple{}) = 0


@pure tail(t::Tuple) = Base.argtail(t...)
@pure tail(I::BoolIndex{N,M,mask}) where {N,M,mask} = BoolIndex(tail(mask))
@pure tail(I::MyBoolIndex{N,M,mask}) where {N,M,mask} = MyBoolIndex(Base.tuple_type_tail(mask))

@pure Base.first(I::BoolIndex{N,M,mask}) where {N,M,mask} = first(mask.parameters)

mygetindex(t::Anys{N}, I::BoolIndex{N,M,mask}) where {N,M,mask} = _getindex(t, I)
function _getindex(t::Anys{N}, I::BoolIndex{N,M,mask}) where {N,M,mask}
    rest  = _getindex(tail(t), tail(I))
    return first(mask) === true ? (first(t), rest...) : rest
end
_getindex(::Tuple{}, ::BoolIndex{0,0,()}) = ()

mygetindex(t::Anys{N}, I::MyBoolIndex{N,M,mask}) where {N,M,mask} = _getindex(I,t)
function _getindex(I::MyBoolIndex{N,M,<:Tuple{true,Vararg{Any}}}, t::Anys{N}) where {N,M}
    (first(t), _getindex(tail(I), tail(t))...)
end
function _getindex(I::MyBoolIndex{N,M,<:Tuple{false,Vararg{Any}}}, t::Anys{N}) where {N,M}
    _getindex(tail(I), tail(t))
end
_getindex(::MyBoolIndex{0,0,Tuple{}},::Tuple{}) = ()

@pure valtail(::Val{I}) where {I} = Val(tail(I))
function mygetindex(t::Tuple, Ival::Val{I}) where {I}
    rest = mygetindex(tail(t), valtail(Ival))
    first(I) ? (first(t), rest...) : rest
end
mygetindex(t::Tuple{}, Ival::Val{()}) = ()

function mygetindex(t::Tuple, ::Type{I}) where {I}
    rest = mygetindex(tail(t), Base.tuple_type_tail(I))
    first(I.parameters) === true ? (first(t), rest...) : rest
end
mygetindex(t::Tuple{}, ::Type{Tuple{}}) = ()
#function mygetindex(t::Tuple, I::Type{<:Tuple{true,Vararg{Any}}})
#    (first(t), mygetindex(tail(t), Base.tuple_type_tail(I))...)
#end
#function mygetindex(t::Tuple, I::Type{<:Tuple{false,Vararg{Any}}})
#    mygetindex(tail(t), Base.tuple_type_tail(I))
#end
#mygetindex(t::Tuple{}, ::Type{Tuple{}}) = ()

my_tail_const_prop(i, tail...) = tail
@inline mytail2(i::Bool, tail::Bool...) = i ? (i, mytail2(tail...)...) : mytail2(tail...)
mytail2() = 10
function foo_tail_const_prop()
    #Val{my_tail_const_prop(1,2,3,4)}()
    Val{mytail2(true,false,true)}()
end
#@test (@inferred foo_tail_const_prop()) == Val{(2,3,4)}()

#mydam(i::Bool, tail::Bool...) = i ? (i, mydam)

function test2()
    t = (1,2,3)
    #I = MyBoolIndex((true,false,true))
    #I = Val((true,false,true))
    #mygetindex((1,2,3),Val((true,false,true)))
    #mygetindex((1,2,3),Tuple{true,false,true})
    foo_tail_const_prop()
end


end # module

using SpecialArrays
using SpecialArrays.TupleTools
using Cthulhu
