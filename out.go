package main

import fmt "fmt"
import runtime "runtime"

type ADT struct {
	t int
	v interface{}
}

func main() {
	__lmmain(World{})
	var stats runtime.MemStats 
	runtime.ReadMemStats(&stats)
	fmt.Println(stats.Mallocs)
}

func ___add(a int) func (int) int {
	return func (b int) int {
		return a + b;
	}
}

func ___sub(a int) func (int) int {
	return func (b int) int {
		return a - b;
	}
}

func ___mul(a int) func (int) int {
	return func (b int) int {
		return a * b;
	}
}

func ___div(a int) func (int) int {
	return func (b int) int {
		return a / b;
	}
}

type World struct {}
type Unit struct {}

var ___world = World{}

func ___printChar(c int) IO {
	return func (_ World) World {
		fmt.Printf("%c", c)
		return ___world
	}
}

type IO func (World) World

func __adtType(a ADT) int {
	return a.t;
}

func __adtValue(a ADT) interface{} {
	return a.v;
}

func __index[A any](a []A) func(int) A {
	return func (i int) A {
		return a[i]
	}
}

var _cniTrue = 0
var _cnvTrue = ADT{_cniTrue,nil}
var _cniFalse = 1
var _cnvFalse = ADT{_cniFalse,nil}

var _cniCons = 0
type _cnsCons struct {
    _0 int
    _1 ADT
}
var _cnvCons = func (_0 int) func (ADT) ADT {
    return func (_1 ADT) ADT {
    return ADT{_cniCons,_cnsCons{_0,_1}}}}
var _cniNil = 1
var _cnvNil = ADT{_cniNil,nil}

func _fn0() func (ADT) ADT {
    _0 := func (_a ADT) ADT {
    _1 := _a
    
    _2 := _1.t
    var _3 ADT
    switch (_2) {
    case _cniFalse:
    
    
    _4 := _cnvNil
    _5 := 101
    _6 := _cnvCons
    _7 := _6(_5)
    _8 := _7(_4)
    _9 := 115
    _10 := _cnvCons
    _11 := _10(_9)
    _12 := _11(_8)
    _13 := 108
    _14 := _cnvCons
    _15 := _14(_13)
    _16 := _15(_12)
    _17 := 97
    _18 := _cnvCons
    _19 := _18(_17)
    _20 := _19(_16)
    _21 := 70
    _22 := _cnvCons
    _23 := _22(_21)
    _24 := _23(_20)
    _3 = _24
    case _cniTrue:
    
    
    _25 := _cnvNil
    _26 := 101
    _27 := _cnvCons
    _28 := _27(_26)
    _29 := _28(_25)
    _30 := 117
    _31 := _cnvCons
    _32 := _31(_30)
    _33 := _32(_29)
    _34 := 114
    _35 := _cnvCons
    _36 := _35(_34)
    _37 := _36(_33)
    _38 := 84
    _39 := _cnvCons
    _40 := _39(_38)
    _41 := _40(_37)
    _3 = _41
    }
    return _3
    }
    return _0;
}

var _show = _fn0()

func _fn1() func (ADT) func (World) World {
    _0 := func (_n ADT) func (World) World {
    _1 := _n
    
    _2 := _1.t
    var _3 func (World) World
    switch (_2) {
    case _cniNil:
    
    
    _4 := 10
    _5 := ___printChar
    _6 := _5(_4)
    _3 = _6
    case _cniCons:
    
    
    _7 := _1.v.(_cnsCons)._0
    _i := _7
    
    _8 := _1.v.(_cnsCons)._1
    _l := _8
    
    _9 := _l
    _10 := _fn1()
    _11 := _10(_9)
    _12 := _i
    _13 := ___printChar
    _14 := _13(_12)
    _15 := _bind
    _16 := _15(_14)
    _17 := _16(_11)
    _3 = _17
    }
    return _3
    }
    return _0;
}

var _print = _fn1()

func _fn2() func (World) World {
    _0 := _cnvTrue
    _1 := _show
    _2 := _1(_0)
    _3 := _print
    _4 := _3(_2)
    return _4;
}

var __lmmain = _fn2()

func _fn3() func (func (World) World) func (func (World) World) func (World) World {
    _0 := func (_a func (World) World) func (func (World) World) func (World) World {
    _1 := func (_b func (World) World) func (World) World {
    _2 := func (_x World) World {
    _3 := _x
    _4 := _a
    _5 := _4(_3)
    _6 := _b
    _7 := _6(_5)
    return _7
    }
    return _2
    }
    return _1
    }
    return _0;
}

var _bind = _fn3()