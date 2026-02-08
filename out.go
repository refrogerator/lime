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

func ___printChar(c int) func (World) Unit {
	return func (_ World) Unit {
		fmt.Printf("%c", c)
		return Unit{}
	}
}

var _cniTrue = 0
var _cnvTrue = ADT{_cniTrue,nil}
var _cniFalse = 1
var _cnvFalse = ADT{_cniFalse,nil}

var _cniIOHelper = 0
type _cnsIOHelper struct {
    _0 World
    _1 Unit
}
var _cnvIOHelper = func (_0 World) func (Unit) ADT {
    return func (_1 Unit) ADT {
    return ADT{_cniIOHelper,_cnsIOHelper{_0,_1}}}}

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

func _fn1() func (int) func (World) ADT {
    _0 := func (_c int) func (World) ADT {
    _1 := func (_w World) ADT {
    _2 := _w
    _3 := _c
    _4 := ___printChar
    _5 := _4(_3)
    _6 := _5(_2)
    _7 := _w
    _8 := _cnvIOHelper
    _9 := _8(_7)
    _10 := _9(_6)
    return _10
    }
    return _1
    }
    return _0;
}

var _printChar = _fn1()

func _fn2() func (ADT) func (World) ADT {
    _0 := func (_n ADT) func (World) ADT {
    _1 := _n
    
    _2 := _1.t
    var _3 func (World) ADT
    switch (_2) {
    case _cniNil:
    
    
    _4 := 10
    _5 := _printChar
    _6 := _5(_4)
    _3 = _6
    case _cniCons:
    
    
    _7 := _1.v.(_cnsCons)._0
    _i := _7
    
    _8 := _1.v.(_cnsCons)._1
    _l := _8
    
    _9 := func (_x Unit) func (World) ADT {
    _10 := _l
    _11 := _fn2()
    _12 := _11(_10)
    return _12
    }
    _13 := _i
    _14 := _printChar
    _15 := _14(_13)
    _16 := _bind
    _17 := _16(_15)
    _18 := _17(_9)
    _3 = _18
    }
    return _3
    }
    return _0;
}

var _print = _fn2()

func _fn3() func (World) ADT {
    _0 := _cnvTrue
    _1 := _show
    _2 := _1(_0)
    _3 := _print
    _4 := _3(_2)
    return _4;
}

var __lmmain = _fn3()

func _fn4() func (func (World) ADT) func (func (Unit) func (World) ADT) func (World) ADT {
    _0 := func (_a func (World) ADT) func (func (Unit) func (World) ADT) func (World) ADT {
    _1 := func (_f func (Unit) func (World) ADT) func (World) ADT {
    _2 := func (_w World) ADT {
    _3 := _w
    _4 := _a
    _5 := _4(_3)
    
    _6 := _5.t
    var _7 ADT
    switch (_6) {
    case _cniIOHelper:
    
    
    _8 := _5.v.(_cnsIOHelper)._0
    _w2 := _8
    
    _9 := _5.v.(_cnsIOHelper)._1
    _g := _9
    
    _10 := _w2
    _11 := _g
    _12 := _f
    _13 := _12(_11)
    _14 := _13(_10)
    _7 = _14
    }
    return _7
    }
    return _2
    }
    return _1
    }
    return _0;
}

var _bind = _fn4()