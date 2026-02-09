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

func ___printChar(c int) Unit {
	fmt.Printf("%c", c)
	return Unit{}
}

var _cniIOHelper = 0
type _cnsIOHelper struct {
    _0 World
    _1 string
}
var _cnvIOHelper = func (_0 World) func (string) ADT {
    return func (_1 string) ADT {
    return ADT{_cniIOHelper,_cnsIOHelper{_0,_1}}}}

func _fn0() func (string) func (World) ADT {
    _0 := _primIO
    return _0;
}

var _pure = _fn0()

func _fn1() func (string) func (World) ADT {
    _0 := func (_a string) func (World) ADT {
    _1 := func (_w World) ADT {
    _2 := _a
    _3 := _w
    _4 := _cnvIOHelper
    _5 := _4(_3)
    _6 := _5(_2)
    return _6
    }
    return _1
    }
    return _0;
}

var _primIO = _fn1()

func _fn2() func (World) ADT {
    _0 := "test"
    _1 := _pure
    _2 := _1(_0)
    return _2;
}

var __lmmain = _fn2()