package main

import fmt "fmt"

type ADT struct {
	t int
	v interface{}
}

func main() {
	fmt.Println(__lmmain)
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

func _fn0() func (ADT) func (int) func (int) int {
    _0 := func (_a ADT) func (int) func (int) int {
    _1 := func (_b int) func (int) int {
    _2 := func (_c int) int {
    _3 := _a
    
    _4 := __adtType
    _5 := _4(_3)
    var _6 int
    switch (_5) {
    case _cniFalse:
    
    
    _7 := _c
    _6 = _7
    case _cniTrue:
    
    
    _8 := _b
    _6 = _8
    }
    return _6
    }
    return _2
    }
    return _1
    }
    return _0;
}

func _fn1() int {
    _0 := 7
    _1 := 6
    _2 := _cnvTrue
    _3 := _gaming
    _4 := _3(_2)
    _5 := _4(_1)
    _6 := _5(_0)
    return _6;
}

var _gaming = _fn0()

var __lmmain = _fn1()