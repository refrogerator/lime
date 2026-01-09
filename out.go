package main

import fmt "fmt"

type ADT struct {
	t int
	v interface{}
}

func main() {
	fmt.Println(__lmmain)
}

func __add(a int) func (int) int {
	return func (b int) int {
		return a + b;
	}
}

func __sub(a int) func (int) int {
	return func (b int) int {
		return a - b;
	}
}

func __mul(a int) func (int) int {
	return func (b int) int {
		return a * b;
	}
}

func __div(a int) func (int) int {
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

var _cniTest = 0
type _cnsTest struct {
    _0 ADT
    _1 int
}
var _cnvTest = func (_0 ADT) func (int) ADT {
    return func (_1 int) ADT {
    return ADT{_cniTest,_cnsTest{_0,_1}}}}
var _cniJoe = 1
var _cnvJoe = ADT{_cniJoe,nil}

func _fn0() int {
    _0 := 20
    _1 := _cnvFalse
    _2 := _cnvTest
    _3 := _2(_1)
    _4 := _3(_0)
    _5 := amogus
    _6 := _5(_4)
    return _6;
}

func _fn1() func (ADT) int {
    _0 := func (a ADT) int {
    _1 := a
    
    _2 := __adtType
    _3 := _2(_1)
    var _4 int
    switch (_3) {
    case _cniJoe:
    
    
    _5 := 4
    _4 = _5
    case _cniTest:
    
    
    _6 := _1.v.(_cnsTest)._1
    v := _6
    
    _7 := v
    _4 = _7
    }
    return _4
    }
    return _0;
}

func _fn2() func (ADT) int {
    _0 := func (a ADT) int {
    _1 := a
    
    _2 := __adtType
    _3 := _2(_1)
    var _4 int
    switch (_3) {
    case _cniFalse:
    
    
    _5 := 7
    _4 = _5
    case _cniTrue:
    
    
    _6 := 6
    _4 = _6
    }
    return _4
    }
    return _0;
}

var __lmmain = _fn0()

var amogus = _fn1()

var bean = _fn2()