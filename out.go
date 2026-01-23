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

func _fn0() func (ADT) int {
    _0 := func (_n ADT) int {
    _1 := _n
    
    _2 := __adtType
    _3 := _2(_1)
    var _4 int
    switch (_3) {
    case _cniNil:
    
    
    _5 := 0
    _4 = _5
    case _cniCons:
    
    
    _6 := _1.v.(_cnsCons)._0
    _i := _6
    
    _7 := _1.v.(_cnsCons)._1
    _l := _7
    
    _8 := _l
    _9 := _fn0()
    _10 := _9(_8)
    _11 := _i
    _12 := ___add
    _13 := _12(_11)
    _14 := _13(_10)
    _4 = _14
    }
    return _4
    }
    return _0;
}

func _fn1() func (ADT) int {
    _0 := func (_n ADT) int {
    _1 := _n
    
    _2 := __adtType
    _3 := _2(_1)
    var _4 int
    switch (_3) {
    case _cniNil:
    
    
    _5 := 0
    _4 = _5
    case _cniCons:
    
    
    _6 := _1.v.(_cnsCons)._0
    _i := _6
    
    _7 := _1.v.(_cnsCons)._1
    _l := _7
    
    _8 := _l
    _9 := _fn1()
    _10 := _9(_8)
    _11 := _i
    _12 := ___add
    _13 := _12(_11)
    _14 := _13(_10)
    _4 = _14
    }
    return _4
    }
    return _0;
}

func _fn2() int {
    _0 := _cnvNil
    _1 := 3
    _2 := _cnvCons
    _3 := _2(_1)
    _4 := _3(_0)
    _5 := 2
    _6 := _cnvCons
    _7 := _6(_5)
    _8 := _7(_4)
    _9 := _cnvNil
    _10 := 1
    _11 := _cnvCons
    _12 := _11(_10)
    _13 := _12(_9)
    _14 := _funny1
    _15 := _14(_13)
    _16 := _cnvCons
    _17 := _16(_15)
    _18 := _17(_8)
    _19 := _funny
    _20 := _19(_18)
    return _20;
}

var _funny1 = _fn0()

var _funny = _fn1()

var __lmmain = _fn2()