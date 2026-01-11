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

func _fn0() func (int) func (int) int {
    _0 := func (_a int) func (int) int {
    _1 := func (_b int) int {
    _2 := _b
    return _2
    }
    return _1
    }
    return _0;
}

func _fn1() int {
    _0 := 2
    _1 := 1
    _2 := _const
    _3 := _2(_1)
    _4 := _3(_0)
    return _4;
}

var _const = _fn0()

var __lmmain = _fn1()