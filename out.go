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

func _fn0() int {
    var _0 int
    {
    _1 := 4
    _a := _1
    _2 := 5
    _3 := _a
    _4 := ___add
    _5 := _4(_3)
    _6 := _5(_2)
    _0 = _6
    }
    _7 := 2
    _8 := ___add
    _9 := _8(_7)
    _10 := _9(_0)
    return _10;
}

var __lmmain = _fn0()