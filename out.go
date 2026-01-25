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
    _1 := func (_b int) int {
    _2 := _b
    _3 := 4
    _4 := ___add
    _5 := _4(_3)
    _6 := _5(_2)
    return _6
    }
    _a := _1
    _7 := 3
    _8 := _a
    _9 := _8(_7)
    _0 = _9
    }
    _10 := 2
    _11 := ___add
    _12 := _11(_10)
    _13 := _12(_0)
    return _13;
}

var __lmmain = _fn0()
