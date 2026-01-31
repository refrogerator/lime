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
