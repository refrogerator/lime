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
		return a + b
	}
}

func ___sub(a int) func (int) int {
	return func (b int) int {
		return a - b
	}
}

func ___mul(a int) func (int) int {
	return func (b int) int {
		return a * b
	}
}

func ___div(a int) func (int) int {
	return func (b int) int {
		return a / b
	}
}

type World struct {}
type Unit struct {}

func ___printChar(c int) Unit {
	fmt.Printf("%c", c)
	return Unit{}
}

func ___printString(s string) Unit {
	fmt.Printf("%s", s)
	return Unit{}
}

func ___strDrop(s string) func (int) string {
	return func (i int) string {
		return s[i:]
	}
}

func ___strTake(s string) func (int) string {
	return func (i int) string {
		return s[:i]
	}
}

func ___strSlice(s string) func (int) func (int) string {
	return func (a int) func (int) string {
		return func (b int) string {
			return s[a:b]
		}
	}
}

func ___strLength(s string) int {
	return len(s)
}
