package main

import fmt "fmt"

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

var True = 0
var False = 1


func main () {
    fmt.Println(True)
}
