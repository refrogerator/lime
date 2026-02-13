package main

import fmt "fmt"
import runtime "runtime"

type ADT struct {
	t int
	v interface{}
}

func main() {
	__lmmain.v.(_cnsIO)._0(World{})
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

var _cniIO = 0
type _cnsIO struct {
    _0 func (World) ADT
}
var _cnvIO = func (_0 func (World) ADT) ADT {
    return ADT{_cniIO,_cnsIO{_0}}}

var _cniIOHelper = 0
type _cnsIOHelper struct {
    _0 World
    _1 Unit
}
var _cnvIOHelper = func (_0 World) func (Unit) ADT {
    return func (_1 Unit) ADT {
    return ADT{_cniIOHelper,_cnsIOHelper{_0,_1}}}}

// putStrLn :: String -> IO Unit
func _fn0() func (string) ADT {
    _0 := func (_s string) ADT {
    _1 := func (_a Unit) ADT {
    _2 := 10
    _3 := _printChar
    _4 := _3(_2)
    return _4
    }
    _5 := _s
    _6 := _putStr
    _7 := _6(_5)
    _8 := _bind
    _9 := _8(_7)
    _10 := _9(_1)
    return _10
    }
    return _0;
}

var _putStrLn = _fn0()

// putStr :: String -> IO Unit
func _fn1() func (string) ADT {
    _0 := func (_s string) ADT {
    _1 := _s
    _2 := ___printString
    _3 := _2(_1)
    _4 := _primIO
    _5 := _4(_3)
    return _5
    }
    return _0;
}

var _putStr = _fn1()

// printChar :: Char -> IO Unit
func _fn2() func (int) ADT {
    _0 := func (_c int) ADT {
    _1 := _c
    _2 := ___printChar
    _3 := _2(_1)
    _4 := _primIO
    _5 := _4(_3)
    return _5
    }
    return _0;
}

var _printChar = _fn2()

// primIO :: Unit -> IO Unit
func _fn3() func (Unit) ADT {
    _0 := func (_a Unit) ADT {
    _1 := func (_w World) ADT {
    _2 := _a
    _3 := _w
    _4 := _cnvIOHelper
    _5 := _4(_3)
    _6 := _5(_2)
    return _6
    }
    _7 := _cnvIO
    _8 := _7(_1)
    return _8
    }
    return _0;
}

var _primIO = _fn3()

// _lmmain :: IO Unit
func _fn4() ADT {
    _0 := "test"
    _1 := _putStrLn
    _2 := _1(_0)
    return _2;
}

var __lmmain = _fn4()

// bind :: IO Unit -> (Unit -> IO Unit) -> IO Unit
func _fn5() func (ADT) func (func (Unit) ADT) ADT {
    _0 := func (_a ADT) func (func (Unit) ADT) ADT {
    _1 := func (_f func (Unit) ADT) ADT {
    _2 := func (_w World) ADT {
    _3 := _a
    
    _4 := _3.t
    var _5 ADT
    switch (_4) {
    case _cniIO:
    
    
    _6 := _3.v.(_cnsIO)._0
    _a_ := _6
    
    _7 := _w
    _8 := _a_
    _9 := _8(_7)
    
    _10 := _9.t
    var _11 ADT
    switch (_10) {
    case _cniIOHelper:
    
    
    _12 := _9.v.(_cnsIOHelper)._0
    _w2 := _12
    
    _13 := _9.v.(_cnsIOHelper)._1
    _g := _13
    
    _14 := _g
    _15 := _f
    _16 := _15(_14)
    
    _17 := _16.t
    var _18 ADT
    switch (_17) {
    case _cniIO:
    
    
    _19 := _16.v.(_cnsIO)._0
    _b_ := _19
    
    _20 := _w2
    _21 := _b_
    _22 := _21(_20)
    _18 = _22
    }
    _11 = _18
    }
    _5 = _11
    }
    return _5
    }
    _23 := _cnvIO
    _24 := _23(_2)
    return _24
    }
    return _1
    }
    return _0;
}

var _bind = _fn5()