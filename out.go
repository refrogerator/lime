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

var _cniIOHelper = 0
type _cnsIOHelper struct {
    _0 World
    _1 Unit
}
var _cnvIOHelper = func (_0 World) func (Unit) ADT {
    return func (_1 Unit) ADT {
    return ADT{_cniIOHelper,_cnsIOHelper{_0,_1}}}}

// putStrLn :: String -> IO Unit
func _fn0() func (string) func (World) ADT {
    _0 := func (_s string) func (World) ADT {
    _1 := func (_a Unit) func (World) ADT {
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
func _fn1() func (string) func (World) ADT {
    _0 := func (_s string) func (World) ADT {
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
func _fn2() func (int) func (World) ADT {
    _0 := func (_c int) func (World) ADT {
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

// primIO :: Unit -> World -> IOHelper Unit
func _fn3() func (Unit) func (World) ADT {
    _0 := func (_a Unit) func (World) ADT {
    _1 := func (_w World) ADT {
    _2 := _a
    _3 := _w
    _4 := _cnvIOHelper
    _5 := _4(_3)
    _6 := _5(_2)
    return _6
    }
    return _1
    }
    return _0;
}

var _primIO = _fn3()

// _lmmain :: IO Unit
func _fn4() func (World) ADT {
    _0 := "test"
    _1 := _putStrLn
    _2 := _1(_0)
    return _2;
}

var __lmmain = _fn4()

// bind :: (World -> IOHelper Unit) -> (Unit -> World -> IOHelper Unit) -> World -> IOHelper Unit
func _fn5() func (func (World) ADT) func (func (Unit) func (World) ADT) func (World) ADT {
    _0 := func (_a func (World) ADT) func (func (Unit) func (World) ADT) func (World) ADT {
    _1 := func (_f func (Unit) func (World) ADT) func (World) ADT {
    _2 := func (_w World) ADT {
    _3 := _w
    _4 := _a
    _5 := _4(_3)
    
    _6 := _5.t
    var _7 ADT
    switch (_6) {
    case _cniIOHelper:
    
    
    _8 := _5.v.(_cnsIOHelper)._0
    _w2 := _8
    
    _9 := _5.v.(_cnsIOHelper)._1
    _g := _9
    
    _10 := _w2
    _11 := _g
    _12 := _f
    _13 := _12(_11)
    _14 := _13(_10)
    _7 = _14
    }
    return _7
    }
    return _2
    }
    return _1
    }
    return _0;
}

var _bind = _fn5()