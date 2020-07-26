module code;

import std.io showing (print, println);
import lib showing (hello_world);


let pub x = "hello there!" :: String; // some comment here
let y = "inferred";


pub data Maybe(a) {
    Nothing,
    Just(a),
    Named {
        a :: a,
        b :: String
    }
}


pub data List(a) {
    Nil,
    Cons(a, List(a))
}


pub record SomeRecord(a) {
    value :: a -> a ;
}


pub class (Ord a) => Seq(a) {
    length :: a -> Integer ;
}


instance Seq => List(a) {
    let length(list) {
        when list {
            Nil => 0,
            List(head, tail) => 1 + length(tail)
        }
    }
}


data NamedRecord() {
    NamedRecord {
        x :: Maybe(Integer),
        y :: String -> (Integer, Integer, (Integer, Float))
    }
}


f :: [String] -> Bool;
let f(xs) {
    for x <- xs {
        println(x);
    }

    while True {
        println(x);
    }

    if length(xs) > 0 { True } else { False }
}

add :: (Number(a), Add(a)) => a -> a -> a;
let add(x, y) {
    return x + y;
}


let ==(a, b) {
    ...
}


main :: [String] -> ();
let main(args) {

    println("Hi");
    println(f(args));
    println(10);
    hello_world();

    let a :: Integer;
    let mut a = 15;
    a = 16;
    println(a);

    let mut s = SomeRecord { value: 1 }; // Comment
    s = s { value: 2 };
    println(s.value);

    let lambda = \(x, y) -> println(x);

    let equals = a == b;

    let other_lambda = \(x, y) -> {
        println("it is lambda");
        x + y
    };

}
