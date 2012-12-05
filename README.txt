SKI Combinator Calculus is a system with three operations which do the
following:
    s f g x = g x (f x)
    k x y = x
    i x = x
The only valid syntax of this lanugage is composed of those functions, (), and
literals (strings of many letters prefaced by a quote). Examples:
    > (i k)
    => k
    > (k 'a 'b)
    => 'a
    > (s i i 'a)
    => ('a 'a)
When not enough arguments are supplied to one of these functions, the arguments
are evaluated and then return as an argument to the base function. Examples:
    > (k (i 'a))
    => (k 'a)
    > (i)
    => i
    > (s i k (i k))
    => (k (k k))
When too many arguments are given, the function is applied to the number that it
takes, then the resulting function evalates the remainder of the arguments.
Examples:
    > (i k 'x 'y)
    => 'x
    > (k i s s)
    => s
    > (i k (s i k k))
    => (k (k (k k)))
To use the interactive interpreter, just call `runhaskell SKI.hs`. To run a
file, supply the file at the end (`runhaskell SKI.hs test.ski`).
Enjoy!
