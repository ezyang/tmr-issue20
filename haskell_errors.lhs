\documentclass{tmr}

%include polycode.fmt

\author{Jan Stolarek\email{jan.stolarek@@p.lodz.pl}}
\title{Understanding Basic Haskell Error Messages}
\date{}

\hyphenation{ex-hau-sti-ve}

\begin{document}
\begin{introduction}
Haskell is a~language that differs greatly from the mainstream languages of today. No side-effects, strong typing system, lack of loops and many other features make it a~bit hard to learn for programmers familiar only with imperative programming. One of the main problems I~faced on my initial contact with Haskell were unclear error messages. Later, seeing some discussions on \#haskell, I~noticed that I~wasn't the only one. Correcting a~program without understanding error messages is certainly not an easy task. In this tutorial I~aim to remedy this problem by explaining how to understand Haskell's error messages. I~will present code snippets that generate errors, following them with explanations and solutions. I~used GHC 7.4.1 and Haskell Platform 2012.2.0.0~\cite{he:platform} for demonstration. I~assume reader's working knowledge of GHCi. I~also assume that reader knows how data types are constructed, what are type classes and how they are used. Knowledge of monads and language extensions is not required.
\end{introduction}

\section{Compilation errors}
\subsection{Simple mistakes}
I'll begin by discussing some simple mistakes that you are likely to make because you're not yet used to the Haskell syntax. Some of these errors will be similar to what you know from other languages. Other will be Haskell specific.

Let's motivate our exploration of Haskell errors with a~short case study. Standard \verb+Prelude+ provides some basic list operating functions like \verb+head+, \verb+tail+, \verb+init+ and \verb+last+. These are \emph{partial functions}. They work correctly only for a~subset of all possible inputs. Any of them will explode in your face when you apply it to an empty list:

\begin{Verbatim}
ghci> head []
*** Exception: Prelude.head: empty list
\end{Verbatim}
You end up with an exception and there's not much you can do about it. However, it is possible to create a~safe version of \verb+head+ function which will work for all possible inputs without throwing an error. Such a function is called a \emph{total function}.

To reach our goal we will use \verb+Maybe+ data type. The function will return \verb+Nothing+ when given an empty list. Otherwise it will return the result of applying \verb+head+ to \verb+xs+ wrapped in \verb+Just+. Here's our completely bugged first attempt:

\begin{Verbatim}
safeHead [a] -> Maybe a
safeHead [] = Nothing
safeHead xs = Maybe head xs
\end{Verbatim}
First line is intended to be a type annotation. Remaining two lines is the actual code. Loading it into \verb+ghci+ produces a~\emph{parse error}:

\begin{Verbatim}
ghci> :l tmr.hs
[1 of 1] Compiling Main             ( tmr.hs, interpreted )

tmr.hs:1:14: parse error on input `->'
Failed, modules loaded: none.
\end{Verbatim}
Parse error means that the program violates Haskell syntax. The error message starts in the third line (not counting the blank one). It begins with name of the file and exact location of the error expressed as line and column numbers separated with colons. In this case the error is in the first line, which means our intended type annotation. The compiler complains about \verb+->+ which was not expected to appear in this place. The problem is that the name of a function should be separated from type annotation with \verb+::+. If there's no \verb+::+, the compiler assumes we are defining a~function body, treats \verb+[a]+ as a pattern binding and expects that it is followed either by \verb+=+ or \verb+|+ (a guard). Of course there are lots of other ways to cause parse error, but all are dealt with in the same way: checking what syntax Haskell expects for a particular expression. Let's fix this particular mistake by adding the missing \verb+::+ symbol:

\begin{Verbatim}
safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead xs = Maybe head xs
\end{Verbatim}
and see what happens:

\begin{Verbatim}
ghci> :r
[1 of 1] Compiling Main             ( tmr.hs, interpreted )

tmr.hs:3:15: Not in scope: data constructor `Maybe'
Failed, modules loaded: none.
\end{Verbatim}
Now that the type annotation is correct, it turns out that there's an error in third line. \emph{Not in scope} means that some variable, function or, as implied in this case, a~data constructor, is not known to the compiler. You certainly know this kind of error from different programming languages. Let's look at the definition of \verb+Maybe+ data type to understand what is wrong:

\begin{Verbatim}
data Maybe a = Nothing || Just a
  deriving (Eq, Ord, Generic)
\end{Verbatim}
In this definition \verb+Maybe+ is a~\emph{type constructor}, while \verb+Nothing+ and \verb+Just+ are \emph{value constructors}, also referred to as \emph{data constructors}. This means that when you want to create a~value of a~given type you must use either \verb+Nothing+ or \verb+Just+. In our code we have mistakenly used \verb+Maybe+. That's what the compiler complained about: there is no data constructor called \verb+Maybe+, though there is a~type constructor called \verb+Maybe+. Let's replace that \verb+Maybe+ with \verb+Just+, which of course was our original intention:

\begin{Verbatim}
safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead xs = Just head xs
\end{Verbatim}

\begin{listing}[b]
\begin{Verbatim}
ghci> :r
[1 of 1] Compiling Main             ( tmr.hs, interpreted )

tmr.hs:3:15:
    The function `Just' is applied to two arguments,
    but its type `a0 -> Maybe a0' has only one
    In the expression: Just head xs
    In an equation for `safeHead': safeHead xs = Just head xs
Failed, modules loaded: none.
\end{Verbatim}
\caption{Applying data constructor to too many arguments.}\label{l:too_many_arguments}
\end{listing}
\noindent
The previous error is gone, only to produce a~new one, shown in Listing \ref{l:too_many_arguments}. This time the first two lines of the error message are quite explanatory. Every data constructor is in fact a~function. The definition of \verb+Maybe+ data type shows that the \verb+Just+ value constructor takes one parameter, while in our code we have mistakenly passed two parameters: \verb+head+ and \verb+xs+. From a formal point of view this is a type system error. These will be discussed in the next subsection, but this one is here because it is very easy to make if you forget that function application is left-associative. This means that \verb+Just head xs+ is the same as \verb+((Just head) xs)+. We can use either parentheses or function composition and \verb+$+ operator to override the default associativity. I've elaborated on the second approach on my blog~\cite{he:yalb} so I~will not go into explaining it here and we'll just use the parentheses:

\begin{Verbatim}
safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead xs = Just (head xs)
\end{Verbatim}
Surprise, surprise! The code finally compiles:

\begin{Verbatim}
ghci> :r
[1 of 1] Compiling Main             ( tmr.hs, interpreted )
Ok, modules loaded: Main.
\end{Verbatim}
Our function \verb+safeHead+ should now work as intended. Just remember that there already is a library called \verb+safe+~\cite{he:safe}, which provides different safe versions of originally unsafe functions, so there's no need to actually implement your own.

Let's recall the not-in-scope error that was shown earlier. It was caused by using a function that doesn't exist. A common situation is using an existing function, but not importing it. Let's look at a~simple piece of code in which a~different name is given to an already existing function:

\begin{Verbatim}
module Main where
  sortWrapper xs = sort xs
\end{Verbatim}
Loading this code also produces the \verb+not in scope+ error:

\begin{Verbatim}
ghci> :r
[1 of 1] Compiling Main             ( tmr.hs, interpreted )

tmr.hs:2:22:
    Not in scope: `sort'
    Perhaps you meant `sqrt' (imported from Prelude)
Failed, modules loaded: none.
\end{Verbatim}
GHCi doesn't know \verb+sort+ function, but it knows \verb+sqrt+ from standard \verb+Prelude+ and suggests that we might have made a typo. The \verb+sort+ function we wanted to use is part of the \verb+Data.List+ package. Since this is not in the standard \verb+Prelude+ it must be explicitly imported:

\begin{Verbatim}
module Main where
  import Data.List (sort)
  sortWrapper xs = sort xs
\end{Verbatim}
or even more explicitly:
\begin{Verbatim}
module Main where
  import qualified Data.List as Lists (sort)
  sortWrapper xs = Lists.sort xs
\end{Verbatim}
So if you get a not-in-scope error, but you know that the function really exists, the missing import is most likely the culprit. In case you don't know the name of the package in which a~certain function is located you can use hoogle~\cite{he:hoogle}.

A common mistake made by beginners is attempt to invoke functions directly from the module. A typical example might look like this
\begin{Verbatim}
module Main where
fact :: Int -> Int
fact 0 = 1
fact n = n * fact ( n - 1 )

print (fact 5)
\end{Verbatim}
This code gives a correct definition of factorial function and then tries to print the result of invoking this function. This however is incorrect and results in a following error:

\begin{Verbatim}
ghci> :r
[1 of 1] Compiling Main             ( tmr.hs, interpreted )

tmr.hs:6:1: Parse error: naked expression at top level
Failed, modules loaded: none
\end{Verbatim}
In Haskell everything in the module must be enclosed within function definitions. It is forbidden to call functions directly in a way shown in the above example. There are two possible solutions. First one is the removal of call to \verb+print (fact 5)+, loading module into GHCi and invoking \verb+fact 5+ from interactive prompt. Second solution, if you want your program to run as a separate executable, is to enclose the call to \verb+print (fact 5)+ within \verb+main+ function, which is an entry point to every Haskell program:

\begin{Verbatim}
module Main where
fact :: Int -> Int
fact 0 = 1
fact n = n * fact ( n - 1 )

main = print (fact 5)
\end{Verbatim}
This code can be compiled and executed as a standalone executable:

\begin{Verbatim}
[jan.stolarek@@GLaDOS : ~] ghc --make tmr.hs
[1 of 1] Compiling Main             ( tmr.hs, tmr.o )
Linking tmr ...
[jan.stolarek@@GLaDOS : ~] ./tmr
120
\end{Verbatim}
Naked expression error can also be easily caused by accidentally typing \verb+Import+ instead of \verb+import+:

\begin{Verbatim}
module Main where
    Import Data.List
    sortWrapper xs = sort xs
\end{Verbatim}
This results in an error, because compiler treats \verb+Import+ as an application of data constructor to parameter \verb+Data.List+. Remember -- capitalization matters in Haskell!

\subsection{Type system errors}

Complexity of Haskell's strong static type system can cause problems to beginners. One of the bigger challenges you'll have to face is getting very different messages for seemingly identical errors. In this subsection I will show what kind of type system errors you may encounter and explain what concepts underlie each of them.

\begin{listing}[ht]
\begin{Verbatim}
ghci> True && 1

 <interactive>:23:9:
    No instance for (Num Bool)
      arising from the literal `1'
    Possible fix: add an instance declaration for (Num Bool)
    In the second argument of `(&&)', namely `1'
    In the expression: True && 1
    In an equation for `it': it = True && 1
\end{Verbatim}
\caption{Non-exhaustive pattern in a guard.}\label{l:type_class_error1}
\end{listing}

I'll begin exploration of errors related to type system by a~very simple example shown in Listing \ref{l:type_class_error1}. It demonstrates what can happen when you pass parameters of the wrong type to a function. Let's break that scarry error message into elementary components. The \verb+<interactive>:23:9:+ says that the error occurred in the interactive mode (GHCi) on the 9th column in the 23rd command entered in the prompt since it was started. The next two lines -- \verb+No+ \verb+instance+ \verb+for+ \verb+(Num+ \verb+Bool)+ \verb+arising+ \verb+from+ \verb+the+ \verb+literal+ \verb+`1'+ -- are the key to understanding the error message. Obviously there's a~problem with \verb+1+ literal.

To understand what exactly happens, you must know that numeric types in Haskell are polymorphic. This means that when you write an integer literal in Haskell -- just as we've written \verb+1+ in our example -- it can be interpreted as different type depending on the context it is used in. Here's an example:

\begin{Verbatim}
ghci> 1 :: Int
1
ghci> 1 :: Double
1.0
\end{Verbatim}
In the first case literal \verb+1+ is interpreted as \verb+Int+ and in the the second one as \verb+Double+. Both of these uses are correct and don't cause type error. This is done by the implicit call to \verb+fromInteger+ function defined in the \verb+Num+ class. This means that \verb+True && 1+ and \verb+True && fromInteger 1+ are equivalent expressions. Let's check the type signature of \verb+fromInteger+:

\begin{Verbatim}
ghci> :t fromInteger
fromInteger :: Num a => Integer -> a
\end{Verbatim}
This means that \verb+fromInteger+ takes an \verb+Integer+ and returns a~value of \emph{any} type \verb+a+, with a~restriction that the type \verb+a+ belongs to the \verb+Num+ type class. What type exactly should be returned? That depends on the context in which \verb+fromInteger+ was applied, but it allows integer literal to become an instance of a type belonging to \verb+Num+ type class.

With this knowledge we can go back to our example in which integer literal \verb+1+ was used as a~parameter to \verb+(&&)+ function. This function takes two \verb+Bool+ parameters and returns a~single \verb+Bool+:

\begin{Verbatim}
ghci> :t (&&)
(&&) :: Bool -> Bool -> Bool
\end{Verbatim}
\begin{listing}[t]
\begin{Verbatim}
ghci> :i Bool
data Bool = False || True -- Defined in GHC.Bool
instance Bounded Bool -- Defined in GHC.Enum
instance Enum Bool -- Defined in GHC.Enum
instance Eq Bool -- Defined in GHC.Classes
instance Ord Bool -- Defined in GHC.Classes
instance Read Bool -- Defined in GHC.Read
instance Show Bool -- Defined in GHC.Show
\end{Verbatim}
\caption{Information about \verb+Bool+ data type.}\label{l:bool}
\end{listing}
which means that in order for literal \verb+1+ to be a valid parameter to \verb+(&&)+, the type returned by \verb+fromInteger+ should be \verb+Bool+. There is one problem though. The \verb+Bool+ type is not an instance of \verb+Num+ type class, as shown in Listing~\ref{l:bool}. It should be however, since the \verb+fromInteger+ function imposes a~constraint that its return value belongs to \verb+Num+ type class. This is exactly what the error message said! The parameter \verb+1+ that we passed to \verb+(&&)+ should be a~\verb+Bool+, because \verb+(&&)+ requires it, but \verb+Bool+ is not an instance of \verb+Num+ and so a type error occurs. The next line of the error message suggests a~solution: \verb+Possible+ \verb+fix:+ \verb+add+ \verb+an+ \verb+instance+ \verb+declaration+ \verb+for+ \verb+(Num+ \verb+Bool)+. Indeed, if we made \verb+Bool+ type an instance of \verb+Num+ type class the problem would be gone. In some cases this may be the solution, depending on what you want. Deriving \verb+Bool+ to be an instance of \verb+Num+ is certainly a good exercise that you can try out. In many other cases however this error means you wanted something different than you actually wrote. Let's assume here that we wanted \verb+1+~to denote logical truth. Fixing that is easy:

\begin{Verbatim}
ghci> True && True
True
\end{Verbatim}
The three remaining lines of the error message provide information about expression that caused the error:

\begin{Verbatim}
    In the second argument of `(&&)', namely `1'
    In the expression: True && 1
    In an equation for `it': it = True && 1
\end{Verbatim}
The \verb+it+ value is equal to the value of last expression evaluated in GHCi.

The constraints imposed by the type classes propagate in the type system. Here's a~simple example that demonstrates that. Let's say we want to write a~function that tells if the two parameters passed to it are equal:

\begin{Verbatim}
isEq :: a -> a -> Bool
isEq x y = x == y
\end{Verbatim}
Our \verb+isEq+ function expects two parameters that are of the same type and returns a~\verb+Bool+. This looks perfectly reasonable, but loading that code into GHCi results with an error shown in Listing~\ref{l:eq_error}.

\begin{listing}[h]
\begin{Verbatim}
ghci> :l tmr.hs
[1 of 1] Compiling Main             ( tmr.hs, interpreted )

tmr.hs:2:14:
    No instance for (Eq a)
      arising from a use of `=='
    In the expression: x == y
    In an equation for `isEq': isEq x y = x == y
Failed, modules loaded: none.
\end{Verbatim}
\caption{Error caused by a lack of type class constraint.}\label{l:eq_error}
\end{listing}
\noindent
The first two lines of this message are the most important. They say that type \verb+a+~should be an instance of \verb+Eq+ type class and that this requirement is imposed by the use of \verb+==+ function. Let's inspect the type of \verb+(==)+:

\begin{Verbatim}
ghci> :t (==)
(==) :: Eq a => a -> a -> Bool
\end{Verbatim}
Indeed the \verb+(==)+ function expects that its arguments are instances of \verb+Eq+ type class. That constraint was propagated to \verb+isEq+ function, which uses \verb+(==)+. We must therefore add a~type class constraint to parameters of \verb+isEq+:

\begin{Verbatim}
isEq :: Eq a => a -> a -> Bool
isEq x y = x == y
\end{Verbatim}
This fixes the problem. Let's now see what happens if we try to compare parameters of two different types, both belonging to \verb+Eq+ type class:

\begin{Verbatim}
isEq :: (Eq a, Eq b) => a -> b -> Bool
isEq x y = x == y
\end{Verbatim}

\begin{listing}[t]
\begin{Verbatim}
[1 of 1] Compiling Main             ( tmr.hs, interpreted )

tmr.hs:2:17:
  Could not deduce (b ~ a)
  from the context (Eq a, Eq b)
    bound by the type signature for
             isEq :: (Eq a, Eq b) => a -> b -> Bool
    at tmr.hs:2:1-17
    `b' is a rigid type variable bound by
      the type signature for isEq :: (Eq a, Eq b) => a -> b -> Bool
      at tmr.hs:2:1
    `a' is a rigid type variable bound by
      the type signature for isEq :: (Eq a, Eq b) => a -> b -> Bool
      at tmr.hs:2:1
  In the second argument of `(==)', namely `y'
  In the expression: x == y
  In an equation for `isEq': isEq x y = x == y
Failed, modules loaded: none.

\end{Verbatim}
\caption{Comparing two different instances of \verb+Eq+ type class.}\label{l:rigid_types}
\end{listing}
\noindent
The error message is shown in Listing~\ref{l:rigid_types}. There are a~few important things in this message. The \verb+(b ~ a)+ notation means that types \verb+a+ and \verb+b+ are the same. The compiler \verb+Could not deduce (b ~ a)+, which means that \verb+a+ and \verb+b+ should be identical, but the type definition we provided does not guarantee it. The requirement of types \verb+a+ and \verb+b+ being of the same type is a~direct consequence of a~type signature of \verb+(==)+ which, as we recall, requires its parameters to be of the same type. The \verb+rigid type variable+ messages mean that our types \verb+a+ and \verb+b+ have been directly specified by the type annotation~\cite{he:pj} and the compiler is not free to unify them. We already saw the correct version of this code, but we can also make it work in a~different way:

\begin{Verbatim}
{-# LANGUAGE TypeFamilies #-}

isEq :: (Eq a, Eq b, a ~ b) => a -> b -> Bool
isEq x y = x == y
\end{Verbatim}
Enabling \verb+TypeFamilies+ language extension and adding \verb+a ~ b+ type constraint informs the compiler that \verb+a+ and \verb+b+ can be unified to be the same type. The above code is for demonstration purposes, it makes no real sense to write the type declaration in this way, since \verb+a+ and \verb+b+ will be unified into one type. Loading this into GHCi and checking type of \verb+isEq+ will show that it's actually \verb+isEq+ \verb+::+ \verb+Eq b+ \verb+=>+ \verb+b+ \verb+->+ \verb+b+ \verb+->+ \verb+Bool+. The two distinct types introduced deliberately are gone because the compiler was allowed to unify them.

When you begin working with type classes you may find it difficult to figure out to which type classes the parameters should be constrained. Here's where the Haskell's type inference comes into play. You can aid yourself by writing down your function without type declaration, loading it into GHCi and using Haskell's type inference to determine function's type for you. This can be done using the \verb+:t+~command on a function, as we've already seen in previous examples. Suppose the \verb+isEq+ function was written without type declaration. Here's what Haskell infers about the \verb+isEq+'s type:

\begin{Verbatim}
ghci> :t isEq
isEq :: Eq a => a -> a -> Bool
\end{Verbatim}
This is correct type signature and you can simply copy it to your code.

The above examples demonstrated what will happen when type signature does not contain appropriate class restrictions. Let's now see what happens when a~function's type signature is inconsistent with function's implementation. Assume we want to write a~function that returns a~first letter from a~\verb+String+. We could want to have a type signature like this:

\begin{Verbatim}
getFirstLetter :: String -> String
\end{Verbatim}
This means that \verb+getFirstLetter+ function takes a value of type \verb+String+ and returns a value of type \verb+String+. For example, if we pass \verb+"Some string"+ as a~parameter then \verb+getFirstLetter+ will return value \verb+"S"+. Since \verb+String+ in Haskell is a~synonym for \verb+[Char]+ (list of \verb+Char+s) we could use \verb+head+ function to take the first element of a~\verb+String+. Our \verb+getFirstLetter+ function would then look like this:

\begin{Verbatim}
getFirstLetter :: String -> String
getFirstLetter = head
\end{Verbatim}
The \verb+head+ function has type \verb+[a] -> a+, which means it takes a list of some type \verb+a+~and returns a~single element of type \verb+a+, not a~list. However, the type signature for \verb+getFirstLetter+ requires that the return argument be a list of \verb+Char+s. Therefore the provided signature is inconsistent with the actual return type of the function. Haskell will notice that when you try to load the code into GHCi and report an error as shown in Listing~\ref{l:mismatch_error}. GHCi says that the annotation provided by the user means that user expected the type \verb+[Char]+, but the actual inferred type is \verb+Char+. There are two possible fixes for this problem. First, if we are really fine with getting a~single \verb+Char+ instead of a~\verb+String+, we can change the type signature to \verb+String -> Char+ (or \verb+[Char] -> Char+, since it's the same). On the other hand, if we really expect to get a~\verb+String+ from our function, we need to change the implementation. In this case the result of \verb+head+ should be wrapped in a~list, which can be done like this:
\begin{Verbatim}
getFirstLetter :: String -> String
getFirstLetter xs = [head xs]
\end{Verbatim}
or using a~point-free style:

\begin{Verbatim}
getFirstLetter :: String -> String
getFirstLetter = (: []) . head
\end{Verbatim}
\begin{listing}[tb]
\begin{Verbatim}
ghci> :r
[1 of 1] Compiling Main             ( tmr.hs, interpreted )

tmr.hs:2:18:
    Couldn't match expected type `String' with actual type `Char'
    Expected type: String -> String
      Actual type: [Char] -> Char
    In the expression: head
    In an equation for `getFirstLetter': getFirstLetter = head
Failed, modules loaded: none
\end{Verbatim}
\caption{Expected and actual type mismatch error.}\label{l:mismatch_error}
\end{listing}

The type mismatch error is probably the one you'll be seeing most often. There's really no way of anticipating all possible situations where it might occur. The above case was easy, because the implementation of a function was correct and the only problem was fixing the type annotation. Most often however you'll end up in situations where you've written some code and the type-checker complains that this code is type-incorrect. Here's an example of how this may look like:

\begin{listing}[t]
\begin{Verbatim}
ghci> :r
[1 of 1] Compiling Main             ( tmr.hs, interpreted )

tmr.hs:4:26:
    Couldn't match expected type `Int' with actual type `Char'
    In the first argument of `(*)', namely `acc'
    In the first argument of `(+)', namely `acc * 10'
    In the expression: acc * 10 + digitToInt x
Failed, modules loaded: none.
\end{Verbatim}
\caption{Expected and actual type mismatch error.}\label{l:mismatch_error2}
\end{listing}

\begin{Verbatim}
import Data.Char

asInt :: String -> Int
asInt = foldl (\x acc -> acc * 10 + digitToInt x) 0
\end{Verbatim}
This code takes a~\verb+String+ representation of a~number and turns it into an \verb+Int+. Well, almost. If you try to compile it you'll get an error about mismatching types, as shown in Listing~\ref{l:mismatch_error2}. The message says that \verb+acc+ (accumulator) is of the wrong type -- \verb+Char+ instead of \verb+Int+. In such cases it might not be immediately obvious what exactly is wrong. The only way to deal with such errors is to inspect the incorrect expressions. In this example the code isn't really complicated so this will be easy. The \verb+asInt+ function uses only \verb+foldl+ and one anonymous lambda function. Let's begin by checking the type signature of \verb+foldl+:

\begin{Verbatim}
ghci> :t foldl
foldl :: (a -> b -> a) -> a -> [b] -> a
\end{Verbatim}
Type signature says that \verb+foldl+ takes three parameters. These parameters are a function of type \verb+a -> b -> a+, an accumulator of type \verb+a+ and a list containing elements of type \verb+b+. By looking at the type variables in this signature -- that is \verb+a+~and \verb+b+ -- you can see that the first parameter to folding function, the accumulator and the return value of \verb+foldl+ are of the same type \verb+a+. In this case that type is \verb+Int+, since we passed \verb+0+ as the accumulator and also expect the return value of \verb+asInt+ function to be \verb+Int+. The compiler complains however that variable \verb+acc+ used as parameter to \verb+(*)+ is of type \verb+Char+, not \verb+Int+. The parameters to the lambda function are \verb+x+ and \verb+acc+. According to type signature of \verb+foldl+, \verb+x+ should be of type \verb+Int+, because it is of the same type as the accumulator. On the other hand \verb+acc+ is of the same type as elements of the list passed as third parameter to \verb+foldl+. In this example the list is of type \verb+[Char]+ so \verb+acc+, being a single element, is of type \verb+Char+. Well, this should be the other way around: \verb+acc+ should be of type \verb+Int+; \verb+x+ should be of type \verb+Char+. This error was caused by writing the parameters of lambda in incorrect order. The correct code therefore is:

\begin{Verbatim}
import Data.Char

asInt :: String -> Int
asInt = foldl (\acc x -> acc * 10 + digitToInt x) 0
\end{Verbatim}

In some cases you will get an error message that doesn't specify concrete type. Consider example shown in Listing~\ref{l:wrong_param}. The \verb+isPrefixOf+ function expects its both parameters to be lists of the same type:
\begin{listing}[t]
\begin{Verbatim}
ghci> :m + Data.List
ghci> 'A' `isPrefixOf` "An error"

 <interactive>:8:1:
    Couldn't match expected type `[a0]' with actual type `Char'
    In the first argument of `isPrefixOf', namely 'A'
    In the expression: 'A' `isPrefixOf` "An error"
    In an equation for `it': it = 'A' `isPrefixOf` "An error"
\end{Verbatim}
\caption{Passing argument of wrong type to a function.}\label{l:wrong_param}
\end{listing}

\begin{Verbatim}
ghci> :t isPrefixOf
isPrefixOf :: Eq a => [a] -> [a] -> Bool
\end{Verbatim}
As soon as compiler realizes that the first parameter is not a~list, it complains. It doesn't even reach the second parameter to analyze it and infer that the first list should contain \verb+Char+s. That is the reason why it uses a~generic list \verb+[a0]+. Listing~\ref{l:wrong_param2} shows result of swapping the two parameters. This time compiler is able to infer the exact type of elements required in the second list because by the time it reaches the second parameter it has already analyzed the first one and it knows that it was a~list of \verb+Char+s.

\begin{listing}[htb]
\begin{Verbatim}
ghci> "An error" `isPrefixOf` 'A'

 <interactive>:9:25:
    Couldn't match expected type `[Char]' with actual type `Char'
    In the second argument of `isPrefixOf', namely 'A'
    In the expression: "An error" `isPrefixOf` 'A'
    In an equation for `it': it = "An error" `isPrefixOf` 'A'
\end{Verbatim}
\caption{Passing argument of a wrong type to a function.}\label{l:wrong_param2}
\end{listing}

Another common error related to types is type ambiguity. I'll demonstrate it using the \verb+read+ function, which is used to convert \verb+String+ representation of some value to that value. The \verb+read+ function is defined in the \verb+Read+ type class and has type signature \verb+read :: Read a => String -> a+. All data types within standard Prelude, except function types and \verb+IO+ types, are instances of \verb+Read+. Every type that is an instance of \verb+Read+ type class provides its own definition of \verb+read+ function. This means that the compiler must know what resulting type to expect in order to call a~correct implementation of the \verb+read+ function. This is polymorphism that was already discussed when we talked about \verb+fromIntegral+ function. Listing~\ref{l:ambiguity} shows the error that occurs when the polymorphic type variable cannot be inferred to a~concrete type. In this case the compiler doesn't know which version of \verb+read+ function should be called and suggests to solve the problem by adding a~type signature. Let's try that:

\begin{Verbatim}
ghci> read "5.0" :: Double
5.0
\end{Verbatim}
It is important to provide correct type signature:

\begin{Verbatim}
ghci> read "5.0" :: Int
*** Exception: Prelude.read: no parse
\end{Verbatim}
This code produces an exception because implementation of \verb+read+ for \verb+Int+ instances of \verb+Read+ doesn't expect any decimal signs.

\begin{listing}[h]
\begin{Verbatim}
ghci> read "5.0"

 <interactive>:11:1:
   Ambiguous type variable `a0' in the constraint:
     (Read a0) arising from a use of `read'
   Probable fix: add a type signature that fixes these
     type variable(s)
   In the expression: read "5.0"
   In an equation for `it': it = read "5.0"
\end{Verbatim}
\caption{Type ambiguity.}\label{l:ambiguity}
\end{listing}

Type system can come into play in very unexpected moments. Let's play a little bit with \verb+id+, the identity function, which returns any parameter passed to it:

\begin{Verbatim}
ghci> id 1
1
ghci> id 0
0
ghci> id (-1)
-1
\end{Verbatim}

\begin{listing}[t]
\begin{Verbatim}
ghci> id -1

 <interactive>:16:4:
    No instance for (Num (a0 -> a0))
      arising from a use of `-'
    Possible fix: add an instance declaration for (Num (a0 -> a0))
    In the expression: id - 1
    In an equation for `it': it = id - 1
\end{Verbatim}
\caption{Incorrect usage of unary negation.}\label{l:negation}
\end{listing}
\noindent
Notice that the negative parameter was wrapped in parentheses. Had we neglected to do that it would result in a \verb+No instance for (Num (a0 -> a0))+ error, as shown in Listing~\ref{l:negation}. We've already seen this error before when we talked about polymorphism of integer literals. This time the compiler expects the \verb+a0 -> a0+ type to be an instance of \verb+Num+ type class. The \verb+a0 -> a0+ denotes a~function that takes an value of type \verb+a0+ and returns a value of the same type \verb+a0+. What seems strange is the fact that the compiler expects literal \verb+1+ to be of that type. The problem here is that the \verb+(-)+ sign is treated as an infix binary operator denoting subtraction, not as unary negation operator as we expected \footnote{Section 3.4 of Haskell 2010 Language Report~\cite{he:report} gives more detail on that matter.}. The strange error message blaming \verb+1+ literal is caused by the fact that \verb+(-)+ operator expects its parameters to be of the same type:
\begin{Verbatim}
ghci> :t (-)
(-) :: Num a => a -> a -> a
\end{Verbatim}

\noindent
The type of first parameter was inferred to be \verb+a0 -> a0+. Therefore the second parameter is also expected to be of type \verb+a0 -> a0+, but this type is not an instance of \verb+Num+ type class. Just as before, this error results from the implicit use of \verb+fromIntegral+ function. In this example however there's one thing that might be wondering you. From the type signature of \verb+(-)+ we can see that its parameters should belong to \verb+Num+ type class. The question is how can we be certain that this error is raised by the constraint in the \verb+fromIntegral+ function and not by the constraint in the \verb+(-)+ function itself? There's an easy way to verify this. Let's replace the second argument of \verb+(-)+ with a value of type \verb+String+. We use \verb+String+, because string literals don't implicitly call any function that would impose additional type constraints. The error that results in this case, shown in Listing~\ref{l:couldn_match}, says that compiler expects the second argument to be the same type as the first one, which is a~restriction resulting from the type signature of \verb+(-)+. There is no complaint about \verb+Num+ type class, which allows to infer that at this point \verb+Num+ type class constraint imposed by \verb+(-)+ hasn't been checked yet. Let's verify this conclusion by supplying \verb+(-)+ with two arguments of the same type that is not an instance of \verb+Num+. The result is shown in Listing~\ref{l:nif}. This time the compiler successfully verified that both parameters of \verb+(-)+ are of the same type \verb+a0 -> a0+ and it could go further to check if type class constraints are satisfied. However, the \verb+a0 -> a0+ type is not an instance of \verb+Num+ type class, hence the type class constraint is violated and \verb+No instance for+ error arises.
\begin{listing}[t]
\begin{Verbatim}
ghci> id - "a string"

 <interactive>:11:6:
   Couldn't match expected type `a0 -> a0' with actual type `[Char]'
   In the second argument of `(-)', namely `"a string"'
   In the expression: id - "a string"
   In an equation for `it': it = id - "a string"
\end{Verbatim}
\caption{Error caused by incorrect parameters that should be of the same type.}\label{l:couldn_match}
\end{listing}
\begin{listing}[h]
\begin{Verbatim}
ghci> id - id

 <interactive>:20:4:
    No instance for (Num (a0 -> a0))
      arising from a use of `-'
    Possible fix: add an instance declaration for (Num (a0 -> a0))
    In the expression: id - id
    In an equation for `it': it = id - id
\end{Verbatim}
\caption{Another \verb+No instance for (Num (Int -> Int))+ error.}\label{l:nif}
\end{listing}
\section{Some runtime errors}
We finally managed to get past the compilation errors. It was a lot of work, probably more than in other programming languages. That's another characteristic feature of Haskell. Strong type system moves much of the program debugging up front, into the compilation phase. You probably already heard that once a Haskell program compiles it usually does what the programmer intended. That's mostly true, but this doesn't mean that there are no runtime errors in Haskell. This section will discuss some of them.

Runtime errors in Haskell most often take the form of runtime exceptions. At the very beginning of this paper I showed you that some functions don't work for all possible inputs and can raise an exception:

\begin{Verbatim}
ghci> head []
*** Exception: Prelude.head: empty list
\end{Verbatim}
In languages like Java exceptions are your friend. They provide a stack trace that allows to investigate the cause of an error. It is not that easy in Haskell. Runtime exceptions don't give you any stack trace as it is not possible to do so in language with lazy evaluation. You are usually left only with a short error message and line number.

One of the commonly made errors resulting in runtime exceptions is non-exhaus\-tive patterns in constructs like function definitions or guards. Let's recall \verb+safeHead+ function that we've written in the first section:

\begin{Verbatim}
safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHEad xs = Just (head xs)
\end{Verbatim}
This function contains a typo. Function name in the third line is misspelled as \verb+safeHEad+ (notice the capital \verb+E+). This code compiles perfectly, but will result in an error if we try to call \verb+safeHead+ function for non-empty list:

\begin{Verbatim}
ghci> safeHead []
Nothing
ghci> safeHead [1,2,3]
*** Exception: tmr.hs:2:1-21: Non-exhaustive patterns in function
safeHead
\end{Verbatim}
The argument \verb+[1,2,3]+ passed to the function \verb+safeHead+ couldn't be matched against any pattern in the function definition. That's what \emph{non-exhaustive pattern} error means. This is the result of the typo I made in the third line. For Haskell compiler everything is perfectly fine. It treats the code as definition of two different function: \verb+safeHead+ matching only empty lists and \verb+safeHEad+ matching both empty and non-empty lists (though for empty list there will be a runtime exception resulting from applying \verb+head+ to an empty list).

The thing here is that we were unexpectedly hit by the non-exhaustive pattern problem during runtime because we're newbies and used GHCi to load modules and call functions interactively. In real life sources file containing the functions would be compiled. During the compilation process GHC could warn us about non-exhaustive patterns in \verb+safeHead+ function and lack of type signature for accidentally defined \verb+safeHEad+ function. Warnings can be enabled using \verb+-Wall+ command line switch, as shown in Listing~\ref{l:compilation_warnings}. Also, to compile the code you have to add \verb+main+ function so the whole code looks like this:

\begin{Verbatim}
module Main where

main :: IO ()
main = undefined

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHEad xs = Just (head xs)
\end{Verbatim}

\begin{listing}[ht]
\begin{Verbatim}
[jan.stolarek@@GLaDOS : ~] ghc -Wall --make tmr.hs
[1 of 1] Compiling Main             ( tmr.hs, tmr.o )

tmr.hs:7:1:
  Warning: Pattern match(es) are non-exhaustive
           In an equation for `safeHead': Patterns not matched: _ : _

tmr.hs:8:1:
  Warning: Top-level binding with no type signature:
           safeHEad :: forall a. [a] -> Maybe a
Linking tmr ...
\end{Verbatim}
\caption{Compilation warnings.}\label{l:compilation_warnings}
\end{listing}

The non-exhaustive pattern error can also occur in an incorrect guard, even though guards rely on evaluating conditions, not matching patterns. To illustrate this let's create our own signum function:

\begin{Verbatim}
mySignum :: Int -> Int
mySignum x
    || x > 0  = 1
    || x == 0 = 0
\end{Verbatim}
You see the error, don't you? Listing~\ref{l:wrong_signum} show what happens when we call \verb+mySignum+ function for negative arguments. This error is easily corrected by adding the third guard:

\begin{listing}[t]
\begin{Verbatim}
ghci> mySignum 1
1
ghci> mySignum 0
0
ghci> mySignum (-1)
*** Exception: tmr.hs:(14,1)-(16,16): Non-exhaustive patterns in
function mySignum
\end{Verbatim}
\caption{Non-exhaustive pattern in a guard.}\label{l:wrong_signum}
\end{listing}

\begin{Verbatim}
mySignum :: Int -> Int
mySignum x
    || x > 0     = 1
    || x == 0    = 0
    || otherwise = -1
\end{Verbatim}
The \verb+otherwise+ function is defined to always return \verb+True+, so the third guard will always evaluate if the previous guards didn't. The compiler would also give a~warning about non-exhaustive guards in a~same way it gives warning about non-exhaustive patterns in function definition.

\section{Summary}
This completes our overview of basic Haskell error messages. By now you should know how to read error messages and how to correct problems in your code that caused them. The above list is by no means an exhaustive one. There are a~lot of different errors you will encounter when you start using some more advanced features of Haskell. Perhaps one day you'll even write a~program that will cause \verb+My brain just exploded+ error.

\section{Acknowledgements}
I thank the great Haskell community at \#haskell IRC channel. They helped me to understand error messages when I~was beginning my adventure with Haskell. I~also thank Marek Zdankiewicz for reviewing draft version of this paper.

\bibliography{haskell_errors}

\end{document}
