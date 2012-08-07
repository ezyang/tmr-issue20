% -*- LaTeX -*-
\documentclass{tmr}

%include lhs2TeX.fmt
%include polycode.fmt
%include lineno.fmt

%format __(a) = "\underline{" a "}"
%format <$> = "\mathbin{\langle \! \$ \! \rangle}"
%format -< = "\mathbin{-\!\!\!\!\prec}"
%format (bd a) = "\bnd[" a "]"
%format `mplus` = "\mathbin{\oplus}"
%format >>> ="\mathbin{\succ\!\!\!\!\!\succ\!\!\!\!\!\succ}"
%format (proj a) = "\text{Proj}_{" a "}"
%format === = "\mathbin{\;\cong\;}"
%format <-> = "\mathbin{\;\leftrightarrow\;}"
%format ^>>= = "\mathbin{\accentset{\longrightarrow}{\bnd}}"
%format !>>= = "\mathbin{\tbind}"
%format (bdf a) = "\tbind[\mathrm{" a "}]"
%format (bdfh a) = "\bnd[hom\;\mathrm{" a "}]" 

%\usepackage[utf8x]{inputenc}
%\usepackage{prettyref}
\usepackage{amsthm}
\usepackage{accents}
\usepackage{ifthen}
\usepackage[all]{xy}



\DeclareGraphicsExtensions{png}

\author{Julian Porter\email{julian.porter@@porternet.org}}
\title{The MapReduce type of a Monad}

\newcommand{\authornote}[3]{{\color{#2} {\sc #1}: #3}}
\newcommand\ezy[1]{\authornote{edward}{blue}{#1}}

\numbersoff

\begin{document}



%\newtheorem{theorem}{Theorem}[subsection]
%\newtheorem{lemma}[theorem]{Lemma} 
\newtheorem{proposition}{Proposition}
%\newtheorem{corollary}[theorem]{Corollary}
%\newtheorem{definition}[theorem]{Definition}



\newcommand\tbind[1][MR]{\accentset{#1}{\bind}}
\def\lift{\mathrel{\mathchoice{\LL}{\LL}{\scriptsize\LL}{\LL}}}
\newcommand\LL[0]{\hspace{0.5em}\makebox[0pt][c]{\raisebox{-1.5pt}{---}}\makebox[0pt][c]{$\wedge$}\hspace{0.5em}}
\newcommand\bnd[1][]{\mathbin{\bind_{#1}}}

\begin{introduction}
\par MapReduce is a popular paradigm for distributed computing that is particularly important as a framework for massively parallel computation.  Many existing algorithms have been recast in terms of MapReduce, and new algorithms are being discovered in the paradigm.  The MapReduce framework imposes a fixed structure on algorithms: they consist of one or more stages, which are executed in series.  Each stage takes as input a list of key/value pairs and produces as output another such list.  In each stage, the input key/value pairs are sorted on the key value and divided into chunks, one per key value.  Each chunk is passed to a transformation function.  The resulting key/value pairs are gathered together and output as a list which can be passed to the next stage.

\par In this paper, we build on our earlier paper \cite{monad}, where we showed that MapReduce can be implemented as a kind of monad, with |>>=| corresponding to the composition of processing steps, and demonstrated a simple application (word count).  Here we show how this can be seen as the result of applying a general transformation, which is a kind of monad transformer, to the List monad.  Finally, we show that the familiar Haskell State monad can be seen as equivalent to the MapReduce type associated to the reader monad, of functions |s -> a| for fixed |s|.  This raises the question of how many other applications, apart from the obvious one of MapReduce itself, this transformer might have.

\par All of the ideas described in this paper have been implemented, and we have successfully demonstrated a MapReduce application using the transformed List monad to represent MapReduce.  A DARCS repository containing the code may be found at \cite{repository}.
\end{introduction}

\section{The idea}
\subsection{MapReduce in a nutshell}

\par We start with a brief summary of the MapReduce algorithm.  MapReduce takes a list of key/value pairs and processes them using a cluster of many processing nodes and one control node.  A MapReduce algorithm consists of a number of repetitions of a basic processing step, which has two parts:

\begin{enumerate}
\item \textbf{Map}. The control node distributes the list of values randomly between the processing nodes.  Each processing node applies a function called a \textit{mapper} to its values and produces a new list of key/value pairs, which it returns to the control node.
\item \textbf{Reduce}. This has three sub-parts:
\begin{enumerate}
\item The control node gathers the outputs from each of the processing nodes, concatenates them and sorts the resulting list by the key.  It divides the sorted list into chunks, one per key value and distributes the chunks among processing nodes, each chunk to one node.
\item Each processing node takes the chunk of key/value pairs it has been passed and uses them as input to a function called a \textit{reducer}.  This produces a list of values. 
\item The processing nodes return their output lists to the control node, which concatenates them to form a single list of values.
\end{enumerate}
\end{enumerate}

\noindent The control node then concatenates these output lists and proceeds to use them as input to the Map part of the next stage of processing.

\par Observe that we if we modify this by making Reduce produce not key/value pairs with random keys, then the random distribution of values among processing nodes in Map can be done with the distribution algorithm used in Reduce.  Therefore. we can treat Map and Reduce as being two instances of the same basic processing stage.

\subsection{Generalising MapReduce} 

\par Looking at this abstractly, what we have is a function
\begin{spec}
f :: a -> [(x,a)] -> [(y,b)]
\end{spec}
Here and elsewhere, |x|, |y| and |z| are value types and |a|, |b| and |c| are key types.  The first argument is the key value that selects a chunk of data and the second argument is the data, consisting of key/value pairs.  The function therefore corresponds to selecting a key, extracting the corresponding values and applying a mapper or reducer to them.

\par The mapper and reducer transform lists of values into lists of key/value pairs
\begin{spec}
[x] -> [(y,b)]
\end{spec}
where |x|, |y| and |b| are as above, so a stage takes a function (mapper or reducer) |[x]->[(y,b)]| and turns it into a function |a->[(x,a)->(y,b)]|, and so looks like
\begin{spec}
wrap :: ([x] -> [(y,b)]) -> a -> [(x,a)] -> [(y,b)]
\end{spec}

\par Looking at the algorithm as a whole, a MapReduce process is a function 
\begin{spec}
[(x,a)]->[(y,b)]
\end{spec}
where |x|, |y|, |a|, and |b| are as above.  Applying a mapper or reducer to the output of a MapReduce process gives another MapReduce process, so the act of adding a new stage to the end of a chain of processing can be described as
\begin{spec}
([(x,a)]->[(y,b)]) -> (b -> [(y,b)] -> [(z,c)]) -> [(x,a)] -> [(z,c)]
\end{spec}
where |z| is a value type and |c| is a key type, where the existing process is the first argument, the additional stage is the second, and the resulting combined process is the output. This looks like the signature of a monadic bind where the underlying type is a function |([(a,x)] -> [(b,y)])|. Therefore, MapReduce can be expressed as
\begin{spec}
... !>>= wrap map !>>= wrap reduce !>>= ...
\end{spec}
where |!>>=| is a suitable bind function.  

\section{A quasi-monadic approach to MapReduce}

\subsection{Quasi-monads}

\par A standard monad is a parameterised type |m u| with operations
\begin{spec}
return  :: u -> m u
(>>=)   :: m u -> (u -> m v) -> m v
\end{spec}
We need to generalise this.  First, we need monads with two parameters.  There are several ways of doing this; for our purposes, we take types |m t u| with operations
\begin{spec}
return  :: t -> m t t
(>>=)   :: m t u -> (u -> m u v) -> m u v
\end{spec}

\par We need one further generalisation.  The structure we will define obeys natural generalisations of the first two monad laws, but the third breaks down.  The third monad law is
\begin{spec}
(x >>= f) >>= g == x >>= (\y -> f y >>= g)
\end{spec}
To see why it must break down, let us se how we could interpret this in terms of the model set out above, where |x| is a MapReduce process and |f| and |g| correspond to individual stages.  

\par The left hand side has obvious meaning: |x| is a process to which we append first the stage |f| and then the stage |g|.  On the right hand side we have |\y -> f y !>>= g| which is the concatenation of two stages, but we cannot append |g| to |f| until we know what the output of |f| is, because a critical part of appending a stage to a process is dividing the process' output into chunks based on their key.  So for MapReduce, |\y -> f y !>>= g| is meaningless: the only meaningful order in which to apply stages is one at a time starting from the left:
\begin{spec}
(((x !>>= f) !>>= g) !>>= h) !>>= ...
\end{spec}
That is to say, stages cannot be composed together without an input being specified. Thus, we cannot expect MapReduce to obey the third monad law, because of the sorting and partitioning on keys that is a fundamental part of the algorithm.  

\par Looking at this another way, we can, of course, combine two stages, but the resulting entity is not itself a stage.  A single stage sorts on the key, passes the resulting chunks through a function, then recombines the result, represented by this data-flow pattern:
\begin{equation*}
\xymatrix
{
       & \bullet \ar[r]^{f} & \bullet \ar[dr] & \\
\bullet \ar[ur] \ar[r] \ar[dr] &  \bullet \ar[r]^{f} & \bullet \ar[r] & \bullet\\
       & \bullet \ar[r]^{f} & \bullet \ar[ur] & \\
}
\end{equation*}
So combining two stages gives the pattern:
\begin{equation*}
\xymatrix
{
       & \bullet \ar[r]^{f} & \bullet \ar[dr] & & \bullet \ar[r]^{g} & \bullet \ar[dr] & \\
\bullet \ar[ur] \ar[r] \ar[dr] &  \bullet \ar[r]^{f} & \bullet \ar[r] & \bullet \ar[ur] \ar[r] \ar[dr] &  \bullet \ar[r]^{g} & \bullet \ar[r] & \bullet\\
       & \bullet \ar[r]^{f} & \bullet \ar[ur] & & \bullet \ar[r]^{g} & \bullet \ar[ur] & \\
}
\end{equation*}
which is clearly only equivalent to a single stage when |f| and |g| are extremely simple.  This is not surprising: otherwise MapReduce could be collapsed down to a single stage, and would not be a very interesting processing paradigm.  What this means, then, is that if it were possible to compute
\begin{spec}
\y -> f y !>>= g
\end{spec}
then it would not be anything that could be composed with |x| via |!>>=|, so 
\begin{spec}
x !>>= (\y -> f y !>>= g)
\end{spec}
is meaningless.  Sorting on keys therefore makes it impossible that MapReduce could obey the third monad law.

\par Therefore, we coin the term \textbf{quasi-monad} to indicate an object that has monadic operations and obeys natural generalisations of the first two monad laws, but does not obey the third law.  We have just seen that any complex stage-based distributed algorithm must fail to obey the third law, and so is at most a quasi-monad.  In fact, MapReduce is a two-parameter quasi-monad:
\begin{spec}
newtype MapReduce x a y b = MR { run :: ([(x,a)] -> [(y,b)]) } 
\end{spec}
We shall assume that all key types belong to the class |Eq|, which is necessary to enable sorting and partitioning on keys.)  This should remind us of the |State| monad with, as it turns out, good reason.  The monadic operations are:
\numberson
\begin{spec}
return :: b -> MapReduce x a x b
return k = MR (map (first $ const k))

(!>>=) :: MapReduce x a y b -> (b -> MapReduce y b z c) -> MapReduce x a z c
(!>>=) f g = MR (\kvs ->
        let
                fs = run f kvs
                gs = map g $ nub $ fst <$> fs
        in
        concatMap (\x -> run x fs) gs)
\end{spec}
\numbersreset
\numbersoff

\noindent So |return| forces all keys to a given quantity.  To see how |!>>=| works, rewrite lines 6 and 7 as
\begin{spec}
fs  = run f kvs
ks  = nub (fst <$> fs)
gs  = map g gs
\end{spec}
So |fs| is the output of the mapper/reducer, |ks| is the \textit{set} of keys from the output, and |gs| is a set of functions, one per key in the set.  So we have one function per key in the output of |f|.  Then in |concatMap|, we apply each function to the full data set (leaving it to each function to decide what subset of the data to process) and combine the result.

\par They do this with the help of the function |wrap|, which, we recall, takes a key and a function and then applies the function to those records matching the key.  We can now write |wrap| as
\begin{spec}
wrap' :: ([x] -> [(b,y)]) -> (a -> MapReduce x a y b)
wrap' f = (\k -> MR (g k))
        where
        g k kds = f $ snd <$> filter (\kd -> k == fst kd) kds
\end{spec}
\noindent As can be seen, it selects the key/value records in |kds| whose key is |k|, extracts their values and then pushes the resulting list through |f|.

\section{MapReduce as a monad transformer}

\subsection{The |MapReduceT| type}

\par |[]| is a monad with |>>= == concatMap|, so |MapReduce| can be thought of as a transformation of the list monad.  It turns out that there is a very natural way of describing a general transformer that will apply to any monad, with |MapReduce| the result of applying it to |[]|.

\par So, let |m| be a monad; then the \textbf{|MapReduceT| type of} |m| is
\begin{spec}
newtype (Monad m) => MapReduceT m t u = MR { run :: m t -> m u }
\end{spec}
From now on, |m| will be a monad type, while |t|, |u| and |v| are arbitrary data types parameterising |m|.  There is also a natural way to lift monads into the transformer:	
\begin{spec}	
lift ::  (Monad m) => m t -> MapReduceT m t t
lift x  = MR (const x)
\end{spec}
In addition, we define the operations:
\begin{spec}
(>>>) ::  MapReduceT m t u -> MapReduceT m u v -> MapReduceT m t v
(>>>) f g  = MR (\ss -> run g $ run f ss)

(-<)  ::  MapReduceT m t u -> m t -> m u
(-<) f x   = run f x
\end{spec}
These are arrow-like but |MapReduceT m| is not itself an arrow.  Note that |>>>| is just function composition in disguise, and |-<| is just function application in disguise.

\par Now, if |MapReduceT| is to be a monad transformer, then |MapReduceT m| must be a quasi-monad with a |lift| operation.  The quasi-monadic operations for |MapReduceT m| are:
\numberson	
\begin{spec}			
return ::  (Monad m) => t -> MapReduceT m t t
return x  = lift (return' x)

bind :: (Monad m) => MapReduceT m u u -> MapReduceT m t u 
			-> (u -> MapReduceT m u v) -> MapReduceT m t v
bind p f g  = MR (\xs -> ps xs >>= gs xs)
			where
		     ps xs      =  (f >>> p) -< xs
		     gs xs x    =  (f >>> g x) -< xs
\end{spec}
\numbersreset
\numbersoff
\noindent where |>>=| is bind in |m| and |return'| is return in |m|. If |p :: MapReduceT m u u| define 
\begin{spec}
((bd p)) = bind p
\end{spec}
The parameter |p| seems mysterious, and it is not clear why we do not just take |p = MR id|.  The reason is that, in order to generalise MapReduce, we need a way of filtering the output of |f| before passing it to |g|.  So |p| is the equivalent of the |nub . snd <$>| in |!>>=|.

\par It can be shown that that |MapReduceT m| obeys natural modifications of the first and second monad laws, but the third law breaks down entirely, as expected.  Subject to the reasonable assumption that |run p $ return' x == return' x|, which is certainly true in all the cases we care about, we have:
\begin{spec}
f (bd p) return    == f >>= p
return x (bd p) f  == return x >>> f (return' x)
\end{spec}
which are natural generalisations of the standard monadic laws.  Any attempt at proving an analogue to the third law quickly becomes intractable, however, because the crucial filter function |p| mixes with the monadic functions in complex and confusing ways.

\subsection{Equivalence to |MapReduce|}

\par Before we can show that |MapReduce| is equivalent to |MapReduceT []| we need to choose a filter function, that is, a suitable value of |p|.  Define
\begin{spec}
dedupe :: (Eq a) => MapReduceT [] (a,x) (a,x)
dedupe = MR (nubBy (\ kd kd' -> fst kd == fst kd'))
\end{spec}
\noindent Clearly |dedupe| takes a list of key/value pairs and returns a list with one pair per unique key (whose value is ill-defined but irrelevant).  If we wanted to be really rigorous we could amend the definition to read
\begin{spec}
dedupe'  :: (Eq a) => MapReduceT [] (a,Maybe x) (a,Maybe x)
dedupe'  = MR (\vks -> second (const Nothing) <$> (nubBy (\ kd kd' -> fst kd == fst kd')) vks)
\end{spec} 
which makes the output determinate at the price of hugely complicating the proof of equivalence and adding no meaningful value.  Therefore we stick with |dedupe|.  

\par Let |x|, |y| and |z| be value types and |a|, |b| and |c| be key types, and set |t = (a,x)|, |u = (b,y)| |v = (c,z)|.  Therefore, trivially, at the level of type definitions
\begin{spec}
MapReduce x a y b == MapReduceT [] t u
\end{spec}
We now show that this equality extends to the quasi-monadic structures.  

Say we are given |f :: MapReduce x a y b| and |g :: (b -> MapReduce y b z c)|.  We rewrite the definition of |!>>=| as:  
\begin{spec}
f !>>= g = MR (\ xs -> 
        let
                fs = f -< xs
                gs = map (g . fst) $ dedupe -< fs
        in
        concat $ map (\ y -> y -< fs ) gs)  
\end{spec}
Rewriting this some more we get:
\begin{spec}
f !>>= g = MR (\ xs -> 
        let
                fs = f -< xs
                ks = dedupe -< fs
        in
        concatMap (\ k -> (g . fst) k -< fs) ks)  
\end{spec}
But in the list monad |xs >>= f = concatMap f xs|, so we can write this as:
\begin{spec}
f !>>= g = MR (\ xs ->
        let
                bs ys     = f >>> dedupe -< ys
                hs ys z   = f >>> (g . fst) z -< ys
        in
        bs xs >>= hs xs)  
\end{spec}
where we have used the trivial identity |n -< (m -< l) === (m >>> n) -< l|.  Therefore
\begin{proposition}
The |MapReduce| monad is equivalent to |MapReduceT []| with
\begin{spec}
f !>>= g === f (bd dedupe) (g . fst)
\end{spec}
\end{proposition}
\begin{proof}
The identity was proved above; the |return| functions are trivially identical.
\end{proof}

\subsection{|MapReduceT| and the State Monad}

\par There is an obvious similarity between |MapReduceT| and the state monad, in that members of |MapReduceT m| are functions |m a -> m b| while members of |State s| are functions |s -> (s,a)|.  In fact this similarity runs very deep, and it turns out that the state monad can be related to |MapReduceT| of a very simple monad.  

\par Define the \textbf{Reader Monad}:
\begin{spec}
data Hom a b = H { run::(a->b)}
\end{spec}
and make |Hom s| a monad by taking 
\begin{spec}
return x         = H (const x)
f (bdf H) g      = H (\x -> g' (f' x) x)
\end{spec}
where |f' == run f| and |g' x y == run (g x) y|.  Define
\begin{spec}
(>>>) :: Hom a b -> Hom b c -> Hom a c
(>>>) f g = H $ (run g) . (run f)
id    :: Hom a a
id = H id
\end{spec}

\par Now consider |MapReduceT (Hom s) b c|, which consists of functions |Hom s b -> Hom s c|.  

\begin{lemma}
There is a natural map 
\begin{spec}
hom::Hom b c -> MapReduceT (Hom s) b c
\end{spec}
Such that
\begin{spec}
(hom q) (bdfh p) (hom r) == hom $ (q >>> p) (bdf H) (q >>> r)
\end{spec}
\end{lemma}
\begin{proof}
\par Define
\begin{spec}
hom f  = MR $ \h -> H (run f) . (run h)
       = MR $ \h -> h >>> f
\end{spec}
We have the following, easily proved, identities:
\begin{spec}
hom $ f >>> g                          == hom f >>> hom g
run . hom f                            == \x -> x >>> f
\end{spec}
from which we can deduce that
\begin{spec}
(hom q) (bdfh p) (hom r)  == MR $ \x -> (run . hom q >>> run . hom p ) -< x 
						(bdf H) \y -> (run . hom q >>> run . hom (r y)) -< x
                          == MR $ \x -> run $ hom (q >>> p) -< x 
                          	(bdf H) \y -> run $ hom (q >>> (r y)) -< x
                          == MR $ \x -> (q >>> p (bdf H) \y -> q >>> (r y)) -< x
                          == MR $ \x -> x >>> $ (q >>> p) (bdf H) \y -> (q >>> r y)
                          == hom $ ( q >>> p) (bdf H) (q >>> r)
\end{spec}
\end{proof}

\par We can now prove:

\begin{proposition}
Given |f :: State s a|, |g :: a -> State s b|, and
\begin{spec}
p     = H $ \x -> (e,snd x)           :: Hom (s,a) (s,a) 
\end{spec}
Then there exist |q :: Hom (s,a) (s,a)| depending only on |f| and |r :: (s,a) -> Hom (s,a) (s,b)| depending only on |g| such that
\begin{spec}
(hom q) (bdfh p) (hom r) == hom $ (H fst) >>> H (run $ f (bdf S) g)  
\end{spec}
\end{proposition}

\begin{proof}

\par Define 
\begin{spec}
m ::  s->(s,a)
n ::  a->s->(s,b)
\end{spec}
by
\begin{spec}
f  = state m
g  = \x -> state (n x)
\end{spec}
In the state monad, the bind function |(bdf S)| obeys
\begin{spec}
f (bdf S) g  == state $ (\s -> n (snd $ m s) (fst $ m s))
             == state $ run $ u (bdf H) v 
\end{spec}
where 
\begin{spec}
u       = H $ snd . m      :: Hom s a 
v x y   = n' x (m y)       :: a -> Hom s (s,b)
n' x y  = H $ n x (fst y)  :: a -> Hom (s,a) (s,b)
\end{spec} 
Now pick an arbitrary value |e::s| (if |s| is a monoid we can take |e=mempty|, but the value is irrelevant).  Define
\begin{spec}
p     = H $ \x -> (e,snd x)           :: Hom (s,a) (s,a)   
q      = H $ m . fst                  :: Hom (s,a) (s,a)
r    = \x -> H $ \y -> n' (snd x) y   :: (s,a) -> Hom (s,a) (s,b)
\end{spec}
so
\begin{spec}
q x     = H $ (run f) . fst
r x y   = H $ run (g (snd x)) (fst y)
\end{spec}
and these functions are thus the natural extension of |f| and |g| to functions where all variables are of type |(s,a)|.  Now
\begin{spec}
(q >>> p) (bdf H) (q >>> r)   == \x -> H $ (e, snd . m . fst x) (bdf H) \x -> n' (snd x) (m . fst)
                              == \x -> n' (snd . m . fst x) ( m . fst x)
                              == (H fst) >>> (u (bdf H) v)
\end{spec}
And, putting this together with the identity proved above, we get
\begin{spec}
(hom q) (bdfh p) (hom r)  == hom $ (q >>> p) (bdf H) ( q >>> r)
                          == hom $ (H fst) >>> (u (bdf H) v)
                          == hom $ (H fst) >>> H (run $ f (bdf S) g)  
\end{spec}
\end{proof}

This is a very complex expression, and one may object that the |(bdfh p)| join gives rise to a map |(s,a) -> (s,b)| while the |(bdf S)| join is a map |s -> (s,b)|.  So let us unpick the type-structure of the two sides.  On the left-hand side
\begin{spec}
hom q  :: MapReduceT (Hom s) (s,a) (s,a)
hom r  :: (s,a) -> MapReduceT (Hom s) (s,a) (s,b)
\end{spec}
so
\begin{spec}
(hom q) (bdfh p) (hom r) :: MapReduceT (Hom s) (s,a) (s,b)
\end{spec}
On the right-hand side
\begin{spec}
f  :: State s a
g  :: a -> State s b
\end{spec}
so
\begin{spec}
f (bdf S) g :: State s b => run $ f (bdf S) g :: s -> (s,b)
\end{spec}
Now |fst :: (s,a) -> s|, which means that
\begin{spec}
H fst >>> H (run $ f (bdf S) g) :: Hom (s,a) (s,b)
\end{spec}
and so 
\begin{spec}
hom $ H fst >>> H (run $ f (bdf S) g) :: MapReduceT (Hom s) (s,a) (s,b)
\end{spec}
and so the two sides do have the same type.

\section{Conclusion}

\subsection{Implementation}

\par We have implemented |MapReduce| and |MapReduceT| as simple libraries and implemented a simple test application using the standard word-count MapReduce algorithm (see \cite{repository}).  It turns out that swapping out |MapReduce| and replacing it with |MapReduceT []| has no effect on the output.  Practical results therefore bear out the theoretical equivalence proved above.

\subsection{Future directions}

\par The obvious next step is to examine and analyse |MapReduceT| of other well known monads, e.g. |Maybe|.  Given that the richness of MapReduce arises from |MapReduceT| applied to the list monad, this is obviously of some interest.

\par It is also worth examining the role of the choice of filtering function |p|.  Taking |p==dedupe| on the list monad gave us classical MapReduce, but the condition that |p . return' x == return' x| simply states that |p| is the identity on singletons, which is not a particularly strong condition.  Therefore there is the possibility of there being MapReduce-like algorithms for other choices of |p|.

\par Finally, the concept of the pseudo-monad is surely an interesting generalisation in and of itself.

\bibliography{mrmonad.bib}

\end{document}
