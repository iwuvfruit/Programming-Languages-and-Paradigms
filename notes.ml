(*)
Ocaml: statically typed functional programming languaged
-type check program before running them
-types approximate runtime behaviour 
-allows us to find bugs berfore running and testing the program 
-variable binding is not an assignment, variables cannot be updated. 
we can only overshadow a previous binding/ variable binding persist/ 
garabage collection disposes off variable bindings that are not needed anymore. /
variable bindings are local they exist within a scope./ variables are bound to a value, not an expression. 
-Functions are values. /Function names establish a binding of the function name to its body. /Functions are values/ 
Recursive Functions are declared using the keyword let rec
ex) let area(r:float) = pi *. r *. r;; 
ex) let rec fact n = 
		if n = 0 then 1 
		else n * fact(n-1)
fact 3 
=> 3 * fact 2 
=> 3 * (2 * fact 1)
=> 3 * (2 * 1 * fact 0)
=> 3 * (2 * 1 * 1)
=> 3 * (2 * 1)
=> 3 * 2
=> 6
/had to remeber all the pending multiplications as the recursion unfolded. 
/these pending operations are the result of writing n * fact(n-1)
/each of these pending operations requires memory and this is wasteful. so space complexity is O(n)
/we can avoid these pending operations using tail recursive functions. 
-Tail recursive functions: if there are no operations to do after the recursive call.
let fact_tr n = 
	let rec f n m = 
		if n = 0 then m
	else 
		f (n-1) (n*m)
	in 
	f n 1  
inner helper function f actually does the work 
f has an extra argument m that holds the partialy computed factorial 
fact_tr 3 
=> f 3 1 
=> f 2 (3 * 1)
=> f 1 (2 * 3)
=> f 0 (1 * 6)
=> 6
The factorial is gradually accumulated in the second argument to f./
No unwinding phase in which the multiplications are performed after all the recursion has happened. 
O(1) space usage; still O(n) time 
Since the local bindings of the function are unused after the recurisve call, 
the stack frame they reside it can be overwritten. therefore tail recurisve functions take only constant O(1) stack space. 
All recursive functions can be translated into tail recursive form 
*)
Code 

(-1: int)
5 /2 
((5.0/.2.0):float)
(*type error: 5 /. 3 or 5.0 / 3 *)
"comp" ^ "302" ^ " =" ^ " Fun";; 
'c';;
'3';;
(true: bool);; 
if 0 = 0 then 1.0 else 2.0;;
(*
if 0 = 1 then 1 else 2.0;; type error: this expression has type float but an expression was expected of type int

if true then 4.0 else 1.0 /. 0.0;;
not caught statically during type checking
*)
let pi = 3.14;; 
let (pi:float) = 3.14;; 
(* overshadowed the previous variable binding pi with another building also called pi *)
(*local binding*)
let (m: int) = 3 in
let (n: int) = m * m in
let (k:int) = m* m in
k* n 

let (m:int) = 3 and (n:int) = 4 in 
let (k:int) = m + n in k * n 

(*bindings persist but later binders overshadow earlier binders *)
let(k:int)=4;;
let(k:int)=3 in k * k;; (*9 *)
k;; (*k is 4 here *)

(*we can bind variable to functions *)
let area: float -> float = 
	fun r -> pi *. r *. r;;
let area(r:float) = pi *. r *.r;;
let a2 = area 2.0;;
let (pi:float) = 6.0;; (* //overshadowed pi *)
let a3 = area 2.0;; (* //the area function will remain unchanged. 
 *)
let pi = 4. *. atan 
let area (r:float) = pi *.r *.r;;
let a4 = area 2.0;;

let cylinder r h = area r *. h;;
let a5 = cylinder 2.0 5.0 
(* 
val fact: int -> int
fact(n) = n!
invariant: n >= 0 
effect:None *)
exception Domain;;
let rec fact n = 
	if n < 0 then raise Domain
	else if n = 0 then 1 
	else n * fact(n-1);; (* 
better to check the invariant for the externally visible function only, not during recursion *)
let rec fact n = 
	let rec f n = 
	if n = 0 then 1
	else
		n * f(n-1)
	in
	if n < 0 then raise Domain
	else f n(* 
A function is said to be tail recursive if there's nothing to do except return the final value.
Since the execution of the function is done, saving its stack fram is rebundant.
instead of nesting the stack deeper, the current stack frame is reused. 
the caller is replaced with the callee. 
writing recursive definitions without worrying about space efficiency.  *)

let rec fact_tr1 n =
	let rec f n m = 
	if n = 0 then m 
	else f (n -1)(n * m)
	in
	f n 1 

let rec fact_tr n =
	let rec f n m = 
	match m with 
	| 0 -> m
	| n -> f (n-1)(n*m)
	in
	f n 1

(* compound types:products. 
suppose a and b are some predefinied types. Then a * b is the type of tuples of a and b 
if x:a and y:b, then (x,y): a *b.
larger tuples can be made with more stars and commas, (x, y, z): a*b*c 
how do extrat hello from (3, ("hello", "world"))?
pattern matching: to analyze edlements of a given type 
match <expression> with 
|<pattern> -> <expression> 
a pattern is either a variable, underscore, tuple of patterns 
patterns allows pattern matching on the possible elements of a type. 

match (3, ("hello", "world")) with 
|(_, (x,y)) -> x 

let(_, (x, _)) = (3, ("hello", "world")) in x 

model a collection of cards
with user definied(non recursive) data type
type suit = Clubs | Spades | Hearts | Diamonds
we call Clubs, Spades, Hearts, Diamonds constructors, constructors begin with a capital letter
we can use pattern matching to analyze elements of a given type 
ex)
dom: suit ->suit-> bool 
dom s1 s2 = true
iff suit s1 beats or is equal to suit s2 
relative to the ordering S > H > D > C 
The trick is to match on two inputs at once by forming a tuple 

let rec dom s1 s2 = match (s1, s2) with 
|(Spades, _) -> true
|(Hearts, Diamonds) -> true 
|(Hearts, Clubs) -> true
|(Diamonds, Clubs) -> true
|(s1, s2) -> s1 = s2 

Describe the collection of cards in a hand inductively 
-Empty is of type hand 
-If c is a card and h is of type hand, then Hand(c,h) is of type hand 
type hand = 
	|Empty 
	|Hand of card * hand 
these kinds of types are also called recursive types or inductive types because they are described inductively 

let hand0:hand = Empty 
let hand1:hand = Hand((Ace, Hearts), Empty)
let hand2:hand = Hand((Queen, Diamonds), hand1)
let hand5:hand = Hand((Ace, Spades),
					Hand((Ten, Diamonds),
						Hand((Seven, Clubs),
							Hand((Queen, Spades),
								Hand((Eight, Clubs), Empty)))))

ex)write a function find which when given a rank and a hand, finds the first card in hand of the specified rank
and returns its coresponding suit. 
but what if no such card exist? 
Option Data Type(predefinied)
type 'a option = None | Some of 'a
so we want 
find: rank * hand -> suit option 
1)essentially a list of cards so definie lists abstractly for an arbitrary type of data 'a
this will be an inductive definition. The constructors will be: 
[] is of type 'a list
x :: xs is of type 'a list if x: 'a and xs: 'a list 
type 'a list = 
|[]
|(::) of 'a * 'a list 
polymorphism: when we say []: 'a list, we are saying for any type alpha, [] is a list of alphas 
anytime we say for any type alpha, we are usingpolymorphism. 
we can wrtie polymorphic functions to process data abstractly. 

ex)write a function rev which given a list l of type 'a list. it returns its reverse 
'a list ->'a list 
let rec rev l =
	match l with 
	|[] -> []
	|x :: xs -> rev l  *)
Code
(* 
we can extend the pre-definied types of int, float, char, string with user defined types 
*)
type suit = Clubs |Spades| Hearts|Diamonds
type rank = Two |Three|Four|Five|Six|Seven|Jack|Queen 
type card = rank * suit 
(* 
recursive data type
ex)I have 2 cards in my hand and 77 cards in my hand or no hand at all in my hand
we can define it recursively:
A hand is either empty or it consists of a card followed by the rest of the hand 
-Empty is of type hand
-If c is a a card and h is of type hand then Hand(c, h) is of type hand 
nothing else is of type hand  *)(* 
constructors allow us to construct an element of a certain type *)
type hand = Empty | Hand of card * hand 
let hand0: hand = Empty
let hand1: hand = Hand((Ace, Hearts), Empty)
let hand2: hand = Hand((Ace, Diamonds), hand1)
let hand5:hand = Hand((Ace, Spades),
					Hand((Ten, Diamonds),
						Hand((Seven, Clubs),
							Hand((Queen, Spades),
								Hand((Eight, Clubs), Empty)))))

(* extract:suit -> hand -> hand 
extract s h returns a hand consisting of all card in h of suit s 
invariants: None
effect: none  *)

let rec extract (s:suit) (h:hand) = match h with 
|Empty -> Empty
|Hand((r', s') as c, h') -> 
	if s = s' then Hand(c, extract s h')
	else extract s h' 
let (spades5:hand) = extract Spades hand5 

(* count: hand -> int 
count(h) counts the number of cards in a hand h 
 *)
let rec count h = match h with 
|Empty -> 0
|Hand(c, h) -> count h + 1 

(* find: (rank * hand) -> suit option 
find the first card with rank r in h and 
return its corresponding suit s by Some(s)
if there's no card with rank r return None 
To write this function we make use of the pre-defined, parametrized datatype 'a option.
type 'a option = None | Some of 'a  *)

let rec find(r, h) = match h with 
|Empty -> None 
|Hand((r', s'), h') -> 
	if r = r' then Some s'
	else find(r, h')


(* list 
here is how one might define lists of elements of the same type  *)
type 'a myList = Nil | Cons of 'a * 'a myList
let list0 : int myList = Nil 
let list1 : int myList = Cons(1, Nil)
let list2 : int myList = Cons(2, Cons(1, Nil))
let list3 : int myList = Cons(3, list2)

let lst0 : float mylist = Nil 
let lst1 : float mylist = Cons(3.1, Cons(2.6, lst0))
(* using the predefinied list
 *)
 let rlist1 : float list = [6.5; 5.2]
 let rlist2 : float list = 2.3 :: 1.2 :: []

 append: 'a list -> 'a list -> 'a list
 append(l1, l2) returns a list consisting of the elements of l1 followed by the elements of l2 
 invariants: None
 effect: None

 let rec append l1 l2 = match l1 with 
 |[] -> l2 
 |x::l' -> x::append l' l2 

 let appl2 : float list = append rlist1 rlist2
 let appl2' : float list = rlist1 @ rlist2

(* destructor style 
 *) 
 let head(h::t) = h;; 
 let tail l = match l with 
 |[] -> []
 |h::t -> t 
 let rec app(l1, l2) = 
 	if l1=[] then l2 
 	else
 		head(l1) :: (app (tail(l1), l2))

 (* reverse a list (but the code below is inefficient)
 let rev: 'a list ->'a list 
 rev(l) returns a list consisting of elements of l in reverse order 
 invariants: none 
 effect: none
  *)

  let rec rev l = match l with 
  |[] -> []
  |x::l -> (rev l) @ [x]
(* 
  tail recursive version of this is  *)

 let rev' l = 
 	let rec rev_tr l acc = match l with 
 	| [] -> acc
 	| h :: t -> rev_tr t (h::acc)
 in
 rev_tr l [] 

 mergeSort 
 split lst = (l, l') s.t lst = merge(l, l')
 split: 'a list -> ('a list * 'a list) 
 merge: 'a list *'a list -> 'a list 
 mergeSort: 'a list ->'a list 
 let rec split l = match l with 
 |[] -> ([], [])
 |[h] -> ([h],[])
 |(h1::h2::t) -> 
 	let(left, right) = split t in (h1::left, h2::right)

 let rec merge l x = match l, x with 
 |[], x -> x
 |x, [] -> x 
 |h::t, h'::t' ->
 	if (h <= h') then h::merge t (h' :: t')
 else h':: merge(h::t) t' 

let rec mergeSort l = match l with 
|[] -> []
|[x] -> [x]
|lst -> let (slst, slst') = split lst 
in merge (mergeSort slst) (mergeSort slst')

(* 
insertion sort
'a *'b -> ('a *'b) list ->('a *'b )list
inset(k,v) l = l' 
if l is ordered and (k, v)is a key value pair then 
resulting list l' is ordered where (k,v) has been inserted at the approporitate position in l 
 *)
let rec insert (k,v) l = match l with 
|[] -> [(k,v)]
|((k', v') as h) :: t ->
	if k = k' then (k,v)::t 
	else 
		if k < k' then (k, v)::l 
		else h :: insert (k,v) t

(* lookup: 'a -> ('a * 'b) list ->'b option 
lookup k l 
returns Some v if (k,v) is in l 
otherwise None 
 *)
 let rec lookup k l = match l wtih 
 |[] -> None 
 |(k', v) :: t -> 
 if k = k' then Some v 
else lookup k t 
(* 
ordered: ('a *'b) list -> bool 
ordered l retruns true if 
l is a key-value list and for all (k_i, v_i) at position i and (k_j, v_j) at position j in l s.t 
k_i < k_j iff i < j *)

let rec ordered l = match l with 
|[] -> true 
|[x] -> true 
|(k0, x):: (k1::y)::t -> 
if k0 < k1 then ordered ((k1, y)::t)
else false

(* A binary search tree is a binary tree in which the elements are sorted. 
 *)
type bst = 
|Empty 
|Node of bst * int * bst 
(* 
A higher order function is a functoin taking as input a function or producing as output a function
ex) ('a ->'b)-> 'a list ->'b list takes two argument. it takes a function and a list. 
whereas ordinary functions let us abstract over data, 
higher order functions let us abstract over functionality. 
programs can be very short/compact. programs are reusable, well structured, modular. 
Each significant piece of functionality is implemented in one place.
functions are first class values. 

Abstracting over common functionality 
let rec sum(a,b) = 
	if a > b then 0 else a+sum(a+1, b)
let rec sum(a,b) = 
	if a > b then 0 else square(a) + sum(a+1, b)
let rec sum(a,b) =
	if a > b then 0 else exp(2, a) + sum(a+1, b)
we can wrtie a generic sum function 
None generic: sum: int * int -> int 
Generic Sum using a function as an arugment: sum: (int -> int) -> int *int -> int 

let rec sum f (a,b)=
	if (a > b) then 0 else (f a) + sum f (a+1, b)

we can abstract over more common functionality such as increment 
let rec sum f (a, b) inc = 
	if (a > b) then 0  
	else (f a) + sum f (inc(a), b) inc 

let rec sumOdd (a, b) = 
	if (a mod 2) = 1 then 
		sum (fun x -> x) (a, b) (fun x -> x +2)
	else
		sum (fun x -> x) (a+1, b) (fun x -> x+2)

multiplying numbers 
let rec product f (a, b) inc = 
	if (a > b) then 1 
	else (f a) * product f (inc(a), b) inc 

tail-reucrusively 
let rec sum f (a, b) inc acc =
	if (a > b) then 0 
	else sum f (inc(a), b) inc (f a + acc) 
let rec product f (a, b) inc acc =
	if (a > b) then 1 
	else product f (inc(a), b) inc (f a * acc)

abstraction and higher order functions are very powerful mechanisms for writing reusuable programs 
ex) computing a series 
series: (int -> int -> int ) //comb
-> (int -> int) // f
-> (int * int) //(a,b)
-> (int -> int) // inc
-> int //acc
->int //result 

let sum f (a,b) inc = series (fun x y -> x + y) f (a, b) inc 0 
let prod f (a,b) inc = series (fun x y -> x * y) f (a, b) inc 1 

Common Higher Order Functions (Built in): 
List.map: ('a ->'b) -> 'a list ->'b list 
List.filter: ('a -> bool) -> 'a list ->'a list 
List.fold_right: ('a -> 'b -> 'b) -> 'a list -> 'b -> 'b '
List.fold_left: ('a -> 'b -> 'a) -> 'a ->'b list -> 'a
List.for_all: ('a ->bool) -> 'a list -> bool 
List.exists: ('a -> bool) -> 'a list -> bool
 *)
 code 

 let rec cube n = n*n*n
 let rec rcube n = n *. n *. n 
 let rec square n = n * n
 let rec exp (b, n) = if n = 0 then 1 else b * exp(b, n-1)

 let rec sumInt(a,b) = if (a>b) then 0 else a + sumInt(a+1, b)
 let rec sumSquare(a,b) = if (a>b) then 0 else square(a) + sumSquare(a+1, b)
 let rec sumCube(a,b) = if (a>b) then 0 else cube(a) + sumCube(a+1, b)
 let rec sumExp(a,b) = if (a>b) then 0 else exp(2, a) + sumExp(a+1, b)

 (*we will abstract over the function f:cube, squre, exp *)
 sum: (int -> int) -> int * int -> int 
 let rec sum f (a, b) = 
 	if (a > b) then 0
 	else (f a) + sum f (a+1, b)

 let rec sumInt(a,b) = sum (fun x -> x) (a, b)
 let rec sumCube(a,b) = sum cube (a,b)
 let rec sumSquare(a,b) = sum square (a,b)
 let rec sumExp(a,b) = sum (fun x -> exp(2,x)) (a, b)

 difference between fun and functions:
 function allows the use of pattern matching but fun doesn't allow pattern matching but can pass multiple arguments
ex) fun x y -> x + y 


let prod_tr f (a, b) inc = 
	let rec prod (a,b) acc = 
	if(a > b) then acc
	else prod (inc(a), b) (f(a) * acc) 
in
	prod (a,b) 1 

let rec fact n = prod_tr (fun x -> x) (1, n) (fun x -> x+1)


let rec series comb f (a, hi) inc r = 
	let rec series' (a, r) = 
	if (a > hi) then r
	else
		series' (inc(a), comb r (f a))
	in
	 	series' (a, r)
(* 
An iterative version of sum modified to deal with real values *)
let rec iter_sum f (lo, hi) inc = 
	let rec sum' (lo, r) = 
	if (lo > hi) then r 
	else
		sum' (inc(lo), r +. f lo)
	in
	sum' (lo, 0.0)

The integral of f(x) is the area below the curve f(x) in the interval [a, b].
This will use rectangled method to approximate the integral of f(x) in the interval [a, b]
made by summing up a series of small rectangles. 

x = a + dx/2
l = dx 
let rec integral f (lo, hi) dx = 
	dx *. iter_sum (fun x -> f x *. dx) (lo +. (dx /. 2.0), hi)
