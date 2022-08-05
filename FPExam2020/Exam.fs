module Exam2020
(* If you are importing this into F# interactive then comment out
   the line above and remove the comment for the line bellow.

   Do note that the project will not compile if you do this, but 
   it does allow you to work in interactive mode and you can just remove the '=' 
   to make the project compile work again.

   Do not remove the line (even though that does work) because you may inadvertantly
   introduce indentation errors in your code that may be hard to find if you want
   to switch back to project mode. 

   Alternative, keep the line as is, but load ExamInteractive.fsx into the interactive environment
   *)
(* module Exam2020 = *)

(* 1: Insertion sort *)

(* Question 1.1 *)

    let rec insert (x: 'a) (lst: 'a list) =
        match lst with
        | [] -> [x]
        | y :: ys when x <= y -> x :: y :: ys
        | y :: ys -> y :: insert x ys
    
    let rec insertionSort (lst: 'a list) =
        match lst with
        | [] -> []
        | x :: xs -> insert x (insertionSort xs)
 
    
(* Question 1.2 *)
    
    let insertTail (x: 'a) (lst: 'a list) =
        let rec aux acc lst' =
            match lst' with
            | [] -> (x::acc)
            | y :: ys when x <= y -> aux (y :: acc) ys
            | y :: ys -> aux (y :: x :: acc) ys
        aux [] lst
        
    let insertionSortTail (lst: 'a list) =
        let rec aux acc lst' =
            match lst' with
            | [] -> acc
            | x :: xs -> aux (insertTail x acc) xs
        aux [] lst
    
        


(* Question 1.3 *)

(* Q: Why are the higher-order functions from the List library not a good fit to implement insert ? If
you are unsure, try writing insert using a higher-order function and see where things start to
get troublesome*)

(* A: Because higher-order functions from the list library typically go through the entire list whereas insert
       terminates as soon as it has found the insertion point. It is possible to use
       these higher-order functions to write insert, it is just not practical.*)
        
    let insertionSort2 lst = List.fold(fun acc elem -> insertTail elem acc ) [] lst
    
    let insertionSort2Jesper lst = List.foldBack insertTail lst []
    
(* Question 1.4 *)

    let rec insertBy f (x: 'a) (lst: 'a list) =
        match lst with
        | [] -> [x]
        | y :: ys when f x <= f y -> x :: y :: ys
        | y :: ys -> y :: insertBy f x ys
    
    let rec insertionSortBy f (lst: 'a list) =
        match lst with
        | [] -> []
        | x :: xs -> insertBy f x (insertionSortBy f xs)


(* 2: Code Comprehension *)
    let rec foo x = 
        function 
        | y :: ys when x = y -> ys
        | y :: ys            -> y :: (foo x ys)

    let rec bar x =
        function
        | []        -> []
        | xs :: xss -> (x :: xs) :: bar x xss

    let rec baz =
        function
        | [] -> []
        | [x] -> [[x]]
        | xs  -> 
            let rec aux =
                function
                | []      -> []
                | y :: ys -> ((foo y >> baz >> bar y) xs) @ (aux ys)
            aux xs

(* Question 2.1 *)

    (* 
    
    Q: What are the types of functions foo,  bar, and baz?

    A: 
    foo: 'a -> 'a list -> 'a list 
    bar: 'a -> 'a list list -> 'a list list 
    baz: 'a list -> 'a list list


    Q: What do functions foo, bar, and baz do? 
       Focus on what they do rather than how they do it.

    A:  foo x xs returns a list xs' which is the same as xs but with the first occurrence of x removed from xs
        bar x xss returns a list of lists xss' which is the same as xss but with x added to the front of all the lists(xs) within xss
        baz xs returns a list of lists xss which contain all permutations of xs.


    Q: What would be appropriate names for functions 
       foo, bar, and baz?

    A: foo: removeSingle
       bar: appendAll
       baz: allPermutations
    
    *)
        

(* Question 2.2 *)

 
    (* 
    The function foo generates a warning during compilation: 
    Warning: Incomplete pattern matches on this expression.

    
    Q: Why does this happen, and where? 

    A: It happens because foo does not have a case that matches the empty list.
       In particular, this case will be reached whenever we try to remove an element from a list
       that is not in that list.

    Q: For these particular three functions will this incomplete 
       pattern match ever cause problems for any possible execution of baz? 
       If yes, why; if no, why not.

    A: It will not cause problems since we always call foo with an element that is in the list.

    *)


    let rec foo2 x = 
        function
        | [] -> []
        | y :: ys when x = y -> ys
        | y :: ys            -> y :: (foo x ys)

(* Question 2.3 *)
    
    let whatIsType y = foo y >> baz >> bar y

    (* 
    In the function baz there is a sub expression foo y >> baz >> bar y

    Q: What is the type of this expression

    A: 'a list -> 'a list list

    Q: What does it do? Focus on what it does rather than how it does it.

    A: The function takes a list `lst` and returns all possible permutations of `lst` that start with `y`

    *)

(* Question 2.4 *)

    let bar2 x lss = List.map(fun acc -> x::acc) lss

(* Question 2.5 *)

    let rec baz2 lst =
        match lst with
        | [] -> []
        | [x] -> [[x]]
        | xs -> List.collect (fun y -> (foo y >> baz >> bar y) xs) xs
        
        //or List.foldBack(fun elem acc -> (foo elem >> baz >> bar elem) xs @ acc) xs []
    

(* Question 2.6 *)

    (*
    
    Q: The function foo is not tail recursive. Why?
    
    A: Consider the following function call. As we can see the cons operators are only evaluated at the end of the computation. This is therefore not tail recursive.
    
    JA: The function foo is not called in a tail-recursive position and a string of cons-operators will
       be generated that cannot be reduced until the base case is called.
       
    foo  3 [1;2;3;4]
    
    1 :: (foo 3 [2;3;4])
    
    1 :: 2 :: (foo 3 [3;4])
    
    1 :: 2 :: [4]
    
    [1;2;4]
    *)
    
    let fooTail x lst =
        let rec aux cont =
            function
            | [] -> cont []
            | y :: ys when x = y -> cont ys
            | y :: ys -> aux (fun result -> cont (y :: result)) ys
        aux id lst

(* 3: Rock Paper Scissors *)

(* Question 3.1 *)

    type shape = Rock | Paper | Scissors
    
    type result = P1Win | P2Win | Draw

    let rps (s1: shape) (s2: shape) =
        match s1, s2 with
        |(Rock,Scissors) -> P1Win
        |(Paper, Rock) -> P1Win
        |(Scissors, Paper) -> P1Win
        |(Scissors, Rock) -> P2Win
        |(Rock, Paper) -> P2Win
        |(Paper, Scissors) -> P2Win
        | _ -> Draw

(* Question 3.2 *)

    type strategy = ((shape * shape) list -> shape)

    let parrot (s: shape) moves =
        match moves with
        | [] -> s
        | (_,m) :: _ -> m
    let parrot2 s (ls: (shape * shape) list) =
        List.tryHead ls |> Option.map snd |> Option.defaultValue s

    let beatenBy = function
        | Rock -> Paper
        | Scissors -> Rock
        | Paper -> Scissors

    let beatingStrat (ms: (shape * shape) list)  =
        ms
        |> List.countBy snd
        |> fun list ->
            let max = if List.isEmpty list then 0 else List.maxBy snd list |> snd
            List.filter(fun (_,count) -> count = max) list
        |> List.map(fun elem -> fst elem)
        |> List.map(fun elem -> beatenBy elem)
        |> List.sort
        |> List.tryHead
        |> Option.defaultValue Rock

    let roundRobin (shapes: shape list) : strategy =
        let mutable index = -1
        (fun _ -> 
            index <- (index + 1) % List.length shapes
            List.item index shapes
            )

(* Question 3.3 *)

    (* 
    
    Q: It may be tempting to generate a function that calculates your 
       point tuple after n rounds and then use Seq.initInfinite to 
       generate the sequence. This is not a good solution. Why?

    A: Because Seq.initInfinite has a much higher running time than unfold in this case.
    
    *)

    let bestOutOf (strat1: strategy) (strat2: strategy) =
        Seq.unfold(fun (prevms,(p1,p2)) ->
            let p1shape = strat1 (List.rev prevms)
            let p2shape = strat2 (List.rev prevms)
            match (rps p1shape p2shape) with
            | P1Win -> Some((p1,p2),(((p1shape,p2shape) :: prevms),(p1+1,p2)))
            | P2Win -> Some((p1,p2),(((p1shape,p2shape) :: prevms),(p1,p2+1)))
            | Draw -> Some((p1,p2),(((p1shape,p2shape) :: prevms),(p1,p2)))
            ) ([],(0,0))

(* Question 3.4 *)

    let playTournament _ = failwith "not implemented"

(* 4: Reverse Polish Notation *)

(* Question 4.1 *)

    type stack = ST of int list

    let emptyStack = ST[]

(* Question 4.2 *)

    type SM<'a> = S of (stack -> ('a * stack) option)

    let ret x = S (fun s -> Some (x, s))
    let fail  = S (fun _ -> None)
    let bind f (S a) : SM<'b> = 
        S (fun s -> 
            match a s with 
            | Some (x, s') -> 
                let (S g) = f x             
                g s'
            | None -> None)
        
    let (>>=) x f = bind f x
    let (>>>=) x y = x >>= (fun _ -> y)
    let evalSM (S f) = f emptyStack 
    let push (x: int) =
        S(fun (ST list) -> Some((),x :: list |> ST))
    let pop = S(fun(ST list) ->
        match list with
        | [] -> None
        | x:: xs -> Some(x,xs |> ST))

(* Question 4.3 *)

    let write str : SM<unit> = S (fun s -> printf "%s" str; Some ((), s))

    let read =
        let rec aux acc =
            match System.Console.Read() |> char with
            | '\n' when acc = [] -> None
            | c    when System.Char.IsWhiteSpace c -> 
                acc |> List.fold (fun strAcc ch -> (string ch) + strAcc) "" |> Some
            | c -> aux (c :: acc)

        S (fun s -> Some (aux [], s))

    (* 
    
    Q: Consider the definition of write There is a reason that the definition 
       is S (fun s -> printf "%s" str; Some ((), s)) and not just 
       ret (printf "%s" str). For a similar reason, in read, we write 
       S (fun s -> Some (aux [], s)) and not ret (aux []). 
       What is the problem with using ret in both of these cases?
    
    A: This is because ret prints the message before evaluating the monad, 
       but write evaluates the monad first and then prints the message.
    
    *)

(* Question 4.4 *)

    (* You may solve this exercise either using monadic operators or 
        using computational expressions. *)

    type StateBuilder() =

        member this.Bind(f, x)    = bind x f
        member this.Return(x)     = ret x
        member this.ReturnFrom(x) = x
        member this.Combine(a, b) = a >>= (fun _ -> b)

    let state = new StateBuilder()

    let calculateRPN _ = failwith "not implemented"
