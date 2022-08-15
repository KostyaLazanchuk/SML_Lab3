fun same_string(s1 : string, s2 : string) =
    s1 = s2
    
(*Task 1 a*)
fun all_except_option (str, sl) =
  case sl of
    [] => NONE
    | x::xs => case same_string(str, x) of
                 true => SOME(xs)
                 | false => case all_except_option(str, xs) of
                              NONE => NONE
                              | SOME y => SOME(x::y) 

(*Task 2 b*)
fun get_substitutions1 (subs, str) =
	case subs of
		[] => []
	|	x :: xs =>
			case all_except_option(str, x) of
				NONE => [] @ get_substitutions1(xs, str)
			|	SOME lst => lst @ get_substitutions1(xs, str)


(*Task 3 c*)
fun get_substitutions2 (subs, str) =
	let fun f (xs, acc) =
		case xs of
			[] => acc
		|	x :: xs =>
			case all_except_option(str, x) of
				NONE => f(xs, acc)
			|	SOME lst => f(xs, acc @ lst)
	in
		f(subs, [])
	end


(*Task 4 d*)
fun similar_names (ssl: string list list, {first=f, middle=m, last=z}) = 
  let
   fun fn_it (sl:string list) = 
     case sl of
        [] => []
	    | x::sl' => {first=x, last=z, middle=m}::fn_it(sl')
   in
    {first=f, last=z, middle=m}::fn_it(get_substitutions2(ssl, f))
   end

(* second part *)

datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove


(*Task 21 a*)
fun card_color (s:suit, r:rank) = 
  case s of 
    Clubs => Black 
    | Spades => Black
    | Diamonds => Red 
    | Hearts => Red 
   


(*Task 22 b*)
fun card_value (s:suit, r:rank) = 
  case r of 
    Ace => 11
    | Num i => i
    | _ => 10


(*Task 23 c*)
fun remove_card (cards, c, ex) =
    case cards of
        [] => raise ex
    |   x :: xs =>
        if x = c then
            xs
        else x :: remove_card(xs, c, ex)


(*Task 24 d*)
fun all_same_color (cards: card list) = 
    case cards of 
      [] => true
      | c::[] => true
      | c1::(c2::cards') => card_color(c1) = card_color(c2) andalso all_same_color(c2::cards')


(*Task 25 e*)
fun sum_cards (cards: card list) = 
  let
   fun local_func (cards: card list, accu) = 
    case cards of 
      [] => accu
      | c::cards' => local_func(cards', card_value(c)+accu)
  in
    local_func(cards, 0)
  end

(*Task 26 f*)
fun score (cards: card list, goal) = 
   let
      val sum = sum_cards(cards)
      val pre = if sum > goal then 3 * (sum-goal) else goal-sum
   in
      if all_same_color(cards) then pre div 2 else pre
   end

(*Task 27 g*)
fun officiate (cards: card list, ds: move list, goal) = 
    let
      fun local_func (cards: card list, ds: move list, accu_cards: card list) =
        case (cards, ds) of
	        (_, []) => score(accu_cards, goal)
	        | (_, (Discard c)::ds') => local_func(cards, ds', remove_card(accu_cards, c, IllegalMove))
          | ([], Draw::ds') => score(accu_cards, goal)
          | (c::cards', Draw::ds') => 
                if sum_cards(c::accu_cards) > goal 
                then score(c::accu_cards, goal) 
                else local_func(cards', ds', c::accu_cards)		   
    in
       local_func(cards, ds, [])
    end

(* test for functions *)

(* test for task1 a *)
val Test1 = all_except_option ("hello", ["hi", "What`s up", "hello"])

(* test for task2 b *)
val Test2 = get_substitutions1([["Fred","Fredrick"],
["Elizabeth","Betty"],
["Freddie","Fred","F"]],
"Fred")

(* test for task3 c *)
val Test3 = get_substitutions2(	[["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]],
    "Fred");

(* test for task4 d *)
val Test4 = similar_names([["Fred","Fredrick"],
["Elizabeth","Betty"],
["Freddie","Fred","F"]], 
{first="Fred", middle="W", last="Smith"})

(* test for task21 a *)
val Test21 = card_color((Diamonds, Num 4))

(* test for task22 b *)
val Test22 = card_value((Clubs, Num 1))

(* test for task23 c *)
val Test23 = remove_card([(Hearts, Ace)], (Hearts, Ace), IllegalMove)

(* test for task24 d *)
val Test24 = all_same_color([(Hearts, Ace), (Hearts, Ace)]) 

(* test for task25 e *)
val Test25 = sum_cards([(Clubs, Num 2),(Clubs, Num 2)])

(* test for task26 f *)
val Test26 = score([(Hearts, Num 2),(Clubs, Num 4)],10)

(* test for task27 g *)
val test27 = officiate([(Hearts, Num 2),(Clubs, Num 4)],[Draw], 15)




