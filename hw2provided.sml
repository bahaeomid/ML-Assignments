(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)

fun all_except_option ([],str) = NONE
  | all_except_option ( x :: xs,str) = 
    case same_string(x, str) of 
      true  => SOME xs
    | false => case all_except_option(xs,str) of 
                 NONE   => NONE
               | SOME y => SOME (x::y)



 						           
fun get_substitutions1 ([],str) = [] 
  | get_substitutions1 (first :: rest,str) =  
    case all_except_option(first,str) of
      NONE   => get_substitutions1(rest,str)
    | SOME y => y @ get_substitutions1(rest,str) 		  


fun similar_names (strlistlist,strtup)=
    let 
	
        fun getstr (w)=
	    case w of 
	    {first=i,middle=j,last=k} => i
	val namelist=get_substitutions1(strlistlist,getstr(strtup))
	fun replace (z,strlist1)=
	    case (z,strlist1) of
	    ({first=i,middle=j,last=k},hd1::tl) => {first=hd1,middle=j,last=k}::replace(z,tl)
	    | _ => [] 
	    
      in 

	strtup :: replace(strtup,namelist)
    end
	
	   
	    


(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* put your solutions for problem 2 here *)
fun card_color (card) =
    case card of
	(Clubs,_) => Black
	|(Spades,_)  => Black
        | _ => Red

fun card_value (card) =
    case card of
	(_,Ace) => 11
        | (_,Num i)=> i
	| _ => 10

fun remove_card (cs,c,e)=
    case cs of
	[] => raise e
        | first::rest  => if c=first
			  then rest
			  else first :: remove_card(rest,c,e)

fun all_same_color (cs)=
    case cs of
	[] => true
        | first::rest => let val base=card_color(first) in
			   case rest of 
			     [] => true
			     | first1::rest1 => card_color(first1)=base andalso all_same_color(rest1)
			  end

fun sum_cards (cs)=
    case cs of
	[] => 0
        |first::rest => card_value(first)+sum_cards(rest)

fun score (cs, goal) =
    let val mysum=sum_cards(cs)in
	if (mysum>goal) 
	then 3*(mysum-goal)
        else (goal-mysum)
    end

fun officiate (cs, moves, goal)=
    let val held=[]  in

 case cs of
	[] => score(held,goal)
        |first::rest  => case moves of
			     []=> score(held,goal)
			     |Draw::rest1  => if null cs
				              then score(held,goal)
					      else score(held@first,goal)+ officiate(rest,rest1,goal)
			     | Discard(c)::rest1 => score(remove_card(held,c,IllegalMove),goal)+ officiate(rest,rest1,goal)



end
