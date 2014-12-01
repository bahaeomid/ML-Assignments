
fun is_older (date1 ,date2)=
    
    case (date1, date2) of
    ((x1,x2,x3),(y1,y2,y3)) => if x1>y1 then false else if x1=y1 andalso x2>y2 then false else if
x2=y2 andalso x3>y3 then false else if x1=y1 andalso x2=y2 andalso x3=y3 then false else true 

fun number_in_month (dates,month) =
    case (dates, month) of
   ( (_,x,_)::tl1 , hd) => if  x=hd then 1+number_in_month(tl1,hd) else number_in_month(tl1,hd)
     | ([],_) => 0
   
    
fun number_in_months (dates,months)=
    case (dates, months) of
     (dates,hd2::tl2) => number_in_month(dates,hd2)+number_in_months(dates,tl2)
     | _ => 0 
    
fun dates_in_month (dates,month) =
    case (dates, month) of
    ((x,y,z)::tl, hd) => if y=hd then (x,y,z)::dates_in_month(tl,hd) else dates_in_month(tl,hd)
     | _ => []

fun dates_in_months (dates,months)=
    case (dates, months) of
    (dates,hd2::tl2) => dates_in_month(dates,hd2)@ dates_in_months(dates,tl2)
     | _ =>[] 

fun get_nth (strlist,n)=
let val pos=1 in 
 case (strlist, n) of
   (hd1::tl1,n) => if pos=n then hd1 else get_nth(tl1,n-1)
    | _ => "error!"
end

fun date_to_string date =
    let val m=["January","February","March","April","May","June","July","August","September","October","November","December"] in
	case date of
	(x,y,z)=> get_nth(m,y)^" "^Int.toString(z)^", "^Int.toString(x)
    end


fun number_before_reaching_sum (sum,alist)=    
    case alist of
    [] => 0
    |hd::tl  => if sum>hd 
	        then 1+number_before_reaching_sum(sum-hd,tl)
		else number_before_reaching_sum (sum-hd,tl)


fun what_month day=
    let val l=[31,28,31,30,31,30,31,31,30,31,30,31] in
	1+number_before_reaching_sum(day,l)
    end

fun month_range (day1,dayn)=
    if day1>dayn then [] else what_month(day1)::month_range(day1+1,dayn)

fun oldest dates =
    if null dates
    then NONE
    else let val oldest_date=oldest(tl dates) in
	     if isSome oldest_date andalso is_older(hd dates, valOf oldest_date)
	     then SOME (hd dates)
             else oldest_date
			  
		     
    
end
