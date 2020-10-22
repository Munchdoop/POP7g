type card = int
type deck = card list 

//deal-funktion
let deal (d: deck) : deck*deck =
    let rec stack dl (xs, ys) =
        match dl with
        | x::y::rest -> stack rest (x::xs, y::ys)
        | [x] -> (x::xs, ys)
        | _ -> (xs, ys)
    stack d ([], [])

let a = deal([4;9;2;5;3])
let b = deal([])
let c = deal([2])
printf "%A" a
printf "%A" b
printf "%A" c

//randomfunktion - makes random list 
let rand (n:int) =
    let rnd = System.Random()
    List.init n (fun _ -> rnd.Next (0,n))

let deck1 = rand 52
printfn "%A" deck1

//shuffle funktion, shuffle random list. 
let shuffle (de:deck) : deck =
    let divide = (List.length de) / 2
    de
    |> List.mapi (fun x y -> (if x < divide then x *2 else ((x-divide)*2)+1), y)
    |> List.sortBy fst 
    |> List.map snd

let f = shuffle(deck1)

//newdeck - skla lave et dæk af 4 serier med 2-14 tal (et reelt kortdæk)
let newdeck() : deck=


 