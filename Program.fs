open System.IO

let inputFile = File.ReadAllLines "/tmp/aoc/input" |> Array.toList

let inputChars = inputFile |> List.map (fun s -> s.ToCharArray () |> Array.toList)

inputChars |> List.map (printfn "%A")

type Pos = int*int
let width = inputChars.Head.Length
let height = inputChars.Length

printfn $"({width},{height})"

let map = [ for x in [0..width-1] do
            for y in [0..height-1] do
            yield ((x,y),(inputChars[y][x],1000000)) ] |> Map.ofList

let neighbors1 ((x,y):Pos) =
   [(x+1,y);(x-1,y);(x,y-1);(x,y+1)]

let neighborsF ((x,y):Pos) =
   let d = [-1;0;1]
   [ for dx in d do
     for dy in d do
     yield (x+dx,y+dy) ] |> List.filter (fun p -> p = (0,0) |> not)

let rec search (path:char list) (best:int) (steps:int) (top:char) (map:Map<Pos,(char*int)>) (pos:Pos) : (int*Map<Pos,char*int>) =
    // printfn $"(search steps={steps} best={best} map={map})"
    let (curr,currbest) = map[pos]
    let path = curr::path 
    if steps >= best then 1000000,map
    elif steps >= currbest then 1000000,map
    else
    let map = map.Add (pos,(curr,steps))
    let cands : Pos list = neighbors1 pos
    let cands = cands |> List.filter map.ContainsKey
    let cands = cands |> List.map (fun p -> p,map[p])
    let cands = cands |> List.filter (fun (_,(c,_)) ->
                            if c = 'E' then curr = top
                            elif curr = 'S' then c = 'a'
                            else (c |> int) < ((curr |> int) + 2))
    printfn $"(curr: {pos}='{curr} cands={cands}"
    let cands = cands |> List.map (fst)
    if curr = 'E' then
        printf $"END: "
        path |> List.map (printf "%c")
        printfn ""
        steps,map
    else
        let rec findNext best map (cands:Pos list) =
            match cands with
            | [] -> best,map
            | cand::rest ->
                let (value,map) = search path best (steps+1) top map cand
                let best = min best value
                // printfn $"best={best}"
                findNext best map rest
        findNext best map cands 
let top = map.Values |> Seq.max |> fst 
printfn $"top={top}"

let start = (map |> Seq.filter (fun p -> (p.Value |> fst) = 'S') |> Seq.head).Key
printfn $"POS={start}"

// search [] 475 0 top map start |> printfn "%A"

let rec addAll (map:Map<'a,'b>) (entries:list<'a*'b>) =
    match entries with
    | [] -> map 
    | entry::rest ->
        addAll (map.Add entry) rest
        
let rec updateMap (map:Map<Pos,char*int>) =
    let canClimbFrom (curr:char) ((prev,_):char*int) =
        if prev = 'S' then curr = 'a'
        elif curr = 'E' then prev > 'x'
        else (curr |> int) <= (prev |>int) + 1
    let updateValue (pos:Pos) : Option<Pos*(char*int)> =
        let (c,value) = map[pos]
        let neis = neighbors1 pos |> List.filter (map.ContainsKey) |> List.map (fun p -> map[p])
        let neis = neis |> List.filter (canClimbFrom c)
        let neis = 100000::(neis |> List.map snd) |> List.min
        let neis = neis + 1
        if c = 'E' then printfn $"E {value}: neis={neis}"
        if value > neis then Some(pos,(c,neis)) else None 
    let poss = map.Keys |> Seq.toList
    let updates = poss |> List.map updateValue |> List.filter Option.isSome |> List.map Option.get 
    if updates.IsEmpty then map 
    else
        let map = addAll map updates
        updateMap map
    
let map1 = map.Add(start,('S',0))
let map2 = updateMap map1
map2 |> printfn "%A"

let peak = (map2 |> Seq.filter (fun p -> (p.Value |> fst) = 'E') |> Seq.head)

printfn $"{peak}"

let mapt2 = map |> Seq.toList
            |> Seq.map (fun k -> if (fst k.Value) = 'a' then (k.Key,('a',0)) else (k.Key,k.Value))
            |> Map.ofSeq
mapt2 |> printfn "%A"

let mapt2_2 = updateMap mapt2
let peak2 = (mapt2_2 |> Seq.filter (fun p -> (p.Value |> fst) = 'E') |> Seq.head)

printfn $"{peak2}"
