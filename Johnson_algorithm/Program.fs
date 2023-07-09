open System.Collections.Generic
open System.Linq

let initListF n (f: int -> 'a) = List(Enumerable.Range(0, n).Select(f))

let initListC n c = List(Enumerable.Repeat(c, n))

type Graph = List<int * List<int>>

let index (g: Graph) v = g[v] |> fst (* 元のグラフでの頂点番号 *)
let edges (g: Graph) v = g[v] |> snd

let addEdge (g: Graph) x y = (edges g x).Add y

(* 各j について頂点集合V_j = { i | group[i] = j } からなる部分グラフを作成。
       group[i]が未定義の頂点はどのグラフにも加えられない *)
let subgraphs (g: Graph) (group: List<int>) =
    let numGroups = group.Max() + 1
    let newIndex = initListC group.Count (-1)
    let res = initListF numGroups (fun _ -> List())

    for v = 0 to group.Count - 1 do
        let group = group[v]
        newIndex[v] <- res[group].Count
        res[ group ].Add(index g v, List())

    for v = 0 to group.Count - 1 do
        for u in edges g v do
            if u < group.Count && group[v] = group[u] then
                addEdge res[group[v]] newIndex[v] newIndex[u]

    res

(* 0, ..., size-1番目までの頂点からなる部分グラフについて強連結成分分解する *)
let tarjanWithSize (g: Graph) size =
    let ord = List(Enumerable.Repeat(-1, size))
    let low = List(Enumerable.Repeat(-1, size))
    let group = List(Enumerable.Repeat(-1, size))
    let stack = Stack<int>(size)
    let mutable nextOrd = 0
    let mutable numGroups = 0

    let rec dfs v =
        ord[v] <- nextOrd
        low[v] <- nextOrd
        nextOrd <- nextOrd + 1
        stack.Push v

        for u in edges g v do
            if u < size then
                if ord[u] = -1 then
                    dfs u
                    low[v] <- min low[v] low[u]
                else if group[u] = -1 then
                    low[v] <- min low[v] ord[u]

        if ord[v] = low[v] then
            let rec loop () =
                let u = stack.Pop()
                group[u] <- numGroups
                if v <> u then loop ()

            loop ()
            numGroups <- numGroups + 1

    for v = 0 to size - 1 do
        if ord[v] = -1 then dfs v

    group


type State =
    { g: Graph
      start: int
      stack: Stack<int>
      blocked: List<bool>
      blockMap: List<HashSet<int>> }

let rec unblock (s: State) u =
    s.blocked[ u ] <- false

    for w in s.blockMap[u] do
        if s.blocked[w] then unblock s w

    s.blockMap[ u ].Clear()


let rec circuit (res: List<List<int>>) (s: State) v =
    let mutable f = false
    s.stack.Push v
    s.blocked[ v ] <- true

    for w in edges s.g v do
        if w = s.start then
            res.Add(List(s.stack.Select(index s.g)))
            f <- true
        else if not s.blocked[w] then
            if circuit res s w then f <- true

    if f then
        unblock s v
    else
        for w in edges s.g v do
            if not (s.blockMap[ w ].Contains v) then
                s.blockMap[ w ].Add v |> ignore

    s.stack.Pop() |> ignore

    f

let rec searchSCG (res: List<List<int>>) (g: Graph) =
    let n = g.Count

    if n <= 1 then
        ()
    else
        let state =
            { g = g
              start = n - 1
              stack = Stack()
              blocked = initListC n false
              blockMap = initListF n (fun _ -> HashSet()) }

        ignore <| circuit res state state.start

        let hs = subgraphs g (tarjanWithSize g (n - 1))

        for h in hs do
            searchSCG res h

let johnson (g: Graph) =
    let res = List()
    let hs = subgraphs g (tarjanWithSize g g.Count)

    for h in hs do
        searchSCG res h

    res


(*---------------------------------------------------------------------------*)
let print (res: List<List<int>>) =
    for r in res do
        for x in r do 
            System.Console.Write(x)
            System.Console.Write(" ")
        System.Console.Write("\n")


let fromList (g: int list list) : Graph =
    initListF g.Length (fun i -> (i, List(g.Item i)))

let g =
    fromList
    <| [ []
         [ 8; 2 ]
         [ 9; 7; 3 ]
         [ 2; 4; 6; 1 ]
         [ 5 ]
         [ 2 ]
         [ 4 ]
         []
         [ 9 ]
         [ 8 ] ]

print (johnson g)
