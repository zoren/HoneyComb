namespace HoneyComb

module ShortestPath =
  let inc v i = (v + i + 6) % 6

  type Cell<'a> =
    {
      Label : 'a
      mutable Neighbours : Cell<'a> option[]
    }

  let mkCell label = {Label = label; Neighbours = Array.zeroCreate 6}

  let connect c1 dir c2 =
    let setNeighbour c dir c' =
      if Option.isSome c.Neighbours.[dir]
      then
        failwithf "already set %A %i, was %A, trying to set %A" c1.Label dir (Option.get c.Neighbours.[dir]).Label <| c'.Label
      c.Neighbours.[dir] <- Some c'
    setNeighbour c1 dir c2
    setNeighbour c2 (inc dir 3) c1

  open System.Collections.Generic

  exception ReachedMax

  let makeComb max =
    let list = List()
    let init = mkCell 1
    list.Add init
    let currentCellRef = ref init
    let stopRef = ref false
    let create dir =
      let currentCell = !currentCellRef
      let lab = currentCell.Label + 1
      if lab > max
      then raise ReachedMax
      let newNode = mkCell lab
      list.Add newNode
      connect currentCell dir newNode
      let rotDir = inc dir 1
      let prevRotOpt = currentCell.Neighbours.[rotDir]
      prevRotOpt
        |> Option.iter (fun prevRot ->
          connect newNode (inc dir 2) prevRot
          prevRot.Neighbours.[dir]
            |> Option.iter (fun n' -> connect newNode (inc dir 1) n')
          )
      currentCellRef := newNode
    try
      let ringRef = ref 1
      while true do
        let ring = !ringRef
        incr ringRef
        create 0
        for i = 0 to ring - 2 do
          create 1
        for dir = 2 to 6 do
          for i = 0 to ring - 1 do
            create <| dir % 6
      failwith "internal error"
    with
    | ReachedMax -> list

  let findAll cell =
    let s = HashSet(HashIdentity.Reference)
    let rec loop c =
      if s.Add c
      then c.Neighbours |> Seq.choose id |> Seq.iter loop
      else ()
    loop cell
    s :> _ seq

  let tryGetValueOpt (d:Dictionary<_,_>) k =
    match d.TryGetValue k with
    | true, v -> Some v
    | false, _ -> None

  // shameless copy from wikipedia
  let djikstra source =
    let Q = HashSet(HashIdentity.Reference)
    findAll source |> Seq.iter (ignore << Q.Add)
    let dist = Dictionary(HashIdentity.Reference)
    Q |> Seq.iter (fun c -> dist.[c] <- System.Int32.MaxValue)
    dist.[source] <- 0
    while Q.Count <> 0 do
      let u =
        Q
          |> Seq.choose (fun v -> (tryGetValueOpt dist v) |> Option.map (fun d -> v, d))
          |> Seq.minBy snd |> fst
      ignore <| Q.Remove u
      u.Neighbours
        |> Seq.choose id
        |> Seq.iter
          (fun v ->
            assert (dist.[u] <> System.Int32.MaxValue)
            let alt = dist.[u] + 1
            if alt < dist.[v]
            then
              dist.[v] <- alt)
    dist

  let shortest src dest =
    let myComb = makeComb <| max src dest
    let get n = myComb.[n-1]
    let distDic = djikstra <| get src
    distDic.[get dest]
