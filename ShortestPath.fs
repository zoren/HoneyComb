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

  let addRing initCell ring =
    let currentCellRef = ref initCell
    let create dir =
      let currentCell = !currentCellRef
      let newNode = mkCell <| currentCell.Label + 1
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
    create 0
    let startCell = !currentCellRef
    for i = 0 to ring - 2 do
      create 1
    for dir = 2 to 6 do
      for i = 0 to ring - 1 do
        create <| dir%6
    !currentCellRef

  open System.Collections.Generic

  let findAll cell =
    let s = HashSet(HashIdentity.Reference)
    let rec loop c =
      if s.Add c
      then c.Neighbours |> Seq.choose id |> Seq.iter loop
      else ()
    loop cell
    s :> _ seq

  let mkComb n =
    assert (n >= 0)
    let center = mkCell 1
    let c = ref center
    for i = 1 to n do
      c := addRing !c i
    findAll !c

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
