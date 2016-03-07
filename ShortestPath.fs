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

  let findAll cell =
    let s = System.Collections.Generic.HashSet(HashIdentity.Reference)
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
