namespace HoneyComb

module GraphViz =
    // change this as needed for your local environment
    let graphVizPath = @"C:\Program Files (x86)\Graphviz2.34\bin\"

    let writeEdge writer (fromNode, label, toNode) =
        fprintfn writer " %s -> %s[label = %s];" fromNode toNode label

    // Create a DOT file for graphviz to read.
    let createDotFile dotFilename edges nodes =
        let inFile = System.IO.Path.Combine(__SOURCE_DIRECTORY__,dotFilename)
        use writer = new System.IO.StreamWriter(path=inFile)
        fprintfn writer "digraph G {"
        fprintfn writer "    fontsize=10;"
        nodes
        |> Seq.iter (fun(name, label) -> fprintfn writer "\"%s\" [label=\"%s\"];" name label)
        edges
        |> Seq.iter (writeEdge writer)
        fprintfn writer "   }"

    // shell out to run a command line program
    let startProcessAndCaptureOutput cmd cmdParams =
        let debug = false

        if debug then
            printfn "Process: %s %s" cmd cmdParams
        let si = new System.Diagnostics.ProcessStartInfo(cmd, cmdParams)
        si.UseShellExecute <- false
        si.RedirectStandardOutput <- true
        use p = new System.Diagnostics.Process()
        p.StartInfo <- si
        if p.Start() then
            if debug then
                use stdOut = p.StandardOutput
                stdOut.ReadToEnd() |> printfn "%s"
                printfn "Process complete"
        else
            failwith "Process failed"

    /// Generate an image file from a DOT file
    /// algo = dot, neato
    /// format = gif, png, jpg, svg
    let generateImageFile dotFilename layout algo format imgFilename =
        let cmd = sprintf @"""%s%s.exe""" graphVizPath algo
        let inFile = System.IO.Path.Combine(__SOURCE_DIRECTORY__,dotFilename)
        let outFile = System.IO.Path.Combine(__SOURCE_DIRECTORY__,imgFilename)
        let cmdParams = sprintf "-K%s -T%s -o\"%s\" \"%s\"" layout format outFile inFile
        startProcessAndCaptureOutput cmd cmdParams

module DotGenerator =
  open ShortestPath

  let cellToDotEdges (cell:Cell<'a>) =
    cell.Neighbours |> Seq.mapi (fun i toNodeOpt -> toNodeOpt |> Option.map (fun toNode -> cell, i, toNode)) |> Seq.choose id

  let cellsToDot cells =
    let n2s (c:Cell<int>) = string c.Label
    let edges = Seq.collect cellToDotEdges cells |> Seq.map (fun (f, i, t) -> n2s f, i.ToString(), n2s t)
    let nodes = cells |> Seq.map (fun cell -> n2s cell, n2s cell)
    nodes, edges

  let mkFile cells =
    let nodes, edges = cellsToDot cells
    let path = "tmp.dot"
    GraphViz.createDotFile path edges nodes
    GraphViz.generateImageFile path "neato" "dot" "png" "honey.png"
