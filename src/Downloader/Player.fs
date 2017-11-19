open System
open Scraper

let rec displayList choice choices =
    // Traverses a list of results incrementing the associated number every call, printing each result with its associated number
    match choices with
    | [] -> printfn "-"
    | [(name, _)] -> printfn "%i. %s" choice name
    | (name, _) :: tail ->
    begin
        printfn "%i. %s" choice name;
        displayList (choice + 1) tail
    end

let rec findUrl n (songs: (string * string) list) choice  =
    // Looks up the url of a search result's associated number
    match songs with
    | [] -> None
    | [(_, url)] -> if n = choice then Some(url) else None
    | (_, url) :: tl -> if n = choice then Some(url) else findUrl (n + 1) tl choice

let exec (cmdString: string) =
    // Creates and executes a System.Diagnostics.Process with given args
    let args = cmdString.Split [|' '|]
    let executableProcess = new System.Diagnostics.Process()
    let startInfo = System.Diagnostics.ProcessStartInfo()
    startInfo.CreateNoWindow <- true
    startInfo.FileName <- args.[0]
    startInfo.Arguments <- args.[1..] |> String.concat " "
    startInfo.UseShellExecute <- false
    startInfo.RedirectStandardOutput <- true
    executableProcess.StartInfo <- startInfo
    executableProcess.Start() |> ignore
    executableProcess.StandardOutput.ReadToEnd()

let downloadSong song =
    // Downloads, plays and deletes a song given a url
    exec ("youtube-dl --extract-audio --audio-format mp3 --output \"song.%(ext)s\" \"" + song + "\"") |> ignore
    //let dlCode = 
    //    exec ("youtube-dl -F " + song)
    //    |> (fun table -> table.Split [|'\n'|])
    //    |> Array.toSeq
    //    |> Seq.filter (fun row -> row.Contains("audio only"))
    //    |> Seq.map (fun row -> row.Split [|' '|])
    //    |> Seq.item 0
    //    |> Array.item 0
    //
    //exec (sprintf "youtube-dl -f %s %s" dlCode song) |> ignore
    exec "cvlc --play-and-exit song.mp3" |> ignore
    exec "rm song.mp3" |> ignore
    0

[<EntryPoint>]
let main argv =
    // Gets search results from arguments and allows the user to choose one, then continues to look up its url and plays the url if it exists
    let args = argv.[1..] |> String.concat " "
    let results = Scraper.search args |> Seq.toList
    results
    |> displayList 1
    printf "Your choice: "
    let songUrl = 
        Console.ReadLine()
        |> Int32.Parse
        |> findUrl 1 results

    match songUrl with
    | Some(url) -> downloadSong url
    | None -> 1