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
    | [(name, url)] -> if n = choice then Some((name, url)) else None
    | (name, url) :: tl -> if n = choice then Some((name, url)) else findUrl (n + 1) tl choice

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

let rec downloadSong played autoLimit songInfo =
    // Downloads, plays and deletes a song given a url
    if played = autoLimit then
        0
    else
        exec ("youtube-dl --extract-audio --audio-format mp3 --output \"/tmp/PlaySharp/"+(songInfo |> fst)+".%(ext)s\" \"" + (songInfo |> snd) + "\"") |> ignore
        songInfo
        |> fst
        |> sprintf "cvlc --play-and-exit /tmp/PlaySharp/%s.mp3"
        |> exec
        |> ignore
        downloadSong (played+1) autoLimit (songInfo |> snd |> findNext)

[<EntryPoint>]
let main argv =
    if not (System.IO.Directory.Exists @"/tmp/PlaySharp") then
        System.IO.Directory.CreateDirectory(@"/tmp/PlaySharp") |> ignore
    // Gets search results from arguments and allows the user to choose one, then continues to look up its url and plays the url if it exists
    printf "Search for a song: "
    let searchTerm = Console.ReadLine()
    let results = Scraper.search searchTerm |> Seq.toList
    results
    |> displayList 1
    printf "Your choice: "
    let songUrl = 
        Console.ReadLine()
        |> Int32.Parse
        |> findUrl 1 results

    printf "How many songs should play before stopping? "
    let limit =
        Console.ReadLine()
        |> Int32.Parse

    match songUrl with
    | Some(songInfo) -> downloadSong 0 limit songInfo
    | None -> 1