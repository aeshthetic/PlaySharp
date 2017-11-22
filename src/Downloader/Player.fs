open System
open Scraper
open FSharp.Data

type Config = JsonProvider<"config.json">
let config = Config.Load("config.json")
let rec displayList choice (choices: Song list) =
    // Traverses a list of results incrementing the associated number every call, printing each result with its associated number
    match choices with
    | [] -> printfn "-"
    | [{name = songName}] -> printfn "%i. %s" choice songName
    | {name = songName} :: tail ->
    begin
        printfn "%i. %s" choice songName;
        displayList (choice + 1) tail
    end

let rec findUrl n (songs: Song list) choice  =
    // Looks up the url of a search result's associated number
    match songs with
    | [] -> None
    | [{name = songName; url = link; duration = timeSpan}] -> if n = choice then Some({name = songName; url = link; duration = timeSpan}) else None
    | {name = songName; url = link; duration = timeSpan} :: tl -> if n = choice then Some({name = songName; url = link; duration = timeSpan}) else findUrl (n + 1) tl choice

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


let rec playOnline played autoLimit (initialSong: Song) =
    // Recursively downloads and plays a song (and suggested songs) until $autoLimit songs have been played
    let ffmpegBin = config.FfmpegBinary
    if played >= autoLimit then
        0
    else
        printfn "Downloading and playing %s..." (initialSong.name)
        exec ("youtube-dl --extract-audio --audio-format vorbis --output \""+config.DownloadPath+(initialSong.name |> removeSpaces)+".%(ext)s\" \"" + initialSong.url + "\"") |> ignore
        exec (sprintf "%s -loglevel quiet -i %s -acodec libmp3lame %s.mp3" ffmpegBin (sprintf "%s%s.ogg" config.DownloadPath (initialSong.name |> removeSpaces)) (config.DownloadPath + (initialSong.name |> removeSpaces))) |> ignore
        initialSong.name
        |> removeSpaces
        |> sprintf "vlc --play-and-exit %s%s.mp3" config.DownloadPath
        |> exec
        |> ignore
        playOnline (played + initialSong.duration) autoLimit (initialSong |> findNext)

[<EntryPoint>]
let main argv =
    if not (System.IO.Directory.Exists config.DownloadPath) then
        System.IO.Directory.CreateDirectory(config.DownloadPath) |> ignore
    // Gets search results from arguments and allows the user to choose one, then continues to look up its url and plays the url if it exists
    let searchTerm = 
        match config.FirstSearch with
        | "BLANK" -> 
        begin
            printfn "Choose a song: "
            Console.ReadLine()
        end
        | value -> value

    let results = Scraper.search searchTerm |> Seq.toList
    results
    |> displayList 1
    printf "Your choice: "
    let song = 
        Console.ReadLine()
        |> Int32.Parse
        |> findUrl 1 results

    printf "How long should music play? (HH:MM:SS) "
    let limit =
        Console.ReadLine() |> TimeSpan.Parse

    match song with
    | Some(initialSong) -> playOnline ("00:00:00" |> TimeSpan.Parse) limit initialSong
    | None -> 1