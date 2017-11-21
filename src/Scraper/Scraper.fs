module Scraper

open FSharp.Data

// Creates a Results type using the Html type provider using "twice likey" as the example page
type Results = HtmlProvider<"https://www.youtube.com/results?search_query=twice+likey">
type Video = HtmlProvider<"https://www.youtube.com/watch?v=sTWsjLjCsA0">

let removeSpaces (str: string) = str.Replace(" ", "_")

let search keyword =
    // Scrapes a search page by filtering out links that don't have "watch" or have "googleads" in them
    let url = "https://www.youtube.com/results?search_query=" + keyword
    let results = Results.Load(url)
    results.Html.Descendants ["a"]
    |> Seq.choose (fun it -> it.TryGetAttribute("href") |> Option.map (fun a -> it.InnerText() |> removeSpaces , "https://youtube.com" + a.Value()))
    |> Seq.filter (fun (name, url) -> url.Contains("watch") && name <> "" && url.Contains("googleads") |> not )


let cleanName (name: string): string =
    let durationIndex: int = name.IndexOf("Duration")
    name.[1..(durationIndex-5)]
    |> removeSpaces

let findNext (url: string) =
    let video: Video = Video.Load(url)
    video.Html.Descendants ["a"]
    |> Seq.choose (fun it -> it.TryGetAttribute("href") |> Option.map (fun a -> it.InnerText(), a.Value() |> sprintf "https://youtube.com%s"))
    |> Seq.filter (fun (_, it) -> it.Contains("watch?v="))
    |> Seq.map (fun (it, url) -> (it |> cleanName, url))
    |> Seq.item 0