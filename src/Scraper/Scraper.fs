module Scraper

open FSharp.Data

// Creates a Results type using the Html type provider using "twice likey" as the example page
type Results = HtmlProvider<"https://www.youtube.com/results?search_query=twice+likey">
let search keyword =
    // Scrapes a search page by filtering out links that don't have "watch" or have "googleads" in them
    let url = "https://www.youtube.com/results?search_query=" + keyword
    let results = Results.Load(url)
    results.Html.Descendants ["a"]
    |> Seq.choose (fun it -> it.TryGetAttribute("href") |> Option.map (fun a -> it.InnerText(), "https://youtube.com" + a.Value()))
        |> Seq.filter (fun (name, url) -> url.Contains("watch") && name <> "" && url.Contains("googleads") |> not )