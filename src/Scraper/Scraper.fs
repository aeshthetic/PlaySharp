module Scraper

open FSharp.Data

type Results = HtmlProvider<"https://www.youtube.com/results?search_query=twice+likey">
let search keyword =
    let url = "https://www.youtube.com/results?search_query=" + keyword
    let results = Results.Load(url)
    results.Html.Descendants ["a"]
    |> Seq.choose (fun it -> it.TryGetAttribute("href") |> Option.map (fun a -> it.InnerText(), "https://youtube.com" + a.Value()))
        |> Seq.filter (fun (name, url) -> url.Contains("watch") && name <> "" && url.Contains("googleads") |> not )