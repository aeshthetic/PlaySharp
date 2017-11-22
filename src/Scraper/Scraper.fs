module Scraper

open FSharp.Data
open System

// Creates a Results type using the Html type provider using "twice likey" as the example page
type Results = HtmlProvider<"https://www.youtube.com/results?search_query=twice+likey">
type Video = HtmlProvider<"https://www.youtube.com/watch?v=sTWsjLjCsA0">
let removeSpaces (str: string) = str.Replace(" ", "_")

type Song = { name : string; url : string; duration : TimeSpan}

let search keyword =
    // Scrapes a search page by filtering out links that don't have "watch" or have "googleads" in them
    let url = "https://www.youtube.com/results?search_query=" + keyword
    let results = Results.Load(url)
    let vid = results.Html.CssSelect "div.yt-lockup-dismissable"

    let name (node: HtmlNode): string =
        node.CssSelect "a"
        |> List.choose (HtmlNode.tryGetAttribute "title")
        |> Seq.item 0
        |> HtmlAttribute.value


    let link (node: HtmlNode): string =
        node.CssSelect "a"
        |> List.choose (HtmlNode.tryGetAttribute "href")
        |> List.item 0
        |> HtmlAttribute.value
        |> sprintf "https://youtube.com%s"

    let extractTime (times: HtmlNode list list) =
        match times with
        | [] -> "00:00:00"
        | [[time]] -> time.InnerText() |> sprintf "00:%s"
    let rawTime (node: HtmlNode) = 
        node.CssSelect "span.video-time"
        |> List.map HtmlNode.elements

    let time = rawTime >> extractTime

    let rec songList (names, links, times) acc =
        match (names, links, times) with
        | (currentName :: nameTl, link :: linkTl, time :: timeTl) -> songList (nameTl, linkTl, timeTl) ({name = currentName; url = link; duration = TimeSpan.Parse(time)} :: acc)
        | ([], [], []) -> acc

    songList ((vid |> List.map name), (vid |> List.map link), (vid |> List.map time)) []
    |> List.rev

let cleanDuration duration =
    duration
    |> String.length
    |> (fun it -> duration.[13..(it-3)])

let findNext song =
    let video: Video = Video.Load(song.url)
    let contentLink =
        video.Html.CssSelect "li.video-list-item"
        |> List.item 0
    let name =
        contentLink.CssSelect "a"
        |> Seq.choose (HtmlNode.tryGetAttribute "title")
        |> Seq.item 0
        |> HtmlAttribute.value

    let link =
        contentLink.CssSelect "a"
        |> Seq.choose (HtmlNode.tryGetAttribute "href")
        |> Seq.item 0
        |> HtmlAttribute.value
        |> sprintf "https://youtube.com%s"
    
    let duration =
        contentLink.CssSelect "span.accessible-description"
        |> List.item 0
        |> HtmlNode.elements
        |> List.item 0
        |> HtmlNode.innerText
        |> cleanDuration

    {name = name; url = link; duration = TimeSpan.Parse(duration)}