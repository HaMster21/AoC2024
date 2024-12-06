// For more information see https://aka.ms/fsharp-console-apps
open System;

type Report =
| UnsafeAlternating of string
| UnsafeAmplitude of string
| SafeTolerable of string
| Safe of string

let isOrderedAscending (elem: int list) = 
    let forceOrder = List.sort elem
    forceOrder = elem

let isOrderedDescending (elem: int list) = 
    let forceOrder = List.sortDescending elem
    forceOrder = elem

let islevelsInCheck levels =
    levels
    |> Seq.pairwise
    |> Seq.forall (fun (a,b) -> 
        match Int32.Abs(a - b) with
        | 1 | 2 | 3 -> true
        | _ -> false)

let checkReport (report:string) = 
    let levels =
        report.Split(" ")
        |> Seq.map (fun x -> Int32.Parse(x))
        |> Seq.toList

    if isOrderedAscending levels || isOrderedDescending levels then
        if islevelsInCheck levels
        then Safe report
        else UnsafeAmplitude report
    else UnsafeAlternating report

let getInput filepath = System.IO.File.ReadAllText(filepath)

[<EntryPoint>]
let main argv =
    let input = getInput("input.txt")
    let checkedReports = 
        input.Split("\n")
        |> Seq.filter (fun x -> x.Length > 0)
        |> Seq.map(fun x -> checkReport x)

    checkedReports |> Seq.iter (fun x -> printfn $"Report: {x}")

    let validReportCount =
        checkedReports
        |> Seq.filter (fun x -> match x with | Safe(_) -> true | _ -> false)
        |> Seq.length
    printfn $"Valid reports: {validReportCount}"
    0
