module SqlParser.Tests.RuleNumberingTests

open System
open System.IO
open System.Text.RegularExpressions
open Xunit

// A numbered grammar heading, e.g. `11.3 <table definition>`.
let private headingRx = Regex @"^(\d+\.\d+)\s+(.*)$"
// A rule name inside a heading or body, e.g. `<table definition>`.
let private nameRx = Regex @"<([^<>]+)>"
// A rule citation inside a `//` comment, e.g. `// 11.3 <table definition>`.
let private citationRx = Regex @"(\d+\.\d+)\s*<([^<>]+)>"

/// Walk up from the test assembly directory until `sql-2016-grammar.txt` is found.
let private findGrammar () =
    let rec walk (dir: DirectoryInfo) =
        if isNull dir then
            None
        else
            let candidate = Path.Combine(dir.FullName, "sql-2016-grammar.txt")

            if File.Exists candidate then
                Some candidate
            else
                walk dir.Parent

    walk (DirectoryInfo AppContext.BaseDirectory)

type private GrammarIndex =
    {
        /// Rule name -> the number of the clause whose heading names it.
        Numbered: Map<string, string>
        /// Rule name -> every clause number whose heading or body mentions it.
        Mentions: Map<string, Set<string>>
        Current: string
    }

let private mention name current mentions =
    let existing = Map.tryFind name mentions |> Option.defaultValue Set.empty
    Map.add name (Set.add current existing) mentions

let private step (state: GrammarIndex) (line: string) =
    let heading = headingRx.Match line

    if heading.Success then
        let number = heading.Groups.[1].Value

        let names =
            [ for m in nameRx.Matches(heading.Groups.[2].Value) -> m.Groups.[1].Value ]

        let numbered =
            (state.Numbered, names)
            ||> List.fold (fun acc name ->
                if Map.containsKey name acc then
                    acc
                else
                    Map.add name number acc)

        { state with
            Current = number
            Numbered = numbered
            Mentions = (state.Mentions, names) ||> List.fold (fun acc name -> mention name number acc) }
    else
        let names = [ for m in nameRx.Matches line -> m.Groups.[1].Value ]

        { state with
            Mentions =
                (state.Mentions, names)
                ||> List.fold (fun acc name -> mention name state.Current acc) }

let private buildIndex (grammarPath: string) =
    let initial =
        { Numbered = Map.empty
          Mentions = Map.empty
          Current = "" }

    let final = File.ReadAllLines grammarPath |> Array.fold step initial
    final.Numbered, final.Mentions

/// The number a citation should use, or None when the name is mentioned only by
/// a clause other than the cited one (allowed as a cross-reference).
/// An unknown name (absent from the grammar entirely) is an error — reported as "?".
let private expectedNumber
    (numbered: Map<string, string>)
    (mentions: Map<string, Set<string>>)
    (num: string)
    (name: string)
    =
    match Map.tryFind name numbered with
    | Some expected -> if expected = num then None else Some expected
    | None ->
        match Map.tryFind name mentions with
        | Some clauses when Set.contains num clauses -> None
        | Some clauses -> Some(String.concat "/" (clauses |> Set.toList |> List.sort))
        | None -> Some "unknown rule"

let private citationsIn (path: string) =
    File.ReadAllLines path
    |> Array.indexed
    |> Array.collect (fun (i, line) ->
        if line.Contains "//" then
            let comment = line.Substring(line.IndexOf "//" + 2)

            [| for m in citationRx.Matches comment ->
                   Path.GetFileName path, i + 1, m.Groups.[1].Value, m.Groups.[2].Value |]
        else
            [||])

let private failOn (numbered, mentions) (path: string) (failures: ResizeArray<string>) =
    for file, line, num, name in citationsIn path do
        match expectedNumber numbered mentions num name with
        | Some expected ->
            failures.Add(sprintf "%s:%d  %s <%s> -> %s" (Path.GetFileName path) line num name expected)
        | None -> ()

[<Fact>]
let ``Rule-number comments match sql-2016-grammar.txt`` () =
    match findGrammar () with
    | None -> Assert.Skip "sql-2016-grammar.txt not found" |> ignore
    | Some grammarPath ->
        let index = buildIndex grammarPath
        let root = Path.GetDirectoryName grammarPath
        let failures = ResizeArray<string>()

        for dir in [ "SqlParser"; "SqlParser.Tests" ] do
            let sourceDir = Path.Combine(root, dir)

            if Directory.Exists sourceDir then
                for path in Directory.GetFiles(sourceDir, "*.fs") do
                    failOn index path failures

        if failures.Count > 0 then
            Assert.Fail(
                "Rule-number mismatches (see sql-2016-grammar.txt):"
                + Environment.NewLine
                + String.concat Environment.NewLine failures
            )
