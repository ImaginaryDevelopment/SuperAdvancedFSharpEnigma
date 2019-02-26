module Client

open Elmish
open Elmish.React
open Fable.Helpers.React
open Fable.Helpers.React.Props
open Fable.PowerPack.Fetch
open Thoth.Json
open Shared
open Fulma

// The model holds data that you want to keep track of while the application is running
// in this case, we are keeping track of a counter
// we mark it as optional, because initially it will not be available from the client
// the initial value will be requested from server
type Model =
    { Counter : Counter option
      Data: string list list
    }

// The Msg type defines what events/actions can occur while the application is running
// the state of the application changes *only* in reaction to these events
type Msg =
    | Increment
    | Decrement
    | InitialCountLoaded of Result<Counter, exn>

module Server =
    open Shared
    open Fable.Remoting.Client

    /// A proxy you can use to talk to server directly
    let api : ICounterApi =
        Remoting.createApi()
        |> Remoting.withRouteBuilder Route.builder
        |> Remoting.buildProxy<ICounterApi>

let initialCounter = Server.api.initialCounter

// defines the initial state and initial command (= side-effect) of the application
let init() : Model * Cmd<Msg> =
    let initialModel = { Counter = None;Data=[["1";"Hello"];["2";"World"]]}
    let loadCountCmd =
        Cmd.ofAsync initialCounter () (Ok >> InitialCountLoaded)
            (Error >> InitialCountLoaded)
    initialModel, loadCountCmd

// The update function computes the next state of the application based on the current state and the incoming events/messages
// It can also run side-effects (encoded as commands) like calling the server via Http.
// these commands in turn, can dispatch messages to which the update function will react.
let update (msg : Msg) (currentModel : Model) : Model * Cmd<Msg> =
    match currentModel.Counter, msg with
    | Some counter, Increment ->
        let nextModel =
            { currentModel with Counter = Some { Value = counter.Value + 1 } }
        nextModel, Cmd.none
    | Some counter, Decrement ->
        let nextModel =
            { currentModel with Counter = Some { Value = counter.Value - 1 } }
        nextModel, Cmd.none
    | _, InitialCountLoaded(Ok initialCount) ->
        let nextModel = { currentModel with Counter = Some initialCount }
        nextModel, Cmd.none
    | _ -> currentModel, Cmd.none

let safeComponents =
    let components =
        span []
            [ a [ Href "https://github.com/giraffe-fsharp/Giraffe" ]
                  [ str "Giraffe" ]
              str ", "
              a [ Href "http://fable.io" ] [ str "Fable" ]
              str ", "
              a [ Href "https://elmish.github.io/elmish/" ] [ str "Elmish" ]
              str ", "
              a [ Href "https://fulma.github.io/Fulma" ] [ str "Fulma" ]
              str ", "

              a [ Href "https://zaid-ajaj.github.io/Fable.Remoting/" ]
                  [ str "Fable.Remoting" ] ]
    p [] [ strong []
               [ a [ Href "https://safe-stack.github.io/" ] [ str "SAFE" ]
                 str " "

                 a
                     [ Href
                           "https://safe-stack.github.io/docs/template-overview/#template-options" ]
                     [ str "Template" ] ]
           str " powered by: "
           components ]

let showCounter =
    function
    | Some counter -> string counter.Value
    | None -> "Loading..."
let showData =
    function
    | [] | [[]] ->
        None
    | x when x |> Seq.collect id |> Seq.exists(fun _ -> true) |> not -> None
    | x -> Some x
    >> function
        |None -> str "No data loaded"
        |Some tbl ->
            table [Id "myTable"][
                thead [] [
                    tr [] [
                        th [] [str "Index"]
                        th [] [str "Item"]
                    ]
                ]
                tbody [] (
                    tbl
                    |> List.map(fun row ->
                        tr [] (
                            row
                            |> List.map(fun s -> td [] [str s])
                        )
                    )
                )

                ]


let button txt onClick =
    Button.button [ Button.IsFullWidth
                    Button.Color IsPrimary
                    Button.OnClick onClick ] [ str txt ]

let view (model : Model) (dispatch : Msg -> unit) =
    let x = showData model.Data
    div []
        [ Navbar.navbar [ Navbar.Color IsPrimary ]
              [ Navbar.Item.div [] [ Heading.h2 [] [ str "SAFE Template" ] ] ]

          Container.container []
              [ Content.content
                    [ Content.Modifiers
                          [ Modifier.TextAlignment
                                (Screen.All, TextAlignment.Centered) ] ]
                    [ Heading.h3 []
                          [ str
                                ("Press buttons to manipulate counter: "
                                 + showCounter model.Counter) ] ]

                Columns.columns []
                    [ Column.column []
                          [ button "-" (fun _ -> dispatch Decrement) ]

                      Column.column []
                          [ button "+" (fun _ -> dispatch Increment) ] ] ]
          Container.container []
              [ Content.content
                    [ Content.Modifiers
                          [ Modifier.TextAlignment
                                (Screen.All, TextAlignment.Centered) ] ]
                    [ Heading.h3 []
                          [ str "datatables"]
                    ]
                (showData model.Data)
              ]
          Footer.footer []
              [ Content.content
                    [ Content.Modifiers
                          [ Modifier.TextAlignment
                                (Screen.All, TextAlignment.Centered) ] ]
                    [ safeComponents ] ] ]
#if DEBUG

open Elmish.Debug
open Elmish.HMR
#endif


Program.mkProgram init update view
#if DEBUG
|> Program.withConsoleTrace
|> Program.withHMR
#endif

|> Program.withReact "elmish-app"
#if DEBUG
//https://github.com/elmish/templates/issues/36
//|> Program.withDebugger
#endif

|> Program.run
