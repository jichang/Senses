module Labels

open Elmish
open Fable.Helpers.React
open Fable.Helpers.React.Props
open Fable.PowerPack.Fetch

open Shared.Model
open Elmish.Browser.Navigation

type Model =
    { loading: bool
      totalCount: int64
      labels: Label list }

type Msg =
    | Init of Result<ModelCollection<Label>, exn>
    | Choose of Label
    | Create

let init () =
    let model =
        { loading = true
          totalCount = 0L
          labels = List.empty }

    let session: Result<Session, string> = Token.load ()
    match session with
    | Ok session ->
        let authorization = sprintf "Bearer %s" session.token

        let defaultProps =
            [ RequestProperties.Method HttpMethod.GET
              requestHeaders
                  [ ContentType "application/json" 
                    Authorization authorization ]]

        let cmd =
            let decoder =(ModelCollection<Label>.Decoder Label.Decoder)
            Cmd.ofPromise
                (fun _ -> fetchAs "/api/labels" decoder defaultProps)
                ()                
                (Ok >> Init)
                (Error >> Init)
        model, cmd
    | Error _ ->
        model, Cmd.none

let update (msg: Msg) (model: Model) =
    match msg with
    | Init (Ok response) ->
        { model with totalCount = response.totalCount; labels = response.items; loading = false }, Cmd.none
    | Init (Error exn) ->
        { model with loading = false }, Navigation.newUrl "/sign"
    | Choose label ->
        let labelUrl = sprintf "/labels/%i" label.id
        model, Navigation.newUrl labelUrl
    | Create ->
        model, Navigation.newUrl "/labels/create"


let view (model: Model) (dispatch: Msg -> unit) =
    if model.loading then
        div [] [ str "loading" ]
    else
        let labelRow (label: Label) =
            let labelId = label.id.ToString()
            tr []
                [ td [] [ str labelId ]
                  td [] [ str label.title ]
                  td [] [ str label.color ] ]

        let labelsRow =
            if Seq.isEmpty model.labels then
               [ tr [ ] [ th [ColSpan 3] [p [classList [("text--placeholder", true)]] [ str "No labels yet" ] ] ] ]
            else
                Seq.map labelRow model.labels
                |> Seq.toList

        let labelsTable =
            let header =
                thead []
                    [ tr [] 
                        [ th [ classList [("text--left", true)] ] [ str "Id" ]
                          th [ classList [("text--left", true)] ] [ str "Title" ]
                          th [ classList [("text--left", true)] ] [ str "Color" ] ] ]

            table [  classList [("table table--fullwidth", true)] ]
                [ header; tbody [] labelsRow ]

        let tableHeader =
            header [ classList [("flex__box", true)] ]
                [ p [ classList [("flex__item", true)] ] [ a [ Href "/" ] [ str "  <  " ]; str "Labels" ]
                  button [ classList [("button--primary button--solid", true)]; OnClick (fun _ -> dispatch Create )] [ str "Create Label" ] ] 

        div []
            [ div [] [ tableHeader; labelsTable] ]
