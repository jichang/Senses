module LabelCreate

open Shared.Model
open Elmish
open Fable.Helpers.React
open Fable.Helpers.React.Props
open Fable.Import.React
open Fable.PowerPack.Fetch
open Thoth.Json
open Elmish.Browser.Navigation

type Model =
    { creating: bool
      labelCreateParams: LabelCreateParams }

type Msg =
    | ChangeTitle of string
    | ChangeColor of string
    | Create
    | CreateResponse of Result<Label, exn>

let init () =
    let model =
        { creating = false
          labelCreateParams = { title = ""; color = "#000000" } }

    model, Cmd.none

let update msg model =
    match msg with
    | ChangeTitle title ->
        let newLabelCreateParams = { model.labelCreateParams with title = title}
        { model with labelCreateParams = newLabelCreateParams }, Cmd.none
    | ChangeColor color ->
        let newLabelCreateParams = { model.labelCreateParams with color = color }
        { model with labelCreateParams = newLabelCreateParams }, Cmd.none
    | Create ->
        let session: Result<Session, string>  = Token.load ()
        match session with
        | Ok session ->
            let cmd =
                let authorization = sprintf "Bearer %s" session.token
                let body = Encode.toString 0 (LabelCreateParams.Encoder model.labelCreateParams)
                let defaultProps =
                    [ RequestProperties.Method HttpMethod.POST
                      requestHeaders
                          [ ContentType "application/json" 
                            Authorization authorization ]
                      RequestProperties.Body <| unbox body ]

                Cmd.ofPromise
                        (fun _ -> fetchAs "/api/labels" Label.Decoder defaultProps)
                        ()
                        (Ok >> CreateResponse)
                        (Error >> CreateResponse)

            { model with creating = true }, cmd
        | Error _ ->
            model, Cmd.none
    | CreateResponse (Ok label) ->
        { model with creating = false }, Navigation.newUrl "/labels"
    | CreateResponse (Error err) ->
        { model with creating = false}, Cmd.none


let view model dispatch =
    let submit (evt: FormEvent) =
        evt.preventDefault()

        dispatch Create

    let changeTitle (evt: FormEvent) =
        dispatch <| ChangeTitle evt.Value

    let changeColor (evt: FormEvent) =
        dispatch <| ChangeColor evt.Value

    form [ OnSubmit submit ]
        [ div [] [ input [ Placeholder "Input label title"; OnChange changeTitle; Value model.labelCreateParams.title ] ]
          div [] [ input [ Placeholder "Input label color"; OnChange changeColor; Value model.labelCreateParams.color ] ]
          div [] [ button [ Disabled model.creating ] [ str "Create Label" ] ] ] 
