module LabelCreate

open Shared.Model
open Elmish
open Fable.React
open Fable.React.Props
open Fetch
open Thoth.Json
open Elmish.Navigation
open Browser.Types
open Api

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

                Cmd.OfPromise.either
                        (fun _ -> fetchAs "/api/labels" defaultProps Label.Decoder)
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
    let submit (evt: Event) =
        evt.preventDefault()

        dispatch Create

    let changeTitle (evt: Event) =
        dispatch <| ChangeTitle evt.Value

    let changeColor (evt: Event) =
        dispatch <| ChangeColor evt.Value

    let createForm =
        form [ClassName "form form--vertical"; OnSubmit submit ]
            [ div [ClassName "form__field" ] [ input [ClassName "form__control"; Placeholder "Input label title"; OnChange changeTitle; Value model.labelCreateParams.title ] ]
              div [ClassName "form__field"] [ input [ClassName "form__control"; Placeholder "Input label color"; OnChange changeColor; Value model.labelCreateParams.color ] ]
              div [ClassName "form__field"] [ button [ClassName "form__control button--solid button--primary"; Disabled model.creating ] [ str "Create Label" ] ] ] 

    let pageHeader =
        header [ classList [("flex__box", true)] ]
            [ p [ classList [("flex__item", true)] ] [ a [ Href "/labels" ] [ str "  <  " ]; str "Labels" ] ] 

    div [] [createForm]
