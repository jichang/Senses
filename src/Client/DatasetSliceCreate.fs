module DatasetSliceCreate

open Elmish
open Fable.Helpers.React
open Fable.Helpers.React.Props
open Fable.Import.Browser
open Fable.Import.React
open Fable.PowerPack.Fetch
open Fable.Core.JsInterop

open Shared.Model
open Elmish.Browser.Navigation

type Model =
    { uploading: bool
      title: string
      file: obj option
      datasetId: int64 }

type Msg =
    | ChangeTitle of string
    | Choose of obj
    | Submit
    | CreateResponse of Result<ResourceCreateResponse, exn>

let init (datasetId: int64) : Model * Cmd<Msg> =
    { uploading = false; title = ""; file = None; datasetId = datasetId }, Cmd.none

let update (msg: Msg) (model: Model) : Model * Cmd<Msg> =
    match msg with
    | ChangeTitle title ->
        { model with title = title }, Cmd.none
    | Choose file ->
        { model with file = Some file }, Cmd.none
    | Submit ->
        match model.file with
        | Some file ->
            let session: Result<Session, string>  = Token.load ()
            match session with
            | Ok session ->
                let cmd =
                    let authorization = sprintf "Bearer %s" session.token
                    let formData = FormData.Create()
                    formData.append("title", model.title)
                    formData.append("resources", file)
                    let defaultProps =
                        [ RequestProperties.Method HttpMethod.POST
                          requestHeaders
                              [ Authorization authorization ]
                          RequestProperties.Body <| unbox formData ]

                    let apiUrl = sprintf "/api/datasets/%d/slices" model.datasetId

                    Cmd.ofPromise
                        (fun _ -> fetchAs apiUrl ResourceCreateResponse.Decoder defaultProps)
                        ()
                        (Ok >> CreateResponse)
                        (Error >> CreateResponse)

                { model with uploading = true }, cmd
            | _ ->
                model, Cmd.none
        | None ->
            model, Cmd.none
    | CreateResponse (Ok dataset) ->
        let datasetUrl = sprintf "/datasets/%i" model.datasetId
        { model with uploading = false }, Navigation.newUrl datasetUrl
    | CreateResponse (Error err) ->
        { model with uploading = false}, Cmd.none

let view (model: Model) dispatch =
    let chooseFile (evt: FormEvent) =
        let result = evt.target?files?item(0)
        dispatch (Choose result)

    let pageHeader =
        header [ ClassName "flex__box" ]
            [ p [ ClassName "flex__item" ] [ a [ Href (sprintf "/datasets/%d" model.datasetId) ] [ str "  <  " ]; str "Datasets" ] ] 

    div [] [
        pageHeader
        form [ ClassName "form form--vertical"; OnSubmit (fun evt -> evt.preventDefault(); dispatch Submit) ] [
            div
                [ ClassName "form__field" ]
                [ input [ ClassName "form__control"; OnChange (fun evt -> dispatch (ChangeTitle evt.Value)); ] ]
            div
                [ ClassName "form__field square" ]
                [ input [ ClassName "form__control"; OnChange chooseFile; Type "file" ]
                  img [ Src "/images/upload.svg" ]
                  p [ClassName "text--center"] [ str "Choose File" ] ]
            div
                [ ClassName "form__field" ]
                [ button [ClassName "form__control button button--solid button--primary";] [ str "Upload" ] ] ] ]
