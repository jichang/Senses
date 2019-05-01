module Sign

open Elmish
open Elmish.Navigation
open Fable.React
open Fable.React.Props

open Thoth.Json
open Shared.Model
open Api
open Fetch
open Browser.Types

type Model =
    { redirectUrl: string option
      loading: bool
      signing: bool
      totalCount: int64
      users: User list
      choosedUser: User option }

type Msg =
    | Init of Result<ModelCollection<User>, exn>
    | Choose of User option
    | Submit
    | CreateResponse of Result<Session, exn>
    | LoginResponse of Result<Session, exn>

let init (redirectUrl: string option) =
    let model =
        { redirectUrl = redirectUrl
          loading = true
          signing = false
          totalCount = 0L
          users = List.empty
          choosedUser = None }

    let cmd =
        let defaultProps =
            [ RequestProperties.Method HttpMethod.GET
              requestHeaders
                  [ ContentType "application/json" ]]

        let decoder = ModelCollection<User>.Decoder User.Decoder
        Cmd.OfPromise.either
            (fun _ -> fetchAs "/api/users" defaultProps decoder)
            ()
            (Ok >> Init)
            (Error >> Init)
    model, cmd

let update (msg: Msg) (model: Model) =
    match msg with
    | Init (Ok response) ->
        { model with loading = false; totalCount = response.totalCount; users = response.items }, Cmd.none
    | Init (Error exn) ->
        { model with loading = false }, Cmd.none
    | Choose user ->
        { model with choosedUser = user }, Cmd.none
    | Submit ->
        match model.choosedUser with
        | Some user ->
            let body = Encode.toString 0 (User.Encoder user)
            let defaultProps =
                [ RequestProperties.Method HttpMethod.POST
                  requestHeaders [ContentType "application/json"]
                  RequestProperties.Body <| unbox body ]
            let cmd =
                Cmd.OfPromise.either
                    (fun _ -> fetchAs "/api/signin" defaultProps Session.Decoder )
                    ()
                    (Ok >> LoginResponse)
                    (Error >> LoginResponse)
            { model with signing = true }, cmd
        | None ->
            let defaultProps =
                [ RequestProperties.Method HttpMethod.POST
                  requestHeaders [ContentType "application/json"] ]
            let cmd =
                Cmd.OfPromise.either
                    (fun _ -> fetchAs "/api/users" defaultProps Session.Decoder)
                    ()
                    (Ok >> CreateResponse)
                    (Error >> CreateResponse)
            { model with signing = true }, cmd
    | CreateResponse (Ok user) ->
        let redirectUrl =
          match model.redirectUrl with
          | Some redirectUrl ->
              redirectUrl
          | None ->
              "/"
        { model with signing = false }, Navigation.newUrl redirectUrl
    | CreateResponse (Error exn) ->
        { model with signing = false }, Cmd.none
    | LoginResponse (Ok session) ->
        let redirectUrl =
          match model.redirectUrl with
          | Some redirectUrl ->
              redirectUrl
          | None ->
              "/"
        { model with signing = false }, Navigation.newUrl redirectUrl
    | LoginResponse (Error exn) ->
        { model with signing = false }, Cmd.none


let view (model: Model) (dispatch: Msg -> unit) =
    if model.loading then
        div [] [ str "loading" ]
    else
        let options =
            model.users
            |> Seq.map (fun user -> option [ Value (user.id.ToString()) ] [ str (user.uuid.ToString()) ] )

        let defaultOption =
            option [ Value "" ] [ str "New Account" ]

        let allOptions = Seq.append [defaultOption] options |> List.ofSeq

        let chooseUser (evt: Event) dispatch =
            let userId = evt.Value
            let msg =
                match userId with
                | "" ->
                    Choose None
                | _ ->
                    let user = Seq.tryFind (fun (user: User) -> user.id.ToString() = userId) model.users
                    Choose user
            dispatch msg

        let submit (evt: Event) dispatch =
            evt.preventDefault()

            dispatch Submit

        let signForm =
            form [ classList [("form", true); ("form--sign", true)]; OnSubmit (fun evt -> submit evt dispatch ) ]
                [ div [ classList [("form__field", true)] ]
                    [ label [] [str "Choose account"]
                      div [ classList [("form__control", true) ] ] [ select [ OnChange (fun evt -> chooseUser evt dispatch ); DefaultValue ""] allOptions ] ]

                  div [ classList [("form__field", true); ("form__field--submit", true)] ]
                    [ div [] [ button [ ClassName "button button--block button--solid button--primary"; Disabled model.signing ] [ str "Start" ] ] ] ]

        div [] [ SiteLogo.logo; signForm ]
