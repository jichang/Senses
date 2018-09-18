module Sign

open Elmish
open Elmish.Browser.Navigation
open Fable.Import.React
open Fable.Helpers.React
open Fable.Helpers.React.Props
open Fable.PowerPack.Fetch

open Thoth.Json
open Shared.Model

type Model =
    { loading: bool
      signing: bool
      totalCount: int64
      users: User list
      choosedUser: User option }

type Msg =
    | Init of Result<ModelCollection<User>, exn>
    | Choose of User option
    | Submit
    | CreateResponse of Result<User, exn>
    | LoginResponse of Result<Session, exn>

let init () =
    let model =
        { loading = true
          signing = false
          totalCount = 0L
          users = List.empty
          choosedUser = None }

    let cmd =
        let defaultProps =
            [ RequestProperties.Method HttpMethod.GET
              requestHeaders
                  [ ContentType "application/json" ]]

        let decoder = ModelCollection.Decoder User.Decoder
        Cmd.ofPromise
            (fun _ -> fetchAs "/api/users" decoder defaultProps)
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
                Cmd.ofPromise
                    (fun _ -> fetchAs "/api/sessions" Session.Decoder defaultProps)
                    ()
                    (Ok >> LoginResponse)
                    (Error >> LoginResponse)
            { model with signing = true }, cmd
        | None ->
            let defaultProps =
                [ RequestProperties.Method HttpMethod.POST
                  requestHeaders [ContentType "application/json"] ]
            let cmd =
                Cmd.ofPromise
                    (fun _ -> fetchAs "/api/users" User.Decoder defaultProps)
                    ()
                    (Ok >> CreateResponse)
                    (Error >> CreateResponse)
            { model with signing = true }, cmd
    | CreateResponse (Ok user) ->
        let cmd = Navigation.newUrl "/datasets"
        { model with users = List.append model.users [user]; signing = false }, cmd
    | CreateResponse (Error exn) ->
        { model with signing = false }, Cmd.none
    | LoginResponse (Ok session) ->
        Token.save session
        { model with signing = false }, Cmd.none
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

        let chooseUser (evt: FormEvent) dispatch =
            let userId = evt.Value
            let msg =
                match userId with
                | "" ->
                    Choose None
                | _ ->
                    let user = Seq.tryFind (fun (user: User) -> user.id.ToString() = userId) model.users
                    Choose user
            dispatch msg

        let submit (evt: FormEvent) dispatch =
            evt.preventDefault()

            dispatch Submit

        let signForm =
            form [ classList [("form", true); ("form--sign", true)]; OnSubmit (fun evt -> submit evt dispatch ) ]
                [ div [ classList [("form__field", true)] ]
                    [ label [] [str "Choose account"]
                      div [ classList [("form__control", true) ] ] [ select [ OnChange (fun evt -> chooseUser evt dispatch ); DefaultValue ""] allOptions ] ]

                  div [ classList [("form__field", true); ("form__field--submit", true)] ]
                    [ div [] [ button [ Disabled model.signing ] [ str "Start" ] ] ] ]

        div [] [ SiteLogo.logo; signForm ]
