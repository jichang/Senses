module Router
    open Elmish.Browser.UrlParser

    let i64 state =
        custom "i64" (System.Int64.TryParse >> function true, value -> Ok value | _ -> Error "Can't parse int64" ) state

    type Page =
        | Home
        | Sign
        | Datasets
        | DatasetCreate
        | DatasetDetails of int64
        | Tasks
        | Labels
        | LabelCreate
        | LabelDetails of int32

    let pageParser: Parser<Page->Page, Page>  =
        oneOf
            [ map Home top
              map Sign (s "sign")
              map Datasets (s "datasets")
              map DatasetCreate (s "datasets" </> s "create")
              map DatasetDetails (s "datasets" </> i64)
              map Datasets (s "tasks")
              map Labels (s "labels")
              map LabelCreate (s "labels" </> s "create")
              map LabelDetails (s "labels" </> i32) ]
