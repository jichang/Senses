module Router
    open Browser
    open Elmish.UrlParser

    let currentUrl () = window.location.href

    let i64 state =
        custom "i64" (System.Int64.TryParse >> function true, value -> Ok value | _ -> Error "Can't parse int64" ) state

    type Page =
        | Home
        | Sign of string option
        | Datasets
        | DatasetCreate
        | DatasetDetails of int64
        | DatasetTaskCreate of int64
        | DatasetTaskDetails of int64 * int64
        | DatasetSliceCreate of int64
        | DatasetSliceDetails of int64 * int64
        | Tasks
        | Labels
        | LabelCreate

    let curry2 f x y = f (x, y)

    let pageParser: Parser<Page->Page, Page>  =
        oneOf
            [ map Home top
              map Sign (s "sign" <?> stringParam "redirectUrl")
              map Datasets (s "datasets")
              map DatasetCreate (s "datasets" </> s "create")
              map DatasetDetails (s "datasets" </> i64)
              map DatasetSliceCreate (s "datasets" </> i64 </> s "slices" </> s "create")
              map (curry2 DatasetSliceDetails) (s "datasets" </> i64 </> s "slices" </> i64)
              map DatasetTaskCreate (s "datasets" </> i64 </> s "tasks" </> s "create")
              map (curry2 DatasetTaskDetails) (s "datasets" </> i64 </> s "tasks" </> i64)
              map Labels (s "labels")
              map LabelCreate (s "labels" </> s "create")
              map Tasks (s "tasks") ]
