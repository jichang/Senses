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

    let pageParser: Parser<Page->Page, Page>  =
        oneOf
            [ map Home top
              map Sign (s "sign")
              map Datasets (s "datasets")
              map DatasetCreate (s "datasets" </> s "create")
              map DatasetDetails (s "datasets" </> i64) ]
