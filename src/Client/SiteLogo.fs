module SiteLogo

open Fable.Helpers.React
open Fable.Helpers.React.Props

let logo =
    div [ classList [("site-logo", true)] ] [ img [ Src "/images/logo.svg" ] ]
