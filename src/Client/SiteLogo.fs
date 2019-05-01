module SiteLogo

open Fable.React
open Fable.React.Props

let logo =
    div [ classList [("site-logo", true)] ] [ img [ Src "/images/logo.svg" ] ]
