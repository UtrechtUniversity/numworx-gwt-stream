port module Ports exposing (downloadToast, checkpoint)

{--

  Allows interaction with JavaScript
  Originally used for file upload, now only for a modal
  Great thanks to the tutorial at Paramander, written by Tolga Paksoy https://www.paramander.com/blog/using-ports-to-deal-with-files-in-elm-0-17

--}


port downloadToast : String -> Cmd msg
port checkpoint : String -> Cmd msg