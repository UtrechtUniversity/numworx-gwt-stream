port module Ports exposing (JsonPortData, downloadToast, fileContentRead, fileSelected)

{--

  Allows file upload
  Great thanks to the tutorial at Paramander, written by Tolga Paksoy https://www.paramander.com/blog/using-ports-to-deal-with-files-in-elm-0-17

--}


type alias JsonPortData =
    { contents : String
    , filename : String
    }


port fileContentRead : (JsonPortData -> msg) -> Sub msg


port fileSelected : String -> Cmd msg


port downloadToast : String -> Cmd msg
