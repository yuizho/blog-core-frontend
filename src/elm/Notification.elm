module Notification exposing (MessageType(..), Notification)


type alias Notification =
    { messageType : MessageType
    , message : String
    }


type MessageType
    = Info
    | Success
    | Warn
    | Error
