module Session exposing (Credential, LoggedinToken, Session(..))


type alias Credential =
    { username : String
    , password : String
    }


type alias LoggedinToken =
    String


type Session
    = Loggedin LoggedinToken
    | Guest (Maybe Credential)
