module Assets exposing (AssetPath(..), assetUrl, logo)


assetUrl : AssetPath -> String
assetUrl (AssetPath str) =
    str


type AssetPath
    = AssetPath String


logo =
    AssetPath "../assets/images/logo.png"
