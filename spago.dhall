{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name =
    "my-project"
, dependencies =
    [ "argonaut-codecs"
    , "console"
    , "control"
    , "effect"
    , "either"
    , "foreign-object"
    , "generics-rep"
    , "heterogeneous"
    , "identity"
    , "leibniz"
    , "lens"
    , "machines"
    , "maybe"
    , "profunctor"
    , "proxy"
    , "psci-support"
    , "record"
    , "record-extra"
    , "test-unit"
    , "type-equality"
    , "typelevel-eval"
    , "typelevel-prelude"
    , "unsafe-coerce"
    , "variant"
    ]
, packages =
    ./packages.dhall
}
