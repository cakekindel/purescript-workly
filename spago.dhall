{ name         = "purescript-workly"
, dependencies = [ "console"
                 , "effect"
                 , "psci-support"
                 , "maybe"
                 , "functions"
                 , "newtype"
                 , "avar"
                 ]
, packages     = ./packages.dhall
, sources      = [ "src/**/*.purs" ]
}
