{ name         = "workly"
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
, license      = "Unlicense"
, repository   = "https://github.com/cakekindel/purescript-workly"
}
