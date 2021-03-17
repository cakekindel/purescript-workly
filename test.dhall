let conf = ./spago.dhall

in conf // { sources = conf.sources # [ "test/**/*.purs" ]
           , dependencies = conf.dependencies
                          # [ "spec"
                            , "aff-promise"
                            , "spec-mocha"
                            , "refs"
                            ]
           }
