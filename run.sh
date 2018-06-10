echo Compiling Elm code
elm make elm/src/Main.elm --output public/elm.js
echo Starting server
crystal run src/elm-planning-poker.cr
echo Opening in browser
open http://localhost:8080/index.html