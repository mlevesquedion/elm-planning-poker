elm make elm/src/Main.elm --output public/elm.js
crystal run src/elm-planning-poker.cr &
sleep 5
open http://localhost:8123/index.html