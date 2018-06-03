shards install
cd elm
elm package install
elm make src/Main.elm --output ../public/elm.js
