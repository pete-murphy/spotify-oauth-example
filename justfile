set dotenv-load := true

default:
    just --list

run:
    cabal run -v0
