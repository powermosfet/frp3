all: haskell 

elm: app.js 

app.js: frontend/*.elm
	elm-make frontend/Main.elm --yes --output static/app.js

haskell: app/*.hs
	stack build --install-ghc

install: haskell
	stack install

run: all
	stack exec frp

develop: app/*.hs
	stack build --pedantic --fast --file-watch

clean:
	-rm -r static/app.js elm-stuff/ .stack-work/
