all: elm 

elm: app.js 

app.js: frontend/*.elm
	elm-make frontend/Main.elm --yes --output static/app.js

haskell: app/*.hs
	stack build

run: all
	stack exec fn-roast

clean:
	-rm -r static/app.js elm-stuff/ .stack-work/
