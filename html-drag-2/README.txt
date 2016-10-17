This is copied from the prior example html-drag-1, and then modified (by editing the .elm file) to permit dragging of multiple objects independently. (I also made all imports explicit, and commented all code I don't yet fully understand.)

To remake index.html, just rerun this command in your shell:

% elm-make htmldrag2.elm --output=index.html

As an Elm beginner, I don't know if I did this modification in the "best way". In particular, the model data structure is a list of object states (not a dict, quadtree, etc). 

(I also used "object" as a variable name, even though it conflicted with Html.object until I removed the implicit import of that symbol.)

To run this example directly in your browser, go to https://oresmus.github.io/elm-examples/html-drag-2 .
