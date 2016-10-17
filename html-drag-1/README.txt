This will start out as a copy of the drag example at http://elm-lang.org/examples/drag , and then [perhaps as copied into other directories] be modified to add various features, such as dragging more than one item at once, behaving differently when different "tools" are active, etc.

To create this directory from an empty state, it was sufficient to:

- make this directory and this README file

- copy the elm example code into htmldrag1.elm (to be further edited later)

- install the elm tools according to the elm guide ("getting started" section)

- run this shell command, and say "yes" to its suggestions:

  % elm-make htmldrag1.elm --output=index.html

- and then as a response to its error message (about not finding module Mouse), run:

  % elm-package install elm-lang/mouse

  % elm-make htmldrag1.elm --output=index.html

which "Successfully generated index.html".

To run this example directly in your browser, navigate to https://oresmus.github.io/elm-examples/html-drag-1/ (which only works because I set up "github pages" on this repository).

To run it locally, just open index.html in a browser. (This is a large, self-contained html file containing a lot of js code. There are also ways of getting elm to put that js code in a separate file, so the html file stays small.)
