# Interactive Typechecker

This interactive web application is aimed towards students who want to learn the fundamental concepts of type inference in the *simply typed lambda calculus*.
The type inference is performed in natural deduction style, building a proof tree by making use of the type inference rules.
Future versions intend to include additional type systems. 

## Setup

In order to run the STLC-Typechecker it is required to compile the Elm code using [Elm compiler 0.19.1](https://github.com/elm/compiler/releases/tag/0.19.1).
The compilation process will create a *JavaScript* file named `elm.js`.
Do not change the file name so it can be accessed by `typechecker.html`. Use a web browser of your choice to open `typechecker.html`. This will load up the application in your web browser.

## Manual

A basic manual is given in the application itself. The *start page* is the initial page and covers all relevant instructions for exporting or starting new typechecking exercises.
Exporting an exercise will create a URL with a *prooftree*-query containing the encoded exercise.
The *typechecking site* is the page which opens up after starting an exercise from the start page, or opening the application through a URL which contains a prooftree-query.
All changes you apply to the prooftree on the typechecking site will be tracked automatically by the prooftree query, i.e. you can copy the URL from your browsers address bar at any time to export the current state of your prooftree.

In the top right corner of the typechecking site you can access a basic manual through the '**?**' button. This manual does not cover **mouse and keyboard input** yet.

##### Mouse and Keyboard Input on the Typechecking Page

Here a description for some input functions is given which is not covered by the application manual yet. The keyboard input only works when the website's focus i *not* onto a text input field.

Function | Mouse | Keyboard
--- | --- | ---
Select an inference rule | Leftclick on an inference rule | Press `1`, `2`, `3` to select (Var), (App), (Abs) respectively
Select a tree node | Leftclick on the target tree node | Press `←`, `↑`, `→`, `↓` to select a tree node which is adjacent to the currently selected one
Reset a tree node | Double-Leftclick on the target tree node | Press `DEL` to reset the currently selected tree node
Apply inputs to prooftree (only available when an inference rule is selected) | Leftclick on the `Apply` button | Press `Enter`

## Example Proof Tree

For testing purposes you can use the following URL queries:

##### Exercise / Starting Node

`?prooftree=_AB_{}(λz.((λx.(λy.x))%20z))$?$_H_`

##### Full Prooftree

`?prooftree=_AB_{}(λz.((λx.(λy.x))%20z))$(a→(b→a))$_AP_{z:a}((λx.(λy.x))%20z)$(b→a)$_AB_{z:a}(λx.(λy.x))$(a→(b→a))$_AB_{z:a,x:a}(λy.x)$(b→a)$_V_{z:a,y:b,x:a}x$a$T_V_{z:a}z$a$T`