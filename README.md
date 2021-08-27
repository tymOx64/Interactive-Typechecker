# Interactive Typechecker

This interactive web application is aimed towards students who want to learn the fundamental concepts of type inference in the *simply typed lambda calculus*.
The type inference is performed in natural deduction style, building a proof tree by making use of the type inference rules.
Future versions intend to include additional type systems. 

## Setup

TODO

Queries for testing purposes (giving you a starting node or a full prooftree respectively):

`?prooftree=_AB_{}(λz.((λx.(λy.x))%20z))$?$_H_`

`?prooftree=_AB_{}(λz.((λx.(λy.x))%20z))$(a→(b→a))$_AP_{z:a}((λx.(λy.x))%20z)$(b→a)$_AB_{z:a}(λx.(λy.x))$(a→(b→a))$_AB_{z:a,x:a}(λy.x)$(b→a)$_V_{z:a,y:b,x:a}x$a$T_V_{z:a}z$a$T`

## Usage

TODO `🧹` `💊`

#### Hint Function

In case some help is needed, a hint function can be used to get selective guidance for finding the correct input. 
Therefore, a `💡`-button next to each input field can be clicked.

#### Import/Export

TODO