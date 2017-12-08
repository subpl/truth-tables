A fairly rough-and-ready pile of haskell which defines and interprets sequential expressions in propositional logic, with the capability to evaluate and pretty-print truth tables for expressions.

#### Usage
```haskell
e2 = And (Not (Or (And (Var 'a') (Var 'b') ) 
                  (And (Var 'c') (Var 'd'))))
         (Or (Var 'a')(And (Var 'c') (And (Var 'd') (Not (Var 'b')))))
```

`e2` corresponds to an expression like `~((a & b) | (c & d)) & (a | (c & d & ~b))`. `table e2` prints the following:

```
truth table for (!((a AND b) OR (c AND d)) AND (a OR (c AND (d AND !b)))):
A       B       C       D       RESULT
False   False   False   False   False
False   False   False   True    False
False   False   True    False   False
False   False   True    True    False
False   True    False   False   False
False   True    False   True    False
False   True    True    False   False
False   True    True    True    False
True    False   False   False   True
True    False   False   True    True
True    False   True    False   True
True    False   True    True    False
True    True    False   False   False
True    True    False   True    False
True    True    True    False   False
True    True    True    True    False
truth density: 3/16 = 18.75%
```