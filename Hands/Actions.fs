namespace Cards

type Action = 
| AllIn
| MinRaise
| RaiseToAmount of int
| Call
| Check
| Fold