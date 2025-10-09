Class {
	#name : 'MyPawnTests',
	#superclass : 'TestCase',
	#category : 'Myg-Chess-Tests',
	#package : 'Myg-Chess-Tests'
}

{ #category : 'tests' }
MyPawnTests >> testBlackSingleStepForward [
	 
	 "Dans ce test, je place un pion noir en e7 et vérifie que e6 est légal → même logique côté noir (direction inverse)."
		
	 "context: un pion noir est posé en e7, la case e6 est libre."
    | board pawn |
    board := MyChessBoard empty.
    board at: 'e7' put: (pawn := MyPawn black).

    "trigger: on calcule ses cases légalement jouables."
    "assert-check: e6 doit être proposé. Même règle que le pion blanc, mais vers le bas (direction inverse)."
    self assert: (pawn legalTargetSquares includes: (board at: 'e6'))
]

{ #category : ' tests' }
MyPawnTests >> testWhiteSingleStepForward [ 
	
	 "Dans ce test, je place un pion blanc en e2 et vérifie que e3 est dans ses cases légales → valide le pas simple vers l’avant si la case est vide."

    "context: un pion blanc est posé en e2, la case e3 est libre"
    | board pawn |
    board := MyChessBoard empty.
    board at: 'e2' put: (pawn := MyPawn white).

    "trigger: on demande les cases légalement jouables du pion"
    "assert-check: e3 doit apparaître dans ces cases. Cela valide le pas simple en avant pour un pion blanc quand la case devant est vide"
    self assert: (pawn legalTargetSquares includes: (board at: 'e3'))

]
