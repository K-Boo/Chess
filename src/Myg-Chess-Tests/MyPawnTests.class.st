Class {
	#name : 'MyPawnTests',
	#superclass : 'TestCase',
	#category : 'Myg-Chess-Tests',
	#package : 'Myg-Chess-Tests'
}

{ #category : 'tests' }
MyPawnTests >> testBlackDiagonalCaptureOnlyOnOpponent [
	 
	 "Dans ce test, je vérifie que la capture diagonale fonctionne uniquement sur une pièce adverse. meme logique que testWhiteDiagonalCaptureOnlyOnOpponent"
			
	 "context: pion noir en e7; une tour blanche est en d6 (diagonale avant-gauche pour noir), une tour noire est en f6 (diagonale avant-droite pour noir)."
    | board pawn |
    board := MyChessBoard empty.
    board at: 'e7' put: (pawn := MyPawn black).
    board at: 'd6' put: MyRook white.
    board at: 'f6' put: MyRook black.

    "trigger: on calcule les cases légalement jouables."
    "assert-check: d6 doit être proposée (capture adverse); f6 ne doit pas l’être (pièce alliée). C’est l’équivalent côté noir, avec la direction inversée."
    self assert: (pawn legalTargetSquares includes: (board at: 'd6')).
    self deny: (pawn legalTargetSquares includes: (board at: 'f6'))

]

{ #category : 'tests' }
MyPawnTests >> testBlackDoubleStepBlockedByIntermediateSquare [ 
    "context: pion noir en e7; e6 est occupée; e5 est vide"
    | board pawn |
    board := MyChessBoard empty.
    board at: 'e7' put: (pawn := MyPawn black).
    board at: 'e6' put: MyRook white.
    
    "trigger: calcul des cases légales du pion"
    "assert-check: e5 ∉ legalTargetSquares (intermédiaire occupée)"
    self deny: (pawn legalTargetSquares includes: (board at: 'e5'))

]

{ #category : 'tests' }
MyPawnTests >> testBlackDoubleStepFromStartingRank [ 
    "context: pion noir en e7 (rang de départ); e6 et e5 sont vides"
    | board pawn |
    board := MyChessBoard empty.
    board at: 'e7' put: (pawn := MyPawn black).
    
    "trigger: calcul des cases légales du pion"
    "assert-check: e5 ∈ legalTargetSquares (double pas autorisé depuis rang de départ)"
    self assert: (pawn legalTargetSquares includes: (board at: 'e5'))

]

{ #category : 'tests' }
MyPawnTests >> testBlackDoubleStepNotAllowedOutsideStartingRank [ 
    "context: pion noir en e6 (hors rang de départ 7); e5 et e4 sont vides"
    | board pawn |
    board := MyChessBoard empty.
    board at: 'e6' put: (pawn := MyPawn black).
    
    "trigger: calcul des cases légales du pion"
    "assert-check: e4 ∉ legalTargetSquares (double pas interdit hors rang de départ)"
    self deny: (pawn legalTargetSquares includes: (board at: 'e4'))

]

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

{ #category : 'tests' }
MyPawnTests >> testEnPassantStateCreatedAfterDoubleStep [ 
	"context: pion blanc fait e2→e4; pion noir adjacent en f4"
	| board whitePawn blackPawn game |
	board := MyChessBoard empty.
	game := MyChessGame freshGame.
	board game: game.
	board at: 'e2' put: (whitePawn := MyPawn white).
	board at: 'f4' put: (blackPawn := MyPawn black).
	
	"trigger: pion blanc fait double pas e2→e4"
	game move: whitePawn to: (board at: 'e4').
	
	"assert-check: état en passant créé (case e3 cible)"
	self assert: game enPassantTargetSquare equals: 'e3'

]

{ #category : 'tests' }
MyPawnTests >> testWhiteBlockedForward [ 
	 
	 "Dans ce test, je place un pion blanc en e2 et une pièce sur e3, puis vérifie que e3 n’est pas légal → si la case devant est occupée, on ne peut pas avancer."
		
    "context: un pion blanc est en e2, et une pièce (peu importe la couleur pour ce test) occupe e3."
    | board pawn |
    board := MyChessBoard empty.
    board at: 'e2' put: (pawn := MyPawn white).
    board at: 'e3' put: MyRook black.

    "trigger: on calcule les cases légalement jouables."
    "assert-check: e3 ne doit pas être proposé. Un pion ne peut pas avancer si la case immédiatement devant est occupée."
    self deny: (pawn legalTargetSquares includes: (board at: 'e3'))
]

{ #category : 'tests' }
MyPawnTests >> testWhiteDiagonalCaptureOnlyOnOpponent [
		
	"Dans ce test, je vérifie que la capture diagonale fonctionne uniquement sur une pièce adverse.	"
	
	 "context: ppion blanc en e2; une tour noire est en f3 (diagonale avant-droite), une tour blanche est en d3 (diagonale avant-gauche)"
    | board pawn |
    board := MyChessBoard empty.
    board at: 'e2' put: (pawn := MyPawn white).
    board at: 'f3' put: MyRook black.
    board at: 'd3' put: MyRook white.

    "trigger: on calcule les cases légalement jouables"
    "assert-check: f3 doit être proposée (capture d’une pièce adverse); d3 ne doit pas l’être (on ne capture pas une pièce alliée)."
    self assert: (pawn legalTargetSquares includes: (board at: 'f3')).
    self deny: (pawn legalTargetSquares includes: (board at: 'd3'))
]

{ #category : 'tests' }
MyPawnTests >> testWhiteDoubleStepBlockedByIntermediateSquare [ 
    "context: pion blanc en e2; e3 est occupée; e4 est vide"
    | board pawn |
    board := MyChessBoard empty.
    board at: 'e2' put: (pawn := MyPawn white).
    board at: 'e3' put: MyRook black.
    
    "trigger: calcul des cases légales du pion"
    "assert-check: e4 ∉ legalTargetSquares (intermédiaire occupée)"
    self deny: (pawn legalTargetSquares includes: (board at: 'e4'))

]

{ #category : 'tests' }
MyPawnTests >> testWhiteDoubleStepFromStartingRank [ 
    "context: pion blanc en e2 (rang de départ); e3 et e4 sont vides"
    | board pawn |
    board := MyChessBoard empty.
    board at: 'e2' put: (pawn := MyPawn white).
    
    "trigger: calcul des cases légales du pion"
    "assert-check: e4 ∈ legalTargetSquares (double pas autorisé depuis rang de départ)"
    self assert: (pawn legalTargetSquares includes: (board at: 'e4'))

]

{ #category : 'tests' }
MyPawnTests >> testWhiteDoubleStepNotAllowedOutsideStartingRank [ 
    "context: pion blanc en e3 (hors rang de départ 2); e4 et e5 sont vides"
    | board pawn |
    board := MyChessBoard empty.
    board at: 'e3' put: (pawn := MyPawn white).
    
    "trigger: calcul des cases légales du pion"
    "assert-check: e5 ∉ legalTargetSquares (double pas interdit hors rang de départ)"
    self deny: (pawn legalTargetSquares includes: (board at: 'e5'))

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
