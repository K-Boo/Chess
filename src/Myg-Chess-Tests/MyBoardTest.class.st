Class {
	#name : 'MyBoardTest',
	#superclass : 'TestCase',
	#instVars : [
		'board'
	],
	#category : 'Myg-Chess-Tests',
	#package : 'Myg-Chess-Tests'
}

{ #category : 'running' }
MyBoardTest >> setUp [
"initialise le plateau pour les tests"
    board := MyChessBoard empty.
    board initialize.
]

{ #category : 'tests' }
MyBoardTest >> testEmptyPieceIsSingleton [
    | e1 e2 |
    e1 := MyEmptyPiece new.
    e2 := MyEmptyPiece new.
    self assert: e1 == e2. "Toutes les cases vides partagent la même instance"
]

{ #category : 'tests' }
MyBoardTest >> testEmptyPiecePolymorphism [
"Vérifie que MyEmptyPiece se comporte comme une pièce polymorphique"
    | empty |
    empty := MyEmptyPiece new.

    self assert: empty isEmpty.
    self deny: empty isPiece.
    self assert: (empty moveTo: nil == empty).
    self deny: empty hasColor.
]

{ #category : 'tests' }
MyBoardTest >> testEmptySquareContainsEmptyPiece [
"une case vide contient une MyEmptyPiece"
    | square |
    square := board at: 'a3'.
    self assert: (square contents isKindOf: MyEmptyPiece).
]

{ #category : 'tests' }
MyBoardTest >> testHasPiece [
"Vérifie que hasPiece fonctionne correctement sans nil-check"
    | square piece |
    square := board at: 'a3'.
    self deny: square hasPiece.

    piece := MyPawn white.
    board at: 'a3' put: piece.
    self assert: square hasPiece.
]

{ #category : 'tests' }
MyBoardTest >> testMoveEmptyPieceDoesNothing [
"Vérifier que deplacer une MyEmptyPiece ne fait rien"
    | empty to |
    empty := (board at: 'a4') contents. 
    to := board at: 'a5'.
    empty moveTo: to.
    
    self assert: ((board at: 'a4') contents == empty). "La case reste vide"
    self assert: ((board at: 'a5') contents isKindOf: MyEmptyPiece). "La destination reste vide"
]

{ #category : 'tests' }
MyBoardTest >> testMovePieceToEmptySquare [
"Déplacer une pièce vers une case vide"
    | from to piece |
    from := board at: 'a2'.
    to := board at: 'a3'.
    piece := MyPawn white.
    board at: 'a2' put: piece.
    piece moveTo: to.
    self assert: (from contents isKindOf: MyEmptyPiece). "Case d'origine devient vide"
    self assert: (to contents == piece).                  "Case de destination contient la pièce"

]
