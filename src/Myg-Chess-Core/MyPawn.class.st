Class {
	#name : 'MyPawn',
	#superclass : 'MyPiece',
	#category : 'Myg-Chess-Core',
	#package : 'Myg-Chess-Core'
}

{ #category : 'as yet unclassified' }
MyPawn >> attackingSquares [ 
	"Les pions attaquent uniquement en diagonale vers l'avant"
	^ (self isWhite
		ifTrue: [ { square up ifNotNil: #left. square up ifNotNil: #right } ]
		ifFalse: [ { square down ifNotNil: #left. square down ifNotNil: #right } ])
			select: [ :s | s notNil ]

]

{ #category : 'as yet unclassified' }
MyPawn >> getEnPassantSquare [ 
	"Retourne la case en passant si elle est disponible"
	| boardObject gameObject enPassantTarget |
	"Sécuriser l'accès au jeu: certains tests n'attachent pas de game au board"
	boardObject := self board.
	boardObject isNil ifTrue: [ ^ nil ].
	gameObject := boardObject game.
	gameObject isNil ifTrue: [ ^ nil ].
	enPassantTarget := gameObject enPassantTargetSquare.
	(enPassantTarget isNil or: [ enPassantTarget = 'NO ENPASSANT' ]) ifTrue: [ ^ nil ].
	(enPassantTarget = 'NO ENPASSANT') ifTrue: [ ^ nil ].
	
	"Vérifier si cette case est adjacente et en diagonale"
	^ self attackingSquares detect: [ :s | s name = enPassantTarget ] ifNone: [ nil ]



]

{ #category : 'as yet unclassified' }
MyPawn >> getPromotionSquare [ 
	"Retourne la case de promotion si le pion peut y arriver"
	| promotionRank nextSquare |
	promotionRank := self isWhite ifTrue: [ $8 ] ifFalse: [ $1 ].
	
	"Vérifier si on peut avancer vers la dernière rangée"
	nextSquare := self isWhite
		ifTrue: [ square up ]
		ifFalse: [ square down ].

	nextSquare isNil ifTrue: [ ^ nil ].
	^ (nextSquare file = promotionRank)
		ifTrue: [ nextSquare ]
		ifFalse: [ nil ]

]

{ #category : 'accessing' }
MyPawn >> id [

	^ 'P'
]

{ #category : 'rendering' }
MyPawn >> renderBlackPieceOn: aSquare [ 
	^ aSquare renderBlackPawn.
]

{ #category : 'rendering' }
MyPawn >> renderWhitePieceOn: aSquare [ 
	^ aSquare renderWhitePawn.
]

{ #category : 'rendering' }
MyPawn >> targetSquaresLegal: aBoolean [

	^ (self isWhite
		   ifTrue: [ { square up } ]
		   ifFalse: [ { square down } ]) select: [ :s |
		  s notNil and: [
			  s hasPiece not or: [ s contents color ~= color ] ] ]
]
