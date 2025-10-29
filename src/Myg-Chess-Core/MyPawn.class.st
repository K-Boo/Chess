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
	| gameObject enPassantTarget |
	"Sécuriser l'accès au jeu: certains tests n'attachent pas de game au board"
	gameObject := self board ifNotNil: #game.
	gameObject isNil ifTrue: [ ^ nil ].
	enPassantTarget := gameObject enPassantTargetSquare ifNil: [ 'NO ENPASSANT' ].
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

{ #category : 'testing' }
MyPawn >> isOnStartingRank [ 
	"Vérifie si le pion est sur sa rangée de départ"
	^ self isWhite
		ifTrue: [ square file = $2 ]
		ifFalse: [ square file = $7 ]

]

{ #category : 'rendering' }
MyPawn >> renderPieceOn: aSquare [ 
	
	^ aSquare renderPawn:  self
]

{ #category : 'rendering' }
MyPawn >> targetSquaresLegal: aBoolean [ 
    "Calcule les cases légales pour un pion: pas simple, double pas initial, captures diagonales, en passant, promotion"
    | forwardSquare doubleStepSquare captureSquares enPassantSquare promotionSquare result |
	
	"Pas simple vers l'avant"
	forwardSquare := self isWhite
		ifTrue: [ square up ]
		ifFalse: [ square down ].
	
	"Double pas initial (seulement depuis rang de départ)"
	doubleStepSquare := (self isOnStartingRank and: [ forwardSquare notNil and: [ forwardSquare hasPiece not ] ])
		ifTrue: [ self isWhite
			ifTrue: [ forwardSquare up ]
			ifFalse: [ forwardSquare down ] ]
		ifFalse: [ nil ].
	
    "Captures diagonales (uniquement sur pièce adverse)"
	captureSquares := (self attackingSquares) select: [ :s |
		s hasPiece and: [ s contents color ~= color ] ].
    
    "En passant et promotion"
    enPassantSquare := self getEnPassantSquare.
	 promotionSquare := self getPromotionSquare.
	
	"Assemblage du résultat"
	result := OrderedCollection new.
	(forwardSquare notNil and: [ forwardSquare hasPiece not ]) ifTrue: [ result add: forwardSquare ].
	(doubleStepSquare notNil and: [ doubleStepSquare hasPiece not ]) ifTrue: [ result add: doubleStepSquare ].
    result addAll: captureSquares.
    enPassantSquare ifNotNil: [ result add: enPassantSquare ].
    promotionSquare ifNotNil: [ result add: promotionSquare ].
	
	^ result




]
