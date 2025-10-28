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
	"Calcule les cases légales pour un pion: pas simple, double pas initial, captures diagonales"
	| forwardSquare doubleStepSquare captureSquares result |
	
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
	
	"Assemblage du résultat"
	result := OrderedCollection new.
	(forwardSquare notNil and: [ forwardSquare hasPiece not ]) ifTrue: [ result add: forwardSquare ].
	(doubleStepSquare notNil and: [ doubleStepSquare hasPiece not ]) ifTrue: [ result add: doubleStepSquare ].
	result addAll: captureSquares.
	
	^ result

]
