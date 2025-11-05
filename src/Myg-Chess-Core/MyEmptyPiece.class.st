Class {
	#name : 'MyEmptyPiece',
	#superclass : 'MyPiece',
	#instVars : [
		'INSTANCE'
	],
	#classInstVars : [
		'INSTANCE'
	],
	#category : 'Myg-Chess-Core',
	#package : 'Myg-Chess-Core'
}

{ #category : 'accessing' }
MyEmptyPiece class >> instance [
    "Retourne l'instance unique d'EmptyPiece"
    ^ INSTANCE ifNil: [ INSTANCE := super new ]
]

{ #category : 'instance creation' }
MyEmptyPiece class >> new [
    ^ self instance
]

{ #category : 'accessing' }
MyEmptyPiece >> color [
    ^ nil
]

{ #category : 'testing' }
MyEmptyPiece >> hasColor [
    ^ false
]

{ #category : 'testing' }
MyEmptyPiece >> isEmpty [
    ^ true
]

{ #category : 'testing' }
MyEmptyPiece >> isPiece [
    ^ false
]

{ #category : 'path commands' }
MyEmptyPiece >> moveTo: aSquare [
    ^ self

]

{ #category : 'rendering' }
MyEmptyPiece >> renderPieceOn: aSquare [
    "Ne fait rien pour les cases vides"
]

{ #category : 'rendering' }
MyEmptyPiece >> targetSquaresLegal: aBoolean [
    ^ OrderedCollection new
]
