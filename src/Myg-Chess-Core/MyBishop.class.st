Class {
	#name : 'MyBishop',
	#superclass : 'MyPiece',
	#category : 'Myg-Chess-Core',
	#package : 'Myg-Chess-Core'
}

{ #category : 'accessing' }
MyBishop >> id [
	
	^ 'B'
]

{ #category : 'rendering' }
MyBishop >> renderBlackPieceOn: aSquare [ 
	^ aSquare renderBlackBishop.

]

{ #category : 'rendering' }
MyBishop >> renderPieceOn: aSquare [ 
	^ self isWhite
		ifTrue: [ self renderWhitePieceOn: aSquare ]
		ifFalse: [ self renderBlackPieceOn: aSquare ].

]

{ #category : 'rendering' }
MyBishop >> renderWhitePieceOn: aSquare [ 
	^ aSquare renderWhiteBishop.

]

{ #category : 'rendering' }
MyBishop >> targetSquaresLegal: aBoolean [

	^ (self upRightDiagonalLegal: aBoolean)
	  , (self upLeftDiagonalLegal: aBoolean)
	  , (self downRightDiagonalLegal: aBoolean)
	  , (self downLeftDiagonalLegal: aBoolean)
]
