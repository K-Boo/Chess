Class {
	#name : 'MyEmptyPiece',
	#superclass : 'MyPiece',
	#category : 'Myg-Chess-Core',
	#package : 'Myg-Chess-Core'
}

{ #category : 'accessing' }
MyEmptyPiece >> color [
    ^ nil
]

{ #category : 'testing' }
MyEmptyPiece >> isPiece [
    ^ false
]

{ #category : 'path commands' }
MyEmptyPiece >> moveTo: aSquare [
    ^ self

]
