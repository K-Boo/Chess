Class {
	#name : 'MyPieceRenderingTest',
	#superclass : 'TestCase',
	#category : 'Myg-Chess-Tests',
	#package : 'Myg-Chess-Tests'
}

{ #category : 'tests' }
MyPieceRenderingTest >> testRenderBlackKnightOnBlackSquare [ 
	"context: un cavalier noir est créé et une case noire est créée"
	| square knight |
	square := MyChessSquare black.
	knight := MyKnight black.
	
	"trigger: on demande le rendu du cavalier noir sur la case noire"
	"assert-check: le caractère retourné doit être 'm' (cavalier noir sur case noire)"
	self assert: (knight renderPieceOn: square) equals: 'm'.
]

{ #category : 'tests' }
MyPieceRenderingTest >> testRenderBlackKnightOnWhiteSquare [ 
	"context: un cavalier noir est créé et une case blanche est créée"
	| square knight |
	square := MyChessSquare white.
	knight := MyKnight black.
	
	"trigger: on demande le rendu du cavalier noir sur la case blanche"
	"assert-check: le caractère retourné doit être 'M' (cavalier noir sur case blanche)"
	self assert: (knight renderPieceOn: square) equals: 'M'.
]

{ #category : 'tests' }
MyPieceRenderingTest >> testRenderWhiteKnightOnBlackSquare [ 
	"context: un cavalier blanc est créé et une case noire est créée"
	| square knight |
	square := MyChessSquare black.
	knight := MyKnight white.
	
	"trigger: on demande le rendu du cavalier blanc sur la case noire"
	"assert-check: le caractère retourné doit être 'n' (cavalier blanc sur case noire)"
	self assert: (knight renderPieceOn: square) equals: 'n'.
]

{ #category : 'tests' }
MyPieceRenderingTest >> testRenderWhiteKnightOnWhiteSquare [ 
	"context: un cavalier blanc est créé et une case blanche est créée"
	| square knight |
	square := MyChessSquare white.
	knight := MyKnight white.
	
	"trigger: on demande le rendu du cavalier blanc sur la case blanche"
	"assert-check: le caractère retourné doit être 'N' (cavalier blanc sur case blanche)"
	self assert: (knight renderPieceOn: square) equals: 'N'.


]
