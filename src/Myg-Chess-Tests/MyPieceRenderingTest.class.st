Class {
	#name : 'MyPieceRenderingTest',
	#superclass : 'TestCase',
	#category : 'Myg-Chess-Tests',
	#package : 'Myg-Chess-Tests'
}

{ #category : 'bishopRendering' }
MyPieceRenderingTest >> testRenderBlackBishopOnBlackSquare [ 
	"context: un fou noir est créé et une case noire est créée"
	| square bishop |
	square := MyChessSquare black.
	bishop := MyBishop black.
	
	"trigger: on demande le rendu du fou noir sur la case noire"
	"assert-check: le caractère retourné doit être 'v' (fou noir sur case noire)"
	self assert: (bishop renderPieceOn: square) equals: 'v'.

]

{ #category : 'bishopRendering' }
MyPieceRenderingTest >> testRenderBlackBishopOnWhiteSquare [ 
	"context: un fou noir est créé et une case blanche est créée"
	| square bishop |
	square := MyChessSquare white.
	bishop := MyBishop black.
	
	"trigger: on demande le rendu du fou noir sur la case blanche"
	"assert-check: le caractère retourné doit être 'V' (fou noir sur case blanche)"
	self assert: (bishop renderPieceOn: square) equals: 'V'.

]

{ #category : 'kingRendering' }
MyPieceRenderingTest >> testRenderBlackKingOnBlackSquare [ 
	"context: un roi noir est créé et une case noire est créée"
	| square king |
	square := MyChessSquare black.
	king := MyKing black.
	
	"trigger: on demande le rendu du roi noir sur la case noire"
	"assert-check: le caractère retourné doit être 'l' (roi noir sur case noire)"
	self assert: (king renderPieceOn: square) equals: 'l'.
]

{ #category : 'kingRendering' }
MyPieceRenderingTest >> testRenderBlackKingOnWhiteSquare [ 
	"context: un roi noir est créé et une case blanche est créée"
	| square king |
	square := MyChessSquare white.
	king := MyKing black.
	
	"trigger: on demande le rendu du roi noir sur la case blanche"
	"assert-check: le caractère retourné doit être 'L' (roi noir sur case blanche)"
	self assert: (king renderPieceOn: square) equals: 'L'.
]

{ #category : 'knightRendering' }
MyPieceRenderingTest >> testRenderBlackKnightOnBlackSquare [ 
	"context: un cavalier noir est créé et une case noire est créée"
	| square knight |
	square := MyChessSquare black.
	knight := MyKnight black.
	
	"trigger: on demande le rendu du cavalier noir sur la case noire"
	"assert-check: le caractère retourné doit être 'm' (cavalier noir sur case noire)"
	self assert: (knight renderPieceOn: square) equals: 'm'.
]

{ #category : 'knightRendering' }
MyPieceRenderingTest >> testRenderBlackKnightOnWhiteSquare [ 
	"context: un cavalier noir est créé et une case blanche est créée"
	| square knight |
	square := MyChessSquare white.
	knight := MyKnight black.
	
	"trigger: on demande le rendu du cavalier noir sur la case blanche"
	"assert-check: le caractère retourné doit être 'M' (cavalier noir sur case blanche)"
	self assert: (knight renderPieceOn: square) equals: 'M'.
]

{ #category : 'bishopRendering' }
MyPieceRenderingTest >> testRenderWhiteBishopOnBlackSquare [ 
	"context: un fou blanc est créé et une case noire est créée"
	| square bishop |
	square := MyChessSquare black.
	bishop := MyBishop white.
	
	"trigger: on demande le rendu du fou blanc sur la case noire"
	"assert-check: le caractère retourné doit être 'b' (fou blanc sur case noire)"
	self assert: (bishop renderPieceOn: square) equals: 'b'.

]

{ #category : 'bishopRendering' }
MyPieceRenderingTest >> testRenderWhiteBishopOnWhiteSquare [ 
	"context: un fou blanc est créé et une case blanche est créée"
	| square bishop |
	square := MyChessSquare white.
	bishop := MyBishop white.
	
	"trigger: on demande le rendu du fou blanc sur la case blanche"
	"assert-check: le caractère retourné doit être 'B' (fou blanc sur case blanche)"
	self assert: (bishop renderPieceOn: square) equals: 'B'.

]

{ #category : 'kingRendering' }
MyPieceRenderingTest >> testRenderWhiteKingOnBlackSquare [ 
	"context: un roi blanc est créé et une case noire est créée"
	| square king |
	square := MyChessSquare black.
	king := MyKing white.
	
	"trigger: on demande le rendu du roi blanc sur la case noire"
	"assert-check: le caractère retourné doit être 'k' (roi blanc sur case noire)"
	self assert: (king renderPieceOn: square) equals: 'k'.
]

{ #category : 'kingRendering' }
MyPieceRenderingTest >> testRenderWhiteKingOnWhiteSquare [ 
	"context: un roi blanc est créé et une case blanche est créée"
	| square king |
	square := MyChessSquare white.
	king := MyKing white.
	
	"trigger: on demande le rendu du roi blanc sur la case blanche"
	"assert-check: le caractère retourné doit être 'K' (roi blanc sur case blanche)"
	self assert: (king renderPieceOn: square) equals: 'K'.

]

{ #category : 'knightRendering' }
MyPieceRenderingTest >> testRenderWhiteKnightOnBlackSquare [ 
	"context: un cavalier blanc est créé et une case noire est créée"
	| square knight |
	square := MyChessSquare black.
	knight := MyKnight white.
	
	"trigger: on demande le rendu du cavalier blanc sur la case noire"
	"assert-check: le caractère retourné doit être 'n' (cavalier blanc sur case noire)"
	self assert: (knight renderPieceOn: square) equals: 'n'.
]

{ #category : 'knightRendering' }
MyPieceRenderingTest >> testRenderWhiteKnightOnWhiteSquare [ 
	"context: un cavalier blanc est créé et une case blanche est créée"
	| square knight |
	square := MyChessSquare white.
	knight := MyKnight white.
	
	"trigger: on demande le rendu du cavalier blanc sur la case blanche"
	"assert-check: le caractère retourné doit être 'N' (cavalier blanc sur case blanche)"
	self assert: (knight renderPieceOn: square) equals: 'N'.


]
