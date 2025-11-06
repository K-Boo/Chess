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

{ #category : 'pawnRendering' }
MyPieceRenderingTest >> testRenderBlackPawnOnBlackSquare [ 
	"context: un pion noir est créé et une case noire est créée"
	| square pawn |
	square := MyChessSquare black.
	pawn := MyPawn black.
	
	"trigger: on demande le rendu du pion noir sur la case noire"
	"assert-check: le caractère retourné doit être 'o' (pion noir sur case noire)"
	self assert: (pawn renderPieceOn: square) equals: 'o'.
]

{ #category : 'pawnRendering' }
MyPieceRenderingTest >> testRenderBlackPawnOnWhiteSquare [ 
	"context: un pion noir est créé et une case blanche est créée"
	| square pawn |
	square := MyChessSquare white.
	pawn := MyPawn black.
	
	"trigger: on demande le rendu du pion noir sur la case blanche"
	"assert-check: le caractère retourné doit être 'O' (pion noir sur case blanche)"
	self assert: (pawn renderPieceOn: square) equals: 'O'.
]

{ #category : 'queenRendering' }
MyPieceRenderingTest >> testRenderBlackQueenOnBlackSquare [ 
	"context: une reine noire est créée et une case noire est créée"
	| square queen |
	square := MyChessSquare black.
	queen := MyQueen black.
	
	"trigger: on demande le rendu de la reine noire sur la case noire"
	"assert-check: le caractère retourné doit être 'w' (reine noire sur case noire)"
	self assert: (queen renderPieceOn: square) equals: 'w'.
]

{ #category : 'queenRendering' }
MyPieceRenderingTest >> testRenderBlackQueenOnWhiteSquare [ 
	"context: une reine noire est créée et une case blanche est créée"
	| square queen |
	square := MyChessSquare white.
	queen := MyQueen black.
	
	"trigger: on demande le rendu de la reine noire sur la case blanche"
	"assert-check: le caractère retourné doit être 'W' (reine noire sur case blanche)"
	self assert: (queen renderPieceOn: square) equals: 'W'.
]

{ #category : 'rookRendering' }
MyPieceRenderingTest >> testRenderBlackRookOnBlackSquare [ 
	"context: une tour noire est créée et une case noire est créée"
	| square rook |
	square := MyChessSquare black.
	rook := MyRook black.
	
	"trigger: on demande le rendu de la tour noire sur la case noire"
	"assert-check: le caractère retourné doit être 't' (tour noire sur case noire)"
	self assert: (rook renderPieceOn: square) equals: 't'.
]

{ #category : 'rookRendering' }
MyPieceRenderingTest >> testRenderBlackRookOnWhiteSquare [ 
	"context: une tour noire est créée et une case blanche est créée"
	| square rook |
	square := MyChessSquare white.
	rook := MyRook black.
	
	"trigger: on demande le rendu de la tour noire sur la case blanche"
	"assert-check: le caractère retourné doit être 'T' (tour noire sur case blanche)"
	self assert: (rook renderPieceOn: square) equals: 'T'.
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

{ #category : 'pawnRendering' }
MyPieceRenderingTest >> testRenderWhitePawnOnBlackSquare [ 
	"context: un pion blanc est créé et une case noire est créée"
	| square pawn |
	square := MyChessSquare black.
	pawn := MyPawn white.
	
	"trigger: on demande le rendu du pion blanc sur la case noire"
	"assert-check: le caractère retourné doit être 'p' (pion blanc sur case noire)"
	self assert: (pawn renderPieceOn: square) equals: 'p'.
]

{ #category : 'pawnRendering' }
MyPieceRenderingTest >> testRenderWhitePawnOnWhiteSquare [ 
	"context: un pion blanc est créé et une case blanche est créée"
	| square pawn |
	square := MyChessSquare white.
	pawn := MyPawn white.
	
	"trigger: on demande le rendu du pion blanc sur la case blanche"
	"assert-check: le caractère retourné doit être 'P' (pion blanc sur case blanche)"
	self assert: (pawn renderPieceOn: square) equals: 'P'.
]

{ #category : 'queenRendering' }
MyPieceRenderingTest >> testRenderWhiteQueenOnBlackSquare [ 
	"context: une reine blanche est créée et une case noire est créée"
	| square queen |
	square := MyChessSquare black.
	queen := MyQueen white.
	
	"trigger: on demande le rendu de la reine blanche sur la case noire"
	"assert-check: le caractère retourné doit être 'q' (reine blanche sur case noire)"
	self assert: (queen renderPieceOn: square) equals: 'q'.
]

{ #category : 'queenRendering' }
MyPieceRenderingTest >> testRenderWhiteQueenOnWhiteSquare [ 
	"context: une reine blanche est créée et une case blanche est créée"
	| square queen |
	square := MyChessSquare white.
	queen := MyQueen white.
	
	"trigger: on demande le rendu de la reine blanche sur la case blanche"
	"assert-check: le caractère retourné doit être 'Q' (reine blanche sur case blanche)"
	self assert: (queen renderPieceOn: square) equals: 'Q'.
]

{ #category : 'rookRendering' }
MyPieceRenderingTest >> testRenderWhiteRookOnBlackSquare [ 
	"context: une tour blanche est créée et une case noire est créée"
	| square rook |
	square := MyChessSquare black.
	rook := MyRook white.
	
	"trigger: on demande le rendu de la tour blanche sur la case noire"
	"assert-check: le caractère retourné doit être 'r' (tour blanche sur case noire)"
	self assert: (rook renderPieceOn: square) equals: 'r'.
]

{ #category : 'rookRendering' }
MyPieceRenderingTest >> testRenderWhiteRookOnWhiteSquare [ 
	"context: une tour blanche est créée et une case blanche est créée"
	| square rook |
	square := MyChessSquare white.
	rook := MyRook white.
	
	"trigger: on demande le rendu de la tour blanche sur la case blanche"
	"assert-check: le caractère retourné doit être 'R' (tour blanche sur case blanche)"
	self assert: (rook renderPieceOn: square) equals: 'R'.
]
