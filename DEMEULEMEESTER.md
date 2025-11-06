## Suivi du kata Remove nil checks - Gautam Demeulemeester (grp13)

Le fait d'avoir déja joué aux echecs m'a permis de mieux aborder le parcours du code et de mieux comprendre d'ou je partais. 
Le kata "Remove nil check" vise le refactoring avec comme mission principale la suppression des ifNil/ifNotNil/nil. Voici les étapes que j'ai identifié pour ce kata:

  - Creer un objet emptyPiece héritant de piece qui representera une case vide.
  - Remplacer les nil par des objets emptyPiece
  - Remplacer les ifNil/ifNotNil par des appels polymorphes
  - Verifier/ajouter les tests
  - (étendre la notion de emptyPiece aux cases hors du plateau)

# Etape 1 : Création de tests
Dans cette étape, j'ai défini une suite de tests unitaires pour valider le comportement du plateau d’échecs et des cases vides (MyEmptyPiece) en suivant la démarche TDD :
```
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
 	| result empty |
    empty := MyEmptyPiece instance.

    self assert: empty isEmpty.
    self deny: empty isPiece.
	 result := empty moveTo: nil.
	 self assert: (result isKindOf: MyEmptyPiece).
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

{ #category : 'tests' }
MyBoardTest >> testMovePieceEmptiesOriginalSquare [
"Vérifie que lorsqu'on déplace un pion, sa case d'origine devient une MyEmptyPiece"
    | from to piece |
    from := board at: 'b2'.
    to := board at: 'b3'.
    
    piece := MyPawn white.
    board at: 'b2' put: piece.

    self assert: (from contents == piece).
    self deny: (from contents isKindOf: MyEmptyPiece).

    piece moveTo: to.

    self assert: (from contents isKindOf: MyEmptyPiece). "La case d'origine est vide"
    self assert: (to contents == piece).                  "La destination contient la pièce"
]
```

# Etape 2 : Création de MyEmptyPiece
On crée MyEmptyPiece qui hérite de myPiece. On redefini un certain nombre de de méthodes selon les propriétés d'une case vide. Un objet MyEmptyPiece : n'est pas une piece, n'a pas de couleur, ne bouge pas. On redéfini les méthodes moveTo: (EmptyPiece ne bouge pas) et renderPieceOn: (EmptyPiece ne renvoie rien) ainsi que les méthode sur les couleurs (hasColor, color) et états (isPiece, isEmpty).
```
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
MyEmptyPiece >> targetSquaresLegal: aBoolean [
    ^ OrderedCollection new
]
```
L'objet MyEmptyPiece utilisera le design pattern singleton. Ainsi toutes les cases vides partagent la même instance de MyEmptyPiece.

On déclare ensuite toutes les cases vides comme des MyEmptyPiece (nil->MyEmptyPiece instance) dans MyChessBoard >> initializesquares :
```
					          MyChessSquare basicNew
						          color: Color black;
						          board: self;
						          initialize;
						          contents: MyEmptyPiece instance;
						          yourself ]
				          ifFalse: [
					          black := true.
					          MyChessSquare basicNew
						          color: Color white;
						          board: self;
						          initialize;
						          contents: MyEmptyPiece instance;
						          yourself ].
```

# Etape 3 : Intégration des MyEmptyPiece dans le game
On change d'abord MyChessSquare >> emptyContents
```
emptyContents
	"self contents: nil"
	self contents: MyEmptyPiece instance
```
désormais une case laissé vide dans moveTo: deviendra une MyEmptyPiece.

On redefinit MyChessSquare >> hasPiece 
```
hasPiece
	"^ contents isNil not"
    ^ contents isPiece
```
hasPiece verifit maitenant si l'objet pointé est une pièce ou non (pièce classique ou piece vide).

On modifie enfin la méthode MyChessSquare >> contents:
```
contents: aPiece

	| text |
	contents := aPiece.

	text := contents renderPieceOn: self .

	piece text: (text asRopedText
			 fontSize: 48;
			 foreground: self foreground;
			 fontName: MyOpenChessDownloadedFont new familyName)
```
On appelle renderPieceOn dans tout les cas, si c'est une MyEmptyPiece le traitement original de la couleur se fera dans MyEmptyPiece>>renderPieceOn:
```
renderPieceOn: aSquare
    ^ aSquare color isBlack
        ifTrue: [ 'x' ]
        ifFalse: [ 'z' ]
```

On utilisera le MyEmptyPiece seulement pour les cases vides du plateau, on ne gère pas le hors plateau. On pourrait cependant utiliser le même principe de polymorphisme pour gérer ce cas et supprimer les derniers Nil Check.
