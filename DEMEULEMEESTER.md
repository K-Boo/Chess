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

Tests sur les cases vides

Vérification que les cases initialement vides contiennent MyEmptyPiece au lieu de nil.

Vérification du polymorphisme de MyEmptyPiece : isEmpty → vrai, isPiece → faux, moveTo: renvoie self, aucune couleur.

Test de déplacement d’un EmptyPiece : ne modifie ni la case de départ ni la case cible.

Tests sur les pièces normales

Vérification de la présence d’une pièce après mouvement dans une case vide.

Déplacement d’une pièce vers une case vide : la case d’origine devient un EmptyPiece et la case cible contient la pièce.

Vérifie que toutes les instances de MyEmptyPiece sont identiques, garantissant l’unicité.


# Etape 2 : Création de MyEmptyPiece
On crée MyEmptyPiece qui hérite de myPiece. On redefini un certain nombre de de méthodes selon les propriétés d'une case vide. Un objet MyEmptyPiece : n'est pas une piece, n'a pas de couleur, ne bouge pas. On redéfini les méthodes moveTo: (EmptyPiece ne bouge pas) et renderPieceOn: (EmptyPiece ne renvoie rien) ainsi que les méthode sur les couleurs (hasColor, color) et états (isPiece, isEmpty).

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
