## Suivi du kata Remove nil checks - Gautam Demeulemeester (grp13)

Le fait d'avoir déja joué aux echecs m'a permis de mieux aborder le parcours du code et de mieux comprendre d'ou je partais. 
Le kata "Remove nil check" vise le refactoring avec comme mission principale la suppression des ifNil/ifNotNil/nil. Voici les étapes que j'ai identifié pour ce kata:

  - Creer un objet emptyPiece héritant de piece qui representera une case vide.
  - Remplacer les nil par des objets emptyPiece
  - Remplacer les ifNil/ifNotNil par des appels polymorphes
  - Verifier/ajouter les tests
  - (étendre la notion de emptyPiece aux cases hors du plateau)

# Etape 1 : Création de MyEmptyPiece
On crée MyEmptyPiece qui hérite de myPiece. On redefini un certain nombre de de méthodes selon les propriétés d'une case vide. Un objet MyEmptyPiece : n'est pas une piece, n'a pas de couleur, ne bouge pas. On redéfini ainsi les méthodes suivantes :
```
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
```
