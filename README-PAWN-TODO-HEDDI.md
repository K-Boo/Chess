# Plan de travail – Fix pawn moves - HEDDI ABDELKADER !

Ce document readme liste les étapes que je vais suivre pour corriger et compléter le comportement des pions.

Je fais cela sur conseil du professeur et ainsi prioriser les différentes tâches du projet et travailler dessus une à une.

## Objectifs

- Déplacements vers l’avant (blancs montent, noirs descendent) uniquement sur case vide
- Double pas initial depuis la rangée de départ si chemin libre
- Captures diagonales uniquement sur pièce adverse
- Prise en passant (fenêtre d’un coup, case cible « en passant »)
- Promotion à l’arrivée en dernière rangée (UI/bot à définir)

## Étapes proposées

1. Clarifier le périmètre
   - Distinguer « cases attaquées » (diagonales) et « cases jouables » (devant)
   - Ranger les règles par couleur (offsets blanc/noir) pour éviter la duplication
2. Écrire les tests de base
   - Blanc: pas simple (2→3), double pas (2→4), blocages (pièce en 3), captures diagonales
   - Noir: pas simple (7→6), double pas (7→5), blocages (pièce en 6), captures diagonales
   - Interdiction de « capturer » tout droit
   - Critères d’acceptation:
     - Tous les cas ci-dessus passent pour blanc et noir
     - Aucun mouvement hors-plateau n’est proposé
3. Implémenter déplacements de base
   - Générer 1 pas avant si case vide
   - Générer 2 pas depuis rangée initiale si case intermédiaire et destination vides
   - Critères d’acceptation:
     - Si la case devant est occupée, aucun pas avant (ni simple ni double)
     - Le double pas n’est jamais proposé hors de la rangée initiale
4. Implémenter les captures diagonales
   - Ajouter uniquement les diagonales contenant une pièce adverse
   - Critères d’acceptation:
     - Jamais de capture diagonale si la diagonale est vide
     - Jamais de capture tout droit
5. Ajouter la prise en passant
   - Enregistrer la case « en passant » après un double pas adverse adjacent
   - Autoriser la capture en passant seulement au coup suivant, puis effacer l’état
   - Critères d’acceptation:
     - Cas positifs/négatifs couverts par tests d’intégration
6. Gérer la promotion
   - Détecter l’arrivée en rangée 8 (blanc) / 1 (noir)
   - Définir: UI (popup) et bot (choix par défaut, ex. reine)
7. Tester les cas limites
   - Bords/hors-plateau ignorés
   - Double pas interdit si la case juste devant est occupée
   - En passant invalide si fenêtre dépassée ou mauvaise colonne
8. Vérifier l’intégration
   - Historique/notation minimale pour en passant/promotion
   - Bot: coups générés conformes aux nouvelles règles
9. Refactoring et documentation
   - Factoriser les offsets et prédicats blanc/noir :
     - précision : ce que j'entends par la c'est d'éviter de dupliquer la logique entre pions blancs et noirs. On définit un “offset” de direction (par ex. +1 pour blancs, −1 pour noirs) et on calcule pas simple, double pas, et diagonales avec ce même offset. On utilise aussi des prédicats (conditions) partagés, par ex. “est sur rang de départ”, “case devant libre”, “case diagonale contient une pièce adverse”. Résultat: code plus court, lisible et moins sujet aux bugs.
   - Isoler la génération de coups de l’UI
   - Documenter les invariants (fenêtre en passant, rangées départ/arrivée)

## Checklist rapide

- [x] Tests déplacements simples (blanc/noir)
- [x] Tests double pas + blocages
- [x] Tests captures diagonales + impossibilité de capturer en avant
- [x] Tests bords/hors-plateau
- [x] Implémentation base → captures
- [x] Exécution des tests et corrections
- [x] Tests en passant (création, exécution, expiration)
- [x] Tests promotion (déclenchement, choix par défaut si bot)
- [x] Implémentation en passant → promotion
- [x] Vérifications UI/bot/notation
- [x] Refactoring + docs

---

## Décisions d’implémentation (résumé)

- Phase 1 (déplacements de base):

  - Séparation des responsabilités:
    - `attackingSquares` (pion): retourne uniquement les deux diagonales vers l’avant, sans conditions d’occupation (utile pour menaces/échec et sélection visuelle).
    - `targetSquaresLegal:` (pion): génère les destinations réellement jouables.
  - Règles implémentées dans `targetSquaresLegal:`:
    - Pas simple: avance d’une case uniquement si la case devant est vide.
    - Double pas initial: seulement depuis la rangée de départ (2 pour blanc, 7 pour noir) ET si la case devant et la case d’arrivée sont vides.
    - Captures diagonales: uniquement sur pièce adverse; jamais de capture en ligne droite.
  - Utilitaires:
    - `isOnStartingRank` (pion): détecte la rangée de départ.
    - `isPawn` (pion/pièce): évite les tests de type fragiles côté moteur.
  - Choix de conception:
    - Offsets “sens de marche” gérés via `isWhite` (up/down) pour éviter dupliquer la logique blanc/noir.
    - Pas d’UI/bot spécifique en Phase 1; pure logique de mouvement/test unitaire.

- En passant:
  - `MyChessGame` maintient `enPassantTargetSquare` (initialisé à `NO ENPASSANT`).
  - `move:` efface l’état précédent, crée l’état après un double pas, exécute et nettoie lors d’une prise en passant.
  - `MyPawn >> getEnPassantSquare` est robuste si `board game` est absent (tests unitaires).
- Promotion:
  - Détection côté `MyPawn` via `getPromotionSquare` (avance vers la dernière rangée si atteignable).
  - Choix UI/bot non interactif pour l’instant (détection uniquement). À enrichir si besoin d’une popup/stratégie bot.
- Pions (Phase 1):
  - `attackingSquares` (diagonales avant), `targetSquaresLegal:` (pas simple, double pas si rang de départ & chemin libre, captures diagonales, en passant, promotion).
  - Méthodes utilitaires: `isOnStartingRank`, `isPawn`.

---

## Approche méthodologique (TDD, Design Thinking, Reverse Engineering)

J'ai adopté une approche TDD stricte: pour chaque règle (pas simple, captures diagonales, double pas, en passant, promotion), j'ai écrit les tests en premier avec une documentation "context / trigger / assert-check" comme on l'a fait en classe avec le pair programming sur le katta rover. Les tests ont guidé l’implémentation minimale pour faire passer le rouge au vert, puis ont servi de filet de sécurité lors des ajustements (ordre d’effacement/création de l’état en passant, robustesse quand `board game` est absent, parenthésage des `and:/or:` en Smalltalk).

Côté Design Thinking, j'ai clarifié le problème avant d’implémenter: séparation des responsabilités (`attackingSquares` vs `targetSquaresLegal:`), identification des invariants (rang de départ, chemin libre, diagonales adverses, fenêtre d’un coup pour l’en passant), et priorisation par phases (simple d’abord, options ensuite) pour éviter la complexité prématurée tout en assurant une valeur incrémentale.

En Reverse Engineering, j'ai parcouru le code existant (plateau, cases, pièces, moteur, FEN) pour comprendre l’architecture, repérer les points d’extension (moteur de jeu pour l’en passant, pion pour la génération des coups), et aligner nos changements avec les conventions du projet (méthodes d’accès, style Bloc/Toplo, structure des tests). Cette lecture a permis de minimiser l’impact (ajouts ciblés, méthodes utilitaires, robustesse) et de conserver la cohérence globale.

---

### 1. Clarifier le périmètre (détails)

- Définitions précises (sans code):
  - Cases attaquées (attackingSquares): uniquement les deux diagonales vers l’avant (gauche/droite), indépendamment du fait qu’une pièce s’y trouve. Utilisé pour: vérifier échec, afficher cibles potentielles de capture.
  - Cases jouables (targetSquares / legalTargetSquares): ensemble des destinations réellement jouables selon les règles (pas simple si vide, double pas initial si chemin libre, captures diagonales seulement sur pièce adverse, jamais de capture en ligne droite).
- Offsets et orientation:
  - Blanc: direction +1 rang (monte). Noir: direction −1 rang (descend).
  - Diagonales: (±1 colonne, +1 rang pour blanc) / (±1 colonne, −1 rang pour noir).
- Rangées de départ / d’arrivée:
  - Blanc: départ = rang 2; arrivée (promotion) = rang 8. Noir: départ = rang 7; arrivée = rang 1.
- Règles d’occupation de cases:
  - Pas simple/double: interdit si la case juste devant est occupée; double interdit si la case intermédiaire est occupée.
  - Capture diagonale: uniquement si la diagonale contient une pièce adverse; diagonale vide = pas de capture.
- Hors-plateau et limites:
  - Toute case calculée en dehors de a..h ou 1..8 est ignorée (aucun mouvement/capture proposé).
- État transitoire (Phase 2):
  - En passant: nécessite une « case en passant » valable uniquement au coup suivant après un double pas adverse adjacent.
