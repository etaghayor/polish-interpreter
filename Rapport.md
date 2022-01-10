# Polish Analyser

### Identifiants
Taghayor, Fatemeh, @taghayor, 22015541 </br>
Bacha,    Rayane,  @bacha,    22018752

## Fonctionnalités
Le projet est capable de lire un fichier Polish et de transformer son
programme en une représentation OCaml, cela nous permet de :
    
    - évaluer le programme polish et donner les résultats calculés
    - appliquer un calcul statique et retourner les variables pouvant etre accedées avant leur initialisation
    - analyser statiquement le fichier : c'est a dire de faire une interpretaiton asbtraite
    - transformer la représentation OCaml en représentation polish pour pouvoir la réafficher

Le projet a été divisé en deux parties :

### Premier Rendu
Dans cette premiere partie, on a ecrit 3 fichiers: 

    - Reader.ml : lis le fichier Polish ligne par ligne et transforme celui-ci en un bloc d'instructions.
    - Printer.ml : reécrit une syntaxe polish concrète à partir d'un bloc d'instructions.
    - Evaluator.ml : évalue un bloc d'instructions et renvoie les résultats trouvés.

Dans ces trois fichiers, les opérations, expressions, comparateurs, conditions, instructions, 
et blocs sont gérés dans des fonctions différentes.

A noter que les fonctions marchent aussi sans passer par la lecture d'un fichier.p, par exemple :
read_op Add créera un "type op" Add; </br>
`` utop # print_expr Op(Add, Num 1, Num 3)`` donnera "+ 1 3"; </br>
`` utop # eval_expr Op(Add, Num 1, Num 3)`` donnera 4.

Quant à l'évolution au cours du temps, le reader a pris le plus de temps, et les reader et evaluator ont suivi assez vite.

### Deuxieme Partie
Dans cette dexieme partie, on a ecrit 3 fichiers aussi : 

    - Simplifier.ml : Simplifie un programme en effectuant une propagation des constantes et en éliminant les blocs “morts”.
    - Analyser.ml : calcule statiquement les variables risquant d’être accédées avant d’être écrites et affiche les eventuelles erreurs.
    - Signer.ml : analyse statiquement les signes possibles des variables lors du déroulement du programme, et détermine s'il y a un  risque de division par zéro.

Ici aussi, le code est modularisé afin de mieux comprendre et interpréter la logique;
les fonctions agissent aussi bien sur un bloc, que sur une expression ou une condition...
Par exemple : simpl_expr Op(Add, Num 0, Var x) donnera x

NB :: Pour l'analyse statique, nous nous interessons aux signes des variables et non pas leurs valeurs 
Par exemple, dans le code suivant :
    n := -5
    WHILE n < 0
        n := + n 1
la valeur effective finale de n sera forcément 0.
Mais vu que n passe aussi par des valeurs négatives, ses signes seront - et Zero et non pas Zero uniquement

## Extensions
De petites extensions sont présentes afin de donner plus d'options à l'utilisateur :
    
    -copy copie le contenu d'un fichier polish dans un fichier dest
    -simpCopy simplifie un fichier polish puis place le nouveau contenu dans un fichier dest

## Compilation et Exécution
Vous aurez besoin de la version 4.13.1 d'Ocaml pour l'execution du programme car nous utilisons des fonctions 
de la bibliotheque Str uniquement présentes dans cette version.
Nous nous sommes aussi appuyés des bibliotheques String et Printer.

La compilation se fait à l'aide de la commande "make" à la racine du dépot, et l'execution 
est de la forme 
    "./run -eval $path/fichier_polish.p" pour l'evaluation du fichier polish 
    "./run -reprint $path/fichier_polish.p" pour la reécriture
    "./run -simpl $path/fichier_polish.p" pour la simplification
    "./run -vars $path/fichier_polish.p" pour le calcul statique
    "./run -sign $path/fichier_polish.p" pour l'analyse statique
    "./run -copy $path/fichier_polish.p $dest" pour la generation d'une copie
    "./run -simpCopy $path/fichier_polish.p $dest" pour la generation d'une copie simplifiée 
