# Polish Analyser

### Identifiants
Taghayor, Fatemeh, @taghayor, 22015541 </br>
Bacha,    Rayane,  @bacha,    22018752

## Fonctionnalités
Le projet est capable de lire un fichier Polish et de transformer son
programme en une représentation OCaml, ce qui nous permet de pouvoir analyser 
un fichier Polish statiquement : c'est a dire faire une interpretaiton asbtraite.

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
Dans cette dexieme partie, on a ecrit 3 fichiers: 

    - Simplifier.ml : Simplifie un programme effectuant une propagation des constantes et l’élimination des blocs “morts”.
    - Analyser.ml : calcul statique des variables risquant d’être accédées avant d’être écrites.
    - Signer.ml : analyse statique du signe possible des variables lors du déroulement du programme, et application à la détermination du risque de division par zéro.



## Compilation et Exécution

Vous aurez besoin de la version 4.13.1 d'Ocaml pour l'execution du programme.

La compilation se fait à l'aide de la commande "make" à la racine du dépot, et l'execution 
est de la forme "./run -eval $path/fichier_polish.p" pour l'evaluation du fichier polish 
ou "./run -reprint $path/fichier_polish.p" pour la reécriture.
