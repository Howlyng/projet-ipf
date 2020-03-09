# projet-ipf
Projet de camel du module d'IPF.

# Technologie utilisée
Caml.

# Sujet du projet

Le but de ce projet est d’implanter un protocole de communication avec les habitants des mondes lointains.

Plus précisément, il s’agira de prendre un message entrée et de donner la suite d’instructions permettant de piloter une (ou plusieurs) antenne(s) afin d’envoyer ledit message.

Dans l’urgence de cette nouvelle, nous avons développé un système d’antennes rudimentaires composées chacune d’une roue comportant les 26 lettres de l’alphabet et du caractère espace. Nous pouvons donc voir l’ensemble des symboles utilisables comme un cycle. Le caractère suivant A est B, ..., le caractère suivant Y est Z, le caractère suivant Z est espace suivi de A. 

Toutes les antennes sont initialement positionnées sur le caractère espace.
Les commandes disponibles pour manipuler une antenne sont au nombre de 3 :
* "N" : passe au caractère suivant. ;
* "P" : revient au caractère précédent ;
* "E" : envoie la commande d’émission du caractère courrant.

Nous devons commencer rapidement à envoyer des messages afin d’ouvrir au plus vite la voie à nos ingénieurs financiers et autres analystes de données. Le projet sera donc découper en trois phases (une antenne, plusieurs antennes et compression du signal).

Durant toutes ces phases vos programme devront lire deux lignes sur leur entrée standard.

La première ligne comportera un entier n donnant le nombre d’antennes disponibles. Pour la première phase, cet entier sera toujours égal à 1. La deuxième ligne comportera le message lui-même.

Le programme affichera donc une suite de commandes permettant d’émettre le message.
