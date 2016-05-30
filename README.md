# MonteCarlo
Monte Carlo et simulation: projets 2015-16
Nicolas Chopin
April 11, 2016

Queue d’attente
Une queue d’attente a les caractéristiques suivantes: le temps entre deux arrivées
de client suit une loi Exp(λ1), et le temps de service d’un client suit une loi
Exp(λ2). On souhaite estimer la probabilité que le nombre de clients dans la
salle d’attente dépasse un certain seuil critique n (entre le temps 0 et le temps
t).
1. Simuler la queue d’attente, et expliquer comment varie cette probabilité
en fonction de λ1, λ2, n et t.
2. Dans le cas où cette probabilité est très petite, on se propose d’appliquer la
méthode de l’entropie croisée. Expliquer comment appliquer cette méthode
dans ce cas précis, et l’appliquer pour un certain jeu de paramètres λ1, λ2,
n et t.
3. Reprendre cet exercise en remplaçant les lois exponentielles par des lois de
Weibul.
Note: une difficulté de l’exercice est que le nombre de variables devant être
simulées entre le temps 0 et le temps t est lui-même aléatoire, bien expliquer
comment vous avez traité ce problème.
