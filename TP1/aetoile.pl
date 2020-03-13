%*******************************************************************************
%                                    AETOILE
%*******************************************************************************

/*
Rappels sur l'algorithme
 
- structures de donnees principales = 2 ensembles : P (etat pendants) et Q (etats clos)
- P est dedouble en 2 arbres binaires de recherche equilibres (AVL) : Pf et Pu
 
   Pf est l'ensemble des etats pendants (pending states), ordonnes selon
   f croissante (h croissante en cas d'egalite de f). Il permet de trouver
   rapidement le prochain etat a developper (celui qui a f(U) minimum).
   
   Pu est le meme ensemble mais ordonne lexicographiquement (selon la donnee de
   l'etat). Il permet de retrouver facilement n'importe quel etat pendant

   On gere les 2 ensembles de fa�on synchronisee : chaque fois qu'on modifie
   (ajout ou retrait d'un etat dans Pf) on fait la meme chose dans Pu.

   Q est l'ensemble des etats deja developpes. Comme Pu, il permet de retrouver
   facilement un etat par la donnee de sa situation.
   Q est modelise par un seul arbre binaire de recherche equilibre.

Predicat principal de l'algorithme :

   aetoile(Pf,Pu,Q)

   - reussit si Pf est vide ou bien contient un etat minimum terminal
   - sinon on prend un etat minimum U, on genere chaque successeur S et les valeurs g(S) et h(S)
	 et pour chacun
		si S appartient a Q, on l'oublie
		si S appartient a Ps (etat deja rencontre), on compare
			g(S)+h(S) avec la valeur deja calculee pour f(S)
			si g(S)+h(S) < f(S) on reclasse S dans Pf avec les nouvelles valeurs
				g et f 
			sinon on ne touche pas a Pf
		si S est entierement nouveau on l'insere dans Pf et dans Ps
	- appelle recursivement etoile avec les nouvelles valeurs NewPF, NewPs, NewQs

*/

%*******************************************************************************

:- ['avl.pl'].       % predicats pour gerer des arbres bin. de recherche   
:- ['taquin.pl'].    % predicats definissant le systeme a etudier

%*******************************************************************************

main :-
	% initialisations Pf, Pu et Q 

	% lancement de Aetoile

	initial_state(S0),
    heuristique(S0,H0),
    G0 is 0, % cas général Gn = Gn-1 + Hn
    F0 is 0,
    empty(Pf),
    empty(Ps),
    empty(Qs),
    insert([ [F0,H0,G0], S0 ], Pf, Pf2),
    insert([S0, [F0,H0,G0], nil, nil], Ps, Ps2),
    aetoile(Pf2, Ps2, Qs).

%*******************************************************************************

aetoile(nil,nil,_) :-
    nl,
    write("PAS DE SOLUTION : L'ETAT FINAL N'EST PAS ATTEIGNABLE !").
aetoile(Pf, Ps, Qs) :-
    Pf \= nil,
    Ps \= nil,
    final_state(Fin),
    suppress_min([[F,H,G],U], Pf, Pf2),
    ( U = Fin ->
        suppress([U, [F,H,G], P, A], Ps, _),
        insert([U, [F,H,G], P, A], Qs, Qs_new),
        nl,
        affiche_solution(Fin, Qs_new)
    ;
        suppress([U,[F,H,G], P, A], Ps, Ps2),
        expand(S,U,G),
        loop_successors(S,Qs,Ps_new,Ps2,Pf_new,Pf2),
        insert([U, [F,H,G], P, A], Qs, Qs_new),
        aetoile(Pf_new,Ps_new,Qs_new)
    ).

affiche_solution(nil,_).
affiche_solution(U,Q) :-
    U \= nil,
    belongs([U,_,Pere,A],Q),
    affiche_solution(Pere,Q),
    (A=nil -> 
        write('Solution')
    ;
        write(' -> '), write(A)
    ).

expand(S,U,G) :-
    findall([S2, [F,H,G2], U, R],
            (rule(R,1,U,S2),heuristique(S2,H),G2 is G+H,F is G2+H),
            S).

loop_successors([],_,Ps,Ps,Pf,Pf).
loop_successors([[S2, [F,H,G], U, A]|R],Q,Ps_goal,Ps,Pf_goal,Pf) :-  
    ( belongs([S2,_,_,_],Q) ->
        loop_successors(R,Q,Ps_goal,Ps,Pf_goal,Pf) 
    ;
        ( belongs([S2, [F2,H2,G2], U2, A2],Ps) ->
            (G2 < G ->
                suppress([S2, [F2,H2,G2], U2, A2],Ps,Ps2),
                insert([S2, [F,H,G], U, A],Ps2,Ps_new),
                suppress([[F2,H2,G2], S2],Pf,Pf2),
                insert([[F,H,G], S2], Pf2, Pf_new), 
                loop_successors(R,Q,Ps_goal,Ps_new,Pf_goal,Pf_new)
            ;
                loop_successors(R,Q,Ps_goal,Ps,Pf_goal,Pf)
            )
        ;
            insert([S2, [F,H,G], U, A], Ps, Ps_new),   
            insert([[F,H,G], S2], Pf, Pf_new), 
            loop_successors(R,Q,Ps_goal,Ps_new,Pf_goal,Pf_new)
        )
    ).

test_time(Runtime) :-
    statistics(runtime,[Start,_]),
    main,
    statistics(runtime,[Stop,_]),
    Runtime is Stop-Start. 
