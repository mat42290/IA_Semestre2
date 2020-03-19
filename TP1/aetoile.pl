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
            (G2<G ->
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

% Ensemble des fonctions de test :

test_affiche_solution() :-
	empty(Q),
	initial_state(Ini),
	final_state(Fin),
	S2=[[a,vide,c],[h,b,d],[g,f,e]],
	S1=[[a,c,vide],[h,b,d],[g,f,e]],
	insert([Fin,[4,5,1],S2,down],Q,Q1),
	insert([S2,[0,3,2],S1,left],Q1,Q2),
	insert([S1,[1,2,3],Ini,up],Q2,Q3),
	insert([Ini,[4,3,0],nil,nil],Q3,Q4),
	affiche_solution(Fin,Q4).

test_expand(Succ) :-
	initial_state(Ini),
	expand(Succ,Ini,0).

test_loop_successors_1() :-
	empty(Ps),
	empty(Pf),
	empty(Q),
	initial_state(Ini),
	I2=[ [b, h, c], [a, vide, d], [g,f,e] ], % successeur possible de l'état initial (rule up)
	I3=[ [b, h, c], [a, f, d], [g,e,vide] ], % successeur possible de l'état initial (rule right)
	insert([Ini,[1,2,3],nil,rien],Ps,Ps1),
	insert([I3,[6,4,2],Ini,no_modif_I3],Ps1,Ps4),
	insert([[1,2,3],Ini],Pf,Pf1),
	insert([[6,4,2],I3],Pf1,Pf2),
	put_90(Ps4),
	loop_successors([[I2,[8,2,5],Ini,insert_I2]|[]],Q,Ps5,Ps4,Pf3,Pf2),
	put_90(Ps5),
	loop_successors([[I3,[1,4,5],Ini,modif_I3]|[]],Q,Ps_new,Ps5,_,Pf3),
	put_90(Ps_new).

test_loop_successors_2() :-
	empty(Ps),
	empty(Pf),
	empty(Q),
	initial_state(Ini),
	I2=[ [b, h, c], [a, vide, d], [g,f,e] ], % successeur possible de l'état initial (rule up)
	I3=[ [b, h, c], [a, f, d], [g,e,vide] ], % successeur possible de l'état initial (rule right)
	insert([Ini,[4,3,2],nil,rien],Ps,Ps1),
	insert([[4,3,2],Ini],Pf,Pf1),
	insert([I3,[8,4,2],Ini,need_modif_I3],Ps1,Ps2),
	insert([[8,4,2],I3],Pf1,Pf2),
	insert([I2,[4,5,6],Ini,no_modif_I2],Ps2,Ps3),
	insert([[4,5,6],I2],Pf2,Pf3),
	put_90(Ps3),
	expand(List,Ini,5),
	loop_successors(List,Q,Ps4,Ps3,_,Pf3),
	put_90(Ps4).

test_time(Runtime) :-
    statistics(runtime,[Start,_]),
    main,
    statistics(runtime,[Stop,_]),
    Runtime is Stop-Start.
