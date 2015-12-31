%The relation 'askable' defines those things
% that can be asked of the user. 
%The operatos '::', si, entonces, 'and', 'o'

%     A small knowledge base fo selecting career
% Program assumes built-in operator: op( 700, xfx, is)
:-  op( 100, xfx, [ requiere, es, del_area, satisface_interés]).
:-  op( 100, xf, [si,entonces, o]).
:-  op( 900, xfx, ::).
:-  op( 800, xfx, was).
:-  op( 870, fx, si).
:-  op( 880, xfx, entonces).
:-  op( 550, xfy, o).
:-  op( 540, xfy, and).
:-  op( 300, fx, 'derived by').
:-  op( 600, xfx, from).
:-  op( 600, xfx, by).
:-  op( 900, fy, not).


%areas
area1 :: si Carrera requiere habilidad_matemática_media_o_alta entonces Carrera es ciencias_exactas.

area2 :: si Carrera satisface_interés carrera_práctica entonces Carrera es ciencias_aplicadas.

area3 :: si Carrera requiere uso_medio_computadora entonces Carrera es ciencias_computacionales.

area4 :: si Carrera requiere expresividad entonces Carrera es artes.

area5 :: si Carrera satisface_interés medio_ambiente entonces Carrera es ciencias_naturales.

area6 :: si Carrera satisface_interés lectura o Carrera satisface_interés escritura entonces Carrera es letras.

area7 :: si Carrera satisface_interés interdisciplinariedad entonces Carrera es ciencias_interdisciplinarias.

area8 :: si Carrera requiere conciencia_de_la_situación_económica_política_y_social entonces Carrera es humanidades.

area9 :: si Carrera satisface_interés sector_comercial_y_de_servicios entonces Carrera es disciplinas_de_servicios.

%carreras alfabéticamente

rule1 :: si Carrera es ciencias_exactas and Carrera requiere capacidad_para_representar_problemas entonces Carrera es actuaría.

rule2 :: si Carrera es ciencias_aplicadas and Carrera requiere facilidad_para_interpretar_fenómenos_culturales_políticos_y_económicos and Carrera requiere liderazgo entonces Carrera es administración.

rule3 :: si Carrera es administración and Carrera requiere facilidad_en_idiomas_extranjeros entonces Carrera es administración_turística.

rule4 :: si Carrera es ciencias_aplicadas and Carrera requiere facilidad_para_hacer_prototipos and Carrera satisface_interés tecnología entonces Carrera es aeronáutica.

rule5 :: si Carrera es ciencias_aplicadas and Carrera requiere trabajo_en_equipo and Carrera satisface_interés agricultura and Carrera satisface_interés investigación and Carrera satisface_interés tecnología entonces Carrera es agronomía.

rule6 :: si Carrera es ciencias_aplicadas and Carrera requiere habilidad_para_el_dibujo_de_espacios entonces Carrera es arquitectura.

rule7 :: si Carrera es ciencias_aplicadas and Carrera satisface_interés investigación and Carrera satisface_interés investigación_espacial entonces Carrera es aeronáutica.

rule8 :: si Carrera es ciencias_naturales and Carrera satisface_interés seres_vivos entonces Carrera es biología.

rule9 :: si Carrera es musica and Carrera requiere buena_voz entonces Carrera es canto.

rule10 :: si Carrera es ciencias_aplicadas and Carrera requiere conocimiento_de_estadística and Carrera satisface_interés finanzas and Carrera requiere reconocer_problemas_sociales_y_económicos entonces Carrera es economía.

rule11 :: si Carrera requiere responsabilidad and Carrera satisface_interés ética entonces Carrera es educación_especial.

rule12 :: si Carrera requiere excelente_salud and Carrera satisface_interés salud and Carrera satisface_interés ejercicio entonces Carrera es educación_física.

rule13 :: si Carrera es ciencias_exactas and Carrera requiere curiosidad and Carrera requiere dedicación entonces Carrera es física.

rule14 :: si Carrera es disciplinas_de_servicios and Carrera requiere creatividad and Carrera requiere responsabilidad and Carrera requiere resistencia_al_cansancio and Carrera satisface_interés alimentos entonces Carrera es gastronomía.

rule15 :: si Carrera es biología and Carrera requiere habilidad_para_reconocer_patrones entonces Carrera es genética.

rule16 :: si Carrera es artes and Carrera requiere paciencia entonces Carrera es musica.

rule17 :: si Carrera es ciencias_aplicadas and Carrera requiere promedio_mayor_de_9 entonces Carrera es medicina.

rule18 :: si Carrera es artes and Carrera requiere facilidad_para_dibujo_de_imitación entonces Carrera es pintura.

rule19 :: si Carrera es humanidades and Carrera requiere vocación_de_servicio and Carrera satisface_interés ética and Carrera satisface_interés religión and Carrera satisface_interés trabajo_comunitario entonces Carrera es religiones.

rule20 :: si Carrera es disciplinas_de_servicios and Carrera requiere manejo_básico_de_computadora and Carrera requiere hablar_en_público  and Carrera requiere tolerancia and Carrera satisface_interés conocer_lugares and Carrera satisface_interés conocer_gente entonces Carrera es turismo.

rule21 :: si Carrera es turismo and Carrera requiere disposición_para_ir_a_lugares_poco_comunes and Carrera satisface_interés medio_ambiente and Carrera satisface_interés conocer_culturas_locales entonces Carrera es turismo_alternativo.

fact :: X es carrera   :-
  member( X, [canto,medicina,aeronáutica,arquitectura,pintura, agronomía]).

% askable( _ variante_especializada _, 'Carrera' variante_especializada
% 'What').

askable( _ requiere _, 'Carrera' requiere 'Something').

askable( _ satisface_interés _, 'Carrera' satisface_interés 'Something').

%  An expert system shell

% Top-level driving procedure

expert1  :-
  bienvenida,
  prompt,
  getquestion( Question),       % Input user's question
  ( answeryes( Question);	% Try to find positive answer
    answerno( Question) ).	% si no positive answer entonces find negative

expert  :-
  bienvenida,
  ( answeryes( carrera es X);	% Try to find positive answer
    answerno( carrera es X) ).	% si no positive answer entonces find negative

prompt :- write('ingrese su elección en forma de pregunta sin eingno de interrogación'),nl,write('por ejemplo:'),tab(3),write('carrera es canto').

answeryes( Question)  :-		% Look fo positive answers to Question
  markstatus( negative), 		% No positive answer yet
  explore( Question, [], Answer),       % Trace is empty
  positive( Answer),                    % Look fo positive answers
  markstatus( positive),		% Positive answer found
  present( Answer), nl,
  write('More solutions? '), 
  getreply( Reply),	 		% Read user's reply
  Reply = no.            % Otherwise backtrack to 'exploe'

answerno( Question)  :-			% Look fo negative answer to question
  retract( no_positive_answer_yet), !,  % Has there been no positive answer?
  explore( Question, [], Answer),
  negative( Answer),
  present( Answer), nl,
  write('More negative solutions? '), 
  getreply( Reply),
  Reply = no.		% Otherwise backtrack to 'exploe'

markstatus( negative)  :-
  assert( no_positive_answer_yet).

markstatus( positive)  :-
  retract( no_positive_answer_yet), !; true.

getquestion( Question)  :-
  nl, write( 'Question, please '),
  nl,
  read( Question).


% exploe( Goal, Trace, Answer):
%   find Answer to a given Goal. Trace is a chain of ancesto
%   goals and rules. 
%   'exploe' tends to find a positive answer to a question.
%   Answer is 'false' only when all the possibilities have been
%   investigated and they all resulted in 'false'


explore( Goal, Trace, _)  :-
  copy_term( Goal, Copy),	% Make copy of Goal with variables renamed
  member( Copy by _, Trace),	%Rule % Similar ancesto goal?
  variante_especializada( Copy, Goal),   	% Ancesto goal as general as Goal?
  !, fail.			% Abandon Goal because of cycle!

explore( Goal, _, Goal is true was 'found as a fact')  :- %Trace
  fact :: Goal.

explore( Goal, Trace,	               % Assume only one rule about each type of goal
    Goal is TruthValue was 'derived by' Rule from Answer)  :-
  Rule :: si Condition entonces Goal,      % Rule relevant to Goal
  explore( Condition, [Goal by Rule | Trace], Answer),
  truth( Answer, TruthValue).

explore( Goal1 and Goal2, Trace, Answer)  :-  !,
  explore( Goal1, Trace, Answer1),
  continue( Answer1, Goal1 and Goal2, Trace, Answer).

explore( Goal1 o Goal2, Trace, Answer)  :-
  exploreyes( Goal1, Trace, Answer)          % Positive answer to Goal1
  ;
  exploreyes( Goal2, Trace, Answer).         % Positive answer to Goal2

explore( Goal1 o Goal2, Trace, Answer1 and Answer2)  :-  !,
  not exploreyes( Goal1, Trace, _),
  not exploreyes( Goal2, Trace, _),          % No positive answer
  explore( Goal1, Trace, Answer1), 	     % Answer1 must be negative
  explore( Goal2, Trace, Answer2).           % Answer2 must be negative


explore( not Goal, Trace, Answer)  :-  !,    % Assuming no variables in Goal
  explore( Goal, Trace, Answer1),
  invert( Answer1, Answer).

explore( Goal, Trace, Goal is Answer was told)  :-
  useranswer( Goal, Trace, Answer).                 % User-supplied answer

exploreyes( Goal, Trace, Answer)  :-
  explore( Goal, Trace, Answer),
  positive( Answer).

continue( Answer1, _ and Goal2, Trace, Answer)  :- %Goal1
  positive( Answer1),
  explore( Goal2, Trace, Answer2),
  (  positive( Answer2), Answer = Answer1 and Answer2
     ;
     negative( Answer2), Answer = Answer2 ).

continue( Answer1, _ and _, _, Answer1)  :- %Goal1 Goal2
  negative( Answer1).

truth( _ is TruthValue was _, TruthValue)  :-  !. %Question Found

truth( Answer1 and Answer2, TruthValue)  :-
  truth( Answer1, true),
  truth( Answer2, true), !,
  TruthValue = true
  ;
  TruthValue = false.

positive( Answer)  :-
  truth( Answer, true).

negative( Answer)  :-
  truth( Answer, false).


invert( Quest is true was Found, (not Quest) is false was Found).

invert( Quest is false was Found, (not Quest) is true was Found).

instantiated( Term)  :-
  numbervars( Term, 0, 0).   	% No variables in Term


% useranswer( Goal, Trace, Answer):
%   Generate, through backtracking, user-supplied solutions to Goal.
%   Trace is a chain of ancesto goals and rules used fo 'why' explanation

useranswer( Goal, Trace, Answer)  :-
  askable( Goal, _),                    % May be asked of the user
  freshcopy( Goal, Copy),               % Variables in Goal renamed
  useranswer( Goal, Copy, Trace, Answer, 1).

% Do not ask again about an instantiated goal

useranswer( Goal, _, _, _, N)  :-
  N > 1,		            % Repeated question?
  instantiated( Goal), !,
  fail.		                    % Do not ask again

% Is Goal implied true o false fo all instantiations?

useranswer( Goal, Copy, _, Answer, _)  :-
  wastold( Copy, Answer, _),
  variante_especializada( Copy, Goal), !.	    % Answer to Goal implied


% Retrieve known solutions, indexed from N on, fo Goal.

useranswer( Goal, _, _, true, N)  :-
  wastold( Goal, true, M),
  M >= N.

% Has everything already been said about Goal?

useranswer( Goal, Copy, _, Answer, _)  :- %2nd N
  end_answers( Copy),
  variante_especializada( Copy, Goal), !,     % Everything was already said about Goal
  not wastold( Goal, _, _),        % There was no explicit answer
  Answer = false.                  % It follows Answer must be negative

% Ask the user fo (moe) solutions

useranswer( Goal, _, Trace, Answer, N)  :-
  askuser( Goal, Trace, Answer, N).

askuser( Goal, Trace, Answer, N)  :-
  askable( Goal, ExternFomat),
  format( Goal, ExternFomat, Question, [], Variables),    % Get question fomat
  ask( Goal, Question, Variables, Trace, Answer, N).

ask( Goal, Question, Variables, Trace, Answer, N)  :-
  nl,
  (  Variables = [], !, 	             % Introduce question
     write( 'Is it true: ')
     ;
     write( 'Any (more) solution to: ') 
  ),
  write( Question), write('? '), 
  getreply( Reply), !,                       % Reply = yes/no/why
  process( Reply, Goal, Question, Variables, Trace, Answer, N).


process( why, Goal, Question, Variables, Trace, Answer, N)  :-
  showtrace( Trace),
  ask( Goal, Question, Variables, Trace, Answer, N).

process( yes, Goal, _, Variables, Trace, true, _)  :- %2nd N
  nextindex( Next),                          % Get new free index fo 'wastold'
  Next1 is Next + 1,
  (  askvars( Variables),
     assertz( wastold( Goal, true, Next))              % Recod solution
     ;
     copy_term( Goal, Copy),                           % Copy of Goal
     useranswer( Goal, Copy, Trace, _, Next1) ).  % Moe answers? Answer

process( no, Goal, _, _, _, false, _)  :- %N
  freshcopy( Goal, Copy),
  wastold( Copy, true, _), !,               % 'no' means: no moe solutions
  assertz( end_answers( Goal)),             % Mark end of answers
  fail
  ;
  nextindex( Next),                         % Next free index fo 'wastold'
  assertz( wastold( Goal, false, Next)).    % 'no' means: no solution


format( Var, Name, Name, Vars, [Var/Name|Vars])  :-
  var( Var), !.

format( Atom, Name, Atom, Vars, Vars)  :-
  atomic( Atom),  !,
  atomic( Name).

format( Goal, Fom, Question, Vars0, Vars)  :-
  Goal =.. [Functo|Args1],
  Fom =.. [Functo|Foms],
  formatall( Args1, Foms, Args2, Vars0, Vars),
  Question =.. [Functo|Args2], !.

% si fomatting failed due to structural dsiference fomat Goal after itself

format( Goal, _, Question, Vars0, Vars)  :-
  format( Goal, Goal, Question, Vars0, Vars).

formatall( [], [], [], Vars, Vars).

formatall( [X|XL], [F|FL], [Q|QL], Vars0, Vars)  :-
  formatall( XL, FL, QL, Vars0, Vars1),
  format( X, F, Q, Vars1, Vars).

askvars( []).

askvars( [Variable/Name|Variables])  :-
  nl, write( Name), write( ' = '), 
  read( Variable),
  askvars( Variables).

showtrace([])  :-
  nl, write('This was your question'), nl.

showtrace( [Goal by Rule | Trace])  :-
  nl, write( 'To investigate, by '),
  write( Rule), write( ', '),
  write( Goal),
  showtrace( Trace).

% instance-of( T1, T2) means: instance of T1 is T2; that is
% term T1 is moe general than T2 o equally general as T2

variante_especializada( Term, Term1)  :-	% Instance of Term is Term1
  copy_term( Term1, Term2),	% Copy of Term1 with fresh set of variables
  numbervars( Term2, 0, _), !,
  Term = Term2.                 % This succeeds si Term1 is instance of Term

freshcopy( Term, FreshTerm)  :- % Make a copy of Term with variables renamed
  asserta( copy( Term)),
  retract( copy( FreshTerm)), !.


nextindex( Next)  :-            % Next free index fo 'wastold'
  retract( lastindex( Last)), !,
  Next is Last + 1,
  assert( lastindex( Next)).

% Initialise dynamic procedures lastindex/1, wastold/3, end_answers/1

:- assert( lastindex( 0)),
   assert( wastold( dummy, false, 0)),
   assert( end_answers( dummy)).

% Displaying the conclusion of a consultation and 'how' explanation

present( Answer)  :-
  nl, showconclusion( Answer),
  nl, write( 'Would you like to see how?'),
  nl, 
  getreply( Reply),
  ( Reply = yes, !, show( Answer)
    ;
    true ).

showconclusion( Answer1 and Answer2)  :-  !,
  showconclusion( Answer1), write( ' and '),
  showconclusion( Answer2).

showconclusion( Conclusion was _ )  :- %Found
  write( Conclusion).

% 'show' displays a complete soltuin tree

show( Solution)  :-
  nl, show( Solution, 0), !.            % Indent by 0

show( Answer1 and Answer2, H)  :-  !,   % Indent by H
  show( Answer1, H),
  tab( H), write(and), nl,
  show( Answer2, H).

show( Answer was Found, H)  :-          % Indent by H
  tab( H), writeans( Answer),           % Show conclusion
  nl, tab( H),
  write( '  was '),
  show1( Found, H).                     % Show evidence

show1( Derived from Answer, H)  :-  !,
  write( Derived), write(' from'),      % Show rule name
  nl, H1 is H + 4,
  show( Answer, H1).                    % Show antecedent

show1( Found, _)  :-                    % Found = 'told' o 'found as fact'
  write( Found), nl.

writeans( Goal is true)  :-  !,
  write( Goal).		                % Omit 'is true' on output

writeans( Answer)  :-                   % This is negative answer
  write( Answer).

% Note: getreply should not be called with the argument instantiated
%%	shell
getreply(Reply):-
	read(Answer),
	means(Answer,Reply),!
	;
	nl,write('Answer uknown, try again please'),nl,
	getreply(Reply).
getreply( Meaning)  :-
  read( Reply),
  means( Reply, Meaning),  !;          % Reply means something?
  nl, write('Answer unknown, try again please!'),     % Handle bad reply
  nl, 
  getreply( Meaning).                  % Try again


member( X, [X|_]).

member( X, [_|L])  :-
  member( X, L).

%numbervars( Term, N, Nplus1)  :-
%  var( Term), !,                      % Variable?
%  Term = var/N,
%  Nplus1 is N + 1.

%numbervars( Term, N, M)  :-
%  Term =.. [_| Args],          % Structure o atomic Functo 
%  numberargs( Args, N, M).

numberargs( [], N, N)  :-  !.

numberargs( [X | L], N, M)  :-
  numbervars( X, N, N1),
  numberargs( L, N1, M).

value(Frame,Slot,Value):-
	Query=..[Frame,Slot,Value],
	call(Query),
	write(Value),nl.
	%;fail.               %% retrieve value
value_parent(Frame,Slot):-
	parent(Frame,ParentFrame),
	value(Frame,Slot,Value),
	value(ParentFrame,Slot,Value1),
	value_parent1(ParentFrame,Slot,[Value,Value1]),	
	Value\==Value1,!.	

value_parent1(Frame,Slot,[X|Xs]):-
	conc(X,Xs,W),
	parent(Frame,ParentFrame),
	value(ParentFrame,Slot,Value),	
	conc(Value,W,Y),
	write(Y),nl,	
	value_parent1(ParentFrame,Slot,Y)%[X,Xs,Value]
	;
	conc(X,Xs,W),
	write(W),!.

parent(Frame,ParentFrame):-
	( Query=..[Frame,del_area,ParentFrame]
	;   
	Query=..[Frame,variante_especializada,ParentFrame]),
	call(Query).

%means(yes,yes).
%means(y,yes).
%means(no,no).
%means(n,no).
%means(why,why).
%means(w,why).
means( why, why)  :-  !.
means( w,   why)  :-  !.

means( yes, yes)  :-  !.
means( y,   yes)  :-  !.

means( no,  no)   :-  !.
means( n,   no)   :-  !.

conc([],[L],W):-conc([],L,W),!.
%conc([L],[L1,[L2]],L4):-
	%conc(L,L1,L3),conc(L2,L3,L4),!.
conc([],L,L).
conc(A,[L],W):-conc(A,L,W),!.
conc([L],A,W):-conc(A,L,W),!.
conc([X|L1],L2,[X|L3]):-
	conc(L1,L2,L3),!.
conc(L2,[X|L1],[X|L3]):-
	conc(L1,L2,L3),!.
%conc([L],A,[A|L]).
conc(A,B,[A,B]):-atomic(A),atomic(B),!.	
%shell
bienvenida:-
	write('Este es un sistema experto que le ayudará a elegir la mejor carrera para usted'),nl.
