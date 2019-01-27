%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                  %
%         MSc Prolog 531                           %
%                                                  %
%         Lexis Test                               %
%                                                  %
%         Question 1 (prison)                      %
%                                                  %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                  %
%         Question 1 (prison)                      %
%                                                  %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% compile the Prison Database etc.

:- ensure_loaded(utilities).



cell(N) :-
   cells(Cells),
   in_range(1,Cells,N).


forall(C1, C2) :- \+ (C1, \+ C2).

%% ------ Add your code to this file here.

%% Uncomment the next two lines to skip Question 1 (a)
%% :- use_module(library(between)).
%% in_range(Min,Max,N) :- between(Min,Max,N).

% in_range(+Min,+Max,?N)
in_range(Min, Max, N) :-
  (Min =< Max -> calculate_range(Min, Max, Min, N)).

calculate_range(Min, Max, Acc, N) :-
  Max > Acc,
  NewAcc is Acc + 1,
  calculate_range(Min, Max, NewAcc, N).

calculate_range(_, _, Acc, Acc).





% --- empty cell
% empty_cell(?Cell)

empty_cell(Cell) :-
  cell(Cell),
	\+ prisoner(_,_,Cell,_,_,_).

% all_female_cell(?Cell)

all_female_cell(Cell) :-
  cell(Cell),
  \+ empty_cell(Cell),
  forall(prisoner(_, Firstname, Cell, _, _, _), female_name(Firstname)).


% female_prisoners(?N)

female_prisoners(N) :-
  findall(Firstname, prisoner(_,Firstname,_,_,_,_), L1),
  length(L1, N1),
  findall(Fname, (prisoner(_,Fname,_,_,_,_), \+ female_name(Fname)), L2),
  length(L2, N2),
  N is N1 - N2.

% cell_occupancy(?Cell, ?N)

cell_occupancy(Cell,N) :-
  cell(Cell),
  findall(Firstname, prisoner(_,Firstname,Cell,_,_,_), L),
  length(L, N).

% fullest_cell(?Cell)

fullest_cell(Cell) :-
  cell_occupancy(Cell, N),
  \+ (cell_occupancy(_, OtherN),
  OtherN > N).

% worst_psychopath(?S,?F,?Crime,?T)

worst_psychopath(S, F, Crime, T) :-
  psychopath(S,F),
  prisoner(S,F,_,Crime,T,_),
  \+ (psychopath(OtherS,OtherF),
  OtherS \= S,
  OtherF \= F,
  prisoner(OtherS,OtherF,_,_,OtherT,_),
  OtherT > T).


% criminals(?Crime,?N)

criminals(Crime, N) :-
  bagof(Surname-Firstname-Cell-Sentence-Time, prisoner(Surname,Firstname,Cell,Crime,Sentence,Time), L),
  length(L, N).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                  %
%         Question 2 (ciphers)                     %
%                                                  %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



% character codes
%     a  97
%     z 122
%     A  65     (97 - 32)
%     Z  90     (122 -32)

%% ------ Add your code to this file here.


% upper_case_string(+String)

upper_case_string([X|Tail]) :-
  X == 32,!,
  upper_case_string(Tail).

upper_case_string([X|Tail]) :-
  X >= 65,
  X =< 90,
  upper_case_string(Tail).

upper_case_string([]).



% subst_string(+Input, +Subst, -Output)

subst_string(Input, Subst, Output) :-
  subst_str(Input, Subst, [], Out),
  reverse(Out, Output).

subst_str([], _, Output, Output).

subst_str([X|Tail], In-Out, Acc, Output) :-
  member(X, In),!,
  substitute(X, In-Out, Sub),
  subst_str(Tail, In-Out, [Sub|Acc], Output).

subst_str([X|Tail], In-Out, Acc, Output) :-
  subst_str(Tail, In-Out, [X|Acc], Output).

substitute(X, [X|Tail1]-[Z|Tail2], Z).

substitute(X, [Y|Tail1]-[Z|Tail2], Sub) :-
  substitute(X, Tail1-Tail2, Sub).

reverse(List, NewList) :-
  reverse_list(List, [], NewList).

reverse_list([], NewList, NewList).

reverse_list([X|Tail], Acc, NewList) :-
  reverse_list(Tail, [X|Acc], NewList).

% encrypt_string(+Plain, +Key, -Cipher)

%encrypt_string(Plain, Key, Cipher) :-
%  subst_string(Plain, "ABCDEFGHIJKLMNOPQRSTUVWXYZ"-KEY, Cipher).


%encrypt_string( "more paper please", "JLPAWIQBCTRZYDSKEGFXHUONVM",CipherText).
%"YSGW KJKWG KZWJFW"
%encrypt_string( "More paper. Please!!", "JLPAWIQBCTRZYDSKEGFXHUONVM",CipherText).


encrypt_string(Plain, Key, Cipher) :-
  subst_str2(Plain, "abcdefghijklmnopqrstuvwxyz"-Key, [], Output),
  reverse(Output, Cipher).

subst_str2([], _, Output, Output).

subst_str2([X|Tail], In-Out, Acc, Output) :-
  member(X, In),!,
  substitute(X, In-Out, Sub),
  subst_str2(Tail, In-Out, [Sub|Acc], Output).

subst_str2([X|Tail], In-Out, Acc, Output) :-
  upper_case_string([X]),
  X \== 32,!,
  NewX is X + 32,
  substitute(NewX, In-Out, Sub),
  subst_str2(Tail, In-Out, [Sub|Acc], Output).

subst_str2([X|Tail], In-Out, Acc, Output) :-
  subst_str2(Tail, In-Out, [X|Acc], Output).





% decrypt_string(+Cipher, +Key, -Plain)

decrypt_string(Cipher, Key, Plain) :-
  subst_str(Cipher, Key-"ABCDEFGHIJKLMNOPQRSTUVWXYZ", [], Output),
  convertToLowerCase(Output, [], Plain).

convertToLowerCase([], Acc, Acc).

convertToLowerCase([X|Tail], Acc, LowercaseVersion) :-
  X >= 65,
  X =< 90,!,
  NewX is X + 32,
  convertToLowerCase(Tail, [NewX|Acc], LowercaseVersion).

convertToLowerCase([X|Tail], Acc, LowercaseVersion) :-
  convertToLowerCase(Tail, [X|Acc], LowercaseVersion).

% keyphrase_cipher(+Keyphrase, -Key)

keyphrase_cipher(Keyphrase, Key) :-
  construct_key(Keyphrase, [], InitialKey),
  InitialKey = [X|Tail],
  complete_key(X, InitialKey, CompleteKey),
  reverse(CompleteKey, Key).

% keyphrase has been parsed
construct_key([], Acc, Acc).

construct_key([X|Tail], Acc, InitialKey) :-
  X == 32,!,
  construct_key(Tail, Acc, InitialKey).

construct_key([X|Tail], Acc, InitialKey) :-
  \+ member(X, Acc),!,
  construct_key(Tail, [X|Acc], InitialKey).

% X is a member
construct_key([X|Tail], Acc, InitialKey) :-
  construct_key(Tail, Acc, InitialKey).

complete_key(Letter, Acc, Acc) :-
  length(Acc, 26),!.

complete_key(Letter, Acc, CompleteKey) :-
  \+ member(Letter,Acc),!,
  (Letter == 90 -> NewLetter is 65; NewLetter is Letter + 1),
  complete_key(NewLetter, [Letter|Acc], CompleteKey).

complete_key(Letter, Acc, CompleteKey) :-
  (Letter == 90 -> NewLetter is 65; NewLetter is Letter + 1),
  complete_key(NewLetter, Acc, CompleteKey).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                  %
%         Question 3 (graphs)                      %
%                                                  %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%% ------ Add your code to this file here.


/* Uncomment this to skip Question 3 (a)

merge_ordered(Left,Right,Merged) :-
   append(Left,Right,Both),
   sort(Both,Merged).

*/

% merge_ordered(+Left,+Right,-Merged)

merge_ordered(Left, Right, Merged) :-
  construct_merged(Left, Right, [], ReversedMerged),
  reverse(ReversedMerged, Merged).

construct_merged([], [], Acc, Acc).

construct_merged(_L, [X|RightTail], [X|Acc], Merged) :-
  construct_merged(_L, RightTail, [X|Acc], Merged),!.

construct_merged([X|LeftTail], _R, [X|Acc], Merged) :-
  construct_merged(LeftTail, _R, [X|Acc], Merged),!.

construct_merged([], [X|RightTail], Acc, Merged) :-
  construct_merged([], RightTail, [X|Acc], Merged).

construct_merged([X|LeftTail], [], Acc, Merged) :-
  construct_merged(LeftTail, [], [X|Acc], Merged).

construct_merged([X|LeftTail], [Y|RightTail], Acc, Merged):-
  X @> Y,!,
  construct_merged([X|LeftTail], RightTail, [Y|Acc], Merged).

construct_merged([X|LeftTail], [Y|RightTail], Acc, Merged):-
  construct_merged(LeftTail, [Y|RightTail], [X|Acc], Merged).



% hf_to_graph_term(+Hform, -Graph)

hf_to_graph_term(Hform, Graph) :-
  construct_graph(Hform, [], [], Graph).

construct_graph([], NodeList, EdgeList, graph(NodeList, EdgeList)).

construct_graph([X > Y|Tail], NodeList, EdgeList, Graph) :-
  member(X,NodeList),
  member(Y,NodeList),!,
  merge_ordered([e(X,Y)], EdgeList, Merged),
  construct_graph(Tail, NodeList, Merged, Graph).

construct_graph([X > Y|Tail], NodeList, EdgeList, Graph) :-
  member(X,NodeList),
  \+ member(Y,NodeList),!,
  merge_ordered([Y], NodeList, NewNodeList),
  merge_ordered([e(X,Y)], EdgeList, Merged),
  construct_graph(Tail, NewNodeList, Merged, Graph).

construct_graph([X > Y|Tail], NodeList, EdgeList, Graph) :-
  \+ member(X,NodeList),
  member(Y,NodeList),!,
  merge_ordered([X], NodeList, NewNodeList),
  merge_ordered([e(X,Y)], EdgeList, Merged),
  construct_graph(Tail, NewNodeList, Merged, Graph).

construct_graph([X > Y|Tail], NodeList, EdgeList, Graph) :-
  \+ member(X,NodeList),
  \+ member(Y,NodeList),!,
  merge_ordered([X], NodeList, NewNodeList),
  merge_ordered([Y], NewNodeList, NewerNodeList),
  merge_ordered([e(X,Y)], EdgeList, Merged),
  construct_graph(Tail, NewerNodeList, Merged, Graph).

construct_graph([X|Tail], NodeList, EdgeList, Graph) :-
  \+ member(X,NodeList),
  merge_ordered([X], NodeList, NewNodeList),
  construct_graph(Tail, NewNodeList, EdgeList, Graph).

construct_graph([X|Tail], NodeList, EdgeList, Graph) :-
  member(X,NodeList),
  construct_graph(Tail, NodeList, EdgeList, Graph).


% graph_term_to_adj_list(+Graph, -AdjList)

graph_term_to_adj_list(Graph, AdjList) :-
  Graph = graph(NodesList,EdgesList),
  construct_adj_lists(NodesList, EdgesList, [], AdjList).

construct_adj_lists([], EdgesList, Acc, Acc).

construct_adj_lists([X|Tail], EdgesList, Acc, AdjList) :-
  construct_list(X, EdgesList, [], List),
  merge_ordered([n(X,List)], Acc, NewAcc),
  construct_adj_lists(Tail, EdgesList, NewAcc, AdjList).


construct_list(X, [], Acc, Acc).

construct_list(X, [e(X,Y)|Tail], Acc, List) :-
  merge_ordered([Y], Acc, NewAcc),!,
  construct_list(X, Tail, NewAcc, List).

construct_list(X, [e(Y,Z)|Tail], Acc, List) :-
  construct_list(X, Tail, Acc, List).
