:- ['sigmas1.pl'].
:- ['taus2.pl'].
:- ['counts1.pl'].

	
% Compute all possible "solution(Probability,TagList)" for a given WordList
% ===================================================
viterbi_all(WordList,solution(P,Sequence)) :-
          sequences(WordList,[solution(1,[@])],PSs),
          member(solution(P,Sequence1),PSs),
          reverse(Sequence1,[@|Sequence]).

% Base case: no more words to parse.
% ===================================================
sequences([],PSs,PSs).

% Recursive case: find max (of all taggings for Word * all transitions)
% ===================================================
sequences([Word|Words],PSs0,PSs) :-
     findall(PS2,
                   (ta(L3,Word,T2),
					count(L4, T2),
					Prob1 is L3/L4,
                    findall(solution(Prob,[T2,T1|Ts]),
                                  (member(solution(Prob3,[T1|Ts]),PSs0),
                                     sig(L2,T2,T1),
									 count(L, T2),
									 Prob2 is L2/L,
                                     Prob is Prob1*Prob2*Prob3),
                               PSs),
                    max_key(PSs,PS2)),
                PSs1),
sequences(Words,PSs1,PSs).

% Find tagging with maximum Probability (tail-recursive, with accumulator) 
% ===================================================
max_key(PList,Max):-  max_key(PList,solution(0,[]),Max).
max_key([],X,X).
max_key([solution(P,_)|Rest],solution(N,L),M):- N > P, max_key(Rest,solution(N,L),M).
max_key([solution(P,L)|Rest],solution(N,_),M):- P > N, max_key(Rest,solution(P,L),M).