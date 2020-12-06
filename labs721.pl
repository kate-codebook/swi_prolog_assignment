only_odd_digits(X) :- X < 10, only_odd_digits(X,X mod 2).
only_odd_digits(X) :- X >= 10, X1 is X mod 10, only_odd_digits(X,X1 mod 2).
only_odd_digits(X,Y) :- X < 10, Y1 is X mod 2, Y2 is Y1 * Y, Y2 =:= 1.
only_odd_digits(X,Y) :- X >= 10, X1 is X div 10, X2 is X mod 10, X3 is X2 mod 2, Y1 is Y * X3, only_odd_digits(X1, Y1).  


domino_cycle([H|T]) :- append([H],T, X), domino_cycle(X, H).
domino_cycle([(A, B)|[(C, D)|TT]], S) :- between(1, 6, A), between(1, 6, B), between(1, 6, C), between(1, 6, D), append([(C, D)], TT, X), B =:= C, domino_cycle(X, S).
domino_cycle([(_, X)|[]], (Y, _)) :- X = Y.

first_missing_positive(L, N) :- O is 1, first_missing_positive(L, N, O). 
first_missing_positive(L, N, O) :- member(O, L), F is O + 1, first_missing_positive(L, N, F). 
first_missing_positive(L, N, O) :- not(member(O, L)), N is O. 

three_summers(List, Sum, A, B, C) :- 
    select(X, List, L0), 
    select(Y, L0, L1), 
    select(Z, L1, _), 
    X + Y + Z =:= Sum, 
    X =< Y, 
    Y =< Z,
    A is X,
    B is Y,
    C is Z. 

extract_increasing(S, L) :- 
    atom_chars(S, [H|[TH|TT]]),
    atom_number(H, HI),
    extract_increasing(S, L, [HI], [H], [TH], TT). 
extract_increasing(S, L, N, P, ['0'], [TH|TT]) :- 
    extract_increasing(S, L, N, P, [TH], TT). 
extract_increasing(S, L, N, [P], [H], [TH|TT]) :-  
    atom_number(P, PI), 
    atom_number(H, HI), 
    PI < HI, 
    append(N, [HI], X), 
    extract_increasing(S, L, X, [H], [TH], TT).    
extract_increasing(S, L, N, [P], [H], [TH|TT]) :-  
    atom_number(P, PI), 
    atom_number(H, HI), 
    PI >= HI, 
    atom_concat(H, TH, Y), 
    extract_increasing(S, L, N, [P], [Y], TT).  
extract_increasing(S, L, N, [P], [H], []) :-  
    atom_number(P, PI), 
    atom_number(H, HI), 
    PI < HI,  
    append(N, [HI], X), 
    extract_increasing(S, L, X, [], [], []). 
extract_increasing(S, L, N, [P], [H], []) :-  
    atom_number(P, PI), 
    atom_number(H, HI), 
    PI >= HI,  
    extract_increasing(S, L, N, [], [], []).
extract_increasing(_, L, N, [], [], []) :- L = N.

replace([_|T], 0, X, [X|T]).
replace([H|T], I, X, [H|R]):- I > 0, I1 is I-1, replace(T, I1, X, R).
taxi_zum_zum(S, P) :- 
    atom_chars(S, SL), 
    I is 1, 
    C = [0,0,0,0], 
    taxi_zum_zum(S, P, SL, C, I).
taxi_zum_zum(S, P, [H|T], C, I) :- 
    H = r, % when r move index from N 2 to 3
    NI is (I + 1) mod 4,
    taxi_zum_zum(S, P, T, C, NI). 
taxi_zum_zum(S, P, [H|T], C, I) :- 
    H = l, % when r move index from N 2 to 3
    NI is (I - 1) mod 4,
    taxi_zum_zum(S, P, T, C, NI). 
taxi_zum_zum(S, P, [H|T], C, I) :- 
    H = f, 
    nth0(I, C, IV), 
    V is IV + 1,
    replace(C, I, V, NC), 
    taxi_zum_zum(S, P, T, NC, I). 
taxi_zum_zum(_, P, [] , C, _) :- 
    nth0(0, C, L), NL is L * (-1),
    nth0(1, C, N),
    nth0(2, C, R),
    X is NL + R,
    nth0(3, C, S), NS is S * (-1), 
    Y is N + NS, 
    P = (X, Y). 


len([], LenResult):- LenResult is 0.
len([_|Y], LenResult):- len(Y, L), LenResult is L + 1.
riffle(LL, RL, R, M) :- 
    var(LL), var(RL), nonvar(R), var(M), 
    len(R, RLEN), 
    RLEN mod 2 == 0, !. 
riffle(LL, RL, R, M) :- 
    nonvar(LL), nonvar(RL), 
    riffle(LL, RL, R, M, []). 
riffle([LH|LT], [RH|RT], R, M, BL) :- 
    M = left, 
    append(BL, [LH], BBL), 
    append(BBL, [RH], BBBL), 
    riffle(LT, RT, R, M, BBBL). 
riffle([LH|LT], [RH|RT], R, M, BL) :- 
    M = right, 
    append(BL, [RH], BBL), 
    append(BBL, [LH], BBBL), 
    riffle(LT, RT, R, M, BBBL). 
riffle([], [], R, _, BL) :-  
    R = BL, !. 


asc(S, E, L) :- AL = [], asc(S, E, L, AL). 
asc(S, E, L, AL) :- S =< E, append(AL, [S], AAL), NS is S + 1, asc(NS, E, L, AAL). 
asc(S, E, L, AL) :- S > E, L = AL. 
dsc(S, E, L) :- AL = [], dsc(S, E, L, AL). 
dsc(S, E, L, AL) :- E >= S, append(AL, [E], AAL), NE is E - 1, dsc(S, NE, L, AAL). 
dsc(S, E, L, AL) :- E < S, L = AL. 
boustrophedon(S, ROW, COL, R) :- 
    BL = [], 
    E is (S + (ROW*COL) - 1), 
    I is 1, 
    boustrophedon(S, ROW, COL, R, E, BL, I). %[], RC, CC, Index 
boustrophedon(S, ROW, COL, R, E, BL, I):- 
    I mod 2 =:= 1, I =< ROW, 
    SP is COL*(I-1) + S, 
    EP is COL*I + S - 1, 
    asc(SP, EP, L), 
    append(BL, [L], BBL), 
    NI is I + 1,
    boustrophedon(S, ROW, COL, R, E, BBL, NI).
boustrophedon(S, ROW, COL, R, E, BL, I):- 
    I mod 2 =:= 0, I =< ROW, 
    SP is COL*(I-1) + S, 
    EP is COL*I + S - 1, 
    dsc(SP, EP, L), 
    append(BL, [L], BBL), 
    NI is I + 1,
    boustrophedon(S, ROW, COL, R, E, BBL, NI).
boustrophedon(_, ROW, _, R, _, BL, I):- 
    I > ROW, R = BL, !. 

flip(SS, SE, L, R) :- SS =< (SS + SE) // 2, SSI is SS - 1, SEI is SE - 1, nth0(SSI, L, SST), nth0(SEI, L, SET), replace(L, SSI, SET, BL), replace(BL, SEI , SST, BBL), NSS is SS + 1, NSE is SE - 1, flip(NSS, NSE, BBL, R). 
flip(SS, SE, L, R) :- SS > (SS + SE) // 2, R = L. 
%Text, Result, Textlength, Swamp_start, Swamp_end, BuildList 
pancake_scramble(T, R) :- atom_chars(T, BL),len(BL, TL), TL =< 1, R = T. 
pancake_scramble(T, R) :- atom_chars(T, BL),
    len(BL, TL), TL > 1, 
    SS is 1, SE is SS + 1, 
    pancake_scramble(T, R, TL, SS, SE, BL). %SS-swamp_start_p SE- swamp_end_p
pancake_scramble(T, R, TL, SS, SE, BL) :- 
    SE =< TL, 
    flip(SS, SE, BL, BBL), 
    NSE is SE + 1, 
    pancake_scramble(T, R, TL, SS, NSE, BBL). 
pancake_scramble(_, R, TL, _, SE, BL) :- 
    SE > TL, 
    atomics_to_string(BL, R).

sum([], 0).
sum([H|T], N):- sum(T, X), N is X + H.
compute(L, R):- BL = [],len(L, LEN), compute(L, R, BL, LEN). 
compute([H|T], R, BL, LEN) :- NH is H - 1, append(BL, [NH], BBL), compute(T, R, BBL, LEN). 
compute([], R, BL, LEN) :- delete(BL, 0, BBL), append([LEN],BBL,R). 
all(L, MAX):- sort(0, @<, L, SL), numlist(1,MAX,NL), SL == NL.    
bulgarian_solitaire(L, K, M):- 
    sum(L, LSUM),
    KSUM is K*(K+1)/2, 
    TM is 0, 
    bulgarian_solitaire(L, K, M, KSUM, LSUM, TM). %TempMove 
bulgarian_solitaire(L, K, M, KSUM, LSUM, TM) :- 
    KSUM =\= LSUM, 
    compute(L, BL), 
    sum(BL, BLSUM),
    TTM is TM + 1, 
    bulgarian_solitaire(BL, K, M, KSUM, BLSUM, TTM). 
bulgarian_solitaire(L, K, M, KSUM, LSUM, TM) :- 
    KSUM == LSUM,
    not(all(L, K)), 
    compute(L, BL), 
    sum(BL, BLSUM),
    TTM is TM + 1, 
    bulgarian_solitaire(BL, K, M, KSUM, BLSUM, TTM). 
bulgarian_solitaire(L, K, M, KSUM, LSUM, TM) :- 
    KSUM == LSUM,
    all(L, K), 
    M = TM,!. 