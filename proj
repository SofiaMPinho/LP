:- [codigo_comum].
% Maria Sofia Mateus Pinho 99272

%----------------------------------------------------------------
%         combinacoes_soma(N, Els, Soma, Combs)
% N e um inteiro, Els e uma lista de inteiros, e Soma e
% um inteiro, significa que Combs e a lista ordenada cujos
% elementos sao as combinacoes N a N, dos elementos de Els
% cuja soma e Soma
%----------------------------------------------------------------
combinacoes_soma(N, Els, Soma, Combs) :-
    findall(Comb, (combinacao(N, Els, Comb),
                   sum_list(Comb, Soma)), Combs).

%----------------------------------------------------------------
%         permutacoes_soma(N, Els, Soma, Perms)
% N e um inteiro, Els e uma lista de inteiros, e Soma e um
% inteiro, significa que Perms e a lista ordenada cujos
% elementos sao permutacoes das combinacoes N a N, dos
% elementos de Els cuja soma e Soma
%----------------------------------------------------------------
permutacoes_soma(N, Els, Soma, Perms) :-
    combinacoes_soma(N, Els, Soma, Combs),
    findall(Perm, (member(Comb, Combs),
                 permutation(Comb, Perm)), PermsAux),
    sort(PermsAux, Perms).

%----------------------------------------------------------------
%                    Struct espaco
% Cada espaco e representado por uma estrutura
% espaco(soma, variaveis), em que a soma e a soma das
% posicoes do espaco e variaveis e a lista de variaveis do
% espaco
% ---------------------------------------------------------------
% construtor
cria_espaco(Soma, LVars, espaco(Soma, LVars)).

% seletores
soma_espaco(espaco(Soma, _), Soma).
variaveis_espaco(espaco(_, LVars), LVars).

%----------------------------------------------------------------
%             espaco_fila(Fila, Esp, H_V)
% Fila e uma fila (linha ou coluna) de um puzzle e H_V e
% um dos atomos h ou v, conforme se trate de uma fila
% horizontal ou vertical, respetivamente, significa que
% Esp e um espaco de Fila
%----------------------------------------------------------------
espaco_fila([], _, _) :- !, fail.

espaco_fila(Fila, Esp, h) :-
    Ind is 1, !,
    espaco_fila(Fila, Esp, Ind, 0, []).

espaco_fila(Fila, Esp, v) :-
    Ind is 0, !,
    espaco_fila(Fila, Esp, Ind, 0, []).

espaco_fila([], _, _, _, []) :- !, fail.

espaco_fila([], Esp, _, Soma, Vars) :-
    Vars \== [],
    cria_espaco(Soma, Vars, Esp), !.

espaco_fila([Current | _], Esp, _, Soma, Vars) :-
    is_list(Current),
    Vars \== [],
    cria_espaco(Soma, Vars, Esp).

espaco_fila([Current | NewFila], Esp, Ind, Soma, Vars) :-
    \+is_list(Current), !,
    append(Vars, [Current], NewVars),
    espaco_fila(NewFila, Esp, Ind, Soma, NewVars).

espaco_fila([Current | NewFila], Esp, Ind, _, _) :-
    is_list(Current),
    nth0(Ind, Current, Soma),
    espaco_fila(NewFila, Esp, Ind, Soma, []).

%----------------------------------------------------------------
%              espacos_fila(H_V, Fila, Esps)
% Fila e uma fila (linha ou coluna) de uma grelha e H_V e
% um dos atomos h ou v, significa que Esps e a lista de
% todos os espacos de Fila, da esquerda para a direita
%----------------------------------------------------------------
espacos_fila(H_V, Fila, Esps) :-
    bagof(Esp, espaco_fila(Fila, Esp, H_V), Esps), !.

espacos_fila(_, _, []).

%----------------------------------------------------------------
%             espacos_puzzle(Puzzle, Espacos)
% Puzzle e um puzzle, significa que espacos e a lista de
% espacos de Puzzle
%----------------------------------------------------------------
espacos_puzzle([Linha | Filas], Esps) :-
    espacos_puzzle([Linha | Filas], Esps, Filas, [], h).

espacos_puzzle(_, Esps, [], Esps, v) :- !.

espacos_puzzle(Puzzle, Esps, [], EspsAux, h) :-
    mat_transposta(Puzzle, [_ | NewFilas]), !,
    espacos_puzzle(Puzzle, Esps, NewFilas, EspsAux, v).

espacos_puzzle(Puzzle, Esps, [Fila | RFilas], EspsAux, H_V) :-
    espacos_fila(H_V, Fila, EspsFila),
    append(EspsAux, EspsFila, NewEspsAux),
    espacos_puzzle(Puzzle, Esps, RFilas, NewEspsAux, H_V).

%----------------------------------------------------------------
%      espacos_com_posicoes_comuns(Espacos, Esp, Esps_com)
% Espacos e uma lista de espacos e Esp e um espaco, significa
% que Esps_com e a lista de espacos com variaveis em comum
% com Esp, exceptuando Esp
%----------------------------------------------------------------
pertence(_, []) :- !, fail.
pertence(E, [P | _]) :- E == P, !.
pertence(E, [_ | R]) :- pertence(E, R).

intersecao(_, [], []) :- !.
intersecao(L1, [P | L2], I) :-
    \+ pertence(P, L1), !,
    intersecao(L1, L2, I).
intersecao(L1, [P | L2], [P | I]) :-
    intersecao(L1, L2, I).

espacos_com_posicoes_comuns(Esps, Esp, Esps_com) :-
    variaveis_espaco(Esp, VarsEsp),

    espacos_com_posicoes_comuns(Esps, Esp, Esps_com,
                                [], VarsEsp).

espacos_com_posicoes_comuns([], _, Esps_com, Esps_com, _) :- !.

espacos_com_posicoes_comuns([EPoss | REsps],
                            Esp, Esps_com, Aux, VarsEsp) :-
    EPoss \== Esp,
    variaveis_espaco(EPoss, VarsPoss),
    intersecao(VarsEsp, VarsPoss, VarsCom),
    VarsCom \== [], !,
    append(Aux, [EPoss], NewAux),
    espacos_com_posicoes_comuns(REsps, Esp, Esps_com,
                                NewAux, VarsEsp).

espacos_com_posicoes_comuns([_| REsps], Esp,
                            Esps_com, Aux, VarsEsp):-
    espacos_com_posicoes_comuns(REsps, Esp,
                                Esps_com, Aux, VarsEsp).

%----------------------------------------------------------------
%       permutacoes_soma_espacos(Espacos, Perm_soma)
% Espacos e uma lista de espacos, significa que Perms_soma e
% a lista de listas de 2 elementos, em que o primeiro
% elemento e um espaco de Espacos e o segundo e a lista
% ordenada de permutacoes cuja soma e igual a soma do espaco
%----------------------------------------------------------------
permutacoes_soma_espacos(Esps, Perm_soma) :-
    permutacoes_soma_espacos(Esps, Perm_soma, [],
                            [1,2,3,4,5,6,7,8,9]).

permutacoes_soma_espacos([], Perm_soma, Perm_soma,_) :- !.

permutacoes_soma_espacos([Esp|REsps],Perm_soma,PermAux,Els) :-
    soma_espaco(Esp, Soma),
    variaveis_espaco(Esp, Vars),
    length(Vars, N),
    permutacoes_soma(N, Els, Soma, Perms),
    append(PermAux, [[Esp, Perms]], NPermAux),
    permutacoes_soma_espacos(REsps, Perm_soma, NPermAux, Els).

%----------------------------------------------------------------
%   permutacao_possivel_espaco(Perm, Esp, Espacos, Perm_soma)
% Perm e uma permutacao, Esp e um espaco, Espacos e uma lista
% de espacos, e Perm_soma e uma lista de listas, significa que
% Perm e uma permutacao possivel para o Esp
%----------------------------------------------------------------
permutacao_possivel_espaco(Perm, Esp, Esps, PS) :-
    espacos_com_posicoes_comuns(Esps, Esp, EspsCom),
    permutacoes_espaco(Esp, PS, Perms),
    variaveis_espaco(Esp, Vars),
    length(Vars, Len),
    permutacao_possivel_espaco(Perm, PS, EspsCom, EspsCom,
                               Vars, 0, Len, Perms).

permutacao_possivel_espaco(_, _, _, _, _, _, _, []) :- !, fail.

permutacao_possivel_espaco(PossPerm, _, _, _, _, Len, Len,
                           [PossPerm | _]).

permutacao_possivel_espaco(Perm, PS, EspsCom, _, Vars,
                           Len, Len, [_ | NewPerms]) :- !,
     permutacao_possivel_espaco(Perm, PS, EspsCom, EspsCom,
                                Vars, 0, Len, NewPerms).

permutacao_possivel_espaco(Perm, PS, EspsCom, [], Vars,
                           _, Len, [_ | NewPerms]) :- !,
    permutacao_possivel_espaco(Perm, PS, EspsCom, EspsCom,
                               Vars, 0, Len, NewPerms).

permutacao_possivel_espaco(Perm, PS, EspsCom,[EspCom|REspsCom],
                           Vars, Ind, Len, [PossPerm | R]):-
    variaveis_espaco(EspCom, VarsEspCom),
    nth0(Ind, Vars, Var),
    calc_ind(Var, VarsEspCom, IndCom),
    nth0(Ind, PossPerm, N),
    existe_permutacao_de_espcom(IndCom, N, EspCom, PS), !,
    NewInd is Ind + 1,
    permutacao_possivel_espaco(Perm, PS, EspsCom, REspsCom,
                               Vars, NewInd, Len,[PossPerm|R]).

permutacao_possivel_espaco(Perm, PS, EspsCom, [_ | NewEspsCom],
                           Vars, Ind, Len, Perms) :-
    permutacao_possivel_espaco(Perm, PS, EspsCom, NewEspsCom,
                               Vars, Ind, Len, Perms).

permutacoes_espaco(_, [], []) :- !.

permutacoes_espaco(Esp, [L | _], Perms) :-
    nth0(0, L, EspPoss),
    Esp == EspPoss, !,
    nth0(1, L, Perms).

permutacoes_espaco(Esp, [L | RPerm_soma], Perms) :-
    nth0(0, L, EspPoss),
    Esp \== EspPoss,
    permutacoes_espaco(Esp, RPerm_soma, Perms).

%
calc_ind(Var, Vars, Ind) :-
    calc_ind(Var, Vars, Ind, 0).

calc_ind(_, [], _, _) :- !, fail.
calc_ind(Var, [Var2 | _], Ind, IndAux) :-
    Var == Var2, !,
    Ind = IndAux.

calc_ind(Var, [Var2 | NewVars], Ind, IndAux) :-
    Var \== Var2,
    NewIndAux is IndAux + 1,
    calc_ind(Var, NewVars, Ind, NewIndAux).

%
existe_permutacao_de_espcom(Ind, N, EspCom, Perms_soma) :-
    permutacoes_espaco(EspCom, Perms_soma, Perms),
    existe_permutacao_de_espcom(Ind, N, Perms).

existe_permutacao_de_espcom(_, _, []) :- fail, !.

existe_permutacao_de_espcom(Ind, N, [Perm | NewPerms]):-
    nth0(Ind, Perm, PossN),
    N =:= PossN ->
    !;
    existe_permutacao_de_espcom(Ind, N, NewPerms).

%----------------------------------------------------------------
%               permutacoes_possiveis_espaco
%          (Espacos, Perms_soma, Esp, Perms_poss)
% Significa que Perms_poss e uma lista de 2 elementos em que o
% primeiro e uma de variaveis de Esp e o segundo e a lista
% ordenadas de permutacoes possiveis para o espaco Esp
%----------------------------------------------------------------
permutacoes_possiveis_espaco(Esps, PS, Esp, [Vars, Perms]) :-
    variaveis_espaco(Esp, Vars),
    bagof(Perm, permutacao_possivel_espaco(Perm,Esp,Esps,PS),
          Perms).

%----------------------------------------------------------------
%   permutacoes_possiveis_espacos(Espacos, Perm_poss_esps)
% Significa que Perm_poss_esps e a lista de permutacoes possiveis
%----------------------------------------------------------------
permutacoes_possiveis_espacos(Esps, Perm_poss_esps) :-
    permutacoes_soma_espacos(Esps, Perms_soma),
    permutacoes_possiveis_espacos(Esps, Perm_poss_esps,
                                  Perms_soma, Esps, []).

permutacoes_possiveis_espacos(_, Perm_poss_esps,
                              _, [], Perm_poss_esps) :- !.

permutacoes_possiveis_espacos(Esps, Perm_poss_esps, Perms_soma,
                              [Esp | NewEsps], AuxPPE) :-
    permutacoes_possiveis_espaco(Esps, Perms_soma, Esp,PermsP),
    append(AuxPPE, [PermsP], NewAuxPPE),
    permutacoes_possiveis_espacos(Esps, Perm_poss_esps,
                                  Perms_soma, NewEsps,
                                  NewAuxPPE).
%----------------------------------------------------------------
%                     Struct Pares
% Representada por (Pos, Numero), em que Pos indica a posicao
% numa permutacao e Numero e um valor
%----------------------------------------------------------------
% construtor
cria_par(Pos, N, (Pos, N)).

% seletores
posicao_par((Pos, _), Pos).
numero_par((_, N), N).

%----------------------------------------------------------------
%      numeros_comuns(Lst_Perms, Numeros_comuns)
% Lst_Perms e uma lista de permutacoes, significa que
% Numeros_comuns e uma lista de pares (pos, numero),
% significando que todas as listas de Lst_Perms contem o
% numero numero na posicao pos
% ---------------------------------------------------------------
numeros_comuns([Perm | RLPerms], NCom) :-
    length(Perm, Len),
    numeros_comuns([Perm | RLPerms], [Perm | RLPerms],
                   NCom, [], Len, 0, 0).

numeros_comuns(_, _, NCom, NCom, Len, Len, _) :- !.

numeros_comuns([Perm | RLPerms], _, NCom,
               NComAux, Len, Ind, 0) :- !,
    nth0(Ind, Perm, NewN),
    numeros_comuns([Perm | RLPerms], [Perm | RLPerms], NCom,
                   NComAux, Len, Ind, NewN).

numeros_comuns(LPerms,[],NCom,NComAux,Len,Ind,N):- !,
    NewInd is Ind + 1,
    cria_par(NewInd, N, Par),
    append(NComAux, [Par], NewNComAux),
    numeros_comuns(LPerms, LPerms, NCom,
                   NewNComAux, Len, NewInd, 0).

numeros_comuns(LPerms, [Perm | RLPerms], NCom,
               NComAux, Len, Ind, N) :-
    nth0(Ind, Perm, NPoss),
    N =:= NPoss, !,
    numeros_comuns(LPerms, RLPerms, NCom, NComAux, Len, Ind, N).

numeros_comuns(LPerms, [Perm |_], NCom, NComAux, Len, Ind, N) :-
    nth0(Ind, Perm, NPoss),
    N =\= NPoss,
    NewInd is Ind + 1,
    numeros_comuns(LPerms, LPerms, NCom,
                   NComAux, Len, NewInd, 0).

%----------------------------------------------------------------
%              atribui_comuns(Perms_Possiveis)
% Perms_Possiveis e uma lista de permutacoes possiveis,
% atualiza esta lista atribuindo a cada espaco numeros comuns
% a todas as permutacoes possiveis para esse espaco
%----------------------------------------------------------------
atribui_comuns([]) :- !.

atribui_comuns([Perm_Poss | RPerms_Poss]) :-
    nth0(0, Perm_Poss, Vars),
    nth0(1, Perm_Poss, Perms),
    numeros_comuns(Perms, NComs),
    atribui_comuns_aux(Vars, NComs),
    atribui_comuns(RPerms_Poss).

%
atribui_comuns_aux(_, []) :- !.

atribui_comuns_aux(Vars, [Par | RNComs]) :-
    posicao_par(Par, Pos),
    numero_par(Par, N),
    nth1(Pos, Vars, Var),
    Var = N,
    atribui_comuns_aux(Vars, RNComs).

%----------------------------------------------------------------
%  retira_impossiveis(Perms_Possivies, Novas_Perms_Possiveis)
%  Perms_Possiveis e uma lista de permutacoes, significa que
%  Novas_Perms_Possiveis e o resultadode tirar permutacoes
%  impossiveis
%----------------------------------------------------------------
retira_impossiveis(Perms_Poss, NPerms_Poss) :-
    retira_impossiveis(Perms_Poss, NPerms_Poss, []).

retira_impossiveis([], NPerms_Poss, NPerms_Poss) :- !.

retira_impossiveis([Perm_Poss|RPerms_Poss], NPerms_Poss, Aux) :-
    nth0(0, Perm_Poss, Vars),
    nth0(1, Perm_Poss, Perms),
    length(Vars, Len),
    retira_impossiveis_aux(Vars, Perms, Len, NewPerms),
    append(Aux, [[Vars, NewPerms]], NewAux),
    retira_impossiveis(RPerms_Poss, NPerms_Poss, NewAux).

%
retira_impossiveis_aux(Vars, Perms, Len, NewPerms) :-
    retira_impossiveis_aux(Vars, Perms, Len, 0, NewPerms, []).

retira_impossiveis_aux(_, [], _, _, NewPerms, NewPerms) :- !.

retira_impossiveis_aux(Vars, [Perm | RPerms], Len, Len,
                       NewPerms, Aux) :- !,
    append(Aux, [Perm], NewAux),
    retira_impossiveis_aux(Vars, RPerms, Len, 0, NewPerms, NewAux).

retira_impossiveis_aux(Vars, Perms, Len, Ind, NewPerms, Aux) :-
    nth0(Ind, Vars, Var),
    \+number(Var), !,
    NInd is Ind + 1,
    retira_impossiveis_aux(Vars, Perms, Len, NInd, NewPerms, Aux).

retira_impossiveis_aux(Vars, Perms, Len, Ind, NewPerms, Aux) :-
    nth0(Ind, Vars, Var),
    number(Var),
    nth0(0, Perms, Perm),
    nth0(Ind, Perm, N),
    Var =:= N, !,
    NInd is Ind + 1,
    retira_impossiveis_aux(Vars, Perms, Len, NInd, NewPerms, Aux).

retira_impossiveis_aux(Vars, [_|RPerms], Len, _, NewPerms, Aux) :-
    retira_impossiveis_aux(Vars, RPerms, Len, 0, NewPerms, Aux).

%----------------------------------------------------------------
%      simplifica(Perms_Possiveis, Novas_Perms_Possiveis)
% Perms_Possiveis e uma lista de permutacoes possiveis, significa
% que Novas_Perms_Possiveis e o resultado de simplificar
% Perms_Possiveis os predicados atribui_comuns e
% retira_impossiveis ate nao haver alteracoes
%----------------------------------------------------------------
simplifica(Perms_Poss, NPerms_Poss) :-
    atribui_comuns(Perms_Poss),
    retira_impossiveis(Perms_Poss, NoImp),
    simplifica(Perms_Poss, NPerms_Poss, NoImp).

simplifica(Perms_Poss, Perms_Poss, Perms_Poss) :- !.

simplifica(Perms_Poss, NPerms_Poss, NoImp) :-
    Perms_Poss \== NoImp, !,
    atribui_comuns(NoImp),
    retira_impossiveis(NoImp, NewNoImp),
    simplifica(NoImp, NPerms_Poss, NewNoImp).

%----------------------------------------------------------------
%             inicializa(Puzzle, Perms_Possiveis)
% Significa que Perms_Possiveis e a lista de permutacoes
% possiveis simplificada para Puzzle
%----------------------------------------------------------------
inicializa(Puzzle, Perms_Poss) :-
    espacos_puzzle(Puzzle, Esps),
    permutacoes_possiveis_espacos(Esps, Perms),
    simplifica(Perms, Perms_Poss).

%----------------------------------------------------------------
%     escolhe_menos_alternativas(Perms_Possiveis, Escolha)
% Significa que Escolha e o elemento de Perms_Possiveis escolhido
% , se todos os espacos em Perms_Possiveis tiverem associadas
% listas de permutacoes unitarias o predicado devolve "falso"
%----------------------------------------------------------------
escolhe_menos_alternativas(PermsPoss, E) :-
    escolhe_menos_alternativas(PermsPoss, E, _, 0).

escolhe_menos_alternativas([], _, _, 0) :- !, fail.

escolhe_menos_alternativas([], E, E, _) :- !.

escolhe_menos_alternativas([PermP | RPermsP], E, _, 0):-
    nth0(1, PermP, LPerms),
    length(LPerms, Len),
    Len =\= 1, !,
    NewMin is Len,
    escolhe_menos_alternativas(RPermsP, E, PermP, NewMin).

escolhe_menos_alternativas([PermP | RPermsP], E, _, 0):- !,
    nth0(1, PermP, LPerms),
    length(LPerms, Len),
    Len =:= 1,
    escolhe_menos_alternativas(RPermsP, E, _, 0).

escolhe_menos_alternativas([PermP | RPermsP], E, _, Min):-
    nth0(1, PermP, LPerms),
    length(LPerms, Len),
    Len < Min,
    Len =\= 1, !,
    NewMin is Len,
    escolhe_menos_alternativas(RPermsP, E, PermP, NewMin).

escolhe_menos_alternativas([PermP | RPermsP], E, EPoss, Min):-
    nth0(1, PermP, LPerms),
    length(LPerms, Len),
    \+Len < Min, !,
    escolhe_menos_alternativas(RPermsP, E, EPoss, Min).

escolhe_menos_alternativas([PermP | RPermsP], E, EPoss, Min):-
    nth0(1, PermP, LPerms),
    length(LPerms, Len),
    Len =:= 1,
    escolhe_menos_alternativas(RPermsP, E, EPoss, Min).


%----------------------------------------------------------------
%   experimenta_perm(Escolha, Perms_Possiveis, Novas_Perms_Poss)
% 1. Sendo Esp e Lst_Perms o espaco e a lista de permutacoes
% de Escolha, escolhe uma permutacao de Lst_Perms, Perm
% 2. Unifica Esp com Perm
% 3. Novas_Perms_Possiveis e o resultado de substituir, em
% Perms_Possiveis, o elemento Escolha pelo elemento [Esp,[Perm]]
%----------------------------------------------------------------
experimenta_perm(E, PermsPoss, NPermsPoss) :-
    nth0(0, E, Esp),
    nth0(1, E, LPerms),
    member(Perm, LPerms),
    substitui_perm(PermsPoss, Esp, Perm, NPermsPoss).

substitui_perm(PermsPoss, Esp, Perm, NPermsPoss) :-
    substitui_perm(PermsPoss, Esp, Perm, NPermsPoss, []).

substitui_perm([], _, _, NPermPoss, NPermPoss) :- !.

substitui_perm([PermP|RPermsPoss], Esp, Perm, NPermsPoss, Aux) :-
    nth0(0, PermP, EspPoss),
    EspPoss == Esp, !,
    Esp = Perm,
    append(Aux, [[Esp, [Perm]]], NewAux),
    substitui_perm(RPermsPoss, Esp, Perm, NPermsPoss, NewAux).

substitui_perm([PermP|RPermsPoss], Esp, Perm, NPermsPoss, Aux) :-
    nth0(0, PermP, EspPoss),
    EspPoss \== Esp,
    append(Aux, [PermP], NewAux),
    substitui_perm(RPermsPoss, Esp, Perm, NPermsPoss, NewAux).

%----------------------------------------------------------------
%     resolve_aux(Perms_Possiveis, Novas_Perms_Possiveis)
% Significa que Novas_Perms_Possiveis e o resultado de aplicar
% o algoritmo descrito na Seccao 2.2 a Perms_Possiveis
%----------------------------------------------------------------
resolve_aux(PermsPoss, PermsPoss) :-
    n_perm_um(PermsPoss), !.

resolve_aux(PermsPoss, NPermsPoss) :-
    escolhe_menos_alternativas(PermsPoss, E),
    experimenta_perm(E, PermsPoss, ExpPermsPoss),
    simplifica(ExpPermsPoss, SpPermsPoss),
    resolve_aux(SpPermsPoss, NPermsPoss).

%
n_perm_um([]).

n_perm_um([PermPoss | RPermsPoss]) :-
    nth0(1, PermPoss, Perms),
    length(Perms, Len),
    Len =:= 1, !,
    n_perm_um(RPermsPoss).

n_perm_um([PermPoss | _]) :-
    nth0(1, PermPoss, Perms),
    length(Perms, Len),
    Len =\= 1, fail.

%----------------------------------------------------------------
%                       resolve(Puzzle)
% Resolve o Puzzle, isto e, apos a sua invocacaoa grelha de
% Puzzle tem todas as variaveis substituidas por numeros que
% respeitam as restricoes Puzzle
%----------------------------------------------------------------
resolve(Puzzle) :-
    inicializa(Puzzle, PermsPoss),
    resolve_aux(PermsPoss, _).

