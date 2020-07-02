isMember(H,[H|_]):-!.
isMember(X,[_|T]):-
    isMember(X,T).

add(F,L,[F|L]).

rev([],R,R).

rev([H|T],R,A):- rev(T,R,[H|A]).

copy(L1,L1).

% return the index of H.
indexOf(H,[H|_],0):-!.
indexOf(X,[H|T],Index):-
    indexOf(X,T,NIndex),!,
    Index is NIndex+1.

%return the value at Index in the list
elementAt(0,[H|_],H):-!.
elementAt(Index , [_|T],X):-
    Index > 0,
    NIndex is Index -1,
    elementAt(NIndex,T,X).


replace([_|T], 0, X, [X|T]).
replace([H|T], I, X, [H|R]):-
    I > -1, NI is I-1, replace(T, NI, X, R), !.
replace(L, _, _, L).

same_length([],[]).
same_length([_|L1],[_|L2]) :- same_length(L1, L2).

remove_duplicates([], []).
remove_duplicates([Head|Tail], Result):-
member(Head, Tail),!,
remove_duplicates(Tail, Result).
remove_duplicates([Head|Tail],[Head|Result]):-
remove_duplicates(Tail, Result).

%---------------------------------------------------
process(p1).
process(p2).
process(p3).

resource(r1).
resource(r2).
resource(r3).

allocated(p1 , r2).
allocated(p2 , r1).
%allocated(p3 , r3).
allocated(p4 , r2).

%requested(p1,r1).
requested(p2,r2).
requested(p1,r1).
requested(p3,r3).

availableInstances([0,5,5]).

getRequsted(P,L):-
    findall(X,requested(P,X),L).

getAllocated(P,L):-
    findall(X,allocated(P,X),L).

getRequestedProcess(Processes):-
    findall(X,requested(X,_),L),
    remove_duplicates(L,Processes).

%--------------------------------------------------------------------
% update Available :- increment the available resources that is
% allocated by the executed process.
% in recursive case, loop over the allocated resources, increment each
% element and delete it from the list in the recursive call
%
updateAvailable([],_,NL,_,R):-
    copy(NL,R),!.% base case..

%recursive case..
updateAvailable([H|Talloc], Resources, AvailList, L , NewAvailList):-
    indexOf(H ,Resources,In), % to know which resource is (r1,r2,...,rn)
    %indexOf(OldAvail,AvailList,In), % to know current available of r
    elementAt(In,AvailList,OldAvail),
    NewAvail is OldAvail + 1,
    replace(AvailList ,In , NewAvail , NL),%increment and store the new value
    updateAvailable(Talloc , Resources , NL, [NewAvail|L]
                   , NewAvailList),!.

% get the allocated resources by the process , and update its values in
% available resources list
executeProcess(Process ,Resources, AvailList , NewAvailList):-
    getAllocated(Process,Allocated),
    updateAvailable(Allocated,Resources ,AvailList, AvailList
                   ,NewAvailList).

%--------------------------------------------------------------------
% can_run :- check if all requested resources for Process ia available
% or not.
% base case:- if we looped over all requested resources.
% recursive case:- loop over the requested list, if there is one element
% that its availavble count less than 0 , then the process can not run,
% else check the second element in the list.
ifCanRun(_,[],_):-!. %base case

ifCanRun( Resources ,[ReqH|ReqT],AvailResources):- % recursive case
    indexOf(ReqH , Resources , In),% to know which resource is (r1,r2,...,rn)
     %indexOf(Avail , AvailResources , In), % to know n.available of r
     elementAt(In , AvailResources  , Avail),
    (Avail > 0)->
    (NAvail is Avail -1,
     replace(AvailResources , In , NAvail , NewAvailResources),
     ifCanRun(Resources , ReqT , NewAvailResources)
     )
    ;!,fail.

% get the requested resources by the process, and check if all these
% resources all available or not.
can_run(Process , Resources , AvailResources):-
    getRequsted(Process,Requested),
    ifCanRun(Resources , Requested , AvailResources),!.
%----------------------------------------------------------------------
%base case.. check if all processes tried to be executed,
%if yes , then the recursive case didnt find any executable process
%then there is a deadlock (fail).
findExecutedProcess([], _ , _ ,_ , _ , _):- fail,!.

% findExecutedProcess :- loop over all Processes untill you find a
% process that can execute
findExecutedProcess([HP|TP], Resources , SafeSeq ,AvailRes , NAvail
, NSafeSeq):-
    (not(member(HP,SafeSeq)),can_run(HP,Resources , AvailRes))
     ->
       (copy([HP|SafeSeq],NSafeSeq),
        executeProcess( HP ,Resources ,AvailRes , NAvail),!)
    ;
    findExecutedProcess(TP,Resources,SafeSeq,AvailRes ,NAvail,NSafeSeq).

%-----------------------------------------------------------------------
% Base case.. check if the safe state contain all procosses that had a
% request
isSafeState(Processes , _ , X , _ ,X):-
    same_length(Processes,X),!.

%loop until there no process can Execute now
% if at one itteration there is no process can execute
%the fail(Deadlock)
isSafeState(Processes , Resources , X ,AvailRes,SafeSeq):-
    findExecutedProcess(Processes , Resources , X
                       ,AvailRes , NAvail , NSafeSeq)
    ->
    isSafeState(Processes,Resources, NSafeSeq,NAvail,SafeSeq);
    fail,!.
%-----------------------------------------------------------------------
% safe_state :- return the safe seq.
safe_state(SafeSeq):-
    getRequestedProcess(Processes),
    %findall(P , process(P) , Processes),
    findall(R , resource(R),Resources),
    availableInstances(AvailRes),
    X = [],
    isSafeState(Processes , Resources , X
               ,AvailRes,Result),
    rev(Result,SafeSeq,[]),!.






