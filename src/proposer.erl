-module(proposer).
-export([start/5]).

-define(timeout, 2000).
-define(backoff, 10).

% PROPOSER is active part of the application, setting the round (basically "clock") and sending messages to the
% reactive/passive ACCEPTORs which just wait and send responses

% entry point of the module, creating a new process representing an instance of this module (like object in OOP)
start(Name, Proposal, Acceptors, Sleep, PanelId) ->
    spawn(fun() -> init(Name, Proposal, Acceptors, Sleep, PanelId) end).

% constructior function initializing with default values and parameters and starting the loop-function of the process
% - which is round(), all the other functions only get used by round()
init(Name, Proposal, Acceptors, Sleep, PanelId) ->
    timer:sleep(Sleep),
    Round = order:first(Name), % Round initialized at 0
    round(Name, ?backoff, Round, Proposal, Acceptors, PanelId).

% recursive loop-function that sends messages to acceptors, waits for the response(s), handles them, calls itself at the end
%
% ============ FUNCTION PARAMETERS: =================
% Name = name of acceptor = "willard", ...
% Backoff = time constant
% Round = number of an action of the proposer = (N, Id)
% Proposal = proposer asks to vote for this value = RED/BLUE/...
% Acceptors = list of all acceptors
% PanelID = corresponding GUI PID
round(Name, Backoff, Round, Proposal, Acceptors, PanelId) ->
    % Update gui at the beginning of each iteration (and also at change later)
    io:format("[Proposer ~w] Phase 1: round ~w proposal ~w~n", [Name, Round, Proposal]),
    PanelId ! {updateProp, "Round: " 
            ++ io_lib:format("~p", [Round]), "Proposal: "
            ++ io_lib:format("~p", [Proposal]), Proposal},
    case ballot(Name, Round, Proposal, Acceptors, PanelId) of
        {ok, Decision} ->
            io:format("[Proposer ~w] ~w DECIDED ~w in round ~w~n", [Name, Acceptors, Decision, Round]),
            PanelId ! stop,
            {ok, Decision};
        abort ->
            timer:sleep(rand:uniform(Backoff)),
            Next = order:inc(Round),
            round(Name, (2*Backoff), Next, Proposal, Acceptors, PanelId)
    end.

ballot(Name, Round, Proposal, Acceptors, PanelId) ->
    prepare(Round, Acceptors),
    Quorum = (length(Acceptors) div 2) + 1, %calculating needed acceptors for majority
    MaxVoted = order:null(),
    case collect(Quorum, Round, MaxVoted, Proposal, Quorum) of
        {accepted, Value} ->
            io:format("[Proposer ~w] Phase 2: round ~w proposal ~w (was ~w)~n", [Name, Round, Value, Proposal]),
            PanelId ! {updateProp, "Round: " 
                    ++ io_lib:format("~p", [Round]), "Proposal: "
                    ++ io_lib:format("~p", [Value]), Value},
            accept(Round, Value, Acceptors),
            case vote(Quorum, Round, Quorum) of
                ok -> % in case result of vote is an ok, return decision
                    {ok, Value};
                abort ->
                    abort % in case result of vote is an abort, propagate abort
            end;
        abort ->
            abort % propagate abort
    end.

% Two collect()-functions for iterating a number of acceptors
collect(0, _, _, Proposal, _) -> % when N reaches 0, the needed quorum of proposers has been asked, so accept proposal
    io:format("ACCEPTED BASED ON ACCEPTORS~n"),
    {accepted, Proposal};
collect(_,_,_,_,0) -> %end of loop
    io:format("TERMINATED BASED ON SORRIES~n"),
    abort;
collect(N, Round, MaxVoted, Proposal, Sorries) ->
    receive
        {promise, Round, _, na} ->
            collect(N-1, Round, MaxVoted, Proposal, Sorries);
        {promise, Round, Voted, Value} ->
            case order:gr(Voted, MaxVoted) of
                true ->
                    collect(N-1, Round, Voted, Value, Sorries);
                false ->
                    collect(N-1, Round, MaxVoted, Proposal, Sorries)
            end;
        {promise, _, _, _} ->
            collect(N-1, Round, MaxVoted, Proposal, Sorries);
        {sorry, {prepare, Round}} ->
            collect(N, Round, MaxVoted, Proposal, Sorries-1); %on sorry, decrease counter
        {sorry, _} ->
            collect(N, Round, MaxVoted, Proposal, Sorries-1) %on sorry, decrease counter
    after ?timeout ->
            abort
    end.

vote(0, _, _) ->
    io:format("TERMINATED BASED ON VOTES~n"),
    ok;
vote(_, _, 0) -> % sorry-counter at 0
    io:format("TERMINATED BASED ON SORRIES~n"),
    abort;
vote(N, Round, Sorries) ->
    receive
        {vote, Round} ->
            vote(N-1, Round, Sorries);
        {vote, _} ->
            vote(N-1, Round, Sorries);
        {sorry, {accept, Round}} ->
            vote(N-1, Round, Sorries-1);
        {sorry, _} ->
            vote(N-1, Round, Sorries-1)
    after ?timeout ->
            abort
    end.

prepare(Round, Acceptors) ->
    Fun = fun(Acceptor) -> 
        send(Acceptor, {prepare, self(), Round}) 
    end,
    lists:foreach(Fun, Acceptors).

accept(Round, Proposal, Acceptors) ->
    Fun = fun(Acceptor) -> 
        send(Acceptor, {accept, self(), Round, Proposal}) 
    end,
    lists:foreach(Fun, Acceptors).

%%send(Name, Message) ->
%%    Name ! Message.

send(Name, Message) ->
    if is_tuple(Name) -> %remote
        Name ! Message;
        true -> %local
            case whereis(Name) of
                undefined ->
                    down;
                Pid ->
                    Pid ! Message
            end
    end.
