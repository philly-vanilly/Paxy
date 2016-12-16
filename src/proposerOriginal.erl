-module(proposerOriginal).
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
    case collect(Quorum, Round, MaxVoted, Proposal) of
        {accepted, Value} ->
            io:format("[Proposer ~w] Phase 2: round ~w proposal ~w (was ~w)~n", [Name, Round, Value, Proposal]),
            PanelId ! {updateProp, "Round: " 
                    ++ io_lib:format("~p", [Round]), "Proposal: "
                    ++ io_lib:format("~p", [Value]), Value},
            accept(Round, Value, Acceptors),
            case vote(Quorum, Round) of
                ok -> % in case result of vote is an ok, return decision
                    {ok, Value};
                abort ->
                    abort % in case result of vote is an abort, propagate abort
            end;
        abort ->
            abort % propagate abort
    end.

% Two collect()-functions for iterating a number of acceptors
collect(0, _, _, Proposal) -> % when N reaches 0, the needed quorum of proposers has been asked, so accept proposal
    {accepted, Proposal};
collect(N, Round, MaxVoted, Proposal) -> % decrease N upon receiving promise
    receive 
        {promise, Round, _, na} ->
            collect(N-1, Round, MaxVoted, Proposal);
        {promise, Round, Voted, Value} ->
            case order:gr(Voted, MaxVoted) of
                true ->
                    collect(N-1, Round, Voted, Value);
                false ->
                    collect(N-1, Round, MaxVoted, Proposal)
            end;
        {promise, _, _,  _} ->
            collect(N-1, Round, MaxVoted, Proposal);
        {sorry, {prepare, Round}} ->
            collect(N, Round, MaxVoted, Proposal);
        {sorry, _} ->
            collect(N, Round, MaxVoted, Proposal)
    after ?timeout ->
            abort
    end.

vote(0, _) -> % counter at 0, stop collecting votes
    ok;
vote(N, Round) -> % decrease counter N with every vote
    receive
        {vote, Round} ->
            vote(N-1, Round);
        {vote, _} ->
            vote(N-1, Round);
        {sorry, {accept, Round}} ->
            vote(N-1, Round);
        {sorry, _} ->
            vote(N-1, Round)
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

send(Name, Message) ->
    Name ! Message.
