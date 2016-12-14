-module(proposer).
-export([start/5]).

-define(timeout, 2000).
-define(backoff, 10).

start(Name, Proposal, Acceptors, Sleep, PanelId) ->
    spawn(fun() -> init(Name, Proposal, Acceptors, Sleep, PanelId) end).

init(Name, Proposal, Acceptors, Sleep, PanelId) ->
    timer:sleep(Sleep),
    Round = order:first(Name), %PL: note, Name = Id (used as part of number-tuple), Round = whole tuple = (index, name), see order.erl
    round(Name, ?backoff, Round, Proposal, Acceptors, PanelId).

round(Name, Backoff, Round, Proposal, Acceptors, PanelId) ->
    % Update gui
    io:format("[Proposer ~w] Phase 1: round ~w proposal ~w~n",
    [Name, Round, Proposal]),
    PanelId ! {updateProp, "Round: " 
            ++ io_lib:format("~p", [Round]), "Proposal: "
            ++ io_lib:format("~p", [Proposal]), Proposal},
    case ballot(Name, Round, Proposal, Acceptors, PanelId) of %PL: GAPS FILLED
        {ok, Decision} ->
            io:format("[Proposer ~w] ~w DECIDED ~w in round ~w~n", 
            [Name, Acceptors, Decision, Round]),
            PanelId ! stop,
            {ok, Decision};
        abort ->
            timer:sleep(rand:uniform(Backoff)),
            Next = order:inc(Round), %PL: GAPS FILLED
            round(Name, (2*Backoff), Next, Proposal, Acceptors, PanelId)
    end.

ballot(Name, Round, Proposal, Acceptors, PanelId) ->
    prepare(Round, Acceptors), %PL: GAPS FILLED
    Quorum = (length(Acceptors) div 2) + 1, %PL: GAPS FILLED (calculating needed acceptors for majority)
    MaxVoted = order:null(),
    case collect(Quorum, Round, MaxVoted, Proposal) of %PL: GAPS FILLED; not sure about N=Quorum
        {accepted, Value} ->
            % update gui
            io:format("[Proposer ~w] Phase 2: round ~w proposal ~w (was ~w)~n", 
            [Name, Round, Value, Proposal]),
            PanelId ! {updateProp, "Round: " 
                    ++ io_lib:format("~p", [Round]), "Proposal: "
                    ++ io_lib:format("~p", [Value]), Value},
            accept(Round, Proposal, Acceptors), %PL: GAPS FILLED
            case vote(Value, Round) of %PL: GAPS FILLED
                ok -> %PL: in case result of vote is an ok, return OK+Decision
                    {ok, Value}; %PL: GAPS FILLED
                abort ->
                    abort %PL: in case result of vote is an abort, do nothing
            end;
        abort ->
            abort
    end.

collect(0, _, _, Proposal) -> %PL: when no Acceptors needed to accept, accept immediately
    {accepted, Proposal}; %PL: return accepted-the-proposal-.... to the ballot function which called this
collect(N, Round, MaxVoted, Proposal) -> %PL: when more than 0 Acceptors needed to accept, wait for their replies
    receive 
        {promise, Round, _, na} -> %PL: when no value received,
            collect(N, Round, MaxVoted, Proposal); %PL: GAPS FILLED
        {promise, Round, Voted, Value} ->
            case order:gr(Voted, Round) of %PL: GAPS FILLED
                true ->
                    collect(N, Round, order:inc(MaxVoted), Value); %PL: GAPS FILLED
                false ->
                    collect(N, Round, MaxVoted, Proposal) %PL: GAPS FILLED
            end;
        {promise, _, _,  _} ->
            collect(N, Round, order:inc(MaxVoted), Proposal);
        {sorry, {prepare, Round}} ->
            collect(N, Round, order:inc(MaxVoted), Proposal); %PL: GAPS FILLED
        {sorry, _} ->
            collect(N, Round, order:inc(MaxVoted), Proposal)
    after ?timeout ->
            abort
    end.

vote(0, _) ->
    ok;
vote(N, Round) ->
    receive
        {vote, Round} ->
            vote(N, Round); %PL: GAPS FILLED
        {vote, _} ->
            vote(N, Round);
        {sorry, {accept, Round}} ->
            vote(N, Round); %PL: GAPS FILLED
        {sorry, _} ->
            vote(N, Round)
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
