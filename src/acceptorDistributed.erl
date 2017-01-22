-module(acceptorDistributed).
-export([start/2]).

start(Name, PanelId) ->
    spawn(fun() -> init(Name, PanelId) end).

init(Name, PanelId) ->
    Promised = order:null(),
    Voted = order:null(),
    Value = na,
    acceptor(Name, Promised, Voted, Value, PanelId).

acceptor(Name, Promised, Voted, Value, PanelId) ->
  receive
    {prepare, Proposer, Round} ->
      case order:gr(Round, Promised) of
        true ->
          Proposer ! {promise, Round, Voted, Value},
            if % Update gui with new color or default color black when not voted before
              Value == na ->
                Colour = {0,0,0};
              true ->
                Colour = Value
            end,
            io:format("[Acceptor ~w] Phase 1: promised ~w voted ~w colour ~w~n", [Name, Round, Voted, Value]),
            PanelId ! {updateAcc, "Voted: "
                      ++ io_lib:format("~p", [Voted]), "Promised: "
                      ++ io_lib:format("~p", [Round]), Colour},
            acceptor(Name, Round, Voted, Value, PanelId);
        false ->
          io:format("Acceptor ~w: Round ~w <= last Promise ~w, sorry proposer!~n", [Name, Round, Promised]),
          Proposer ! {sorry, {prepare, Round}},
          acceptor(Name, Promised, Voted, Value, PanelId)
      end;

    {accept, Proposer, Round, Proposal} ->
      case order:goe(Round, Promised) of
        true ->
          Proposer ! {vote, {Round}},
          case order:goe(Round, Voted) of
            true -> % update gui only if there was a vote (i.e. current vote-request being equal or higher numbered than a previous one)
              io:format("[Acceptor ~w] Phase 2: promised ~w voted ~w colour ~w~n", [Name, Promised, Round, Proposal]),
              PanelId ! {updateAcc, "Voted: "
                      ++ io_lib:format("~p", [Round]), "Promised: "
                      ++ io_lib:format("~p", [Promised]), Proposal},
              acceptor(Name, Promised, Round, Proposal, PanelId);
            false -> % send old values to gui
              io:format("[Acceptor ~w] Phase 2: promised ~w voted ~w colour ~w~n", [Name, Promised, Voted, Value]),
              PanelId ! {updateAcc, "Voted: "
                      ++ io_lib:format("~p", [Voted]), "Promised: "
                      ++ io_lib:format("~p", [Promised]), Value},
              acceptor(Name, Promised, Voted, Value, PanelId)
            end;
        false ->
          % In Paxos optional sorry message back to proposer containing Round, so Proposer knows to which original
          % message this is a response to
          Proposer ! {sorry, {accept, Round}},
          acceptor(Name, Promised, Voted, Value, PanelId)
      end;

    stop ->
      io:format("Stop Acceptor ~w~n", [Name]),
      PanelId ! stop,
      ok;

    _ -> io:format("Acceptor received unknown message")

  end.
