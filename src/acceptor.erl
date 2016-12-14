-module(acceptor).
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
        case order:gr(Round, Promised) of %PL: GAPS FILLED
            true -> %PL: when current proposers Number bigger than the previously voted ....
              %PL: then tell the proposer, you give him the promise, the original Number you respond to, the other
              %PL:  Number it was compared against and which of the values was chosen
              Proposer ! {promise, Round, Voted, Value}, %PL: GAPS FILLED
                % Update gui
                if
                    Value == na ->
                        Colour = {0,0,0};
                    true ->
                        Colour = Value
                end,
    io:format("[Acceptor ~w] Phase 1: promised ~w voted ~w colour ~w~n",
                [Name, Round, Voted, Value]), %PL: GAPS FILLED
                PanelId ! {updateAcc, "Voted: " 
                        ++ io_lib:format("~p", [Voted]), "Promised: " 
                        ++ io_lib:format("~p", [Promised]), Colour}, %PL: GAPS FILLED
                acceptor(Name, Promised, Voted, Value, PanelId); %PL: GAPS FILLED
            false -> %PL: otherwise ignore or optionally send sorry message back to proposer
                Proposer ! {sorry, {prepare, Voted}}, %PL: GAPS FILLED
                acceptor(Name, Promised, Voted, Value, PanelId) %PL: GAPS FILLED
        end;
    {accept, Proposer, Round, Proposal} ->
        case order:goe(Round, Promised) of %PL: GAPS FILLED
            true ->
                Proposer ! {vote, Proposal}, %PL: GAPS FILLED
              %PL: update gui only if there was a change
              case order:goe(Round, Promised) of %PL: GAPS FILLED NOT SURE HERE
                    true ->
                        % Update gui
    io:format("[Acceptor ~w] Phase 2: promised ~w voted ~w colour ~w~n",
                        [Name, Promised, Voted, Value]),
                        PanelId ! {updateAcc, "Voted: " 
                                ++ io_lib:format("~p", [Voted]), "Promised: "  %PL: GAPS FILLED
                                ++ io_lib:format("~p", [Promised]), Promised}, %PL: GAPS FILLED
                        acceptor(Name, Promised, Voted, Value, PanelId); %PL: GAPS FILLED
                    false ->
                        % Update gui
    io:format("[Acceptor ~w] Phase 2: promised ~w voted ~w colour ~w~n",
                        [Name, Promised, Voted, Value]), %PL: GAPS FILLED
                        PanelId ! {updateAcc, "Voted: " 
                                ++ io_lib:format("~p", [Voted]), "Promised: "  %PL: GAPS FILLED
                                ++ io_lib:format("~p", [Promised]), Promised}, %PL: GAPS FILLED
                        acceptor(Name, Promised, Voted, Value, PanelId) %PL: GAPS FILLED
                end;                            
            false ->
                Proposer ! {sorry, {accept, Voted}}, %PL: GAPS FILLED
                acceptor(Name, Promised, Voted, Value, PanelId)
        end;
    stop ->
        PanelId ! stop,
        ok
  end.
