-module(acceptor).
-export([start/2]).

start(Name, PanelId) ->
    spawn(fun() -> init(Name, PanelId) end).
        
init(Name, PanelId) ->
    Promised = order:null(), %initialized at 0 because not promised anything yet
    Voted = order:null(), %initialized at 0 because not voted for anything yet
    Value = na, %initialized at "null" because not voted for anything yet
    acceptor(Name, Promised, Voted, Value, PanelId).
%Name = name of acceptor
%Promised = acceptor promised not to accept a ballot below this number
%Voted = highest ballot number so far
%Value = value of highest ballot number so far
%PanelID = corresponding GUI PID

acceptor(Name, Promised, Voted, Value, PanelId) ->
  receive %The acceptor is waiting for two types of messages: prepare requests and accept requests.
    {prepare, Proposer, Round} -> %A prepare request from process Proposer, {prepare Proposer, Round}, will result in a
      % promise, if we have not made any promise that prevents us to make such a promise. In order to check this, the Round
      % number of the prepare request must be compared with the highest promise
      % already given (Promised).
        case order:gr(Round, Promised) of
            true -> %PL: when current proposers Number bigger than the previously voted ....
              %PL: then tell the proposer, you give him the promise, the original Number you respond to, the other
              %PL:  Number it was compared against and which of the values was chosen
              Proposer ! {promise, Round, Voted, Value}, %If the Round number is higher, the acceptor can return a promise, {promise, Round, Voted, Value}.
                % Update gui
                if
                    Value == na ->
                        Colour = {0,0,0}; %Black color corresponds to no value voted yet.
                    true ->
                        Colour = Value
                end,
    io:format("[Acceptor ~w] Phase 1: promised ~w voted ~w colour ~w~n",
                [Name, Round, Voted, Value]), %PL: GAPS FILLED
                PanelId ! {updateAcc, "Voted: " 
                        ++ io_lib:format("~p", [Voted]), "Promised: " 
                        ++ io_lib:format("~p", [Promised]), Colour}, %PL: GAPS FILLED
                acceptor(Name, Promised, Voted, Value, PanelId); %PL: GAPS FILLED
            false -> %PL: you could ignore this but optionally send sorry message back to proposer
              %To help the proposer we should inform it that we have promised not to vote in the round requested by the proposer
                Proposer ! {sorry, {prepare, Round}}, %PL: GAPS FILLED
                acceptor(Name, Promised, Voted, Value, PanelId) %PL: GAPS FILLED
        end;

    % An accept request, sent by a Proposer when it has received promises from a majority, also has two outcomes; either
    % we can vote the request and then save the value that comes in the ballot (if the ballot number is higher than the
    % current maximum one) or we have a promise that prevents us from voting the request. Note that we do not change our
    % promise just because we vote for a new value.
    {accept, Proposer, Round, Proposal} ->
        case order:goe(Proposal, Promised) of %PL: GAPS FILLED  when it has received promises from a majority
            true ->
                Proposer ! {vote, {Round}}, %PL: GAPS FILLED. vote the request ... and then save the value
              %PL: update gui only if there was a change
              case order:goe(Proposal, Voted) of %PL: GAPS FILLED. if the ballot number is higher/equals the current maximum one
                    true ->
                        % Update gui
    io:format("[Acceptor ~w] Phase 2: promised ~w voted ~w colour ~w~n", [Name, Proposal, Proposal, Value]),
                        PanelId ! {updateAcc, "Voted: " 
                                ++ io_lib:format("~p", [Proposal]), "Promised: "  %PL: GAPS FILLED
                                ++ io_lib:format("~p", [Proposal]), Proposal}, %PL: GAPS FILLED
                        acceptor(Name, Promised, Voted, Value, PanelId); %PL: GAPS FILLED. save the value that comes in the ballot
                    false ->
                        % Update gui
    io:format("[Acceptor ~w] Phase 2: promised ~w voted ~w colour ~w~n", [Name, Promised, Voted, Value]), %PL: GAPS FILLED
                        PanelId ! {updateAcc, "Voted: " 
                                ++ io_lib:format("~p", [Voted]), "Promised: "  %PL: GAPS FILLED
                                ++ io_lib:format("~p", [Promised]), Promised}, %PL: GAPS FILLED
                        acceptor(Name, Promised, Voted, Value, PanelId) %PL: GAPS FILLED
                end;                            
            false ->
                % Again, if we cannot vote the request we could simply ignore the message but it is polite
                % to inform the Proposer.
                Proposer ! {sorry, {accept, Voted}}, %PL: GAPS FILLED useless, there is no handler for sorry
                acceptor(Name, Promised, Voted, Value, PanelId)
        end;
    stop ->
        io:format("Stop Acceptor ~w~n", [Name]),
        PanelId ! stop,
        ok
  end.
