-module(acceptor).
-export([start/2]).

-define(delay, 200).
-define(drop, 2000).

% entry point of the module, creating a new process representing an instance of this module (like object in OOP)
start(Name, PanelId) ->
    spawn(fun() -> init(Name, PanelId) end).

% constructior function initializing with default values and parameters and starting the loop-function of the process
init(Name, PanelId) ->
  pers:open(Name),
  case PanelId of
    na ->
      io:format("Acceptor ~w crashed and restarted~n", [Name]),
      {Promised, Voted, Value, PanelId} = pers:read(Name);
    _->
      % initializing everything at 0 or not-available because there is no promise or vote (with corresponding value) at the
      % beginning
      Promised = order:null(),
      Voted = order:null(),
      Value = na
  end,
  io:format("Acceptor ~w storing data~n", [Name]),
  pers:store(Name, Promised, Voted, Value, PanelId), %store(Name, Pr, Vt, Ac, Pn)-> ...
  acceptor(Name, Promised, Voted, Value, PanelId).

% indefinitely running loop-function of the process which makes it alive.
% after initialization listens to incoming messages for the process it belongs to. those messages control the function
% and thus the process. at the end of control-flow for all messages (except the stop/terminate-call) is a recursive call
% of the function, to
%     (1) update "instance variables"  with recursive function-call with updated parameters
%     (2) then start listening for the next message to arrive.
%
% ============ FUNCTION PARAMETERS: =================
% Name = name of acceptor = "Acceptor a", ...
% Promised = acceptor promised not to accept a ballot below this number = (N, Id)
% Voted = highest ballot number so far = (N, Id)
% Value = String-value of highest ballot number so far = RED/BLUE/... Colour = RGB-value
% PanelID = corresponding GUI PID
acceptor(Name, Promised, Voted, Value, PanelId) ->
  io:format("Acceptor ~w called with parameters: Promised = ~w, Voted = ~w, Value = ~w~n", [Name, Promised, Voted, Value]),

  receive
    % A message can be a single atom (defining type of message) or a list, with one of the elements (typically first one)
    % being the atom-identifier.
    % The acceptor is waiting for two types of messages: prepare requests and accept requests (and stop-process-call)

    % ============ MESSAGE PARAMETERS: =================
    % Proposer = PID of proposer that sent the message
    % Round = proposer asks not to vote for anything smaller than that number = (N, Id)
    {prepare, Proposer, Round} ->
      R = rand:uniform(?delay), % delay of random time between 1 and ?delay ms
      timer:sleep(R),

      io:format("Acceptor ~w received prepare-message from Proposer = ~w at Round = ~w~n", [Name, Proposer, Round]),
      % A prepare request from one Proposer will result in a promise, if there is no promise with an equal or higher
      % number, i.e. for (N_1, Id_1) and (N_2, Id_2), either N2>N1 or the prepare-message gets ignored/sorry-replied
      case order:gr(Round, Promised) of
        true ->
          io:format("Acceptor ~w: Round ~w > last Promise ~w, make promise!~n", [Name, Round, Promised]),

          case rand:uniform(?drop) of
            ?drop ->
              io:format("message dropped~n");
            _->
              Proposer ! {promise, Round, Voted, Value}
          end,
%%          Proposer ! {promise, Round, Voted, Value},

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
            pers:store(Name, Round, Voted, Value, PanelId),
            acceptor(Name, Round, Voted, Value, PanelId);
        false ->
          io:format("Acceptor ~w: Round ~w <= last Promise ~w, sorry proposer!~n", [Name, Round, Promised]),
          % In Paxos optional sorry message back to proposer containing Round, so Proposer knows to which original
          % message this is a response to
          Proposer ! {sorry, {prepare, Round}},
          acceptor(Name, Promised, Voted, Value, PanelId)
      end;

    % ============ MESSAGE PARAMETERS: =================
    % Proposer = PID of proposer that sent the message
    % Round = proposer asks to vote for this number (and the corresponding value) = (N, Id)
    % Proposal = proposer asks to vote for this value = RED/BLUE/...
    {accept, Proposer, Round, Proposal} ->
      R = rand:uniform(?delay), % delay of random time between 1 and ?delay ms
      timer:sleep(R),

      io:format("Acceptor ~w received prepare-message from Proposer = ~w at Round = ~w for Proposal = ~w~n", [Name, Proposer, Round, Proposal]),
      case order:goe(Round, Promised) of
        true -> % Vote only if current proposal has same or higher number than previously given promise
          io:format("Acceptor ~w: Round ~w >= last Promise ~w, send vote!~n", [Name, Round, Promised]),

          case rand:uniform(?drop) of
            ?drop ->
              io:format("message dropped~n");
            _->
              Proposer ! {vote, {Round}}
          end,
          %Proposer ! {vote, {Round}},

          case order:goe(Round, Voted) of
            true -> % update gui only if there was a vote (i.e. current vote-request being equal or higher numbered than a previous one)
              io:format("[Acceptor ~w] Phase 2: promised ~w voted ~w colour ~w~n", [Name, Promised, Round, Proposal]),
              PanelId ! {updateAcc, "Voted: "
                      ++ io_lib:format("~p", [Round]), "Promised: "
                      ++ io_lib:format("~p", [Promised]), Proposal},
              pers:store(Name, Promised, Round, Proposal, PanelId),
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
