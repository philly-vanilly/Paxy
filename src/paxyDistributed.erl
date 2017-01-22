-module(paxyDistributed).
-export([startProposerServer/1, startAcceptorServer/1, stop/0, stop/1, crash/1]). % exposing interfaces for console. start program with (for example) paxy:start([50,100,200]).

-define(RED, {255,0,0}). % defining colors for GUI. colors represent values to be voted for
-define(BLUE, {0,0,255}).
-define(GREEN, {0,255,0}).
-define(YELLOW, {255,255,0}).
-define(CYAN, {0,255,255}).
-define(MAGENTA, {255,0,255}).


% Entry point of the application. Sleep is a list with the initial sleep time for each proposer
startProposerServer(Sleep) ->
    Node='paxy-acceptors@localhost',
    AccRegister = [{a, Node}, {b, Node},{c, Node},{d, Node},{e, Node},{f, Node},{g, Node},{h, Node},{i, Node},{j, Node}],
    PropInfo = [{kurtz, ?RED}, {kilgore, ?GREEN}, {willard, ?BLUE},{peter, ?YELLOW}, {george, ?CYAN}, {klaus, ?MAGENTA}],
    {gui, Node} ! {reqState, self()}, % send reference of self (own PID) to GUI
    receive
        {reqState, State} ->
            {_, PropIds} = State,
            start_proposers(PropIds, PropInfo, AccRegister, Sleep)
    end,
    true.

startAcceptorServer(Sleep) ->
    AcceptorNames = ["Acceptor a", "Acceptor b", "Acceptor c", "Acceptor d", "Acceptor e"],
    AccRegister = [a, b, c, d, e, f, g, h, i, j],
    ProposerNames = [{"Proposer kurtz", ?RED}, {"Proposer kilgore", ?GREEN}, {"Proposer willard", ?BLUE}],
    register(gui, spawn(fun() -> gui:start(AcceptorNames, ProposerNames) end)), % create new gui process
    gui ! {reqState, self()}, % send reference of self (own PID) to GUI
    receive
        {reqState, State} ->
            {AccIds, _} = State,
            start_acceptors(AccIds, AccRegister)
    end,
    true.


% acceptors don't know each other or any of the proposers (until receiving a message), they know only themselves
start_acceptors(AccIds, AccReg) ->
    case AccIds of
        [] ->% break on list end
            ok;
        [AccId|Rest] ->% process list head first
            [RegName|RegNameRest] = AccReg,
            register(RegName, acceptorDistributed:start(RegName, AccId)),
            start_acceptors(Rest, RegNameRest)
    end.

% proposers don't know each other, but they know all of the acceptors
start_proposers(PropIds, PropInfo, Acceptors, Sleep) ->
    case PropIds of
        [] ->
            ok;
        [PropId|Rest] ->
            [{RegName, Colour}|RestInfo] = PropInfo,
            [FirstSleep|RestSleep] = Sleep,
            proposer:start(RegName, Colour, Acceptors, FirstSleep, PropId),	
            start_proposers(Rest, RestInfo, Acceptors, RestSleep)
        end.

stop() ->
    stop(a),
    stop(b),
    stop(c),
    stop(d),
    stop(e),
    stop(gui).

stop(Name) ->
    case whereis(Name) of
        undefined ->
            ok;
        Pid ->
            pers:delete(Name),
            Pid ! stop
    end.

crash(Name) ->
    case whereis(Name) of
        undefined ->
            ok;
        Pid ->
            pers:open(Name),
            {_, _, _, Pn} = pers:read(Name),
            Pn ! {updateAcc, "Voted: CRASHED", "Promised: CRASHED", {0,0,0}},
            dets:close(Name),
            unregister(Name),
            exit(Pid, "crash"),
            timer:sleep(2000),
            register(Name, acceptor:start(Name, na))
    end.