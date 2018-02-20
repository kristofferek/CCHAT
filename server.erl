-module(server).
-export([start/1,stop/1]).

-record(serverState, {
    channels = [],
    nicks = []
}).

% Start a new server process with the given name
% Do not change the signature of this function.
start(ServerAtom) ->
  genserver:start(ServerAtom, #serverState{}, fun handle_server/2).

% Stop the server process registered to the given name,
% together with any other associated processes
stop(ServerAtom) ->
  % Stopping the processes of all the channels associated with this server.
  genserver:request(ServerAtom, stop),

  % Stopping the server atom.
  genserver:stop(ServerAtom).


% JOIN
handle_server(State = #serverState{channels = Channels}, {join, Channel, Nick, Sender} ) ->
  % Check if the channel already exists. If it does, we send a message
  % to the channel that we want to add the user. If it doesn't exist
  % we spawn a new process for that channel and add the user to it in
  % its initial state. Rest of it should be self-explanatory.
  ChannelExists = lists:member(Channel, Channels),
  if ChannelExists ->
      Response = (catch(genserver:request(list_to_atom(Channel), {join, Sender}))),

      case Response of
        error -> {reply, error, State};
        join ->
          NickExists = lists:member(Nick, State#serverState.nicks),
          if NickExists ->
              {reply, join, State};
            true ->
              NewState = State#serverState{nicks = [ Nick | State#serverState.nicks ]},
              {reply, join, NewState}
          end
      end;

    % If channel doesn't exist, spawn one as a new process.
    % Initial state of the channel is set to the record channelState, initializing the
    % name to the name of the channel and users to a list that contains this first user.
    true ->
      channel:start(Channel, Sender),
      {reply, join, State#serverState{channels = [ Channel | Channels ]}}
  end;

% LEAVE
handle_server(State = #serverState{channels = Channels}, {leave, Channel, Sender} ) ->
  % If channel exists, and the user is a member of it, it leaves it, otherwise return an error.
  ChannelExists = lists:member(Channel, Channels),
  if ChannelExists ->
      Response = (catch(genserver:request(list_to_atom(Channel), {leave, Sender}))),

      case Response of
        leave -> {reply, leave, State};
        error -> {reply, error, State}
      end;
    true ->
      {reply, error, State}
  end;

% NICK
handle_server(State = #serverState{nicks = Nicks}, {nick, Nick} ) ->
  NickExists = lists:member(Nick, Nicks),
  if NickExists ->
      {reply, error, State};
    true ->
      NewState = State#serverState{nicks = [ Nick | Nicks ]},
      { reply, ok, NewState }
  end;

% DISCONNECT USER
handle_server(State = #serverState{channels = Channels, nicks = Nicks}, {disconnect, UserPid, UserNick}) ->
  % Remove user nickname from taken nicks
  NewNicks = [N || N <- Nicks, N =/= UserNick],
  % Remove user from all channels it's connected to
  [ genserver:request(list_to_atom(Channel), {leave, UserPid}) || Channel <- Channels ],
  { reply, ok, State#serverState{nicks = NewNicks}} ;

% Stops all channel processeses
handle_server(State, {stop} ) ->
  [ genserver:stop(list_to_atom(Channel)) || Channel <- State#serverState.channels ] ;

% Catch-all for any unhandled requests
handle_server(State, _) ->
  {reply, error, State} .
