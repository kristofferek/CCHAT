-module(client).
-export([handle/2, initial_state/3]).

% We chose not to add anything to the client state. Discussed if the client
% should hold a list of all the channel it's connected to but we figured
% this would be redundant. The check that the user is connected to a channel
% which it wish to send a message to is done in the server.
-record(client_st, {
    gui, % atom of the GUI process
    nick, % nick/username of the client
    server % atom of the chat server
}).

% Return an initial state record. This is called from GUI.
% Do not change the signature of this function.
initial_state(Nick, GUIAtom, ServerAtom) ->
    #client_st{
        gui = GUIAtom,
        nick = Nick,
        server = ServerAtom
    }.

% handle/2 handles each kind of request from GUI
% Parameters:
%   - the current state of the client (St)
%   - request data from GUI
% Must return a tuple {reply, Data, NewState}, where:
%   - Data is what is sent to GUI, either the atom `ok` or a tuple {error, Atom, "Error message"}
%   - NewState is the updated state of the client

% Join channel
handle(St = #client_st{nick = Nick, server = Server}, {join, Channel}) ->
    Response = (catch genserver:request(Server, {join, Channel, Nick, self()})),

    case Response of
        {'EXIT', _} ->
            {reply, {error, server_not_reached, "Server does not respond"}, St};
        join -> {reply, ok, St};
        error -> {reply, {error, user_already_joined, "User already joined"}, St}
    end;

% Leave channel
handle(St = #client_st{server = Server}, {leave, Channel}) ->
    Response = (catch genserver:request(Server, {leave, Channel, self()})),

    case Response of
        {'EXIT', _} ->
            {reply, {error, server_not_reached, "Server does not respond"}, St};
        leave -> {reply, ok, St};
        error -> {reply, {error, user_not_joined, "User has not joined that channel"}, St}
    end;

% Sending message (from GUI, to channel)
handle(St = #client_st{nick = Nick}, {message_send, Channel, Text}) ->
    Response = (catch genserver:request(list_to_atom(Channel), {message_send, Nick, Text, self()})),

    case Response of
        {'EXIT', _} ->
            {reply, {error, server_not_reached, "Channel does not respond"}, St};
        message_send -> {reply, ok, St};
        error -> {reply, {error, user_not_joined, "User has not joined that channel"}, St}
    end;

% ---------------------------------------------------------------------------
% The cases below do not need to be changed...
% But you should understand how they work!

% Get current nick
handle(St, whoami) ->
    {reply, St#client_st.nick, St} ;

% Change nick
handle(St, {nick, NewNick}) ->
    Response = (catch genserver:request(St#client_st.server, {nick, NewNick})),
    case Response of
        {'EXIT', _} ->
            {reply, {error, server_not_reached, "Server does not respond"}, St};
        error -> {reply, {error, nick_taken, "Nick already taken"}, St};
        ok -> {reply, ok, St#client_st{nick = NewNick}}
    end;

% Incoming message (from channel, to GUI)
handle(St = #client_st{gui = GUI}, {message_receive, Channel, Nick, Msg}) ->
    gen_server:call(GUI, {message_receive, Channel, Nick++"> "++Msg}),
    {reply, ok, St} ;

% Quit client via GUI
handle(St, quit) ->
    % Any cleanup should happen here, but this is optional
    genserver:request(St#client_st.server, {disconnect, self(), St#client_st.nick}),
    {reply, ok, St} ;

% Catch-all for any unhandled requests
% Did an _Data instead of Data to supress compiler warning.
handle(St, _Data) ->
    {reply, {error, not_implemented, "Client does not handle this command"}, St} .
