-module(channel).
-export([start/2]).

-record(channelState, {
    name,
    users
}).

start(Name, InitialUser) ->
  genserver:start(list_to_atom(Name), #channelState{ name=Name, users=[ InitialUser ]}, fun handle_channel/2) .


% Functions for handling requests to a channel-process.

% JOIN
handle_channel(State = #channelState{users = UsersInChannel}, {join, NewPid}) ->
  % Joining a channel, if the user is already joined an error is returned.
  IsMember = lists:member(NewPid, UsersInChannel),
  if IsMember ->
    {reply, error, State};
  true ->
    {reply, join, State#channelState{users = [ NewPid | UsersInChannel ]}}
  end;

% LEAVE
handle_channel(State = #channelState{users = UsersInChannel}, {leave, Sender}) ->
  % User wants to leave the channel. Respond with an error if user is not in channel.
  IsMember = lists:member(Sender, UsersInChannel),
  if IsMember ->
      NewUsers = lists:delete(Sender, UsersInChannel),
      {reply, leave, State#channelState{users = NewUsers}};
    true ->
      {reply, error, State}
  end;

% SEND MESSAGE
handle_channel(State = #channelState{name = ChannelName, users = UsersInChannel}, {message_send, Nick, Msg, Sender}) ->
  % Sending a message to all clients connected to the channel.
  % As above, if a user is not a member of channel, an error is sent.
    IsMember = lists:member(Sender, UsersInChannel),
    case IsMember of
      true ->
        spawn(
          fun() ->
            [ genserver:request(
                Receiver,
                {message_receive, ChannelName, Nick, Msg}
              ) || Receiver <- UsersInChannel, Receiver =/= Sender]
          end
        ),
        {reply, message_send, State};

      false ->
        {reply, error, State}
    end ;

% Catch-all for any unhandled requests
handle_channel(State, _) ->
  {reply, error, State} .
