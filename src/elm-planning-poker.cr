require "kemal"
require "json"

rooms = {} of String => Room

class Room
  def initialize(dealer : HTTP::WebSocket)
    @dealer = dealer
    @players = [] of HTTP::WebSocket
  end

  def dealer
    @dealer
  end

  def players
    @players
  end

  def add_player(player : HTTP::WebSocket)
    @players.push(player)
  end
end

class Json
  JSON.mapping(
    type: String,
  )
end

class JoinJson
  JSON.mapping(
    name: String,
    room_number: String,
  )
end

class GoalJson
  JSON.mapping(
    room_number: String,
    goal: String
  )
end

class SendGoalJson
  JSON.mapping(
    room_number: String,
    goal: String,
    player_id: String
  )
end

class VoteJson
  JSON.mapping(
    room_number: String,
    vote: String,
    player_id: String
  )
end

ws "/app" do |socket|
  puts "In contact with socket: #{socket}"

  # Handle incoming message and dispatch it to all connected clients
  socket.on_message do |message|
    json = Json.from_json message

    case json.type
    when "create_room"
      room_number = (Random.new.rand(8999) + 1000).to_s
      rooms[room_number] = Room.new socket
      puts "Room created successfully! Telling #{socket}."
      socket.send ({"type": "room_success", "room_number": room_number, "player_id": "#{socket}"}.to_json)
    when "join_room"
      joinjson = JoinJson.from_json message
      player_name = joinjson.name
      begin
        rooms[joinjson.room_number].players.push socket
        puts "Room joined successfully! Telling #{socket}."
        socket.send ({"type": "join_success", "player_id": "#{socket}"}.to_json)
        room_dealer = rooms[joinjson.room_number].dealer
        puts "Telling #{room_dealer} a new player has arrived."
        room_dealer.send ({"type": "add_player", "player_name": player_name, "player_id": "#{socket}"}.to_json)
      rescue ex : KeyError
        socket.send ({"type": "join_failure"}.to_json)
      end
    when "create_goal"
      goaljson = GoalJson.from_json message
      room_number = goaljson.room_number
      goal = goaljson.goal
      room = rooms[room_number]
      puts "New goal created! Telling all players in room #{room_number}."
      json_message = ({"type": "new_goal", "goal": goal}.to_json)
      room.players.each do |player|
        player.send json_message
      end
      room.dealer.send json_message
    when "send_goal"
      goaljson = SendGoalJson.from_json message
      room_number = goaljson.room_number
      goal = goaljson.goal
      player_id = goaljson.player_id
      room = rooms[room_number]
      json_message = ({"type": "new_goal", "goal": goal}.to_json)
      room.players.each do |player|
        if "#{player}" == player_id
          puts "Sending goal to player #{player_id}."
          player.send json_message
        end
      end
    when "new_vote"
      votejson = VoteJson.from_json message
      room_number = votejson.room_number
      vote = votejson.vote
      player_id = votejson.player_id
      room = rooms[room_number]
      puts "New vote: #{vote}! Telling #{room.dealer}."
      json_message = ({"type": "new_vote", "vote": vote, "player_id": player_id}.to_json)
      room.dealer.send json_message
    else
      puts "ERROR: Could not understand message"
      puts json
    end
  end

  socket.on_close do |_|
    rooms.each_value do |room|
      room.players.each do |player|
        if player == socket
          room.players.delete(player)
          puts "Closing socket: #{socket}"
          puts "Contacting dealer: #{room.dealer}"
          room.dealer.send ({"type": "remove_player", "player_id": "#{player}"}.to_json)
        end
      end
    end
  end
end

Kemal.config.port = 8080
Kemal.run
