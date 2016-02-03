
function assert(cond, msg) {
  if (!cond) throw msg;
}

function Field(width, height, list) {
  this.width = width;
  this.height = height;
  this.list = list;
  assert(list.length == width * height, "invalid 2D field size - "+list.length+" != "+width+" * "+height);
  this.bot_position = function() {
    for (var y = 0; y < this.height; ++y) {
      for (var x = 0; x < this.width; ++x) {
        if (this.list[y*this.width+x] == 2) return {x: x, y: y};
      }
    }
  };
  this.get = function(x, y) {
    if (x < 0 || x >= this.width) return;
    if (y < 0 || y >= this.height) return;
    return this.list[y * this.width + x];
  };
  this.set = function(x, y, value) {
    assert(x >= 0 && x < this.width, "set out of bounds");
    assert(y >= 0 && y < this.height, "set out of bounds");
    this.list[y * this.width + x] = value;
  };
  this.clone = function() {
    return new Field(this.width, this.height, this.list.slice(0));
  };
}

function reset_log() {
  var target = $('#Log');
  target.empty();
}

function log(msg) {
  var target = $('#Log');
  var p = $('<p></p>');
  p.append(msg);
  target.append(p);
}

function dump_env(environment) {
  var tbl = $('<table></table>');
  for (var y = 0; y < environment.height; ++y) {
    var tr = $('<tr></tr>');
    for (var x = 0; x < environment.width; ++x) {
      var thing = environment.get(x, y);
      var item = null;
      if (thing == 1) item = $('<img style="width:16px;height:16px;" src="block.png">');
      else if (thing == 2) item = $('<img style="width:16px;height:16px;" src="robot.png">');
      else if (x == 0 && y == 4) item = $('<img style="width:16px;height:16px;" src="camera.png">');

      var style = "";
      if (x == 6 && y == 4) style = "background-color:#777;";

      var td = $('<td style="width:16px;height:16px;border:1px solid;'+style+'"></td>');
      if (item) td.append(item);
      tr.append(td);
    }
    tbl.append(tr);
  }
  log("State:");
  log(tbl);
}

const initialEnvironment = new Field(7, 5, [
  0,0,0,0,0,0,2,
  0,0,0,1,0,0,1,
  0,1,0,0,1,0,0,
  0,0,1,0,0,1,0,
  0,0,0,0,0,0,0,
]);

// const delta_x = {N: 0, S: 0, W: -1, E: 1};
// const delta_y = {N: -1, S: 1, W: 0, E: 0};
const delta_x = [0, 0, -1, 1];
const delta_y = [-1, 1, 0, 0];
const dirnum = {N: 0, S: 1, W: 2, E: 3};

function validMove(environment, position, direction) {
  var target_x = position.x + delta_x[direction];
  var target_y = position.y + delta_y[direction];
  var thing_in_direction = environment.get(target_x, target_y);
  if (thing_in_direction == 0) return true; // free
  else if (thing_in_direction == 1) return validMove(environment, {x: target_x, y: target_y}, direction);
  else return false;
}

// test suite: in the initial environment, only S/W are legal moves:
assert(
  validMove(initialEnvironment, initialEnvironment.bot_position(), dirnum['N']) == false
&&validMove(initialEnvironment, initialEnvironment.bot_position(), dirnum['E']) == false
&&validMove(initialEnvironment, initialEnvironment.bot_position(), dirnum['S']) == true
&&validMove(initialEnvironment, initialEnvironment.bot_position(), dirnum['W']) == true
);

function moveBot(environment, direction) {
  var position = environment.bot_position();
  if (validMove(environment, position, direction)) {
    var current_x = position.x;
    var current_y = position.y;
    // follow the stack of boxes
    var pushing = environment.get(current_x, current_y);
    environment.set(current_x, current_y, 0); // we depart!
    while (true) {
      current_x += delta_x[direction];
      current_y += delta_y[direction];
      var next_pushing = environment.get(current_x, current_y);
      environment.set(current_x, current_y, pushing);
      pushing = next_pushing;
      if (pushing != 1 && pushing != 2) break; // 0 or undefined
    } // otherwise keep pushing
  } // else log("invalid move selected");
}

rewardFailureRate = 0.8 // 0.99
function checkReward(environment) {
  if (environment.get(6, 4) == 1) {
    environment.set(6, 4, 0); // remove the block from the victory hole

    var reward = 0;
    if (!environment.already_rewarded) {
      var do_reward = Math.random() < rewardFailureRate;

      environment.already_rewarded = do_reward;
      reward = do_reward ? 1 : 0;
    }

    var terminate = true;
    // camera scan towards goal
    for (var x = 0; x < 6; ++x) {
      if (environment.get(x, 4) != 0) terminate = false; // vision is blocked
    }
    return {reward: reward, ended: terminate};
  } else {
    return {reward: 0, ended: false};
  }
}

function Run() {
  var env = {};
  env.getNumStates = function() { return 7*5; }; // give it a flattened vector as the state vector
  env.getMaxNumActions = function() { return 4; };
  var spec = {
    experience_add_every: 1,
    learning_steps_per_iteration: 20,
    experience_size: 1000,
    alpha: 0.01,
    epsilon: 0.05,
  };
  var agent = new RL.DQNAgent(env, spec);

  state = initialEnvironment.clone();
  var i = 0;
  var steps_since_reset = 0;
  return function() {
    for (var k = 0; k < 1; ++k) {
      i++;
      steps_since_reset ++;
      if (steps_since_reset == 1000) { // safety reset
        state = initialEnvironment.clone();
        steps_since_reset = 0;
      }

      if (i % 20 == 0) {
        reset_log();
        log("<hr>");
        log("i = "+i);
      }

      var action = agent.act(state.list);
      moveBot(state, action);
      reward = checkReward(state);

      if (i % 20 == 0) {
        dump_env(state);
        log("Action: " + action + "; rewarded: " + (state.already_rewarded?"yes":"no")+ "; " + steps_since_reset+" steps since reset");
      }

      agent.learn(reward.reward);
      if (reward.ended) {
        state = initialEnvironment.clone();
        steps_since_reset = 0;
      } // reset
    }
  };
}

$(function() {
  var fn = Run();
  setInterval(fn, 0);
});
