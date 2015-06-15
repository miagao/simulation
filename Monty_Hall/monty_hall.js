/********************************************************
 * Runs the Monty Hall problem continually,
 * graphing the results of switching or not switching.
*********************************************************/
var sketchProc=function(processingInstance){ with (processingInstance){

    size(400, 400);


var graphX = 50;
var graphY = 300;
var graphWidth = 300;
var graphHeight = 200;
var barWidth = 100;
var barGap = (graphWidth - 2*barWidth) / 3;
var bar1X = graphX + barGap;
var bar2X = graphX + barGap*2 + barWidth;

// Speed of simulation - don't set to high
frameRate(50);

var number_of_doors = 3;
var wins_if_you_switch = 0;
var wins_if_you_dont_switch = 0;
var number_of_games = 0;

var playGame = function() {
    number_of_games++;
    var prize_door = floor(random(0, number_of_doors));
    var chosen_door = floor(random(0, number_of_doors));

    // Show door which isn't picked and isn't the prize door
    var possible_shown_doors = [];
    for (var i=0; i < number_of_doors; i++) {
        if (i !== prize_door && i !== chosen_door) {
            possible_shown_doors.push(i);
        }
    }

    var random_num = floor(random(0, possible_shown_doors.length));
    var shown_door = possible_shown_doors[random_num];

    // If you don't switch then you win if you picked the right door
    if (chosen_door === prize_door) {
        wins_if_you_dont_switch++;
    }

    // Switch to a door that isn't open
    var switched_door = null;
    for (var j=0; j < number_of_doors; j++) {
        if (j !== shown_door && j !== chosen_door) {
            switched_door = j;
        }
    }

    if (switched_door === prize_door) {
        wins_if_you_switch++;
    }
};

textSize(20);

var draw = function() {
    playGame();
    background(240, 240, 240);

    // Bars
    var scaleY = graphHeight / (1 + number_of_games);

    noStroke();
    fill(0, 255, 0);
    var y1 = scaleY * wins_if_you_switch;
    var y2 = scaleY * wins_if_you_dont_switch;
    rect(bar1X, graphY - y1, barWidth, y1);
    rect(bar2X, graphY - y2, barWidth, y2);

    // Axes
    stroke(8, 8, 8);
    fill(8,8,8);
    strokeWeight(2);
    line(graphX, graphY, graphX + graphWidth, graphY);

    // Labels
    text("Switch", bar1X+10, graphY+30);
    text("Don't switch", bar2X, graphY+30);

    var percent1 = round(1000 * wins_if_you_switch / number_of_games)/10;
    var percent2 = round(1000 * wins_if_you_dont_switch / number_of_games)/10;
    text(percent1 + "%", bar1X+20, graphY - y1 - 5);
    text(percent2 + "%", bar2X+20, graphY - y2 - 5);
    text("Games won (" + number_of_games + " played)", graphX, graphY - graphHeight);

}
} };
