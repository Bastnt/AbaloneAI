var board = new Array();
var selection = {};

$(function() {

	// Bind buttons
	$("#restart").click(restart);

	// Bind click on tiles
	for(var i=0; i<9; i++) {
		if (i<5) {
			max=5+i;
		}else{
			max=5+(8-i);
		}
		for(var j=0; j<max; j++) {
			$("#"+i+j).click({i: i, j: j}, select);
		}
	}

	// Load the board
	var path = server+"boards/"+startingBoard+".txt";
	$.get(path)
	.done(function(data) {
		importBoard(data);
		updateUIBoard();
		nextTurn();
	})
	.fail(function() { 
		alert("Error: Could not read "+path);
	});
});

function copyBoard(b) {
	var tmp = new Array();
	for(var l in b) {
		tmp[l] = b[l].slice();
	}

	return tmp;
}

function restart() {
	$("#restart").html("…");
	$("#log").val("");
	location.reload();
}

function updateUIBoard() {
	for (var i = 0; i < board.length; i++) {
		for (var j = 0; j < board[i].length; j++) {
			$("#"+i+j).removeClass("black");
			$("#"+i+j).removeClass("white");
			if(board[i][j] == 1)
				$("#"+i+j).addClass("black");
			else if(board[i][j] == 2)
				$("#"+i+j).addClass("white");
		}
	}
}

function opponent() {
	return (activePlayer == 1 ? 2 : 1); 
}

function isOut(m) {
	if(m[0] < 0 || m[0] >= board.length)
		return true;
	if(m[1] < 0 || m[1] >= board[m[0]].length)
		return true;
	return false;
}

function nextMarble(dir, m) {
	tmp = m.slice();
	switch(dir) {
		case 1:	tmp[1]++;	break;
		case 2:	if(tmp[0]<4)	tmp[1]++;	tmp[0]++;	break;
		case 3:	if(tmp[0]>=4)	tmp[1]--;	tmp[0]++;	break;
		case 4:	tmp[1]--;	break;
		case 5:	if(tmp[0]<=4)	tmp[1]--;	tmp[0]--;	break;
		case 6:	if(tmp[0]>4)	tmp[1]++;	tmp[0]--;	break;
		default:	return -1;
	}
	return tmp;
}

function importBoard(str) {
	var	tmp = new Array();
	tmp = str.split(" ");
	for (var i = 0; i < tmp.length; i++) {
		tmp[i] = tmp[i].split("");
	}

	for (var i = 0; i < tmp.length; i++) {
		for (var j = 0; j < tmp[i].length; j++) {
		tmp[i][j] = parseInt(tmp[i][j]);
		}
	}

	board = tmp;
}

function boardToString() {
	var tmp = "";

	for(var i = 0; i < board.length; i ++) {
		if(i != 0)
			tmp = tmp + " ";
		for(var j = 0; j < board[i].length; j++) {
			tmp = tmp + board[i][j];
		}
	}

	return tmp;
}

function select(event) {
	var coord = new Array(event.data.i, event.data.j);
	var coord_s = event.data.i.toString()+event.data.j.toString();

	if (board[coord[0]][coord[1]] == activePlayer) {
		if(selection[coord_s] == null){
			selection[coord_s] = coord;
			$("#"+coord_s).html("<img src='img/S.png' alt='o'>");
		}

		else {
			delete selection[coord_s];
			$("#"+coord_s).html("");
		}
	}
}

function clearSelection() {
	for(var key in selection)
		$("#"+key).html("");
	selection = {};
}

function playHuman(event) {
	var tmp = event.data.dir.toString();
	for(var key in selection)
		tmp = tmp + " " + key;

	bindInterface(false);
	play(tmp+"\n");
}

function playAI() {
	$.post(server+"playAI.php", {exec: AI[activePlayer-1]+" "+depth, player: activePlayer, board: boardToString()})
	.done(function(data) {
		play(data);
	})
	.fail(function(xhr, textStatus, errorThrown){ alert("playAI.php\nStatus:"+textStatus+"\nError: "+errorThrown+"\n"+xhr.responseText); });

}

function animateMarble(dir, m) {
	switch(dir) {
		case 1: l="+=48";t="+=0"; break;
		case 2: l="+=27";t="+=46"; break;
		case 3: l="-=27";t="+=46"; break;
		case 4: l="-=48";t="+=0"; break;
		case 5: l="-=27";t="-=46"; break;
		case 6: l="+=27";t="-=46"; break;
	}
	$("#"+m[0]+m[1])
	.animate({
		left: l,
		top: t
	}, 
	animationTime, 
	function() {
		$('#'+m[0]+m[1]).removeAttr('style'); 
	});
}


function play(str) {

	clearSelection();

	var data = str.split(" ");
	data[0] = parseInt(data[0]);

	if(isNaN(data[0]) || str.length > 20) {
		alert("Wrong Move: \n"+str);
	}

	else {
		var nboard = copyBoard(board);

		$("#log").val((activePlayer == 2 ? "W" : "B")+": "+str+$("#log").val());
		for(var a=1; a < data.length && a < 4; a++) {
			data[a] = data[a].split("");
			data[a][0] = parseInt(data[a][0]);
			data[a][1] = parseInt(data[a][1]);

			animateMarble(data[0], data[a]);
			collapseMarble(nboard, data[0], data[a]);
			removeMarble(nboard, data[a]);
		}

		for(var a=1; a < data.length && a < 4; a++) {
			addMarble(nboard, nextMarble(data[0], data[a]), activePlayer);
		}
	}
	
	setTimeout(function(){switchBoard(nboard)}, animationTime);
}

function switchBoard(b) {
	board = b;
	updateUIBoard();
	nextTurn();
}

function collapseMarble(b, dir, m) {
	var m2 = nextMarble(dir, m);

	if(isOut(m2)) {
		animateMarble(dir, m);
		looseMarble(b, m);
		return;
	}
	
	if(board[m2[0]][m2[1]] == opponent())
		collapseMarble(b, dir, m2);

	if(board[m[0]][m[1]] == activePlayer)
		return;

	animateMarble(dir, m);
	removeMarble(b, m);
	addMarble(b, m2, opponent());
}

function removeMarble(b, m) {
	addMarble(b, m, 0);
}

function addMarble(b, m, p) {
	b[m[0]][m[1]] = p;
}

function looseMarble(b, m) {
	removeMarble(b, m);
	score[activePlayer-1]++;
	$("#score").html("B: "+score[0]+"             W: "+score[1]);
}

function nextTurn() {
	if(score[0] >= 6 || score[1] >= 6)
		return;
	activePlayer = opponent();
	if(players[activePlayer-1] == 1)
		playAI();
	else
		bindInterface(true);
}

function bindInterface(bool) {
	// Bind click on arrows
	for(var i=1; i<7; i++) {
		if(bool)
				$("#A"+i).click({dir : i}, playHuman);
			else
				$("#A"+i).unbind("click");
		
	}
}