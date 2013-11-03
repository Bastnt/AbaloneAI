// To change the starting board please change the value of the variable startingBoard below with one of this value 

// alliances
// allienage
// boutonniere
// centrifugeuse
// domination
// duel
// faceaface
// fujiyama
// infiltration
// margueriteallemande
// margueritebelge 
// margueritehollandaise
// margueritesuisse
// standard
// star
var startingBoard = "standard";


// Difficulty
var depth = 3;

// Players type (0: Human, 1: AI)
var players = new Array(1, 0);
//                      W  B

var server = "http://bastnt.dyndns.tv/abalone/";
var animationTime = 1000;

// Starting Player
var activePlayer = 2;

// Path of AI to execute
// Color                W              B
var	AI = new Array("./abalone", "./abalone");

// Initial Score
score = new Array(0,0);
