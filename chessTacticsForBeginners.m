(* ::Package:: *)

(* IMPORT E DEFINIZIONE FUNZIONE DI CREAZIONE NUOVA SCACCHIERA*)
dataset = Import["C:\\Users\\gabri\\Documents\\Universit\[AGrave]\\Matematica computazionale\\chess-tactics-for-beginners\\test.csv","Dataset","HeaderLines"->1];
board = ImportString["rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1", "FEN"];

generateNewChessBoard[]:= Module[ {num,fen,best},
num = RandomInteger[{1,Length[dataset]}];
fen = dataset[num,"fen"];
best = dataset[num,"best"];
ImportString[fen,"FEN"]
]

(* Inserimento del nome da input utente *)
nomeUtente = InputString["Inserisci il tuo nome:"];

newChessboard = Button["Nuova scacchiera", board=generateNewChessBoard[]];
clearChessboard = Button["Pulisci scacchiera", board=ImportString["rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1", "FEN"]];
newChessboard clearChessboard
nomeUtente "sta giocando!"
Dynamic@board

(*
TODO: 
 - ricordarsi del seed della posizione appena giocata --> scrivere su file txt stringa fen 
	e successivamente creare menu a tendina per rigiocare la posizione salvata.
 - muovere i pezzi tramite GUI e/o tramite input esterni (menu a tendina, etc.)
*)



