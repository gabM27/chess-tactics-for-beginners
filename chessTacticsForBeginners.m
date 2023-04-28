(* ::Package:: *)

(* IMPORT E DEFINIZIONE FUNZIONE DI CREAZIONE NUOVA SCACCHIERA*)
dataset = Import["C:\\Users\\gabri\\Documents\\Universit\[AGrave]\\Matematica computazionale\\chess-tactics-for-beginners\\test.csv","Dataset","HeaderLines"->1];
board = ImportString["rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1", "FEN"];

generateNewChessBoard[]:= (
num = RandomInteger[{1,Length[dataset]}];
fen = dataset[num,"fen"];
best = dataset[num,"best"];
ImportString[fen,"FEN"]
)


(* Inserimento del nome da input utente *)
nomeUtente = InputString[]


newChessboard = Button["Nuova scacchiera", board=generateNewChessBoard[]];
clearChessboard = Button["Pulisci scacchiera", board=ImportString["rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1", "FEN"]];
newChessboard clearChessboard
Dynamic@board



