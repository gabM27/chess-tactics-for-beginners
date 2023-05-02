(* ::Package:: *)

(* IMPORT E DEFINIZIONE FUNZIONE DI CREAZIONE NUOVA SCACCHIERA*)
SetDirectory[NotebookDirectory[]]

dataset = Import["test.csv","Dataset","HeaderLines"->1];
board = ImportString["rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1", "FEN"];
bestMove = aaaa;

generateNewChessBoard[]:= Module[ {num,fen,best},
num = RandomInteger[{1,Length[dataset]}];
fen = dataset[num,"fen"];
bestMove = dataset[num,"best"];
ImportString[fen,"FEN"]
]

(* Inserimento del nome da input utente *)
nomeUtente = InputString["Inserisci il tuo nome:"];
newChessboard clearChessboard
newChessboard = Button["Nuova scacchiera", board=generateNewChessBoard[]];
clearChessboard = Button["Pulisci scacchiera", board=ImportString["rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1", "FEN"]];
(* newChessboard clearChessboard *)
generateNewChessBoard[]
nomeUtente "sta giocando!"


bestMove "miglior mossa:" 
Dynamic@board
movePiece = Button["Muovi pezzo", board = InputString["inserisci la mossa ad esempio 1a4b"], Method -> "Queued"];
checkmigliormossa[bestMove, movePiece]

checkmigliormossa[]:= Module[ {soluzione,tentativo},
(*result = [AlphabeticOrder[soluzione,tentativo];*)
soluzione === tentativo
]


(*
TODO: 
 - ricordarsi del seed della posizione appena giocata --> scrivere su file txt stringa fen 
	e successivamente creare menu a tendina per rigiocare la posizione salvata.
 - muovere i pezzi tramite GUI e/o tramite input esterni (menu a tendina, etc.)
*)


dir = NotebookDirectory[];
SetDirectory[dir<>"Chess-master"];
Get[dir<>"\Chess-master\Chess.wl"];

data = Import[dir<>"prova.zip","*.pgn"][[1]];
MakePGNfiles[data]

