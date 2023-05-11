(* ::Package:: *)

(* IMPORT E DEFINIZIONE FUNZIONE DI CREAZIONE NUOVA SCACCHIERA*)
SetDirectory[NotebookDirectory[]]

dataset = Import["test.csv","Dataset","HeaderLines"->1];
board = ImportString["rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1", "FEN"];
bestMove = aaaa;


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


(* carico il package *)
dir = NotebookDirectory[];
SetDirectory[NotebookDirectory[]];
Get[dir<>"Chess-master/Chess.wl"];


(* Carico il dataset e creo i file pgn *)
problems = Import["dataset.zip","*.txt"][[1]];
MakePGNfiles[problems];


whoIsPlaying;
filepgn;
correctMove;
generateNewChessBoard[]:= Module[ {randomNum, board},
(* So che i PGN files totali sono 11715*)
randomNum=RandomInteger[{1,11715}];
filepgn = PGNfile[randomNum]["PGN"];
correctMove=Last[filepgn];
board=PGNconvert[filepgn];
Chess[ShowBoard->board,Interact->True];
If[StringMatchQ[PGNfile[randomNum]["Result"], "1-0"], whoIsPlaying = "mossa al BIANCO, trova lo scacco matto", whoIsPlaying = "mossa al NERO, trova lo scacco matto"];
Move[MoveFromPGN[#][[1]]]&/@ Drop[board, Length[Movelist]-1];
]

board = Startposition;
Chess[ShowBoard->Interactive]
Button["Nuova scacchiera", board=generateNewChessBoard[]] Button["Restart",Startposition] 
Button["Verifica mossa" (*funzionedaimplementare*)]
(*stringa che mostra chi deve giocare*)
Dynamic@whoIsPlaying
(*Lista di mosse della partita --> dalla quale prendere ultima mossa per confronto*)
Dynamic@filepgn
"CORRECT_MOVE: "Dynamic@correctMove

(*
Ultima mossa effettuata
TODO: vedere nel package se c'\[EGrave] gi\[AGrave] qualcosa
o creare switch per fare diventare la stringa "pawn e4" --> "e4"; "knight f6" --> "Kf6"
attenzione a quando si mangia un pezzo: si scrive con una x in mezzo.
Per esempio se il cavallo in f6 mangia il pedone in e4 si scriver\[AGrave] Kxe4.
altro esempio: se il pedone in e4 mangia il pedone in d5 si scriver\[AGrave] exd5.
I pezzi diversi dai pedoni quindi devono sempre essere esplicitati 
tramite lettera maiuscola rappresentativa.
*)
piece = Dynamic@Last[Movelist][[2]][[2]];
coord = Dynamic@Coord[Last[Movelist][[2]][[1]]];
StringForm["`1` `2`",piece,coord]



