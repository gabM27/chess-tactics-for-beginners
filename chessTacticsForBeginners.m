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
generateNewChessBoard[]:= Module[ {randomNum, board},
(* So che i PGN files totali sono 11715*)
randomNum=RandomInteger[{1,11715}];
filepgn = PGNfile[randomNum]["PGN"];
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
(*Lista di mosse GIOCATE --> anche da questa prendere ultima mossa per confronto con lista precedente*)
PGN//Dynamic



