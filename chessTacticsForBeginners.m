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


(* carico il package *)
dir = NotebookDirectory[];
SetDirectory[NotebookDirectory[]];
Get[dir<>"Chess-master/Chess.wl"];


(* Carico il dataset e creo i file pgn *)
problems = Import["dataset.zip","*.txt"][[1]];
MakePGNfiles[problems]


(* 
converto il file della partita #100 e la mostro in scacchiera 
N.B. : non tutte le partite vanno bene, 
forse perch\[EGrave] formattata in modo errato 
rispetto a quanto descritto dal package che usiamo
*)

(* PER CONTROLLARE SE FUNZIONANO TUTTI I PGN DEL FILE besmoves.txt
n = 1; While[n < 11726, Print[PGNconvert[PGNfile[n]["PGN"]]]; n++] *)

(* So che i PGN files totali sono 11726*)
randomNum=RandomInteger[{1,11726}];
board=PGNconvert[PGNfile[randomNum]["PGN"]];
board

Chess[ShowBoard->board,Interact->True]




