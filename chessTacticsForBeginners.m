(* ::Package:: *)

(* :Title: Chess Tactics for Beginners                                   *)
(* :Author: A.Accornero, A.Bianchi, M.Frega, G.Magazz\[UGrave]                   *)
(* :Summary:
	Lo scopo del progetto consiste nel creare un Mathematica Package 
	che proponga ad un utente principiante diverse partite per aiutarlo
	a migliorare le proprie abilit\[AGrave] nelle risoluzioni 
	di problemi posizionali negli scacchi.
	Per il momento vengono trattati solo scacchi matti in una mossa.
*)
(* :Context: chessTacticsForBeginners`                                   *) 
(* :Package Version: 0.1                                                 *)
(* :Copyright: Wolfram Knights                                           *)
(* :History: Progetto universitario per il corso 
	di Matematica Computazionale a.a. 2022/2023                          *)
(* :Keywords: Chess, Scacchi, Scacco Matto, Checkmate, Tattica,          *)
(* :Source:                                 *)
(* :Warning: None.                          *)
(* :Mathematica Version: 13.2               *)
(* :Limitation: None.                       *)
(* :Discussion:                             *)
BeginPackage["chessTacticsForBeginners`"];
Main::usage="Main function";


(* navigo nella sottodirectory per caricare il package Chess by Arne Eide *)
dir = NotebookDirectory[];
dir = dir<>"Chess-master";
SetDirectory[dir];
Needs["Chess`"];
SetDirectory[NotebookDirectory[]];

problems = Import["dataset.zip","*.txt"][[1]];
MakePGNfiles[problems];

grid::usage="grid";
board::usage="scacchiera";
filepgn::usage="file PGN della partita";                                                                                    (* file PGN della partita*)
pgntosplit::usage="variabile di appoggio per manipolare il file PGN";                                                       (* variabile di appoggio per manipolare il file PGN *)
moveToCheck::usage="mossa che compie il giocatore e che deve essere controllata per verificare se corretta";                (* mossa che compie il giocatore e che deve essere controllata per verificare se corretta *)
lastgame::usage="variabile utile a ricaricare l'ultima partita giocata";                                                    (* variabile utile a ricaricare l'ultima partita giocata*)
nomeUtente="";                                                                                                              (*Variabile usata per memorizzare il nickname dell'utente che sta giocando*)
whoIsPlaying = "";                                                                                                          (* memorizza il nome del giocatore *)
endgame = "";                                                                                                               (* contiene il messaggio di successo o sconfitta di fine partita *)
correctMove::usage="mossa corretta, ovvero mossa che porta allo scacco matto ";                                             (* mossa corretta, ovvero mossa che porta allo scacco matto *)
correctMoveToPrint = "";                                                                                                    (* formato stampa della mossa corretta *)
gameResult = 0;                                                                                                             (* partita vinta oppure persa *)
dimensionBoard::usage="Var. per settare la dimensione della board";                                                         (* Var. per settare la dimensione della board*)
checkMovelist::usage="";                                                                                                    (*                                            *)
colorBoard::usage="Var. per colore RGB della scacchiera, inizializzata a\[NonBreakingSpace]color\[NonBreakingSpace]default";                                    (* Var. per colore RGB della scacchiera, inizializzata a\[NonBreakingSpace]color\[NonBreakingSpace]default*)
                                                                                                                            
seed = "";
randomNum = "";
showSeed = " ";

generateNewChessBoard::usage="funzione che genera una nuova scacchiera";
repeatChessBoard::usage="funzione che fa rigiocare la partita precedente";
dropCharWhiteMove::usage="funzione che droppa i caratteri in eccesso nella stringa di output che mostra la soluzione";
changeDimensionBoard::usage="funzione che modifica la dimensione della scacchiera";
resetColorBoard::usage="funzione che resetta il colore iniziale della scacchiera";
changeColorBoard::usage="funzione che modifica il colore iniziale della scacchiera";
checkMove::usage="funzione che verifica se la mossa scelta \[EGrave] quella corretta";

dimensionBoard = 240;  
colorBoard=RGBColor[0.8196,0.5451,0.2784]; 
(* boolean di attivazione dei pulsanti*)
newBoardEnabled = True;
restartEnabled = False;
repeatEnabled = False;
backEnabled = False;
checkMoveEnabled = False;
showSolutionEnabled = False;
changeColorEnabled = True;
resetColorEnabled = False;
changeDimensionEnabled = True;

Begin["`Private`"];

(* funzione che viene attivata una volta cliccato il button "Nuova Scacchiera":
- Genera un numero casuale intero compreso tra 1 e 11715 (num di partite del dataset)
- Estrae la partita dal dataset in base al numero generato 
- Viene memorizzata l'ultima mossa della partita (correctMove) che servir\[AGrave] per il controllo del vincitore
- Viene caricata la partita nella scacchiera traimite Chess[...]
- A seconda degli ultimi 3 caratteri presenti in ogni partita ("1-0" o "0-1") l'utente giocher\[AGrave] con i bianchi o con i neri
- Vengono poi mosse tutte le pedine tramite Move[...] fino alla penultima mossa della partita
- Salviamo le mosse che sono state fatte per la partita *)
generateNewChessBoard[] := Module[{},

  lastgame = randomNum;
  filepgn = PGNfile[randomNum]["PGN"];
  correctMove = Last[filepgn];
  correctMove = StringDrop[correctMove, -1];
  board = PGNconvert[filepgn];
  Chess[ShowBoard -> board, Interact -> True,ImageSize -> dimensionBoard,BoardColour -> colorBoard];
  If[StringMatchQ[PGNfile[randomNum]["Result"], "1-0"],
    whoIsPlaying = "Mossa al BIANCO",
    whoIsPlaying = "Mossa al NERO"];
    
  Move[MoveFromPGN[#][[1]]] & /@ Drop[board, Length[Movelist] - 1];
  checkMovelist = Length[Movelist];
  showSeed = ToString[randomNum];
  
]
(* Funzione che viene invocata al  click del button "Rigioca Partita":
- Tramite la variabile 'lastgame' sappiamo qual'\[EGrave] l'ultima partita giocata dall'utente e la mossa corretta (quella finale)
- Carichiamo nella board la partita in questione e poi la mostriamo nella scacchiera tramite Chess[...]
- Controlliamo sempre tramite "1-0" o "0-1" quale pedine dovr\[AGrave] muovere l'utente
- Mostriamo nella scacchiera le pedine nelle penultime posizioni della partita *)
repeatChessBoard[] := Module[{board},
  filepgn = PGNfile[lastgame]["PGN"];
  showSeed = ToString[lastgame];
  correctMove = Last[filepgn];
  correctMove = StringDrop[correctMove, -1];
  board = PGNconvert[filepgn];
  Chess[ShowBoard -> board, Interact -> True,ImageSize -> dimensionBoard,BoardColour -> colorBoard];
  If[StringMatchQ[PGNfile[lastgame]["Result"], "1-0"],
    whoIsPlaying = "mossa al BIANCO",
    whoIsPlaying = "mossa al NERO"];
  tmp= PGN // Dynamic;
  Move[MoveFromPGN[#][[1]]] & /@ Drop[board, Length[Movelist] - 1];
  checkMovelist = Length[Movelist];
  
]

(*Questa funzione serve solo nel caso in cui l'utente stia giocando con le pedine bianche: 
- Serve per stampare correttamente l'output del button "Mostra Soluzione"
- Senza questa funzione l'ultima mossa stampata non sarebbe formattata correttamente*)
dropCharWhiteMove[] := Module[{},
	correctMoveToPrint = correctMove;
	If[StringMatchQ[PGNfile[lastgame]["Result"], "1-0"], (* Se la mossa \[EGrave] al bianco*)
	(* Elimino dalla stringa tutti i caratteri in questo formato: numeriInt.."." (.. significa qualsiasi cosa e poi il punto (".")) *)
	correctMoveToPrint=StringDelete[correctMoveToPrint, DigitCharacter.. ~~ ".", IgnoreCase -> False];
];
]

(*Funzione per il cambio dimensione alla scacchiera, 4 possibili dimensioni: 120,240,300,400
- Viene richiamata nel button "Dimensione Scacchiera"
- La dimensione cambia in ordine crescente, quindi dalla scacchiera pi\[UGrave] piccola (120) a quella pi\[UGrave] grande (400)- 
- Per applicare la dimensione passo al comando Chess[...] il paramatro 'dimensionBoard' che applico a ImageSize *)
changeDimensionBoard := Module[{},

Switch[dimensionBoard,120,dimensionBoard=240,
					  240,dimensionBoard=300,
					  300,dimensionBoard=400,
					  400,dimensionBoard=120];
					 
Chess[ShowBoard -> board,Interact -> False,ImageSize -> dimensionBoard,BoardColour -> colorBoard];
]

(*-Resetto il colore della scacchiera a quello iniziale di default tramite la variabile colorBoard
 -Per applicare il colore lo passo come parametro assegnandolo alla "BoardColour" il valore colorBoard*)
resetColorBoard := Module[{},
	colorBoard=RGBColor[0.8196,0.5451,0.2784];
	Chess[ShowBoard -> board,Interact -> False, ImageSize->dimensionBoard, BoardColour ->\[NonBreakingSpace]colorBoard];
	
]

(*Cambio colore alla scacchiera:
- la funzione \[EGrave] la stessa del reset color, solo che in questo caso il colore applicato \[EGrave] quello scelto dall'utente tramite ColorSetter*)
changeColorBoard := Module[{},
	colorBoard = selectedColor;
	Chess[ShowBoard -> board, Interact -> False, ImageSize->dimensionBoard, BoardColour ->\[NonBreakingSpace]selectedColor];
]
(* Funzione che viene attivata quando si preme "Verifica Mossa":
- Per sapere quante sono le mosse della partita, vengono salvate in una stringa tutte le mosse ("lista")
- Si memorizza il numero di tutte le mosse della partita e l'ultima mossa
- Per controllare se l'utente ha compiuto la mossa corretta, si confronta "moveToCheck" (l'ultima mossa della partita estratta dal dataset)
con "correctMove" (la mossa eseguita dall'utente): se sono uguali allora viene stampato il messaggio di successo, altrimenti di sconfitta.
 *)
checkMove[] := Module[{pgntosplit, delimitatori, lista, len, moveToCheck},
  pgntosplit = PGN // Dynamic;
  delimitatori = {" ", ", "};
  lista = StringSplit[pgntosplit[[1]], delimitatori];
  len = Dimensions[lista];
  moveToCheck = Last[lista];
  (*Print["Correct: " <> correctMove <> " moveToCheck: " <> moveToCheck];
  Print[lista];
  *)
  If[StringMatchQ[moveToCheck, correctMove],
    (gameResult=1; endgame = "MOSSA CORRETTA, BRAVO!"; MessageDialog["MOSSA CORRETTA, HAI VINTO!"];),(gameResult=0; endgame = "hai sbagliato, riprova o guarda la soluzione :(";MessageDialog["mossa errata, hai perso!"];)];
  Movelist = Most[Movelist];
  Chess[ShowBoard -> board, Interact -> False,ImageSize -> dimensionBoard,BoardColour -> colorBoard];
]

Main[]:=
Quiet@Block[{},

(* Carico il dataset e creo i file pgn *)
(* Questo \[EGrave] il messaggio pop-up della scelta del nome dell'utente all'avvio del programma:
- L'utente sar\[AGrave] obbligato a mettere un nome (che non sia una stringa vuota) ed eventuali spazi verranno eliminati restituendo un'unica stringa *)
While[True,
  nomeUtente = InputString["Inserisci il tuo nome:"];
  If[! StringMatchQ[StringTrim[nomeUtente]][""], Break[]];
];
nomeUtente=StringReplace[nomeUtente, " " -> ""];

board = Startposition; (* La board viene settata con le pedine in posizioni specifiche (iniziali, in caso di schermata iniziale, o penultime)*)

Chess[ShowBoard -> Interactive,ImageSize -> dimensionBoard,BoardColour -> colorBoard]
Chess[ShowBoard -> board, Interact -> False,ImageSize -> dimensionBoard,BoardColour -> colorBoard]; (*expr per disabilitare l'interazione con la scacchiera*)
(*Parametri del comando Chess[]:
- ShowBoard -> serve per specifiare l'interazione con la scacchiera visualizzata (
			  Interactive: possibilit\[AGrave] di interagire con essa e muovere le pedine
			  PGN-values (board) le pedine sono mosse tenendo conto dei valori PGN specificati
-ImageSize -> Tramite un valore numerico intero specifico la dimensione della scacchiera
-BoardColour -> Tramite valore RGBColor vado ad applicare un determinato colore alla scacchiera
)*)
grid
];

(* funzioni dei pulsanti*)
newBoardBtn = Button["Nuova Scacchiera", 
	Quiet@Block[{},
	While[True,  
     seed = InputString["Inserisci il seed della partita oppure 0 per generare una partita randomica (le partite sono 11715)"];
     If[! StringMatchQ[StringTrim[seed]][""] && StringMatchQ[seed, NumberString], 
           tmp = Interpreter["Number"][seed];
        If[tmp < 11716 && tmp >= 0, Break[]];
     ];
        
    ];
    If[StringMatchQ[seed, "0"],
    randomNum = RandomInteger[{1, 11715}],
    randomNum = Interpreter["Number"][seed]];
    
	board = generateNewChessBoard[]; 
	
	gameResult = 0;
	restartEnabled = False;
	repeatEnabled = False;
	backEnabled = True;
	checkMoveEnabled = True;
	showSolutionEnabled = True;
	newBoardEnabled = False;
	resetColorEnabled = False;
	changeDimensionEnabled=False;
	changeColorEnabled = False;],
		Method->"Queued",
			Enabled->Dynamic@newBoardEnabled];
			
	
repeatBtn = Button["Rigioca Partita", 
	board = repeatChessBoard[];
	repeatEnabled = False;
	restartEnabled = False;
	showSolutionEnabled = True;
	checkMoveEnabled = True;
	backEnabled = True;
	changeColorEnabled = False;
	resetColorEnabled = False;
	changeDimensionEnabled= False;
	newBoardEnabled = False;,
		Enabled->Dynamic@repeatEnabled];
	

restartBtn = Button["Restart",
	showSeed = " ";
	whoIsPlaying = ""; 
	endgame = ""; 
	correctMoveToPrint =""; 
	correctMove=""; 
	restartEnabled = False;
	repeatEnabled = True;
	newBoardEnabled = True;
	showSolutionEnabled = False;
	checkMoveEnabled = False;
	backEnabled = False;
	changeColorEnabled = True;
	changeDimensionEnabled=True;
	resetColorEnabled = True;
	Chess[ShowBoard -> Startposition, Interact -> False,ImageSize -> dimensionBoard,BoardColour -> colorBoard];,
		Enabled->Dynamic@restartEnabled];

checkBtn = Button["Verifica Mossa", 
	checkMove[];
	restartEnabled = True;
	checkMoveEnabled = False;,
		Enabled->Dynamic@checkMoveEnabled ];
	
showSolutionBtn = Button["Mostra Soluzione", 
	dropCharWhiteMove[];
	showSolutionEnabled = False;
	"La mossa corretta \[EGrave] " <> correctMoveToPrint,
		Enabled->Dynamic@showSolutionEnabled ];
	
changeColorBtn = Button["Colora Scacchiera:",
	changeColorBoard[];
	resetColorEnabled = True;,
		Enabled->Dynamic@changeColorEnabled];

resetColorBtn = Button["Reset Colore", resetColorBoard[];
	resetColorEnabled = False;,
		Enabled->Dynamic@resetColorEnabled];

changeSizeBtn = Button["Dimensione Scacchiera",
	changeDimensionBoard[];,
	Enabled->Dynamic@changeDimensionEnabled];
	
(* GraphicsGrid per generare tabella grafica dei comandi di gioco*)
(* All'interno dei CompressData sono presenti le immagini dei pezzi di scacchi
che vengono mostrati in alcune celle della tabella, al solo scopo decorativo *)
grid = GraphicsGrid[
{
{Import["whiteking.png"]," sta giocando"Dynamic@nomeUtente, Import["blacking.png"]},
{newBoardBtn, restartBtn, checkBtn },
{showSolutionBtn," seed partita"Dynamic@showSeed,repeatBtn},
{Dynamic@whoIsPlaying,Dynamic@endgame,Dynamic@correctMoveToPrint},
{changeSizeBtn, changeColorBtn,resetColorBtn },
{Import["blackqueen.png"],
ColorSetter[Dynamic[selectedColor]],
Import["whitequeen.png"] (*ColorSetter permette di scegliere un colore RGB,per colorare\[NonBreakingSpace]la\[NonBreakingSpace]scacchiera*)
}
}, Frame->All , AspectRatio->2/5, ImageSize->Large];

End[];
EndPackage[];
(*
Quiet[Main[]]*)
