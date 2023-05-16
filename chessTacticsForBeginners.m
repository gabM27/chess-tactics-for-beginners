(* ::Package:: *)

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
pgntosplit;
moveToCheck;
correctMove;

(* Crea la nuova scacchiera*)
generateNewChessBoard[]:= Module[ {randomNum, board},
(* So che i PGN files totali sono 11715*)
randomNum=RandomInteger[{1,11715}];
filepgn = PGNfile[randomNum]["PGN"];
correctMove=Last[filepgn];
correctMove = StringDrop[correctMove,-1]; (* droppo il "#" finale*)
board=PGNconvert[filepgn];
Chess[ShowBoard->board,Interact->True];
If[StringMatchQ[PGNfile[randomNum]["Result"], "1-0"], whoIsPlaying = "mossa al BIANCO, trova lo scacco matto", whoIsPlaying = "mossa al NERO, trova lo scacco matto"];
Move[MoveFromPGN[#][[1]]]&/@ Drop[board, Length[Movelist]-1];
] 

(* verifico che la mossa fatta equivalga a quella corretta
TODO:
1. sistemare stampa solo una volta e cancellazione stampa precedente.
2. creare pulsante rigioca la stessa partita (possibilmente salvare per dopo) 
*)
checkMove[]:= Module[{pgntosplit,delimitatori,lista,var,len, moveToCheck},
pgntosplit = PGN // Dynamic;
delimitatori ={" " , ", "};
lista = StringSplit[pgntosplit[[1]],delimitatori];
len = Dimensions[lista];
moveToCheck = Last[lista];
Print["Correct: " correctMove <> " moveToCheck: " moveToCheck];
Print[lista];
If[StringMatchQ[moveToCheck,correctMove], endgame = "MOSSA CORRETTA, BRAVO!",endgame = "hai sbagliato, riprova o visualizza la soluzione :("];
Print[endgame];
Chess[ShowBoard->board,Interact->False];
]

(* Inserimento del nome da input utente
Controllo che la stringa non sia vuota o che non contenga solo whitespace *)
While[True,
 nomeUtente = InputString["Inserisci il tuo nome:"];
 If[!StringMatchQ[StringTrim[nomeUtente]][""], Break[]];
 ]
 (*Nel nome vengono rimpossi i whitespace *)
 StringReplace[nomeUtente," "-> ""] "sta giocando!"

board = Startposition;
Chess[ShowBoard->Interactive]
newBoardBtn=Button["Nuova scacchiera", board=generateNewChessBoard[]];
backBtn=Button["Back",Move[MoveFromPGN[filepgn[[Length[Movelist]-1]]][[1]]]]; (*non funziona ancora*)
restartBtn=Button["Restart",Chess[ShowBoard->Startposition,Interact->True]];
checkBtn=Button["Verifica mossa", checkMove[]];
showSolutionBtn=Button["Mostra soluzione", Print["La mossa corretta \[EGrave] " <> correctMove]];
(*TODO: capire perch\[EGrave] stampa prima showSolutionBtn degli altri*)
newBoardBtn restartBtn checkBtn showSolutionBtn backBtn

(*stringa che mostra chi deve giocare*)
Dynamic@whoIsPlaying
(*Lista di mosse della partita --> dalla quale prendere ultima mossa per confronto*)
Dynamic@filepgn




(*
PROVE per delimitatori
For[i=1, i<Dimensions[StringSplit[pgntosplit[[1]],", "]],i++, AppendTo[delList,ToString[i<>"."]]]
For[i=1, i<Dimensions[delList],i++,Print[delList[[i]]]]
delList*)

