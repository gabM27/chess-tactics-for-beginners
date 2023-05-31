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


filepgn;
pgntosplit;
moveToCheck;
lastgame;
whoIsPlaying = "";
endgame = "";
correctMove;
correctMovetoShow = "";
gameResult = 0;

generateNewChessBoard[] := Module[{randomNum, board},
  randomNum = RandomInteger[{1, 11715}];
  lastgame = randomNum;
  filepgn = PGNfile[randomNum]["PGN"];
  correctMove = Last[filepgn];
  correctMove = StringDrop[correctMove, -1];
  board = PGNconvert[filepgn];
  Chess[ShowBoard -> board, Interact -> True];
  If[StringMatchQ[PGNfile[randomNum]["Result"], "1-0"],
    whoIsPlaying = "mossa al BIANCO, trova lo scacco matto",
    whoIsPlaying = "mossa al NERO, trova lo scacco matto"];
    
  Move[MoveFromPGN[#][[1]]] & /@ Drop[board, Length[Movelist] - 1];
  
]

repeatChessBoard[] := Module[{board},
  filepgn = PGNfile[lastgame]["PGN"];
  correctMove = Last[filepgn];
  correctMove = StringDrop[correctMove, -1];
  board = PGNconvert[filepgn];
  Chess[ShowBoard -> board, Interact -> True];
  If[StringMatchQ[PGNfile[lastgame]["Result"], "1-0"],
    whoIsPlaying = "mossa al BIANCO, trova lo scacco matto",
    whoIsPlaying = "mossa al NERO, trova lo scacco matto"];
    
 
  Move[MoveFromPGN[#][[1]]] & /@ Drop[board, Length[Movelist] - 1];
]

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
    (gameResult=1; endgame = "MOSSA CORRETTA, BRAVO!";),(gameResult=0; endgame = "hai sbagliato, riprova o visualizza la soluzione :(";)];
  Delete[Movelist,-1];
  Chess[ShowBoard -> board, Interact -> False];
]

While[True,
  nomeUtente = InputString["Inserisci il tuo nome:"];
  If[! StringMatchQ[StringTrim[nomeUtente]][""], Break[]];
]

StringReplace[nomeUtente, " " -> ""] <> " sta giocando!"

board = Startposition;
Chess[ShowBoard -> Interactive]
newBoardBtn = Button["Nuova scacchiera", board = generateNewChessBoard[]; gameResult = 0;];
repeatBtn = Button["Rigioca Partita", board = repeatChessBoard[]];
backBtn = Button["Back", Move[MoveFromPGN[filepgn[[Length[Movelist] - 1]]][[1]]]];
restartBtn = Button["Restart", whoIsPlaying = ""; endgame = ""; correctMovetoShow =""; correctMove=""; Chess[ShowBoard -> Startposition, Interact -> True]];
checkBtn = Button["Verifica mossa", checkMove[]];
showSolutionBtn = Button["Mostra soluzione", correctMovetoShow = "La mossa corretta \[EGrave] " <> correctMove];

Column[{newBoardBtn, restartBtn, checkBtn, showSolutionBtn, backBtn,repeatBtn}]

Dynamic@whoIsPlaying
Dynamic@endgame
Dynamic@correctMovetoShow


(*
PROVE per delimitatori
For[i=1, i<Dimensions[StringSplit[pgntosplit[[1]],", "]],i++, AppendTo[delList,ToString[i<>"."]]]
For[i=1, i<Dimensions[delList],i++,Print[delList[[i]]]]
delList*)

