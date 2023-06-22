(* ::Package:: *)

(*
TODO: 
 - pulsante back
 - BUG --> restart + rigioca stessa partita rimane selezionata la mossa e non funziona correttamente NEL CASO IN CUI NON SI SIA CLICCATO VERIFICA MOSSA
  per risoluzione bug: fare Movelist = Most[Movelist]; da qualche parte (teoricamente quando si clicca restart MA SOLO DOPO aver mosso un pezzo, senn\[OGrave] si creano altri errori)
 - BUG --> cambiare il colore della scacchiera in una fase in cui lo stato della scacchiera \[EGrave] Interactive -> false la rende Interactive -> true e pu\[OGrave] generare problemi.
 
*)


(* carico il package *)
dir = NotebookDirectory[];
SetDirectory[NotebookDirectory[]];
Get[dir<>"Chess-master/Chess.wl"];


(* Carico il dataset e creo i file pgn *)
problems = Import["dataset.zip","*.txt"][[1]];
MakePGNfiles[problems];


filepgn;                    (* file PGN della partita*)
pgntosplit;                 (* variabile di appoggio per manipolare il file PGN *)
moveToCheck;                (* mossa che compie il giocatore e che deve essere controllata per verificare se corretta *)
lastgame;                   (* variabile utile a ricaricare l'ultima partita giocata*)
whoIsPlaying = "";          (* memorizza il nome del giocatore *)
endgame = "";               (* contiene il messaggio di successo o sconfitta di fine partita *)
correctMove;                (* mossa corretta, ovvero mossa che porta allo scacco matto *)
correctMoveToPrint = "";    (* formato stampa della mossa corretta *)
gameResult = 0;             (* partita vinta oppure persa *)
dimensionBoard = 240;       (* Var. per settare la dimensione della board*)
colorBoard=RGBColor[0.8196,0.5451,0.2784];     (* Var. per colore RGB della scacchiera, inizializzata a\[NonBreakingSpace]color\[NonBreakingSpace]default*)


generateNewChessBoard[] := Module[{randomNum, board},
  randomNum = RandomInteger[{1, 11715}];
  lastgame = randomNum;
  filepgn = PGNfile[randomNum]["PGN"];
  correctMove = Last[filepgn];
  correctMove = StringDrop[correctMove, -1];
  board = PGNconvert[filepgn];
  Chess[ShowBoard -> board, Interact -> True,ImageSize -> dimensionBoard,BoardColour -> colorBoard];
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
  Chess[ShowBoard -> board, Interact -> True,ImageSize -> dimensionBoard,BoardColour -> colorBoard];
  If[StringMatchQ[PGNfile[lastgame]["Result"], "1-0"],
    whoIsPlaying = "mossa al BIANCO, trova lo scacco matto",
    whoIsPlaying = "mossa al NERO, trova lo scacco matto"];
 
  Move[MoveFromPGN[#][[1]]] & /@ Drop[board, Length[Movelist] - 1];
]

backMove[] := Module[{board, filepgn},
   Drop[filepgn, -1];
   board = PGNconvert[filepgn];
   Chess[ShowBoard -> board, Interact -> True,ImageSize -> dimensionBoard,BoardColour -> colorBoard];
]

dropCharWhiteMove[] := Module[{},
correctMoveToPrint = correctMove;
If[StringMatchQ[PGNfile[lastgame]["Result"], "1-0"], (* Se la mossa \[EGrave] al bianco*)
correctMoveToPrint=StringDelete[correctMoveToPrint, DigitCharacter.. ~~ ".", IgnoreCase -> False];
];
]

(*Cambio dimensione alla scacchiera, 4 possibili dimensioni: 120,240,300,400*)
changeDimensionBoard := Module[{},

Switch[dimensionBoard,120,dimensionBoard=240,
					  240,dimensionBoard=300,
					  300,dimensionBoard=400,
					  400,dimensionBoard=120]
					 
Chess[ShowBoard -> board,ImageSize -> dimensionBoard,BoardColour -> colorBoard]
]
(*Cambio colore alla scacchiera*)
changeColorBoard := Module[{},
colorBoard=x;
Chess[ShowBoard -> board,ImageSize->dimensionBoard,BoardColour ->\[NonBreakingSpace]colorBoard]

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
    (gameResult=1; endgame = "MOSSA CORRETTA, BRAVO!";),(gameResult=0; endgame = "hai sbagliato, riprova o guarda la soluzione :(";)];
  Movelist = Most[Movelist];
  Chess[ShowBoard -> board, Interact -> False,ImageSize -> dimensionBoard,BoardColour -> colorBoard];
]

While[True,
  nomeUtente = InputString["Inserisci il tuo nome:"];
  If[! StringMatchQ[StringTrim[nomeUtente]][""], Break[]];
]
(*output soppresso perch\[EGrave] \[EGrave] nella nuova interfaccia*)
StringReplace[nomeUtente, " " -> ""] <> " sta giocando!";

board = Startposition;

Chess[ShowBoard -> Interactive,ImageSize -> dimensionBoard,BoardColour -> colorBoard]
Chess[ShowBoard -> board, Interact -> False,ImageSize -> dimensionBoard,BoardColour -> colorBoard]; (*LASCIARE SENNO RIMANE INTERACT -> TRUE*)

(* boolean di attivazione dei pulsanti*)
restartEnabled = False;
(* funzioni dei pulsanti*)
newBoardBtn = Button["Nuova scacchiera", 
	board = generateNewChessBoard[]; 
	gameResult = 0;
	restartEnabled = True;];
	
repeatBtn = Button["Rigioca Partita", 
	board = repeatChessBoard[]];
	
backBtn = Button["Back",
	board = backMove[]];

restartBtn = Button["Restart",
	whoIsPlaying = ""; 
	endgame = ""; 
	correctMoveToPrint =""; 
	correctMove=""; 
	restartEnabled = False;
	Chess[ShowBoard -> Startposition, Interact -> False,ImageSize -> dimensionBoard,BoardColour -> colorBoard],
		Enabled->Dynamic@restartEnabled];

checkBtn = Button["Verifica mossa", 
	checkMove[] ];
	
showSolutionBtn = Button["Mostra soluzione", 
	dropCharWhiteMove[]; "La mossa corretta \[EGrave] " <> correctMoveToPrint ];
	
changeColorBtn = Button["Colora Scacchiera",
	changeColorBoard[]];

changeSizeBtn = Button["Dimensione Scacchiera", 
	changeDimensionBoard[]];
	
GraphicsGrid[
{
{Image[CompressedData["
1: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
"], "Byte", ColorSpace -> "RGB", ImageResolution -> {96, 96}, Interleaving -> True], " sta giocando"Dynamic@nomeUtente,Image[CompressedData["
1:eJztnQe4VNX19kcSoymKohHpShWQEpBOQBBRCEUREJEOAhfpoCAgUhSRpiK9
g/ReBSEovUhTQNql6KUJEYIYNSbC+X7/836zns3MHeQOXrhJZj0P8wxzz5yz
9yrvetdu82CTdjWapwoEAp1u56VG4y7lOnZs/Mozd/GfWm07xbVo+0KzSm07
v9DihY7Fm/yKD/v9/3//996LSUxiEpOYxCQmMYlJTGISk5j8nPz73//m9fLl
y/rvv/71L95funRJ//3nP/+pNz/++ONNad7/oJjyf/rpJ/fzb7/9FmN98803
if41JskkFhqeEwUon0jZsmXL1q1bN27c+N133128eNEMF5MbILILVtB/T506
xeucOXMWL1784Ycfen688NcLFy7cxEb+T4miw6KAuFi4cOGDDz549913//GP
f5wwYcKyZctuagP/5+SHH36w99hl9uzZixYtSpMmjSyCdQgWPrcgiknyifK1
CJVihNfx48e///77gaCM9wVkiyX3GyDCKzm/XtH82LFjJ06ciC1uu+02XidN
mjRq1CgvRrd+UTFlEgK8VyrXh4Is8SvewKymT58+fPjw/5vp+dWvbr/9diyC
jTzfWFaeSFy4i0mSxLL2ZV/s/blz5xQmWMSueffdd0klt9xyi1Br9OjRihET
M2iMEkctiarOTPP999+rclcIEBHDhg279dZbU6VKBXDxX/KI7oD5MITKxn/8
4x+eX/LHIuUXEczxoy8jR45s3Lhx69atO3bs2KBBg7Zt25YvX75UqVKW2cuV
K1ekSJHmzZu3aNGiadOmnTp1atOmjUEZFnHLzJgkVS4HhaxB3Xf69Ok///nP
0jwpQ2/Aq9/+9rdKIsTIr3/9az4kXvRXPueVaiVPnjzx8fEhmSUmSRXARxZB
k9TgZ8+eLVmyJAAlVf/+978PJCaYJpUvXJk6dWo+eeCBBzDKF198ERuBjE4E
LO6gLmhz8eJF7AJS5cyZM23atNmyZcuVK1fWrFnv9QXNEx3Eyx133HHPPfdQ
KqZLl46akSvTp09f2ZdYBolaLgXFxXwSOhaJi4vD27NkyZIjRw4MkT179vvu
u0+BIIDCEIQGRsmcOTO20JvatWvXqlUL3IvFyC8rRArYdfLkyaO+HDt2LCEh
oVevXoMGDTLI6tevX8+ePQ8cOEDW2Ldv36FDhwCrM2fOEGJiaDFJDnHp8YQJ
E6gQiQuleI2iqJz8yZFE4y4m1ykaP7QqXjJmzJgRI0YIssjmVIjjxo2LhcON
ESziFhQaTsQcEydOFBl2LSLOzJsQC8YkuWWsL6LEABfvqQdvdqP+V0RBoeFH
RIFA1jDUwiKjRo0Cx7QiItE7xOIluWXatGlTpkxJkyYNBSMFCO8BMWV2N5vH
DJHcYnNS6J+goDD83e9+h0UmT54M+0o0s9uYzA1v7H+DmJO7NZ3pmVLxxIkT
33zzDby3efPmDRs2pAykVKRUr1evXs2aNadPn05+p2Cx7+o+7jhATKIQU+C/
fOHN3//+d9nl3LlzvFm8eHHFihWrVq1KIZ8pUyZqecxRt27dGTNmzJ49++zZ
s1z59ddf627U7DevK//ZErJw0X3/ww8/rFy5csOGDXnz5n3ooYdAKqrC3/zm
N6K+ZHaN+pJTeF+uXDmCRUuGvNhCx+sQm7oN+Ry7kDWIhbt9wRC33XYbGeTO
O+8kRnLmzJk7d24+d8eEs2XLxp/ee++9pUuXytCauopJkkSqM6b0/fffe74m
Aa558+aRL1Ay2tZUiNJHnjx5smbNiv4pFdOmTatguffee++///6MGTOOHDkS
DubFlkNEK0oZltz1YXx8PGCVPXt2DfPmyJEDzeP8n3322d69e0kWp0+fJt2v
Xr16yZIlbdq0qV+/vsIE0+TPnx+rrV27NpZKohPZIgT2ydRoXtGBRfB88OrV
V1/FIps3b4ZWnTp16syZM7t27froo49at26NRbhGq4aIo/Tp0/O5F0Ot6xBl
EyEYIYDbN2jQAA1nyJCBbE4grFq1ykUhjUMqvjANf+rRowepnxghswBxtWrV
0kjXzepR8om5mSAF1bkIQ/6FdoIwW7ZsmTp1KjX1wIEDceY+ffr079+/X79+
Y8eOXbZs2QcffPD555/v3r1bHNW+rqyhMV59vm/fPqIDP8fnb731VrhulSpV
PL8q+e677/SVixcv/u1vf/vqq69om+mcD6tVq1a2bFllnHTp0hFQ+/fv9xzy
pgXb7kp7rH/+/PkDBw6MHz9+4cKFNPuNN97o1KkTcffyyy/37Nnz9ddf509/
/etfYXGQ8MOHD7tW1iINz/cldeGG+QAFGo+2qVI1A6CmSUD6Jl9m+DJq1Chs
8eabb/JK7/gvPcXJv/zyy4MHD1JieM4aRWnSVu3yBotgO7I2yINua9eujZ7R
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
ihQRq5Tgh0uXLuVxKoEDftJ0Bw+TKvZFEQNbJYU3PvLII6CWxpwJE7JY1qxZ
deKZUF0H12hhKv3FHFiNBtNsjQBHqf0IYnwSbsD9Z86cuXr1ahuy1lJYnu6O
mwE4NsEXhUX4ypEjRyi+cuTIIRMT+0ToggULPGedCQYCP0kxqIVI0aNtWjOp
3UwVFPdDdQqjf/bZZ1Bfc7NqvgSC3KlmzZrghk5/4q8U7DgtDbblcFErP1z0
xIBvDvI1FauGl03WrVu3fv16KiYhKt5C+2m8jS4mdWJXlEb1Pt10ayiqM4G5
zZLjjadPnybF16hRQ9fIeaIoScJZtETTRsuWLdOKRz2XRNm6dWtTtQp84zAw
Afc04Ojy2s8KvYaNCzc0iqjzIbXWyzoFNyZUuczGTqNY66iOg4dUu4Ggo2Ia
6KWtZtdIOEjCs8j7zz33nPquTBoFdhlYBRKzDtl80aJF6hTdxxxxcXGWHVr4
YvUyvF1rGq9ntCSSaPwNwQ2oQbRCzHOwiGqdwLHBh7JlyxYsWFAHhSXJCiY2
9kIHZ82apUJYjSGZohnyi9DDMOTw4cNECjAip5VDmlu69UXgSmNdi+HUNS1o
8YKlH8y2b9++dkHXrl27dOliE9m4KCzLTk0JXLVQCpk91HipCJJ13GoQvWnQ
oAFYrTkRwYVQSMvwNJ4jvk2XqWF79eoVouSkphJztrlz5+q2cjYgcfny5VjE
FpVJP7SNrKotG7aWzNQFIbHVa9Zl/Vclv8iVRBilAHHTyqBBgwYOHGgtfMMX
e0Q3XwycO3fujIHUBr1qujBR4qFHGMyGXCB3shwtG1GHQm4tfWhZGqpYs2bN
Rx99RP1uiwyRkSNHDh06VGs7tW4tqRax1d2ev8K2pi+BYJEF7QSjSJ1bt24N
WVyxY8cOoFUJV3a0+Wtzv0iV2tWDBaXhZgSCvIXwJwQGDBhgf+W/VB/WzWbN
moFpIRzDJg4Svb+thDc3kFbd+QUKUnCbZiQkJMgc7saEjRs3AvJaCaOO8x6W
uH//fpvp865jU4lItefzFpgDbBPOb4706KOPFitWDKPIdjY2C65ixOeff95m
V221v7mZNjJYgJhO5MDKIKq/tEFAHQScO3bsaM3D6wYPHmzWBMR69+7t+b4H
T8aFAHn5s8h5+ASum7ZCxKUlqjh0WfPmzXFCkXDP2RoGWMG6jdtIQHsIBmzE
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
"], "Byte", ColorSpace -> "RGB", ImageResolution -> {96, 96}, Interleaving -> True]},
{newBoardBtn, restartBtn, checkBtn },
{showSolutionBtn, backBtn,repeatBtn},
{Dynamic@whoIsPlaying,Dynamic@endgame,Dynamic@correctMoveToPrint},
{changeSizeBtn, changeColorBtn,ColorSetter[Dynamic[x]]  } (*ColorSetter permette di scegliere un colore RGB,per colorare\[NonBreakingSpace]la\[NonBreakingSpace]scacchiera*)
}, Frame->All , AspectRatio->2/5]


(*output soppresso perch\[EGrave] \[EGrave] nella nuova interfaccia*)
Dynamic@whoIsPlaying;
Dynamic@endgame;
Dynamic@correctMoveToPrint;
