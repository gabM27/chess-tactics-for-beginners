(* ::Package:: *)

(*
TODO: 
 - Disabilitare la possibilit\[AGrave] di muovere le pedine dell'avversario dopo che abbiamo fatto la mossa.
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
nomeUtente="";              (*Variabile usata per memorizzare il nickname dell'utente che sta giocando*)
whoIsPlaying = "";          (* memorizza il nome del giocatore *)
endgame = "";               (* contiene il messaggio di successo o sconfitta di fine partita *)
correctMove;                (* mossa corretta, ovvero mossa che porta allo scacco matto *)
correctMoveToPrint = "";    (* formato stampa della mossa corretta *)
gameResult = 0;             (* partita vinta oppure persa *)
dimensionBoard = 240;       (* Var. per settare la dimensione della board*)
checkMovelist;
colorBoard=RGBColor[0.8196,0.5451,0.2784];     (* Var. per colore RGB della scacchiera, inizializzata a\[NonBreakingSpace]color\[NonBreakingSpace]default*)

(* boolean di attivazione dei pulsanti*)
newBoardEnabled = True;
restartEnabled = False;
repeatEnabled = False;
backEnabled = False;
checkMoveEnabled = False;
showSolutionEnabled = False;
changeColorEnabled = True;
resetColorEnabled = False;

(* funzione che viene attivata una volta cliccato il button "Nuova Scacchiera":
- Genera un numero casuale intero compreso tra 1 e 11715 (num di partite del dataset)
- Estrae la partita dal dataset in base al numero generato 
- Viene memorizzata l'ultima mossa della partita (correctMove) che servir\[AGrave] per il controllo del vincitore
- Viene caricata la partita nella scacchiera traimite Chess[...]
- A seconda degli ultimi 3 caratteri presenti in ogni partita ("1-0" o "0-1") l'utente giocher\[AGrave] con i bianchi o con i neri
- Vengono poi mosse tutte le pedine tramite Move[...] fino alla penultima mossa della partita
- Salviamo le mosse che sono state fatte per la partita *)

generateNewChessBoard[] := Module[{randomNum, board},
  randomNum = RandomInteger[{1, 11715}];
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
]
(* Funzione che viene invocata al  click del button "Rigioca Partita":
- Tramite la variabile 'lastgame' sappiamo qual'\[EGrave] l'ultima partita giocata dall'utente e la mossa corretta (quella finale)
- Carichiamo nella board la partita in questione e poi la mostriamo nella scacchiera tramite Chess[...]
- Controlliamo sempre tramite "1-0" o "0-1" quale pedine dovr\[AGrave] muovere l'utente
- Mostriamo nella scacchiera le pedine nelle penultime posizioni della partita *)

repeatChessBoard[] := Module[{board},
  filepgn = PGNfile[lastgame]["PGN"];
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
					  400,dimensionBoard=120]
					 
Chess[ShowBoard -> board,ImageSize -> dimensionBoard,BoardColour -> colorBoard]
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
(* Questo \[EGrave] il messaggio pop-up della scelta del nome dell'utente all'avvio del programma:
- L'utente sar\[AGrave] obbligato a mettere un nome (che non sia una stringa vuota) ed eventuali spazi verranno eliminati restituendo un'unica stringa *)
While[True,
  nomeUtente = InputString["Inserisci il tuo nome:"];
  If[! StringMatchQ[StringTrim[nomeUtente]][""], Break[]];
]
nomeUtente=StringReplace[nomeUtente, " " -> ""];


board = Startposition; (* La board viene settata con le pedine in posizioni specifiche (iniziali, in caso di schermata iniziale, o penultime)*)
(*Parametri del comando Chess[]:
- ShowBoard -> serve per specifiare l'interazione con la scacchiera visualizzata (
			  Interactive: possibilit\[AGrave] di interagire con essa e muovere le pedine
			  PGN-values (board) le pedine sono mosse tenendo conto dei valori PGN specificati
-ImageSize -> Tramite un valore numerico intero specifico la dimensione della scacchiera
-BoardColour -> Tramite valore RGBColor vado ad applicare un determinato colore alla scacchiera
)*)
Chess[ShowBoard -> Interactive,ImageSize -> dimensionBoard,BoardColour -> colorBoard]
Chess[ShowBoard -> board, Interact -> False,ImageSize -> dimensionBoard,BoardColour -> colorBoard]; (*expr per disabilitare l'interazione con la scacchiera*)

(* funzioni dei pulsanti*)
newBoardBtn = Button["Nuova Scacchiera", 
	board = generateNewChessBoard[]; 
	gameResult = 0;
	restartEnabled = False;
	repeatEnabled = False;
	backEnabled = True;
	checkMoveEnabled = True;
	showSolutionEnabled = True;
	newBoardEnabled = False;
	resetColorEnabled = False;
	changeColorEnabled = False;,
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
	newBoardEnabled = False;,
		Enabled->Dynamic@repeatEnabled];
	

restartBtn = Button["Restart",
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
	resetColorEnabled = True;
	Chess[ShowBoard -> Startposition, Interact -> False,ImageSize -> dimensionBoard,BoardColour -> colorBoard];,
		Enabled->Dynamic@restartEnabled];

checkBtn = Button["Verifica Mossa", 
	checkMove[];
	restartEnabled = True;
	checkMoveEnabled = False;
	,
		Enabled->Dynamic@checkMoveEnabled ];
	
showSolutionBtn = Button["Mostra Soluzione", 
	dropCharWhiteMove[]; showSolutionEnabled = False; "La mossa corretta \[EGrave] " <> correctMoveToPrint,
		Enabled->Dynamic@showSolutionEnabled ];
	
changeColorBtn = Button["Colora Scacchiera:",
	changeColorBoard[];
	resetColorEnabled = True;,
		Enabled->Dynamic@changeColorEnabled];

resetColorBtn = Button["Reset Colore", resetColorBoard[];
resetColorEnabled = False;,
		Enabled->Dynamic@resetColorEnabled];

changeSizeBtn = Button["Dimensione Scacchiera", 
	changeDimensionBoard[]];
	
(* GraphicsGrid per generare tabella grafica dei comandi di gioco*)
(* All'interno dei CompressData sono presenti le immagini dei pezzi di scacchi
che vengono mostrati in alcune celle della tabella, al solo scopo decorativo *)
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
{showSolutionBtn,Image[CompressedData["
1:eJzt3H/sd2Vdx3EB5UekiL9SS0SwaYaF1bKyTUw3p7lK1MxWKQaYWYigM9zS
2eYya6u1MudMx5psrJw1V6IusRIXNtcPXJmaKFkkmRj+4DefaPfG7t3353X5
/X7uc53rnOt6PDYY8Nd9vT/nvK8n/5xHvfjlZ59/9L3uda+Lj7/7b2ef8+qn
XHTROZc85/53/8vzLrj4pS+54Lxzn3HBK897yXkX/cCLj7n7P/7D0Qf++v9/
3gAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAABT
uyv7avaF7Ibsa1nrMQAAFekNAKA2vQEA1KY3AIDa9AYAUJveAABq0xsAQG16
AwCoTW8AAHt3W/bh7OLsrOyMnfxIdkl2dXZH1vrXAIA+6Q29AQC16Q29AQC1
6Q29AQC16Q29AQC16Q29AQC16Q29AQC16Q29AQC16Q29AQCTKHzY86Lsgdm9
Fu9bstdmN2atf0MAWDq9oTcAoDa9oTcAoDa9oTcAoDa9oTcAoDa9oTcAoDa9
oTcAoDa9oTcAoDa9oTcAYBL/lT03Oyor3OYnZGdmZ2c/kT0+OzYr/OGPyc7N
pAgAbPSG3gCA+vSG3gCA2vSG3gCA2vSG3gCA2vSG3gCA2vSG3gCA2vSG3gCA
2vSG3gCASdyWvTwrXL4FZ2SXZTdkt+/k+uxt2aOz3VKk8FXSO7PWzwsA7EJv
6A0AqE1v6A0AqE1v6A0AqE1v6A0AqE1v6A0AqE1v6A0AqE1v6A0AqE1v6A0A
qO2D2cnZblHxsaz1GL6xK7PTssKgHppdnbUeAwDsQm/skd4AgJ3pjT3SGwCw
M72xR3oDAHamN/ZIbwDAzvTGHukNANiZ3tgjvQEAO9Mbe6Q3AGBnemOP9AYA
lN2V/WJWuCuPy96ZtR5DLb+f3TsrjPfVWeuzAkCkN6rSGwCw0RuV6Q0A2OiN
yvQGAGz0RmV6AwA2eqMyvQEAG71Rmd4AgI3eqExvAMBGb1SmNwDgbjdmT8wK
F+J3Z1/IWo+hls9l354VxvvU7GtZ6zEAMDq9UZXeAICN3qhMbwDARm9UpjcA
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
"], "Byte", ColorSpace -> "RGB", ImageResolution -> {144, 144}, Interleaving -> True, MetaInformation -> <|"Exif" -> <|"ImageWidth" -> 720, "ImageLength" -> 720, "XResolution" -> 144, "YResolution" -> 144, "ResolutionUnit" -> "Inch", "Software" -> "Created with the Wolfram Language : www.wolfram.com", "DateTime" -> DateObject[{2023, 6, 22, 15, 3, 39.}, "Instant", "Gregorian", 2.], "TimeZoneOffset" -> 2|>, "Comments" -> <|"Software" -> "Created with the Wolfram Language : www.wolfram.com", "Creation Time" -> DateObject[{2023, 6, 22, 15, 3, 39.}, "Instant", "Gregorian", None]|>|>],repeatBtn},
{Dynamic@whoIsPlaying,Dynamic@endgame,Dynamic@correctMoveToPrint},
{changeSizeBtn, changeColorBtn,resetColorBtn },
{Image[CompressedData["
1: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
"], "Byte", ColorSpace -> "RGB", ImageResolution -> {144, 144}, Interleaving -> True, MetaInformation -> <|"Exif" -> <|"ImageWidth" -> 160, "ImageLength" -> 160, "XResolution" -> 144, "YResolution" -> 144, "ResolutionUnit" -> "Inch", "Software" -> "Created with the Wolfram Language : www.wolfram.com", "DateTime" -> DateObject[{2023, 6, 22, 10, 24, 38.}, "Instant", "Gregorian", 2.], "TimeZoneOffset" -> 2|>, "Comments" -> <|"Software" -> "Created with the Wolfram Language : www.wolfram.com", "Creation Time" -> DateObject[{2023, 6, 22, 10, 24, 38.}, "Instant", "Gregorian", None]|>|>] , 
ColorSetter[Dynamic[selectedColor]] (*ColorSetter permette di scegliere un colore RGB,per colorare\[NonBreakingSpace]la\[NonBreakingSpace]scacchiera*),
Image[CompressedData["
1:eJzt3XmwfEdZxvGfiUtckF1EUURZBERQRAMRBQVNJCAEMICAJiREjCZm0RgB
MSggiwu4QlAQRbZyCRSKgiVEZRcimxoEcQVcQAEVhADh+fjHW9XVhzNz752Z
e+xvFVRyp0/POf2c9HS/W9/g1LNPevBRR44cOe+YK//vpFMuuOO5555y4T2v
duW/3Pus8x5yxlmnn3bCWeeffsbp5x576tFX/vHp//e/T/zzxwaDwWAwGAwG
g8FgMBgMBoPBVvlA+Ifwj+G/w7bva7A/DH2Xx3vDL4RvCjcMNwonhGcE6m/7
fgerMfTdBT4S/jW8I/xL8Pf1+nx3uF/41HCkw2eE7w3/Edb7xg+HdwVP8e/h
irBen8tg6LtULgtnhluFG4SvDKeHV4f5fRrPHwxVxy8Mdwp3DNcJPj06PCqs
+l0vCw8MNw9fEr46nBPeElYfocPN0Hep/EGwzunNnDBWvxvm9PzmcL2gh1uG
V4X/CXZGdLlJ0NL9/G2Y/paPhqeHzw/TT/EV4dKwD8O38wx9l8pfB8/r2T89
UOH4YGaz8tHGvuYvwnT/zwnmW9f+ROi1rzP5MeH3w/S3vDyY8137meE2wVN4
xrq68+nfhXXG7jAw9N19fe1c/jeYqeZf+8jgeY8K5wc7mg+Gfws/Fur40GK6
/18JdW785dBr/7igpbfiuaHX3prqjOAq76F+3hM8xT8HOy8tPyX8bJg/YjDa
dmGrXrsqQ9/l6ftX4dHh3uGu4fvC7wWrl14PnvouwfOa3/4ytO3NY2Zm7e8Q
pm2J7sRM66qzQvseGqvvClpeJUyvgij4NcFVtwgsG237Pw/XCtrfN0xbb4zV
Hwb3b7TvFahAkV4PqzL0xfL0tTe5cTjS4XPC2cEItP3Q5RuDq+xNzGNte7M0
u4f21ie8Br271dtXBVd9XjArWqG9PhilqwUtbx/YFXv9+x1hx3DV7cL7Qtv+
beGLgvYnhg+Ftr1+LgifG3pj/uXhktC72zkMfStL0vdPwxcHfVqBGDH3/FnB
p9ZLF4Z2/vEXNn/tjS07Q/vtrw3VimiOMndN3/mvhTo+1mnmyWuGuofylzlW
FG8pC6drjU/PAun34rOD9tZmbUsrtx8PdVVp58ViYzTqnV8//FmYvvOWoW/L
MvQ1e1gJ6MeK5aHhTeFvwm+HOl9Rnz2w7ZlN79NCnRV/K7wmPD/cOdQ35+fC
nPu3m/jFUG2VFbsVNsnfCPM9kjwRtTfv3guDp3h28JtSlfKkbZ9vDPVu3RuL
jdE28g8LetPSfzW9Ob9l6DvNYdf37YG3Tg/2Jv8Z2vZPDlTQ/qdC29LqyDq/
jg8rgRm77m7wbYHff879w4xnTfWg4NfEPsi+4/Iwv0+I0/Nm1vt0556C3bV+
ai/W299dHLx102NIhfobQSmqzbn/oe80h11fu/JrBD2cEnrt/zjUtdZ5odee
7eIBoa49Kv7+HeGtYc6d93h4qP0/PuylTzMqi037TsK79ODQ2wniouAq78YL
Qq89RbSnFNXm3PnQdw6HV1+2ry8IevAUPQvks0Jd1VvnT3+LmepFwcrttsFu
RZTOeju7irXWPUIdeRE1e49/swNiqzHOnsIb9ZIwJ5b+icG9maWfEtqWVGAn
0Z5S8y2WQ9/5HEZ93x/qDsWagfeNBU8bMXKix7VkqxSBs+pYfU+oKtB91X4q
VmXeltozFfYSE+vdMPfWnh8SVu2NNenqQT8sn3Zb7JZsv/aYteW3hPlR+kPf
ORxefWFnbYWgH2se1nvfXmNUcHJYL+PjtFB7E6vT877Nwf7o2qH2zJIgXmi9
ntkcvizUnj3Fqr2ZdU8NtTfxe95Go+G/IJ+ywU7HJPQY+k5z2PW1MnlsqH60
FisrVghPvep3odWX3aNn05sDC2q1iIJ9jw99vZ6tf6pVZy/6ws7x7qG954r5
WVwQpVb9rqHvNIddX4hjETV6/2B+Fv9pxvCXHw5z/Hc9Wn3BsrdedBk/fn1b
auTtk8Kqfdrv1B3KfulrluZjvV3BaBtndiEr2L1H3A19W5akb0VvImfskn49
mOusxH4nrNd/T18+9FUjyuxfjIZ+RPuIiPOX9fYyvJ81am6/9DV6RtLaSdSB
0TbyBxclO/TFUvVteWmoPoKvD+vtaKq+1g+elL1uvn8fbBfHhjrP1xghlpn/
CvN7/tGgByvPGgu0nr7vDMcF/Vw1vCKs2tt+MfRdtr6i068b3JX9glXNqr1V
fUW2fHPwF/ZSdtE5vbFdVAsMn2CNrmGdmJ/zxdpZ49u/PdQ80/X0NWJ1t+VX
aS/7zb0z9F22vuK9a74nxG2+Lszvreor/uRngohQM+GfhDm92UFY+7EViNx7
XtCn+X9+n3rgzRfPwMpR12yr6muUjFgdQ3vP6Xj7g2bou2x97fTVLTnSwIc+
nXdWqfrKZOF//NLg73I35vTGduEqMQNvCDxu3hYrt18N072Jnq13eOvAf1Ej
hOfra2SMUjt67L17sRftnaHvsvWVd/ndob1Dnqz53oE6et8Q7HFEt/q7Sg7T
sWpgu3DVzYJfE/VFa3UXOebTvYnxqzHD7LF2gjcNq+prZKq/r9LLZNkkQ99l
64safcp6X2Nl7dnt36f7qfqKHjeDsdHpWezotEebvaJGDakmaq7zaY0Pl10y
nd/xS8H+hTVDZpzKiqvq21oz2IjquE1XhtkkQ99l6/vU4K7kMJ4UjirMqf7X
6ksLc2DN9f7O0LO0s1fUyBnZKLWN+oc+tTrq2VTdgzdEe2s/MW/m/FX1NRpG
xhpPzg6bhr/Icv1kY78Jhr7L1leMul2/eYbiduieWj64rO1eP62+NU7Pzqj2
phph249Y0xoZqMp3bfOE4NPp/A4W/pqt85jg01XXV0agVmwQu8vTyqtilhYb
P2f8D5qh77L1ZYWo/m7ZjiKxa6akCJ9evkarb/UmWM/wmmnDetn2w15hlmOf
bOPtrdncm/hA8cBtb/KpfaPs9fomzNfXUxsBLVlNWTjlg9cKGPLXVtXiIBj6
LltfFoNaQ+mHgkzkujIxe/d2N9P66k2muTZ2QG1tolo1VO5VW0WtfSdVRKxt
aFd/ZeykqsV1vr6eusZCyCvxXDUOUG9zbDibYei7bH2NcN2t3zOIcBNDXvOh
ZFu09z+tL/jotTFXixHyKRvF3YI2vACtl40u1S8gs6ZWWREbz65iL9NWK52j
ryf11PXO5ZZqUzNT7L9259Seoe+y9TUmainUUZXPyApRcz+tfNoc8Dn68sfV
iKBaC6JVjd2gtYSwVfLBtffszax+E/73Nh9tjr6etNZO0UZeSWtNFQG4O2fr
DH2XrS+MszsU1VZHw2q/eupbi8ccfenC/qml6CAWe7VDefN9+iOhd89qomop
E9NKTFRbjZmxr2nfk2l9W2uGE0PqWQP/FGqEntXpfumyXwx9l61vjYexC1A3
qbb5yVCjQM1F5qhq8+/pC5YT1gBWAlFzVkTsFb5l2krPbukbraNk0llHmVH1
z/7Q9tDq6yk8UY2l11sbOSy+rr6TPx/WlOHAGPouW1+5UUbDs6jHUtvYKdSc
EbZ0o1drlUzr6xSqGmPDY1htifwLfA2uMrdXP36NoXUVK0e1yYjtUZOwvZNW
X0/hiaq/3lO3u8JqKTV688/z2iRD32XrK2uyVnLorW3aZ//aUEe1+gftI0TC
sKW0tYmsjkTf+Yu1nHWdWsGqtLExPi1Yj9VKblZr1Rrz/cGdf7TgL62+nsIT
te9wOxq8JFoavVVP1dwMQ99l66s+bd1TtBY/UK1WCbZSqoqz0Rl/tjs5IOJm
nxnMY/WqCnug2Ne2Wq/ZW85prcJdEblqxfVHwRslIp1H3lObw6uatYq+E4V6
WajOGNLS7ml+pd9NMvRdtr7t2U9fF3o1AK186gkaFTNtnbEr5jH52tVWWakZ
4v5iB1TrDhnPXg1/6zc1EttzYK3KqNM769ZVvSqp7ZlfRq93xtB2GfouW9/W
N+fp6g6l0jtLHWwUdZYTtdJWWqt2+x6sByreOzN9unoYtKl1j83z9RehfWcq
LI09T4G4iOoNmRNpvy2GvsvWF/VEVDgjUkS33Yo9jl2PbM16ilYLbwX7iQxr
q6xeVXz4VEu17o0zmwk7YW9ur3h/7Lacc8cSO+f8uHqygF2VaByWWyvM+v6s
V69pkwx9l62veqS9UTKeau59a7Ci6O1xxOzRtH6LeBuqsaI8sOAvrurVN7Br
Y9t/RLDOYZdgS+SFFAmvuhqNvJnsEr3Kq2wmVoA/EGS5qh9Vo3wrNXJ+Nxn6
LltflofqAWwr6E5jX0MpFRXmfG9rOVwVuxUncJlFp+vu8vXzS6r8ueqTVlzr
pLz17n8zDH2Xra8sklpzwAxsjlKjjKWRTV7Wpzg3eeL2U9utK7IqvIfWXX53
WE7M3qId/LN1msj26tfoRUTsGkPfZetrz17Po2GXoLuduyp/IuLUvD1cak4j
j4yPgKfv0uCfvQnWftULyR5r9Lb9BFMMfZetL8St1Z27E+LmVwetWDV5N6x8
VEVQh0FEK0/iK4PxNNfJAXlhB5/KcHGVKAW92RP5Fu8ky4w7WW8tpwd2SCPD
tumM3VV72xZD3x7L0JdVoVb35U2ze6ot2Rm0d/54jVBlqXAWKvsDr70oGtXG
2Ex4ENgNrO7YTGpVpRafWtu4Sg9607MVkTUhywx12CvU+laP4sXBW2Gf1XoW
RAVUy6r1mPabVWl9hr7L1hdWEdXDLhJGXA27332DHGq+/mmvwW5Sa0HY9Yht
EFnkXZUnW+Pu7JhUp9m2Vusw9F22vnY9tf6tcZijoFWHEeA9tOfi71MVn/Ve
hIxqn48M/Pjs/9YtF3dQ28TZyj8dVAX0u6Aqml8HvgbVIdhkWDDUTWrj91r1
zf/Vhilaby+ny22Xoe+y9QUPfhvDRmXxtKp9ygGnjkwxuxXZmuLba8T7XrwJ
8/EtvAliXNlk5MjIAOWplBfmrbBqEtvQej+tDOd7T3aZoe+y9bVHqPVDzL0q
pRglqm1Gr82ghhJ7o/m/zuGimJbxvEPfZesLkWmezhqjtXUcBN4uVpQrOhz0
OMsxrysr1cIP7hs3z9B32fqy59ezle1l2pZGm5eNH4HFj++AR0Aci/WMSFdn
IquXYt/BfqhCwskd7hPsgNRFsdsSl+4OvZnySS8JzsNSnVi+Nr9JL47dvXlq
NhCWzIMf9c0x9F22vkajZmkZWxHvYtdFh1qJOTOLH4HFj+XfGzInJ2V/qbVW
WFxZU8Wxs7qooXRRUE1UJeHqDRSrv+pZ1bvP0HfZ+rIJiCP1pOx1RmlOthes
UthGZIio8Wvc2A2MuZxr33jrDj7V0lV60JuefUutMjrnPsU26KHWb+Fh2c0M
0L0w9F22vux76gb3RoNXXdysept8E3wHrAS/GfjUnHyh1t/l4R1B5J7KJyIH
3tPBp1paKelBZUU9vzz4Rns6d+KuHhDYV70hPA41NqnSq1lx2Bn6Lltf1Aqi
7HUyIp20KNvLOLPk71qN3BZ36G5VCrWmkrdij3a4MkD3wtB3qfqKa5Ud6Umd
H8e/sO27Oyjsg6pvVITAenHCu8zQd9n6ihi/dvCkIl7m1xKpudXGh8efD04l
BBG2ZkhxBWyJl3aQy6alXwd2GBEF5ls7Ox7M3omWPczbssk8NQvP7pxdtV8M
fZetrzPjqn1APJtPrVLsWd4SVO4VBSeG3FpF/SXzvJwstcXMgXLZ7FBkb7Gi
1Aj2+heWRi1dxd6ior7qEKLpWCBl2fBfsEPyO6heaI+mnltdGar/4KlVfHpG
2J4a+8/Qd6n6mldr7X3xZvTl42PHYLuTFcKmt3k/wqq4Q0/kHZNBw0vCj+98
nGqDPSMsIz5n6Ltsfa1P6imNZmkz5HwFWfjNoqJqa87X8UHFMG8LW8qZ4ZwO
Ylm9e06zckqXehFqjJuf+R1YUHk35tx5Vb+2590wMtvWZ68MfZetLy9Arw5Y
xcqHlf7EoDq3/CyROeLJ3xZqzrVcGHb79eY9V+lBb3ZhVOB3sMuz9pMTystg
r+d986TT6mtjZA5izDfJ0HfZ+qqBX5/OHsE6SuyKWHd1ElgVDlfFFVYX0YDi
AFWoMNub29sYBvWUtn3ve2Xou1R9WfNkcNfn4g2X7ywOdtt3elDIUmH98NR1
HEQYrmrz3B2GvsvWl72xnkZX11H2CPYv4tUvC9Yzh+up6eiXRTwPy4YMdPF7
bVY7+6pZfdtPsA5D32Xry8ZuN8H+UOuOtisu3kOWEJYKeSJOl1OVxejx4v19
sEti1efFq+dITmeWeYu0pJHdlrg7kUJ8hbyH8kq8jc66lZPOA2hnJw6/tz9i
pVF9hZfh8NZnGPouW9+KcbOmOjeYt0WA13NwethfiDbnxauVx4yYU+28S+qh
ORuul1kmXpc1Uu3i44J3jF58heyivB5zoty1Ycdg4eRTEEnondy2JvvJ0HfZ
+lZYAlXkM++Z8eRRGnNVwY0tTefnsBw04l391sh9oyBbzXlBPANbBzV3P9Z3
vxj6/n/GmsfeytqGHV4OKV+DKmR8fDzp5mEzLUsC7556gL38MhEFon1ErjrR
0uqOx5APUTSOlZ7YdZF7ImBF+u3yuXK7w9B3UKl+PSMsKpV3z76pl1/mN0JL
V9lbeceWET9z2Bn6DgaDwWAwGAwGg8FgMBgMBoPBYLB5Pg5EvlNY
"], "Byte", ColorSpace -> "RGB", ImageResolution -> {144, 144}, Interleaving -> True, MetaInformation -> <|"Exif" -> <|"ImageWidth" -> 160, "ImageLength" -> 160, "XResolution" -> 144, "YResolution" -> 144, "ResolutionUnit" -> "Inch", "Software" -> "Created with the Wolfram Language : www.wolfram.com", "DateTime" -> DateObject[{2023, 6, 22, 10, 24, 21.}, "Instant", "Gregorian", 2.], "TimeZoneOffset" -> 2|>, "Comments" -> <|"Software" -> "Created with the Wolfram Language : www.wolfram.com", "Creation Time" -> DateObject[{2023, 6, 22, 10, 24, 21.}, "Instant", "Gregorian", None]|>|>]}
}, Frame->All , AspectRatio->2/5, ImageSize->Large]

