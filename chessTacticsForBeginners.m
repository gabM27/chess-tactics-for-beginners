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

checkCorrectMove[] :=Module[{},
	If[StringMatchQ[PGNfile[lastgame]["Result"], "1-0"],
    correctMove = StringDrop[correctMove, 2],
    correctMove=correctMove];  
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
  Delete[Movelist,-1];
  Chess[ShowBoard -> board, Interact -> False];
]

While[True,
  nomeUtente = InputString["Inserisci il tuo nome:"];
  If[! StringMatchQ[StringTrim[nomeUtente]][""], Break[]];
]
(*output soppresso perch\[EGrave] \[EGrave] nella nuova interfaccia*)
StringReplace[nomeUtente, " " -> ""] <> " sta giocando!";

board = Startposition;
Chess[ShowBoard -> Interactive]
newBoardBtn = Button["Nuova scacchiera", board = generateNewChessBoard[]; gameResult = 0;];
repeatBtn = Button["Rigioca Partita", board = repeatChessBoard[]];
backBtn = Button["Back", Move[MoveFromPGN[filepgn[[Length[Movelist] - 1]]][[1]]]];
restartBtn = Button["Restart", whoIsPlaying = ""; endgame = ""; correctMovetoShow =""; correctMove=""; Chess[ShowBoard -> Startposition, Interact -> True]];
checkBtn = Button["Verifica mossa", checkMove[] ];
showSolutionBtn = Button["Mostra soluzione", checkCorrectMove[]; correctMovetoShow = "La mossa corretta \[EGrave] " <> correctMove ];
(*
versione di interfaccia precedente:
Column[{newBoardBtn, restartBtn, checkBtn, showSolutionBtn, backBtn,repeatBtn}]
*)
GraphicsGrid[
{
{Graphics[Inset[Graphics[Raster[CompressedData["
1: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
hz3rxoLXOJG+GLKro8jt6iKr8NuCTnjG5kop4C7SaIGBSU24YtGVD8/3F6n3
Z4eQ7+PeS/dcRKw5+Tn3P1UKlxWpHx6b66g1Z4ljvzDqAL1HEH13AXrchvCT
9HE91dFTtlyQKDYxXuUOJf7Pxdb2ZtSHt+CCn5MIWxVQzQAv6RKXu27Rz6dv
Ss5ytdHPmVwkZ00MoA7+QD2889ecH0y9uLRTpBQ5NXfifz31SDmSSaRrI5G6
S0ROnBb5jtrSDGcbQB1IndXV1v1zpHNLke43RNagM1q62pp/noTSQ01Vm05q
72pr/nkSAmahatOQRPwSLNXBbDTYjUBLJ+L3P2UA/VOxPCJv5RVZnFPkcrDI
XLBbiO7rTy70gbvQq3Shn7mbztXWPl9yi554+VRqb7jI77+L+KENLtMTgt1N
g7tco8eDGwbA/Q6hxz6nL3uCd3r+rXIvg8jgsMd/5jMPfAfmc7XVz4/Mhg83
oP9YOlNkxSyRCzNEftsq8ova1PhubJvNvu3of9qJhD8Hv117nsS4DdIFDbX/
HWapv2H2+jFJbPdxEtPf/5a+YDZJbdonsf4mWKaC2TS11eDdifglWIz/Z3Lk
FZFvqb01n+IZ9v9XWZ9cpJO/SFR1kUJP+Qw7URIlURIlURIlUf5N8n/j6vs2

"], {{0, 0}, {80, 80}}, {0, 65535}, ColorFunction -> GrayLevel], ImageSize -> 24, PlotRange -> {{0, 80}, {0, 80}}], {4.5, 6.5}]], " sta giocando"Dynamic@nomeUtente, Graphics[Inset[Graphics[Raster[CompressedData["
1:eJztXAl8jFf3fggRhGjs+1IVW4O01FpbSylqqX0XIdYial8/glprKRFrq9RS
RZWvC2qtnapSe2jsxBKCJHi+583MJBNLyDbD/5/z+x2Zmfe995773HPPec59
Z+Tv8FnDTskB+Djpn4bt+1ft06f9wEZv6E3jnj5dOvf06lirZ1+vzl59SpXx
dtCn9c1qtEmSJEmSJEmSJEmS/+tyxPjHESiaHqiRFShWANhfHLheGgiuBLSs
CQTVljYCUjYDDjY1abB0eWNgy8fA1o+AgZUBt7LAAnegeUHgeHagQQbgUipg
fTJ7zzL+MkRzuOYMPBI+Ae8D89vo9VBg02zg7BogbAcQ+DfQ6hzgchVYe1Of
3QGO3QcqhQLnwoEsjwS1WTNIOz4EBulalwfC8i5w6xZQ85reBwLVjgnvPUC3
/2rshcAGX6BMZ+CQ1qN2UWCdK+DnYG9UYpYqaYF3SwL9OwKF/eQfW4FswueY
cPlL8ydj0gzSwdJ90pnJSUpDzergQNbQ30W6VvsF/URpZuk0rUffS8J6H9B+
sbQPMFp+3k14brE3YGYJzyae7g1M/kX4yZd2R8PKVXogBTkpAzkmn96/Q1au
QbZrTp7uQn41gJw8hvzjS7KiP7lzLllkCdlgKVnSrJ7S0d+SLrpWbhbpOImc
NoLc2pvs3J78/BMyfUX1UYS8l5Wsk4ZcnexZmDaTOgYD6XbLz4cJVzcgux33
+4fvAXd2Ar0eWmzMI7snCqsqpTSnluQyYXPoe/nQHmESQF4KImveJ90fkWsZ
PwmVbnosvw0jjwSTvpfINkfIQhu1JvPICv3JhvU0jpt8Ny3ZPhqWtaT3TgLb
a9kPv0yK8TvCDXtcHLXlPiDfnUq23k0GXyPXPIwnQPEUmUAfgRxw3gBa9o0l
b1cmTzhaMByndfdtaT/8/AoDnc4btsxJRe5oojX+kZwtP5jwyL7YWYu2M6fe
IC//rv3gJQzTWvBbdgG4XcR++OVOA1T/xTrGrBSOJwsLx9ZkpxlksW3C9V+y
awg563HiY5VF6qb44HSRPL9TuUgxc3UnxVDFk37OZL5oe7jjBu3jtPbDz5CJ
I5+XA+spFk5NR+4tSPpXU+701ORGksMUmzL9RJb+Q3nkMPngNOmh+XbVhusr
P/nPbV2/Sz6+Y1JXvV6k+LZd15pfV/yUf28IIMcp1h3eRf68npy+UO8Va6t3
VrxVflqjNXzoQr6RPKYcXc7XvtgZEiw+u/aeYc+iN8is+cnsToqHMXCLKsJ1
lWJQ9fRkTjnM+jzKr4rxR92195Wfq5UTbpX03qzh0qYV5Mvvak1KKlcIm97K
5YuVa5e7mGLHmmfmW5Omk76ve05pnFEuls8LiUfeqGNv9IB+uYHNxw2b/qu5
jz0gDOUPIaPJm+Iovu8pjstuV9ntlpIc9NL8LfbaSrpaXKmO1iWjxmxUhiwv
G5rLlpNK9t+IA4wsYbm/9Gnlvnz2Rk9cKqXqr2URNmUkR+yNikWLlX+z3yKH
a3/2UCwK/IGcN5NcKu5WqgfppRg5V9wNyttpxN+myb++8hCnK0ZuEZc7Yda9
0q1FybuKYU10j598sWl18m9xk2GtyFzdycbDFQO+UoxYKQ64Q3v5lHjhTcUE
Kw5QU/jlcbXg9/cq1Y6O9kbPJA69DJvGS8NnvFycPy7Nrnzyrbib7z3hpzgX
pvi2X/zw7Stk7sv6a9Y3pVulXrp2wrhHsbCq2nir7Q3l+b9fMreM0NotifTX
Df3sjVqUHCkHfHnTsGtwM8W+8JeckA1lsPywSWsLdl+rBulU2d6oRcmyPOKh
pw3bhmsP/+NN5piteK86oLIcbZxyZs4HqgNswF/WaIwKGmuTxjyqsdOJN38j
W5J1JS9mtuD3lnjf1kL2Ri1KsnkAa65bx3In5cP6Iq4Nc6p2U9y6+pH2YgfV
Aqp3T08mV4hvbFml68K4i2JjXuWdEuIjOf5RHXxG9wWonjWr8dr4zLhm3GPc
a7RporYDV4tfqq9LRp8DyUoao2AtspfGXKWxD8mGik/lZl/535cV7Y1alAxs
BOQJzyjb1hdQnnhTOUJ2b4ohV1bWnJIrH19OrdimfLlAcd1dfGRRNvKccucn
4iflzGq8Nj4zrhn3LNS9O9TmhtpWVR8tYuAuxrnOd7pnbnZySC7lk4jPcz8G
xra1N2pREjLAsLW9uGqYP3khUOsv39ikfPjXZ+ITyrEnSgsjYbslE7lZ9VNB
zSl3DPN+WR0h7e0gHxeWOcQ/C+VWDSQeWU0cekZH2TJOe1p+PvWYePYC1W4p
LG0rjbE3aib5KRmwdIFhUxHVGvm2RY9H86VlwkxcYsA58sNDer+FdNa+a6Z9
t3C69pzq+iFDxYv7KCf3Iiep3mqhuQ826+fSQ9LF4jz+vcUlB2lvjtIaTSH3
zCELL9c6/Up2FHcKOqm+Vce4qIbb+ES8rbBPnPQNC37bVgA/pLA3eop96YDD
Ww2bNuTVPE7HL/4bqVtuS1E69jSrYGM/aUD8uuaK8/LHQpFnL/uAiq72Rg9Y
kg/YeMawKai87Lsdz0kmorQKkd9Ws+CX8iKwoLC90QNSqYb8965h03Lxq6BX
6NzqSRF95uBOkRxQ9W/OpvbDLYd8/6+uQNg/llieuTbZQPEv20VyVqhyq70B
M4uTgkKNq+QuxcfbTazzz3Rx1uN9gNRZbItdlmKKHz8C/cOtc+ED6VblkOKq
V5eqNl2qoOU8V1ztN3LiYbLdBTKL9ndmYftIfnoyAbA5Ky2lPDFQfe5R33kv
aYyj4s7izdOUn94fonzcSLlHOXmNckeXp86zLj4E6m5UHC8rn0hk3Hyk06oA
Fw5Yxv+P+MAD1fxeb4kfpHyaX9QVRxkvbjE+i/zATbVAWdWzH4tvaK8vVJJw
GyxOJ47RearyrrhPoXniakvI2lbPj4zXxmfGNeMe416jjdG2uZKLSxtxpDpk
SDnVPFo7b/HE2+KfR5/Bj+bLxlP5lcffFld1sL7WQ7446lP5YyJ9Le2O+j2t
eDHlnGXMY+JxJT4n39FG/VCOUOl78TEfxcHqZBpxvevyxSMxnl8+42xQekXz
nqd24ebnlz9Ly0q76ZrzS/aT0zjL0Nrekc95FNb+VWxpptrnc9m4ThyhvHKx
n7B3S23dbrBqqIs95IepEhY7HwfglBdwLbJGq5tDay/u9V5o9P0UwTWU54oL
z1PbZe9izUX8zreb6oVPyVpV5TvviD9rTtXzKp7LV/xUM/tonktVU+zWmgRJ
T5h9Obk0o3A45Eiulo5yUr+6XslF/qZ2D2SHu3z/I+0BaN16NRMHFY88Km7Y
4gfZIc73s+JGKtXEm5/Y+yfukZOnqa7MYo1h5RCg8UigSJqEwW69NKCdcsUN
yxjOxbWnxFe3xeI8QFAzzUPyY2Gb7YYwU46ZqZr2imLVqoOKS5pnXdW0LlsV
C4R70famsYxnjsNEAkf8THquU8z8hXTUPR/sUo3xJ7nvOJlJvuQbRN5X36c0
xplYxM8/FYffWivc3awxPKbcPGWIOE7K+OPnXUn54ryl71JlZP/+WBgYR9k+
wjSev/TWnMQfb5DW7rC7NYZDbgNvNYsfdpddgBnrLX3+Vkz+YAPsDMltxm+C
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
"], {{0, 0}, {80, 80}}, {0, 65535}, ColorFunction -> GrayLevel], ImageSize -> 24, PlotRange -> {{0, 80}, {0, 80}}], {3.5, 2.5}]]},
{newBoardBtn, restartBtn, checkBtn },
{showSolutionBtn, backBtn,repeatBtn},
{Dynamic@whoIsPlaying,Dynamic@endgame,Dynamic@correctMovetoShow}
}, Frame->All , AspectRatio->1/3]

Dynamic@whoIsPlaying
Dynamic@endgame
Dynamic@correctMovetoShow


(*
PROVE per delimitatori
For[i=1, i<Dimensions[StringSplit[pgntosplit[[1]],", "]],i++, AppendTo[delList,ToString[i<>"."]]]
For[i=1, i<Dimensions[delList],i++,Print[delList[[i]]]]
delList*)

