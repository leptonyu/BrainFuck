BF function processor
---------------------
=       start of function
;       end of function
$       compiler directive

Compiler directives
-------------------
'comment, rest of the line ignored
$include("function.bf") 'no recursion.. use this to know what stays
                        '(current file), and what goes after use (function
                        'file)
$int(VariableName)      'set up a link beween variable name & memory loc
$map(VariableName)      'move the memory pointer to location of Variable
$array(VariableName,length) 'this would be cool
$string("abc")              '?? need to think thru this idea

remember that < and > are relative so if a function messes with them $map
will not work right.  I can't think of any way around it right now
also when a function is entered, its parameter names temporarily map
onto the original variables.  need to structure to allow recursion
Variable names can't have spaces or any of $'()=;
names not case sensitive and be multiple chars in length

Function syntax
---------------
FunctionName(param,param2,...)=commands;

A space or newline char (13/10) seperates commands
BF native commands +-<>[].,~_{} need no seperation
Immediate data (constants) should not be passed directly as a function
parameter.. instead they need to be assigned to a variable first like
everything else.

Example function file
---------------------
'===START==================================================================
$int    m   'memory location
$int    d   'destination
$int    d1  'destination 1
$int    d2  'destination 2
$int    s   'source
$int    s1  'source 1
$int    s2  'source 2
$int    t   'temporary
$int    t1  'temporary
$int    t2  'temporary
$int    a   '(address) temporary 
$int    x   'temporary
$int    n   'constant (immediate data)
0(a)=$map(a) [-];       'a=0
inc(a)=                 'a=a+1
  $map(a) +;
dec(a)=                 'a=a-1
  $map(a) -;
for(a)=                 'for a=a to 0 step -1
  $map(a) [;
next(a)=                'next a
  $map(a) -];
move(s,d)=              'move s to d
  for(s)
    $map(d) +
  next(s);
move2(s,d1,d2)=         'move s to both d1 and d2
  for(s)
    $map(d1) +
    $map(d2) +
  next(s);
copy(s,d,t)=            'copy s to d
  move2(s,d,t)
  move(t,s);
double(s,d)=            'd=s*2
  move2(s,d,d);
multiply(s1,s2,d,t)=    'd=s1*s2
  for(s1)
    copy(s2,d,t)
  next(s1)
  zero(s2);
if(a)=
  to(a) [;
endif(a)=
  zero(a) ];
ifelse(a,t)=
  inc(t)
  if(a)
  dec(t);
else(a,t)=
  endif(a)
  if(t);
endelse(t)=
  dec(t) ];
or(s1,s2,d)=
  move(s1,d)
  move(s2,d);
and(s1,s2,d)=
  if(s1)
    move(s2,d)
  endif(s1)
  zero(s2);
not(s,d)=
  inc(d)
  if(s)
    dec(d)
  endif(s)
'these #'s will test the recursion pretty well :)
'probably want to expand out when tested to increase speed & save memory 
1(a)=+;
2(a)=1(a) +;
3(a)=2(a) +;
4(a)=3(a) +;
5(a)=4(a) +;
6(a)=5(a) +;
7(a)=6(a) +;
8(a)=7(a) +;
9(a)=8(a) +;
10(a)=9(a) +;
11(a)=10(a) +;
12(a)=11(a) +;
13(a)=12(a) +;
14(a)=13(a) +;
15(a)=14(a) +;
16(a)=>[-]++++[<++++>-]<;
'experiment by replacing ++++ with 4(a). it should work
17(a)=16(a) +;
18(a)=>[-]+++[<++++++>-]<;
19(a)=18(a) +;
20(a)=>[-]++++[<+++++>-]<;
21(a)=20(a) +;
22(a)=21(a) +;
24(a)=>[-]++++[<++++++>-]<;
23(a)=24(a) -;
25(a)=>[-]+++++[<+++++>-]<;
26(a)=25(a) +;
28(a)=>[-]++++[<+++++++>-]<;
27(a)=28(a) -;
29(a)=28(a) +;
30(a)=>[-]+++++[<++++++>-]<;
31(a)=30(a) +;
32(a)=0(a) >[-]++++[<++++++++>-]<;
33(a)=32(a) +;
35(a)=>[-]+++++[<+++++++>-]<;
34(a)=35(a) -;
36(a)=>[-]++++++[<++++++>-]<;
37(a)=36(a) +;
38(a)=37(a) +;
40(a)=>[-]+++++[<++++++++>-]<;
39(a)=40(a) -;
41(a)=40(a) +;
42(a)=>[-]++++++[<+++++++>-]<;
43(a)=42(a) +;
45(a)=>[-]+++++[<+++++++++>-]<;
44(a)=45(a) -;
46(a)=45(a) +;
48(a)=>[-]++++++[<++++++++>-]<;
47(a)=48(a) -;
49(a)=>[-]+++++++[<+++++++>-]<;
50(a)=49(a) +;
51(a)=50(a) +;
54(a)=>[-]++++++[<+++++++++>-]<;
53(a)=54(a) -;
52(a)=53(a) -;
55(a)=54(a) +;
56(a)=>[-]+++++++[<++++++++>-]<;
57(a)=56(a) +;
58(a)=57(a) +;
60(a)=>[-]++++++[<++++++++++>-]<;
59(a)=60(a) -;
61(a)=60(a) +;
63(a)=>[-]+++++++[<+++++++++>-]<;
62(a)=63(a) -;
64(a)=>[-]++++++++[<++++++++>-]<;
65(a)=64(a) +;
66(a)=>[-]++++++[<+++++++++++>-]<;
67(a)=66(a) +;
68(a)=67(a) +;
70(a)=>[-]+++++++[<++++++++++>-]<;
69(a)=70(a) -;
71(a)=70(a) +;
72(a)=>[-]++++++++[<+++++++++>-]<;
73(a)=72(a) +;
74(a)=73(a) +;
77(a)=>[-]+++++++[<+++++++++++>-]<;
76(a)=77(a) -;
75(a)=76(a) -;
78(a)=77(a) +;
80(a)=>[-]++++++++[<++++++++++>-]<;
79(a)=80(a) -;
81(a)=>[-]+++++++++[<+++++++++>-]<;
82(a)=81(a) +;
84(a)=>[-]+++++++[<++++++++++++>-]<;
83(a)=84(a) -;
85(a)=84(a) +;
86(a)=85(a) +;
88(a)=>[-]++++++++[<+++++++++++>-]<;
87(a)=88(a) -;
89(a)=88(a) +;
90(a)=>[-]+++++++++[<++++++++++>-]<;
91(a)=90(a) +;
92(a)=91(a) +;
93(a)=92(a) +;
96(a)=>[-]++++++++[<++++++++++++>-]<;
95(a)=96(a) -;
94(a)=95(a) -;
97(a)=96(a) +;
99(a)=>[-]+++++++++[<+++++++++++>-]<;
98(a)=99(a) -;
100(a)=>[-]++++++++++[<++++++++++>-]<;
101(a)=100(a) +;
102(a)=101(a) +;
104(a)=>[-]++++++++[<+++++++++++++>-]<;
103(a)=104(a) -;
105(a)=104(a) +;
106(a)=105(a) +;
108(a)=>[-]+++++++++[<++++++++++++>-]<;
107(a)=108(a) -;
109(a)=108(a) +;
110(a)=>[-]++++++++++[<+++++++++++>-]<;
111(a)=110(a) +;
112(a)=>[-]++++++++[<++++++++++++++>-]<;
113(a)=112(a) +;
114(a)=113(a) +;
117(a)=>[-]+++++++++[<+++++++++++++>-]<;
116(a)=117(a) -;
115(a)=116(a) -;
118(a)=117(a) +;
119(a)=120(a) -;
120(a)=>[-]++++++++++[<++++++++++++>-]<;
121(a)=>[-]+++++++++++[<+++++++++++>-]<;
122(a)=121(a) +;
123(a)=122(a) +;
126(a)=>[-]+++++++++[<++++++++++++++>-]<;
125(a)=126(a) -;
124(a)=125(a) -;
127(a)=126(a) +;
128(a)=127(a) +;
-126(a)=>[-]+++++++++[<-------------->-]<;
-127(a)=126(a) -;
'finish these later
'===END==================================================================


*X      >[-]>[-]<<<[>[>+>+<<-]>[<+>-]<-]>>>[<<<+>>>-]<<<
*0      [-]
*1
*2      >[-][<->++][<+>-]
*3      >[-][<->+++][<+>-]
*4      >[-][<->++++][<+>-]
*5      >[-][<->+++++][<+>-]
*6      >[-][<->++++++][<+>-]
*7      >[-][<->+++++++][<+>-]
*8      >[-][<->++++++++][<+>-]
*9      >[-][<->+++++++++][<+>-]
*10     >[-][<->++++++++++][<+>-]
*11     >[-][<->+++++++++++][<+>-]
*12     >[-][<->++++++++++++][<+>-]
*13     >[-][<->+++++++++++++][<+>-]
*14     >[-][<->++++++++++++++][<+>-]
*15     >[-][<->+++++++++++++++][<+>-]
*16     >[-][<->++++++++++++++++][<+>-]
*17     >[-][<->+++++++++++++++++][<+>-]
*18     >[-][<->++++++++++++++++++][<+>-]
*19     >[-][<->+++++++++++++++++++][<+>-]

BF 8 bit math functions
?power(x,p,d,t1,t2)=
?  to(d) +
?    for(p)
?    times(x,d,t1,t2)
?    move(t1,d)
?  next(p)
?  zero(x);

compare(x1,x2,t1,t2,t3)=
  for(x1)
    copy(x1,t2)
    ifelse(t1,t2)
      dec(x1)
    else(t1,t2)
      inc(t3)
    endelse(t2)
  next(x1)
  move(t3,x1)

CUT     M(X+1)=M(X):M(X)=0              >[-]<[>+<-]
        M(X)=M(X+1):M(X+1)=0            [-]>[<+>-]
ADD     M(X)=M(X)+M(X+1):M(X+1)=0       >[<+>-]<
        M(X+1)=M(X+1)+M(X):M(X)=0       [>+<-]<
COPY    M(X+1)=M(X):M(X+2)=0            >[-]>[-]<<[>+>+<<-]>>[<<+>>-]
SWAP    M(X)@M(X+1):M(X+2)=0            >>[-]<[>+<-]<[>+<-]>>[<<+>>-]<<
MUL     ?                               >[-]>[-]<< <[>[>+>+<<-] >[<+>-] <-] >>>[<<<+>>>-]<<<
                                        >[-]>[-]<<<[>[>+>+<<-]>[<+>-]<-]>>>[<<<+>>>-]<<<
IF      ?                               >[-]<[>[-]+<-]> (your program here) <

+128    >[-]++++[>[-]++++[<<++++++++>>-]<-]<
+128    [-]++++[>++++<-]>[<++++++++>-]<

 1 2 3 4 5 6 7 8 91011121314  
1- - - - - - - - - - - - - -  
2\ - - - - - - - - - - - - -  
3  \ - - - - - - - - - - - -  
4    \ -19202122 - - - - - -  
5      \2021222324 - - - - -  
6        \222324252627 - - -  
7          \242526272829 - -  
8            \26272829303132  
9              \282930313233  
10               \303132 > 
11                 \32 >
12                   \
13                     \
14                       \
