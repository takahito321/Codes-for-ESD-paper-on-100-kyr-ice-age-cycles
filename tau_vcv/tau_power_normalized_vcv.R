%!PS-Adobe-3.0 EPSF-3.0
%%DocumentNeededResources: font Helvetica
%%+ font Helvetica-Bold
%%+ font Helvetica-Oblique
%%+ font Helvetica-BoldOblique
%%+ font Symbol
%%Title: R Graphics Output
%%Creator: R Software
%%Pages: (atend)
%%BoundingBox: 18 169 577 673
%%EndComments
%%BeginProlog
/bp  { gs sRGB gs } def
% begin .ps.prolog
/gs  { gsave } bind def
/gr  { grestore } bind def
/ep  { showpage gr gr } bind def
/m   { moveto } bind def
/l  { rlineto } bind def
/np  { newpath } bind def
/cp  { closepath } bind def
/f   { fill } bind def
/o   { stroke } bind def
/c   { newpath 0 360 arc } bind def
/r   { 4 2 roll moveto 1 copy 3 -1 roll exch 0 exch rlineto 0 rlineto -1 mul 0 exch rlineto closepath } bind def
/p1  { stroke } bind def
/p2  { gsave bg fill grestore newpath } bind def
/p3  { gsave bg fill grestore stroke } bind def
/p6  { gsave bg eofill grestore newpath } bind def
/p7  { gsave bg eofill grestore stroke } bind def
/t   { 5 -2 roll moveto gsave rotate
       1 index stringwidth pop
       mul neg 0 rmoveto show grestore } bind def
/ta  { 4 -2 roll moveto gsave rotate show } bind def
/tb  { 2 -1 roll 0 rmoveto show } bind def
/cl  { grestore gsave newpath 3 index 3 index moveto 1 index
       4 -1 roll lineto  exch 1 index lineto lineto
       closepath clip newpath } bind def
/rgb { setrgbcolor } bind def
/s   { scalefont setfont } bind def
% end   .ps.prolog
/sRGB { [ /CIEBasedABC
          << /DecodeLMN
               [ { dup 0.03928 le
                        {12.92321 div}
                        {0.055 add 1.055 div 2.4 exp }
                     ifelse
                 } bind dup dup
               ]
             /MatrixLMN [0.412457 0.212673 0.019334
                         0.357576 0.715152 0.119192
                         0.180437 0.072175 0.950301]
             /WhitePoint [0.9505 1.0 1.0890]
           >>
         ] setcolorspace } bind def
/srgb { setcolor } bind def
% begin encoding
/WinAnsiEncoding [
 /.notdef /.notdef /.notdef /.notdef /.notdef /.notdef /.notdef /.notdef
 /.notdef /.notdef /.notdef /.notdef /.notdef /.notdef /.notdef /.notdef
 /.notdef /.notdef /.notdef /.notdef /.notdef /.notdef /.notdef /.notdef
 /.notdef /.notdef /.notdef /.notdef /.notdef /.notdef /.notdef /.notdef
 /space /exclam /quotedbl /numbersign /dollar /percent /ampersand /quoteright
 /parenleft /parenright /asterisk /plus /comma /minus /period /slash
 /zero /one /two /three /four /five /six /seven
 /eight /nine /colon /semicolon /less /equal /greater /question
 /at /A /B /C /D /E /F /G
 /H /I /J /K /L /M /N /O
 /P /Q /R /S /T /U /V /W
 /X /Y /Z /bracketleft /backslash /bracketright /asciicircum /underscore
 /grave /a /b /c /d /e /f /g
 /h /i /j /k /l /m /n /o
 /p /q /r /s /t /u /v /w
 /x /y /z /braceleft /bar /braceright /asciitilde /.notdef
 /Euro /.notdef /quotesinglbase /florin /quotedblbase /ellipsis /dagger /daggerdbl
 /circumflex /perthousand /Scaron /guilsinglleft /OE /.notdef /Zcaron /.notdef
 /.notdef /quoteleft /quoteright /quotedblleft /quotedblright /bullet /endash /emdash
 /tilde /trademark /scaron /guilsinglright /oe /.notdef /zcaron /Ydieresis
 /space /exclamdown /cent /sterling /currency /yen /brokenbar /section
 /dieresis /copyright /ordfeminine /guillemotleft /logicalnot /hyphen /registered /macron
 /degree /plusminus /twosuperior /threesuperior /acute /mu /paragraph /periodcentered
 /cedilla /onesuperior /ordmasculine /guillemotright /onequarter /onehalf /threequarters /questiondown
 /Agrave /Aacute /Acircumflex /Atilde /Adieresis /Aring /AE /Ccedilla
 /Egrave /Eacute /Ecircumflex /Edieresis /Igrave /Iacute /Icircumflex /Idieresis
 /Eth /Ntilde /Ograve /Oacute /Ocircumflex /Otilde /Odieresis /multiply
 /Oslash /Ugrave /Uacute /Ucircumflex /Udieresis /Yacute /Thorn /germandbls
 /agrave /aacute /acircumflex /atilde /adieresis /aring /ae /ccedilla
 /egrave /eacute /ecircumflex /edieresis /igrave /iacute /icircumflex /idieresis
 /eth /ntilde /ograve /oacute /ocircumflex /otilde /odieresis /divide
 /oslash /ugrave /uacute /ucircumflex /udieresis /yacute /thorn /ydieresis
]
 def
% end encoding
%%IncludeResource: font Helvetica
/Helvetica findfont
dup length dict begin
  {1 index /FID ne {def} {pop pop} ifelse} forall
  /Encoding WinAnsiEncoding def
  currentdict
  end
/Font1 exch definefont pop
%%IncludeResource: font Helvetica-Bold
/Helvetica-Bold findfont
dup length dict begin
  {1 index /FID ne {def} {pop pop} ifelse} forall
  /Encoding WinAnsiEncoding def
  currentdict
  end
/Font2 exch definefont pop
%%IncludeResource: font Helvetica-Oblique
/Helvetica-Oblique findfont
dup length dict begin
  {1 index /FID ne {def} {pop pop} ifelse} forall
  /Encoding WinAnsiEncoding def
  currentdict
  end
/Font3 exch definefont pop
%%IncludeResource: font Helvetica-BoldOblique
/Helvetica-BoldOblique findfont
dup length dict begin
  {1 index /FID ne {def} {pop pop} ifelse} forall
  /Encoding WinAnsiEncoding def
  currentdict
  end
/Font4 exch definefont pop
%%IncludeResource: font Symbol
/Symbol findfont
dup length dict begin
  {1 index /FID ne {def} {pop pop} ifelse} forall
  currentdict
  end
/Font5 exch definefont pop
%%EndProlog
%%Page: 1 1
bp
90.00 291.34 534.08 636.94 cl
0.1176 0.5647 1 srgb
0.75 setlinewidth
[] 0 setdash
1 setlinecap
1 setlinejoin
10.00 setmiterlimit
np
108.34 457.66 m
5.35 -19.67 l
o
np
119.61 425.08 m
1.08 -1.60 l
o
np
134.81 414.08 m
7.23 54.17 l
o
np
155.56 470.59 m
2.29 -4.23 l
o
np
171.64 467.51 m
6.67 38.39 l
o
np
181.29 506.01 m
5.65 -22.55 l
o
np
202.28 478.60 m
0.22 -0.28 l
o
np
210.12 466.20 m
2.82 -5.77 l
o
np
228.65 448.54 m
2.30 -4.27 l
o
np
235.74 445.00 m
6.40 33.06 l
o
np
253.79 471.70 m
6.85 -42.54 l
o
np
263.40 415.04 m
5.90 -25.66 l
o
np
274.98 376.42 m
1.02 -1.48 l
o
np
284.42 363.26 m
0.42 -0.55 l
o
np
330.09 398.98 m
0.44 0.58 l
o
np
339.35 410.94 m
0.20 0.26 l
o
np
 101.10 464.61 m
5.35 5.34 l
5.34 -5.34 l
-5.34 -5.35 l
cp p1
np
 110.24 431.04 m
5.34 5.35 l
5.35 -5.35 l
-5.35 -5.34 l
cp p1
np
 119.38 417.52 m
5.34 5.34 l
5.35 -5.34 l
-5.35 -5.35 l
cp p1
np
 128.51 406.94 m
5.35 5.35 l
5.35 -5.35 l
-5.35 -5.35 l
cp p1
np
 137.65 475.39 m
5.35 5.34 l
5.34 -5.34 l
-5.34 -5.35 l
cp p1
np
 146.79 476.93 m
5.34 5.34 l
5.35 -5.34 l
-5.35 -5.35 l
cp p1
np
 155.93 460.03 m
5.34 5.34 l
5.35 -5.34 l
-5.35 -5.35 l
cp p1
np
 165.06 460.42 m
5.35 5.34 l
5.34 -5.34 l
-5.34 -5.35 l
cp p1
np
 174.20 512.99 m
5.35 5.35 l
5.34 -5.35 l
-5.34 -5.34 l
cp p1
np
 183.34 476.48 m
5.34 5.34 l
5.35 -5.34 l
-5.35 -5.35 l
cp p1
np
 192.48 484.25 m
5.34 5.35 l
5.35 -5.35 l
-5.35 -5.34 l
cp p1
np
 201.61 472.67 m
5.35 5.34 l
5.34 -5.34 l
-5.34 -5.35 l
cp p1
np
 210.75 453.96 m
5.35 5.34 l
5.34 -5.34 l
-5.34 -5.35 l
cp p1
np
 219.89 454.87 m
5.34 5.35 l
5.35 -5.35 l
-5.35 -5.34 l
cp p1
np
 229.02 437.93 m
5.35 5.35 l
5.35 -5.35 l
-5.35 -5.34 l
cp p1
np
 238.16 485.13 m
5.35 5.35 l
5.34 -5.35 l
-5.34 -5.35 l
cp p1
np
 247.30 478.80 m
5.34 5.35 l
5.35 -5.35 l
-5.35 -5.34 l
cp p1
np
 256.44 422.05 m
5.34 5.35 l
5.35 -5.35 l
-5.35 -5.34 l
cp p1
np
 265.57 382.37 m
5.35 5.34 l
5.35 -5.34 l
-5.35 -5.35 l
cp p1
np
 274.71 368.99 m
5.35 5.35 l
5.34 -5.35 l
-5.34 -5.34 l
cp p1
np
 283.85 356.98 m
5.34 5.34 l
5.35 -5.34 l
-5.35 -5.35 l
cp p1
np
 292.99 366.73 m
5.34 5.34 l
5.35 -5.34 l
-5.35 -5.35 l
cp p1
np
 302.12 375.27 m
5.35 5.34 l
5.34 -5.34 l
-5.34 -5.35 l
cp p1
np
 311.26 382.98 m
5.35 5.34 l
5.34 -5.34 l
-5.34 -5.35 l
cp p1
np
 320.40 393.24 m
5.34 5.35 l
5.35 -5.35 l
-5.35 -5.34 l
cp p1
np
 329.54 405.29 m
5.34 5.35 l
5.35 -5.35 l
-5.35 -5.34 l
cp p1
np
 338.67 416.85 m
5.35 5.35 l
5.34 -5.35 l
-5.34 -5.34 l
cp p1
np
 347.81 425.30 m
5.35 5.35 l
5.34 -5.35 l
-5.34 -5.34 l
cp p1
np
 356.95 429.73 m
5.34 5.34 l
5.35 -5.34 l
-5.35 -5.35 l
cp p1
np
 366.08 431.14 m
5.35 5.34 l
5.35 -5.34 l
-5.35 -5.35 l
cp p1
np
 375.22 431.33 m
5.35 5.34 l
5.34 -5.34 l
-5.34 -5.35 l
cp p1
np
 384.36 431.50 m
5.35 5.34 l
5.34 -5.34 l
-5.34 -5.35 l
cp p1
np
 393.50 432.15 m
5.34 5.34 l
5.35 -5.34 l
-5.35 -5.35 l
cp p1
np
 402.63 433.37 m
5.35 5.34 l
5.35 -5.34 l
-5.35 -5.35 l
cp p1
np
 411.77 435.11 m
5.35 5.34 l
5.34 -5.34 l
-5.34 -5.35 l
cp p1
np
 420.91 437.27 m
5.34 5.35 l
5.35 -5.35 l
-5.35 -5.34 l
cp p1
np
 430.05 439.77 m
5.34 5.35 l
5.35 -5.35 l
-5.35 -5.35 l
cp p1
np
 439.18 442.51 m
5.35 5.34 l
5.35 -5.34 l
-5.35 -5.35 l
cp p1
np
 448.32 445.41 m
5.35 5.34 l
5.34 -5.34 l
-5.34 -5.35 l
cp p1
np
 457.46 448.40 m
5.34 5.35 l
5.35 -5.35 l
-5.35 -5.34 l
cp p1
np
 466.60 451.44 m
5.34 5.34 l
5.35 -5.34 l
-5.35 -5.35 l
cp p1
np
 475.73 454.46 m
5.35 5.34 l
5.34 -5.34 l
-5.34 -5.35 l
cp p1
np
 484.87 457.42 m
5.35 5.34 l
5.34 -5.34 l
-5.34 -5.35 l
cp p1
np
 494.01 460.28 m
5.34 5.35 l
5.35 -5.35 l
-5.35 -5.34 l
cp p1
np
 503.15 463.02 m
5.34 5.35 l
5.35 -5.35 l
-5.35 -5.34 l
cp p1
np
 512.28 465.61 m
5.35 5.34 l
5.34 -5.34 l
-5.34 -5.35 l
cp p1
18.00 168.94 577.28 672.94 cl
0 0 0 srgb
0.75 setlinewidth
[] 0 setdash
1 setlinecap
1 setlinejoin
10.00 setmiterlimit
np
174.98 291.34 m
342.65 0 l
o
np
174.98 291.34 m
0 -7.20 l
o
np
289.19 291.34 m
0 -7.20 l
o
np
403.41 291.34 m
0 -7.20 l
o
np
517.63 291.34 m
0 -7.20 l
o
/Font1 findfont 17 s
174.98 265.42 (0.5) .5 0 t
289.19 265.42 (1.0) .5 0 t
403.41 265.42 (1.5) .5 0 t
517.63 265.42 (2.0) .5 0 t
np
90.00 304.14 m
0 320.00 l
o
np
90.00 304.14 m
-7.20 0 l
o
np
90.00 357.48 m
-7.20 0 l
o
np
90.00 410.81 m
-7.20 0 l
o
np
90.00 464.14 m
-7.20 0 l
o
np
90.00 517.48 m
-7.20 0 l
o
np
90.00 570.81 m
-7.20 0 l
o
np
90.00 624.14 m
-7.20 0 l
o
72.72 304.14 (0.00) .5 90 t
72.72 357.48 (0.05) .5 90 t
72.72 410.81 (0.10) .5 90 t
72.72 464.14 (0.15) .5 90 t
72.72 517.48 (0.20) .5 90 t
72.72 570.81 (0.25) .5 90 t
72.72 624.14 (0.30) .5 90 t
np
 90.00 291.34 m
444.08 0 l
0 345.60 l
-444.08 0 l
cp p1
18.00 183.34 577.28 672.94 cl
/Font1 findfont 17 s
0 0 0 srgb
43.92 353.59 (Relativ) 90 ta
-0.425 (e band po) tb
-0.255 (w) tb
-0.170 (er strength) tb gr
90.00 291.34 534.08 636.94 cl
1 0 0 srgb
0.75 setlinewidth
[] 0 setdash
1 setlinecap
1 setlinejoin
10.00 setmiterlimit
np
137.96 325.69 m
0.94 -1.35 l
o
np
147.22 324.25 m
0.69 0.96 l
o
np
173.79 326.20 m
2.38 -4.47 l
o
np
247.79 350.81 m
0.57 0.76 l
o
np
253.07 364.55 m
8.29 141.85 l
o
np
263.55 520.57 m
5.60 22.14 l
o
np
273.29 556.49 m
4.40 12.63 l
o
np
282.99 582.49 m
3.27 7.30 l
o
np
301.55 581.41 m
2.70 -5.39 l
o
np
311.41 563.55 m
1.26 -1.93 l
o
np
329.86 539.44 m
0.91 -1.30 l
o
np
337.89 525.69 m
3.12 -6.76 l
o
np
346.59 505.66 m
4.00 -10.46 l
o
np
355.73 481.75 m
3.99 -10.41 l
o
np
365.14 458.00 m
3.44 -7.98 l
o
np
374.71 437.00 m
2.58 -5.03 l
o
np
384.35 419.44 m
1.58 -2.57 l
o
np
393.98 404.95 m
0.58 -0.78 l
o
np
101.10 309.93 m
10.69 0 l
o
np
106.45 304.59 m
0 10.69 l
o
np
110.24 316.94 m
10.69 0 l
o
np
115.58 311.60 m
0 10.69 l
o
np
119.38 327.05 m
10.69 0 l
o
np
124.72 321.71 m
0 10.69 l
o
np
128.51 331.61 m
10.70 0 l
o
np
133.86 326.27 m
0 10.69 l
o
np
137.65 318.42 m
10.69 0 l
o
np
143.00 313.07 m
0 10.69 l
o
np
146.79 331.05 m
10.69 0 l
o
np
152.13 325.70 m
0 10.69 l
o
np
155.93 335.15 m
10.69 0 l
o
np
161.27 329.80 m
0 10.69 l
o
np
165.06 332.55 m
10.69 0 l
o
np
170.41 327.21 m
0 10.69 l
o
np
174.20 315.37 m
10.69 0 l
o
np
179.55 310.02 m
0 10.70 l
o
np
183.34 325.37 m
10.69 0 l
o
np
188.68 320.02 m
0 10.69 l
o
np
192.48 330.06 m
10.69 0 l
o
np
197.82 324.72 m
0 10.69 l
o
np
201.61 336.58 m
10.69 0 l
o
np
206.96 331.23 m
0 10.70 l
o
np
210.75 332.26 m
10.69 0 l
o
np
216.10 326.91 m
0 10.69 l
o
np
219.89 325.71 m
10.69 0 l
o
np
225.23 320.36 m
0 10.69 l
o
np
229.02 336.74 m
10.70 0 l
o
np
234.37 331.39 m
0 10.70 l
o
np
238.16 345.03 m
10.69 0 l
o
np
243.51 339.68 m
0 10.69 l
o
np
247.30 357.36 m
10.69 0 l
o
np
252.64 352.01 m
0 10.69 l
o
np
256.44 513.59 m
10.69 0 l
o
np
261.78 508.24 m
0 10.70 l
o
np
265.57 549.69 m
10.70 0 l
o
np
270.92 544.35 m
0 10.69 l
o
np
274.71 575.92 m
10.69 0 l
o
np
280.06 570.57 m
0 10.69 l
o
np
283.85 596.37 m
10.69 0 l
o
np
289.19 591.02 m
0 10.69 l
o
np
292.99 587.85 m
10.69 0 l
o
np
298.33 582.50 m
0 10.69 l
o
np
302.12 569.58 m
10.69 0 l
o
np
307.47 564.24 m
0 10.69 l
o
np
311.26 555.59 m
10.69 0 l
o
np
316.61 550.24 m
0 10.70 l
o
np
320.40 545.35 m
10.69 0 l
o
np
325.74 540.00 m
0 10.69 l
o
np
329.54 532.23 m
10.69 0 l
o
np
334.88 526.89 m
0 10.69 l
o
np
338.67 512.39 m
10.69 0 l
o
np
344.02 507.04 m
0 10.69 l
o
np
347.81 488.48 m
10.69 0 l
o
np
353.16 483.13 m
0 10.69 l
o
np
356.95 464.61 m
10.69 0 l
o
np
362.29 459.27 m
0 10.69 l
o
np
366.08 443.41 m
10.70 0 l
o
np
371.43 438.07 m
0 10.69 l
o
np
375.22 425.57 m
10.69 0 l
o
np
380.57 420.22 m
0 10.69 l
o
np
384.36 410.74 m
10.69 0 l
o
np
389.71 405.39 m
0 10.70 l
o
np
393.50 398.38 m
10.69 0 l
o
np
398.84 393.03 m
0 10.69 l
o
np
402.63 387.99 m
10.70 0 l
o
np
407.98 382.64 m
0 10.69 l
o
np
411.77 379.20 m
10.69 0 l
o
np
417.12 373.85 m
0 10.69 l
o
np
420.91 371.72 m
10.69 0 l
o
np
426.25 366.37 m
0 10.69 l
o
np
430.05 365.32 m
10.69 0 l
o
np
435.39 359.97 m
0 10.69 l
o
np
439.18 359.81 m
10.70 0 l
o
np
444.53 354.47 m
0 10.69 l
o
np
448.32 355.05 m
10.69 0 l
o
np
453.67 349.71 m
0 10.69 l
o
np
457.46 350.91 m
10.69 0 l
o
np
462.80 345.57 m
0 10.69 l
o
np
466.60 347.29 m
10.69 0 l
o
np
471.94 341.95 m
0 10.69 l
o
np
475.73 344.11 m
10.69 0 l
o
np
481.08 338.76 m
0 10.69 l
o
np
484.87 341.29 m
10.69 0 l
o
np
490.22 335.94 m
0 10.69 l
o
np
494.01 338.77 m
10.69 0 l
o
np
499.35 333.43 m
0 10.69 l
o
np
503.15 336.53 m
10.69 0 l
o
np
508.49 331.18 m
0 10.69 l
o
np
512.28 334.51 m
10.69 0 l
o
np
517.63 329.16 m
0 10.70 l
o
0.2667 0.0039 0.3294 srgb
np
254.56 333.79 m
5.30 19.17 l
o
np
264.19 366.68 m
4.32 12.20 l
o
np
274.76 391.75 m
1.45 2.30 l
o
np
319.16 410.39 m
4.03 10.59 l
o
np
327.92 434.57 m
4.79 15.13 l
o
np
337.38 463.32 m
4.14 11.19 l
o
np
347.12 487.76 m
2.94 6.17 l
o
np
357.14 506.42 m
1.17 1.76 l
o
np
 106.45 326.96 m
5.09 -8.82 l
-10.18 0 l
cp p1
np
 115.58 320.63 m
5.10 -8.82 l
-10.19 0 l
cp p1
np
 124.72 318.89 m
5.09 -8.81 l
-10.18 0 l
cp p1
np
 133.86 317.83 m
5.09 -8.82 l
-10.18 0 l
cp p1
np
 143.00 323.62 m
5.09 -8.82 l
-10.18 0 l
cp p1
np
 152.13 330.28 m
5.09 -8.82 l
-10.18 0 l
cp p1
np
 161.27 321.66 m
5.09 -8.82 l
-10.18 0 l
cp p1
np
 170.41 323.10 m
5.09 -8.82 l
-10.18 0 l
cp p1
np
 179.55 318.66 m
5.09 -8.82 l
-10.18 0 l
cp p1
np
 188.68 316.84 m
5.09 -8.82 l
-10.18 0 l
cp p1
np
 197.82 319.98 m
5.09 -8.82 l
-10.18 0 l
cp p1
np
 206.96 324.14 m
5.09 -8.82 l
-10.18 0 l
cp p1
np
 216.10 322.57 m
5.09 -8.81 l
-10.19 0 l
cp p1
np
 225.23 323.71 m
5.09 -8.82 l
-10.18 0 l
cp p1
np
 234.37 330.20 m
5.09 -8.82 l
-10.18 0 l
cp p1
np
 243.51 332.54 m
5.09 -8.81 l
-10.18 0 l
cp p1
np
 252.64 332.73 m
5.10 -8.82 l
-10.19 0 l
cp p1
np
 261.78 365.77 m
5.09 -8.81 l
-10.18 0 l
cp p1
np
 270.92 391.54 m
5.09 -8.82 l
-10.18 0 l
cp p1
np
 280.06 406.01 m
5.09 -8.81 l
-10.18 0 l
cp p1
np
 289.19 410.90 m
5.10 -8.81 l
-10.19 0 l
cp p1
np
 298.33 404.06 m
5.09 -8.82 l
-10.18 0 l
cp p1
np
 307.47 399.39 m
5.09 -8.82 l
-10.18 0 l
cp p1
np
 316.61 409.54 m
5.09 -8.82 l
-10.18 0 l
cp p1
np
 325.74 433.59 m
5.09 -8.82 l
-10.18 0 l
cp p1
np
 334.88 462.44 m
5.09 -8.82 l
-10.18 0 l
cp p1
np
 344.02 487.14 m
5.09 -8.82 l
-10.18 0 l
cp p1
np
 353.16 506.30 m
5.09 -8.81 l
-10.18 0 l
cp p1
np
 362.29 520.06 m
5.09 -8.82 l
-10.18 0 l
cp p1
np
 371.43 527.53 m
5.09 -8.82 l
-10.18 0 l
cp p1
np
 380.57 528.94 m
5.09 -8.82 l
-10.18 0 l
cp p1
np
 389.71 525.64 m
5.09 -8.82 l
-10.19 0 l
cp p1
np
 398.84 519.08 m
5.09 -8.82 l
-10.18 0 l
cp p1
np
 407.98 510.40 m
5.09 -8.82 l
-10.18 0 l
cp p1
np
 417.12 500.40 m
5.09 -8.82 l
-10.18 0 l
cp p1
np
 426.25 489.65 m
5.10 -8.82 l
-10.19 0 l
cp p1
np
 435.39 478.57 m
5.09 -8.82 l
-10.18 0 l
cp p1
np
 444.53 467.46 m
5.09 -8.81 l
-10.18 0 l
cp p1
np
 453.67 456.57 m
5.09 -8.82 l
-10.18 0 l
cp p1
np
 462.80 446.06 m
5.10 -8.82 l
-10.19 0 l
cp p1
np
 471.94 436.05 m
5.09 -8.82 l
-10.18 0 l
cp p1
np
 481.08 426.61 m
5.09 -8.81 l
-10.18 0 l
cp p1
np
 490.22 417.81 m
5.09 -8.82 l
-10.18 0 l
cp p1
np
 499.35 409.64 m
5.09 -8.82 l
-10.18 0 l
cp p1
np
 508.49 402.11 m
5.09 -8.82 l
-10.18 0 l
cp p1
np
 517.63 395.20 m
5.09 -8.82 l
-10.18 0 l
cp p1
/bg { 1 1 1 srgb } def
0 0 0 srgb
440.53 636.94 93.54 -80.64 r p3
0.2667 0.0039 0.3294 srgb
np
 455.65 622.66 m
5.09 -8.81 l
-10.18 0 l
cp p1
0.8745 0.3255 0.4196 srgb
np
450.31 596.62 m
10.69 0 l
o
np
455.65 591.28 m
0 10.69 l
o
0.1176 0.5647 1 srgb
np
 450.31 576.46 m
5.34 5.35 l
5.35 -5.35 l
-5.35 -5.34 l
cp p1
/Font1 findfont 17 s
0 0 0 srgb
470.77 610.68 (124 kyr) 0 0 t
470.77 590.52 (95 kyr) 0 0 t
470.77 570.36 (41 kyr) 0 0 t
18.00 168.94 577.28 672.94 cl
0 0 0 srgb
0.75 setlinewidth
[] 0 setdash
1 setlinecap
1 setlinejoin
10.00 setmiterlimit
np
117.75 226.54 m
241.18 0 l
o
np
117.75 226.54 m
0 -7.20 l
o
np
159.35 226.54 m
0 -7.20 l
o
np
289.19 226.54 m
0 -7.20 l
o
np
358.93 226.54 m
0 -7.20 l
o
/Font1 findfont 17 s
117.75 200.62 (23.7) .5 0 t
159.35 200.62 (41) .5 0 t
289.19 200.62 (95) .5 0 t
358.93 200.62 (124) .5 0 t
199.23 241.58 (Timescale control par) 0 ta
-0.170 (ameter r) tb gr
145.59 182.66 (Natur) 0 ta
-0.170 (al per) tb
0.255 (iod rT) tb gr
/Font1 findfont 12 s
273.20 179.55 (0) 0 0 t
/Font1 findfont 17 s
279.88 182.66 ( \(kyr\)) 0 0 t
18.00 183.34 577.28 672.94 cl
/Font2 findfont 14 s
0 0 0 srgb
312.04 645.58 (\(b\) VCV18 model) .5 0 t
ep
%%Trailer
%%Pages: 1
%%EOF
