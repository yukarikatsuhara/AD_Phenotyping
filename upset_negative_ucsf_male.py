import pandas as pd
import io

csv_string = """
,c0,c3,c4,ICD
0,True,False,False,A04.72
1,True,False,False,A04.8
2,True,False,False,A41.50
3,True,False,False,A41.51
4,True,False,False,A41.9
5,True,False,False,A49.01
6,True,False,False,A49.8
7,True,False,False,B02.9
8,True,False,False,B07.9
9,True,False,False,B19.10
10,True,False,False,B35.1
11,True,False,False,B35.3
12,True,False,False,B35.6
13,True,False,False,B35.9
14,True,False,False,B37.0
15,True,False,False,B37.2
16,True,False,False,B37.49
17,True,False,False,B95.2
18,True,False,False,B95.4
19,True,False,False,B95.7
20,True,False,False,B96.1
21,True,False,False,B96.20
22,True,False,False,B96.5
23,True,False,False,B96.89
24,True,False,False,B97.89
25,True,False,False,B99.9
26,True,False,False,C18.9
27,True,False,False,C19
28,True,False,False,C44.320
29,True,False,False,C44.92
30,True,False,False,C61
31,True,False,False,C67.9
32,True,False,False,C79.51
33,True,False,False,C80.1
34,True,False,False,C90.00
35,True,False,False,C92.00
36,True,False,False,D01.0
37,True,False,False,D07.5
38,True,False,False,D12.6
39,True,False,False,D12.8
40,True,False,False,D12.9
41,True,False,False,D17.79
42,True,False,False,D18.00
43,True,False,False,D18.01
44,True,False,False,D22.5
45,True,False,False,D22.9
46,True,False,False,D23.30
47,True,False,False,D23.4
48,True,False,False,D23.5
49,True,False,False,D23.70
50,True,False,False,D23.9
51,True,False,False,D36.9
52,True,False,False,D47.2
53,True,False,False,D48.5
54,True,False,False,D48.7
55,True,False,False,D48.9
56,True,False,False,D49.2
57,True,False,False,D49.9
58,True,False,False,D50.0
59,True,False,False,D50.8
60,True,False,False,D50.9
61,True,False,False,D51.0
62,True,False,False,D51.8
63,True,False,False,D53.9
64,True,False,False,D62
65,True,False,False,D63.1
66,True,False,False,D63.8
67,True,False,False,D64.89
68,True,False,False,D64.9
69,True,False,False,D68.9
70,True,False,False,D69.59
71,True,False,False,D69.6
72,True,False,False,D70.9
73,True,False,False,D72.829
74,True,False,False,D75.89
75,True,False,False,D75.9
76,True,False,False,D84.9
77,True,False,False,E03
78,True,False,False,E03.8
79,True,False,False,E03.9
80,True,False,False,E06.3
81,True,False,False,E07.9
82,True,False,False,E10.39
83,True,False,False,E10.65
84,True,False,False,E10.9
85,True,False,False,E11.01
86,True,False,False,E11.22
87,True,False,False,E11.29
88,True,False,False,E11.39
89,True,False,False,E11.40
90,True,False,False,E11.42
91,True,False,False,E11.49
92,True,False,False,E11.51
93,True,False,False,E11.65
94,True,False,False,E11.69
95,True,False,False,E11.8
96,True,False,False,E11.9
97,True,False,False,E16.2
98,True,False,False,E23.6
99,True,False,False,E29.1
100,True,False,False,E43
101,True,False,False,E46
102,True,False,False,E53.8
103,True,False,False,E55.9
104,True,False,False,E66.8
105,True,False,False,E66.9
106,True,False,False,E73.9
107,True,False,False,E78.0
108,True,False,False,E78.00
109,True,False,False,E78.1
110,True,False,False,E78.2
111,True,False,False,E78.3
112,True,False,False,E78.49
113,True,False,False,E78.5
114,True,False,False,E78.6
115,True,False,False,E78.89
116,True,False,False,E78.9
117,True,False,False,E80.6
118,True,False,False,E83.30
119,True,False,False,E83.39
120,True,False,False,E83.40
121,True,False,False,E83.42
122,True,False,False,E83.51
123,True,False,False,E83.52
124,True,False,False,E83.59
125,True,False,False,E86.0
126,True,False,False,E86.1
127,True,False,False,E87.0
128,True,False,False,E87.1
129,True,False,False,E87.2
130,True,False,False,E87.3
131,True,False,False,E87.4
132,True,False,False,E87.5
133,True,False,False,E87.6
134,True,False,False,E87.70
135,True,False,False,E87.79
136,True,False,False,E87.8
137,True,False,False,E88.09
138,True,False,False,F01.50
139,True,False,True,F02.80
140,True,False,False,F02.81
141,True,False,False,F03.90
142,True,False,False,F03.91
143,True,False,False,F05
144,True,True,False,F06.8
145,True,False,False,F10.10
146,True,False,False,F10.20
147,True,False,False,F17.200
148,True,False,False,F17.210
149,True,False,False,F22
150,True,False,False,F29
151,True,False,False,F32.9
152,True,False,False,F34.1
153,True,False,False,F41.1
154,True,False,False,F41.9
155,True,False,False,F43.20
156,True,False,False,F43.21
157,True,False,False,F52.8
158,True,False,False,F52.9
159,True,False,False,F99
160,True,False,False,G20
161,True,False,False,G25.3
162,True,False,True,G31.84
163,True,False,False,G40.909
164,True,False,False,G45.9
165,True,False,False,G47.00
166,True,False,False,G47.30
167,True,False,False,G47.33
168,True,False,False,G47.9
169,True,False,False,G51.0
170,True,False,False,G58.9
171,True,False,False,G60.9
172,True,False,False,G62.9
173,True,False,False,G81.90
174,True,False,False,G89.18
175,True,False,False,G89.29
176,True,False,False,G90.9
177,True,False,False,G92
178,True,False,False,G93.40
179,True,False,True,G93.41
180,True,False,False,G93.49
181,True,False,False,G93.89
182,True,False,False,H00.029
183,True,False,False,H00.19
184,True,False,False,H01.003
185,True,False,False,H01.006
186,True,False,False,H01.009
187,True,False,False,H01.9
188,True,False,False,H02.409
189,True,False,False,H02.839
190,True,False,False,H04.123
191,True,False,False,H04.129
192,True,False,False,H04.209
193,True,False,False,H10.10
194,True,False,False,H10.9
195,True,False,False,H11.009
196,True,False,False,H11.159
197,True,False,False,H11.30
198,True,False,False,H18.50
199,True,False,False,H20.9
200,True,False,False,H25.019
201,True,False,False,H25.049
202,True,False,False,H25.10
203,True,False,False,H25.11
204,True,False,False,H25.12
205,True,False,False,H25.13
206,True,False,False,H25.89
207,True,False,False,H25.9
208,True,False,False,H26.40
209,True,False,False,H26.499
210,True,False,False,H26.8
211,True,False,False,H26.9
212,True,False,False,H35.039
213,True,False,False,H35.30
214,True,False,False,H35.3190
215,True,False,False,H35.3290
216,True,False,False,H35.359
217,True,False,False,H35.369
218,True,False,False,H40
219,True,False,False,H40.003
220,True,False,False,H40.009
221,True,False,False,H40.019
222,True,False,False,H40.039
223,True,False,False,H40.059
224,True,False,False,H40.10X0
225,True,False,False,H40.1190
226,True,False,False,H40.11X0
227,True,False,False,H40.1290
228,True,False,False,H40.1490
229,True,False,False,H40.9
230,True,False,False,H43.399
231,True,False,False,H43.813
232,True,False,False,H43.819
233,True,False,False,H52.00
234,True,False,False,H52.03
235,True,False,False,H52.10
236,True,False,False,H52.13
237,True,False,False,H52.203
238,True,False,False,H52.209
239,True,False,False,H52.4
240,True,False,False,H52.6
241,True,False,False,H53.2
242,True,False,False,H53.8
243,True,False,False,H53.9
244,True,False,False,H54.7
245,True,False,False,H57.10
246,True,False,False,H57.89
247,True,False,False,H60.399
248,True,False,False,H61.20
249,True,False,False,H61.22
250,True,False,True,H61.23
251,True,False,False,H66.90
252,True,False,False,H81.10
253,True,False,False,H90.3
254,True,False,False,H90.5
255,True,False,False,H90.8
256,True,False,False,H91.10
257,True,False,False,H91.8X9
258,True,False,False,H91.90
259,True,False,False,H91.93
260,True,False,False,H92.09
261,True,False,False,H93.19
262,True,False,False,I05.9
263,True,False,False,I08.0
264,True,False,False,I10
265,True,False,False,I11.0
266,True,False,False,I11.9
267,True,False,False,I12.0
268,True,False,False,I12.9
269,True,False,False,I13.0
270,True,False,False,I20.0
271,True,False,False,I20.9
272,True,False,False,I21.19
273,True,False,False,I21.4
274,True,False,False,I21.9
275,True,False,False,I24.8
276,True,False,False,I25.10
277,True,False,False,I25.2
278,True,False,False,I25.5
279,True,False,False,I25.810
280,True,False,False,I25.89
281,True,False,False,I25.9
282,True,False,False,I26.99
283,True,False,False,I27.89
284,True,False,False,I31.9
285,True,False,False,I33.0
286,True,False,False,I34.0
287,True,False,False,I35.0
288,True,False,False,I35.1
289,True,False,False,I35.8
290,True,False,False,I35.9
291,True,False,False,I36.9
292,True,False,False,I38
293,True,False,False,I42.0
294,True,False,False,I42.8
295,True,False,False,I44.0
296,True,False,False,I44.1
297,True,False,False,I44.2
298,True,False,False,I44.7
299,True,False,False,I45.10
300,True,False,False,I45.2
301,True,False,False,I45.5
302,True,False,False,I46.9
303,True,False,False,I47.1
304,True,False,False,I47.2
305,True,False,False,I48.0
306,True,False,False,I48.1
307,True,False,False,I48.19
308,True,False,False,I48.2
309,True,False,False,I48.20
310,True,False,False,I48.91
311,True,False,False,I48.92
312,True,False,False,I49.1
313,True,False,False,I49.3
314,True,False,False,I49.49
315,True,False,False,I49.5
316,True,False,False,I49.8
317,True,False,False,I49.9
318,True,False,False,I50.1
319,True,False,False,I50.20
320,True,False,False,I50.21
321,True,False,False,I50.22
322,True,False,False,I50.23
323,True,False,False,I50.30
324,True,False,False,I50.31
325,True,False,False,I50.32
326,True,False,False,I50.33
327,True,False,False,I50.9
328,True,False,False,I51.5
329,True,False,False,I51.7
330,True,False,False,I51.9
331,True,False,False,I62.00
332,True,False,False,I62.9
333,True,False,False,I63.30
334,True,False,False,I63.40
335,True,False,False,I63.50
336,True,False,False,I63.9
337,True,False,False,I65.23
338,True,False,False,I65.29
339,True,False,False,I65.8
340,True,False,False,I67.2
341,True,False,False,I67.82
342,True,False,False,I67.89
343,True,False,False,I69.90
344,True,False,False,I69.920
345,True,False,False,I69.959
346,True,False,False,I69.991
347,True,False,False,I70.0
348,True,False,False,I70.209
349,True,False,False,I70.219
350,True,False,False,I70.25
351,True,False,False,I70.8
352,True,False,False,I70.90
353,True,False,False,I71.4
354,True,False,False,I71.9
355,True,False,False,I72
356,True,False,False,I73.9
357,True,False,False,I74.3
358,True,False,False,I74.9
359,True,False,False,I77.1
360,True,False,False,I78.1
361,True,False,False,I82.409
362,True,False,False,I83.009
363,True,False,False,I83.10
364,True,False,False,I83.90
365,True,False,False,I87.2
366,True,False,False,I87.309
367,True,False,False,I87.8
368,True,False,False,I95.1
369,True,False,False,I95.89
370,True,False,False,I95.9
371,True,False,False,I99.8
372,True,False,False,I99.9
373,True,False,False,IMO0001
374,True,False,False,IMO0002
375,True,False,False,J00
376,True,False,False,J01.90
377,True,False,False,J02.9
378,True,False,False,J06.9
379,True,False,False,J10.1
380,True,False,False,J11.00
381,True,False,False,J11.1
382,True,False,False,J15.9
383,True,False,False,J18.1
384,True,False,False,J18.9
385,True,False,False,J20.9
386,True,False,False,J30.89
387,True,False,False,J30.9
388,True,False,False,J31.0
389,True,False,False,J32.9
390,True,False,False,J34.89
391,True,False,False,J40
392,True,False,False,J42
393,True,False,False,J43.8
394,True,False,False,J43.9
395,True,False,False,J44.0
396,True,False,False,J44.1
397,True,False,False,J44.9
398,True,False,False,J45.909
399,True,False,False,J45.998
400,True,False,False,J69.0
401,True,False,False,J81.0
402,True,False,False,J81.1
403,True,False,False,J84.10
404,True,False,False,J90
405,True,False,False,J95.811
406,True,False,False,J95.89
407,True,False,False,J96.00
408,True,False,False,J96.01
409,True,False,False,J96.02
410,True,False,False,J96.21
411,True,False,False,J96.91
412,True,False,False,J96.92
413,True,False,True,J98.11
414,True,False,False,J98.19
415,True,False,False,J98.4
416,True,False,False,J98.8
417,True,False,False,K06.8
418,True,False,False,K08.89
419,True,False,False,K08.9
420,True,False,False,K13.70
421,True,False,False,K20.9
422,True,False,False,K21.0
423,True,False,False,K21.9
424,True,False,False,K22.4
425,True,False,False,K27.9
426,True,False,False,K29.6
427,True,False,False,K29.70
428,True,False,False,K31.89
429,True,False,False,K31.9
430,True,False,False,K40.20
431,True,False,False,K40.90
432,True,False,False,K42.9
433,True,False,False,K43.9
434,True,False,False,K44.9
435,True,False,False,K52.9
436,True,False,False,K56.609
437,True,False,False,K57.30
438,True,False,False,K57.32
439,True,False,False,K57.90
440,True,False,False,K58.9
441,True,False,False,K59.00
442,True,False,False,K59.01
443,True,False,False,K59.09
444,True,False,False,K62.0
445,True,False,False,K62.1
446,True,False,False,K62.5
447,True,False,False,K62.89
448,True,False,False,K63.5
449,True,False,False,K63.89
450,True,False,False,K63.9
451,True,False,False,K64.4
452,True,False,False,K64.8
453,True,False,False,K64.9
454,True,False,False,K74.60
455,True,False,False,K76.89
456,True,False,False,K76.9
457,True,False,False,K80.00
458,True,False,False,K80.20
459,True,False,False,K80.50
460,True,False,False,K81.0
461,True,False,False,K81.9
462,True,False,False,K83.09
463,True,False,False,K83.1
464,True,False,False,K83.8
465,True,False,False,K92.0
466,True,False,False,K92.1
467,True,False,False,K92.2
468,True,False,False,L02.01
469,True,False,False,L02.419
470,True,False,False,L02.619
471,True,False,False,L02.91
472,True,False,False,L03.019
473,True,False,False,L03.039
474,True,False,False,L03.116
475,True,False,False,L03.119
476,True,False,False,L03.818
477,True,False,False,L03.90
478,True,False,False,L08.9
479,True,False,False,L20.89
480,True,False,False,L21.0
481,True,False,False,L21.8
482,True,False,False,L21.9
483,True,False,False,L25.9
484,True,False,False,L27.0
485,True,False,False,L28.0
486,True,False,False,L29.8
487,True,False,False,L29.9
488,True,False,False,L30.8
489,True,False,False,L30.9
490,True,False,False,L53.9
491,True,False,False,L57.0
492,True,False,False,L57.8
493,True,False,False,L60.0
494,True,False,False,L60.2
495,True,False,False,L60.8
496,True,False,False,L71.9
497,True,False,False,L72.0
498,True,False,False,L72.3
499,True,False,False,L73.8
500,True,False,False,L81.4
501,True,False,False,L81.9
502,True,False,False,L82.0
503,True,False,False,L82.1
504,True,False,False,L84
505,True,False,False,L85.1
506,True,False,False,L85.3
507,True,False,False,L89.109
508,True,False,False,L89.152
509,True,False,False,L89.309
510,True,False,False,L89.609
511,True,False,False,L89.90
512,True,False,False,L89.92
513,True,False,False,L90.5
514,True,False,False,L97.409
515,True,False,False,L97.509
516,True,False,False,L97.909
517,True,False,False,L98.499
518,True,False,False,L98.8
519,True,False,False,L98.9
520,True,False,False,M10.9
521,True,False,False,M12.9
522,True,False,False,M15.0
523,True,False,False,M15.9
524,True,False,False,M16.10
525,True,False,False,M16.9
526,True,False,False,M17.0
527,True,False,False,M17.10
528,True,False,False,M17.11
529,True,False,False,M17.12
530,True,False,False,M19.019
531,True,False,False,M19.041
532,True,False,False,M19.042
533,True,False,False,M19.049
534,True,False,False,M19.079
535,True,False,False,M19.90
536,True,False,False,M20.40
537,True,False,False,M21.619
538,True,False,False,M25.461
539,True,False,False,M25.469
540,True,False,False,M25.473
541,True,False,False,M25.50
542,True,False,False,M25.511
543,True,False,False,M25.512
544,True,False,False,M25.519
545,True,False,False,M25.529
546,True,False,False,M25.539
547,True,False,False,M25.551
548,True,False,False,M25.552
549,True,False,False,M25.559
550,True,False,False,M25.561
551,True,False,False,M25.562
552,True,False,False,M25.569
553,True,False,False,M25.579
554,True,False,False,M25.78
555,True,False,False,M41.20
556,True,False,False,M43.10
557,True,False,False,M47.812
558,True,False,False,M47.816
559,True,False,False,M47.817
560,True,False,False,M48.00
561,True,False,False,M48.02
562,True,False,False,M48.061
563,True,False,False,M50.30
564,True,False,False,M50.90
565,True,False,False,M51.26
566,True,False,False,M51.36
567,True,False,False,M51.37
568,True,False,False,M53.80
569,True,False,False,M54.12
570,True,False,False,M54.16
571,True,False,False,M54.2
572,True,False,False,M54.30
573,True,False,False,M54.5
574,True,False,False,M54.6
575,True,False,False,M54.9
576,True,False,False,M62.81
577,True,False,False,M62.838
578,True,False,False,M62.9
579,True,False,False,M65.30
580,True,False,False,M67.919
581,True,False,False,M71.9
582,True,False,False,M75.80
583,True,False,False,M76.899
584,True,False,False,M77.9
585,True,False,False,M79.18
586,True,False,False,M79.604
587,True,False,False,M79.605
588,True,False,False,M79.606
589,True,False,False,M79.609
590,True,False,False,M79.641
591,True,False,False,M79.671
592,True,False,False,M79.672
593,True,False,False,M79.673
594,True,False,False,M79.81
595,True,False,False,M79.89
596,True,False,False,M81.0
597,True,False,False,M81.8
598,True,False,False,M84.48XA
599,True,False,False,M85.80
600,True,False,False,M86.9
601,True,False,False,M89.8X9
602,True,False,False,M89.9
603,True,False,False,M94.8X9
604,True,False,False,M94.9
605,True,False,False,N03.9
606,True,False,False,N12
607,True,False,False,N13.30
608,True,False,False,N13.8
609,True,False,False,N13.9
610,True,False,False,N17.0
611,True,False,False,N17.9
612,True,False,False,N18.1
613,True,False,False,N18.2
614,True,False,False,N18.3
615,True,False,False,N18.4
616,True,False,False,N18.5
617,True,False,False,N18.6
618,True,False,False,N18.9
619,True,False,False,N19
620,True,False,False,N20.0
621,True,False,False,N21.0
622,True,False,False,N25.81
623,True,False,False,N25.9
624,True,False,False,N28.1
625,True,False,False,N28.89
626,True,False,False,N28.9
627,True,False,False,N30.00
628,True,False,False,N31.8
629,True,False,False,N32.0
630,True,False,False,N32.3
631,True,False,False,N32.81
632,True,False,False,N32.89
633,True,False,False,N39.0
634,True,False,False,N39.41
635,True,False,False,N39.46
636,True,False,False,N39.498
637,True,False,False,N40.0
638,True,False,False,N40.1
639,True,False,False,N41.0
640,True,False,False,N41.9
641,True,False,False,N42.9
642,True,False,False,N43.3
643,True,False,False,N45.3
644,True,False,False,N47.6
645,True,False,False,N48.89
646,True,False,False,N50.89
647,True,False,False,N50.9
648,True,False,False,N52.9
649,True,False,False,R00.0
650,True,False,False,R00.1
651,True,False,False,R00.2
652,True,False,False,R00.8
653,True,False,False,R01.1
654,True,False,False,R03.0
655,True,False,False,R03.1
656,True,False,False,R04.0
657,True,False,False,R04.2
658,True,False,False,R05
659,True,False,False,R06.0
660,True,False,False,R06.00
661,True,False,False,R06.01
662,True,False,False,R06.02
663,True,False,False,R06.09
664,True,False,False,R06.2
665,True,False,False,R06.6
666,True,False,False,R06.82
667,True,False,False,R06.89
668,True,False,False,R06.9
669,True,False,False,R07.0
670,True,False,False,R07.1
671,True,False,False,R07.2
672,True,False,False,R07.81
673,True,False,False,R07.89
674,True,False,False,R07.9
675,True,False,False,R09.02
676,True,False,False,R09.1
677,True,False,False,R09.81
678,True,False,False,R09.89
679,True,False,False,R10.11
680,True,False,False,R10.12
681,True,False,False,R10.13
682,True,False,False,R10.31
683,True,False,False,R10.32
684,True,False,False,R10.84
685,True,False,False,R10.9
686,True,False,False,R11.0
687,True,False,False,R11.10
688,True,False,False,R11.2
689,True,False,False,R12
690,True,False,False,R13.1
691,True,False,False,R13.10
692,True,False,False,R13.12
693,True,False,False,R13.13
694,True,False,False,R13.19
695,True,False,False,R14.0
696,True,False,False,R14.1
697,True,False,False,R14.2
698,True,False,False,R14.3
699,True,False,False,R15.9
700,True,False,False,R17
701,True,False,False,R18.8
702,True,False,False,R19.00
703,True,False,False,R19.5
704,True,False,False,R19.7
705,True,False,False,R19.8
706,True,False,False,R20.0
707,True,False,False,R20.2
708,True,False,False,R20.9
709,True,False,False,R21
710,True,False,False,R22.0
711,True,False,False,R22.1
712,True,False,False,R22.2
713,True,False,False,R22.9
714,True,False,False,R23.3
715,True,False,False,R25.1
716,True,False,False,R25.2
717,True,False,False,R25.8
718,True,False,False,R25.9
719,True,False,False,R26.2
720,True,False,False,R26.81
721,True,False,False,R26.89
722,True,False,False,R26.9
723,True,False,False,R27.9
724,True,False,False,R29.6
725,True,False,False,R29.810
726,True,False,False,R29.818
727,True,False,False,R29.898
728,True,False,False,R30.0
729,True,False,False,R31.0
730,True,False,False,R31.29
731,True,False,False,R31.9
732,True,False,False,R32
733,True,False,False,R33.8
734,True,False,False,R33.9
735,True,False,False,R34
736,True,False,False,R35.0
737,True,False,False,R35.1
738,True,False,False,R35.8
739,True,False,False,R39.11
740,True,False,False,R39.15
741,True,False,False,R39.198
742,True,False,False,R39.89
743,True,False,False,R39.9
744,True,False,False,R40.0
745,True,False,False,R40.4
746,True,False,False,R41.0
747,True,False,False,R41.3
748,True,False,False,R41.81
749,True,False,False,R41.82
750,True,False,True,R41.89
751,True,False,False,R42
752,True,False,False,R45.1
753,True,False,False,R47.1
754,True,False,False,R49.0
755,True,False,False,R50.81
756,True,False,False,R50.9
757,True,False,False,R51
758,True,False,False,R52
759,True,False,False,R53.1
760,True,False,False,R53.81
761,True,False,False,R53.82
762,True,False,False,R53.83
763,True,False,False,R54
764,True,False,False,R55
765,True,False,False,R56.9
766,True,False,False,R57.8
767,True,False,False,R57.9
768,True,False,False,R58
769,True,False,False,R59.0
770,True,False,False,R59.9
771,True,False,False,R60.0
772,True,False,False,R60.9
773,True,False,False,R61
774,True,False,False,R62.7
775,True,False,False,R63.0
776,True,False,False,R63.4
777,True,False,False,R63.5
778,True,False,False,R64
779,True,False,False,R65.10
780,True,False,False,R65.20
781,True,False,False,R65.21
782,True,False,False,R68.89
783,True,False,False,R69
784,True,False,False,R73.01
785,True,False,False,R73.02
786,True,False,False,R73.03
787,True,False,False,R73.09
788,True,False,False,R73.9
789,True,False,False,R74.0
790,True,False,False,R74.8
791,True,False,False,R76.11
792,True,False,False,R78.81
793,True,False,False,R79.1
794,True,False,False,R79.89
795,True,False,False,R79.9
796,True,False,False,R80.8
797,True,False,False,R80.9
798,True,False,False,R82.90
799,True,False,False,R82.998
800,True,False,False,R90.89
801,True,False,False,R91.1
802,True,False,False,R91.8
803,True,False,False,R93.0
804,True,False,False,R93.2
805,True,False,False,R93.3
806,True,False,False,R93.5
807,True,False,False,R93.7
808,True,False,False,R93.8
809,True,False,False,R93.89
810,True,False,False,R94.02
811,True,False,False,R94.2
812,True,False,False,R94.30
813,True,False,False,R94.31
814,True,False,False,R94.39
815,True,False,False,R94.4
816,True,False,False,R94.5
817,True,False,False,R94.6
818,True,False,False,R97.20
819,True,False,False,S00.03XA
820,True,False,False,S00.83XA
821,True,False,False,S01.00XA
822,True,False,False,S01.01XA
823,True,False,False,S01.80XA
824,True,False,False,S01.81XA
825,True,False,False,S01.90XA
826,True,False,False,S06.5X0A
827,True,False,False,S06.5X9A
828,True,False,False,S09.90XA
829,True,False,False,S10.93XA
830,True,False,False,S19.9XXA
831,True,False,False,S20.219A
832,True,False,False,S22.39XA
833,True,False,False,S22.49XA
834,True,False,False,S29.8XXA
835,True,False,False,S29.9XXA
836,True,False,False,S32.009A
837,True,False,False,S40.029A
838,True,False,False,S43.429A
839,True,False,False,S49.80XA
840,True,False,False,S59.909A
841,True,False,False,S59.919A
842,True,False,False,S61.209A
843,True,False,False,S61.219A
844,True,False,False,S69.90XA
845,True,False,False,S72.001A
846,True,False,False,S72.009A
847,True,False,False,S79.919A
848,True,False,False,S79.929A
849,True,False,False,S80.10XA
850,True,False,False,S89.90XA
851,True,False,False,S93.409A
852,True,False,False,S99.919A
853,True,False,False,S99.929A
854,True,False,False,T07.XXXA
855,True,False,False,T14.8XXA
856,True,False,False,T14.90XA
857,True,False,False,T17.908A
858,True,False,False,T17.908D
859,True,False,False,T45.1X5A
860,True,False,False,T78.40XA
861,True,False,False,T81.40XA
862,True,False,False,T82.190A
863,True,False,False,T82.897A
864,True,False,False,T82.898A
865,True,False,False,T83.091A
866,True,False,False,T83.511A
867,True,False,False,T83.89XA
868,True,False,False,T83.9XXA
869,True,False,False,W01.0XXA
870,True,False,False,W06.XXXA
871,True,False,False,W07.XXXA
872,True,False,False,W10.8XXA
873,True,False,False,W18.09XA
874,True,False,False,W18.30XA
875,True,False,False,W19.XXXA
876,True,False,False,W19.XXXD
877,True,False,False,W19.XXXS
878,True,False,False,X58.XXXA
879,True,False,False,Y83.1
880,True,False,False,Y83.8
881,True,False,False,Y84.6
882,True,False,False,Y84.8
883,True,False,False,Y92.009
884,True,False,False,Y92.10
885,True,False,False,Y92.239
886,True,False,False,Y92.89
887,True,False,False,Y92.9
888,True,False,False,Y95
889,True,False,False,Y99.9
890,True,False,False,Z00.00
891,True,False,False,Z00.8
892,True,False,False,Z01.00
893,True,False,False,Z01.810
894,True,False,False,Z01.818
895,True,False,False,Z02.89
896,True,False,False,Z02.9
897,True,False,False,Z03.89
898,True,False,False,Z04.3
899,True,False,False,Z09
900,True,False,False,Z11.1
901,True,False,False,Z11.59
902,True,False,False,Z12.11
903,True,False,False,Z12.12
904,True,False,False,Z12.5
905,True,False,False,Z12.89
906,True,False,False,Z12.9
907,True,False,False,Z13.1
908,True,False,False,Z13.31
909,True,False,False,Z13.6
910,True,False,False,Z13.820
911,True,False,False,Z13.89
912,True,False,False,Z13.9
913,True,False,False,Z23
914,True,False,False,Z29.9
915,True,False,False,Z43.1
916,True,False,False,Z43.8
917,True,False,False,Z45.018
918,True,False,False,Z45.02
919,True,False,False,Z45.2
920,True,False,False,Z46.1
921,True,False,False,Z46.2
922,True,False,False,Z46.59
923,True,False,False,Z46.6
924,True,False,False,Z46.82
925,True,False,False,Z47.1
926,True,False,False,Z47.89
927,True,False,False,Z48.00
928,True,False,False,Z48.02
929,True,False,False,Z48.812
930,True,False,False,Z48.815
931,True,False,False,Z48.89
932,True,False,False,Z51.11
933,True,False,False,Z51.5
934,True,False,False,Z51.81
935,True,False,False,Z51.89
936,True,False,False,Z53.09
937,True,False,False,Z53.20
938,True,False,False,Z53.8
939,True,False,False,Z63.6
940,True,False,False,Z66
941,True,False,False,Z68.1
942,True,False,False,Z71.3
943,True,False,False,Z71.89
944,True,False,False,Z71.9
945,True,False,False,Z72.89
946,True,False,False,Z74.01
947,True,False,False,Z74.09
948,True,False,False,Z74.2
949,True,False,False,Z76.0
950,True,False,False,Z78.1
951,True,False,False,Z78.9
952,True,False,False,Z79.01
953,True,False,False,Z79.02
954,True,False,False,Z79.2
955,True,False,False,Z79.4
956,True,False,False,Z79.51
957,True,False,False,Z79.82
958,True,False,False,Z79.84
959,True,False,False,Z79.899
960,True,False,False,Z80.0
961,True,False,False,Z80.8
962,True,False,False,Z82.0
963,True,False,False,Z82.3
964,True,False,False,Z82.49
965,True,False,False,Z83.3
966,True,False,False,Z85.038
967,True,False,False,Z85.46
968,True,False,False,Z85.820
969,True,False,False,Z85.828
970,True,False,False,Z86.010
971,True,False,False,Z86.19
972,True,False,False,Z86.718
973,True,False,False,Z86.73
974,True,False,False,Z86.79
975,True,False,False,Z87.01
976,True,False,False,Z87.11
977,True,False,False,Z87.19
978,True,False,False,Z87.440
979,True,False,False,Z87.442
980,True,False,False,Z87.81
981,True,False,False,Z87.891
982,True,False,False,Z88.0
983,True,False,False,Z88.2
984,True,False,False,Z88.5
985,True,False,False,Z88.8
986,True,False,False,Z90.79
987,True,False,False,Z91.14
988,True,False,False,Z91.19
989,True,False,False,Z91.81
990,True,False,False,Z91.89
991,True,False,False,Z92.21
992,True,False,False,Z92.3
993,True,False,False,Z93.1
994,True,False,False,Z95.0
995,True,False,False,Z95.1
996,True,False,False,Z95.2
997,True,False,False,Z95.4
998,True,False,False,Z95.5
999,True,False,False,Z95.810
1000,True,False,False,Z95.828
1001,True,False,False,Z96.0
1002,True,False,False,Z96.1
1003,True,False,False,Z96.649
1004,True,False,False,Z96.659
1005,True,False,False,Z97.8
1006,True,False,False,Z98.1
1007,True,False,False,Z98.41
1008,True,False,False,Z98.42
1009,True,False,False,Z98.49
1010,True,False,False,Z98.61
1011,True,False,False,Z98.89
1012,True,False,False,Z98.890
1013,True,False,False,Z99.11
1014,True,False,False,Z99.3
1015,True,False,False,Z99.81
"""

df = pd.read_csv(io.StringIO(csv_string))