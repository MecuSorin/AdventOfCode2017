namespace AdventOfCode2017

module Puzzle2 =
    let public extremeDiff (input: string) = 
        input.Split([|'\n'|], System.StringSplitOptions.RemoveEmptyEntries)
        |> Array.map (fun s -> 
            s.Split([|' '; '\t'|], System.StringSplitOptions.RemoveEmptyEntries)
            |> Array.map int)
        |> Array.sumBy (fun s ->
            let max = Array.max s
            let min = Array.min s
            max - min )


module Puzzle2Tests =
    open Expecto
    open Puzzle2

    let (==?) expected actual = Expect.equal actual expected "Should be equal"

    let basePuzzle21Test testFunction label expected sample= testFunction label <| fun _ ->
        expected ==? extremeDiff sample

    [<Tests>]
    let sample1 = basePuzzle21Test testCase "Sample 5 1 9 5" 8 "5 1 9 5"
    [<Tests>]
    let sample2 = basePuzzle21Test testCase "Sample 7 5 3" 4 "7 5 3"
    [<Tests>]
    let sample3 = basePuzzle21Test testCase "Sample 2 4 6 8" 6 "2 4 6 8"

    let puzzle2Sample = """6046	6349	208	276	4643	1085	1539	4986	7006	5374	252	4751	226	6757	7495	2923
1432	1538	1761	1658	104	826	806	109	939	886	1497	280	1412	127	1651	156
244	1048	133	232	226	1072	883	1045	1130	252	1038	1022	471	70	1222	957
87	172	93	73	67	192	249	239	155	23	189	106	55	174	181	116
5871	204	6466	6437	5716	232	1513	7079	6140	268	350	6264	6420	3904	272	5565
1093	838	90	1447	1224	744	1551	59	328	1575	1544	1360	71	1583	75	370
213	166	7601	6261	247	210	4809	6201	6690	6816	7776	2522	5618	580	2236	3598
92	168	96	132	196	157	116	94	253	128	60	167	192	156	76	148
187	111	141	143	45	132	140	402	134	227	342	276	449	148	170	348
1894	1298	1531	1354	1801	974	85	93	1712	130	1705	110	314	107	449	350
1662	1529	784	1704	1187	83	422	146	147	1869	1941	110	525	1293	158	1752
162	1135	3278	1149	3546	3686	182	149	119	1755	3656	2126	244	3347	157	865
2049	6396	4111	6702	251	669	1491	245	210	4314	6265	694	5131	228	6195	6090
458	448	324	235	69	79	94	78	515	68	380	64	440	508	503	452
198	216	5700	4212	2370	143	5140	190	4934	539	5054	3707	6121	5211	549	2790
3021	3407	218	1043	449	214	1594	3244	3097	286	114	223	1214	3102	257	3345"""
   
    [<Tests>]
    let puzzle2_1 = basePuzzle21Test testCase "Puzzle 2 > 1" 50376 puzzle2Sample



    // let basePuzzle12Test testFunction label sample expected = testFunction label <| fun _ ->
    //     expected ==? findCaptcha2 sample

    
    // [<Tests>]
    // let sample21 = basePuzzle12Test testCase "Sample 1212" "1212" 6
    // [<Tests>]
    // let sample22 = basePuzzle12Test testCase "Sample 1221" "1221" 0
    // [<Tests>]
    // let sample23 = basePuzzle12Test testCase "Sample 123425" "123425" 4
    // [<Tests>]
    // let sample24 = basePuzzle12Test testCase "Sample 123123" "123123" 12
    // [<Tests>]
    // let sample25 = basePuzzle12Test testCase "Sample 12131415" "12131415" 4
    // [<Tests>]
    // let puzzle1_2 = basePuzzle12Test testCase "Puzzle 1 > 2" puzzle1Sample 1152