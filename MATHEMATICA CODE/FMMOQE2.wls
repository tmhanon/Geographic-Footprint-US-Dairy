myDir = DirectoryName[$InputFileName];

figure1 = 
 Show[Plot[{M^(-1/1.1), 1, (M/1.5)^.5, (1 - 1/1.1)*M^(-1/1.1 - 1), 
    Piecewise[{{M^(-1/1.1), 
       M < 0.2847794149600383`}, \
{(0.2847794149600383`*0.2847794149600383`^(-1/1.1) + M - 
          0.2847794149600383`)/M, M >= 0.2847794149600383`}}]}, {M, 0,
     3}, PlotRange -> {0, 4}, 
   PlotStyle -> {{Gray, Thick, Dashed}, {Gray, Thick, Dashed}, 
     Black, {Gray, Dotted}, LightGray}, 
   AxesLabel -> {Style[M, Medium], Style[m, Medium]}, 
   Ticks -> {{{0.2847794149600383`, 
       Style[Subsuperscript[M, 1, b], Medium]}, {1.5, 
       Style[Superscript[M, c], Medium]}, {2.3680445261942342`, 
       Style[Superscript[M, b], Medium]}, {1, 
       Style[Subsuperscript[M, 1, c], Medium]}}, {{1, 
       Style[Superscript[m, c], Medium]}, {1.25646183817741`, 
       Style[Superscript[m, b], Medium]}, {3.1325735645567816`, 
       Style[Subsuperscript[m, 1, b], Medium]}}}, ImageSize -> 600], 
  Graphics[{Text[Style[MC, Bold, Medium, Italic], {2.8, 1.5}], 
    Text[Style[AR, Bold, Italic, Medium, LightGray], {1.4, 1.6}], 
    Text[Style[Subscript[D, 1], Medium], {2.8, .5}], 
    Text[Style[Subscript[D, \[Infinity]], Medium], {2.8, .85}], 
    Text[Style[Subscript[MR, 1], Medium, Italic], {.75, .3}], {Dashed,
      Line[{{{1, 0}, {1, 1}}, {{0.2847794149600383`, 
         0}, {0.2847794149600383`, 3.1325735645567816`}}, {{1.5, 
         0}, {1.5, 1}}, {{0, 1.25646183817741`}, {2.3680445261942342`,
          1.25646183817741`}}, {{2.3680445261942342`, 
         0}, {2.3680445261942342`, 1.25646183817741`}}, {{0, 
         3.1325735645567816`}, {0.2847794149600383`, 
         3.1325735645567816`}}}]}}]]

If[!DirectoryQ["../MANUSCRIPT FIGURES/Figure 1"],
   CreateDirectory[FileNameJoin[{myDir, 
   "../MANUSCRIPT FIGURES/Figure 1"}], 
 CreateIntermediateDirectories -> True]
]

Export[FileNameJoin[{myDir, "..", "MANUSCRIPT FIGURES", 
   "Figure 1", "figure1.png"}], figure1, "PNG"]

figureg1 = 
 Show[Plot[{M^(-1/.4), M^(-1/1.2), (M/2)^.5, 
    x /. NSolve[M == x^(-.4) + x^(-1.2), x][[1]], 1.2, 1, 
    Piecewise[{{4.026420188608882`, 
       M < 0.5728387152134832`}, {M^(-1/.4), 
       0.5728387152134832` <= M < 1}, {1, M >= 1}}], 
    Piecewise[{{1, M < 1}, {M^(-1/1.2), M >= 1}}], 
    Piecewise[{{0.4982359230052799`, 
       M < 2.3071612847865164`}, {M^(-1/1.2), 
       M >= 2.3071612847865164`}}], 
    Piecewise[{{1, M < 2}, {(M/2)^.5, 2 <= M < 2.88}, {1.2, 
       M >= 2.88}}]}, {M, 0, 3.5}, PlotRange -> {0, 4.6}, 
   PlotStyle -> {{Gray, Thick, Dashed}, {Gray, Thick, Dashed}, Black, 
     Black, None, None, None, None, None, None}, 
   AxesLabel -> {Style[M, Medium], Style[m, Medium]}, 
   Filling -> {5 -> {{10}, HatchFilling[]}, 
     7 -> {{6}, Opacity[.3, Cyan]}, 8 -> {{9}, Opacity[.4, Orange]}}, 
   Ticks -> {{{2, Style[Superscript[M, c], Medium]}, {2.88`, 
       Style[Superscript[M, b], Medium]}, {0.5728387152134832`, 
       Style[Subsuperscript[M, 1, b], Medium]}, {2.3071612847865164`, 
       Style[Subsuperscript[M, 2, b], Medium]}}, {{1, 
       Style[Superscript[m, c], Medium]}, {1.2, 
       Style[Superscript[m, b], Medium]}, {0.4982359230052799`, 
       Style[Subsuperscript[m, 2, b], Medium]}, {4.026420188608882`, 
       Style[Subsuperscript[m, 1, b], Medium]}}}, 
   PlotLegends -> 
    Placed[SwatchLegend[{Opacity[.3, Cyan], Opacity[.4, Orange], 
       HatchFilling[]}, {Style[Subscript[\[CapitalDelta]CS, 1] < 0, 
        Medium], Style[Subscript[\[CapitalDelta]CS, 2] > 0, Medium], 
       Style[\[CapitalDelta]PS > 0, Medium]}], {.7, 0.7}], 
   ImageSize -> 600], 
  Graphics[{Text[Style[Subscript[D, 2], Medium], {.12, 4.2}], 
    Text[Style[Subscript[D, 1], Medium], {.5, 4.2}], 
    Text[Style[D, Bold, Medium], {.81, 4.2}], 
    Text[Style[MC, Bold, Medium, Italic], {3.35, 1.4}], {Dashed, 
     Line[{{{0.5728387152134832`, 0}, {0.5728387152134832`, 
         4.026420188608882`}}, {{2.3071612847865164`, 
         0}, {2.3071612847865164`, 0.4982359230052799`}}, {{2, 0}, {2,
          1}}, {{0, 1}, {2, 1}}, {{0, 1.2}, {2*1.2^2, 1.2}}, {{2.88, 
         0}, {2.88, 1.2}}, {{0, 
         4.026420188608882`}, {0.5728387152134832`, 
         4.026420188608882`}}, {{0, 
         0.4982359230052799`}, {2.3071612847865164`, 
         0.4982359230052799`}}}]}}]]

If[!DirectoryQ["../MANUSCRIPT FIGURES/Figure G.1"],
     CreateDirectory[FileNameJoin[{myDir, 
   "../MANUSCRIPT FIGURES/Figure G.1"}], 
 CreateIntermediateDirectories -> True]
]

Export[FileNameJoin[{myDir, "..", "MANUSCRIPT FIGURES", 
   "Figure G.1", "figureg1.png"}], figureg1, "PNG"]

FindTaus[tau_, sigma_, epsilon1_, epsilon2_, eta_] := 
 FindRoot[{sigma*tau1^(-epsilon1) + (1 - sigma)*tau2^(-epsilon2) == 
    tau^eta, 
   sigma*tau1^(1 - epsilon1) + (1 - sigma)*tau2^(1 - epsilon2) == 
    tau^(1 + eta), 
   sigma*tauq^(1 - epsilon1) + (1 - sigma)*tauq^(1 - epsilon2) - 
     eta/(1 + 
         eta)*(sigma*tauq^(-epsilon1) + (1 - sigma)*
          tauq^(-epsilon2))^(1 + 1/eta) == 
    tau^(1 + eta)/(1 + eta)}, {tau1, 1}, {tau2, 1}, {tauq, 1}]

DWLwedge[tau_, sigma_, epsilon1_, epsilon2_, 
  eta_] := (tau^(1 + eta) - 1)/(1 + eta) - 
  sigma*((tau1 /. FindTaus[tau, sigma, epsilon1, epsilon2, eta])^(1 - 
         epsilon1) - 1)/(1 - epsilon1) - (1 - 
     sigma)*((tau2 /. 
         FindTaus[tau, sigma, epsilon1, epsilon2, eta])^(1 - 
         epsilon2) - 1)/(1 - epsilon2)

figureg2a = 
 Show[Plot3D[{DWLwedge[1.003, .2, epsilon1, epsilon2, 3]}, {epsilon1, 
    0.01, .99}, {epsilon2, 0.01, 5}, PlotRange -> {-.005, 0}, 
   RegionFunction -> 
    Function[{epsilon1, epsilon2, z}, epsilon1 < epsilon2], 
   AxesLabel -> {Style[Subscript[\[Epsilon], 1], Medium], 
     Style[Subscript[\[Epsilon], 2], Medium], Style[\!\(\*
TagBox[
FractionBox["\[CapitalDelta]W", 
RowBox[{
TemplateBox[{"m", "c"},
"Superscript"], " ", 
TemplateBox[{"M", "c"},
"Superscript"]}]],
DisplayForm]\), Medium]}, ViewPoint -> {1.3`, -2.4`, 2.`}], 
  Graphics3D[{{PointSize[.03], Blue, 
     Point[{.1, .3, DWLwedge[1.003, .2, .1, .3, 3]}]}, {Dashed, Thick,
      Red, Line[{{0, 0, -.005}, {1, 1, -.005}}]}}], 
  ViewPoint -> {-1.9662642483969808`, -2.306915774739927, 
    1.5039762344305603`}]

If[!DirectoryQ["../MANUSCRIPT FIGURES/Figure G.2"],
   CreateDirectory[FileNameJoin[{myDir, 
   "../MANUSCRIPT FIGURES/Figure G.2"}], 
 CreateIntermediateDirectories -> True]
]

Export[FileNameJoin[{myDir, "..", "MANUSCRIPT FIGURES", 
   "Figure G.2", "figureg2a.png"}], figureg2a, "PNG"]

figureg2b = 
 Show[Plot3D[
   DWLwedge[1.003, .2, epsilon1, epsilon2, .5], {epsilon1, 
    0.01, .99}, {epsilon2, 0.01, 5}, PlotRange -> {-.005, 0}, 
   RegionFunction -> 
    Function[{epsilon1, epsilon2, z}, epsilon1 < epsilon2], 
   AxesLabel -> {Style[Subscript[\[Epsilon], 1], Medium], 
     Style[Subscript[\[Epsilon], 2], Medium], Style[\!\(\*
TagBox[
FractionBox["\[CapitalDelta]W", 
RowBox[{
TemplateBox[{"m", "c"},
"Superscript"], " ", 
TemplateBox[{"M", "c"},
"Superscript"]}]],
DisplayForm]\), Medium]}], 
  Graphics3D[{{PointSize[.03], Blue, 
     Point[{.1, .3, -0.000050530700707131226`}]}, {Dashed, Thick, Red,
      Line[{{0, 0, -.005}, {1, 1, -.005}}]}}], 
  ViewPoint -> {-1.948987858780701, -2.3121628317966008`, 
    1.5183376981369328`}]

Export[FileNameJoin[{myDir, "..", "MANUSCRIPT FIGURES", 
   "Figure G.2", "figureg2b.png"}], figureg2b, "PNG"]

figureg3 = 
 Show[Plot[{M^(-1/.4), M^(-1/1.2), (M/2)^.5, 
    x /. NSolve[M == x^(-.4) + x^(-1.2), x][[1]], 1.2, 
    Piecewise[{{1.2, M < 1.6197440131489613`}, {(M/2)^.5, 
       1.6197440131489613` <= M < 2.88`}, {1.2, M >= 2.88`}}], 
    Piecewise[{{1.311176089366584`, M < 1.6197440131489613`}, {1.2, 
       M >= 1.6197440131489613`}}], 
    Piecewise[{{1.311176089366584`, 
       M < 1.6197440131489613`}, {x /. 
        NSolve[M == x^(-.4) + x^(-1.2), x][[1]], 
       1.6197440131489613` <= M < 2}, {1, M >= 2}}], 1, 
    Piecewise[{{4.026420188608882`, 
       M < 0.5728387152134832`}, {M^(-1/.4), 
       0.5728387152134832` <= M < 1}, {1, M >= 1}}], 
    Piecewise[{{1, M < 1}, {M^(-1/1.2), M >= 1}}], 
    Piecewise[{{0.4982359230052799`, 
       M < 2.3071612847865164`}, {M^(-1/1.2), 
       M >= 2.3071612847865164`}}]}, {M, 0, 3.5}, 
   PlotRange -> {0, 4.6}, 
   PlotStyle -> {{Gray, Thick, Dashed}, {Gray, Thick, Dashed}, Black, 
     Black, None, None, None, None, None, None, None, None, None, 
     None}, AxesLabel -> {Style[M, Medium], Style[m, Medium]}, 
   Filling -> {5 -> {{6}, HatchFilling[]}, 7 -> {{5}, HatchFilling[]},
      8 -> {{9}, Opacity[.5, Yellow]}, 10 -> {{9}, Opacity[.3, Cyan]},
      11 -> {{12}, Opacity[.4, Orange]}}, 
   Ticks -> {{{2, Style[Superscript[M, c], Medium]}, {2.88`, 
       Style[Superscript[M, b], Medium]}, {1.6197440131489613`, 
       Style[Superscript[M, q], Medium]}}, {{1, 
       Style[Superscript[m, c], Medium]}, {1.2, 
       Style[Superscript[m, b], Medium]}, {1.311176089366584`, 
       Style[Superscript[m, q], Medium]}, {0.4982359230052799`, 
       Style[Subsuperscript[m, 2, b], Medium]}, {4.026420188608882`, 
       Style[Subsuperscript[m, 1, b], Medium]}}}, 
   PlotLegends -> 
    Placed[SwatchLegend[{Opacity[.5, Yellow], Opacity[.3, Cyan], 
       Opacity[.4, Orange]}, {Style[
        Superscript[\[CapitalDelta]CS, q] < 0, Medium], 
       Style[Subsuperscript[\[CapitalDelta]CS, 1, b] < 0, Medium], 
       Style[Subsuperscript[\[CapitalDelta]CS, 2, b] > 0, 
        Medium]}], {.7, 0.7}], ImageSize -> 600], 
  Graphics[{Text[Style[Subscript[D, 2], Medium], {.12, 4.2}], 
    Text[Style[Subscript[D, 1], Medium], {.5, 4.2}], 
    Text[Style[D, Bold, Medium], {.81, 4.2}], 
    Text[Style[MC, Bold, Medium, Italic], {3.35, 1.4}], {Dashed, 
     Line[{{{2, 0}, {2, 1}}, {{0, 1}, {2, 1}}, {{0, 1.2}, {2*1.2^2, 
         1.2}}, {{2.88, 0}, {2.88, 1.2}}, {{1.6197440131489613`, 
         0}, {1.6197440131489613`, 1.311176089366584`}}, {{0, 
         1.311176089366584`}, {1.6197440131489613`, 
         1.311176089366584`}}, {{1.6197440131489613`, 
         0.8999288897321169`}, {1.6197440131489613`, 
         1.311176089366584`}}, {{0, 
         4.026420188608882`}, {0.5728387152134832`, 
         4.026420188608882`}}, {{0, 
         0.4982359230052799`}, {2.3071612847865164`, 
         0.4982359230052799`}}}]}}]]

If[!DirectoryQ["../MANUSCRIPT FIGURES/Figure G.3"],
   CreateDirectory[FileNameJoin[{myDir, 
   "../MANUSCRIPT FIGURES/Figure G.3"}], 
 CreateIntermediateDirectories -> True]
]

Export[FileNameJoin[{myDir, "..", "MANUSCRIPT FIGURES", 
   "Figure G.3", "figureg3.png"}], figureg3, "PNG"]

QuotaDominates[tau_, sigma_, epsilon1_, epsilon2_, eta_] := 
 sigma*((tau1 /. FindTaus[tau, sigma, epsilon1, epsilon2, eta])^(1 - 
         epsilon1) - (tauq /. 
         FindTaus[tau, sigma, epsilon1, epsilon2, eta])^(1 - 
         epsilon1))/(1 - epsilon1) + (1 - 
     sigma)*((tau2 /. 
         FindTaus[tau, sigma, epsilon1, epsilon2, eta])^(1 - 
         epsilon2) - (tauq /. 
         FindTaus[tau, sigma, epsilon1, epsilon2, eta])^(1 - 
         epsilon2))/(1 - epsilon2)

Quiet[
figureg4 = 
 Show[RegionPlot3D[{QuotaDominates[1.003, .2, epsilon1, epsilon2, 
       eta] < 0 && epsilon1 < epsilon2}, {epsilon1, 
    0.01, .99}, {epsilon2, 0.01, 5}, {eta, 0.05, 4}, 
   PlotStyle -> Opacity[.4, Orange], 
   AxesLabel -> {Style[Subscript[\[Epsilon], "1"], Medium], 
     Style[Subscript[\[Epsilon], "2"], Medium], 
     Style[\[Eta], Medium]}], 
  Graphics3D[{{Red, Thick, Dashed, 
     Line[{{0, 0, 0}, {1, 1, 0}}]}, {PointSize[.03], Blue, 
     Point[{0.1, .3, 3}]}, {PointSize[.03], Green, 
     Point[{0.05, 1.3, .8}]}}], ViewPoint -> {-2.1, -2.4, .85}]
]

If[!DirectoryQ["../MANUSCRIPT FIGURES/Figure G.4"],
   CreateDirectory[FileNameJoin[{myDir, 
   "../MANUSCRIPT FIGURES/Figure G.4"}], 
 CreateIntermediateDirectories -> True]
]

Export[FileNameJoin[{myDir, "..", "MANUSCRIPT FIGURES", 
   "Figure G.4", "figureg4.png"}], figureg4, "PNG"]

ExportFunction[eta_] := 
 Module[{r, file}, r = QuotaDominates[1.003, .2, .1, 1.65, eta];
  file = FileNameJoin[{myDir, "..", "MANUSCRIPT CLAIMS",
      "Appendix G", "result_eta" <> ToString[eta] <> ".csv"}];
  Export[file, {{r}}, "CSV"]]

ExportFunction[3]

ExportFunction[0.5]

ExportFunction[1]

LandRent[tau_, eta_] := (tau^(1 + eta) - 1)/(1 + eta)

ConsSurpluswedge[tau_, sigma_, epsilon1_, epsilon2_, 
  eta_] := (-sigma*((tau1 /. 
          FindTaus[tau, sigma, epsilon1, epsilon2, eta])^(1 - 
          epsilon1) - 1)/(1 - epsilon1) - (1 - 
      sigma)*((tau2 /. 
          FindTaus[tau, sigma, epsilon1, epsilon2, eta])^(1 - 
          epsilon2) - 1)/(1 - epsilon2))

FindTauq[tau_, sigma_, epsilon1_, epsilon2_, eta_] := 
 FindRoot[{sigma*tauq^(1 - epsilon1) + (1 - sigma)*
      tauq^(1 - epsilon2) - 
     eta/(1 + 
         eta)*(sigma*tauq^(-epsilon1) + (1 - sigma)*
          tauq^(-epsilon2))^(1 + 1/eta) == 
    tau^(1 + eta)/(1 + eta)}, {tauq, 1}]

ConsSurplusquota[tau_, sigma_, epsilon1_, epsilon2_, 
  eta_] := (-sigma*((tauq /. 
          FindTauq[tau, sigma, epsilon1, epsilon2, eta])^(1 - 
          epsilon1) - 1)/(1 - epsilon1) - (1 - 
      sigma)*((tauq /. 
          FindTauq[tau, sigma, epsilon1, epsilon2, eta])^(1 - 
          epsilon2) - 1)/(1 - epsilon2))

figureg5a = 
 ParametricPlot[{{LandRent[tau, 3], 
    ConsSurpluswedge[tau, .2, .1, .3, 3]}, {LandRent[tau, 3], 
    ConsSurplusquota[tau, .2, .1, .3, 3]}, {1.1*(tau - 1), 
    1.1*(1 - tau)}}, {tau, 1, 1.5}, PlotRange -> {{0, 0.2}, {-.2, 0}},
   AxesLabel -> {Style[\!\(\*
TagBox[
FractionBox["\[CapitalDelta]PS", 
RowBox[{
TemplateBox[{"m", "c"},
"Superscript"], " ", 
TemplateBox[{"M", "c"},
"Superscript"]}]],
DisplayForm]\), Medium], Style[\!\(\*
TagBox[
FractionBox["\[CapitalDelta]CS", 
RowBox[{
TemplateBox[{"m", "c"},
"Superscript"], " ", 
TemplateBox[{"M", "c"},
"Superscript"]}]],
DisplayForm]\), Medium]}, 
  PlotLegends -> 
   Placed[{"Price discrimination", "Quota", 
     "Efficient transfer"}, {.7, .8}], 
  PlotStyle -> {{Thick, Black}, {Thick, Black, Dashed}, {Gray, 
     Dotted}}]

If[!DirectoryQ["../MANUSCRIPT FIGURES/Figure G.5"],
  CreateDirectory[FileNameJoin[{myDir, 
   "../MANUSCRIPT FIGURES/Figure G.5"}], 
 CreateIntermediateDirectories -> True]
]

Export[FileNameJoin[{myDir, "..", "MANUSCRIPT FIGURES", 
   "Figure G.5", "figureg5a.png"}], figureg5a, "PNG"]

figureg5b = 
 ParametricPlot[{{LandRent[tau, 0.5], 
    ConsSurpluswedge[tau, .2, .1, .3, 0.5]}, {LandRent[tau, .5], 
    ConsSurplusquota[tau, .2, .1, .3, .5]}, {1.3*(tau - 1), 
    1.3*(1 - tau)}}, {tau, 1, 1.5}, AxesLabel -> {Style[\!\(\*
TagBox[
FractionBox["\[CapitalDelta]PS", 
RowBox[{
TemplateBox[{"m", "c"},
"Superscript"], " ", 
TemplateBox[{"M", "c"},
"Superscript"]}]],
DisplayForm]\), Medium], Style[\!\(\*
TagBox[
FractionBox["\[CapitalDelta]CS", 
RowBox[{
TemplateBox[{"m", "c"},
"Superscript"], " ", 
TemplateBox[{"M", "c"},
"Superscript"]}]],
DisplayForm]\), Medium]}, 
  PlotStyle -> {{Thick, Black}, {Thick, Black, Dashed}, {Gray, 
     Dotted}}, PlotRange -> {{0, 0.2}, {-.2, 0}}]

Export[FileNameJoin[{myDir, "..", "MANUSCRIPT FIGURES", 
   "Figure G.5", "figureg5b.png"}], figureg5b, "PNG"]

figureg5c = 
 ParametricPlot[{{LandRent[tau, 3], 
    ConsSurpluswedge[tau, .2, .1, 2, 3]}, {LandRent[tau, 3], 
    ConsSurplusquota[tau, .2, .1, 2, 3]}, {1.5*(tau - 1), 
    1.5*(1 - tau)}}, {tau, 1, 1.3}, 
  PlotRange -> {{0, 0.3}, {-0.5, 0}}, AxesLabel -> {Style[\!\(\*
TagBox[
FractionBox["\[CapitalDelta]PS", 
RowBox[{
TemplateBox[{"m", "c"},
"Superscript"], " ", 
TemplateBox[{"M", "c"},
"Superscript"]}]],
DisplayForm]\), Medium], Style[\!\(\*
TagBox[
FractionBox["\[CapitalDelta]CS", 
RowBox[{
TemplateBox[{"m", "c"},
"Superscript"], " ", 
TemplateBox[{"M", "c"},
"Superscript"]}]],
DisplayForm]\), Medium]}, 
  PlotStyle -> {{Thick, Black}, {Thick, Black, Dashed}, {Gray, 
     Dotted}}]

Export[FileNameJoin[{myDir, "..", "MANUSCRIPT FIGURES", 
   "Figure G.5", "figureg5c.png"}], figureg5c, "PNG"]

figureg5d = 
 ParametricPlot[{{LandRent[tau, .5], 
    ConsSurpluswedge[tau, .2, .1, 2, .5]}, {LandRent[tau, .5], 
    ConsSurplusquota[tau, .2, .1, 2, .5]}, {1.1*(tau - 1), 
    1.1*(1 - tau)}}, {tau, 1, 1.1}, AxesLabel -> {Style[\!\(\*
TagBox[
FractionBox["\[CapitalDelta]PS", 
RowBox[{
TemplateBox[{"m", "c"},
"Superscript"], " ", 
TemplateBox[{"M", "c"},
"Superscript"]}]],
DisplayForm]\), Medium], Style[\!\(\*
TagBox[
FractionBox["\[CapitalDelta]CS", 
RowBox[{
TemplateBox[{"m", "c"},
"Superscript"], " ", 
TemplateBox[{"M", "c"},
"Superscript"]}]],
DisplayForm]\), Medium]}, PlotRange -> {{0, 0.1}, {-.2, 0}}, 
  PlotStyle -> {{Thick, Black}, {Thick, Black, Dashed}, {Gray, 
     Dotted}}]

Export[FileNameJoin[{myDir, "..", "MANUSCRIPT FIGURES", 
   "Figure G.5", "figureg5d.png"}], figureg5d, "PNG"]