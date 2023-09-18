(* ::Package:: *)

(* ::Title:: *)
(*Discrete Coils*)


(* ::Section::Closed:: *)
(*Package Header*)


Unprotect["NoahH`CreateCoil`*", "NoahH`CreateCoil`Private`*"];
ClearAll["NoahH`CreateCoil`*", "NoahH`CreateCoil`Private`*"];


BeginPackage["NoahH`CreateCoil`"];


FindLoopCoil::usage =
"\!\(\*RowBox[{\"FindLoopCoil\", \"[\", RowBox[{RowBox[{\"{\", RowBox[{SubscriptBox[StyleBox[\"i\", \"TI\"], StyleBox[RowBox[{\"\[Chi]\", \" \", \"1\"}], \"TR\"]], \",\", SubscriptBox[StyleBox[\"i\", \"TI\"], StyleBox[RowBox[{\"\[Chi]\", \" \", \"2\"}], \"TR\"]], \",\", StyleBox[\"\\\"\[Ellipsis]\\\"\", \"TR\"]}], \"}\"}], \",\", StyleBox[\"n\", \"TI\"], \",\", RowBox[{\"{\", RowBox[{SubscriptBox[StyleBox[\"\[Chi]\", \"TI\"], StyleBox[\"\\\"c min\\\"\", \"TI\"]], \",\", SubscriptBox[StyleBox[\"\[Chi]\", \"TI\"], StyleBox[\"\\\"c max\\\"\", \"TI\"]]}], \"}\"}]}], \"]\"}]\) returns, for loop pairs of turn ratios \!\(\*SubscriptBox[StyleBox[\"i\", \"TI\"], StyleBox[RowBox[{\"\[Chi]\", \" \", \"1\"}], \"TR\"]]\), \!\(\*SubscriptBox[StyleBox[\"i\", \"TI\"], StyleBox[RowBox[{\"\[Chi]\", \" \", \"2\"}], \"TR\"]]\), \!\(\*StyleBox[\"\\\"\[Ellipsis]\\\"\", \"TR\"]\), axial separations between \!\(\*SubscriptBox[StyleBox[\"\[Chi]\", \"TI\"], StyleBox[\"\\\"c min\\\"\", \"TI\"]]\) and \!\(\*SubscriptBox[StyleBox[\"\[Chi]\", \"TI\"], StyleBox[\"\\\"c max\\\"\", \"TI\"]]\) optimised to generate the field harmonic of order \!\(\*StyleBox[\"n\", \"TI\"]\).";

MathLink`CallFrontEnd[FrontEnd`CacheTemplateAndUsagePacket[FrontEnd`InputNotebook[],"FindLoopCoil",{{Cell[BoxData[GridBox[{{RowBox[{"FindLoopCoil","[",RowBox[{RowBox[{"{",RowBox[{SubscriptBox[StyleBox["i","TI"],StyleBox[RowBox[{"\[Chi]"," ","1"}],"TR"]],",",SubscriptBox[StyleBox["i","TI"],StyleBox[RowBox[{"\[Chi]"," ","2"}],"TR"]],",",StyleBox["\"\[Ellipsis]\"","TR"]}],"}"}],",",StyleBox["n","TI"],",",RowBox[{"{",RowBox[{SubscriptBox[StyleBox["\[Chi]","TI"],StyleBox["\"c min\"","TI"]],",",SubscriptBox[StyleBox["\[Chi]","TI"],StyleBox["\"c max\"","TI"]]}],"}"}]}],"]"}]}}]],"InlineFormula",CellTags->"FindLoopCoil_templates"]},{Cell[BoxData[RowBox[{" returns, for loop pairs of turn ratios \!\(\*SubscriptBox[StyleBox[\"i\", \"TI\"], StyleBox[RowBox[{\"\[Chi]\", \" \", \"1\"}], \"TR\"]]\), \!\(\*SubscriptBox[StyleBox[\"i\", \"TI\"], StyleBox[RowBox[{\"\[Chi]\", \" \", \"2\"}], \"TR\"]]\), \!\(\*StyleBox[\"\\\"\[Ellipsis]\\\"\", \"TR\"]\), axial separations between \!\(\*SubscriptBox[StyleBox[\"\[Chi]\", \"TI\"], StyleBox[\"\\\"c min\\\"\", \"TI\"]]\) and \!\(\*SubscriptBox[StyleBox[\"\[Chi]\", \"TI\"], StyleBox[\"\\\"c max\\\"\", \"TI\"]]\) optimised to generate the field harmonic of order \!\(\*StyleBox[\"n\", \"TI\"]\)."}]],"Output",CellTags->"FindLoopCoil_usages"]}}]]


FindSaddleCoil::usage =
"\!\(\*RowBox[{\"FindSaddleCoil\", \"[\", RowBox[{RowBox[{\"{\", RowBox[{SubscriptBox[StyleBox[\"i\", \"TI\"], StyleBox[RowBox[{\"\[Chi]\", \" \", \"1\"}], \"TR\"]], \",\", SubscriptBox[StyleBox[\"i\", \"TI\"], StyleBox[RowBox[{\"\[Chi]\", \" \", \"2\"}], \"TR\"]], \",\", StyleBox[\"\\\"\[Ellipsis]\\\"\", \"TR\"]}], \"}\"}], \",\", RowBox[{\"{\", RowBox[{StyleBox[\"n\", \"TI\"], \",\", StyleBox[\"m\", \"TI\"]}], \"}\"}], \",\", SubscriptBox[StyleBox[\"k\", \"TI\"], StyleBox[\"\[Phi]\", \"TI\"]], \",\", RowBox[{\"{\", RowBox[{SubscriptBox[StyleBox[\"\[Chi]\", \"TI\"], StyleBox[\"\\\"c min\\\"\", \"TI\"]], \",\", SubscriptBox[StyleBox[\"\[Chi]\", \"TI\"], StyleBox[\"\\\"c max\\\"\", \"TI\"]]}], \"}\"}]}], \"]\"}]\) returns, for arc groups of turn ratios \!\(\*SubscriptBox[StyleBox[\"i\", \"TI\"], StyleBox[RowBox[{\"\[Chi]\", \" \", \"1\"}], \"TR\"]]\), \!\(\*SubscriptBox[StyleBox[\"i\", \"TI\"], StyleBox[RowBox[{\"\[Chi]\", \" \", \"2\"}], \"TR\"]]\), \!\(\*StyleBox[\"\\\"\[Ellipsis]\\\"\", \"TR\"]\), axial separations between \!\(\*SubscriptBox[StyleBox[\"\[Chi]\", \"TI\"], StyleBox[\"\\\"c min\\\"\", \"TI\"]]\) and \!\(\*SubscriptBox[StyleBox[\"\[Chi]\", \"TI\"], StyleBox[\"\\\"c max\\\"\", \"TI\"]]\) and azimuthal extents optimised to generate the field harmonic of order \!\(\*StyleBox[\"n\", \"TI\"]\) and degree \!\(\*StyleBox[\"m\", \"TI\"]\), while nulling the first \!\(\*SubscriptBox[StyleBox[\"k\", \"TI\"], StyleBox[\"\[Phi]\", \"TI\"]]\) leading\[Hyphen]order error harmonic degrees.";

MathLink`CallFrontEnd[FrontEnd`CacheTemplateAndUsagePacket[FrontEnd`InputNotebook[],"FindSaddleCoil",{{Cell[BoxData[GridBox[{{RowBox[{"FindSaddleCoil","[",RowBox[{RowBox[{"{",RowBox[{SubscriptBox[StyleBox["i","TI"],StyleBox[RowBox[{"\[Chi]"," ","1"}],"TR"]],",",SubscriptBox[StyleBox["i","TI"],StyleBox[RowBox[{"\[Chi]"," ","2"}],"TR"]],",",StyleBox["\"\[Ellipsis]\"","TR"]}],"}"}],",",RowBox[{"{",RowBox[{StyleBox["n","TI"],",",StyleBox["m","TI"]}],"}"}],",",SubscriptBox[StyleBox["k","TI"],StyleBox["\[Phi]","TI"]],",",RowBox[{"{",RowBox[{SubscriptBox[StyleBox["\[Chi]","TI"],StyleBox["\"c min\"","TI"]],",",SubscriptBox[StyleBox["\[Chi]","TI"],StyleBox["\"c max\"","TI"]]}],"}"}]}],"]"}]}}]],"InlineFormula",CellTags->"FindSaddleCoil_templates"]},{Cell[BoxData[RowBox[{" returns, for arc groups of turn ratios \!\(\*SubscriptBox[StyleBox[\"i\", \"TI\"], StyleBox[RowBox[{\"\[Chi]\", \" \", \"1\"}], \"TR\"]]\), \!\(\*SubscriptBox[StyleBox[\"i\", \"TI\"], StyleBox[RowBox[{\"\[Chi]\", \" \", \"2\"}], \"TR\"]]\), \!\(\*StyleBox[\"\\\"\[Ellipsis]\\\"\", \"TR\"]\), axial separations between \!\(\*SubscriptBox[StyleBox[\"\[Chi]\", \"TI\"], StyleBox[\"\\\"c min\\\"\", \"TI\"]]\) and \!\(\*SubscriptBox[StyleBox[\"\[Chi]\", \"TI\"], StyleBox[\"\\\"c max\\\"\", \"TI\"]]\) and azimuthal extents optimised to generate the field harmonic of order \!\(\*StyleBox[\"n\", \"TI\"]\) and degree \!\(\*StyleBox[\"m\", \"TI\"]\), while nulling the first \!\(\*SubscriptBox[StyleBox[\"k\", \"TI\"], StyleBox[\"\[Phi]\", \"TI\"]]\) leading-order error harmonic degrees."}]],"Output",CellTags->"FindSaddleCoil_usages"]}}]]


FindSaddleCoilAxial::usage =
"\!\(\*RowBox[{\"FindSaddleCoilAxial\", \"[\", RowBox[{RowBox[{\"{\", RowBox[{SubscriptBox[StyleBox[\"i\", \"TI\"], StyleBox[RowBox[{\"\[Chi]\", \" \", \"1\"}], \"TR\"]], \",\", SubscriptBox[StyleBox[\"i\", \"TI\"], StyleBox[RowBox[{\"\[Chi]\", \" \", \"2\"}], \"TR\"]], \",\", StyleBox[\"\\\"\[Ellipsis]\\\"\", \"TR\"]}], \"}\"}], \",\", RowBox[{\"{\", RowBox[{StyleBox[\"n\", \"TI\"], \",\", StyleBox[\"m\", \"TI\"]}], \"}\"}], \",\", SubscriptBox[StyleBox[\"k\", \"TI\"], StyleBox[\"\[Phi]\", \"TI\"]], \",\", RowBox[{\"{\", RowBox[{SubscriptBox[StyleBox[\"\[Chi]\", \"TI\"], StyleBox[\"\\\"c min\\\"\", \"TI\"]], \",\", SubscriptBox[StyleBox[\"\[Chi]\", \"TI\"], StyleBox[\"\\\"c max\\\"\", \"TI\"]]}], \"}\"}]}], \"]\"}]\) returns, for arc groups of turn ratios \!\(\*SubscriptBox[StyleBox[\"i\", \"TI\"], StyleBox[RowBox[{\"\[Chi]\", \" \", \"1\"}], \"TR\"]]\), \!\(\*SubscriptBox[StyleBox[\"i\", \"TI\"], StyleBox[RowBox[{\"\[Chi]\", \" \", \"2\"}], \"TR\"]]\), \!\(\*StyleBox[\"\\\"\[Ellipsis]\\\"\", \"TR\"]\) and with \!\(\*SubscriptBox[StyleBox[\"k\", \"TI\"], StyleBox[\"\[Phi]\", \"TI\"]]\) azimuthal extents per saddle, axial separations between \!\(\*SubscriptBox[StyleBox[\"\[Chi]\", \"TI\"], StyleBox[\"\\\"c min\\\"\", \"TI\"]]\) and \!\(\*SubscriptBox[StyleBox[\"\[Chi]\", \"TI\"], StyleBox[\"\\\"c max\\\"\", \"TI\"]]\) optimised to generate the field harmonic of order \!\(\*StyleBox[\"n\", \"TI\"]\) and degree \!\(\*StyleBox[\"m\", \"TI\"]\).";

MathLink`CallFrontEnd[FrontEnd`CacheTemplateAndUsagePacket[FrontEnd`InputNotebook[],"FindSaddleCoilAxial",{{Cell[BoxData[GridBox[{{RowBox[{"FindSaddleCoilAxial","[",RowBox[{RowBox[{"{",RowBox[{SubscriptBox[StyleBox["i","TI"],StyleBox[RowBox[{"\[Chi]"," ","1"}],"TR"]],",",SubscriptBox[StyleBox["i","TI"],StyleBox[RowBox[{"\[Chi]"," ","2"}],"TR"]],",",StyleBox["\"\[Ellipsis]\"","TR"]}],"}"}],",",RowBox[{"{",RowBox[{StyleBox["n","TI"],",",StyleBox["m","TI"]}],"}"}],",",SubscriptBox[StyleBox["k","TI"],StyleBox["\[Phi]","TI"]],",",RowBox[{"{",RowBox[{SubscriptBox[StyleBox["\[Chi]","TI"],StyleBox["\"c min\"","TI"]],",",SubscriptBox[StyleBox["\[Chi]","TI"],StyleBox["\"c max\"","TI"]]}],"}"}]}],"]"}]}}]],"InlineFormula",CellTags->"FindSaddleCoilAxial_templates"]},{Cell[BoxData[RowBox[{" returns, for arc groups of turn ratios \!\(\*SubscriptBox[StyleBox[\"i\", \"TI\"], StyleBox[RowBox[{\"\[Chi]\", \" \", \"1\"}], \"TR\"]]\), \!\(\*SubscriptBox[StyleBox[\"i\", \"TI\"], StyleBox[RowBox[{\"\[Chi]\", \" \", \"2\"}], \"TR\"]]\), \!\(\*StyleBox[\"\\\"\[Ellipsis]\\\"\", \"TR\"]\) and with \!\(\*SubscriptBox[StyleBox[\"k\", \"TI\"], StyleBox[\"\[Phi]\", \"TI\"]]\) azimuthal extents per saddle, axial separations between \!\(\*SubscriptBox[StyleBox[\"\[Chi]\", \"TI\"], StyleBox[\"\\\"c min\\\"\", \"TI\"]]\) and \!\(\*SubscriptBox[StyleBox[\"\[Chi]\", \"TI\"], StyleBox[\"\\\"c max\\\"\", \"TI\"]]\) optimised to generate the field harmonic of order \!\(\*StyleBox[\"n\", \"TI\"]\) and degree \!\(\*StyleBox[\"m\", \"TI\"]\)."}]],"Output",CellTags->"FindSaddleCoilAxial_usages"]}}]]


FindSaddleCoilAzimuthal::usage =
"\!\(\*RowBox[{\"FindSaddleCoilAzimuthal\", \"[\", RowBox[{StyleBox[\"m\", \"TI\"], \",\", SubscriptBox[StyleBox[\"k\", \"TI\"], StyleBox[\"\[Phi]\", \"TI\"]]}], \"]\"}]\) returns azimuthal arc extents optimised to generate the field harmonic of degree \!\(\*StyleBox[\"m\", \"TI\"]\), while nulling the first \!\(\*SubscriptBox[StyleBox[\"k\", \"TI\"], StyleBox[\"\[Phi]\", \"TI\"]]\) leading\[Hyphen]order error harmonic degrees.";

MathLink`CallFrontEnd[FrontEnd`CacheTemplateAndUsagePacket[FrontEnd`InputNotebook[],"FindSaddleCoilAzimuthal",{{Cell[BoxData[GridBox[{{RowBox[{"FindSaddleCoilAzimuthal","[",RowBox[{StyleBox["m","TI"],",",SubscriptBox[StyleBox["k","TI"],StyleBox["\[Phi]","TI"]]}],"]"}]}}]],"InlineFormula",CellTags->"FindSaddleCoilAzimuthal_templates"]},{Cell[BoxData[RowBox[{" returns azimuthal arc extents optimised to generate the field harmonic of degree \!\(\*StyleBox[\"m\", \"TI\"]\), while nulling the first \!\(\*SubscriptBox[StyleBox[\"k\", \"TI\"], StyleBox[\"\[Phi]\", \"TI\"]]\) leading-order error harmonic degrees."}]],"Output",CellTags->"FindSaddleCoilAzimuthal_usages"]}}]]


FindEllipseCoil::usage =
"\!\(\*RowBox[{\"FindEllipseCoil\", \"[\", RowBox[{RowBox[{\"{\", RowBox[{SubscriptBox[StyleBox[\"i\", \"TI\"], StyleBox[RowBox[{\"\[Chi]\", \" \", \"1\"}], \"TR\"]], \",\", SubscriptBox[StyleBox[\"i\", \"TI\"], StyleBox[RowBox[{\"\[Chi]\", \" \", \"2\"}], \"TR\"]], \",\", StyleBox[\"\\\"\[Ellipsis]\\\"\", \"TR\"]}], \"}\"}], \",\", RowBox[{\"{\", RowBox[{StyleBox[\"n\", \"TI\"], \",\", StyleBox[\"m\", \"TI\"]}], \"}\"}], \",\", RowBox[{\"{\", RowBox[{SubscriptBox[StyleBox[\"\[Chi]\", \"TI\"], StyleBox[\"\\\"c min\\\"\", \"TI\"]], \",\", SubscriptBox[StyleBox[\"\[Chi]\", \"TI\"], StyleBox[\"\\\"c max\\\"\", \"TI\"]]}], \"}\"}], \",\", RowBox[{\"{\", RowBox[{SubscriptBox[StyleBox[\"\[Psi]\", \"TI\"], StyleBox[\"\\\"c min\\\"\", \"TI\"]], \",\", SubscriptBox[StyleBox[\"\[Psi]\", \"TI\"], StyleBox[\"\\\"c max\\\"\", \"TI\"]]}], \"}\"}]}], \"]\"}]\) returns, for ellipse groups of turn ratios \!\(\*SubscriptBox[StyleBox[\"i\", \"TI\"], StyleBox[RowBox[{\"\[Chi]\", \" \", \"1\"}], \"TR\"]]\), \!\(\*SubscriptBox[StyleBox[\"i\", \"TI\"], StyleBox[RowBox[{\"\[Chi]\", \" \", \"2\"}], \"TR\"]]\), \!\(\*StyleBox[\"\\\"\[Ellipsis]\\\"\", \"TR\"]\), axial separations between \!\(\*SubscriptBox[StyleBox[\"\[Chi]\", \"TI\"], StyleBox[\"\\\"c min\\\"\", \"TI\"]]\) and \!\(\*SubscriptBox[StyleBox[\"\[Chi]\", \"TI\"], StyleBox[\"\\\"c max\\\"\", \"TI\"]]\) and ellipse extents between \!\(\*SubscriptBox[StyleBox[\"\[Psi]\", \"TI\"], StyleBox[\"\\\"c min\\\"\", \"TI\"]]\) and \!\(\*SubscriptBox[StyleBox[\"\[Psi]\", \"TI\"], StyleBox[\"\\\"c max\\\"\", \"TI\"]]\) optimised to generate the field harmonic of order \!\(\*StyleBox[\"n\", \"TI\"]\) and degree \!\(\*StyleBox[\"m\", \"TI\"]\).";

MathLink`CallFrontEnd[FrontEnd`CacheTemplateAndUsagePacket[FrontEnd`InputNotebook[],"FindEllipseCoil",{{Cell[BoxData[GridBox[{{RowBox[{"FindEllipseCoil","[",RowBox[{RowBox[{"{",RowBox[{SubscriptBox[StyleBox["i","TI"],StyleBox[RowBox[{"\[Chi]"," ","1"}],"TR"]],",",SubscriptBox[StyleBox["i","TI"],StyleBox[RowBox[{"\[Chi]"," ","2"}],"TR"]],",",StyleBox["\"\[Ellipsis]\"","TR"]}],"}"}],",",RowBox[{"{",RowBox[{StyleBox["n","TI"],",",StyleBox["m","TI"]}],"}"}],",",RowBox[{"{",RowBox[{SubscriptBox[StyleBox["\[Chi]","TI"],StyleBox["\"c min\"","TI"]],",",SubscriptBox[StyleBox["\[Chi]","TI"],StyleBox["\"c max\"","TI"]]}],"}"}],",",RowBox[{"{",RowBox[{SubscriptBox[StyleBox["\[Psi]","TI"],StyleBox["\"c min\"","TI"]],",",SubscriptBox[StyleBox["\[Psi]","TI"],StyleBox["\"c max\"","TI"]]}],"}"}]}],"]"}]}}]],"InlineFormula",CellTags->"FindEllipseCoil_templates"]},{Cell[BoxData[RowBox[{" returns, for ellipse groups of turn ratios \!\(\*SubscriptBox[StyleBox[\"i\", \"TI\"], StyleBox[RowBox[{\"\[Chi]\", \" \", \"1\"}], \"TR\"]]\), \!\(\*SubscriptBox[StyleBox[\"i\", \"TI\"], StyleBox[RowBox[{\"\[Chi]\", \" \", \"2\"}], \"TR\"]]\), \!\(\*StyleBox[\"\\\"\[Ellipsis]\\\"\", \"TR\"]\), axial separations between \!\(\*SubscriptBox[StyleBox[\"\[Chi]\", \"TI\"], StyleBox[\"\\\"c min\\\"\", \"TI\"]]\) and \!\(\*SubscriptBox[StyleBox[\"\[Chi]\", \"TI\"], StyleBox[\"\\\"c max\\\"\", \"TI\"]]\) and ellipse extents between \!\(\*SubscriptBox[StyleBox[\"\[Psi]\", \"TI\"], StyleBox[\"\\\"c min\\\"\", \"TI\"]]\) and \!\(\*SubscriptBox[StyleBox[\"\[Psi]\", \"TI\"], StyleBox[\"\\\"c max\\\"\", \"TI\"]]\) optimised to generate the field harmonic of order \!\(\*StyleBox[\"n\", \"TI\"]\) and degree \!\(\*StyleBox[\"m\", \"TI\"]\)."}]],"Output",CellTags->"FindEllipseCoil_usages"]}}]]


Coil\[Chi]c::usage = "Normalised separation of a loop/arc/ellipse pair";


Coil\[Phi]c::usage = "Azimuthal extent of a saddle";


Coil\[Psi]c::usage = "Normalised ellipse extent";


DesToErr::usage = "Ratio of the desired to leading-order error total field harmonic magnitudes";


LoopCoilPlot::usage =
"\!\(\*RowBox[{\"LoopCoilPlot\", \"[\", RowBox[{RowBox[{\"{\", RowBox[{SubscriptBox[StyleBox[\"\[Chi]\", \"TI\"], StyleBox[RowBox[{\"c\", \" \", \"1\"}], \"TR\"]], \",\", SubscriptBox[StyleBox[\"\[Chi]\", \"TI\"], StyleBox[RowBox[{\"c\", \" \", \"2\"}], \"TR\"]], \",\", StyleBox[\"\\\"\[Ellipsis]\\\"\", \"TR\"]}], \"}\"}], \",\", RowBox[{\"{\", RowBox[{SubscriptBox[StyleBox[\"i\", \"TI\"], StyleBox[RowBox[{\"\[Chi]\", \" \", \"1\"}], \"TR\"]], \",\", SubscriptBox[StyleBox[\"i\", \"TI\"], StyleBox[RowBox[{\"\[Chi]\", \" \", \"2\"}], \"TR\"]], \",\", StyleBox[\"\\\"\[Ellipsis]\\\"\", \"TR\"]}], \"}\"}], \",\", SubscriptBox[StyleBox[\"\[Rho]\", \"TI\"], StyleBox[\"c\", \"TI\"]], \",\", StyleBox[\"n\", \"TI\"]}], \"]\"}]\) plots a schematic in the \!\(\*StyleBox[\"\[Phi]\", \"TI\"]\)\!\(\*StyleBox[\"z\", \"TI\"]\)\[Hyphen]plane of the loop\[Hyphen]based coil with axial separations \!\(\*SubscriptBox[StyleBox[\"\[Chi]\", \"TI\"], StyleBox[RowBox[{\"c\", \" \", \"1\"}], \"TR\"]]\), \!\(\*SubscriptBox[StyleBox[\"\[Chi]\", \"TI\"], StyleBox[RowBox[{\"c\", \" \", \"2\"}], \"TR\"]]\), \!\(\*StyleBox[\"\\\"\[Ellipsis]\\\"\", \"TR\"]\), turn ratios \!\(\*SubscriptBox[StyleBox[\"i\", \"TI\"], StyleBox[RowBox[{\"\[Chi]\", \" \", \"1\"}], \"TR\"]]\), \!\(\*SubscriptBox[StyleBox[\"i\", \"TI\"], StyleBox[RowBox[{\"\[Chi]\", \" \", \"2\"}], \"TR\"]]\), \!\(\*StyleBox[\"\\\"\[Ellipsis]\\\"\", \"TR\"]\), radius \!\(\*SubscriptBox[StyleBox[\"\[Rho]\", \"TI\"], StyleBox[\"c\", \"TI\"]]\) and target field harmonic of order \!\(\*StyleBox[\"n\", \"TI\"]\).
\!\(\*RowBox[{\"LoopCoilPlot\", \"[\", RowBox[{RowBox[{\"{\", RowBox[{RowBox[{RowBox[{\"Coil\[Chi]c\", \"[\", \"1\", \"]\"}], \"\[Rule]\", SubscriptBox[StyleBox[\"\[Chi]\", \"TI\"], StyleBox[RowBox[{\"c\", \" \", \"1\"}], \"TR\"]]}], \",\", RowBox[{RowBox[{\"Coil\[Chi]c\", \"[\", \"2\", \"]\"}], \"\[Rule]\", SubscriptBox[StyleBox[\"\[Chi]\", \"TI\"], StyleBox[RowBox[{\"c\", \" \", \"2\"}], \"TR\"]]}], \",\", StyleBox[\"\\\"\[Ellipsis]\\\"\", \"TR\"]}], \"}\"}], \",\", StyleBox[\"\\\"\[Ellipsis]\\\"\", \"TR\"]}], \"]\"}]\) is an alternative way of specifying the axial separations \!\(\*SubscriptBox[StyleBox[\"\[Chi]\", \"TI\"], StyleBox[RowBox[{\"c\", \" \", \"1\"}], \"TR\"]]\), \!\(\*SubscriptBox[StyleBox[\"\[Chi]\", \"TI\"], StyleBox[RowBox[{\"c\", \" \", \"2\"}], \"TR\"]]\), \!\(\*StyleBox[\"\\\"\[Ellipsis]\\\"\", \"TR\"]\).";

MathLink`CallFrontEnd[FrontEnd`CacheTemplateAndUsagePacket[FrontEnd`InputNotebook[],"LoopCoilPlot",{{Cell[BoxData[GridBox[{{RowBox[{"LoopCoilPlot","[",RowBox[{RowBox[{"{",RowBox[{SubscriptBox[StyleBox["\[Chi]","TI"],StyleBox[RowBox[{"c"," ","1"}],"TR"]],",",SubscriptBox[StyleBox["\[Chi]","TI"],StyleBox[RowBox[{"c"," ","2"}],"TR"]],",",StyleBox["\"\[Ellipsis]\"","TR"]}],"}"}],",",RowBox[{"{",RowBox[{SubscriptBox[StyleBox["i","TI"],StyleBox[RowBox[{"\[Chi]"," ","1"}],"TR"]],",",SubscriptBox[StyleBox["i","TI"],StyleBox[RowBox[{"\[Chi]"," ","2"}],"TR"]],",",StyleBox["\"\[Ellipsis]\"","TR"]}],"}"}],",",SubscriptBox[StyleBox["\[Rho]","TI"],StyleBox["c","TI"]],",",StyleBox["n","TI"]}],"]"}]}}]],"InlineFormula",CellTags->"LoopCoilPlot_templates"],Cell[BoxData[GridBox[{{RowBox[{"LoopCoilPlot","[",RowBox[{RowBox[{"{",RowBox[{RowBox[{RowBox[{"Coil\[Chi]c","[","1","]"}],"->",SubscriptBox[StyleBox["\[Chi]","TI"],StyleBox[RowBox[{"c"," ","1"}],"TR"]]}],",",RowBox[{RowBox[{"Coil\[Chi]c","[","2","]"}],"->",SubscriptBox[StyleBox["\[Chi]","TI"],StyleBox[RowBox[{"c"," ","2"}],"TR"]]}],",",StyleBox["\"\[Ellipsis]\"","TR"]}],"}"}],",",StyleBox["\"\[Ellipsis]\"","TR"]}],"]"}]}}]],"InlineFormula",CellTags->"LoopCoilPlot_templates"]},{Cell[BoxData[RowBox[{" plots a schematic in the \!\(\*StyleBox[\"\[Phi]\", \"TI\"]\)\!\(\*StyleBox[\"z\", \"TI\"]\)-plane of the loop-based coil with axial separations \!\(\*SubscriptBox[StyleBox[\"\[Chi]\", \"TI\"], StyleBox[RowBox[{\"c\", \" \", \"1\"}], \"TR\"]]\), \!\(\*SubscriptBox[StyleBox[\"\[Chi]\", \"TI\"], StyleBox[RowBox[{\"c\", \" \", \"2\"}], \"TR\"]]\), \!\(\*StyleBox[\"\\\"\[Ellipsis]\\\"\", \"TR\"]\), turn ratios \!\(\*SubscriptBox[StyleBox[\"i\", \"TI\"], StyleBox[RowBox[{\"\[Chi]\", \" \", \"1\"}], \"TR\"]]\), \!\(\*SubscriptBox[StyleBox[\"i\", \"TI\"], StyleBox[RowBox[{\"\[Chi]\", \" \", \"2\"}], \"TR\"]]\), \!\(\*StyleBox[\"\\\"\[Ellipsis]\\\"\", \"TR\"]\), radius \!\(\*SubscriptBox[StyleBox[\"\[Rho]\", \"TI\"], StyleBox[\"c\", \"TI\"]]\) and target field harmonic of order \!\(\*StyleBox[\"n\", \"TI\"]\)."}]],"Output",CellTags->"LoopCoilPlot_usages"],Cell[BoxData[RowBox[{" is an alternative way of specifying the axial separations \!\(\*SubscriptBox[StyleBox[\"\[Chi]\", \"TI\"], StyleBox[RowBox[{\"c\", \" \", \"1\"}], \"TR\"]]\), \!\(\*SubscriptBox[StyleBox[\"\[Chi]\", \"TI\"], StyleBox[RowBox[{\"c\", \" \", \"2\"}], \"TR\"]]\), \!\(\*StyleBox[\"\\\"\[Ellipsis]\\\"\", \"TR\"]\)."}]],"Output",CellTags->"LoopCoilPlot_usages"]}}]]


LoopCoilPlot3D::usage =
"\!\(\*RowBox[{\"LoopCoilPlot3D\", \"[\", RowBox[{RowBox[{\"{\", RowBox[{SubscriptBox[StyleBox[\"\[Chi]\", \"TI\"], StyleBox[RowBox[{\"c\", \" \", \"1\"}], \"TR\"]], \",\", SubscriptBox[StyleBox[\"\[Chi]\", \"TI\"], StyleBox[RowBox[{\"c\", \" \", \"2\"}], \"TR\"]], \",\", StyleBox[\"\\\"\[Ellipsis]\\\"\", \"TR\"]}], \"}\"}], \",\", RowBox[{\"{\", RowBox[{SubscriptBox[StyleBox[\"i\", \"TI\"], StyleBox[RowBox[{\"\[Chi]\", \" \", \"1\"}], \"TR\"]], \",\", SubscriptBox[StyleBox[\"i\", \"TI\"], StyleBox[RowBox[{\"\[Chi]\", \" \", \"2\"}], \"TR\"]], \",\", StyleBox[\"\\\"\[Ellipsis]\\\"\", \"TR\"]}], \"}\"}], \",\", SubscriptBox[StyleBox[\"\[Rho]\", \"TI\"], StyleBox[\"c\", \"TI\"]], \",\", StyleBox[\"n\", \"TI\"]}], \"]\"}]\) returns a 3D plot of the loop\[Hyphen]based coil with axial separations \!\(\*SubscriptBox[StyleBox[\"\[Chi]\", \"TI\"], StyleBox[RowBox[{\"c\", \" \", \"1\"}], \"TR\"]]\), \!\(\*SubscriptBox[StyleBox[\"\[Chi]\", \"TI\"], StyleBox[RowBox[{\"c\", \" \", \"2\"}], \"TR\"]]\), \!\(\*StyleBox[\"\\\"\[Ellipsis]\\\"\", \"TR\"]\), turn ratios \!\(\*SubscriptBox[StyleBox[\"i\", \"TI\"], StyleBox[RowBox[{\"\[Chi]\", \" \", \"1\"}], \"TR\"]]\), \!\(\*SubscriptBox[StyleBox[\"i\", \"TI\"], StyleBox[RowBox[{\"\[Chi]\", \" \", \"2\"}], \"TR\"]]\), \!\(\*StyleBox[\"\\\"\[Ellipsis]\\\"\", \"TR\"]\), radius \!\(\*SubscriptBox[StyleBox[\"\[Rho]\", \"TI\"], StyleBox[\"c\", \"TI\"]]\) and target field harmonic of order \!\(\*StyleBox[\"n\", \"TI\"]\).
\!\(\*RowBox[{\"LoopCoilPlot3D\", \"[\", RowBox[{RowBox[{\"{\", RowBox[{RowBox[{RowBox[{\"Coil\[Chi]c\", \"[\", \"1\", \"]\"}], \"\[Rule]\", SubscriptBox[StyleBox[\"\[Chi]\", \"TI\"], StyleBox[RowBox[{\"c\", \" \", \"1\"}], \"TR\"]]}], \",\", RowBox[{RowBox[{\"Coil\[Chi]c\", \"[\", \"2\", \"]\"}], \"\[Rule]\", SubscriptBox[StyleBox[\"\[Chi]\", \"TI\"], StyleBox[RowBox[{\"c\", \" \", \"2\"}], \"TR\"]]}], \",\", StyleBox[\"\\\"\[Ellipsis]\\\"\", \"TR\"]}], \"}\"}], \",\", StyleBox[\"\\\"\[Ellipsis]\\\"\", \"TR\"]}], \"]\"}]\) is an alternative way of specifying the axial separations \!\(\*SubscriptBox[StyleBox[\"\[Chi]\", \"TI\"], StyleBox[RowBox[{\"c\", \" \", \"1\"}], \"TR\"]]\), \!\(\*SubscriptBox[StyleBox[\"\[Chi]\", \"TI\"], StyleBox[RowBox[{\"c\", \" \", \"2\"}], \"TR\"]]\), \!\(\*StyleBox[\"\\\"\[Ellipsis]\\\"\", \"TR\"]\).";

MathLink`CallFrontEnd[FrontEnd`CacheTemplateAndUsagePacket[FrontEnd`InputNotebook[],"LoopCoilPlot3D",{{Cell[BoxData[GridBox[{{RowBox[{"LoopCoilPlot3D","[",RowBox[{RowBox[{"{",RowBox[{SubscriptBox[StyleBox["\[Chi]","TI"],StyleBox[RowBox[{"c"," ","1"}],"TR"]],",",SubscriptBox[StyleBox["\[Chi]","TI"],StyleBox[RowBox[{"c"," ","2"}],"TR"]],",",StyleBox["\"\[Ellipsis]\"","TR"]}],"}"}],",",RowBox[{"{",RowBox[{SubscriptBox[StyleBox["i","TI"],StyleBox[RowBox[{"\[Chi]"," ","1"}],"TR"]],",",SubscriptBox[StyleBox["i","TI"],StyleBox[RowBox[{"\[Chi]"," ","2"}],"TR"]],",",StyleBox["\"\[Ellipsis]\"","TR"]}],"}"}],",",SubscriptBox[StyleBox["\[Rho]","TI"],StyleBox["c","TI"]],",",StyleBox["n","TI"]}],"]"}]}}]],"InlineFormula",CellTags->"LoopCoilPlot3D_templates"],Cell[BoxData[GridBox[{{RowBox[{"LoopCoilPlot3D","[",RowBox[{RowBox[{"{",RowBox[{RowBox[{RowBox[{"Coil\[Chi]c","[","1","]"}],"->",SubscriptBox[StyleBox["\[Chi]","TI"],StyleBox[RowBox[{"c"," ","1"}],"TR"]]}],",",RowBox[{RowBox[{"Coil\[Chi]c","[","2","]"}],"->",SubscriptBox[StyleBox["\[Chi]","TI"],StyleBox[RowBox[{"c"," ","2"}],"TR"]]}],",",StyleBox["\"\[Ellipsis]\"","TR"]}],"}"}],",",StyleBox["\"\[Ellipsis]\"","TR"]}],"]"}]}}]],"InlineFormula",CellTags->"LoopCoilPlot3D_templates"]},{Cell[BoxData[RowBox[{" returns a 3D plot of the loop-based coil with axial separations \!\(\*SubscriptBox[StyleBox[\"\[Chi]\", \"TI\"], StyleBox[RowBox[{\"c\", \" \", \"1\"}], \"TR\"]]\), \!\(\*SubscriptBox[StyleBox[\"\[Chi]\", \"TI\"], StyleBox[RowBox[{\"c\", \" \", \"2\"}], \"TR\"]]\), \!\(\*StyleBox[\"\\\"\[Ellipsis]\\\"\", \"TR\"]\), turn ratios \!\(\*SubscriptBox[StyleBox[\"i\", \"TI\"], StyleBox[RowBox[{\"\[Chi]\", \" \", \"1\"}], \"TR\"]]\), \!\(\*SubscriptBox[StyleBox[\"i\", \"TI\"], StyleBox[RowBox[{\"\[Chi]\", \" \", \"2\"}], \"TR\"]]\), \!\(\*StyleBox[\"\\\"\[Ellipsis]\\\"\", \"TR\"]\), radius \!\(\*SubscriptBox[StyleBox[\"\[Rho]\", \"TI\"], StyleBox[\"c\", \"TI\"]]\) and target field harmonic of order \!\(\*StyleBox[\"n\", \"TI\"]\)."}]],"Output",CellTags->"LoopCoilPlot3D_usages"],Cell[BoxData[RowBox[{" is an alternative way of specifying the axial separations \!\(\*SubscriptBox[StyleBox[\"\[Chi]\", \"TI\"], StyleBox[RowBox[{\"c\", \" \", \"1\"}], \"TR\"]]\), \!\(\*SubscriptBox[StyleBox[\"\[Chi]\", \"TI\"], StyleBox[RowBox[{\"c\", \" \", \"2\"}], \"TR\"]]\), \!\(\*StyleBox[\"\\\"\[Ellipsis]\\\"\", \"TR\"]\)."}]],"Output",CellTags->"LoopCoilPlot3D_usages"]}}]];


SaddleCoilPlot::usage =
"\!\(\*RowBox[{\"SaddleCoilPlot\", \"[\", RowBox[{RowBox[{\"{\", RowBox[{SubscriptBox[StyleBox[\"\[Chi]\", \"TI\"], StyleBox[RowBox[{\"c\", \" \", \"1\"}], \"TR\"]], \",\", SubscriptBox[StyleBox[\"\[Chi]\", \"TI\"], StyleBox[RowBox[{\"c\", \" \", \"2\"}], \"TR\"]], \",\", StyleBox[\"\\\"\[Ellipsis]\\\"\", \"TR\"]}], \"}\"}], \",\", RowBox[{\"{\", RowBox[{SubscriptBox[StyleBox[\"\[Phi]\", \"TI\"], StyleBox[RowBox[{\"c\", \" \", \"1\"}], \"TR\"]], \",\", SubscriptBox[StyleBox[\"\[Phi]\", \"TI\"], StyleBox[RowBox[{\"c\", \" \", \"2\"}], \"TR\"]], \",\", StyleBox[\"\\\"\[Ellipsis]\\\"\", \"TR\"]}], \"}\"}], \",\", RowBox[{\"{\", RowBox[{SubscriptBox[StyleBox[\"i\", \"TI\"], StyleBox[RowBox[{\"\[Chi]\", \" \", \"1\"}], \"TR\"]], \",\", SubscriptBox[StyleBox[\"i\", \"TI\"], StyleBox[RowBox[{\"\[Chi]\", \" \", \"2\"}], \"TR\"]], \",\", StyleBox[\"\\\"\[Ellipsis]\\\"\", \"TR\"]}], \"}\"}], \",\", SubscriptBox[StyleBox[\"\[Rho]\", \"TI\"], StyleBox[\"c\", \"TI\"]], \",\", RowBox[{\"{\", RowBox[{StyleBox[\"n\", \"TI\"], \",\", StyleBox[\"m\", \"TI\"]}], \"}\"}]}], \"]\"}]\) plots a schematic in the \!\(\*StyleBox[\"\[Phi]\", \"TI\"]\)\!\(\*StyleBox[\"z\", \"TI\"]\)\[Hyphen]plane of the saddle\[Hyphen]based coil with axial separations \!\(\*SubscriptBox[StyleBox[\"\[Chi]\", \"TI\"], StyleBox[RowBox[{\"c\", \" \", \"1\"}], \"TR\"]]\), \!\(\*SubscriptBox[StyleBox[\"\[Chi]\", \"TI\"], StyleBox[RowBox[{\"c\", \" \", \"2\"}], \"TR\"]]\), \!\(\*StyleBox[\"\\\"\[Ellipsis]\\\"\", \"TR\"]\), azimuthal extents \!\(\*SubscriptBox[StyleBox[\"\[Phi]\", \"TI\"], StyleBox[RowBox[{\"c\", \" \", \"1\"}], \"TR\"]]\), \!\(\*SubscriptBox[StyleBox[\"\[Phi]\", \"TI\"], StyleBox[RowBox[{\"c\", \" \", \"2\"}], \"TR\"]]\), \!\(\*StyleBox[\"\\\"\[Ellipsis]\\\"\", \"TR\"]\), turn ratios \!\(\*SubscriptBox[StyleBox[\"i\", \"TI\"], StyleBox[RowBox[{\"\[Chi]\", \" \", \"1\"}], \"TR\"]]\), \!\(\*SubscriptBox[StyleBox[\"i\", \"TI\"], StyleBox[RowBox[{\"\[Chi]\", \" \", \"2\"}], \"TR\"]]\), \!\(\*StyleBox[\"\\\"\[Ellipsis]\\\"\", \"TR\"]\), radius \!\(\*SubscriptBox[StyleBox[\"\[Rho]\", \"TI\"], StyleBox[\"c\", \"TI\"]]\) and target field harmonic of order \!\(\*StyleBox[\"n\", \"TI\"]\) and degree \!\(\*StyleBox[\"m\", \"TI\"]\).
\!\(\*RowBox[{\"SaddleCoilPlot\", \"[\", RowBox[{RowBox[{\"{\", RowBox[{RowBox[{RowBox[{\"Coil\[Chi]c\", \"[\", \"1\", \"]\"}], \"\[Rule]\", SubscriptBox[StyleBox[\"\[Chi]\", \"TI\"], StyleBox[RowBox[{\"c\", \" \", \"1\"}], \"TR\"]]}], \",\", RowBox[{RowBox[{\"Coil\[Chi]c\", \"[\", \"2\", \"]\"}], \"\[Rule]\", SubscriptBox[StyleBox[\"\[Chi]\", \"TI\"], StyleBox[RowBox[{\"c\", \" \", \"2\"}], \"TR\"]]}], \",\", StyleBox[\"\\\"\[Ellipsis]\\\"\", \"TR\"]}], \"}\"}], \",\", RowBox[{\"{\", RowBox[{RowBox[{RowBox[{\"Coil\[Phi]c\", \"[\", \"1\", \"]\"}], \"\[Rule]\", SubscriptBox[StyleBox[\"\[Phi]\", \"TI\"], StyleBox[RowBox[{\"c\", \" \", \"1\"}], \"TR\"]]}], \",\", RowBox[{RowBox[{\"Coil\[Phi]c\", \"[\", \"2\", \"]\"}], \"\[Rule]\", SubscriptBox[StyleBox[\"\[Phi]\", \"TI\"], StyleBox[RowBox[{\"c\", \" \", \"2\"}], \"TR\"]]}], \",\", StyleBox[\"\\\"\[Ellipsis]\\\"\", \"TR\"]}], \"}\"}], \",\", StyleBox[\"\\\"\[Ellipsis]\\\"\", \"TR\"]}], \"]\"}]\) is an alternative way of specifying the axial separations \!\(\*SubscriptBox[StyleBox[\"\[Chi]c\", \"TI\"], StyleBox[\"1\", \"TR\"]]\), \!\(\*SubscriptBox[StyleBox[\"\[Chi]c\", \"TI\"], StyleBox[\"2\", \"TR\"]]\), \!\(\*StyleBox[\"\\\"\[Ellipsis]\\\"\", \"TR\"]\) and azimuthal extents \!\(\*SubscriptBox[StyleBox[\"\[Phi]\", \"TI\"], StyleBox[RowBox[{\"c\", \" \", \"1\"}], \"TR\"]]\), \!\(\*SubscriptBox[StyleBox[\"\[Phi]\", \"TI\"], StyleBox[RowBox[{\"c\", \" \", \"2\"}], \"TR\"]]\), \!\(\*StyleBox[\"\\\"\[Ellipsis]\\\"\", \"TR\"]\).";

MathLink`CallFrontEnd[FrontEnd`CacheTemplateAndUsagePacket[FrontEnd`InputNotebook[],"SaddleCoilPlot",{{Cell[BoxData[GridBox[{{RowBox[{"SaddleCoilPlot","[",RowBox[{RowBox[{"{",RowBox[{SubscriptBox[StyleBox["\[Chi]","TI"],StyleBox[RowBox[{"c"," ","1"}],"TR"]],",",SubscriptBox[StyleBox["\[Chi]","TI"],StyleBox[RowBox[{"c"," ","2"}],"TR"]],",",StyleBox["\"\[Ellipsis]\"","TR"]}],"}"}],",",RowBox[{"{",RowBox[{SubscriptBox[StyleBox["\[Phi]","TI"],StyleBox[RowBox[{"c"," ","1"}],"TR"]],",",SubscriptBox[StyleBox["\[Phi]","TI"],StyleBox[RowBox[{"c"," ","2"}],"TR"]],",",StyleBox["\"\[Ellipsis]\"","TR"]}],"}"}],",",RowBox[{"{",RowBox[{SubscriptBox[StyleBox["i","TI"],StyleBox[RowBox[{"\[Chi]"," ","1"}],"TR"]],",",SubscriptBox[StyleBox["i","TI"],StyleBox[RowBox[{"\[Chi]"," ","2"}],"TR"]],",",StyleBox["\"\[Ellipsis]\"","TR"]}],"}"}],",",SubscriptBox[StyleBox["\[Rho]","TI"],StyleBox["c","TI"]],",",RowBox[{"{",RowBox[{StyleBox["n","TI"],",",StyleBox["m","TI"]}],"}"}]}],"]"}]}}]],"InlineFormula",CellTags->"SaddleCoilPlot_templates"],Cell[BoxData[GridBox[{{RowBox[{"SaddleCoilPlot","[",RowBox[{RowBox[{"{",RowBox[{RowBox[{RowBox[{"Coil\[Chi]c","[","1","]"}],"->",SubscriptBox[StyleBox["\[Chi]","TI"],StyleBox[RowBox[{"c"," ","1"}],"TR"]]}],",",RowBox[{RowBox[{"Coil\[Chi]c","[","2","]"}],"->",SubscriptBox[StyleBox["\[Chi]","TI"],StyleBox[RowBox[{"c"," ","2"}],"TR"]]}],",",StyleBox["\"\[Ellipsis]\"","TR"]}],"}"}],",",RowBox[{"{",RowBox[{RowBox[{RowBox[{"Coil\[Phi]c","[","1","]"}],"->",SubscriptBox[StyleBox["\[Phi]","TI"],StyleBox[RowBox[{"c"," ","1"}],"TR"]]}],",",RowBox[{RowBox[{"Coil\[Phi]c","[","2","]"}],"->",SubscriptBox[StyleBox["\[Phi]","TI"],StyleBox[RowBox[{"c"," ","2"}],"TR"]]}],",",StyleBox["\"\[Ellipsis]\"","TR"]}],"}"}],",",StyleBox["\"\[Ellipsis]\"","TR"]}],"]"}]}}]],"InlineFormula",CellTags->"SaddleCoilPlot_templates"]},{Cell[BoxData[RowBox[{" plots a schematic in the \!\(\*StyleBox[\"\[Phi]\", \"TI\"]\)\!\(\*StyleBox[\"z\", \"TI\"]\)-plane of the saddle-based coil with axial separations \!\(\*SubscriptBox[StyleBox[\"\[Chi]\", \"TI\"], StyleBox[RowBox[{\"c\", \" \", \"1\"}], \"TR\"]]\), \!\(\*SubscriptBox[StyleBox[\"\[Chi]\", \"TI\"], StyleBox[RowBox[{\"c\", \" \", \"2\"}], \"TR\"]]\), \!\(\*StyleBox[\"\\\"\[Ellipsis]\\\"\", \"TR\"]\), azimuthal extents \!\(\*SubscriptBox[StyleBox[\"\[Phi]\", \"TI\"], StyleBox[RowBox[{\"c\", \" \", \"1\"}], \"TR\"]]\), \!\(\*SubscriptBox[StyleBox[\"\[Phi]\", \"TI\"], StyleBox[RowBox[{\"c\", \" \", \"2\"}], \"TR\"]]\), \!\(\*StyleBox[\"\\\"\[Ellipsis]\\\"\", \"TR\"]\), turn ratios \!\(\*SubscriptBox[StyleBox[\"i\", \"TI\"], StyleBox[RowBox[{\"\[Chi]\", \" \", \"1\"}], \"TR\"]]\), \!\(\*SubscriptBox[StyleBox[\"i\", \"TI\"], StyleBox[RowBox[{\"\[Chi]\", \" \", \"2\"}], \"TR\"]]\), \!\(\*StyleBox[\"\\\"\[Ellipsis]\\\"\", \"TR\"]\), radius \!\(\*SubscriptBox[StyleBox[\"\[Rho]\", \"TI\"], StyleBox[\"c\", \"TI\"]]\) and target field harmonic of order \!\(\*StyleBox[\"n\", \"TI\"]\) and degree \!\(\*StyleBox[\"m\", \"TI\"]\)."}]],"Output",CellTags->"SaddleCoilPlot_usages"],Cell[BoxData[RowBox[{" is an alternative way of specifying the axial separations \!\(\*SubscriptBox[StyleBox[\"\[Chi]c\", \"TI\"], StyleBox[\"1\", \"TR\"]]\), \!\(\*SubscriptBox[StyleBox[\"\[Chi]c\", \"TI\"], StyleBox[\"2\", \"TR\"]]\), \!\(\*StyleBox[\"\\\"\[Ellipsis]\\\"\", \"TR\"]\) and azimuthal extents \!\(\*SubscriptBox[StyleBox[\"\[Phi]\", \"TI\"], StyleBox[RowBox[{\"c\", \" \", \"1\"}], \"TR\"]]\), \!\(\*SubscriptBox[StyleBox[\"\[Phi]\", \"TI\"], StyleBox[RowBox[{\"c\", \" \", \"2\"}], \"TR\"]]\), \!\(\*StyleBox[\"\\\"\[Ellipsis]\\\"\", \"TR\"]\)."}]],"Output",CellTags->"SaddleCoilPlot_usages"]}}]]


SaddleCoilPlot3D::usage =
"\!\(\*RowBox[{\"SaddleCoilPlot3D\", \"[\", RowBox[{RowBox[{\"{\", RowBox[{SubscriptBox[StyleBox[\"\[Chi]\", \"TI\"], StyleBox[RowBox[{\"c\", \" \", \"1\"}], \"TR\"]], \",\", SubscriptBox[StyleBox[\"\[Chi]\", \"TI\"], StyleBox[RowBox[{\"c\", \" \", \"2\"}], \"TR\"]], \",\", StyleBox[\"\\\"\[Ellipsis]\\\"\", \"TR\"]}], \"}\"}], \",\", RowBox[{\"{\", RowBox[{SubscriptBox[StyleBox[\"\[Phi]\", \"TI\"], StyleBox[RowBox[{\"c\", \" \", \"1\"}], \"TR\"]], \",\", SubscriptBox[StyleBox[\"\[Phi]\", \"TI\"], StyleBox[RowBox[{\"c\", \" \", \"2\"}], \"TR\"]], \",\", StyleBox[\"\\\"\[Ellipsis]\\\"\", \"TR\"]}], \"}\"}], \",\", RowBox[{\"{\", RowBox[{SubscriptBox[StyleBox[\"i\", \"TI\"], StyleBox[RowBox[{\"\[Chi]\", \" \", \"1\"}], \"TR\"]], \",\", SubscriptBox[StyleBox[\"i\", \"TI\"], StyleBox[RowBox[{\"\[Chi]\", \" \", \"2\"}], \"TR\"]], \",\", StyleBox[\"\\\"\[Ellipsis]\\\"\", \"TR\"]}], \"}\"}], \",\", SubscriptBox[StyleBox[\"\[Rho]\", \"TI\"], StyleBox[\"c\", \"TI\"]], \",\", RowBox[{\"{\", RowBox[{StyleBox[\"n\", \"TI\"], \",\", StyleBox[\"m\", \"TI\"]}], \"}\"}]}], \"]\"}]\) returns a 3D plot of the saddle\[Hyphen]based coil with axial separations \!\(\*SubscriptBox[StyleBox[\"\[Chi]\", \"TI\"], StyleBox[RowBox[{\"c\", \" \", \"1\"}], \"TR\"]]\), \!\(\*SubscriptBox[StyleBox[\"\[Chi]\", \"TI\"], StyleBox[RowBox[{\"c\", \" \", \"2\"}], \"TR\"]]\), \!\(\*StyleBox[\"\\\"\[Ellipsis]\\\"\", \"TR\"]\), azimuthal extents \!\(\*SubscriptBox[StyleBox[\"\[Phi]\", \"TI\"], StyleBox[RowBox[{\"c\", \" \", \"1\"}], \"TR\"]]\), \!\(\*SubscriptBox[StyleBox[\"\[Phi]\", \"TI\"], StyleBox[RowBox[{\"c\", \" \", \"2\"}], \"TR\"]]\), \!\(\*StyleBox[\"\\\"\[Ellipsis]\\\"\", \"TR\"]\), turn ratios \!\(\*SubscriptBox[StyleBox[\"i\", \"TI\"], StyleBox[RowBox[{\"\[Chi]\", \" \", \"1\"}], \"TR\"]]\), \!\(\*SubscriptBox[StyleBox[\"i\", \"TI\"], StyleBox[RowBox[{\"\[Chi]\", \" \", \"2\"}], \"TR\"]]\), \!\(\*StyleBox[\"\\\"\[Ellipsis]\\\"\", \"TR\"]\), radius \!\(\*SubscriptBox[StyleBox[\"\[Rho]\", \"TI\"], StyleBox[\"c\", \"TI\"]]\) and target field harmonic of order \!\(\*StyleBox[\"n\", \"TI\"]\) and degree \!\(\*StyleBox[\"m\", \"TI\"]\).
\!\(\*RowBox[{\"SaddleCoilPlot3D\", \"[\", RowBox[{RowBox[{\"{\", RowBox[{RowBox[{RowBox[{\"Coil\[Chi]c\", \"[\", \"1\", \"]\"}], \"\[Rule]\", SubscriptBox[StyleBox[\"\[Chi]\", \"TI\"], StyleBox[RowBox[{\"c\", \" \", \"1\"}], \"TR\"]]}], \",\", RowBox[{RowBox[{\"Coil\[Chi]c\", \"[\", \"2\", \"]\"}], \"\[Rule]\", SubscriptBox[StyleBox[\"\[Chi]\", \"TI\"], StyleBox[RowBox[{\"c\", \" \", \"2\"}], \"TR\"]]}], \",\", StyleBox[\"\\\"\[Ellipsis]\\\"\", \"TR\"]}], \"}\"}], \",\", RowBox[{\"{\", RowBox[{RowBox[{RowBox[{\"Coil\[Phi]c\", \"[\", \"1\", \"]\"}], \"\[Rule]\", SubscriptBox[StyleBox[\"\[Phi]\", \"TI\"], StyleBox[RowBox[{\"c\", \" \", \"1\"}], \"TR\"]]}], \",\", RowBox[{RowBox[{\"Coil\[Phi]c\", \"[\", \"2\", \"]\"}], \"\[Rule]\", SubscriptBox[StyleBox[\"\[Phi]\", \"TI\"], StyleBox[RowBox[{\"c\", \" \", \"2\"}], \"TR\"]]}], \",\", StyleBox[\"\\\"\[Ellipsis]\\\"\", \"TR\"]}], \"}\"}], \",\", StyleBox[\"\\\"\[Ellipsis]\\\"\", \"TR\"]}], \"]\"}]\) is an alternative way of specifying the axial separations \!\(\*SubscriptBox[StyleBox[\"\[Chi]c\", \"TI\"], StyleBox[\"1\", \"TR\"]]\), \!\(\*SubscriptBox[StyleBox[\"\[Chi]c\", \"TI\"], StyleBox[\"2\", \"TR\"]]\), \!\(\*StyleBox[\"\\\"\[Ellipsis]\\\"\", \"TR\"]\) and azimuthal extents \!\(\*SubscriptBox[StyleBox[\"\[Phi]\", \"TI\"], StyleBox[RowBox[{\"c\", \" \", \"1\"}], \"TR\"]]\), \!\(\*SubscriptBox[StyleBox[\"\[Phi]\", \"TI\"], StyleBox[RowBox[{\"c\", \" \", \"2\"}], \"TR\"]]\), \!\(\*StyleBox[\"\\\"\[Ellipsis]\\\"\", \"TR\"]\).";

MathLink`CallFrontEnd[FrontEnd`CacheTemplateAndUsagePacket[FrontEnd`InputNotebook[],"SaddleCoilPlot3D",{{Cell[BoxData[GridBox[{{RowBox[{"SaddleCoilPlot3D","[",RowBox[{RowBox[{"{",RowBox[{SubscriptBox[StyleBox["\[Chi]","TI"],StyleBox[RowBox[{"c"," ","1"}],"TR"]],",",SubscriptBox[StyleBox["\[Chi]","TI"],StyleBox[RowBox[{"c"," ","2"}],"TR"]],",",StyleBox["\"\[Ellipsis]\"","TR"]}],"}"}],",",RowBox[{"{",RowBox[{SubscriptBox[StyleBox["\[Phi]","TI"],StyleBox[RowBox[{"c"," ","1"}],"TR"]],",",SubscriptBox[StyleBox["\[Phi]","TI"],StyleBox[RowBox[{"c"," ","2"}],"TR"]],",",StyleBox["\"\[Ellipsis]\"","TR"]}],"}"}],",",RowBox[{"{",RowBox[{SubscriptBox[StyleBox["i","TI"],StyleBox[RowBox[{"\[Chi]"," ","1"}],"TR"]],",",SubscriptBox[StyleBox["i","TI"],StyleBox[RowBox[{"\[Chi]"," ","2"}],"TR"]],",",StyleBox["\"\[Ellipsis]\"","TR"]}],"}"}],",",SubscriptBox[StyleBox["\[Rho]","TI"],StyleBox["c","TI"]],",",RowBox[{"{",RowBox[{StyleBox["n","TI"],",",StyleBox["m","TI"]}],"}"}]}],"]"}]}}]],"InlineFormula",CellTags->"SaddleCoilPlot3D_templates"],Cell[BoxData[GridBox[{{RowBox[{"SaddleCoilPlot3D","[",RowBox[{RowBox[{"{",RowBox[{RowBox[{RowBox[{"Coil\[Chi]c","[","1","]"}],"->",SubscriptBox[StyleBox["\[Chi]","TI"],StyleBox[RowBox[{"c"," ","1"}],"TR"]]}],",",RowBox[{RowBox[{"Coil\[Chi]c","[","2","]"}],"->",SubscriptBox[StyleBox["\[Chi]","TI"],StyleBox[RowBox[{"c"," ","2"}],"TR"]]}],",",StyleBox["\"\[Ellipsis]\"","TR"]}],"}"}],",",RowBox[{"{",RowBox[{RowBox[{RowBox[{"Coil\[Phi]c","[","1","]"}],"->",SubscriptBox[StyleBox["\[Phi]","TI"],StyleBox[RowBox[{"c"," ","1"}],"TR"]]}],",",RowBox[{RowBox[{"Coil\[Phi]c","[","2","]"}],"->",SubscriptBox[StyleBox["\[Phi]","TI"],StyleBox[RowBox[{"c"," ","2"}],"TR"]]}],",",StyleBox["\"\[Ellipsis]\"","TR"]}],"}"}],",",StyleBox["\"\[Ellipsis]\"","TR"]}],"]"}]}}]],"InlineFormula",CellTags->"SaddleCoilPlot3D_templates"]},{Cell[BoxData[RowBox[{" returns a 3D plot of the saddle-based coil with axial separations \!\(\*SubscriptBox[StyleBox[\"\[Chi]\", \"TI\"], StyleBox[RowBox[{\"c\", \" \", \"1\"}], \"TR\"]]\), \!\(\*SubscriptBox[StyleBox[\"\[Chi]\", \"TI\"], StyleBox[RowBox[{\"c\", \" \", \"2\"}], \"TR\"]]\), \!\(\*StyleBox[\"\\\"\[Ellipsis]\\\"\", \"TR\"]\), azimuthal extents \!\(\*SubscriptBox[StyleBox[\"\[Phi]\", \"TI\"], StyleBox[RowBox[{\"c\", \" \", \"1\"}], \"TR\"]]\), \!\(\*SubscriptBox[StyleBox[\"\[Phi]\", \"TI\"], StyleBox[RowBox[{\"c\", \" \", \"2\"}], \"TR\"]]\), \!\(\*StyleBox[\"\\\"\[Ellipsis]\\\"\", \"TR\"]\), turn ratios \!\(\*SubscriptBox[StyleBox[\"i\", \"TI\"], StyleBox[RowBox[{\"\[Chi]\", \" \", \"1\"}], \"TR\"]]\), \!\(\*SubscriptBox[StyleBox[\"i\", \"TI\"], StyleBox[RowBox[{\"\[Chi]\", \" \", \"2\"}], \"TR\"]]\), \!\(\*StyleBox[\"\\\"\[Ellipsis]\\\"\", \"TR\"]\), radius \!\(\*SubscriptBox[StyleBox[\"\[Rho]\", \"TI\"], StyleBox[\"c\", \"TI\"]]\) and target field harmonic of order \!\(\*StyleBox[\"n\", \"TI\"]\) and degree \!\(\*StyleBox[\"m\", \"TI\"]\)."}]],"Output",CellTags->"SaddleCoilPlot3D_usages"],Cell[BoxData[RowBox[{" is an alternative way of specifying the axial separations \!\(\*SubscriptBox[StyleBox[\"\[Chi]c\", \"TI\"], StyleBox[\"1\", \"TR\"]]\), \!\(\*SubscriptBox[StyleBox[\"\[Chi]c\", \"TI\"], StyleBox[\"2\", \"TR\"]]\), \!\(\*StyleBox[\"\\\"\[Ellipsis]\\\"\", \"TR\"]\) and azimuthal extents \!\(\*SubscriptBox[StyleBox[\"\[Phi]\", \"TI\"], StyleBox[RowBox[{\"c\", \" \", \"1\"}], \"TR\"]]\), \!\(\*SubscriptBox[StyleBox[\"\[Phi]\", \"TI\"], StyleBox[RowBox[{\"c\", \" \", \"2\"}], \"TR\"]]\), \!\(\*StyleBox[\"\\\"\[Ellipsis]\\\"\", \"TR\"]\)."}]],"Output",CellTags->"SaddleCoilPlot3D_usages"]}}]];


EllipseCoilPlot::usage =
"\!\(\*RowBox[{\"EllipseCoilPlot\", \"[\", RowBox[{RowBox[{\"{\", RowBox[{RowBox[{\"{\", RowBox[{SubscriptBox[StyleBox[\"\[Chi]\", \"TI\"], StyleBox[RowBox[{\"c\", \" \", \"1\"}], \"TR\"]], \",\", SubscriptBox[StyleBox[\"\[Psi]\", \"TI\"], StyleBox[RowBox[{\"c\", \" \", \"1\"}], \"TR\"]]}], \"}\"}], \",\", RowBox[{\"{\", RowBox[{SubscriptBox[StyleBox[\"\[Chi]\", \"TI\"], StyleBox[RowBox[{\"c\", \" \", \"2\"}], \"TR\"]], \",\", SubscriptBox[StyleBox[\"\[Psi]\", \"TI\"], StyleBox[RowBox[{\"c\", \" \", \"2\"}], \"TR\"]]}], \"}\"}], \",\", StyleBox[\"\\\"\[Ellipsis]\\\"\", \"TR\"]}], \"}\"}], \",\", RowBox[{\"{\", RowBox[{SubscriptBox[StyleBox[\"i\", \"TI\"], StyleBox[RowBox[{\"\[Chi]\", \" \", \"1\"}], \"TR\"]], \",\", SubscriptBox[StyleBox[\"i\", \"TI\"], StyleBox[RowBox[{\"\[Chi]\", \" \", \"2\"}], \"TR\"]], \",\", StyleBox[\"\\\"\[Ellipsis]\\\"\", \"TR\"]}], \"}\"}], \",\", SubscriptBox[StyleBox[\"\[Rho]\", \"TI\"], StyleBox[\"c\", \"TI\"]], \",\", RowBox[{\"{\", RowBox[{StyleBox[\"n\", \"TI\"], \",\", StyleBox[\"m\", \"TI\"]}], \"}\"}]}], \"]\"}]\) plots a schematic in the \!\(\*StyleBox[\"\[Phi]\", \"TI\"]\)\!\(\*StyleBox[\"z\", \"TI\"]\)\[Hyphen]plane of the ellipse\[Hyphen]based coil with axial separations \!\(\*SubscriptBox[StyleBox[\"\[Chi]\", \"TI\"], StyleBox[RowBox[{\"c\", \" \", \"1\"}], \"TR\"]]\), \!\(\*SubscriptBox[StyleBox[\"\[Chi]\", \"TI\"], StyleBox[RowBox[{\"c\", \" \", \"2\"}], \"TR\"]]\), \!\(\*StyleBox[\"\\\"\[Ellipsis]\\\"\", \"TR\"]\), ellipse extents \!\(\*SubscriptBox[StyleBox[\"\[Psi]\", \"TI\"], StyleBox[RowBox[{\"c\", \" \", \"1\"}], \"TR\"]]\), \!\(\*SubscriptBox[StyleBox[\"\[Psi]\", \"TI\"], StyleBox[RowBox[{\"c\", \" \", \"2\"}], \"TR\"]]\), \!\(\*StyleBox[\"\\\"\[Ellipsis]\\\"\", \"TR\"]\), turn ratios \!\(\*SubscriptBox[StyleBox[\"i\", \"TI\"], StyleBox[RowBox[{\"\[Chi]\", \" \", \"1\"}], \"TR\"]]\), \!\(\*SubscriptBox[StyleBox[\"i\", \"TI\"], StyleBox[RowBox[{\"\[Chi]\", \" \", \"2\"}], \"TR\"]]\), \!\(\*StyleBox[\"\\\"\[Ellipsis]\\\"\", \"TR\"]\), radius \!\(\*SubscriptBox[StyleBox[\"\[Rho]\", \"TI\"], StyleBox[\"c\", \"TI\"]]\) and target field harmonic of order \!\(\*StyleBox[\"n\", \"TI\"]\) and degree \!\(\*StyleBox[\"m\", \"TI\"]\).
\!\(\*RowBox[{\"EllipseCoilPlot\", \"[\", RowBox[{RowBox[{\"{\", RowBox[{RowBox[{RowBox[{\"Coil\[Chi]c\", \"[\", \"1\", \"]\"}], \"\[Rule]\", SubscriptBox[StyleBox[\"\[Chi]\", \"TI\"], StyleBox[RowBox[{\"c\", \" \", \"1\"}], \"TR\"]]}], \",\", RowBox[{RowBox[{\"Coil\[Chi]c\", \"[\", \"2\", \"]\"}], \"\[Rule]\", SubscriptBox[StyleBox[\"\[Chi]\", \"TI\"], StyleBox[RowBox[{\"c\", \" \", \"2\"}], \"TR\"]]}], \",\", StyleBox[\"\\\"\[Ellipsis]\\\"\", \"TR\"], \",\", RowBox[{RowBox[{\"Coil\[Psi]c\", \"[\", \"1\", \"]\"}], \"\[Rule]\", SubscriptBox[StyleBox[\"\[Psi]\", \"TI\"], StyleBox[RowBox[{\"c\", \" \", \"1\"}], \"TR\"]]}], \",\", RowBox[{RowBox[{\"Coil\[Psi]c\", \"[\", \"2\", \"]\"}], \"\[Rule]\", SubscriptBox[StyleBox[\"\[Psi]\", \"TI\"], StyleBox[RowBox[{\"c\", \" \", \"2\"}], \"TR\"]]}], \",\", StyleBox[\"\\\"\[Ellipsis]\\\"\", \"TR\"]}], \"}\"}], \",\", StyleBox[\"\\\"\[Ellipsis]\\\"\", \"TR\"]}], \"]\"}]\) is an alternative way of specifying the axial separations \!\(\*SubscriptBox[StyleBox[\"\[Chi]c\", \"TI\"], StyleBox[\"1\", \"TR\"]]\), \!\(\*SubscriptBox[StyleBox[\"\[Chi]c\", \"TI\"], StyleBox[\"2\", \"TR\"]]\), \!\(\*StyleBox[\"\\\"\[Ellipsis]\\\"\", \"TR\"]\) and ellipse extents \!\(\*SubscriptBox[StyleBox[\"\[Psi]\", \"TI\"], StyleBox[RowBox[{\"c\", \" \", \"1\"}], \"TR\"]]\), \!\(\*SubscriptBox[StyleBox[\"\[Psi]\", \"TI\"], StyleBox[RowBox[{\"c\", \" \", \"2\"}], \"TR\"]]\), \!\(\*StyleBox[\"\\\"\[Ellipsis]\\\"\", \"TR\"]\).";

MathLink`CallFrontEnd[FrontEnd`CacheTemplateAndUsagePacket[FrontEnd`InputNotebook[],"EllipseCoilPlot",{{Cell[BoxData[GridBox[{{RowBox[{"EllipseCoilPlot","[",RowBox[{RowBox[{"{",RowBox[{RowBox[{"{",RowBox[{SubscriptBox[StyleBox["\[Chi]","TI"],StyleBox[RowBox[{"c"," ","1"}],"TR"]],",",SubscriptBox[StyleBox["\[Psi]","TI"],StyleBox[RowBox[{"c"," ","1"}],"TR"]]}],"}"}],",",RowBox[{"{",RowBox[{SubscriptBox[StyleBox["\[Chi]","TI"],StyleBox[RowBox[{"c"," ","2"}],"TR"]],",",SubscriptBox[StyleBox["\[Psi]","TI"],StyleBox[RowBox[{"c"," ","2"}],"TR"]]}],"}"}],",",StyleBox["\"\[Ellipsis]\"","TR"]}],"}"}],",",RowBox[{"{",RowBox[{SubscriptBox[StyleBox["i","TI"],StyleBox[RowBox[{"\[Chi]"," ","1"}],"TR"]],",",SubscriptBox[StyleBox["i","TI"],StyleBox[RowBox[{"\[Chi]"," ","2"}],"TR"]],",",StyleBox["\"\[Ellipsis]\"","TR"]}],"}"}],",",SubscriptBox[StyleBox["\[Rho]","TI"],StyleBox["c","TI"]],",",RowBox[{"{",RowBox[{StyleBox["n","TI"],",",StyleBox["m","TI"]}],"}"}]}],"]"}]}}]],"InlineFormula",CellTags->"EllipseCoilPlot_templates"],Cell[BoxData[GridBox[{{RowBox[{"EllipseCoilPlot","[",RowBox[{RowBox[{"{",RowBox[{RowBox[{RowBox[{"Coil\[Chi]c","[","1","]"}],"->",SubscriptBox[StyleBox["\[Chi]","TI"],StyleBox[RowBox[{"c"," ","1"}],"TR"]]}],",",RowBox[{RowBox[{"Coil\[Chi]c","[","2","]"}],"->",SubscriptBox[StyleBox["\[Chi]","TI"],StyleBox[RowBox[{"c"," ","2"}],"TR"]]}],",",StyleBox["\"\[Ellipsis]\"","TR"],",",RowBox[{RowBox[{"Coil\[Psi]c","[","1","]"}],"->",SubscriptBox[StyleBox["\[Psi]","TI"],StyleBox[RowBox[{"c"," ","1"}],"TR"]]}],",",RowBox[{RowBox[{"Coil\[Psi]c","[","2","]"}],"->",SubscriptBox[StyleBox["\[Psi]","TI"],StyleBox[RowBox[{"c"," ","2"}],"TR"]]}],",",StyleBox["\"\[Ellipsis]\"","TR"]}],"}"}],",",StyleBox["\"\[Ellipsis]\"","TR"]}],"]"}]}}]],"InlineFormula",CellTags->"EllipseCoilPlot_templates"]},{Cell[BoxData[RowBox[{" plots a schematic in the \!\(\*StyleBox[\"\[Phi]\", \"TI\"]\)\!\(\*StyleBox[\"z\", \"TI\"]\)-plane of the ellipse-based coil with axial separations \!\(\*SubscriptBox[StyleBox[\"\[Chi]\", \"TI\"], StyleBox[RowBox[{\"c\", \" \", \"1\"}], \"TR\"]]\), \!\(\*SubscriptBox[StyleBox[\"\[Chi]\", \"TI\"], StyleBox[RowBox[{\"c\", \" \", \"2\"}], \"TR\"]]\), \!\(\*StyleBox[\"\\\"\[Ellipsis]\\\"\", \"TR\"]\), ellipse extents \!\(\*SubscriptBox[StyleBox[\"\[Psi]\", \"TI\"], StyleBox[RowBox[{\"c\", \" \", \"1\"}], \"TR\"]]\), \!\(\*SubscriptBox[StyleBox[\"\[Psi]\", \"TI\"], StyleBox[RowBox[{\"c\", \" \", \"2\"}], \"TR\"]]\), \!\(\*StyleBox[\"\\\"\[Ellipsis]\\\"\", \"TR\"]\), turn ratios \!\(\*SubscriptBox[StyleBox[\"i\", \"TI\"], StyleBox[RowBox[{\"\[Chi]\", \" \", \"1\"}], \"TR\"]]\), \!\(\*SubscriptBox[StyleBox[\"i\", \"TI\"], StyleBox[RowBox[{\"\[Chi]\", \" \", \"2\"}], \"TR\"]]\), \!\(\*StyleBox[\"\\\"\[Ellipsis]\\\"\", \"TR\"]\), radius \!\(\*SubscriptBox[StyleBox[\"\[Rho]\", \"TI\"], StyleBox[\"c\", \"TI\"]]\) and target field harmonic of order \!\(\*StyleBox[\"n\", \"TI\"]\) and degree \!\(\*StyleBox[\"m\", \"TI\"]\)."}]],"Output",CellTags->"EllipseCoilPlot_usages"],Cell[BoxData[RowBox[{" is an alternative way of specifying the axial separations \!\(\*SubscriptBox[StyleBox[\"\[Chi]c\", \"TI\"], StyleBox[\"1\", \"TR\"]]\), \!\(\*SubscriptBox[StyleBox[\"\[Chi]c\", \"TI\"], StyleBox[\"2\", \"TR\"]]\), \!\(\*StyleBox[\"\\\"\[Ellipsis]\\\"\", \"TR\"]\) and ellipse extents \!\(\*SubscriptBox[StyleBox[\"\[Psi]\", \"TI\"], StyleBox[RowBox[{\"c\", \" \", \"1\"}], \"TR\"]]\), \!\(\*SubscriptBox[StyleBox[\"\[Psi]\", \"TI\"], StyleBox[RowBox[{\"c\", \" \", \"2\"}], \"TR\"]]\), \!\(\*StyleBox[\"\\\"\[Ellipsis]\\\"\", \"TR\"]\)."}]],"Output",CellTags->"EllipseCoilPlot_usages"]}}]]


EllipseCoilPlot3D::usage =
"\!\(\*RowBox[{\"EllipseCoilPlot3D\", \"[\", RowBox[{RowBox[{\"{\", RowBox[{RowBox[{\"{\", RowBox[{SubscriptBox[StyleBox[\"\[Chi]\", \"TI\"], StyleBox[RowBox[{\"c\", \" \", \"1\"}], \"TR\"]], \",\", SubscriptBox[StyleBox[\"\[Psi]\", \"TI\"], StyleBox[RowBox[{\"c\", \" \", \"1\"}], \"TR\"]]}], \"}\"}], \",\", RowBox[{\"{\", RowBox[{SubscriptBox[StyleBox[\"\[Chi]\", \"TI\"], StyleBox[RowBox[{\"c\", \" \", \"2\"}], \"TR\"]], \",\", SubscriptBox[StyleBox[\"\[Psi]\", \"TI\"], StyleBox[RowBox[{\"c\", \" \", \"2\"}], \"TR\"]]}], \"}\"}], \",\", StyleBox[\"\\\"\[Ellipsis]\\\"\", \"TR\"]}], \"}\"}], \",\", RowBox[{\"{\", RowBox[{SubscriptBox[StyleBox[\"i\", \"TI\"], StyleBox[RowBox[{\"\[Chi]\", \" \", \"1\"}], \"TR\"]], \",\", SubscriptBox[StyleBox[\"i\", \"TI\"], StyleBox[RowBox[{\"\[Chi]\", \" \", \"2\"}], \"TR\"]], \",\", StyleBox[\"\\\"\[Ellipsis]\\\"\", \"TR\"]}], \"}\"}], \",\", SubscriptBox[StyleBox[\"\[Rho]\", \"TI\"], StyleBox[\"c\", \"TI\"]], \",\", RowBox[{\"{\", RowBox[{StyleBox[\"n\", \"TI\"], \",\", StyleBox[\"m\", \"TI\"]}], \"}\"}]}], \"]\"}]\) returns a 3D plot of the ellipse\[Hyphen]based coil with axial separations \!\(\*SubscriptBox[StyleBox[\"\[Chi]\", \"TI\"], StyleBox[RowBox[{\"c\", \" \", \"1\"}], \"TR\"]]\), \!\(\*SubscriptBox[StyleBox[\"\[Chi]\", \"TI\"], StyleBox[RowBox[{\"c\", \" \", \"2\"}], \"TR\"]]\), \!\(\*StyleBox[\"\\\"\[Ellipsis]\\\"\", \"TR\"]\), ellipse extents \!\(\*SubscriptBox[StyleBox[\"\[Psi]\", \"TI\"], StyleBox[RowBox[{\"c\", \" \", \"1\"}], \"TR\"]]\), \!\(\*SubscriptBox[StyleBox[\"\[Psi]\", \"TI\"], StyleBox[RowBox[{\"c\", \" \", \"2\"}], \"TR\"]]\), \!\(\*StyleBox[\"\\\"\[Ellipsis]\\\"\", \"TR\"]\), turn ratios \!\(\*SubscriptBox[StyleBox[\"i\", \"TI\"], StyleBox[RowBox[{\"\[Chi]\", \" \", \"1\"}], \"TR\"]]\), \!\(\*SubscriptBox[StyleBox[\"i\", \"TI\"], StyleBox[RowBox[{\"\[Chi]\", \" \", \"2\"}], \"TR\"]]\), \!\(\*StyleBox[\"\\\"\[Ellipsis]\\\"\", \"TR\"]\), radius \!\(\*SubscriptBox[StyleBox[\"\[Rho]\", \"TI\"], StyleBox[\"c\", \"TI\"]]\) and target field harmonic of order \!\(\*StyleBox[\"n\", \"TI\"]\) and degree \!\(\*StyleBox[\"m\", \"TI\"]\).
\!\(\*RowBox[{\"EllipseCoilPlot3D\", \"[\", RowBox[{RowBox[{\"{\", RowBox[{RowBox[{RowBox[{\"Coil\[Chi]c\", \"[\", \"1\", \"]\"}], \"\[Rule]\", SubscriptBox[StyleBox[\"\[Chi]\", \"TI\"], StyleBox[RowBox[{\"c\", \" \", \"1\"}], \"TR\"]]}], \",\", RowBox[{RowBox[{\"Coil\[Chi]c\", \"[\", \"2\", \"]\"}], \"\[Rule]\", SubscriptBox[StyleBox[\"\[Chi]\", \"TI\"], StyleBox[RowBox[{\"c\", \" \", \"2\"}], \"TR\"]]}], \",\", StyleBox[\"\\\"\[Ellipsis]\\\"\", \"TR\"], \",\", RowBox[{RowBox[{\"Coil\[Psi]c\", \"[\", \"1\", \"]\"}], \"\[Rule]\", SubscriptBox[StyleBox[\"\[Psi]\", \"TI\"], StyleBox[RowBox[{\"c\", \" \", \"1\"}], \"TR\"]]}], \",\", RowBox[{RowBox[{\"Coil\[Psi]c\", \"[\", \"2\", \"]\"}], \"\[Rule]\", SubscriptBox[StyleBox[\"\[Psi]\", \"TI\"], StyleBox[RowBox[{\"c\", \" \", \"2\"}], \"TR\"]]}], \",\", StyleBox[\"\\\"\[Ellipsis]\\\"\", \"TR\"]}], \"}\"}], \",\", StyleBox[\"\\\"\[Ellipsis]\\\"\", \"TR\"]}], \"]\"}]\) is an alternative way of specifying the axial separations \!\(\*SubscriptBox[StyleBox[\"\[Chi]c\", \"TI\"], StyleBox[\"1\", \"TR\"]]\), \!\(\*SubscriptBox[StyleBox[\"\[Chi]c\", \"TI\"], StyleBox[\"2\", \"TR\"]]\), \!\(\*StyleBox[\"\\\"\[Ellipsis]\\\"\", \"TR\"]\) and ellipse extents \!\(\*SubscriptBox[StyleBox[\"\[Psi]\", \"TI\"], StyleBox[RowBox[{\"c\", \" \", \"1\"}], \"TR\"]]\), \!\(\*SubscriptBox[StyleBox[\"\[Psi]\", \"TI\"], StyleBox[RowBox[{\"c\", \" \", \"2\"}], \"TR\"]]\), \!\(\*StyleBox[\"\\\"\[Ellipsis]\\\"\", \"TR\"]\).";

MathLink`CallFrontEnd[FrontEnd`CacheTemplateAndUsagePacket[FrontEnd`InputNotebook[],"EllipseCoilPlot3D",{{Cell[BoxData[GridBox[{{RowBox[{"EllipseCoilPlot3D","[",RowBox[{RowBox[{"{",RowBox[{RowBox[{"{",RowBox[{SubscriptBox[StyleBox["\[Chi]","TI"],StyleBox[RowBox[{"c"," ","1"}],"TR"]],",",SubscriptBox[StyleBox["\[Psi]","TI"],StyleBox[RowBox[{"c"," ","1"}],"TR"]]}],"}"}],",",RowBox[{"{",RowBox[{SubscriptBox[StyleBox["\[Chi]","TI"],StyleBox[RowBox[{"c"," ","2"}],"TR"]],",",SubscriptBox[StyleBox["\[Psi]","TI"],StyleBox[RowBox[{"c"," ","2"}],"TR"]]}],"}"}],",",StyleBox["\"\[Ellipsis]\"","TR"]}],"}"}],",",RowBox[{"{",RowBox[{SubscriptBox[StyleBox["i","TI"],StyleBox[RowBox[{"\[Chi]"," ","1"}],"TR"]],",",SubscriptBox[StyleBox["i","TI"],StyleBox[RowBox[{"\[Chi]"," ","2"}],"TR"]],",",StyleBox["\"\[Ellipsis]\"","TR"]}],"}"}],",",SubscriptBox[StyleBox["\[Rho]","TI"],StyleBox["c","TI"]],",",RowBox[{"{",RowBox[{StyleBox["n","TI"],",",StyleBox["m","TI"]}],"}"}]}],"]"}]}}]],"InlineFormula",CellTags->"EllipseCoilPlot3D_templates"],Cell[BoxData[GridBox[{{RowBox[{"EllipseCoilPlot3D","[",RowBox[{RowBox[{"{",RowBox[{RowBox[{RowBox[{"Coil\[Chi]c","[","1","]"}],"->",SubscriptBox[StyleBox["\[Chi]","TI"],StyleBox[RowBox[{"c"," ","1"}],"TR"]]}],",",RowBox[{RowBox[{"Coil\[Chi]c","[","2","]"}],"->",SubscriptBox[StyleBox["\[Chi]","TI"],StyleBox[RowBox[{"c"," ","2"}],"TR"]]}],",",StyleBox["\"\[Ellipsis]\"","TR"],",",RowBox[{RowBox[{"Coil\[Psi]c","[","1","]"}],"->",SubscriptBox[StyleBox["\[Psi]","TI"],StyleBox[RowBox[{"c"," ","1"}],"TR"]]}],",",RowBox[{RowBox[{"Coil\[Psi]c","[","2","]"}],"->",SubscriptBox[StyleBox["\[Psi]","TI"],StyleBox[RowBox[{"c"," ","2"}],"TR"]]}],",",StyleBox["\"\[Ellipsis]\"","TR"]}],"}"}],",",StyleBox["\"\[Ellipsis]\"","TR"]}],"]"}]}}]],"InlineFormula",CellTags->"EllipseCoilPlot3D_templates"]},{Cell[BoxData[RowBox[{" returns a 3D plot of the ellipse-based coil with axial separations \!\(\*SubscriptBox[StyleBox[\"\[Chi]\", \"TI\"], StyleBox[RowBox[{\"c\", \" \", \"1\"}], \"TR\"]]\), \!\(\*SubscriptBox[StyleBox[\"\[Chi]\", \"TI\"], StyleBox[RowBox[{\"c\", \" \", \"2\"}], \"TR\"]]\), \!\(\*StyleBox[\"\\\"\[Ellipsis]\\\"\", \"TR\"]\), ellipse extents \!\(\*SubscriptBox[StyleBox[\"\[Psi]\", \"TI\"], StyleBox[RowBox[{\"c\", \" \", \"1\"}], \"TR\"]]\), \!\(\*SubscriptBox[StyleBox[\"\[Psi]\", \"TI\"], StyleBox[RowBox[{\"c\", \" \", \"2\"}], \"TR\"]]\), \!\(\*StyleBox[\"\\\"\[Ellipsis]\\\"\", \"TR\"]\), turn ratios \!\(\*SubscriptBox[StyleBox[\"i\", \"TI\"], StyleBox[RowBox[{\"\[Chi]\", \" \", \"1\"}], \"TR\"]]\), \!\(\*SubscriptBox[StyleBox[\"i\", \"TI\"], StyleBox[RowBox[{\"\[Chi]\", \" \", \"2\"}], \"TR\"]]\), \!\(\*StyleBox[\"\\\"\[Ellipsis]\\\"\", \"TR\"]\), radius \!\(\*SubscriptBox[StyleBox[\"\[Rho]\", \"TI\"], StyleBox[\"c\", \"TI\"]]\) and target field harmonic of order \!\(\*StyleBox[\"n\", \"TI\"]\) and degree \!\(\*StyleBox[\"m\", \"TI\"]\)."}]],"Output",CellTags->"EllipseCoilPlot3D_usages"],Cell[BoxData[RowBox[{" is an alternative way of specifying the axial separations \!\(\*SubscriptBox[StyleBox[\"\[Chi]c\", \"TI\"], StyleBox[\"1\", \"TR\"]]\), \!\(\*SubscriptBox[StyleBox[\"\[Chi]c\", \"TI\"], StyleBox[\"2\", \"TR\"]]\), \!\(\*StyleBox[\"\\\"\[Ellipsis]\\\"\", \"TR\"]\) and ellipse extents \!\(\*SubscriptBox[StyleBox[\"\[Psi]\", \"TI\"], StyleBox[RowBox[{\"c\", \" \", \"1\"}], \"TR\"]]\), \!\(\*SubscriptBox[StyleBox[\"\[Psi]\", \"TI\"], StyleBox[RowBox[{\"c\", \" \", \"2\"}], \"TR\"]]\), \!\(\*StyleBox[\"\\\"\[Ellipsis]\\\"\", \"TR\"]\)."}]],"Output",CellTags->"EllipseCoilPlot3D_usages"]}}]];


LoopFieldPlot::usage =
"\!\(\*RowBox[{\"LoopFieldPlot\", \"[\", RowBox[{RowBox[{\"{\", RowBox[{SubscriptBox[StyleBox[\"\[Chi]\", \"TI\"], StyleBox[RowBox[{\"c\", \" \", \"1\"}], \"TR\"]], \",\", SubscriptBox[StyleBox[\"\[Chi]\", \"TI\"], StyleBox[RowBox[{\"c\", \" \", \"2\"}], \"TR\"]], \",\", StyleBox[\"\\\"\[Ellipsis]\\\"\", \"TR\"]}], \"}\"}], \",\", RowBox[{\"{\", RowBox[{SubscriptBox[StyleBox[\"i\", \"TI\"], StyleBox[RowBox[{\"\[Chi]\", \" \", \"1\"}], \"TR\"]], \",\", SubscriptBox[StyleBox[\"i\", \"TI\"], StyleBox[RowBox[{\"\[Chi]\", \" \", \"2\"}], \"TR\"]], \",\", StyleBox[\"\\\"\[Ellipsis]\\\"\", \"TR\"]}], \"}\"}], \",\", SubscriptBox[StyleBox[\"\[Rho]\", \"TI\"], StyleBox[\"c\", \"TI\"]], \",\", StyleBox[\"n\", \"TI\"]}], \"]\"}]\) plots the \!\(\*SubscriptBox[StyleBox[\"B\", \"TI\"], StyleBox[\"x\", \"TI\"]]\), \!\(\*SubscriptBox[StyleBox[\"B\", \"TI\"], StyleBox[\"y\", \"TI\"]]\) and \!\(\*SubscriptBox[StyleBox[\"B\", \"TI\"], StyleBox[\"z\", \"TI\"]]\) field components along each of the \!\(\*StyleBox[\"x\", \"TI\"]\)\[Hyphen], \!\(\*StyleBox[\"y\", \"TI\"]\)\[Hyphen] and \!\(\*StyleBox[\"z\", \"TI\"]\)\[Hyphen]coil axes, generated by the loop\[Hyphen]based coil with axial separations \!\(\*SubscriptBox[StyleBox[\"\[Chi]\", \"TI\"], StyleBox[RowBox[{\"c\", \" \", \"1\"}], \"TR\"]]\), \!\(\*SubscriptBox[StyleBox[\"\[Chi]\", \"TI\"], StyleBox[RowBox[{\"c\", \" \", \"2\"}], \"TR\"]]\), \!\(\*StyleBox[\"\\\"\[Ellipsis]\\\"\", \"TR\"]\), turn ratios \!\(\*SubscriptBox[StyleBox[\"i\", \"TI\"], StyleBox[RowBox[{\"\[Chi]\", \" \", \"1\"}], \"TR\"]]\), \!\(\*SubscriptBox[StyleBox[\"i\", \"TI\"], StyleBox[RowBox[{\"\[Chi]\", \" \", \"2\"}], \"TR\"]]\), \!\(\*StyleBox[\"\\\"\[Ellipsis]\\\"\", \"TR\"]\), radius \!\(\*SubscriptBox[StyleBox[\"\[Rho]\", \"TI\"], StyleBox[\"c\", \"TI\"]]\) and target field harmonic of order \!\(\*StyleBox[\"n\", \"TI\"]\).
\!\(\*RowBox[{\"LoopFieldPlot\", \"[\", RowBox[{RowBox[{\"{\", RowBox[{RowBox[{RowBox[{\"Coil\[Chi]c\", \"[\", \"1\", \"]\"}], \"\[Rule]\", SubscriptBox[StyleBox[\"\[Chi]\", \"TI\"], StyleBox[RowBox[{\"c\", \" \", \"1\"}], \"TR\"]]}], \",\", RowBox[{RowBox[{\"Coil\[Chi]c\", \"[\", \"2\", \"]\"}], \"\[Rule]\", SubscriptBox[StyleBox[\"\[Chi]\", \"TI\"], StyleBox[RowBox[{\"c\", \" \", \"2\"}], \"TR\"]]}], \",\", StyleBox[\"\\\"\[Ellipsis]\\\"\", \"TR\"]}], \"}\"}], \",\", StyleBox[\"\\\"\[Ellipsis]\\\"\", \"TR\"]}], \"]\"}]\) is an alternative way of specifying the axial separations \!\(\*SubscriptBox[StyleBox[\"\[Chi]\", \"TI\"], StyleBox[RowBox[{\"c\", \" \", \"1\"}], \"TR\"]]\), \!\(\*SubscriptBox[StyleBox[\"\[Chi]\", \"TI\"], StyleBox[RowBox[{\"c\", \" \", \"2\"}], \"TR\"]]\), \!\(\*StyleBox[\"\\\"\[Ellipsis]\\\"\", \"TR\"]\).";

MathLink`CallFrontEnd[FrontEnd`CacheTemplateAndUsagePacket[FrontEnd`InputNotebook[],"LoopFieldPlot",{{Cell[BoxData[GridBox[{{RowBox[{"LoopFieldPlot","[",RowBox[{RowBox[{"{",RowBox[{SubscriptBox[StyleBox["\[Chi]","TI"],StyleBox[RowBox[{"c"," ","1"}],"TR"]],",",SubscriptBox[StyleBox["\[Chi]","TI"],StyleBox[RowBox[{"c"," ","2"}],"TR"]],",",StyleBox["\"\[Ellipsis]\"","TR"]}],"}"}],",",RowBox[{"{",RowBox[{SubscriptBox[StyleBox["i","TI"],StyleBox[RowBox[{"\[Chi]"," ","1"}],"TR"]],",",SubscriptBox[StyleBox["i","TI"],StyleBox[RowBox[{"\[Chi]"," ","2"}],"TR"]],",",StyleBox["\"\[Ellipsis]\"","TR"]}],"}"}],",",SubscriptBox[StyleBox["\[Rho]","TI"],StyleBox["c","TI"]],",",StyleBox["n","TI"]}],"]"}]}}]],"InlineFormula",CellTags->"LoopFieldPlot_templates"],Cell[BoxData[GridBox[{{RowBox[{"LoopFieldPlot","[",RowBox[{RowBox[{"{",RowBox[{RowBox[{RowBox[{"Coil\[Chi]c","[","1","]"}],"->",SubscriptBox[StyleBox["\[Chi]","TI"],StyleBox[RowBox[{"c"," ","1"}],"TR"]]}],",",RowBox[{RowBox[{"Coil\[Chi]c","[","2","]"}],"->",SubscriptBox[StyleBox["\[Chi]","TI"],StyleBox[RowBox[{"c"," ","2"}],"TR"]]}],",",StyleBox["\"\[Ellipsis]\"","TR"]}],"}"}],",",StyleBox["\"\[Ellipsis]\"","TR"]}],"]"}]}}]],"InlineFormula",CellTags->"LoopFieldPlot_templates"]},{Cell[BoxData[RowBox[{" plots the \!\(\*SubscriptBox[StyleBox[\"B\", \"TI\"], StyleBox[\"x\", \"TI\"]]\), \!\(\*SubscriptBox[StyleBox[\"B\", \"TI\"], StyleBox[\"y\", \"TI\"]]\) and \!\(\*SubscriptBox[StyleBox[\"B\", \"TI\"], StyleBox[\"z\", \"TI\"]]\) field components along each of the \!\(\*StyleBox[\"x\", \"TI\"]\)-, \!\(\*StyleBox[\"y\", \"TI\"]\)- and \!\(\*StyleBox[\"z\", \"TI\"]\)-coil axes, generated by the loop-based coil with axial separations \!\(\*SubscriptBox[StyleBox[\"\[Chi]\", \"TI\"], StyleBox[RowBox[{\"c\", \" \", \"1\"}], \"TR\"]]\), \!\(\*SubscriptBox[StyleBox[\"\[Chi]\", \"TI\"], StyleBox[RowBox[{\"c\", \" \", \"2\"}], \"TR\"]]\), \!\(\*StyleBox[\"\\\"\[Ellipsis]\\\"\", \"TR\"]\), turn ratios \!\(\*SubscriptBox[StyleBox[\"i\", \"TI\"], StyleBox[RowBox[{\"\[Chi]\", \" \", \"1\"}], \"TR\"]]\), \!\(\*SubscriptBox[StyleBox[\"i\", \"TI\"], StyleBox[RowBox[{\"\[Chi]\", \" \", \"2\"}], \"TR\"]]\), \!\(\*StyleBox[\"\\\"\[Ellipsis]\\\"\", \"TR\"]\), radius \!\(\*SubscriptBox[StyleBox[\"\[Rho]\", \"TI\"], StyleBox[\"c\", \"TI\"]]\) and target field harmonic of order \!\(\*StyleBox[\"n\", \"TI\"]\)."}]],"Output",CellTags->"LoopFieldPlot_usages"],Cell[BoxData[RowBox[{" is an alternative way of specifying the axial separations \!\(\*SubscriptBox[StyleBox[\"\[Chi]\", \"TI\"], StyleBox[RowBox[{\"c\", \" \", \"1\"}], \"TR\"]]\), \!\(\*SubscriptBox[StyleBox[\"\[Chi]\", \"TI\"], StyleBox[RowBox[{\"c\", \" \", \"2\"}], \"TR\"]]\), \!\(\*StyleBox[\"\\\"\[Ellipsis]\\\"\", \"TR\"]\)."}]],"Output",CellTags->"LoopFieldPlot_usages"]}}]]


SaddleFieldPlot::usage =
"\!\(\*RowBox[{\"SaddleFieldPlot\", \"[\", RowBox[{RowBox[{\"{\", RowBox[{SubscriptBox[StyleBox[\"\[Chi]\", \"TI\"], StyleBox[RowBox[{\"c\", \" \", \"1\"}], \"TR\"]], \",\", SubscriptBox[StyleBox[\"\[Chi]\", \"TI\"], StyleBox[RowBox[{\"c\", \" \", \"2\"}], \"TR\"]], \",\", StyleBox[\"\\\"\[Ellipsis]\\\"\", \"TR\"]}], \"}\"}], \",\", RowBox[{\"{\", RowBox[{SubscriptBox[StyleBox[\"\[Phi]\", \"TI\"], StyleBox[RowBox[{\"c\", \" \", \"1\"}], \"TR\"]], \",\", SubscriptBox[StyleBox[\"\[Phi]\", \"TI\"], StyleBox[RowBox[{\"c\", \" \", \"2\"}], \"TR\"]], \",\", StyleBox[\"\\\"\[Ellipsis]\\\"\", \"TR\"]}], \"}\"}], \",\", RowBox[{\"{\", RowBox[{SubscriptBox[StyleBox[\"i\", \"TI\"], StyleBox[RowBox[{\"\[Chi]\", \" \", \"1\"}], \"TR\"]], \",\", SubscriptBox[StyleBox[\"i\", \"TI\"], StyleBox[RowBox[{\"\[Chi]\", \" \", \"2\"}], \"TR\"]], \",\", StyleBox[\"\\\"\[Ellipsis]\\\"\", \"TR\"]}], \"}\"}], \",\", SubscriptBox[StyleBox[\"\[Rho]\", \"TI\"], StyleBox[\"c\", \"TI\"]], \",\", RowBox[{\"{\", RowBox[{StyleBox[\"n\", \"TI\"], \",\", StyleBox[\"m\", \"TI\"]}], \"}\"}]}], \"]\"}]\) plots the \!\(\*SubscriptBox[StyleBox[\"B\", \"TI\"], StyleBox[\"x\", \"TI\"]]\), \!\(\*SubscriptBox[StyleBox[\"B\", \"TI\"], StyleBox[\"y\", \"TI\"]]\) and \!\(\*SubscriptBox[StyleBox[\"B\", \"TI\"], StyleBox[\"z\", \"TI\"]]\) field components along each of the \!\(\*StyleBox[\"x\", \"TI\"]\)\[Hyphen], \!\(\*StyleBox[\"y\", \"TI\"]\)\[Hyphen] and \!\(\*StyleBox[\"z\", \"TI\"]\)\[Hyphen]coil axes, generated by the saddle\[Hyphen]based coil with axial separations \!\(\*SubscriptBox[StyleBox[\"\[Chi]\", \"TI\"], StyleBox[RowBox[{\"c\", \" \", \"1\"}], \"TR\"]]\), \!\(\*SubscriptBox[StyleBox[\"\[Chi]\", \"TI\"], StyleBox[RowBox[{\"c\", \" \", \"2\"}], \"TR\"]]\), \!\(\*StyleBox[\"\\\"\[Ellipsis]\\\"\", \"TR\"]\), azimuthal extents \!\(\*SubscriptBox[StyleBox[\"\[Phi]\", \"TI\"], StyleBox[RowBox[{\"c\", \" \", \"1\"}], \"TR\"]]\), \!\(\*SubscriptBox[StyleBox[\"\[Phi]\", \"TI\"], StyleBox[RowBox[{\"c\", \" \", \"2\"}], \"TR\"]]\), \!\(\*StyleBox[\"\\\"\[Ellipsis]\\\"\", \"TR\"]\), turn ratios \!\(\*SubscriptBox[StyleBox[\"i\", \"TI\"], StyleBox[RowBox[{\"\[Chi]\", \" \", \"1\"}], \"TR\"]]\), \!\(\*SubscriptBox[StyleBox[\"i\", \"TI\"], StyleBox[RowBox[{\"\[Chi]\", \" \", \"2\"}], \"TR\"]]\), \!\(\*StyleBox[\"\\\"\[Ellipsis]\\\"\", \"TR\"]\), radius \!\(\*SubscriptBox[StyleBox[\"\[Rho]\", \"TI\"], StyleBox[\"c\", \"TI\"]]\) and target field harmonic of order \!\(\*StyleBox[\"n\", \"TI\"]\) and degree \!\(\*StyleBox[\"m\", \"TI\"]\).
\!\(\*RowBox[{\"SaddleFieldPlot\", \"[\", RowBox[{RowBox[{\"{\", RowBox[{RowBox[{RowBox[{\"Coil\[Chi]c\", \"[\", \"1\", \"]\"}], \"\[Rule]\", SubscriptBox[StyleBox[\"\[Chi]\", \"TI\"], StyleBox[RowBox[{\"c\", \" \", \"1\"}], \"TR\"]]}], \",\", RowBox[{RowBox[{\"Coil\[Chi]c\", \"[\", \"2\", \"]\"}], \"\[Rule]\", SubscriptBox[StyleBox[\"\[Chi]\", \"TI\"], StyleBox[RowBox[{\"c\", \" \", \"2\"}], \"TR\"]]}], \",\", StyleBox[\"\\\"\[Ellipsis]\\\"\", \"TR\"]}], \"}\"}], \",\", RowBox[{\"{\", RowBox[{RowBox[{RowBox[{\"Coil\[Phi]c\", \"[\", \"1\", \"]\"}], \"\[Rule]\", SubscriptBox[StyleBox[\"\[Phi]\", \"TI\"], StyleBox[RowBox[{\"c\", \" \", \"1\"}], \"TR\"]]}], \",\", RowBox[{RowBox[{\"Coil\[Phi]c\", \"[\", \"2\", \"]\"}], \"\[Rule]\", SubscriptBox[StyleBox[\"\[Phi]\", \"TI\"], StyleBox[RowBox[{\"c\", \" \", \"2\"}], \"TR\"]]}], \",\", StyleBox[\"\\\"\[Ellipsis]\\\"\", \"TR\"]}], \"}\"}], \",\", StyleBox[\"\\\"\[Ellipsis]\\\"\", \"TR\"]}], \"]\"}]\) is an alternative way of specifying the axial separations \!\(\*SubscriptBox[StyleBox[\"\[Chi]c\", \"TI\"], StyleBox[\"1\", \"TR\"]]\), \!\(\*SubscriptBox[StyleBox[\"\[Chi]c\", \"TI\"], StyleBox[\"2\", \"TR\"]]\), \!\(\*StyleBox[\"\\\"\[Ellipsis]\\\"\", \"TR\"]\) and azimuthal extents \!\(\*SubscriptBox[StyleBox[\"\[Phi]\", \"TI\"], StyleBox[RowBox[{\"c\", \" \", \"1\"}], \"TR\"]]\), \!\(\*SubscriptBox[StyleBox[\"\[Phi]\", \"TI\"], StyleBox[RowBox[{\"c\", \" \", \"2\"}], \"TR\"]]\), \!\(\*StyleBox[\"\\\"\[Ellipsis]\\\"\", \"TR\"]\).";

MathLink`CallFrontEnd[FrontEnd`CacheTemplateAndUsagePacket[FrontEnd`InputNotebook[],"SaddleFieldPlot",{{Cell[BoxData[GridBox[{{RowBox[{"SaddleFieldPlot","[",RowBox[{RowBox[{"{",RowBox[{SubscriptBox[StyleBox["\[Chi]","TI"],StyleBox[RowBox[{"c"," ","1"}],"TR"]],",",SubscriptBox[StyleBox["\[Chi]","TI"],StyleBox[RowBox[{"c"," ","2"}],"TR"]],",",StyleBox["\"\[Ellipsis]\"","TR"]}],"}"}],",",RowBox[{"{",RowBox[{SubscriptBox[StyleBox["\[Phi]","TI"],StyleBox[RowBox[{"c"," ","1"}],"TR"]],",",SubscriptBox[StyleBox["\[Phi]","TI"],StyleBox[RowBox[{"c"," ","2"}],"TR"]],",",StyleBox["\"\[Ellipsis]\"","TR"]}],"}"}],",",RowBox[{"{",RowBox[{SubscriptBox[StyleBox["i","TI"],StyleBox[RowBox[{"\[Chi]"," ","1"}],"TR"]],",",SubscriptBox[StyleBox["i","TI"],StyleBox[RowBox[{"\[Chi]"," ","2"}],"TR"]],",",StyleBox["\"\[Ellipsis]\"","TR"]}],"}"}],",",SubscriptBox[StyleBox["\[Rho]","TI"],StyleBox["c","TI"]],",",RowBox[{"{",RowBox[{StyleBox["n","TI"],",",StyleBox["m","TI"]}],"}"}]}],"]"}]}}]],"InlineFormula",CellTags->"SaddleFieldPlot_templates"],Cell[BoxData[GridBox[{{RowBox[{"SaddleFieldPlot","[",RowBox[{RowBox[{"{",RowBox[{RowBox[{RowBox[{"Coil\[Chi]c","[","1","]"}],"->",SubscriptBox[StyleBox["\[Chi]","TI"],StyleBox[RowBox[{"c"," ","1"}],"TR"]]}],",",RowBox[{RowBox[{"Coil\[Chi]c","[","2","]"}],"->",SubscriptBox[StyleBox["\[Chi]","TI"],StyleBox[RowBox[{"c"," ","2"}],"TR"]]}],",",StyleBox["\"\[Ellipsis]\"","TR"]}],"}"}],",",RowBox[{"{",RowBox[{RowBox[{RowBox[{"Coil\[Phi]c","[","1","]"}],"->",SubscriptBox[StyleBox["\[Phi]","TI"],StyleBox[RowBox[{"c"," ","1"}],"TR"]]}],",",RowBox[{RowBox[{"Coil\[Phi]c","[","2","]"}],"->",SubscriptBox[StyleBox["\[Phi]","TI"],StyleBox[RowBox[{"c"," ","2"}],"TR"]]}],",",StyleBox["\"\[Ellipsis]\"","TR"]}],"}"}],",",StyleBox["\"\[Ellipsis]\"","TR"]}],"]"}]}}]],"InlineFormula",CellTags->"SaddleFieldPlot_templates"]},{Cell[BoxData[RowBox[{" plots the \!\(\*SubscriptBox[StyleBox[\"B\", \"TI\"], StyleBox[\"x\", \"TI\"]]\), \!\(\*SubscriptBox[StyleBox[\"B\", \"TI\"], StyleBox[\"y\", \"TI\"]]\) and \!\(\*SubscriptBox[StyleBox[\"B\", \"TI\"], StyleBox[\"z\", \"TI\"]]\) field components along each of the \!\(\*StyleBox[\"x\", \"TI\"]\)-, \!\(\*StyleBox[\"y\", \"TI\"]\)- and \!\(\*StyleBox[\"z\", \"TI\"]\)-coil axes, generated by the saddle-based coil with axial separations \!\(\*SubscriptBox[StyleBox[\"\[Chi]\", \"TI\"], StyleBox[RowBox[{\"c\", \" \", \"1\"}], \"TR\"]]\), \!\(\*SubscriptBox[StyleBox[\"\[Chi]\", \"TI\"], StyleBox[RowBox[{\"c\", \" \", \"2\"}], \"TR\"]]\), \!\(\*StyleBox[\"\\\"\[Ellipsis]\\\"\", \"TR\"]\), azimuthal extents \!\(\*SubscriptBox[StyleBox[\"\[Phi]\", \"TI\"], StyleBox[RowBox[{\"c\", \" \", \"1\"}], \"TR\"]]\), \!\(\*SubscriptBox[StyleBox[\"\[Phi]\", \"TI\"], StyleBox[RowBox[{\"c\", \" \", \"2\"}], \"TR\"]]\), \!\(\*StyleBox[\"\\\"\[Ellipsis]\\\"\", \"TR\"]\), turn ratios \!\(\*SubscriptBox[StyleBox[\"i\", \"TI\"], StyleBox[RowBox[{\"\[Chi]\", \" \", \"1\"}], \"TR\"]]\), \!\(\*SubscriptBox[StyleBox[\"i\", \"TI\"], StyleBox[RowBox[{\"\[Chi]\", \" \", \"2\"}], \"TR\"]]\), \!\(\*StyleBox[\"\\\"\[Ellipsis]\\\"\", \"TR\"]\), radius \!\(\*SubscriptBox[StyleBox[\"\[Rho]\", \"TI\"], StyleBox[\"c\", \"TI\"]]\) and target field harmonic of order \!\(\*StyleBox[\"n\", \"TI\"]\) and degree \!\(\*StyleBox[\"m\", \"TI\"]\)."}]],"Output",CellTags->"SaddleFieldPlot_usages"],Cell[BoxData[RowBox[{" is an alternative way of specifying the axial separations \!\(\*SubscriptBox[StyleBox[\"\[Chi]c\", \"TI\"], StyleBox[\"1\", \"TR\"]]\), \!\(\*SubscriptBox[StyleBox[\"\[Chi]c\", \"TI\"], StyleBox[\"2\", \"TR\"]]\), \!\(\*StyleBox[\"\\\"\[Ellipsis]\\\"\", \"TR\"]\) and azimuthal extents \!\(\*SubscriptBox[StyleBox[\"\[Phi]\", \"TI\"], StyleBox[RowBox[{\"c\", \" \", \"1\"}], \"TR\"]]\), \!\(\*SubscriptBox[StyleBox[\"\[Phi]\", \"TI\"], StyleBox[RowBox[{\"c\", \" \", \"2\"}], \"TR\"]]\), \!\(\*StyleBox[\"\\\"\[Ellipsis]\\\"\", \"TR\"]\)."}]],"Output",CellTags->"SaddleFieldPlot_usages"]}}]]


EllipseFieldPlot::usage =
"\!\(\*RowBox[{\"EllipseFieldPlot\", \"[\", RowBox[{RowBox[{\"{\", RowBox[{RowBox[{\"{\", RowBox[{SubscriptBox[StyleBox[\"\[Chi]\", \"TI\"], StyleBox[RowBox[{\"c\", \" \", \"1\"}], \"TR\"]], \",\", SubscriptBox[StyleBox[\"\[Psi]\", \"TI\"], StyleBox[RowBox[{\"c\", \" \", \"1\"}], \"TR\"]]}], \"}\"}], \",\", RowBox[{\"{\", RowBox[{SubscriptBox[StyleBox[\"\[Chi]\", \"TI\"], StyleBox[RowBox[{\"c\", \" \", \"2\"}], \"TR\"]], \",\", SubscriptBox[StyleBox[\"\[Psi]\", \"TI\"], StyleBox[RowBox[{\"c\", \" \", \"2\"}], \"TR\"]]}], \"}\"}], \",\", StyleBox[\"\\\"\[Ellipsis]\\\"\", \"TR\"]}], \"}\"}], \",\", RowBox[{\"{\", RowBox[{SubscriptBox[StyleBox[\"i\", \"TI\"], StyleBox[RowBox[{\"\[Chi]\", \" \", \"1\"}], \"TR\"]], \",\", SubscriptBox[StyleBox[\"i\", \"TI\"], StyleBox[RowBox[{\"\[Chi]\", \" \", \"2\"}], \"TR\"]], \",\", StyleBox[\"\\\"\[Ellipsis]\\\"\", \"TR\"]}], \"}\"}], \",\", SubscriptBox[StyleBox[\"\[Rho]\", \"TI\"], StyleBox[\"c\", \"TI\"]], \",\", RowBox[{\"{\", RowBox[{StyleBox[\"n\", \"TI\"], \",\", StyleBox[\"m\", \"TI\"]}], \"}\"}]}], \"]\"}]\) plots the \!\(\*SubscriptBox[StyleBox[\"B\", \"TI\"], StyleBox[\"x\", \"TI\"]]\), \!\(\*SubscriptBox[StyleBox[\"B\", \"TI\"], StyleBox[\"y\", \"TI\"]]\) and \!\(\*SubscriptBox[StyleBox[\"B\", \"TI\"], StyleBox[\"z\", \"TI\"]]\) field components along each of the \!\(\*StyleBox[\"x\", \"TI\"]\)\[Hyphen], \!\(\*StyleBox[\"y\", \"TI\"]\)\[Hyphen] and \!\(\*StyleBox[\"z\", \"TI\"]\)\[Hyphen]coil axes, generated by the ellipse\[Hyphen]based coil with axial separations \!\(\*SubscriptBox[StyleBox[\"\[Chi]\", \"TI\"], StyleBox[RowBox[{\"c\", \" \", \"1\"}], \"TR\"]]\), \!\(\*SubscriptBox[StyleBox[\"\[Chi]\", \"TI\"], StyleBox[RowBox[{\"c\", \" \", \"2\"}], \"TR\"]]\), \!\(\*StyleBox[\"\\\"\[Ellipsis]\\\"\", \"TR\"]\), ellipse extents \!\(\*SubscriptBox[StyleBox[\"\[Psi]\", \"TI\"], StyleBox[RowBox[{\"c\", \" \", \"1\"}], \"TR\"]]\), \!\(\*SubscriptBox[StyleBox[\"\[Psi]\", \"TI\"], StyleBox[RowBox[{\"c\", \" \", \"2\"}], \"TR\"]]\), \!\(\*StyleBox[\"\\\"\[Ellipsis]\\\"\", \"TR\"]\), turn ratios \!\(\*SubscriptBox[StyleBox[\"i\", \"TI\"], StyleBox[RowBox[{\"\[Chi]\", \" \", \"1\"}], \"TR\"]]\), \!\(\*SubscriptBox[StyleBox[\"i\", \"TI\"], StyleBox[RowBox[{\"\[Chi]\", \" \", \"2\"}], \"TR\"]]\), \!\(\*StyleBox[\"\\\"\[Ellipsis]\\\"\", \"TR\"]\), radius \!\(\*SubscriptBox[StyleBox[\"\[Rho]\", \"TI\"], StyleBox[\"c\", \"TI\"]]\) and target field harmonic of order \!\(\*StyleBox[\"n\", \"TI\"]\) and degree \!\(\*StyleBox[\"m\", \"TI\"]\).
\!\(\*RowBox[{\"EllipseFieldPlot\", \"[\", RowBox[{RowBox[{\"{\", RowBox[{RowBox[{RowBox[{\"Coil\[Chi]c\", \"[\", \"1\", \"]\"}], \"\[Rule]\", SubscriptBox[StyleBox[\"\[Chi]\", \"TI\"], StyleBox[RowBox[{\"c\", \" \", \"1\"}], \"TR\"]]}], \",\", RowBox[{RowBox[{\"Coil\[Chi]c\", \"[\", \"2\", \"]\"}], \"\[Rule]\", SubscriptBox[StyleBox[\"\[Chi]\", \"TI\"], StyleBox[RowBox[{\"c\", \" \", \"2\"}], \"TR\"]]}], \",\", StyleBox[\"\\\"\[Ellipsis]\\\"\", \"TR\"], \",\", RowBox[{RowBox[{\"Coil\[Psi]c\", \"[\", \"1\", \"]\"}], \"\[Rule]\", SubscriptBox[StyleBox[\"\[Psi]\", \"TI\"], StyleBox[RowBox[{\"c\", \" \", \"1\"}], \"TR\"]]}], \",\", RowBox[{RowBox[{\"Coil\[Psi]c\", \"[\", \"2\", \"]\"}], \"\[Rule]\", SubscriptBox[StyleBox[\"\[Psi]\", \"TI\"], StyleBox[RowBox[{\"c\", \" \", \"2\"}], \"TR\"]]}], \",\", StyleBox[\"\\\"\[Ellipsis]\\\"\", \"TR\"]}], \"}\"}], \",\", StyleBox[\"\\\"\[Ellipsis]\\\"\", \"TR\"]}], \"]\"}]\) is an alternative way of specifying the axial separations \!\(\*SubscriptBox[StyleBox[\"\[Chi]c\", \"TI\"], StyleBox[\"1\", \"TR\"]]\), \!\(\*SubscriptBox[StyleBox[\"\[Chi]c\", \"TI\"], StyleBox[\"2\", \"TR\"]]\), \!\(\*StyleBox[\"\\\"\[Ellipsis]\\\"\", \"TR\"]\) and ellipse extents \!\(\*SubscriptBox[StyleBox[\"\[Psi]\", \"TI\"], StyleBox[RowBox[{\"c\", \" \", \"1\"}], \"TR\"]]\), \!\(\*SubscriptBox[StyleBox[\"\[Psi]\", \"TI\"], StyleBox[RowBox[{\"c\", \" \", \"2\"}], \"TR\"]]\), \!\(\*StyleBox[\"\\\"\[Ellipsis]\\\"\", \"TR\"]\).";

MathLink`CallFrontEnd[FrontEnd`CacheTemplateAndUsagePacket[FrontEnd`InputNotebook[],"EllipseFieldPlot",{{Cell[BoxData[GridBox[{{RowBox[{"EllipseFieldPlot","[",RowBox[{RowBox[{"{",RowBox[{RowBox[{"{",RowBox[{SubscriptBox[StyleBox["\[Chi]","TI"],StyleBox[RowBox[{"c"," ","1"}],"TR"]],",",SubscriptBox[StyleBox["\[Psi]","TI"],StyleBox[RowBox[{"c"," ","1"}],"TR"]]}],"}"}],",",RowBox[{"{",RowBox[{SubscriptBox[StyleBox["\[Chi]","TI"],StyleBox[RowBox[{"c"," ","2"}],"TR"]],",",SubscriptBox[StyleBox["\[Psi]","TI"],StyleBox[RowBox[{"c"," ","2"}],"TR"]]}],"}"}],",",StyleBox["\"\[Ellipsis]\"","TR"]}],"}"}],",",RowBox[{"{",RowBox[{SubscriptBox[StyleBox["i","TI"],StyleBox[RowBox[{"\[Chi]"," ","1"}],"TR"]],",",SubscriptBox[StyleBox["i","TI"],StyleBox[RowBox[{"\[Chi]"," ","2"}],"TR"]],",",StyleBox["\"\[Ellipsis]\"","TR"]}],"}"}],",",SubscriptBox[StyleBox["\[Rho]","TI"],StyleBox["c","TI"]],",",RowBox[{"{",RowBox[{StyleBox["n","TI"],",",StyleBox["m","TI"]}],"}"}]}],"]"}]}}]],"InlineFormula",CellTags->"EllipseFieldPlot_templates"],Cell[BoxData[GridBox[{{RowBox[{"EllipseFieldPlot","[",RowBox[{RowBox[{"{",RowBox[{RowBox[{RowBox[{"Coil\[Chi]c","[","1","]"}],"->",SubscriptBox[StyleBox["\[Chi]","TI"],StyleBox[RowBox[{"c"," ","1"}],"TR"]]}],",",RowBox[{RowBox[{"Coil\[Chi]c","[","2","]"}],"->",SubscriptBox[StyleBox["\[Chi]","TI"],StyleBox[RowBox[{"c"," ","2"}],"TR"]]}],",",StyleBox["\"\[Ellipsis]\"","TR"],",",RowBox[{RowBox[{"Coil\[Psi]c","[","1","]"}],"->",SubscriptBox[StyleBox["\[Psi]","TI"],StyleBox[RowBox[{"c"," ","1"}],"TR"]]}],",",RowBox[{RowBox[{"Coil\[Psi]c","[","2","]"}],"->",SubscriptBox[StyleBox["\[Psi]","TI"],StyleBox[RowBox[{"c"," ","2"}],"TR"]]}],",",StyleBox["\"\[Ellipsis]\"","TR"]}],"}"}],",",StyleBox["\"\[Ellipsis]\"","TR"]}],"]"}]}}]],"InlineFormula",CellTags->"EllipseFieldPlot_templates"]},{Cell[BoxData[RowBox[{" plots the \!\(\*SubscriptBox[StyleBox[\"B\", \"TI\"], StyleBox[\"x\", \"TI\"]]\), \!\(\*SubscriptBox[StyleBox[\"B\", \"TI\"], StyleBox[\"y\", \"TI\"]]\) and \!\(\*SubscriptBox[StyleBox[\"B\", \"TI\"], StyleBox[\"z\", \"TI\"]]\) field components along each of the \!\(\*StyleBox[\"x\", \"TI\"]\)-, \!\(\*StyleBox[\"y\", \"TI\"]\)- and \!\(\*StyleBox[\"z\", \"TI\"]\)-coil axes, generated by the ellipse-based coil with axial separations \!\(\*SubscriptBox[StyleBox[\"\[Chi]\", \"TI\"], StyleBox[RowBox[{\"c\", \" \", \"1\"}], \"TR\"]]\), \!\(\*SubscriptBox[StyleBox[\"\[Chi]\", \"TI\"], StyleBox[RowBox[{\"c\", \" \", \"2\"}], \"TR\"]]\), \!\(\*StyleBox[\"\\\"\[Ellipsis]\\\"\", \"TR\"]\), ellipse extents \!\(\*SubscriptBox[StyleBox[\"\[Psi]\", \"TI\"], StyleBox[RowBox[{\"c\", \" \", \"1\"}], \"TR\"]]\), \!\(\*SubscriptBox[StyleBox[\"\[Psi]\", \"TI\"], StyleBox[RowBox[{\"c\", \" \", \"2\"}], \"TR\"]]\), \!\(\*StyleBox[\"\\\"\[Ellipsis]\\\"\", \"TR\"]\), turn ratios \!\(\*SubscriptBox[StyleBox[\"i\", \"TI\"], StyleBox[RowBox[{\"\[Chi]\", \" \", \"1\"}], \"TR\"]]\), \!\(\*SubscriptBox[StyleBox[\"i\", \"TI\"], StyleBox[RowBox[{\"\[Chi]\", \" \", \"2\"}], \"TR\"]]\), \!\(\*StyleBox[\"\\\"\[Ellipsis]\\\"\", \"TR\"]\), radius \!\(\*SubscriptBox[StyleBox[\"\[Rho]\", \"TI\"], StyleBox[\"c\", \"TI\"]]\) and target field harmonic of order \!\(\*StyleBox[\"n\", \"TI\"]\) and degree \!\(\*StyleBox[\"m\", \"TI\"]\)."}]],"Output",CellTags->"EllipseFieldPlot_usages"],Cell[BoxData[RowBox[{" is an alternative way of specifying the axial separations \!\(\*SubscriptBox[StyleBox[\"\[Chi]c\", \"TI\"], StyleBox[\"1\", \"TR\"]]\), \!\(\*SubscriptBox[StyleBox[\"\[Chi]c\", \"TI\"], StyleBox[\"2\", \"TR\"]]\), \!\(\*StyleBox[\"\\\"\[Ellipsis]\\\"\", \"TR\"]\) and ellipse extents \!\(\*SubscriptBox[StyleBox[\"\[Psi]\", \"TI\"], StyleBox[RowBox[{\"c\", \" \", \"1\"}], \"TR\"]]\), \!\(\*SubscriptBox[StyleBox[\"\[Psi]\", \"TI\"], StyleBox[RowBox[{\"c\", \" \", \"2\"}], \"TR\"]]\), \!\(\*StyleBox[\"\\\"\[Ellipsis]\\\"\", \"TR\"]\)."}]],"Output",CellTags->"EllipseFieldPlot_usages"]}}]]


LoopFieldPlot2D::usage =
"\!\(\*RowBox[{\"LoopFieldPlot2D\", \"[\", RowBox[{RowBox[{\"{\", RowBox[{SubscriptBox[StyleBox[\"\[Chi]\", \"TI\"], StyleBox[RowBox[{\"c\", \" \", \"1\"}], \"TR\"]], \",\", SubscriptBox[StyleBox[\"\[Chi]\", \"TI\"], StyleBox[RowBox[{\"c\", \" \", \"2\"}], \"TR\"]], \",\", StyleBox[\"\\\"\[Ellipsis]\\\"\", \"TR\"]}], \"}\"}], \",\", RowBox[{\"{\", RowBox[{SubscriptBox[StyleBox[\"i\", \"TI\"], StyleBox[RowBox[{\"\[Chi]\", \" \", \"1\"}], \"TR\"]], \",\", SubscriptBox[StyleBox[\"i\", \"TI\"], StyleBox[RowBox[{\"\[Chi]\", \" \", \"2\"}], \"TR\"]], \",\", StyleBox[\"\\\"\[Ellipsis]\\\"\", \"TR\"]}], \"}\"}], \",\", SubscriptBox[StyleBox[\"\[Rho]\", \"TI\"], StyleBox[\"c\", \"TI\"]], \",\", StyleBox[\"n\", \"TI\"]}], \"]\"}]\) plots each of the \!\(\*SubscriptBox[StyleBox[\"B\", \"TI\"], StyleBox[\"x\", \"TI\"]]\), \!\(\*SubscriptBox[StyleBox[\"B\", \"TI\"], StyleBox[\"y\", \"TI\"]]\) and \!\(\*SubscriptBox[StyleBox[\"B\", \"TI\"], StyleBox[\"z\", \"TI\"]]\) field components over the \!\(\*StyleBox[\"x\", \"TI\"]\)\!\(\*StyleBox[\"z\", \"TI\"]\)\[Hyphen]plane, generated by the loop\[Hyphen]based coil with axial separations \!\(\*SubscriptBox[StyleBox[\"\[Chi]\", \"TI\"], StyleBox[RowBox[{\"c\", \" \", \"1\"}], \"TR\"]]\), \!\(\*SubscriptBox[StyleBox[\"\[Chi]\", \"TI\"], StyleBox[RowBox[{\"c\", \" \", \"2\"}], \"TR\"]]\), \!\(\*StyleBox[\"\\\"\[Ellipsis]\\\"\", \"TR\"]\), turn ratios \!\(\*SubscriptBox[StyleBox[\"i\", \"TI\"], StyleBox[RowBox[{\"\[Chi]\", \" \", \"1\"}], \"TR\"]]\), \!\(\*SubscriptBox[StyleBox[\"i\", \"TI\"], StyleBox[RowBox[{\"\[Chi]\", \" \", \"2\"}], \"TR\"]]\), \!\(\*StyleBox[\"\\\"\[Ellipsis]\\\"\", \"TR\"]\), radius \!\(\*SubscriptBox[StyleBox[\"\[Rho]\", \"TI\"], StyleBox[\"c\", \"TI\"]]\) and target field harmonic of order \!\(\*StyleBox[\"n\", \"TI\"]\).
\!\(\*RowBox[{\"LoopFieldPlot2D\", \"[\", RowBox[{RowBox[{\"{\", RowBox[{RowBox[{RowBox[{\"Coil\[Chi]c\", \"[\", \"1\", \"]\"}], \"\[Rule]\", SubscriptBox[StyleBox[\"\[Chi]\", \"TI\"], StyleBox[RowBox[{\"c\", \" \", \"1\"}], \"TR\"]]}], \",\", RowBox[{RowBox[{\"Coil\[Chi]c\", \"[\", \"2\", \"]\"}], \"\[Rule]\", SubscriptBox[StyleBox[\"\[Chi]\", \"TI\"], StyleBox[RowBox[{\"c\", \" \", \"2\"}], \"TR\"]]}], \",\", StyleBox[\"\\\"\[Ellipsis]\\\"\", \"TR\"]}], \"}\"}], \",\", StyleBox[\"\\\"\[Ellipsis]\\\"\", \"TR\"]}], \"]\"}]\) is an alternative way of specifying the axial separations \!\(\*SubscriptBox[StyleBox[\"\[Chi]\", \"TI\"], StyleBox[RowBox[{\"c\", \" \", \"1\"}], \"TR\"]]\), \!\(\*SubscriptBox[StyleBox[\"\[Chi]\", \"TI\"], StyleBox[RowBox[{\"c\", \" \", \"2\"}], \"TR\"]]\), \!\(\*StyleBox[\"\\\"\[Ellipsis]\\\"\", \"TR\"]\).";

MathLink`CallFrontEnd[FrontEnd`CacheTemplateAndUsagePacket[FrontEnd`InputNotebook[],"LoopFieldPlot2D",{{Cell[BoxData[GridBox[{{RowBox[{"LoopFieldPlot2D","[",RowBox[{RowBox[{"{",RowBox[{SubscriptBox[StyleBox["\[Chi]","TI"],StyleBox[RowBox[{"c"," ","1"}],"TR"]],",",SubscriptBox[StyleBox["\[Chi]","TI"],StyleBox[RowBox[{"c"," ","2"}],"TR"]],",",StyleBox["\"\[Ellipsis]\"","TR"]}],"}"}],",",RowBox[{"{",RowBox[{SubscriptBox[StyleBox["i","TI"],StyleBox[RowBox[{"\[Chi]"," ","1"}],"TR"]],",",SubscriptBox[StyleBox["i","TI"],StyleBox[RowBox[{"\[Chi]"," ","2"}],"TR"]],",",StyleBox["\"\[Ellipsis]\"","TR"]}],"}"}],",",SubscriptBox[StyleBox["\[Rho]","TI"],StyleBox["c","TI"]],",",StyleBox["n","TI"]}],"]"}]}}]],"InlineFormula",CellTags->"LoopFieldPlot2D_templates"],Cell[BoxData[GridBox[{{RowBox[{"LoopFieldPlot2D","[",RowBox[{RowBox[{"{",RowBox[{RowBox[{RowBox[{"Coil\[Chi]c","[","1","]"}],"->",SubscriptBox[StyleBox["\[Chi]","TI"],StyleBox[RowBox[{"c"," ","1"}],"TR"]]}],",",RowBox[{RowBox[{"Coil\[Chi]c","[","2","]"}],"->",SubscriptBox[StyleBox["\[Chi]","TI"],StyleBox[RowBox[{"c"," ","2"}],"TR"]]}],",",StyleBox["\"\[Ellipsis]\"","TR"]}],"}"}],",",StyleBox["\"\[Ellipsis]\"","TR"]}],"]"}]}}]],"InlineFormula",CellTags->"LoopFieldPlot2D_templates"]},{Cell[BoxData[RowBox[{" plots each of the \!\(\*SubscriptBox[StyleBox[\"B\", \"TI\"], StyleBox[\"x\", \"TI\"]]\), \!\(\*SubscriptBox[StyleBox[\"B\", \"TI\"], StyleBox[\"y\", \"TI\"]]\) and \!\(\*SubscriptBox[StyleBox[\"B\", \"TI\"], StyleBox[\"z\", \"TI\"]]\) field components over the \!\(\*StyleBox[\"x\", \"TI\"]\)\!\(\*StyleBox[\"z\", \"TI\"]\)-plane, generated by the loop-based coil with axial separations \!\(\*SubscriptBox[StyleBox[\"\[Chi]\", \"TI\"], StyleBox[RowBox[{\"c\", \" \", \"1\"}], \"TR\"]]\), \!\(\*SubscriptBox[StyleBox[\"\[Chi]\", \"TI\"], StyleBox[RowBox[{\"c\", \" \", \"2\"}], \"TR\"]]\), \!\(\*StyleBox[\"\\\"\[Ellipsis]\\\"\", \"TR\"]\), turn ratios \!\(\*SubscriptBox[StyleBox[\"i\", \"TI\"], StyleBox[RowBox[{\"\[Chi]\", \" \", \"1\"}], \"TR\"]]\), \!\(\*SubscriptBox[StyleBox[\"i\", \"TI\"], StyleBox[RowBox[{\"\[Chi]\", \" \", \"2\"}], \"TR\"]]\), \!\(\*StyleBox[\"\\\"\[Ellipsis]\\\"\", \"TR\"]\), radius \!\(\*SubscriptBox[StyleBox[\"\[Rho]\", \"TI\"], StyleBox[\"c\", \"TI\"]]\) and target field harmonic of order \!\(\*StyleBox[\"n\", \"TI\"]\)."}]],"Output",CellTags->"LoopFieldPlot2D_usages"],Cell[BoxData[RowBox[{" is an alternative way of specifying the axial separations \!\(\*SubscriptBox[StyleBox[\"\[Chi]\", \"TI\"], StyleBox[RowBox[{\"c\", \" \", \"1\"}], \"TR\"]]\), \!\(\*SubscriptBox[StyleBox[\"\[Chi]\", \"TI\"], StyleBox[RowBox[{\"c\", \" \", \"2\"}], \"TR\"]]\), \!\(\*StyleBox[\"\\\"\[Ellipsis]\\\"\", \"TR\"]\)."}]],"Output",CellTags->"LoopFieldPlot2D_usages"]}}]]


SaddleFieldPlot2D::usage =
"\!\(\*RowBox[{\"SaddleFieldPlot2D\", \"[\", RowBox[{RowBox[{\"{\", RowBox[{SubscriptBox[StyleBox[\"\[Chi]\", \"TI\"], StyleBox[RowBox[{\"c\", \" \", \"1\"}], \"TR\"]], \",\", SubscriptBox[StyleBox[\"\[Chi]\", \"TI\"], StyleBox[RowBox[{\"c\", \" \", \"2\"}], \"TR\"]], \",\", StyleBox[\"\\\"\[Ellipsis]\\\"\", \"TR\"]}], \"}\"}], \",\", RowBox[{\"{\", RowBox[{SubscriptBox[StyleBox[\"\[Phi]\", \"TI\"], StyleBox[RowBox[{\"c\", \" \", \"1\"}], \"TR\"]], \",\", SubscriptBox[StyleBox[\"\[Phi]\", \"TI\"], StyleBox[RowBox[{\"c\", \" \", \"2\"}], \"TR\"]], \",\", StyleBox[\"\\\"\[Ellipsis]\\\"\", \"TR\"]}], \"}\"}], \",\", RowBox[{\"{\", RowBox[{SubscriptBox[StyleBox[\"i\", \"TI\"], StyleBox[RowBox[{\"\[Chi]\", \" \", \"1\"}], \"TR\"]], \",\", SubscriptBox[StyleBox[\"i\", \"TI\"], StyleBox[RowBox[{\"\[Chi]\", \" \", \"2\"}], \"TR\"]], \",\", StyleBox[\"\\\"\[Ellipsis]\\\"\", \"TR\"]}], \"}\"}], \",\", SubscriptBox[StyleBox[\"\[Rho]\", \"TI\"], StyleBox[\"c\", \"TI\"]], \",\", RowBox[{\"{\", RowBox[{StyleBox[\"n\", \"TI\"], \",\", StyleBox[\"m\", \"TI\"]}], \"}\"}]}], \"]\"}]\) plots each of the \!\(\*SubscriptBox[StyleBox[\"B\", \"TI\"], StyleBox[\"x\", \"TI\"]]\), \!\(\*SubscriptBox[StyleBox[\"B\", \"TI\"], StyleBox[\"y\", \"TI\"]]\) and \!\(\*SubscriptBox[StyleBox[\"B\", \"TI\"], StyleBox[\"z\", \"TI\"]]\) field components over the \!\(\*StyleBox[\"x\", \"TI\"]\)\!\(\*StyleBox[\"z\", \"TI\"]\)\[Hyphen]plane, generated by the saddle\[Hyphen]based coil with axial separations \!\(\*SubscriptBox[StyleBox[\"\[Chi]\", \"TI\"], StyleBox[RowBox[{\"c\", \" \", \"1\"}], \"TR\"]]\), \!\(\*SubscriptBox[StyleBox[\"\[Chi]\", \"TI\"], StyleBox[RowBox[{\"c\", \" \", \"2\"}], \"TR\"]]\), \!\(\*StyleBox[\"\\\"\[Ellipsis]\\\"\", \"TR\"]\), azimuthal extents \!\(\*SubscriptBox[StyleBox[\"\[Phi]\", \"TI\"], StyleBox[RowBox[{\"c\", \" \", \"1\"}], \"TR\"]]\), \!\(\*SubscriptBox[StyleBox[\"\[Phi]\", \"TI\"], StyleBox[RowBox[{\"c\", \" \", \"2\"}], \"TR\"]]\), \!\(\*StyleBox[\"\\\"\[Ellipsis]\\\"\", \"TR\"]\), turn ratios \!\(\*SubscriptBox[StyleBox[\"i\", \"TI\"], StyleBox[RowBox[{\"\[Chi]\", \" \", \"1\"}], \"TR\"]]\), \!\(\*SubscriptBox[StyleBox[\"i\", \"TI\"], StyleBox[RowBox[{\"\[Chi]\", \" \", \"2\"}], \"TR\"]]\), \!\(\*StyleBox[\"\\\"\[Ellipsis]\\\"\", \"TR\"]\), radius \!\(\*SubscriptBox[StyleBox[\"\[Rho]\", \"TI\"], StyleBox[\"c\", \"TI\"]]\) and target field harmonic of order \!\(\*StyleBox[\"n\", \"TI\"]\) and degree \!\(\*StyleBox[\"m\", \"TI\"]\).
\!\(\*RowBox[{\"SaddleFieldPlot2D\", \"[\", RowBox[{RowBox[{\"{\", RowBox[{RowBox[{RowBox[{\"Coil\[Chi]c\", \"[\", \"1\", \"]\"}], \"\[Rule]\", SubscriptBox[StyleBox[\"\[Chi]\", \"TI\"], StyleBox[RowBox[{\"c\", \" \", \"1\"}], \"TR\"]]}], \",\", RowBox[{RowBox[{\"Coil\[Chi]c\", \"[\", \"2\", \"]\"}], \"\[Rule]\", SubscriptBox[StyleBox[\"\[Chi]\", \"TI\"], StyleBox[RowBox[{\"c\", \" \", \"2\"}], \"TR\"]]}], \",\", StyleBox[\"\\\"\[Ellipsis]\\\"\", \"TR\"]}], \"}\"}], \",\", RowBox[{\"{\", RowBox[{RowBox[{RowBox[{\"Coil\[Phi]c\", \"[\", \"1\", \"]\"}], \"\[Rule]\", SubscriptBox[StyleBox[\"\[Phi]\", \"TI\"], StyleBox[RowBox[{\"c\", \" \", \"1\"}], \"TR\"]]}], \",\", RowBox[{RowBox[{\"Coil\[Phi]c\", \"[\", \"2\", \"]\"}], \"\[Rule]\", SubscriptBox[StyleBox[\"\[Phi]\", \"TI\"], StyleBox[RowBox[{\"c\", \" \", \"2\"}], \"TR\"]]}], \",\", StyleBox[\"\\\"\[Ellipsis]\\\"\", \"TR\"]}], \"}\"}], \",\", StyleBox[\"\\\"\[Ellipsis]\\\"\", \"TR\"]}], \"]\"}]\) is an alternative way of specifying the axial separations \!\(\*SubscriptBox[StyleBox[\"\[Chi]c\", \"TI\"], StyleBox[\"1\", \"TR\"]]\), \!\(\*SubscriptBox[StyleBox[\"\[Chi]c\", \"TI\"], StyleBox[\"2\", \"TR\"]]\), \!\(\*StyleBox[\"\\\"\[Ellipsis]\\\"\", \"TR\"]\) and azimuthal extents \!\(\*SubscriptBox[StyleBox[\"\[Phi]\", \"TI\"], StyleBox[RowBox[{\"c\", \" \", \"1\"}], \"TR\"]]\), \!\(\*SubscriptBox[StyleBox[\"\[Phi]\", \"TI\"], StyleBox[RowBox[{\"c\", \" \", \"2\"}], \"TR\"]]\), \!\(\*StyleBox[\"\\\"\[Ellipsis]\\\"\", \"TR\"]\).";

MathLink`CallFrontEnd[FrontEnd`CacheTemplateAndUsagePacket[FrontEnd`InputNotebook[],"SaddleFieldPlot2D",{{Cell[BoxData[GridBox[{{RowBox[{"SaddleFieldPlot2D","[",RowBox[{RowBox[{"{",RowBox[{SubscriptBox[StyleBox["\[Chi]","TI"],StyleBox[RowBox[{"c"," ","1"}],"TR"]],",",SubscriptBox[StyleBox["\[Chi]","TI"],StyleBox[RowBox[{"c"," ","2"}],"TR"]],",",StyleBox["\"\[Ellipsis]\"","TR"]}],"}"}],",",RowBox[{"{",RowBox[{SubscriptBox[StyleBox["\[Phi]","TI"],StyleBox[RowBox[{"c"," ","1"}],"TR"]],",",SubscriptBox[StyleBox["\[Phi]","TI"],StyleBox[RowBox[{"c"," ","2"}],"TR"]],",",StyleBox["\"\[Ellipsis]\"","TR"]}],"}"}],",",RowBox[{"{",RowBox[{SubscriptBox[StyleBox["i","TI"],StyleBox[RowBox[{"\[Chi]"," ","1"}],"TR"]],",",SubscriptBox[StyleBox["i","TI"],StyleBox[RowBox[{"\[Chi]"," ","2"}],"TR"]],",",StyleBox["\"\[Ellipsis]\"","TR"]}],"}"}],",",SubscriptBox[StyleBox["\[Rho]","TI"],StyleBox["c","TI"]],",",RowBox[{"{",RowBox[{StyleBox["n","TI"],",",StyleBox["m","TI"]}],"}"}]}],"]"}]}}]],"InlineFormula",CellTags->"SaddleFieldPlot2D_templates"],Cell[BoxData[GridBox[{{RowBox[{"SaddleFieldPlot2D","[",RowBox[{RowBox[{"{",RowBox[{RowBox[{RowBox[{"Coil\[Chi]c","[","1","]"}],"->",SubscriptBox[StyleBox["\[Chi]","TI"],StyleBox[RowBox[{"c"," ","1"}],"TR"]]}],",",RowBox[{RowBox[{"Coil\[Chi]c","[","2","]"}],"->",SubscriptBox[StyleBox["\[Chi]","TI"],StyleBox[RowBox[{"c"," ","2"}],"TR"]]}],",",StyleBox["\"\[Ellipsis]\"","TR"]}],"}"}],",",RowBox[{"{",RowBox[{RowBox[{RowBox[{"Coil\[Phi]c","[","1","]"}],"->",SubscriptBox[StyleBox["\[Phi]","TI"],StyleBox[RowBox[{"c"," ","1"}],"TR"]]}],",",RowBox[{RowBox[{"Coil\[Phi]c","[","2","]"}],"->",SubscriptBox[StyleBox["\[Phi]","TI"],StyleBox[RowBox[{"c"," ","2"}],"TR"]]}],",",StyleBox["\"\[Ellipsis]\"","TR"]}],"}"}],",",StyleBox["\"\[Ellipsis]\"","TR"]}],"]"}]}}]],"InlineFormula",CellTags->"SaddleFieldPlot2D_templates"]},{Cell[BoxData[RowBox[{" plots each of the \!\(\*SubscriptBox[StyleBox[\"B\", \"TI\"], StyleBox[\"x\", \"TI\"]]\), \!\(\*SubscriptBox[StyleBox[\"B\", \"TI\"], StyleBox[\"y\", \"TI\"]]\) and \!\(\*SubscriptBox[StyleBox[\"B\", \"TI\"], StyleBox[\"z\", \"TI\"]]\) field components over the \!\(\*StyleBox[\"x\", \"TI\"]\)\!\(\*StyleBox[\"z\", \"TI\"]\)-plane, generated by the saddle-based coil with axial separations \!\(\*SubscriptBox[StyleBox[\"\[Chi]\", \"TI\"], StyleBox[RowBox[{\"c\", \" \", \"1\"}], \"TR\"]]\), \!\(\*SubscriptBox[StyleBox[\"\[Chi]\", \"TI\"], StyleBox[RowBox[{\"c\", \" \", \"2\"}], \"TR\"]]\), \!\(\*StyleBox[\"\\\"\[Ellipsis]\\\"\", \"TR\"]\), azimuthal extents \!\(\*SubscriptBox[StyleBox[\"\[Phi]\", \"TI\"], StyleBox[RowBox[{\"c\", \" \", \"1\"}], \"TR\"]]\), \!\(\*SubscriptBox[StyleBox[\"\[Phi]\", \"TI\"], StyleBox[RowBox[{\"c\", \" \", \"2\"}], \"TR\"]]\), \!\(\*StyleBox[\"\\\"\[Ellipsis]\\\"\", \"TR\"]\), turn ratios \!\(\*SubscriptBox[StyleBox[\"i\", \"TI\"], StyleBox[RowBox[{\"\[Chi]\", \" \", \"1\"}], \"TR\"]]\), \!\(\*SubscriptBox[StyleBox[\"i\", \"TI\"], StyleBox[RowBox[{\"\[Chi]\", \" \", \"2\"}], \"TR\"]]\), \!\(\*StyleBox[\"\\\"\[Ellipsis]\\\"\", \"TR\"]\), radius \!\(\*SubscriptBox[StyleBox[\"\[Rho]\", \"TI\"], StyleBox[\"c\", \"TI\"]]\) and target field harmonic of order \!\(\*StyleBox[\"n\", \"TI\"]\) and degree \!\(\*StyleBox[\"m\", \"TI\"]\)."}]],"Output",CellTags->"SaddleFieldPlot2D_usages"],Cell[BoxData[RowBox[{" is an alternative way of specifying the axial separations \!\(\*SubscriptBox[StyleBox[\"\[Chi]c\", \"TI\"], StyleBox[\"1\", \"TR\"]]\), \!\(\*SubscriptBox[StyleBox[\"\[Chi]c\", \"TI\"], StyleBox[\"2\", \"TR\"]]\), \!\(\*StyleBox[\"\\\"\[Ellipsis]\\\"\", \"TR\"]\) and azimuthal extents \!\(\*SubscriptBox[StyleBox[\"\[Phi]\", \"TI\"], StyleBox[RowBox[{\"c\", \" \", \"1\"}], \"TR\"]]\), \!\(\*SubscriptBox[StyleBox[\"\[Phi]\", \"TI\"], StyleBox[RowBox[{\"c\", \" \", \"2\"}], \"TR\"]]\), \!\(\*StyleBox[\"\\\"\[Ellipsis]\\\"\", \"TR\"]\)."}]],"Output",CellTags->"SaddleFieldPlot2D_usages"]}}]]


EllipseFieldPlot2D::usage =
"\!\(\*RowBox[{\"EllipseFieldPlot2D\", \"[\", RowBox[{RowBox[{\"{\", RowBox[{RowBox[{\"{\", RowBox[{SubscriptBox[StyleBox[\"\[Chi]\", \"TI\"], StyleBox[RowBox[{\"c\", \" \", \"1\"}], \"TR\"]], \",\", SubscriptBox[StyleBox[\"\[Psi]\", \"TI\"], StyleBox[RowBox[{\"c\", \" \", \"1\"}], \"TR\"]]}], \"}\"}], \",\", RowBox[{\"{\", RowBox[{SubscriptBox[StyleBox[\"\[Chi]\", \"TI\"], StyleBox[RowBox[{\"c\", \" \", \"2\"}], \"TR\"]], \",\", SubscriptBox[StyleBox[\"\[Psi]\", \"TI\"], StyleBox[RowBox[{\"c\", \" \", \"2\"}], \"TR\"]]}], \"}\"}], \",\", StyleBox[\"\\\"\[Ellipsis]\\\"\", \"TR\"]}], \"}\"}], \",\", RowBox[{\"{\", RowBox[{SubscriptBox[StyleBox[\"i\", \"TI\"], StyleBox[RowBox[{\"\[Chi]\", \" \", \"1\"}], \"TR\"]], \",\", SubscriptBox[StyleBox[\"i\", \"TI\"], StyleBox[RowBox[{\"\[Chi]\", \" \", \"2\"}], \"TR\"]], \",\", StyleBox[\"\\\"\[Ellipsis]\\\"\", \"TR\"]}], \"}\"}], \",\", SubscriptBox[StyleBox[\"\[Rho]\", \"TI\"], StyleBox[\"c\", \"TI\"]], \",\", RowBox[{\"{\", RowBox[{StyleBox[\"n\", \"TI\"], \",\", StyleBox[\"m\", \"TI\"]}], \"}\"}]}], \"]\"}]\) plots each of the \!\(\*SubscriptBox[StyleBox[\"B\", \"TI\"], StyleBox[\"x\", \"TI\"]]\), \!\(\*SubscriptBox[StyleBox[\"B\", \"TI\"], StyleBox[\"y\", \"TI\"]]\) and \!\(\*SubscriptBox[StyleBox[\"B\", \"TI\"], StyleBox[\"z\", \"TI\"]]\) field components over the \!\(\*StyleBox[\"x\", \"TI\"]\)\!\(\*StyleBox[\"z\", \"TI\"]\)\[Hyphen]plane, generated by the ellipse\[Hyphen]based coil with axial separations \!\(\*SubscriptBox[StyleBox[\"\[Chi]\", \"TI\"], StyleBox[RowBox[{\"c\", \" \", \"1\"}], \"TR\"]]\), \!\(\*SubscriptBox[StyleBox[\"\[Chi]\", \"TI\"], StyleBox[RowBox[{\"c\", \" \", \"2\"}], \"TR\"]]\), \!\(\*StyleBox[\"\\\"\[Ellipsis]\\\"\", \"TR\"]\), ellipse extents \!\(\*SubscriptBox[StyleBox[\"\[Psi]\", \"TI\"], StyleBox[RowBox[{\"c\", \" \", \"1\"}], \"TR\"]]\), \!\(\*SubscriptBox[StyleBox[\"\[Psi]\", \"TI\"], StyleBox[RowBox[{\"c\", \" \", \"2\"}], \"TR\"]]\), \!\(\*StyleBox[\"\\\"\[Ellipsis]\\\"\", \"TR\"]\), turn ratios \!\(\*SubscriptBox[StyleBox[\"i\", \"TI\"], StyleBox[RowBox[{\"\[Chi]\", \" \", \"1\"}], \"TR\"]]\), \!\(\*SubscriptBox[StyleBox[\"i\", \"TI\"], StyleBox[RowBox[{\"\[Chi]\", \" \", \"2\"}], \"TR\"]]\), \!\(\*StyleBox[\"\\\"\[Ellipsis]\\\"\", \"TR\"]\), radius \!\(\*SubscriptBox[StyleBox[\"\[Rho]\", \"TI\"], StyleBox[\"c\", \"TI\"]]\) and target field harmonic of order \!\(\*StyleBox[\"n\", \"TI\"]\) and degree \!\(\*StyleBox[\"m\", \"TI\"]\).
\!\(\*RowBox[{\"EllipseFieldPlot2D\", \"[\", RowBox[{RowBox[{\"{\", RowBox[{RowBox[{RowBox[{\"Coil\[Chi]c\", \"[\", \"1\", \"]\"}], \"\[Rule]\", SubscriptBox[StyleBox[\"\[Chi]\", \"TI\"], StyleBox[RowBox[{\"c\", \" \", \"1\"}], \"TR\"]]}], \",\", RowBox[{RowBox[{\"Coil\[Chi]c\", \"[\", \"2\", \"]\"}], \"\[Rule]\", SubscriptBox[StyleBox[\"\[Chi]\", \"TI\"], StyleBox[RowBox[{\"c\", \" \", \"2\"}], \"TR\"]]}], \",\", StyleBox[\"\\\"\[Ellipsis]\\\"\", \"TR\"], \",\", RowBox[{RowBox[{\"Coil\[Psi]c\", \"[\", \"1\", \"]\"}], \"\[Rule]\", SubscriptBox[StyleBox[\"\[Psi]\", \"TI\"], StyleBox[RowBox[{\"c\", \" \", \"1\"}], \"TR\"]]}], \",\", RowBox[{RowBox[{\"Coil\[Psi]c\", \"[\", \"2\", \"]\"}], \"\[Rule]\", SubscriptBox[StyleBox[\"\[Psi]\", \"TI\"], StyleBox[RowBox[{\"c\", \" \", \"2\"}], \"TR\"]]}], \",\", StyleBox[\"\\\"\[Ellipsis]\\\"\", \"TR\"]}], \"}\"}], \",\", StyleBox[\"\\\"\[Ellipsis]\\\"\", \"TR\"]}], \"]\"}]\) is an alternative way of specifying the axial separations \!\(\*SubscriptBox[StyleBox[\"\[Chi]c\", \"TI\"], StyleBox[\"1\", \"TR\"]]\), \!\(\*SubscriptBox[StyleBox[\"\[Chi]c\", \"TI\"], StyleBox[\"2\", \"TR\"]]\), \!\(\*StyleBox[\"\\\"\[Ellipsis]\\\"\", \"TR\"]\) and ellipse extents \!\(\*SubscriptBox[StyleBox[\"\[Psi]\", \"TI\"], StyleBox[RowBox[{\"c\", \" \", \"1\"}], \"TR\"]]\), \!\(\*SubscriptBox[StyleBox[\"\[Psi]\", \"TI\"], StyleBox[RowBox[{\"c\", \" \", \"2\"}], \"TR\"]]\), \!\(\*StyleBox[\"\\\"\[Ellipsis]\\\"\", \"TR\"]\).";

MathLink`CallFrontEnd[FrontEnd`CacheTemplateAndUsagePacket[FrontEnd`InputNotebook[],"EllipseFieldPlot2D",{{Cell[BoxData[GridBox[{{RowBox[{"EllipseFieldPlot2D","[",RowBox[{RowBox[{"{",RowBox[{RowBox[{"{",RowBox[{SubscriptBox[StyleBox["\[Chi]","TI"],StyleBox[RowBox[{"c"," ","1"}],"TR"]],",",SubscriptBox[StyleBox["\[Psi]","TI"],StyleBox[RowBox[{"c"," ","1"}],"TR"]]}],"}"}],",",RowBox[{"{",RowBox[{SubscriptBox[StyleBox["\[Chi]","TI"],StyleBox[RowBox[{"c"," ","2"}],"TR"]],",",SubscriptBox[StyleBox["\[Psi]","TI"],StyleBox[RowBox[{"c"," ","2"}],"TR"]]}],"}"}],",",StyleBox["\"\[Ellipsis]\"","TR"]}],"}"}],",",RowBox[{"{",RowBox[{SubscriptBox[StyleBox["i","TI"],StyleBox[RowBox[{"\[Chi]"," ","1"}],"TR"]],",",SubscriptBox[StyleBox["i","TI"],StyleBox[RowBox[{"\[Chi]"," ","2"}],"TR"]],",",StyleBox["\"\[Ellipsis]\"","TR"]}],"}"}],",",SubscriptBox[StyleBox["\[Rho]","TI"],StyleBox["c","TI"]],",",RowBox[{"{",RowBox[{StyleBox["n","TI"],",",StyleBox["m","TI"]}],"}"}]}],"]"}]}}]],"InlineFormula",CellTags->"EllipseFieldPlot2D_templates"],Cell[BoxData[GridBox[{{RowBox[{"EllipseFieldPlot2D","[",RowBox[{RowBox[{"{",RowBox[{RowBox[{RowBox[{"Coil\[Chi]c","[","1","]"}],"->",SubscriptBox[StyleBox["\[Chi]","TI"],StyleBox[RowBox[{"c"," ","1"}],"TR"]]}],",",RowBox[{RowBox[{"Coil\[Chi]c","[","2","]"}],"->",SubscriptBox[StyleBox["\[Chi]","TI"],StyleBox[RowBox[{"c"," ","2"}],"TR"]]}],",",StyleBox["\"\[Ellipsis]\"","TR"],",",RowBox[{RowBox[{"Coil\[Psi]c","[","1","]"}],"->",SubscriptBox[StyleBox["\[Psi]","TI"],StyleBox[RowBox[{"c"," ","1"}],"TR"]]}],",",RowBox[{RowBox[{"Coil\[Psi]c","[","2","]"}],"->",SubscriptBox[StyleBox["\[Psi]","TI"],StyleBox[RowBox[{"c"," ","2"}],"TR"]]}],",",StyleBox["\"\[Ellipsis]\"","TR"]}],"}"}],",",StyleBox["\"\[Ellipsis]\"","TR"]}],"]"}]}}]],"InlineFormula",CellTags->"EllipseFieldPlot2D_templates"]},{Cell[BoxData[RowBox[{" plots each of the \!\(\*SubscriptBox[StyleBox[\"B\", \"TI\"], StyleBox[\"x\", \"TI\"]]\), \!\(\*SubscriptBox[StyleBox[\"B\", \"TI\"], StyleBox[\"y\", \"TI\"]]\) and \!\(\*SubscriptBox[StyleBox[\"B\", \"TI\"], StyleBox[\"z\", \"TI\"]]\) field components over the \!\(\*StyleBox[\"x\", \"TI\"]\)\!\(\*StyleBox[\"z\", \"TI\"]\)-plane, generated by the ellipse-based coil with axial separations \!\(\*SubscriptBox[StyleBox[\"\[Chi]\", \"TI\"], StyleBox[RowBox[{\"c\", \" \", \"1\"}], \"TR\"]]\), \!\(\*SubscriptBox[StyleBox[\"\[Chi]\", \"TI\"], StyleBox[RowBox[{\"c\", \" \", \"2\"}], \"TR\"]]\), \!\(\*StyleBox[\"\\\"\[Ellipsis]\\\"\", \"TR\"]\), ellipse extents \!\(\*SubscriptBox[StyleBox[\"\[Psi]\", \"TI\"], StyleBox[RowBox[{\"c\", \" \", \"1\"}], \"TR\"]]\), \!\(\*SubscriptBox[StyleBox[\"\[Psi]\", \"TI\"], StyleBox[RowBox[{\"c\", \" \", \"2\"}], \"TR\"]]\), \!\(\*StyleBox[\"\\\"\[Ellipsis]\\\"\", \"TR\"]\), turn ratios \!\(\*SubscriptBox[StyleBox[\"i\", \"TI\"], StyleBox[RowBox[{\"\[Chi]\", \" \", \"1\"}], \"TR\"]]\), \!\(\*SubscriptBox[StyleBox[\"i\", \"TI\"], StyleBox[RowBox[{\"\[Chi]\", \" \", \"2\"}], \"TR\"]]\), \!\(\*StyleBox[\"\\\"\[Ellipsis]\\\"\", \"TR\"]\), radius \!\(\*SubscriptBox[StyleBox[\"\[Rho]\", \"TI\"], StyleBox[\"c\", \"TI\"]]\) and target field harmonic of order \!\(\*StyleBox[\"n\", \"TI\"]\) and degree \!\(\*StyleBox[\"m\", \"TI\"]\)."}]],"Output",CellTags->"EllipseFieldPlot2D_usages"],Cell[BoxData[RowBox[{" is an alternative way of specifying the axial separations \!\(\*SubscriptBox[StyleBox[\"\[Chi]c\", \"TI\"], StyleBox[\"1\", \"TR\"]]\), \!\(\*SubscriptBox[StyleBox[\"\[Chi]c\", \"TI\"], StyleBox[\"2\", \"TR\"]]\), \!\(\*StyleBox[\"\\\"\[Ellipsis]\\\"\", \"TR\"]\) and ellipse extents \!\(\*SubscriptBox[StyleBox[\"\[Psi]\", \"TI\"], StyleBox[RowBox[{\"c\", \" \", \"1\"}], \"TR\"]]\), \!\(\*SubscriptBox[StyleBox[\"\[Psi]\", \"TI\"], StyleBox[RowBox[{\"c\", \" \", \"2\"}], \"TR\"]]\), \!\(\*StyleBox[\"\\\"\[Ellipsis]\\\"\", \"TR\"]\)."}]],"Output",CellTags->"EllipseFieldPlot2D_usages"]}}]]


HarmonicFieldPlot::usage =
"\!\(\*RowBox[{\"HarmonicFieldPlot\", \"[\", RowBox[{\"{\", RowBox[{StyleBox[\"n\", \"TI\"], \",\", StyleBox[\"m\", \"TI\"]}], \"}\"}], \"]\"}]\) plots the field harmonic of order \!\(\*StyleBox[\"n\", \"TI\"]\) and degree \!\(\*StyleBox[\"m\", \"TI\"]\), showing the \!\(\*SubscriptBox[StyleBox[\"B\", \"TI\"], StyleBox[\"x\", \"TI\"]]\), \!\(\*SubscriptBox[StyleBox[\"B\", \"TI\"], StyleBox[\"y\", \"TI\"]]\) and \!\(\*SubscriptBox[StyleBox[\"B\", \"TI\"], StyleBox[\"z\", \"TI\"]]\) components along each of the \!\(\*StyleBox[\"x\", \"TI\"]\)\[Hyphen], \!\(\*StyleBox[\"y\", \"TI\"]\)\[Hyphen] and \!\(\*StyleBox[\"z\", \"TI\"]\)\[Hyphen]coordinate axes.";

MathLink`CallFrontEnd[FrontEnd`CacheTemplateAndUsagePacket[FrontEnd`InputNotebook[],"HarmonicFieldPlot",{{Cell[BoxData[GridBox[{{RowBox[{"HarmonicFieldPlot","[",RowBox[{"{",RowBox[{StyleBox["n","TI"],",",StyleBox["m","TI"]}],"}"}],"]"}]}}]],"InlineFormula",CellTags->"HarmonicFieldPlot_templates"]},{Cell[BoxData[RowBox[{" plots the field harmonic of order \!\(\*StyleBox[\"n\", \"TI\"]\) and degree \!\(\*StyleBox[\"m\", \"TI\"]\), showing the \!\(\*SubscriptBox[StyleBox[\"B\", \"TI\"], StyleBox[\"x\", \"TI\"]]\), \!\(\*SubscriptBox[StyleBox[\"B\", \"TI\"], StyleBox[\"y\", \"TI\"]]\) and \!\(\*SubscriptBox[StyleBox[\"B\", \"TI\"], StyleBox[\"z\", \"TI\"]]\) components along each of the \!\(\*StyleBox[\"x\", \"TI\"]\)-, \!\(\*StyleBox[\"y\", \"TI\"]\)- and \!\(\*StyleBox[\"z\", \"TI\"]\)-coordinate axes."}]],"Output",CellTags->"HarmonicFieldPlot_usages"]}}]]


Begin["`Private`"];


(* ::Section::Closed:: *)
(*Messages*)


finderMessages = <|
	"BadCurrents" -> "Currents: `1` should be a list of one or more real numbers.",
	"BadDesired" -> "Desired harmonic N = `1` should be an integer greater than zero.",
	"BadSeparations" -> "Search range: `1` should be of the form {min\[Chi]c, max\[Chi]c}, where 0 < min\[Chi]c < max\[Chi]c.",
	"BadLeadingError" -> "If \"NulledHarmonics\" is not Automatic, then \"LeadingErrorHarmonic\" must be given explicitly.",
	"BadDesiredNM" -> "Desired harmonic order N = `1` and degree M = `2` should be integers, where N \[GreaterEqual] M > 0.",
	"BadDesiredM" -> "Desired harmonic order M = `1` should be an integer greater than zero.",
	"BadNulledDegrees" -> "The number of degrees to null, k\[Phi] = `1`, should be an integer greater than zero.",
	"BadCurrentRatios" -> "Coil is axially symmetric (N + M = `1` + `2` is odd), so for the arcs to be joined, an even number of currents is required, and sucessive pairs of currents must be equal in magnitude and opposite in parity (e.g. {2, -2, -1, 1, \[Ellipsis]}).",
	"BadExtents" -> "Search range: `1` should be of the form {min\[Psi], max\[Psi]}, where 0 < min\[Psi] < max\[Psi]."
|>;


plotMessages = <|
	"BadSeparations" -> "Separations: `1` should either be a list of one or more ascending positive numbers, or a list of Coil\[Chi]c[i] -> \[Chi]ci rules, e.g. {Coil\[Chi]c[1] -> \[Chi]c1, Coil\[Chi]c[2] -> \[Chi]c2, \[Ellipsis]}, where \[Chi]c1 < \[Chi]c2 < \[Ellipsis]. In the latter case, the list can contain a DesToErr -> val rule (which will be ignored).",
	"BadCurrents" -> "Currents: `1` should be a list of one or more real numbers, equal in length to the number of separations.",
	"BadDesired" -> finderMessages["BadDesired"],
	"BadRadius" -> "Coil radius \[Rho]c = `1` should be a positive number.",
	"BadExtents" -> "Extents: `1` should either be a list of one or more ascending positive numbers, or a list of Coil\[Phi]c[i] -> \[Phi]ci rules, e.g. {Coil\[Phi]c[1] -> \[Phi]c1, Coil\[Phi]c[2] -> \[Phi]c2, \[Ellipsis]}, where \[Phi]c1 < \[Phi]c2 < \[Ellipsis].",
	"BadCurrentRatios" -> finderMessages["BadCurrentRatios"],
	"BadDesiredNM" -> finderMessages["BadDesiredNM"],
	"BadChiPsi" -> "Axial separations and extents: `1` should either be a list of separation and extent pairs, i.e. {{\[Chi]c1, \[Psi]c1}, {\[Chi]c2, \[Psi]c2}, \[Ellipsis]}, or a flat list of Coil\[Chi]c[i] -> \[Chi]ci and Coil\[Psi]c[i] -> \[Psi]ci rules, e.g. {Coil\[Chi]c[1] -> \[Chi]c1, Coil\[Psi]c[1] -> \[Psi]c1, Coil\[Chi]c[2] -> \[Chi]c2, Coil\[Psi]c[2] -> \[Psi]c2, \[Ellipsis]}, where there are as many extents as separations. In both cases, \[Chi]c1 < \[Chi]c2 < \[Ellipsis] must be satisfied. In the latter case, the list can contain a DesToErr -> val rule (which will be ignored).",
	"BadNM" -> "Desired harmonic order N = `1` and degree M = `2` should be integers, where N \[GreaterEqual] M \[GreaterEqual] 0 and N > 0."
|>;


(* ::Section::Closed:: *)
(*Magnetic Field*)


(* ::Text:: *)
(*Calculate the magnetic field as a summation over spherical harmonics.*)


(* ::Text:: *)
(*B = Sum[Ns] Sum[Ms] C[n,m] (X[n,m] x + Y[n,m] y + Z[n,m] z), where Ns and Ms are lists dictated by the coil (i.e. topology, N, and M), C[n,m] are the magnitudes of harmonics in the field and (X[n,m] x + Y[n,m] y + Z[n,m] z) are the field harmonic vector components in the magnetic field.*)


(* ::Text:: *)
(*For the loops case: m = 0, and each n is odd for symmetric coils and even for anti-symmetric coils.*)


p[{n_, m_}, x_] :=
	(-1)^m LegendreP[n, m, x] (* We do not include the Condon-Shortley phase *)


f[{n_, m_}] :=
	Sqrt[(2. - KroneckerDelta[m, 0])(2n + 1)(n - m)! / (4\[Pi] (n + m)!)]


bFieldHarmonicVector[{n_, m_}, r_, \[Theta]_, \[Phi]_] :=
	With[{
		kd = KroneckerDelta[m, 0],
		pm = p[{n - 1, m}, Cos[\[Theta]]],
		pmMinus = p[{n - 1, m - 1}, Cos[\[Theta]]],
		pmPlus = p[{n - 1, m + 1}, Cos[\[Theta]]],
		(* pmMinus may be ComplexInfinity, which, when multiplied by 0, equals Indeterminate. We want
			it to equal 0 instead. *)
		zero = Function[Replace[Chop[#1], Except[0] :> #1 * #2]]},
		
		f[{n, m}] r^(n-1) {
			(* x component *)
			-(1 + kd)/2 pmPlus Cos[(m + 1)\[Phi]] +
			zero[(1 - kd)/2 (n + m - 1)(n + m) Cos[(m - 1)\[Phi]], pmMinus],
			(* y component *)
			-(1 + kd)/2 pmPlus Sin[(m + 1)\[Phi]] -
			zero[(1 - kd)/2 (n + m - 1)(n + m) Sin[(m - 1)\[Phi]], pmMinus],
			(* z component *)
			(n + m) pm Cos[m \[Phi]]}]


bFieldCoilHarmonicVector[{n_, m_}, \[Chi]cVals_, i\[Chi]_, \[Phi]cVals_, tVals_, \[Rho]c_, pt:{x_, y_, z_}] :=
	Block[{\[Chi]c, t},
		With[{r = Norm[pt]},
			Replace[\[Phi]cVals, {None -> 1, _ ->
				Total[Sin[m #]& /@ \[Phi]cVals]}] *
			1/Pi 1/\[Rho]c^n *
			ReplaceAll[
				totalHarmonic[i\[Chi], {n, m}, \[Chi]c, Replace[tVals, Except[None] -> t]],
				Rule @@@ Join[
					Transpose[{Table[\[Chi]c[i], {i, Length[\[Chi]cVals]}], \[Chi]cVals}],
					If[tVals === None, {},
						Transpose[{Table[t[i], {i, Length[tVals]}], tVals}]]]] *
			bFieldHarmonicVector[{n, m}, r, ArcCos[z/r], ArcTan[y/x]]]]


(* ::Section::Closed:: *)
(*Selecting Harmonics*)


(* ::Text:: *)
(*Calculate the simultaneous equations for our desired harmonic, NDes, and the harmonics we desire to be nulled, NNull:*)


(* ::Text:: *)
(*Important: Simplify and N are used frequently at the evaluation stages of the following functions. Simplifying as we go is much faster than only applying Simplify to the final expression.*)


(* ::Text:: *)
(*Use the construct "f[x___] := f[x] = body" to cache results so that long-to-compute expressions only need to be calculated once per kernel session.*)


(* ::Text:: *)
(*Axial harmonic weighting:*)


\[Beta]["Loop"][{n_, 0}, \[Chi]_] := \[Beta]["Loop"][{n, 0}, \[Chi]] =
	Simplify @ N[\[Pi]/2 D[\[Chi] Sqrt[1/(1 + \[Chi]^2)], {\[Chi], n}]]


\[Beta]["Saddle"][{n_, m_}, \[Chi]_] := \[Beta]["Saddle"][{n, m}, \[Chi]] =
	Simplify @ N[
		(\[Pi] (2m)!)/(2^(m + 1) m!) D[
			\[Chi] (1/(1 + \[Chi]^2))^(m + 1/2) -
			Simplify @ N[m (-1)^m Sum[
				Binomial[m - 1, k] (-1)^k/(2m - 2k - 1) (\[Chi]^2/(1 + \[Chi]^2))^(m - k - 1/2),
				{k, 0, m - 1}]],
			{\[Chi], n - m}]]


q[n_, m_, x_] := (-1)^m LegendreQ[n, m, 3, x]


s[m_, t_, \[Chi]_] := s[m, t, \[Chi]] =
	Simplify @ N[(1 - I)/(2 Sqrt[2t] I^m) q[m - 1/2, 0, I (t^2 - \[Chi]^2 - 1)/(2t)]]


\[Beta]["Ellipse"][{n_, m_}, t_, \[Chi]_] := \[Beta]["Ellipse"][{n, m}, t, \[Chi]] =
	Simplify @ N[
		(-1)^(m+1) D[
			Simplify[
				\[Chi] D[s[m, t, \[Chi]], {\[Chi], m}] +
				m D[s[m, t, \[Chi]], {\[Chi], m-1}] +
				t D[Simplify @ D[s[m, t, \[Chi]], {\[Chi], m-1}], t]],
		{\[Chi], n-m}]]


(* ::Text:: *)
(*Harmonic magnitude:*)


harmMag[{n_, 0}, \[Chi]_, None] := 2/n! \[Beta]["Loop"][{n, 0}, \[Chi]]


harmMag[{n_, m:Except[0]}, \[Chi]_, None] := 8/(\[Pi] (n + m)!) \[Beta]["Saddle"][{n, m}, \[Chi]]


harmMag[{n_, m_}, \[Chi]_, t:Except[None]] := 8m/((n + m)!) \[Beta]["Ellipse"][{n, m}, t, \[Chi]]


(* ::Text:: *)
(*Define a function to generate the total harmonic of order N, given the coil currents:*)


(* Compiling the totalHarmonics at this point will slow FindRoot down, which does its own compilation. Hence
	compilation is only offered here incase you want to work with the total harmonics directly by calling
	CreateCoil`Private`totalHarmonic. *)
Options[totalHarmonic] = {"Compile" -> False};


totalHarmonic[i\[Chi]_List, {n_Integer, m_Integer}, \[Chi]c_Symbol, t_Symbol:None, opts:OptionsPattern[]] :=
totalHarmonic[i\[Chi], {n, m}, \[Chi]c, t, opts] =
	With[{compQ = TrueQ[OptionValue["Compile"]]},

		totalHarmonic[i\[Chi], {n, m}, \[Chi]c, t, "Compile" -> compQ] = Module[
			{term, expr, vars, fn, loopCount = Length[i\[Chi]]},

			(* The sum of harmonic contributions from however as many loop/saddle/ellipse-primitives as there are currents. *)
			(* Simplify to quicken root-finding later. *)
			term = Simplify[N[harmMag[{n, m}, \[Chi]c, t] / f[{n, m}]], Assumptions -> {\[Chi]c > 0, t > 0}];
			expr = Re @ Sum[
				i\[Chi][[j]] (term /. {\[Chi]c -> \[Chi]c[j], t -> t[j]}),
				{j, loopCount}];

			(* Compile the total harmonic expression if needed. Note: FindRoot automatically compiles its equations, so compilation
				is only potentially advantageous here if you want to work with the total harmonics directly. *)
			If[!compQ, expr,
				(* Make {\[Chi]c[1], t[1], \[Chi]c[2], t[2], ...} in the ellipse case or {\[Chi]c[1], \[Chi]c[2], ...} otherwise. *)
				vars = Flatten[Through[{\[Chi]c, t /. None -> Nothing}[#]] & /@ Range[loopCount]];
				(* Compile for Reals. "EvaluateSymbolically" -> False is important because it allows us to feed the compiled
					function symbolic values without it attempting to evaluate, so we can build up a symbolic expression
					involving the compiled function. When those symbols get replaced with Reals, the compiled function will
					then evaluate. *)
				fn = Compile @@ {vars, expr, RuntimeOptions -> {"EvaluateSymbolically" -> False}};
				(* Return compiledFn[\[Chi]c[1], t[1], ...]. *)
				fn @@ vars]]]


totalHarmonic[i\[Chi]_List, n_Integer, \[Chi]c_Symbol, t:None:None, o:OptionsPattern[]] :=
	totalHarmonic[i\[Chi], {n, 0}, \[Chi]c, o]


(* ::Text:: *)
(*Determine which harmonics to null, given the coil parameters:*)


harmonicsToNull["Loop"][loopCount_, nDes_] :=
	With[{nullCount = loopCount - 1},
		(* Make a list of the first nullCount numbers with the parity of nDes, ensuring that nDes is not one of them. *)
		DeleteCases[2 Range[nullCount + 1] - If[EvenQ[nDes], 0, 1], nDes][[;; nullCount]]]


harmonicsToNull["Saddle"][loopCount_, {nDes_, mDes_}, k\[Phi]_] :=
	Module[{nNull, mNullAz, mNull, nmNull, nullCount},
		nullCount = loopCount - 1;
		(* Degrees that will be nulled azimuthally *)
		mNullAz = mDes(2 Range[k\[Phi]] + 1);
		(* Degrees that will be nulled axially *)
		mNull = Complement[mDes(2 Range[0, k\[Phi] + loopCount] + 1), mNullAz];
		(* Orders to null *)
		nNull = harmonicsToNull["Loop"][mDes(2 mDes + 1) + loopCount + k\[Phi], nDes];
		(* List out the combinations of {n, m} where n >= m. *)
		nmNull = Catenate @ Outer[
			Function[{n, m}, If[m > n, Nothing, {n, m}]],
			nNull, mNull];
		(* Sort by n+m. Break ties by prioritising larger n. *)
		nmNull = SortBy[nmNull, {Total, 1/First[#]&}];
		nmNull[[;; nullCount]]]


harmonicsToNull["Ellipse"][loopCount_ , {nDes_, mDes_}] :=
	Module[{nullCount, nNull, mNull, nmNull},
		nullCount = loopCount - 1;
		nNull = 2 Range[2 nullCount] - If[EvenQ[nDes], 0, 1];
		mNull = mDes(2 Range[2 nullCount] - 1);
		nmNull = Outer[
			If[Less[##] || {##} === {nDes, mDes}, Nothing, {##}]&,
			nNull, mNull];
		nmNull = SortBy[Catenate[nmNull], {Total, (1./First[#])&}];
		nmNull[[;; nullCount]]]


(* ::Section::Closed:: *)
(*Nulling Code*)


findCoilOpts = Join[
	Options[FindRoot],
	{
		"CoilsReturned" -> 10,
		"ValuesOnly" -> False,
		"NulledHarmonics" -> Automatic,
		"LeadingErrorHarmonic" -> Automatic,
		"MeshPointsPer\[Chi]c" -> 20,
		"DuplicatesProximity" -> Scaled[.03],
		"NullingThreshold" -> 10.^-5,
		"MinSeparationsDifference" -> Scaled[.01],
		"MinSeparationAndExtentDifference" -> Scaled[.01],
		"InterpolationPoints" -> Automatic,
		"ContourMeshNN" -> Automatic,
		"ExpansionBleed" -> 1.5,
		"Seed" -> 1,
		"PrintSteps" -> False,
		"FinalSolutionChecks" -> True}];


realQ[x_] := TrueQ[Element[x, Reals]]


(* If the "ShowSteps" option is True, we want to be able to return echoed, labeled, iconized expressions. Otherwise discard the label. *)
echoFn[printQ_] := If[TrueQ[printQ], echoFnLabel, Identity &]

echoFnLabel[label_] := Echo[#, label, Iconize[# /. {\[Chi]c -> Coil\[Chi]c, t -> Coil\[Psi]c}, label]&]&

echoFnLabel[labelText_, labelIcon_] := Echo[#, labelText, Iconize[# /. {\[Chi]c -> Coil\[Chi]c, t -> Coil\[Psi]c}, labelIcon]&]&


(* ::Subsection::Closed:: *)
(*Loops*)


Options[FindLoopCoil] = FilterRules[findCoilOpts, Except["MinSeparationAndExtentDifference"]];
Options[findLoopChecks] = Options[FindLoopCoil];


FindLoopCoil::BadCurrents = finderMessages["BadCurrents"];
FindLoopCoil::BadDesired = finderMessages["BadDesired"];
FindLoopCoil::BadSeparations = finderMessages["BadSeparations"];
FindLoopCoil::BadLeadingError = finderMessages["BadLeadingError"];


findLoopChecks[i\[Chi]_, nDes_, minMax\[Chi]c_, opts:OptionsPattern[]] := Module[
	{proceed = True, optValNull, optValErr},
	(* Check that arguments have been specified correctly, and issue messages if not. *)
	
	(* Currents must be a list of one or more reals. *)
	If[!MatchQ[i\[Chi], l:{__?realQ} /; Length[l] >= 1],
		Message[FindLoopCoil::BadCurrents, i\[Chi]]; proceed = False];
	
	(* The desired harmonic must be an integer greater than zero. *)
	If[!MatchQ[nDes, n_Integer /; n > 0],
		Message[FindLoopCoil::BadDesired, nDes]; proceed = False];
	
	(* Check that 0 < min separation < max separation. *)
	If[!MatchQ[minMax\[Chi]c, {min_, max_} /; 0 < min < max],
		Message[FindLoopCoil::BadSeparations, minMax\[Chi]c]; proceed = False];
	
	(* Check that if custom nulled harmonics have been given, then a leading error has also been given. *)
	optValNull = OptionValue["NulledHarmonics"];
	optValErr = OptionValue["LeadingErrorHarmonic"];
	If[optValNull =!= Automatic && optValErr === Automatic,
		Message[FindLoopCoil::BadLeadingError]; proceed = False];
	
	proceed]


FindLoopCoil[i\[Chi]_, nDes_, minMax\[Chi]c_, opts:OptionsPattern[]] /; (
	CheckArguments[FindLoopCoil[i\[Chi], nDes, minMax\[Chi]c, opts], 3] &&
	findLoopChecks[i\[Chi], nDes, minMax\[Chi]c,
		Sequence @@ Normal[Merge[{Options[FindLoopCoil], opts}, Last]]]
) :=
	Module[{nNull, nErr, autoHarms, optValNull, optValErr, allOpts},
		
		optValNull = OptionValue["NulledHarmonics"];
		optValErr = OptionValue["LeadingErrorHarmonic"];
		
		(* Explicitly feed findSeparations all option->value pairs. This is incase the user has changed the default value of an option on FindLoopCoil,
			which needs to propagate through to findSeparations. *)
		allOpts = Sequence @@ Normal[Merge[{Options[FindLoopCoil], {opts}}, Last]];
		nNull = Replace[optValNull,
			Automatic :> (
				(* Calculate the nulled harmonics, and the leading error harmonic. *)
				(* Length[i\[Chi]] is clipped between 2 and inf because findSeparations performs a 1-primitive
					search differently, and it still needs harmonicsToNull to generate one nulled and
					one leading error harmonic in that case (which the code below does for Length[i\[Chi]] = 2). *)
				autoHarms = harmonicsToNull["Loop"][Clip[Length[i\[Chi]], {2, Infinity}] + 1, nDes];
				(* Only take the nulled harmonics for nNull. *)
				Drop[autoHarms, -1])];
		nErr = Replace[optValErr, Automatic :> Last[autoHarms]];
		findSeparations["Loop", i\[Chi], nDes, nNull, nErr, minMax\[Chi]c, {None, None}, allOpts]]


(* ::Subsection::Closed:: *)
(*Saddles*)


(* ::Text:: *)
(*The axial and azimuthal optimisations for saddle coils are orthogonal, so the user has the option to perform them both with FindSaddleCoil, or separately with FindSaddleCoilAxial and FindSaddleCoilAzimuthal.*)


(* Given that FindSaddleCoil calls FindSaddleCoilAxial and FindSaddleCoilAzimuthal, we keep the axial and azimuthal options distinct by
	starting all FindSaddleCoilAzimuthal options with "Azimuthal...". *)
azimuthalOpts = Join[
	(* FindRoot options for the azimuthal solver are given as strings, preprended with "Azimuthal". *)
	"Azimuthal" <> ToString[#1] -> #2 & @@@ Options[FindRoot],
	{
		"AzimuthalCoilsReturned" -> 10,
		"AzimuthalMeshPoints" -> 20,
		"AzimuthalSquashingFactor" -> .5,
		"AzimuthalSolutionTolerance" -> 10.^-5,
		"AzimuthalDuplicatesProximity" -> 10.^-4,
		"AzimuthalMinExtent" -> .1
	}];


Options[FindSaddleCoil] = Join[
	FilterRules[findCoilOpts, Except["MinSeparationAndExtentDifference"]],
	azimuthalOpts];
Options[findSaddleChecks] = Options[FindSaddleCoil];


Options[FindSaddleCoilAxial] = FilterRules[findCoilOpts, Except["MinSeparationAndExtentDifference"]];
Options[findSaddleAxialChecks] = Options[FindSaddleCoilAxial];


Options[FindSaddleCoilAzimuthal] = Append[azimuthalOpts, "PrintSteps" -> False];
Options[findSaddleAzimuthalChecks] = Options[FindSaddleCoilAzimuthal];


FindSaddleCoil::BadCurrents = finderMessages["BadCurrents"];
FindSaddleCoil::BadDesiredNM = finderMessages["BadDesiredNM"];
FindSaddleCoil::BadSeparations = finderMessages["BadSeparations"];
FindSaddleCoil::BadLeadingError = finderMessages["BadLeadingError"];
FindSaddleCoil::BadNulledDegrees = finderMessages["BadNulledDegrees"];
FindSaddleCoil::BadCurrentRatios = finderMessages["BadCurrentRatios"];


FindSaddleCoilAxial::BadCurrents = finderMessages["BadCurrents"];
FindSaddleCoilAxial::BadDesiredNM = finderMessages["BadDesiredNM"];
FindSaddleCoilAxial::BadSeparations = finderMessages["BadSeparations"];
FindSaddleCoilAxial::BadLeadingError = finderMessages["BadLeadingError"];
FindSaddleCoilAxial::BadNulledDegrees = finderMessages["BadNulledDegrees"];
FindSaddleCoilAxial::BadCurrentRatios = finderMessages["BadCurrentRatios"];


FindSaddleCoilAzimuthal::BadDesiredM = finderMessages["BadDesiredM"];
FindSaddleCoilAzimuthal::BadNulledDegrees = finderMessages["BadNulledDegrees"];


findSaddleAxialChecks[i\[Chi]_, {nDes_, mDes_}, k\[Phi]_, minMax\[Chi]c_, opts:OptionsPattern[]] := Module[
	{proceed = True, optValNull, optValErr},
	(* Check that arguments have been specified correctly, and issue messages if not. *)
	
	(* Currents must be a list of two or more reals. *)
	If[!MatchQ[i\[Chi], l:{__?realQ} /; Length[l] >= 1],
		Message[FindSaddleCoilAxial::BadCurrents, i\[Chi]]; proceed = False];
	
	(* nDes and mDes must be integers that satisfy n >= m > 0. *)
	If[!MatchQ[{nDes, mDes}, {n_Integer, m_Integer} /; n >= m > 0],
		Message[FindSaddleCoilAxial::BadDesiredNM, nDes, mDes]; proceed = False];
	
	(* If nDes + mDes is odd, then pairs of successive currents must be equal in magnitude and opposite in parity. *)
	If[OddQ[nDes + mDes] && (OddQ[Length[i\[Chi]]] || !AllTrue[Partition[i\[Chi], 2], Total[#] == 0 &]),
		Message[FindSaddleCoilAxial::BadCurrentRatios, nDes, mDes]; proceed = False];
	
	(* Check that kPhi is an integer >= 1. *)
	If[!MatchQ[k\[Phi], int_Integer /; int >= 1],
		Message[FindSaddleCoilAxial::BadNulledDegrees, k\[Phi]]; proceed = False];
	
	(* Check that 0 < min separation < max separation. *)
	If[!MatchQ[minMax\[Chi]c, {min_, max_} /; 0 < min < max],
		Message[FindSaddleCoilAxial::BadSeparations, minMax\[Chi]c]; proceed = False];
	
	(* Check that if custom nulled harmonics have been given, then a leading error has also been given. *)
	optValNull = OptionValue["NulledHarmonics"];
	optValErr = OptionValue["LeadingErrorHarmonic"];
	If[optValNull =!= Automatic && optValErr === Automatic,
		Message[FindSaddleCoilAxial::BadLeadingError]; proceed = False];
	
	proceed]


FindSaddleCoilAxial[i\[Chi]_, {nDes_, mDes_}, k\[Phi]_, minMax\[Chi]c_, opts:OptionsPattern[]] /; (
	findSaddleAxialChecks[i\[Chi], {nDes, mDes}, k\[Phi], minMax\[Chi]c,
		Sequence @@ Normal[Merge[{Options[FindSaddleCoilAxial], opts}, Last]]]
) :=
	Module[{nmErr, autoHarms, nmNull, optValNull, optValErr, allOpts},
		
		optValNull = OptionValue["NulledHarmonics"];
		optValErr = OptionValue["LeadingErrorHarmonic"];
		
		(* Ensure findSeparations inherits all option values from FindSaddleCoilAxial. *)
		allOpts = Sequence @@ Normal[Merge[{Options[FindSaddleCoilAxial], {opts}}, Last]];
		nmNull = Replace[optValNull,
			Automatic :> (
				(* Calculate the nulled harmonics, and the leading-order error harmonic. *)
				(* Length[i\[Chi]] is clipped between 2 and inf because findSeparations performs a 1-primitive
					search differently, and it still needs harmonicsToNull to generate one nulled and
					one leading error harmonic in that case (which the code below does for Length[i\[Chi]] = 2). *)
				autoHarms = harmonicsToNull["Saddle"][Clip[Length[i\[Chi]], {2, Infinity}] + 1, {nDes, mDes}, k\[Phi]];
				(* Only take the nulled harmonics for nmNull. *)
				Drop[autoHarms, -1])];
		nmErr = Replace[optValErr, Automatic :> Last[autoHarms]];
		findSeparations["Saddle", i\[Chi], {nDes, mDes}, nmNull, nmErr, minMax\[Chi]c, {None, None}, allOpts]]


findSaddleAzimuthalChecks[mDes_, k\[Phi]_] := Module[
	{proceed = True},
	(* Check that arguments have been specified correctly, and issue messages if not. *)
	
	(* mDes must be an integer greater than zero. *)
	If[!MatchQ[mDes, m_Integer /; m > 0],
		Message[FindSaddleCoilAzimuthal::BadDesiredM, mDes]; proceed = False];
	
	(* Check that kPhi is an integer >= 1. *)
	If[!MatchQ[k\[Phi], int_Integer /; int >= 1],
		Message[FindSaddleCoilAzimuthal::BadNulledDegrees, k\[Phi]]; proceed = False];
	
	proceed]


FindSaddleCoilAzimuthal[mDes_, k\[Phi]_, opts:OptionsPattern[]] /; (
	findSaddleAzimuthalChecks[mDes, k\[Phi]]
) :=
	Module[{allOpts},
		(* Ensure findAzimuthalExtents inherits all option values from FindSaddleCoilAzimuthal. *)
		allOpts = Sequence @@ Normal[Merge[{Options[FindSaddleCoilAzimuthal], {opts}}, Last]];
		findAzimuthalExtents[mDes, k\[Phi], allOpts]]


findSaddleChecks[i\[Chi]_, {nDes_, mDes_}, k\[Phi]_, minMax\[Chi]c_, opts:OptionsPattern[]] := Module[
	{proceed = True},
	(* Check that arguments have been specified correctly, and issue messages if not. *)
	
	(* Currents must be a list of one or more reals. *)
	If[!MatchQ[i\[Chi], l:{__?realQ} /; Length[l] >= 1],
		Message[FindSaddleCoil::BadCurrents, i\[Chi]]; proceed = False];
	
	(* nDes and mDes must be integers that satisfy n >= m > 0. *)
	If[!MatchQ[{nDes, mDes}, {n_Integer, m_Integer} /; n >= m > 0],
		Message[FindSaddleCoil::BadDesiredNM, nDes, mDes]; proceed = False];
	
	(* If nDes + mDes is odd, then pairs of successive currents must be equal in magnitude and opposite in parity. *)
	If[OddQ[nDes + mDes] && (OddQ[Length[i\[Chi]]] || !AllTrue[Partition[i\[Chi], 2], Total[#] == 0 &]),
		Message[FindSaddleCoil::BadCurrentRatios, nDes, mDes]; proceed = False];
	
	(* Check that 0 < min separation < max separation. *)
	If[!MatchQ[minMax\[Chi]c, {min_, max_} /; 0 < min < max],
		Message[FindSaddleCoil::BadSeparations, minMax\[Chi]c]; proceed = False];
	
	(* Check that if custom nulled harmonics have been given, then a leading error has also been given. *)
	optValNull = OptionValue["NulledHarmonics"];
	optValErr = OptionValue["LeadingErrorHarmonic"];
	If[optValNull =!= Automatic && optValErr === Automatic,
		Message[FindSaddleCoil::BadLeadingError]; proceed = False];
	
	(* Check that kPhi is an integer >= 1. *)
	If[!MatchQ[k\[Phi], int_Integer /; int >= 1],
		Message[FindSaddleCoil::BadNulledDegrees, k\[Phi]]; proceed = False];
	
	proceed]


FindSaddleCoil[i\[Chi]_, {nDes_, mDes_}, k\[Phi]_, minMax\[Chi]c_, opts:OptionsPattern[]] /; (
	CheckArguments[FindSaddleCoil[i\[Chi], {nDes, mDes}, k\[Phi], minMax\[Chi]c, opts], 4] &&
	findSaddleChecks[i\[Chi], {nDes, mDes}, k\[Phi], minMax\[Chi]c,
		Sequence @@ Normal[Merge[{Options[FindSaddleCoil], opts}, Last]]]
) :=
	Module[{nNull, nmErr, autoHarms, nmNull, optValNull, optValErr, allSeparationOpts, allAzimuthalOpts, separations, azimuthalExtents},
		
		optValNull = OptionValue["NulledHarmonics"];
		optValErr = OptionValue["LeadingErrorHarmonic"];
		
		(* Explicitly feed findAzimuthalExtents and findSeparations all of their respective option->value pairs. *)
		(* Separate the findAzimuthalExtents options (which all begin with "Azimuthal...") and the findSeparations options. *)
		{allAzimuthalOpts, allSeparationOpts} = GatherBy[
			(* Ensure GatherBy gives the azimuthal options first. *)
			{{"Azimuthal"}} ~Join~ Normal[Merge[{Options[FindSaddleCoil], {opts}}, Last]],
			StringQ[First[#]] && StringMatchQ[First[#], "Azimuthal*"]&];
		(* "PrintSteps" should apply to both solvers. *)
		AppendTo[allAzimuthalOpts, "PrintSteps" -> OptionValue["PrintSteps"]];
		allAzimuthalOpts = Sequence @@ Drop[allAzimuthalOpts, 1];
		allSeparationOpts = Sequence @@ allSeparationOpts;
		nNull = Replace[optValNull,
			Automatic :> (
				(* Calculate the nulled harmonics, and the leading error harmonic. *)
				(* Length[i\[Chi]] is clipped between 2 and inf because findSeparations performs a 1-primitive
					search differently, and it still needs harmonicsToNull to generate one nulled and
					one leading error harmonic in that case (which the code below does for Length[i\[Chi]] = 2). *)
				autoHarms = harmonicsToNull["Saddle"][Clip[Length[i\[Chi]], {2, Infinity}] + 1, nDes];
				(* Only take the nulled harmonics for nNull. *)
				Drop[autoHarms, -1])];
		nmNull = {#, mDes}& /@ nNull;
		nmErr = Replace[optValErr, Automatic :> {Last[autoHarms], mDes}];
		separations = FindSaddleCoilAxial[i\[Chi], {nDes, mDes}, k\[Phi], minMax\[Chi]c, allSeparationOpts];
		azimuthalExtents = FindSaddleCoilAzimuthal[mDes, k\[Phi], allAzimuthalOpts];
		<|"AxialSeparations" -> separations, "AzimuthalExtents" -> azimuthalExtents|>]


(* ::Subsection::Closed:: *)
(*Ellipses*)


Options[FindEllipseCoil] = Join[
	(* Use fewer mesh points for ellipses by default, given the doubled number of coil parameters. *)
	Replace[findCoilOpts, ("MeshPointsPer\[Chi]c" -> _) -> ("MeshPointsPer\[Chi]c" -> 10), 1]];
Options[findEllipseChecks] = Options[FindEllipseCoil];


FindEllipseCoil::BadCurrents = finderMessages["BadCurrents"];
FindEllipseCoil::BadDesiredNM = finderMessages["BadDesiredNM"];
FindEllipseCoil::BadSeparations = finderMessages["BadSeparations"];
FindEllipseCoil::BadExtents = finderMessages["BadExtents"];
FindEllipseCoil::BadLeadingError = finderMessages["BadLeadingError"];


findEllipseChecks[i\[Chi]_, {nDes_, mDes_}, minMax\[Chi]c_, minMaxT_, opts:OptionsPattern[]] := Module[
	{proceed = True},
	(* Check that arguments have been specified correctly, and issue messages if not. *)
	
	(* Currents must be a list of one or more reals. *)
	If[!MatchQ[i\[Chi], l:{__?realQ} /; Length[l] >= 1],
		Message[FindEllipseCoil::BadCurrents, i\[Chi]]; proceed = False];
	
	(* nDes and mDes must be integers that satisfy n >= m > 0. *)
	If[!MatchQ[{nDes, mDes}, {n_Integer, m_Integer} /; n >= m > 0],
		Message[FindEllipseCoil::BadDesiredNM, nDes, mDes]; proceed = False];
	
	(* Check that 0 < min separation < max separation. *)
	If[!MatchQ[minMax\[Chi]c, {min_, max_} /; 0 < min < max],
		Message[FindEllipseCoil::BadSeparations, minMax\[Chi]c]; proceed = False];
	
	(* Check that 0 < min tan < max tan. *)
	If[!MatchQ[minMaxT, {min_, max_} /; 0 < min < max],
		Message[FindEllipseCoil::BadExtents, minMaxT]; proceed = False];
	
	(* Check that if custom nulled harmonics have been given, then a leading error has also been given. *)
	optValNull = OptionValue["NulledHarmonics"];
	optValErr = OptionValue["LeadingErrorHarmonic"];
	If[optValNull =!= Automatic && optValErr === Automatic,
		Message[FindEllipseCoil::BadLeadingError]; proceed = False];
	
	proceed]


FindEllipseCoil[i\[Chi]_, {nDes_, mDes_}, minMax\[Chi]c_, minMaxT_, opts:OptionsPattern[]] /; (
	CheckArguments[FindEllipseCoil[i\[Chi], {nDes, mDes}, minMax\[Chi]c, minMaxT, opts], 4] &&
	findEllipseChecks[i\[Chi], {nDes, mDes}, minMax\[Chi]c, minMaxT,
		Sequence @@ Normal[Merge[{Options[FindEllipseCoil], opts}, Last]]]
) :=
	Module[{nmNull, nmErr, autoHarms, optValNull, optValErr, allOpts},
		
		optValNull = OptionValue["NulledHarmonics"];
		optValErr = OptionValue["LeadingErrorHarmonic"];
		
		(* Explicitly feed findSeparations all option->value pairs. *)
		allOpts = Sequence @@ Normal[Merge[{Options[FindEllipseCoil], {opts}}, Last]];
		nmNull = Replace[optValNull,
			Automatic :> (
				(* Calculate the nulled harmonics, and the leading error harmonic. *)
				(* Length[i\[Chi]] is clipped between 2 and inf because findSeparations performs a 1-primitive
					search differently, and it still needs harmonicsToNull to generate one nulled and
					one leading error harmonic in that case (which the code below does for Length[i\[Chi]] = 2). *)
				autoHarms = harmonicsToNull["Ellipse"][Clip[2 Length[i\[Chi]], {2, Infinity}] + 1, {nDes, mDes}];
				(* Only take the nulled harmonics for nmNull. *)
				Drop[autoHarms, -1])];
		nmErr = Replace[optValErr, Automatic :> Last[autoHarms]];
		findSeparations["Ellipse", i\[Chi], {nDes, mDes}, nmNull, nmErr, minMax\[Chi]c, minMaxT, allOpts]]


(* ::Subsection::Closed:: *)
(*Solvers*)


Options[findSeparations] = findCoilOpts;


findSeparations[
	topology_?(MatchQ["Loop" | "Saddle" | "Ellipse"]),
	i\[Chi]_,
	nmDes_,
	nmNull_,
	nmErr_,
	{min\[Chi]c_, max\[Chi]c_},
	{mint_, maxt_},
	opts:OptionsPattern[]
] :=
	Catch @ Block[
		(* Scope the separation and tangent variables *)
		{\[Chi]c, t},

		(* Ensure parallel kernels are running *)
		Quiet[LaunchKernels[]];
		Quiet[DistributeDefinitions["CreateCoil`"]];

		Module[
			{
				echo = echoFn[OptionValue["PrintSteps"]],
				ellipseQ = topology === "Ellipse",

				(* Parameters *)
				loopCount, nullCount, varCount, separations, tans, meshPoints, finalChecksQ,
				dupProx, nullThresh, minSepDiff, minSepTanDiff, expPoints, nearestPts, bleed, seed, coilsReturned, valsOnlyQ,

				(* Vars for storing calculated data *)
				contourDim, totalHarms, totalHarmDes, totalHarmsNull, totalHarmErr, findRoot, rawInitialSols, filteredInitialSols,
				initialMesh, nearestNeighbours, gradVs, spreadSizes, bases, contourMeshRaw, contourMesh, gradVsRaw,
				starts, finalSolsRaw, finalSols, coils},
			
			(* Ensure relevant definitions are accessible to all parallel kernels. *)
			(* Note that all these variables store small amounts of data (and thus passing them between kernels
				is not a problem). Avoid adding larger variables to this list. *)
			SetSharedVariable[loopCount, varCount, minSepDiff, nullThresh, separations, tans];


			(* Set up variables and parameters *)
			
			loopCount = Length[i\[Chi]];
			nullCount = Length[nmNull];
			(* In the ellipse case, each ellipse requires a separation and a tangent. The other topos only require separations. *)
			varCount = If[ellipseQ, 2 loopCount, loopCount];
			separations = Table[\[Chi]c[j], {j, loopCount}];
			If[ellipseQ,
				tans = Table[t[j], {j, loopCount}],
				tans = {}; t = None];
			(* Other option values *)
			meshPoints = OptionValue["MeshPointsPer\[Chi]c"];
			dupProx = If[Head[#] === Scaled, (max\[Chi]c - min\[Chi]c)First[#], #]&[
				OptionValue["DuplicatesProximity"]];
			nullThresh = OptionValue["NullingThreshold"];
			minSepDiff = If[Head[#] === Scaled, (max\[Chi]c - min\[Chi]c)First[#], #]&[
				OptionValue["MinSeparationsDifference"]];
			minSepTanDiff = If[Head[#] === Scaled, (max\[Chi]c - min\[Chi]c)First[#], #]&[
				OptionValue["MinSeparationAndExtentDifference"]];
			expPoints = Which[
				# === Automatic && ellipseQ, Ceiling[5/3 OptionValue["MeshPointsPer\[Chi]c"]],
				# === Automatic && !ellipseQ, Round[OptionValue["MeshPointsPer\[Chi]c"]],
				True, #]&[
					OptionValue["InterpolationPoints"]];
			nearestPts = If[# === Automatic, Replace[varCount - Length[nmNull], 0 -> 1], #]&[
				OptionValue["ContourMeshNN"]];
			bleed = OptionValue["ExpansionBleed"];
			seed = OptionValue["Seed"];
			coilsReturned = OptionValue["CoilsReturned"];
			valsOnlyQ = OptionValue["ValuesOnly"];
			finalChecksQ = TrueQ[OptionValue["FinalSolutionChecks"]];


			(* Dimensionality of the solution contour. *)
			contourDim = varCount - nullCount;

			(* Calculate expressions for the total desired, nulled, and leading error harmonics. *)
			totalHarms = With[{tBurn = t},
				ParallelMap[
					(* Using the absolute value of the harmonic seems to yield better coils. *)
					Abs @ totalHarmonic[i\[Chi], #, \[Chi]c, tBurn]&,
					Join[{nmDes}, nmNull, {nmErr}]]];

			totalHarmDes = echo[
				"Total desired harmonic",
				Row[{Switch[topology, "Loop", "n = ", _, "{n, m} = "], nmDes}]
			][totalHarms[[1]]];

			totalHarmsNull = echo[
				"Total nulled harmonics",
				Row[{Switch[topology, "Loop", "n = ", _, "{n, m} = "], Splice[Riffle[nmNull, ", "]]}]
			][totalHarms[[2 ;; -2]]];
			
			totalHarmErr = echo[
				"Total leading error harmonic",
				Row[{Switch[topology, "Loop", "n = ", _, "{n, m} = "], nmErr}]
			][totalHarms[[-1]]];

			(* Find separations at which the total nulled harmonics are all zero. These solutions
				lie on a (varCount - nullCount)-dimensional contour in varCount-dimensional space. *)
			With[
				{
					(* Find the options relevant for FindRoot. *)
					findRootOpts = Sequence @@ FilterRules[{opts}, Options[FindRoot]],
					(* For functions that require the \[Chi]c's to be fed in, followed by the t's, we need to construct a sequence
						of numbered slots to take a list of \[Chi]c1, \[Chi]c2, \[Chi]c3, ..., t1, t2, t3, ... arguments. *)
					\[Chi]cSlots = Sequence @@ Table[Slot[j], {j, loopCount}],
					tSlots = Sequence @@ If[ellipseQ,
						Table[Slot[j], {j, loopCount + 1, varCount}],
						{}],
					(* Ensure there are as many expressions as variables (this is a requirement for
						FindRoot) by padding the list of total nulled harmonics with zeros (i.e. an
						expression that's always a root). *)
					exprs = PadRight[totalHarmsNull, varCount, 0],
					(* Specify the variables, their initial guesses and their limits for FindRoot. The
						initial guess will be fed in via a numbered Slot. *)
					varsAndLimits = Catenate[{
						Table[{\[Chi]c[j], Slot[j], min\[Chi]c, max\[Chi]c}, {j, loopCount}],
						If[ellipseQ,
							Table[{t[j - loopCount], Slot[j], mint, maxt}, {j, loopCount + 1, 2 loopCount}],
							{}]}]},
				
				With[
					(* Ellipses: only operate on points for which \[Chi]c[n] - t[n] > minSepTanDiff *)
					{tanCheck = If[ellipseQ,
						And @@ (#1 - #2 >= minSepTanDiff &) @@@ Transpose[{{\[Chi]cSlots}, {tSlots}}],
						True]},

					(* Construct the FindRoot function. *)
					findRoot = echo["FindRoot function"][
						(* Only operate on points which satisfy \[Chi]c[1] < \[Chi]c[2] < \[Chi]c[3] < ... and other checks. *)
						Function @ If[
							Less[\[Chi]cSlots] && tanCheck,
							(* Quiet needed to suppress message noise from initial guesses that are far from the solution set. *)
							Quiet[FindRoot[exprs, varsAndLimits, findRootOpts]],
							Nothing]];
			
					(* Mesh the solution space. *)
					initialMesh = echo["Initial (coarse) mesh"][Flatten[
						Array[List,
							(* Number of points in each dimension *)
							If[varCount =!= 1,
								Table[meshPoints, varCount],
								(* If we only have one variable, then our 1D search space is the contour, so produce a dense mesh. *)
								{meshPoints * expPoints}],
							(* Start and end values for each dimension *)
							Catenate[{
								Table[{min\[Chi]c, max\[Chi]c}, loopCount],
								If[ellipseQ,
									Table[{mint, maxt}, loopCount],
									{}]}]],
						(* Flatten level *)
						varCount - 1]];
					
					(* Apply findRoot to the mesh. *)
					rawInitialSols = echo["Initial solutions (raw)"][
						ParallelMap[Apply[findRoot], initialMesh]];
					
					filteredInitialSols = echo["Initial solutions (filtered)"][
						(* Remove duplicate points (considered to be duplicates if within a given distance). This is an
							attempt to make the solution mesh more evenly spaced for when we interpolate it. *)
						DeleteDuplicates[
							Part[
								(* Select solutions for which: \[Chi]c1 < \[Chi]c2 < \[Chi]c3 < ..., the separations are no closer to each
									other than minSepDiff, and the total nulled harmonics are small. *)
								Select[
									rawInitialSols,
									Function[sol, And[
										Less @@ separations /. sol,
										DuplicateFreeQ[separations /. sol, Abs[#2-#1] < minSepDiff &],
										AllTrue[totalHarmsNull /. sol, # < nullThresh &]]]],
								(* Sols are in form {\[Chi]c[1] -> a, \[Chi]c[2] -> b, ...} so [[All, All, -1]] converts them to
									{a, b, ...} (i.e. an array of numbers). *)
								All, All, -1],
							(* Delete a point if it is within dupProx. *)
							Norm[#2 - #1] < dupProx &]];
					
					(* Return {} if no legal solutions were found. *)
					If[filteredInitialSols === {}, Throw[{}]];
						
					If[varCount === 1,
						(* If the search space is one-dimensional, then we don't need to find a solution contour, so we are done. *)
						finalSols = filteredInitialSols,

						(* Otherwise, perform a further search. *)
						Switch[filteredInitialSols,
							(* If only one solution was found, expand a cloud of points around it. *)
							{_},
							contourMesh = Flatten[
								Array[First[filteredInitialSols] + List[##] &,
									Table[meshPoints, varCount],
									(* The point cloud should fit within the grain size of the coarse mesh. *)
									Catenate[{
										Table[{-1, 1}(max\[Chi]c - min\[Chi]c)/(meshPoints - 1), loopCount],
										If[ellipseQ,
											Table[{-1, 1}(maxt - mint)/(meshPoints - 1), loopCount],
											{}]}]],
								(* Flatten level *)
								varCount - 1];
							(* Remove points outside of the allowed search ranges. *)
							contourMesh = echo["Cloud Mesh (Only 1 initial sol found)"][Select[contourMesh,
								And @@ Construct @@@ Transpose[{
									Join[
										Table[min\[Chi]c <= # <= max\[Chi]c &, loopCount],
										If[ellipseQ, Table[mint <= # <= maxt &, loopCount], {}]],
									#}]&]],
							(* Otherwise we have multiple points between which we can interpolate. *)
							_,
							(* Perform a very rough (but easy to calculate) mesh interpolation of the solution contour. These points
								will be the new guesses for findRoot (hence why they only need to be near the contour, and why the
								interpolation can be rough). *)
							(* Around each solution, expand an array of points along the solution contour, matching the contour's
								dimensionality. Firstly, find each point's nearestPts*n nearest neighbours (where n is the contour
								dimensionality) so we can approximate the gradient at that point. *)
							nearestNeighbours =
								(* [[All, 2;;]] needed because the first nearest point will be the point itself. *)
								Nearest[filteredInitialSols, filteredInitialSols, nearestPts contourDim + 1][[All, 2;;]];
							(* Find the contour gradient vectors. *)
							gradVsRaw = MapThread[
								Function[{sol, nn}, # - sol & /@ nn],
								{filteredInitialSols, nearestNeighbours}];
							(* We want the vectors to be as orthoginal as possible in order to span the contour accurately. Therefore,
								we found nearestPts*n vectors (n is contour dimensionality) and will now find the most orthogonal n
								vectors in that set. *)
							(* 1. Pick the shortest vector (i.e. the nearest point) to start the best set of n vectors, and group the
								rest. i.e. {vShortest, {vOthers}} *)
							starts = {First[#], Rest[#]}& /@ gradVsRaw;
							gradVs = Map[
								Function[start,
									Reap[
										Sow[First[start]];
										Nest[
											Function[vects,
												(* 3. Pick the most orthogonal (and save it by sowing it), remove the worst ones, and group
													the rest as before ({v2, {vothers}}). *)
												{Sow[First[#]], Drop[Rest[#], -(nearestPts - 1)]}&[
													(* 2. Sort the other vectors by how orthogonal they are to the first vector. *)
													SortBy[Last[vects], Abs[Dot[Normalize[First[vects]], Normalize[#]]]&]]],
											(* 4. Recurse one less times than the contour dimensionality, which will give contourDim vectors. *)
											start, contourDim - 1
										(* 5. Extract the vectors from Reap. *)
										]][[-1, 1]]],
								starts];
							(* When expanding around a solution, use a spread size given by the furthest of the nearest neighbours
								to ensure there are no large gaps in the mesh. (expPoints-1)/expPoints is needed so that there is a
								gap between the edges of each expansion, which would otherwise coincide. *)
							spreadSizes = ((expPoints-1)/expPoints)(Max /@ Map[Norm, gradVs, {2}]);
							(* Orthonormalize the contour gradient vectors. *)
							bases = Orthogonalize /@ gradVs;
							(* Now expand around each solution. *)
							SeedRandom[seed];
							contourMeshRaw = With[
								{slots = Slot /@ Range[contourDim]},
								MapThread[
									Function[{sol, basisVs, spread},
										Array[
											(* Add to the solution, the sum of each basis vector times the distance for that
												vector given by the array. Perturb the points slightly to even out the overall mesh. *)
											sol + Total[RandomReal[{-1,1}spread/(expPoints + 1)/2, contourDim] + basisVs slots]&,
											(* An array of length expPoints in each contour dimension... *)
											Table[expPoints, contourDim],
											(* ...between -spread/2 and spread/2. *)
											Table[spread bleed{-.5, .5}, contourDim]]],
									{filteredInitialSols, bases, spreadSizes}]];
							(* Remove points outside of the allowed separations. *)
							contourMesh = echo["Contour Mesh"][
								Select[
									Flatten[contourMeshRaw, contourDim],
									And[
										AllTrue[#[[;; loopCount]], min\[Chi]c <= # <= max\[Chi]c &],
										If[ellipseQ,
											AllTrue[#[[loopCount + 1 ;; varCount]], mint <= # <= maxt &],
											True]]&]]];
						
						(* Apply findRoot to the new mesh. *)
						finalSolsRaw = echo["Final solutions (unfiltered)"][
							ParallelMap[Apply[findRoot], contourMesh][[All, All, -1]]];
						
						(* Remove illegal solutions, as before. *)
						finalSols = If[finalChecksQ,
							With[
								{check = And @@ Join[
									{Less[\[Chi]cSlots], tanCheck},
									If[loopCount === 1, {},
										MovingMap[Apply[Abs @* Subtract, #] >= minSepDiff &, {\[Chi]cSlots}, 1]]]},
								ParallelMap[
									Apply[Function @ If[check, {##}, Nothing]],
									finalSolsRaw]],
							finalSolsRaw]];
					
					(* Rank solutions by the ratio of the desired harmonic to the leading error harmonic. *)
					coils = With[
						{
							rules = Rule @@@ Transpose[{Join[separations, tans], Join[{\[Chi]cSlots}, {tSlots}]}],
							slots = If[ellipseQ,
								Transpose[{{\[Chi]cSlots}, {tSlots}}],
								{\[Chi]cSlots}],
							partSpec = Replace[coilsReturned, Except[All] -> Span[1, UpTo[coilsReturned]]]},
						ReverseSortBy[
							Function[
								Append[
									rules /. {\[Chi]c -> Coil\[Chi]c, t -> Coil\[Psi]c},
									DesToErr -> Abs[totalHarmDes/totalHarmErr /. rules]]] @@@ finalSols,
							Last][[partSpec]]];
					
					(* If "ValuesOnly" -> True, then only return the separation (and tan) values. *)
					If[valsOnlyQ,
						Function[coil,
							If[ellipseQ, Identity, Catenate][
								GatherBy[Drop[coil, -1], #[[1, 1]]&][[All, All, 2]]]] /@ coils,
						coils]]]]]


azimuthalTerm[\[Phi]c_, m_] := ChebyshevU[m - 1, \[Phi]c] Sqrt[1 - \[Phi]c^2]


Options[findAzimuthalExtents] = Append[azimuthalOpts, "PrintSteps" -> False];


findAzimuthalExtents[mDes_, k\[Phi]_, opts:OptionsPattern[]] :=
	Module[
		{
			mNull, cos\[Phi], extentCosines, eqs, meshPoints, meshPointCount, squashingFactor, max\[Phi],
			lin, sols, echo, findRootOpts, solTolerance, coilsReturned, partSpec, duplicatesDist, minExtent},

		(* Ensure parallel kernels are running *)
		Quiet[LaunchKernels[]];
		Quiet[DistributeDefinitions["CreateCoil`"]];

		meshPointCount = OptionValue["AzimuthalMeshPoints"];
		squashingFactor = OptionValue["AzimuthalSquashingFactor"];
		solTolerance = OptionValue["AzimuthalSolutionTolerance"];
		coilsReturned = OptionValue["AzimuthalCoilsReturned"];
		duplicatesDist = OptionValue["AzimuthalDuplicatesProximity"];
		minExtent = OptionValue["AzimuthalMinExtent"];

		(* The FindRoot options of findAzimuthalExtents are of the form "Azimuthal" <> ToString[-FindRoot option-].
			Hence we need to select options which begin "Azimuthal...", and assign their values to the corresponding FindRoot options. *)
		findRootOpts = Options[FindRoot][[All, 1]];
		findRootOpts = Function[{azimOpt, val},
			FirstCase[findRootOpts,
				opt_ /; ("Azimuthal" <> ToString[opt] === azimOpt) :> (opt -> val),
				Nothing, 1]] @@@ {opts};
		findRootOpts = Sequence @@ findRootOpts;

		echo = echoFn[OptionValue["PrintSteps"]];
		(* Generate the degrees to null: mDes * 3, 5, 7, ...; k\[Phi] long. *)
		mNull = mDes(2 Range[k\[Phi]] + 1);
		extentCosines = cos\[Phi] /@ Range[k\[Phi]];
		eqs = Total /@ Table[azimuthalTerm[j, i], {i, mNull}, {j, extentCosines}];
		echo["Equations in cos(\[Phi])"][eqs /. cos\[Phi] -> Slot];
		(* Mesh the cos(\[Phi]) space, applying FindRoot to each k\[Phi]-dimensional point. *)
		(* Phi is half the azimuthal extent, so \[Phi] may extend from 0 to \[Pi]/(2 mDes). *)
		max\[Phi] = Pi/(2 mDes);
		(* Ideally, we want the \[Phi]'s to be equally spaced within the 0 to max\[Phi] region (with padding on each end).
			First, linearly interpolate the region 0 to 1, with k\[Phi] points and padding each end: *)
		lin = Array[Identity, k\[Phi] + 2, {0, 1}][[2 ;; -2]];
		(* Now, produce a mesh of k\[Phi]-dimensional points by squashing the linear interpolation towards 0, through even spacing, to 1.
			We use this non-linear mesh in order to only mesh around the desired spacing of \[Phi]'s, rather than across the entire solution space. *)
		meshPoints = Array[
			(* Use a squashing function that's symmetric about y = x and y = 1 - x so that squashing towards 0 and towards 1 are the same
				(i.e. the mesh points squashed towards 0 are symmetric about 1/2 to the mesh points squashed towards 1). *)
			Piecewise[{
				{(1 - (1-lin)^-(#-1)) ^ (-1/(#-1)), # < 0},
				{lin, # == 0},
				{1 - (1 - lin^(#+1)) ^ (1/(#+1)), 0 < #}}]&,
			meshPointCount,
			squashingFactor {-1, 1}];
		(* We have found the desired mesh points for \[Phi], but our equations are in terms of cos(\[Phi]), so take cosine of each value. *)
		meshPoints = Map[Cos] /@ meshPoints;
		(* Apply FindRoot to each mesh point. *)
		sols = ParallelMap[
			(* Burn in parameter values to ensure all required data are passed to each parallel kernel. *)
			With[{eqs = eqs, extentCosines = extentCosines, findRootOpts = findRootOpts},
				Quiet @ FindRoot[eqs, Transpose[{extentCosines, #}], findRootOpts]&],
			meshPoints];
		(* Keep only the solutions which solve the equations to the desired tolerance. *)
		sols = Select[sols,
			Function[sol, AllTrue[eqs /. sol, Quiet[# < solTolerance]&]]];
		If[sols === {}, Return[{}]];
		(* We've solved for cos(\[Phi]), so take the ArcCos of each point. *)
		sols = Apply[Chop @ Quiet @ ArcCos[#2] &, sols, {2}];
		(* Filter out solutions with extents less than minExtent and more than max\[Phi]. *)
		sols = Select[sols, AllTrue[max\[Phi] > # > minExtent &]];
		(* Filter out duplicates. *)
		sols = DeleteDuplicates[sols, SameQ @@ Round[{##}, duplicatesDist] &];
		If[sols === {}, Return[{}]];
		(* Sort the extents *)
		sols = Sort /@ sols;
		(* Sort coils by their ease of manufacture, i.e. how close they are to being evenly distributed through max\[Phi]. *)
		sols = Nearest[sols, lin * (Pi / (2 mDes)), All];
		(* Label each extent. *)
		sols = MapIndexed[Coil\[Phi]c[First[#2]] -> #1 &] /@ sols;
		(* Return only the requested number of solutions. *)
		partSpec = Replace[coilsReturned, Except[All] -> Span[1, UpTo[coilsReturned]]];
		sols[[partSpec]]]


(* ::Section::Closed:: *)
(*Coil Data*)





(* ::Section::Closed:: *)
(*Coil Plots*)


reflectX[prim_, x_] := GeometricTransformation[prim, ReflectionTransform[{-1, 0}, {x, 0}]]
reflectX[x_] := GeometricTransformation[#, ReflectionTransform[{-1, 0}, {x, 0}]]&
reflectX3D[prim_, x_] := GeometricTransformation[prim, ReflectionTransform[{-1, 0, 0}, {x, 0, 0}]]
reflectX3D[x_] := GeometricTransformation[#, ReflectionTransform[{-1, 0, 0}, {x, 0, 0}]]&


reflectY[prim_, y_] := GeometricTransformation[prim, ReflectionTransform[{0, -1}, {0, y}]]
reflectY[y_] := GeometricTransformation[#, ReflectionTransform[{0, -1}, {0, y}]]&
reflectY3D[prim_, y_] := GeometricTransformation[prim, ReflectionTransform[{0, -1, 0}, {0, y, 0}]]
reflectY3D[y_] := GeometricTransformation[#, ReflectionTransform[{0, -1, 0}, {0, y, 0}]]&


reflectZ3D[prim_, z_] := GeometricTransformation[prim, ReflectionTransform[{0, 0, -1}, {0, 0, z}]]
reflectZ3D[z_] := GeometricTransformation[#, ReflectionTransform[{0, 0, -1}, {0, 0, z}]]&


arrowHead = Graphics[{Red, RegularPolygon[{1, 0}, 3]}];
arrowHead3D = Graphics3D[{FaceForm[Red], EdgeForm[None], Cone[{{-.25, 0, 0}, {.75, 0, 0}}, .3]}];


addHoverTube[tubeRadius_, hoverRadius_, path_, drawnTubeRadius_:#] := {
	(* Draw the arrows. *)
	Arrow @ Tube[path, drawnTubeRadius&[tubeRadius]],
	(* If the thickness of the arrow is less than "HoverThickness", then add an invisible copy with
		thickness "HoverThickness" to ensure the primitive has a reasonable mouse hover region. *)
	If[TrueQ[tubeRadius < hoverRadius], {FaceForm[], Tube[path, hoverRadius]}, Nothing]
}


coilCylinder[\[Rho]c_, z_, plotPoints_] := Module[
	{bottomDisk, topDisk, sideSurface}, 
	{bottomDisk, topDisk} =
		Through[(Map[Append[# \[Rho]c z]]& /@ {-1, 1})[CirclePoints[{\[Rho]c, 0}, plotPoints - 1]]];
	sideSurface = MovingMap[
		Apply[List @@ Join[#1, Reverse[#2]]&],
		inert @@@ (Append[#, First[#]]& @ Transpose[{bottomDisk, topDisk}]),
		1];
	(* Reduce the radii of the top and bottom disks slightly so they don't obscure
		any wires running around the top or bottom of the coil. *)
	{bottomDisk, topDisk} = Map[Apply[{.995 #1, .995 #2, #3}&]] /@ {bottomDisk, topDisk};
	{Polygon[{bottomDisk, topDisk}], EdgeForm[], Polygon[sideSurface]}]



(* Arrow heads get too large if they scale linearly with current. *)
scaleHead[s_] := .075 s^.5


(* Due to a bug in the Mathematica front end, where ImagePadding -> All does not account for arrowheads and
	thus arrowheads near the edges of plots can get clipped, we have to calculate the appropriate plot
	padding ourselves. *)
plotRangePaddingY[gWidth_, arrowheadS_, i\[Chi]_] :=
	(* Calculate the padding for the outermost primitive. *)
	2 gWidth scaleHead[arrowheadS Last @ Abs[i\[Chi]]]


processPlotStyle[style_, primCount_] := Switch[
	style,
	(* PlotStyle -> Automatic uses the default colour set, ColorData[97]. *)
	Automatic, ColorData[97] /@ Range[primCount],
	(* PlotStyle -> {s1, s2, ...} cycles the si over the sets of primitives. *)
	_List, PadRight[{}, primCount, style],
	(* PlotStyle -> s uses s for all sets of primitives. *)
	_, Table[style, primCount]]


coilPlot2DFrameLabel = {
	{
		TraditionalForm @ RawBoxes["\"\\!\\(\\*StyleBox[\\\"z\\\",FontSlant->\\\"Italic\\\"]\\) (m)\""],
		None},
	{
		TraditionalForm @ RawBoxes["\"\\!\\(\\*SubscriptBox[\\\"\[Rho]\\\", StyleBox[\\\"c\\\",FontSlant->\\\"Italic\\\"]]\\)\[ThinSpace]\[Phi] (m)\""],
		None}};


coilPlot3DAxesLabel = {
	TraditionalForm @ RawBoxes["\"\\!\\(\\*StyleBox[\\\"x\\\",FontSlant->\\\"Italic\\\"]\\) (m)\""],
	TraditionalForm @ RawBoxes["\"\\!\\(\\*StyleBox[\\\"y\\\",FontSlant->\\\"Italic\\\"]\\) (m)\""],
	TraditionalForm @ RawBoxes["\"\\!\\(\\*StyleBox[\\\"z\\\",FontSlant->\\\"Italic\\\"]\\) (m)\""]};


coilGraphic2DOpts = Normal @ Merge[
	{
		Options[Graphics],
		{
			ImageSize -> Large,
			PlotRange -> All,
			Frame -> True,
			FrameLabel -> coilPlot2DFrameLabel,
			"ThicknessScaling" -> .0015,
			"ArrowheadScaling" -> .005,
			PlotStyle -> Black
		}
	},
	Last];


coilGraphic3DOpts = Normal @ Merge[
	{
		Options[Graphics3D],
		{
			ImageSize -> Large,
			PlotRange -> All,
			Boxed -> False,
			Axes -> True,
			AxesOrigin -> Automatic,
			AxesLabel -> coilPlot3DAxesLabel,
			PlotPoints -> 100,
			"ThicknessScaling" -> .002,
			"ArrowheadScaling" -> .03,
			"HoverThickness" -> .007,
			PlotStyle -> Black,
			"ShowCylinder" -> True,
			"CylinderStyle" -> {EdgeForm[{GrayLevel[.8], Thickness[.001]}], FaceForm[GrayLevel[.95, .7]]},
			Show -> None
		}
	},
	Last];


(* ::Subsection::Closed:: *)
(*Dynamic Plot Elements*)


dynLoop[prim_, \[Chi]c\[Rho]c_, i\[Chi]_, transform_, Dynamic[tracker_], style_] := dynPrimByKey[
	prim,
	{
		{
			"\"\\!\\(\\*SubscriptBox[\\\"\[Chi]\\\", StyleBox[\\\"c\\\",FontSlant->\\\"Italic\\\"]]\\)\[ThinSpace]\\!\\(\\*SubscriptBox[\\\"\[Rho]\\\", StyleBox[\\\"c\\\",FontSlant->\\\"Italic\\\"]]\\)\"",
			Row[{\[Chi]c\[Rho]c, " (m)"}]},
		{
			"\"\\!\\(\\*SubscriptBox[StyleBox[\\\"i\\\",FontSlant->\\\"Italic\\\"], \\\"\[Chi]\\\"]\\)\"",
			Row[{i\[Chi], " (A)"}]}
	},
	transform,
	{\[Chi]c\[Rho]c, i\[Chi]},
	Dynamic[tracker],
	style]


dynSaddleText[\[Chi]c\[Rho]c:{_, _}, i\[Chi]_, \[Phi]c\[Rho]c_] := {
	{
		"\"\\!\\(\\*SubscriptBox[\\\"\[Chi]\\\", StyleBox[\\\"c\\\",FontSlant->\\\"Italic\\\"]]\\)\[ThinSpace]\\!\\(\\*SubscriptBox[\\\"\[Rho]\\\", StyleBox[\\\"c\\\",FontSlant->\\\"Italic\\\"]]\\)\"",
		Row[{\[Chi]c\[Rho]c[[1]], ", ", \[Chi]c\[Rho]c[[2]], " (m)"}]},
	{
		"\!\(\*SubscriptBox[\"\[Phi]\", StyleBox[\"c\",FontSlant->\"Italic\"]]\)\[ThinSpace]\!\(\*SubscriptBox[\"\[Rho]\", StyleBox[\"c\",FontSlant->\"Italic\"]]\)",
		Row[{\[Phi]c\[Rho]c, " (m)"}]},
	{
		"\"\\!\\(\\*SubscriptBox[StyleBox[\\\"i\\\",FontSlant->\\\"Italic\\\"], \\\"\[Chi]\\\"]\\)\"",
		Row[{i\[Chi], ", ", -i\[Chi], " (A)"}]}}


dynSaddleText[\[Chi]c\[Rho]c_?NumberQ, i\[Chi]_, \[Phi]c\[Rho]c_] := {
	{
		"\"\\!\\(\\*SubscriptBox[\\\"\[Chi]\\\", StyleBox[\\\"c\\\",FontSlant->\\\"Italic\\\"]]\\)\[ThinSpace]\\!\\(\\*SubscriptBox[\\\"\[Rho]\\\", StyleBox[\\\"c\\\",FontSlant->\\\"Italic\\\"]]\\)\"",
		Row[{\[Chi]c\[Rho]c, " (m)"}]},
	{
		"\!\(\*SubscriptBox[\"\[Phi]\", StyleBox[\"c\",FontSlant->\"Italic\"]]\)\[ThinSpace]\!\(\*SubscriptBox[\"\[Rho]\", StyleBox[\"c\",FontSlant->\"Italic\"]]\)",
		Row[{\[Phi]c\[Rho]c, " (m)"}]},
	{
		"\"\\!\\(\\*SubscriptBox[StyleBox[\\\"i\\\",FontSlant->\\\"Italic\\\"], \\\"\[Chi]\\\"]\\)\"",
		Row[{i\[Chi], " (A)"}]}}


dynSaddle2D[prim_, \[Chi]c\[Rho]c_, i\[Chi]_, \[Phi]c\[Rho]c_, transform_, Dynamic[epilog_], Dynamic[epilogOpt_], style_] := dynPrimByEpilog[
	prim,
	dynSaddleText[\[Chi]c\[Rho]c, i\[Chi], \[Phi]c\[Rho]c],
	transform,
	Dynamic[epilog],
	Dynamic[epilogOpt],
	style]


dynSaddle3D[prim_, \[Chi]c\[Rho]c_, i\[Chi]_, \[Phi]c\[Rho]c_, transform_, Dynamic[tracker_], style_] := dynPrimByKey[
	prim,
	dynSaddleText[\[Chi]c\[Rho]c, i\[Chi], \[Phi]c\[Rho]c],
	transform,
	{\[Chi]c\[Rho]c, \[Phi]c\[Rho]c, i\[Chi]},
	Dynamic[tracker],
	style]


dynEllipse[prim_, \[Chi]c\[Rho]c_, \[Psi]c\[Rho]c_, i\[Chi]_, transform_, Dynamic[tracker_], style_] := dynPrimByKey[
	prim,
	{
		{
			"\"\\!\\(\\*SubscriptBox[\\\"\[Chi]\\\", StyleBox[\\\"c\\\",FontSlant->\\\"Italic\\\"]]\\)\[ThinSpace]\\!\\(\\*SubscriptBox[\\\"\[Rho]\\\", StyleBox[\\\"c\\\",FontSlant->\\\"Italic\\\"]]\\)\"",
			Row[{\[Chi]c\[Rho]c, " (m)"}]},
		{
			"\!\(\*SubscriptBox[\"\[Psi]\", StyleBox[\"c\",FontSlant->\"Italic\"]]\)\[ThinSpace]\!\(\*SubscriptBox[\"\[Rho]\", StyleBox[\"c\",FontSlant->\"Italic\"]]\)",
			Row[{\[Psi]c\[Rho]c, " (m)"}]},
		{
			"\"\\!\\(\\*SubscriptBox[StyleBox[\\\"i\\\",FontSlant->\\\"Italic\\\"], \\\"\[Chi]\\\"]\\)\"",
			Row[{i\[Chi], " (A)"}]}
	},
	transform,
	{\[Chi]c\[Rho]c, \[Psi]c\[Rho]c, i\[Chi]},
	Dynamic[tracker],
	style]


dynPrimByKey[prim_, label:{{_, _}..}, transform_, key_, Dynamic[tracker_], style_] := Tooltip[
	EventHandler[
		{style, Dynamic[If[tracker === key, Red, {}]], transform[prim], prim},
		{"MouseEntered" :> (tracker = key), "MouseExited" :> (tracker = None)}],
	Pane[
		TraditionalForm @ Grid[{RawBoxes[#1], " = ", #2}& @@@ label, Alignment -> Left],
		FrameMargins -> {5{1, 1}, 2{1, 1}}]]


dynPrimByEpilog[prim_, label:{{_, _}..}, transform_, Dynamic[epilog_], Dynamic[epilogOpt_], style_] := With[
	{primPair = {transform[prim], prim}},
	Tooltip[
		EventHandler[Prepend[primPair, style], {
			"MouseEntered" :> (epilog = {{epilogOpt}, style, Red, primPair}),
			"MouseExited" :> (epilog = {epilogOpt})}],
		Pane[
			TraditionalForm @ Grid[{RawBoxes[#1], " = ", #2}& @@@ label, Alignment -> Left],
			FrameMargins -> {5{1, 1}, 2{1, 1}}]]]


(* ::Subsection::Closed:: *)
(*Loops*)


loopPlotChecks[head_][\[Chi]c_, i\[Chi]_, \[Rho]c_, nDes_] := Module[
	{proceed = True},
	(* Check that arguments have been specified correctly, and issue messages if not. *)

	(* Separations must either be a list of one or more positive reals in ascending order, or a list of
		Coil\[Chi]c[...] -> ... rules (can contain a DesToErr -> ... rule, which will be ignored). *)
	If[
		!MatchQ[\[Chi]c, Alternatives[

			{_?Positive},

			l:{__?Positive} /; (Length[l] >= 2 && AllTrue[Differences[l], Positive]),

			l:{(Coil\[Chi]c[_] | DesToErr -> _?Positive)..} /; With[
				{indicesAndVals = Cases[l, (Coil\[Chi]c[i_] -> val_) :> {i, val}]},
				TrueQ @ And[
					(* Only one (or none) DesToErr rule. *)
					Length[l] - Length[indicesAndVals] < 2,
					(* Coil\[Chi]c indices must be consecutive ascending integers, starting from 1. *)
					Sort[indicesAndVals[[All, 1]]] == Range[Length[indicesAndVals]],
					(* Coil\[Chi]c values, as sorted by index, must be ascending positive numbers. *)
					AllTrue[Differences[SortBy[indicesAndVals, First][[All, 2]]], Positive]
			]]
		]],
		Message[head::BadSeparations, \[Chi]c]; proceed = False];
	
	(* Currents must be a list of reals, equal in length to the number of separations. *)
	If[!MatchQ[i\[Chi], l:{__?realQ} /; Length[l] === Length[DeleteCases[\[Chi]c, DesToErr -> _]]],
		Message[head::BadCurrents, i\[Chi]]; proceed = False];
	
	(* The desired harmonic must be an integer greater than zero. *)
	If[!MatchQ[nDes, n_Integer /; n > 0],
		Message[head::BadDesired, nDes]; proceed = False];
	
	(* Radius must be positive. *)
	If[!Positive[\[Rho]c],
		Message[head::BadRadius, \[Rho]c]; proceed = False];
	
	proceed]


(* ::Subsubsection::Closed:: *)
(*2D*)


Options[LoopCoilPlot] = coilGraphic2DOpts;


LoopCoilPlot::BadSeparations = plotMessages["BadSeparations"];
LoopCoilPlot::BadCurrents = plotMessages["BadCurrents"];
LoopCoilPlot::BadDesired = plotMessages["BadDesired"];
LoopCoilPlot::BadRadius = plotMessages["BadRadius"];


LoopCoilPlot[\[Chi]c_, i\[Chi]_, \[Rho]c_, nDes_, opts:OptionsPattern[]] /; (
	CheckArguments[LoopCoilPlot[\[Chi]c, i\[Chi], \[Rho]c, nDes, opts], 4] &&
	loopPlotChecks[LoopCoilPlot][\[Chi]c, i\[Chi], \[Rho]c, nDes]
) :=
	Module[{\[Chi]cVals, thicknessS, arrowheadS, allOpts},
		
		(* Explicitly feed loopGraphic2D all option->value pairs. This is incase the user has changed the default value of an option on LoopCoilPlot,
			which needs to propagate through to loopGraphic2D. *)
		allOpts = Sequence @@ Normal[Merge[{Options[LoopCoilPlot], {opts}}, Last]];
		(* If \[Chi]c is a list of Coil\[Chi]c[index] -> val rules, then sort by index and take the vals. *)
		\[Chi]cVals = Replace[\[Chi]c, l:{__Rule} :> SortBy[
			Cases[l, (Coil\[Chi]c[i_] -> val_) :> {i, val}],
			First][[All, 2]]];
		thicknessS = OptionValue["ThicknessScaling"];
		arrowheadS = OptionValue["ArrowheadScaling"];
		loopGraphic2D[\[Chi]cVals, i\[Chi], \[Rho]c, nDes, thicknessS, arrowheadS, allOpts]]


loopGraphic2D[\[Chi]c_, i\[Chi]_, \[Rho]c_, nDes_, thicknessS_, arrowheadS_, opts___] := Module[
	{styles, gPrims, symTransform},

	styles = processPlotStyle[
		Lookup[Association[opts], PlotStyle, Black],
		Length[\[Chi]c]];

	(* Reverse the direction of the -ve z primitives' currents if the coil is axially antisymmetric. *)
	symTransform = If[EvenQ[nDes], reflectX[Pi \[Rho]c], Identity];

	DynamicModule[{tracker},
		(* Construct the primitives with +ve z coords. The thickness of each primitive is proportional to i\[Chi]. *)
		gPrims = MapThread[

			Function[{\[Chi]cp, i\[Chi]p, flip, style},
				dynLoop[

					flip @ {
						(* Thickness *)
						Thickness[thicknessS Abs[i\[Chi]p]],
						(* Arrowheads (also scaled by i\[Chi]) *)
						Arrowheads[Table[{scaleHead[arrowheadS Abs[i\[Chi]p]], pos, arrowHead}, {pos, .25, .75, .25}]],
						(* Arrow *)
						Arrow[\[Rho]c {{0, \[Chi]cp}, {2 Pi, \[Chi]cp}}]},

					\[Rho]c \[Chi]cp, i\[Chi]p,
					(* Add the primitive with -ve z coords, accounting for the symmetry/antisymmetry of the coil. *)
					reflectY[0] @* symTransform,
					Dynamic[tracker],
					style]],

			{
				\[Chi]c,
				i\[Chi],
				i\[Chi] /. {_?Negative -> reflectX[Pi \[Rho]c], _?Positive -> Identity},
				styles
			}];
		
		Graphics[
			{gPrims},
			PlotRange -> {{0, 2 Pi \[Rho]c}, All},
			PlotRangeClipping -> True,
			PlotRangePadding -> {0, plotRangePaddingY[2 Pi \[Rho]c, arrowheadS, i\[Chi]]},
			Sequence @@ FilterRules[{opts}, Options[Graphics]]]]]


(* ::Subsubsection::Closed:: *)
(*3D*)


Options[LoopCoilPlot3D] = coilGraphic3DOpts;


LoopCoilPlot3D::BadSeparations = plotMessages["BadSeparations"];
LoopCoilPlot3D::BadCurrents = plotMessages["BadCurrents"];
LoopCoilPlot3D::BadDesired = plotMessages["BadDesired"];
LoopCoilPlot3D::BadRadius = plotMessages["BadRadius"];


LoopCoilPlot3D[\[Chi]c_, i\[Chi]_, \[Rho]c_, nDes_, opts:OptionsPattern[]] /; (
	CheckArguments[LoopCoilPlot3D[\[Chi]c, i\[Chi], \[Rho]c, nDes, opts], 4] &&
	loopPlotChecks[LoopCoilPlot3D][\[Chi]c, i\[Chi], \[Rho]c, nDes]
) :=
	Module[{\[Chi]cVals, thicknessS, arrowheadS, allOpts},
		
		(* Explicitly feed loopGraphic3D all option->value pairs. This is incase the user has changed the default value of an option on LoopCoilPlot3D,
			which needs to propagate through to loopGraphic3D. *)
		allOpts = Sequence @@ Normal[Merge[{Options[LoopCoilPlot3D], {opts}}, Last]];
		(* If \[Chi]c is a list of Coil\[Chi]c[index] -> val rules, then sort by index and take the vals. *)
		\[Chi]cVals = Replace[\[Chi]c, l:{__Rule} :> SortBy[
			Cases[l, (Coil\[Chi]c[i_] -> val_) :> {i, val}],
			First][[All, 2]]];
		thicknessS = OptionValue["ThicknessScaling"];
		arrowheadS = OptionValue["ArrowheadScaling"];
		loopGraphic3D[\[Chi]cVals, i\[Chi], \[Rho]c, nDes, thicknessS, arrowheadS, allOpts]]


loopGraphic3D[\[Chi]c_, i\[Chi]_, \[Rho]c_, nDes_, thicknessS_, arrowheadS_, opts___] := Module[
	{styles, hoverThickness, longestDim, plotPoints, gPrims, symTransform},

	styles = processPlotStyle[
		Lookup[Association[opts], PlotStyle, Black],
		Length[\[Chi]c]];

	hoverThickness = Lookup[Association[opts], "HoverThickness", .005];

	(* Arrow thicknesses will be scaled by the longest side length of the plot. *)
	longestDim = Max[Flatten[2 \[Rho]c {\[Chi]c, 1}]];

	plotPoints = Lookup[Association[opts], PlotPoints, 100];

	(* Reverse the direction of the -ve z primitives' currents if the coil is axially antisymmetric. *)
	symTransform = If[EvenQ[nDes], reflectY3D[0], Identity];

	DynamicModule[{tracker},
		(* Construct the primitives with +ve z coords. The thickness of each primitive is proportional to i\[Chi]. *)
		gPrims = MapThread[

			Function[{\[Chi]cp, i\[Chi]p, flip, style},
				dynLoop[

					flip @ {
						(* Arrowheads (size scaled by i\[Chi]) *)
						Arrowheads[Table[{scaleHead[arrowheadS Abs[i\[Chi]p]], pos, arrowHead3D}, {pos, .125, .875, .25}]],
						(* Arrow *)
						addHoverTube[
							(* Tube radius *)
							longestDim thicknessS Abs[i\[Chi]p] / 2,
							(* Hover radius *)
							longestDim hoverThickness / 2,
							Array[\[Rho]c {Cos[#], Sin[#], \[Chi]cp}&, plotPoints, {0, 2. Pi}]
						]
					},

					\[Rho]c \[Chi]cp, i\[Chi]p,
					(* Add the primitive with -ve z coords, accounting for the symmetry/antisymmetry of the coil. *)
					reflectZ3D[0] @* symTransform,
					Dynamic[tracker],
					style]],

			{
				\[Chi]c,
				i\[Chi],
				i\[Chi] /. {_?Negative -> reflectY3D[0], _?Positive -> Identity},
				styles
			}];
		
		Graphics3D[
			{
				gPrims,
				If[TrueQ[<|opts|>["ShowCylinder"]],
					{Replace[<|opts|>["CylinderStyle"], {s__} :> Directive[s]], coilCylinder[\[Rho]c, Max[\[Chi]c], plotPoints]},
					{}],
				Replace[<|opts|>[Show], None | False -> {}]},
			PlotRange -> All,
			Lighting -> AmbientLight[White],
			Sequence @@ FilterRules[{opts}, Options[Graphics3D]]]]]


(* ::Subsection::Closed:: *)
(*Saddles*)


saddlePlotChecks[head_][\[Chi]c_, \[Phi]c_, i\[Chi]_, \[Rho]c_, {nDes_, mDes_}] := Module[
	{proceed = True},
	(* Check that arguments have been specified correctly, and issue messages if not. *)
	
	(* Separations must either be a list of one or more positive reals in ascending order, or a list of
		Coil\[Chi]c[...] -> ... rules (can contain a DesToErr -> ... rule, which will be ignored). *)
	If[
		!MatchQ[\[Chi]c, Alternatives[

			{_?Positive},

			l:{__?Positive} /; (Length[l] >= 2 && AllTrue[Differences[l], Positive]),

			l:{(Coil\[Chi]c[_] | DesToErr -> _?Positive)..} /; With[
					{indicesAndVals = Cases[l, (Coil\[Chi]c[i_] -> val_) :> {i, val}]},
					TrueQ @ And[
						(* Only one (or none) DesToErr rule. *)
						Length[l] - Length[indicesAndVals] < 2,
						(* Coil\[Chi]c indices must be consecutive ascending integers, starting from 1. *)
						Sort[indicesAndVals[[All, 1]]] == Range[Length[indicesAndVals]],
						(* Coil\[Chi]c values, as sorted by index, must be ascending positive numbers. *)
						AllTrue[Differences[SortBy[indicesAndVals, First][[All, 2]]], Positive]
			]]
		]],
		Message[head::BadSeparations, \[Chi]c]; proceed = False];
	
	(* Extents must either be a list of one or more positive reals in ascending order, or a list of
		Coil\[Phi]c[...] -> ... rules. *)
	If[
		!MatchQ[\[Phi]c, Alternatives[

			{_?Positive},

			l:{__?Positive} /; AllTrue[Differences[l], Positive],

			l:{(Coil\[Phi]c[_] -> _?Positive)..} /; With[
				{indicesAndVals = Replace[l, (Coil\[Phi]c[i_] -> val_) :> {i, val}, 1]},
				TrueQ @ And[
					(* Coil\[Phi]c indices must be consecutive ascending integers, starting from 1. *)
					Sort[indicesAndVals[[All, 1]]] == Range[Length[indicesAndVals]],
					(* Coil\[Chi]c values, as sorted by index, must be ascending positive numbers. *)
					AllTrue[Differences[SortBy[indicesAndVals, First][[All, 2]]], Positive]
			]]
		]],
		Message[head::BadExtents, \[Phi]c]; proceed = False];
	
	(* Currents must be a list of reals, equal in length to the number of separations. *)
	If[!MatchQ[i\[Chi], l:{__?realQ} /; Length[l] === Length[DeleteCases[\[Chi]c, DesToErr -> _]]],
		Message[head::BadCurrents, i\[Chi]]; proceed = False];
	
	(* nDes and mDes must be integers that satisfy n >= m > 0... *)
	If[!MatchQ[{nDes, mDes}, {n_Integer, m_Integer} /; n >= m > 0],
		Message[head::BadDesiredNM, nDes, mDes]; proceed = False];
	
	(* If nDes + mDes is odd, then pairs of successive currents must be equal in magnitude and opposite in parity. *)
	If[OddQ[nDes + mDes] && (OddQ[Length[i\[Chi]]] || !AllTrue[Partition[i\[Chi], 2], Total[#] == 0 &]),
		Message[head::BadCurrentRatios, nDes, mDes]; proceed = False];
	
	(* Radius must be positive. *)
	If[!Positive[\[Rho]c],
		Message[head::BadRadius, \[Rho]c]; proceed = False];
	
	proceed]


(* ::Subsubsection::Closed:: *)
(*2D*)


Options[SaddleCoilPlot] = coilGraphic2DOpts;


SaddleCoilPlot::BadSeparations = plotMessages["BadSeparations"];
SaddleCoilPlot::BadExtents = plotMessages["BadExtents"];
SaddleCoilPlot::BadCurrents = plotMessages["BadCurrents"];
SaddleCoilPlot::BadCurrentRatios = plotMessages["BadCurrentRatios"];
SaddleCoilPlot::BadDesiredNM = plotMessages["BadDesiredNM"];
SaddleCoilPlot::BadRadius = plotMessages["BadRadius"];


SaddleCoilPlot[\[Chi]c_, \[Phi]c_, i\[Chi]_, \[Rho]c_, {nDes_, mDes_}, opts:OptionsPattern[]] /; (
	CheckArguments[SaddleCoilPlot[\[Chi]c, \[Phi]c, i\[Chi], \[Rho]c, {nDes, mDes}, opts], 5] &&
	saddlePlotChecks[SaddleCoilPlot][\[Chi]c, \[Phi]c, i\[Chi], \[Rho]c, {nDes, mDes}]
) :=
	Module[{\[Chi]cVals, \[Phi]cVals, thicknessS, arrowheadS, allOpts},
		
		(* Explicitly feed saddleGraphic2D all option->value pairs. This is incase the user has changed the default value of an option on SaddleCoilPlot,
			which needs to propagate through to saddleGraphic2D. *)
		allOpts = Sequence @@ Normal[Merge[{Options[SaddleCoilPlot], {opts}}, Last]];
		(* If \[Chi]c is a list of Coil\[Chi]c[index] -> val rules, then sort by index and take the vals. *)
		\[Chi]cVals = Replace[\[Chi]c, l:{__Rule} :> SortBy[
			Cases[l, (Coil\[Chi]c[i_] -> val_) :> {i, val}],
			First][[All, 2]]];
		(* If \[Phi]c is a list of Coil\[Phi]c[index] -> val rules, then sort by index and take the vals. *)
		\[Phi]cVals = Replace[\[Phi]c, l:{__Rule} :> SortBy[
			Replace[l, (Coil\[Phi]c[i_] -> val_) :> {i, val}, 1],
			First][[All, 2]]];
		thicknessS = OptionValue["ThicknessScaling"];
		arrowheadS = OptionValue["ArrowheadScaling"];
		saddleGraphic2D[\[Chi]cVals, \[Phi]cVals, i\[Chi], \[Rho]c, {nDes, mDes}, thicknessS, arrowheadS, allOpts]]


saddleGraphic2D[\[Chi]c_, \[Phi]c_, i\[Chi]_, \[Rho]c_, {nDes_, mDes_}, thicknessS_, arrowheadS_, opts___] := Module[
	{epilogResolved, styles, gPrims, symQ, arcCentres, arcExtents, wrappingTransforms, \[Chi]cPairs, i = 1},
	
	epilogResolved = Replace[<|opts|>[Epilog], None | False -> {}];
	
	(* Is the coil axially symmetric? *)
	symQ = OddQ[nDes + mDes];
	
	styles = Reverse @ processPlotStyle[
		Lookup[Association[opts], PlotStyle, Black],
		(* If axially symmetric, then pairs of adjacent separations are joined to form one primitive,
			so we only need half as many colours. *)
		Length[\[Chi]c] If[symQ, 1/2, 1] Length[\[Phi]c]];

	(* Construct the primitives' coordinates. *)
	arcCentres = Array[Identity, 2 mDes + 1, {0, 2 Pi}][[;;-2]];
	arcExtents = \[Rho]c Riffle[
		Table[{-#, #} + centre & /@ \[Phi]c, {centre, arcCentres[[;; ;; 2]]}],
		Table[{#, -#} + centre & /@ \[Phi]c, {centre, arcCentres[[2 ;; ;; 2]]}]];
	arcExtents = Flatten[arcExtents, 1];
	(* The first set of primitives have their left halves clipped, so we need to display another copy
		of them on the other side of the plot. *)
	wrappingTransforms = Transpose @ Join[
		Table[Translate[#, {{0, 0}, {2 Pi \[Rho]c, 0}}]&, Length[\[Phi]c]],
		Table[Identity, (Length[arcCentres] - 1) * Length[\[Phi]c]]];

	DynamicModule[{epilog = {epilogResolved}, epilogOpt = epilogResolved},
		If[symQ,

			(* If the coil is axially symmetric, then subsequent pairs of arcs are joined together. *)
			\[Chi]cPairs = Partition[\[Chi]c, 2];
			gPrims = Reverse @ MapThread[
				Function[{zPair, i\[Chi]p, flip},
					Reverse @ MapThread[
						Function[{extent, wt},
							{
								wt @ flip[Mean[extent]] @ {
									(* The thickness and head size of each arrow is proportional to i\[Chi] *)
									Thickness[thicknessS Abs[i\[Chi]p]],
									Arrowheads[{{scaleHead[arrowheadS Abs[i\[Chi]p]], .5, arrowHead}}],
									(* Collection of arrows (one for each line segment). *)
									Arrow[{
										(* All four segments of the saddle. *)
										{{extent[[1]], zPair[[1]]}, {extent[[1]], zPair[[2]]}},
										{{extent[[1]], zPair[[2]]}, {extent[[2]], zPair[[2]]}},
										{{extent[[2]], zPair[[2]]}, {extent[[2]], zPair[[1]]}},
										{{extent[[2]], zPair[[1]]}, {extent[[1]], zPair[[1]]}}
									}]
								},
								(* Arguments for dynSaddle2D *)
								{zPair, i\[Chi]p, Round[Abs[Subtract @@ extent]/2, 10.^-6], reflectY[0], Dynamic[epilog], Dynamic[epilogOpt]}
							}],
							{arcExtents, wrappingTransforms}]],
				{
					\[Rho]c \[Chi]cPairs,
					i\[Chi][[;; ;; 2]],
					i\[Chi][[;; ;; 2]] /. {_?Negative -> reflectX, _?Positive -> (Identity&)}
				}];
			
			gPrims = KeyValueMap[
				dynSaddle2D[#2[[All, 1]], Sequence @@ #1, styles[[i++]]]&,
				GroupBy[Flatten[gPrims, 1], Last]],
			
			(* If the coil is axially antisymmetric, then each arc with +ve z is joined to its corresponding arc with -ve z. *)
			gPrims = Reverse @ MapThread[
				Function[{z, i\[Chi]p, flip},
					Reverse @ MapThread[
						Function[{extent, wt},
							{
								wt @ flip[Mean[extent]] @ {
									(* The thickness and head size of each arrow is proportional to i\[Chi] *)
									Thickness[thicknessS Abs[i\[Chi]p]],
									Arrowheads[{{scaleHead[arrowheadS Abs[i\[Chi]p]], .5, arrowHead}}],
									(* Collection of arrows (one for each line segment). *)
									Arrow[{
										(* All four segments of the saddle. *)
										{{extent[[1]], -z}, {extent[[1]], z}},
										{{extent[[1]], z}, {extent[[2]], z}},
										{{extent[[2]], z}, {extent[[2]], -z}},
										{{extent[[2]], -z}, {extent[[1]], -z}}
									}]
								},
								(* Arguments for dynSaddle2D *)
								{z, i\[Chi]p, Round[Abs[Subtract @@ extent]/2, 10.^-6], {}&, Dynamic[epilog], Dynamic[epilogOpt]}}],
						{arcExtents, wrappingTransforms}]],
				{
					\[Rho]c \[Chi]c,
					i\[Chi],
					i\[Chi] /. {_?Negative -> reflectX, _?Positive -> (Identity&)}
				}];

			gPrims = KeyValueMap[
				dynSaddle2D[#2[[All, 1]], Sequence @@ #1, styles[[i++]]]&,
				GroupBy[Flatten[gPrims, 1], Last]]
		];

		Graphics[
			{gPrims},
			PlotRange -> {{0, 2 Pi \[Rho]c}, All},
			PlotRangeClipping -> True,
			PlotRangePadding -> {0, plotRangePaddingY[2 Pi \[Rho]c, arrowheadS, i\[Chi]]},
			Epilog -> Dynamic[epilog],
			Sequence @@ FilterRules[{opts}, DeleteCases[Options[Graphics], _[Epilog, _]]]]]]


(* ::Subsubsection::Closed:: *)
(*3D*)


Options[SaddleCoilPlot3D] = Normal @ Merge[
	{coilGraphic3DOpts, {"ThicknessScaling" -> .0015}},
	Last];


SaddleCoilPlot3D::BadSeparations = plotMessages["BadSeparations"];
SaddleCoilPlot3D::BadExtents = plotMessages["BadExtents"];
SaddleCoilPlot3D::BadCurrents = plotMessages["BadCurrents"];
SaddleCoilPlot3D::BadCurrentRatios = plotMessages["BadCurrentRatios"];
SaddleCoilPlot3D::BadDesiredNM = plotMessages["BadDesiredNM"];
SaddleCoilPlot3D::BadRadius = plotMessages["BadRadius"];


SaddleCoilPlot3D[\[Chi]c_, \[Phi]c_, i\[Chi]_, \[Rho]c_, {nDes_, mDes_}, opts:OptionsPattern[]] /; (
	CheckArguments[SaddleCoilPlot3D[\[Chi]c, \[Phi]c, i\[Chi], \[Rho]c, {nDes, mDes}, opts], 5] &&
	saddlePlotChecks[SaddleCoilPlot3D][\[Chi]c, \[Phi]c, i\[Chi], \[Rho]c, {nDes, mDes}]
) :=
	Module[{\[Chi]cVals, \[Phi]cVals, thicknessS, arrowheadS, allOpts},
		
		(* Explicitly feed saddleGraphic3D all option->value pairs. This is incase the user has changed the default value of an option on SaddleCoilPlot3D,
			which needs to propagate through to saddleGraphic3D. *)
		allOpts = Sequence @@ Normal[Merge[{Options[SaddleCoilPlot3D], {opts}}, Last]];
		(* If \[Chi]c is a list of Coil\[Chi]c[index] -> val rules, then sort by index and take the vals. *)
		\[Chi]cVals = Replace[\[Chi]c, l:{__Rule} :> SortBy[
			Cases[l, (Coil\[Chi]c[i_] -> val_) :> {i, val}],
			First][[All, 2]]];
		(* If \[Phi]c is a list of Coil\[Phi]c[index] -> val rules, then sort by index and take the vals. *)
		\[Phi]cVals = Replace[\[Phi]c, l:{__Rule} :> SortBy[
			Replace[l, (Coil\[Phi]c[i_] -> val_) :> {i, val}, 1],
			First][[All, 2]]];
		thicknessS = OptionValue["ThicknessScaling"];
		arrowheadS = OptionValue["ArrowheadScaling"];
		saddleGraphic3D[\[Chi]cVals, \[Phi]cVals, i\[Chi], \[Rho]c, {nDes, mDes}, thicknessS, arrowheadS, allOpts]]


saddleGraphic3D[\[Chi]c_, \[Phi]c_, i\[Chi]_, \[Rho]c_, {nDes_, mDes_}, thicknessS_, arrowheadS_, opts___] := Module[
	{plotPoints, du, hoverThickness, styles, longestDim, gPrims, symQ, arcCentres, arcExtents, \[Chi]cPairs},

	plotPoints = Lookup[Association[opts], PlotPoints, 100];
	du = 2. Pi / plotPoints;

	hoverThickness = Lookup[Association[opts], "HoverThickness", .005];

	(* Arrow thicknesses will be scaled by the longest side length of the plot. *)
	longestDim = Max[Flatten[2 \[Rho]c {\[Chi]c, 1}]];

	(* Is the coil axially symmetric? *)
	symQ = OddQ[nDes + mDes];
	
	(* Construct the primitives' coordinates. *)
	arcCentres = Array[Identity, 2 mDes + 1, {0, 2 Pi}][[;;-2]];
	arcExtents = Riffle[
		Table[{-#, #} + centre & /@ \[Phi]c, {centre, arcCentres[[;; ;; 2]]}],
		Table[{#, -#} + centre & /@ \[Phi]c, {centre, arcCentres[[2 ;; ;; 2]]}]];
	arcExtents = Flatten[arcExtents, 1];

	styles = processPlotStyle[
		Lookup[Association[opts], PlotStyle, Black],
		(* If axially symmetric, then pairs of adjacent separations are joined to form one primitive,
			so we only need half as many colours. *)
		Length[\[Chi]c] If[symQ, 1/2, 1] Length[\[Phi]c]];

	DynamicModule[{tracker},
		If[symQ,

			(* If the coil is axially symmetric, then subsequent pairs of arcs are joined together. *)
			\[Chi]cPairs = Partition[\[Chi]c, 2];
			(* Form an Association of key -> style elements, where key is {\[Chi]cPair, extent} for a primitive group. *)
			styles = Association[
				Rule @@@ Transpose[{
					Flatten[Outer[List, \[Chi]cPairs, Round[\[Phi]c, 10.^-6], 1], 1],
					styles}]];
			gPrims = MapThread[
				Function[{\[Chi]cPair, i\[Chi]p, flip},
					Map[
						Function[extent,
							dynSaddle3D[
								(* Reverse current direction of a saddle by flipping around its z-centre. *)
								flip[Mean[\[Chi]cPair]] @ {
									(* Arrowheads (size scaled by i\[Chi]) *)
									Arrowheads[{{scaleHead[arrowheadS Abs[i\[Chi]p]], .5, arrowHead3D}}],
									(* Collection of arrows (one for each line segment). *)
									addHoverTube[
										(* Tube radius *)
										longestDim thicknessS Abs[i\[Chi]p] / 2,
										(* Hover radius *)
										longestDim hoverThickness / 2,
										{
											(* All four segments of the saddle. *)
											\[Rho]c {Cos[extent[[1]]], Sin[extent[[1]]], #}& /@ \[Chi]cPair,
											Array[\[Rho]c {Cos[#], Sin[#], \[Chi]cPair[[2]]}&, Round[Abs @@ Differences[extent] / du], extent],
											\[Rho]c {Cos[extent[[2]]], Sin[extent[[2]]], #}& /@ Reverse[\[Chi]cPair],
											Array[\[Rho]c {Cos[#], Sin[#], \[Chi]cPair[[1]]}&, Round[Abs @@ Differences[extent] / du], Reverse[extent]]
										},
										(* Thickness scaled by i\[Chi] *)
										(* Add a small amount of thickness to the highlighted tube to ensure it is fully shown. *)
										With[
											{
												key = {\[Rho]c \[Chi]cPair, Round[\[Rho]c Abs[Subtract @@ extent]/2, 10.^-6], i\[Chi]p},
												longestDimBurn = longestDim},
											Dynamic[longestDimBurn (thicknessS Abs[i\[Chi]p] / 2 + If[tracker === key, .00025, 0])]]]},
								\[Rho]c \[Chi]cPair,
								i\[Chi]p,
								Round[\[Rho]c Abs[Subtract @@ extent]/2, 10.^-6],
								reflectZ3D[0],
								Dynamic[tracker],
								styles[{\[Chi]cPair, Round[Abs[Subtract @@ extent]/2, 10.^-6]}]]],
						arcExtents]],
				{
					\[Chi]cPairs,
					i\[Chi][[;; ;; 2]],
					i\[Chi][[;; ;; 2]] /. {_?Negative -> reflectZ3D, _?Positive -> (Identity&)}
				}],
			
			(* If the coil is axially antisymmetric, then each arc with +ve z is joined to its corresponding arc with -ve z. *)
			(* Form an Association of key -> style elements, where key is {\[Chi]c, extent} for a primitive group. *)
			styles = Association[
				Rule @@@ Transpose[{
					Flatten[Outer[List, \[Chi]c, Round[\[Phi]c, 10.^-6]], 1],
					styles}]];
			gPrims = MapThread[
				Function[{\[Chi]cp, i\[Chi]p, flip},
					Map[
						Function[extent,
							dynSaddle3D[
								flip @ {
									(* Arrowheads (size scaled by i\[Chi]) *)
									Arrowheads[{{scaleHead[arrowheadS Abs[i\[Chi]p]], .5, arrowHead3D}}],
									(* Collection of arrows (one for each line segment). *)
									addHoverTube[
										(* Tube radius *)
										longestDim thicknessS Abs[i\[Chi]p] / 2,
										(* Hover radius *)
										longestDim hoverThickness / 2,
										{
											(* All four segments of the saddle. *)
											\[Rho]c {Cos[extent[[1]]], Sin[extent[[1]]], #}& /@ {-\[Chi]cp, \[Chi]cp},
											Array[\[Rho]c {Cos[#], Sin[#], \[Chi]cp}&, Round[Abs @@ Differences[extent] / du], extent],
											\[Rho]c {Cos[extent[[2]]], Sin[extent[[2]]], #}& /@ {\[Chi]cp, -\[Chi]cp},
											Array[\[Rho]c {Cos[#], Sin[#], -\[Chi]cp}&, Round[Abs @@ Differences[extent] / du], Reverse[extent]]
										},
										(* Thickness scaled by i\[Chi] *)
										(* Add a small amount of thickness to the highlighted tube to ensure it is fully shown. *)
										With[
											{
												key = {\[Rho]c \[Chi]cp, Round[\[Rho]c Abs[Subtract @@ extent]/2, 10.^-6], i\[Chi]p},
												longestDimBurn = longestDim},
											Dynamic[longestDimBurn (thicknessS Abs[i\[Chi]p] / 2 + If[tracker === key, .00025, 0])]]]},
								\[Rho]c \[Chi]cp,
								i\[Chi]p,
								Round[\[Rho]c Abs[Subtract @@ extent]/2, 10.^-6],
								Identity,
								Dynamic[tracker],
								styles[{\[Chi]cp, Round[Abs[Subtract @@ extent]/2, 10.^-6]}]]],
						arcExtents]],
				{
					\[Chi]c,
					i\[Chi],
					i\[Chi] /. {_?Negative -> reflectZ3D[0], _?Positive -> Identity}
				}]
		];

		Graphics3D[
			{
				gPrims,
				If[TrueQ[<|opts|>["ShowCylinder"]],
					{Replace[<|opts|>["CylinderStyle"], {s__} :> Directive[s]], coilCylinder[\[Rho]c, Max[\[Chi]c], plotPoints]},
					{}],
				Replace[<|opts|>[Show], None | False -> {}]},
			PlotRange -> All,
			Lighting -> AmbientLight[White],
			Sequence @@ FilterRules[{opts}, Options[Graphics3D]]]]]


(* ::Subsection::Closed:: *)
(*Ellipses*)


ellipsePlotChecks[head_][\[Chi]c\[Psi]c_, i\[Chi]_, \[Rho]c_, {nDes_, mDes_}] := Module[
	{proceed = True},
	(* Check that arguments have been specified correctly, and issue messages if not. *)
	
	(* Separations and extents must either be a list of one or more paired positive reals, 
		{{\[Chi]c1, \[Psi]c1}, {\[Chi]c2, \[Psi]c2}, \[Ellipsis]}, 
		or a flat list of Coil\[Chi]c[i] -> \[Chi]ci and Coil\[Psi]c[i] -> \[Psi]ci rules,
		{Coil\[Chi]c[1] -> \[Chi]c1, Coil\[Psi]c[1] -> \[Psi]c1, Coil\[Chi]c[2] -> \[Chi]c2, Coil\[Psi]c[2] -> \[Psi]c2, \[Ellipsis]},
		where there are as many extents as separations. In both cases, \[Chi]c1 < \[Chi]c2 < \[Ellipsis]. *)
	If[
		!MatchQ[\[Chi]c\[Psi]c, Alternatives[

			{{_?Positive, _?Positive}},

			l:{{_?Positive, _?Positive}..} /; (Length[l] >= 2 && AllTrue[Differences[l[[All, 1]]], Positive]),

			l:{(Coil\[Chi]c[_] | Coil\[Psi]c[_] | DesToErr -> _?Positive)..} /; Module[
				{\[Chi]cIndicesAndVals, \[Psi]cIndicesAndVals},
				{\[Chi]cIndicesAndVals, \[Psi]cIndicesAndVals} = Map[
					SortBy[Cases[l, (#[i_] -> val_) :> {i, val}], First]&,
					{Coil\[Chi]c, Coil\[Psi]c}];
				TrueQ @ And[
					(* Same number of separations and extents. *)
					Length[\[Chi]cIndicesAndVals] === Length[\[Psi]cIndicesAndVals],
					(* Coil\[Chi]c and Coil\[Psi]c indices must be consecutive ascending integers, starting from 1. *)
					\[Chi]cIndicesAndVals[[All, 1]] == Range[Length[\[Chi]cIndicesAndVals]],
					\[Psi]cIndicesAndVals[[All, 1]] == Range[Length[\[Psi]cIndicesAndVals]],
					(* Coil\[Chi]c values, as sorted by index, must be ascending positive numbers. *)
					AllTrue[Differences[\[Chi]cIndicesAndVals[[All, 2]]], Positive]
			]]
		]],
		Message[head::BadChiPsi, \[Chi]c\[Psi]c]; proceed = False];
	
	(* Currents must be a list of reals, equal in length to the number of separations. *)
	If[
		!MatchQ[i\[Chi], l:{__?realQ} /; Length[l] === Length[
			Cases[\[Chi]c\[Psi]c, (Coil\[Chi]c[_] -> _) | {_, _}]]],
		Message[head::BadCurrents, i\[Chi]]; proceed = False];
	
	(* nDes and mDes must be integers that satisfy n >= m > 0... *)
	If[!MatchQ[{nDes, mDes}, {n_Integer, m_Integer} /; n >= m > 0],
		Message[head::BadDesiredNM, nDes, mDes]; proceed = False];
	
	(* Radius must be positive. *)
	If[!Positive[\[Rho]c],
		Message[head::BadRadius, \[Rho]c]; proceed = False];
	
	proceed]


(* ::Subsubsection::Closed:: *)
(*2D*)


Options[EllipseCoilPlot] = coilGraphic2DOpts;


EllipseCoilPlot::BadChiPsi = plotMessages["BadChiPsi"];
EllipseCoilPlot::BadCurrents = plotMessages["BadCurrents"];
EllipseCoilPlot::BadDesiredNM = plotMessages["BadDesiredNM"];
EllipseCoilPlot::BadRadius = plotMessages["BadRadius"];


EllipseCoilPlot[\[Chi]c\[Psi]c_, i\[Chi]_, \[Rho]c_, {nDes_, mDes_}, opts:OptionsPattern[]] /; (
	CheckArguments[EllipseCoilPlot[\[Chi]c\[Psi]c, i\[Chi], \[Rho]c, {nDes, mDes}, opts], 4] &&
	ellipsePlotChecks[EllipseCoilPlot][\[Chi]c\[Psi]c, i\[Chi], \[Rho]c, {nDes, mDes}]
) :=
	Module[{\[Chi]cVals, \[Psi]cVals, thicknessS, arrowheadS, allOpts},
		
		(* Explicitly feed ellipseGraphic2D all option->value pairs. This is incase the user has changed the default value of an option on EllipseCoilPlot,
			which needs to propagate through to ellipseGraphic2D. *)
		allOpts = Sequence @@ Normal[Merge[{Options[EllipseCoilPlot], {opts}}, Last]];
		{\[Chi]cVals, \[Psi]cVals} = Switch[
			\[Chi]c\[Psi]c,
			(* If \[Chi]c\[Psi]c is a list of {\[Chi]c, \[Psi]c} pairs, then transpose and assign. *)
			{{_, _}..},
			Transpose[\[Chi]c\[Psi]c],
			(* If \[Chi]c\[Psi]c is a list of Coil\[Chi]c[index] -> val and Coil\[Psi]c[index] -> val rules, then sort each by index and take the vals. *)
			{__Rule},
			SortBy[Cases[\[Chi]c\[Psi]c, (#[i_] -> val_) :> {i, val}], First][[All, 2]]& /@ {Coil\[Chi]c, Coil\[Psi]c}];
		thicknessS = OptionValue["ThicknessScaling"];
		arrowheadS = OptionValue["ArrowheadScaling"];
		ellipseGraphic2D[\[Chi]cVals, \[Psi]cVals, i\[Chi], \[Rho]c, {nDes, mDes}, thicknessS, arrowheadS, allOpts]]


ellipseGraphic2D[\[Chi]c_, \[Psi]c_, i\[Chi]_, \[Rho]c_, {nDes_, mDes_}, thicknessS_, arrowheadS_, opts___] := Module[
	{styles, gPrims, phases, symTransform, periodicityTransform},

	styles = processPlotStyle[
		Lookup[Association[opts], PlotStyle, Black],
		Length[\[Chi]c]];
		
	(* Reverse the direction of the -ve z primitives' currents if the coil is axially antisymmetric. *)
	symTransform = If[EvenQ[nDes + mDes], reflectX[Pi \[Rho]c], Identity];

	DynamicModule[{tracker},

		(* Construct the primitives' coordinates. *)
		phases = Array[Identity, 2 mDes + 1, {0, 2 Pi}][[;; -2]];

		(* Alternating periodicity so that there are mDes lines of symmetry. *)
		periodicityTransform = PadRight[{}, Length[phases], {Identity, reflectX[Pi \[Rho]c]}];

		gPrims = MapThread[
			Function[{z, t, i\[Chi]p, flip, style},
				dynEllipse[
					flip @ MapThread[
						Function[{phase, pt},
							pt @ {
								(* The thickness and head size of each arrow is proportional to i\[Chi] *)
								Thickness[thicknessS Abs[i\[Chi]p]],
								Arrowheads @ Table[
									{scaleHead[arrowheadS Abs[i\[Chi]p]], Mod[pos - phase/(2 Pi), 1.0001], arrowHead},
									{pos, 0, 1, .5}],
								(* Arrows *)
								Arrow @ FirstCase[
									Plot[t Cos[\[Phi] / \[Rho]c + phase] + z, {\[Phi], 0, 2 Pi \[Rho]c}, MaxRecursion -> 3],
									Line[pts_] :> pts,
									{}, Infinity]
							}],
						{phases, periodicityTransform}],
					z, t, i\[Chi]p,
					(* Add the primitives with -ve z coords, accounting for the symmetry/antisymmetry of the coil. *)
					reflectY[0] @* symTransform,
					Dynamic[tracker],
					style]],
			{
				\[Rho]c \[Chi]c,
				\[Rho]c \[Psi]c,
				i\[Chi],
				i\[Chi] /. {_?Negative -> reflectX[Pi \[Rho]c], _?Positive -> Identity},
				styles
			}];
		
		(* Reshape into {{dirs, Arrow[...]}, {dirs, Arrow[...]}, ...} *)
		gPrims = Flatten[gPrims, 1];

		Graphics[
			{gPrims},
			PlotRange -> {{0, 2 Pi \[Rho]c}, All},
			PlotRangeClipping -> True,
			PlotRangePadding -> {0, plotRangePaddingY[2 Pi \[Rho]c, arrowheadS, i\[Chi]]},
			Sequence @@ FilterRules[{opts}, Options[Graphics]]]]]


(* ::Subsubsection::Closed:: *)
(*3D*)


Options[EllipseCoilPlot3D] = coilGraphic3DOpts;


EllipseCoilPlot3D::BadChiPsi = plotMessages["BadChiPsi"];
EllipseCoilPlot3D::BadCurrents = plotMessages["BadCurrents"];
EllipseCoilPlot3D::BadDesiredNM = plotMessages["BadDesiredNM"];
EllipseCoilPlot3D::BadRadius = plotMessages["BadRadius"];


EllipseCoilPlot3D[\[Chi]c\[Psi]c_, i\[Chi]_, \[Rho]c_, {nDes_, mDes_}, opts:OptionsPattern[]] /; (
	CheckArguments[EllipseCoilPlot3D[\[Chi]c\[Psi]c, i\[Chi], \[Rho]c, {nDes, mDes}, opts], 4] &&
	ellipsePlotChecks[EllipseCoilPlot3D][\[Chi]c\[Psi]c, i\[Chi], \[Rho]c, {nDes, mDes}]
) :=
	Module[{\[Chi]cVals, \[Psi]cVals, thicknessS, arrowheadS, allOpts},
		
		(* Explicitly feed ellipseGraphic3D all option->value pairs. This is incase the user has changed the default value of an option on EllipseCoilPlot3D,
			which needs to propagate through to ellipseGraphic3D. *)
		allOpts = Sequence @@ Normal[Merge[{Options[EllipseCoilPlot3D], {opts}}, Last]];
		{\[Chi]cVals, \[Psi]cVals} = Switch[
			\[Chi]c\[Psi]c,
			(* If \[Chi]c\[Psi]c is a list of {\[Chi]c, \[Psi]c} pairs, then transpose and assign. *)
			{{_, _}..},
			Transpose[\[Chi]c\[Psi]c],
			(* If \[Chi]c\[Psi]c is a list of Coil\[Chi]c[index] -> val and Coil\[Psi]c[index] -> val rules, then sort each by index and take the vals. *)
			{__Rule},
			SortBy[Cases[\[Chi]c\[Psi]c, (#[i_] -> val_) :> {i, val}], First][[All, 2]]& /@ {Coil\[Chi]c, Coil\[Psi]c}];
		thicknessS = OptionValue["ThicknessScaling"];
		arrowheadS = OptionValue["ArrowheadScaling"];
		ellipseGraphic3D[\[Chi]cVals, \[Psi]cVals, i\[Chi], \[Rho]c, {nDes, mDes}, thicknessS, arrowheadS, allOpts]]


ellipseGraphic3D[\[Chi]c_, \[Psi]c_, i\[Chi]_, \[Rho]c_, {nDes_, mDes_}, thicknessS_, arrowheadS_, opts___] := Module[
	{styles, hoverThickness, longestDim, plotPoints, gPrims, phases, symTransform, periodicityTransform},

	styles = processPlotStyle[
		Lookup[Association[opts], PlotStyle, Black],
		Length[\[Chi]c]];
	
	hoverThickness = Lookup[Association[opts], "HoverThickness", .005];

	(* Arrow thicknesses will be scaled by the longest side length of the plot. *)
	longestDim = Max[Flatten[2 \[Rho]c {\[Chi]c, 1}]];

	plotPoints = Lookup[Association[opts], PlotPoints, 100];
		
	(* Reverse the direction of the -ve z primitives' currents if the coil is axially antisymmetric. *)
	symTransform = If[EvenQ[nDes + mDes], reflectY3D[0], Identity];

	DynamicModule[{tracker},

		(* Construct the primitives' coordinates. *)
		phases = Array[Identity, 2 mDes + 1, {0, 2 Pi}][[;; -2]];

		(* Alternating periodicity so that there are mDes lines of symmetry. *)
		periodicityTransform = PadRight[{}, Length[phases], {Identity, reflectY3D[0]}];

		gPrims = MapThread[
			Function[{\[Chi]cp, \[Psi]cp, i\[Chi]p, flip, style},
				dynEllipse[
					flip @ MapThread[

						Function[{phase, pt},
							pt @ {
								Arrowheads @ Table[
									{scaleHead[arrowheadS Abs[i\[Chi]p]], Mod[pos - phase/(2 Pi), 1.0001], arrowHead3D},
									{pos, 0, 1, .5}],
								(* Arrows *)
								addHoverTube[
									(* Tube radius *)
									longestDim thicknessS Abs[i\[Chi]p] / 2,
									(* Hover radius *)
									longestDim hoverThickness / 2,
									Array[
										\[Rho]c {Cos[#], Sin[#], \[Psi]cp Cos[# + phase] + \[Chi]cp}&,
										plotPoints, {0, 2. Pi}]]}],

						{phases, periodicityTransform}],

					\[Chi]cp, \[Psi]cp, i\[Chi]p,
					(* Add the primitives with -ve z coords, accounting for the symmetry/antisymmetry of the coil. *)
					reflectZ3D[0] @* symTransform,
					Dynamic[tracker],
					style]],
			{
				\[Chi]c,
				\[Psi]c,
				i\[Chi],
				i\[Chi] /. {_?Negative -> reflectY3D[0], _?Positive -> Identity},
				styles
			}];
		
		(* Reshape into {{dirs, Arrow[...]}, {dirs, Arrow[...]}, ...} *)
		gPrims = Flatten[gPrims, 1];

		Graphics3D[
			{
				gPrims,
				If[TrueQ[<|opts|>["ShowCylinder"]],
					{Replace[<|opts|>["CylinderStyle"], {s__} :> Directive[s]], coilCylinder[\[Rho]c, Max[\[Chi]c + \[Psi]c], plotPoints]},
					{}],
				Replace[<|opts|>[Show], None | False -> {}]},
			PlotRange -> All,
			Lighting -> AmbientLight[White],
			Sequence @@ FilterRules[{opts}, Options[Graphics3D]]]]]


(* ::Section::Closed:: *)
(*Field Plots*)


fieldPlotOpts = Normal @ Merge[
	{
		Options[ListLinePlot],
		Options[Plot],
		{
			MaxRecursion -> 2,
			Axes -> None,
			Frame -> True,
			PlotRange -> {Automatic, Automatic, Automatic},
			ImageSize -> Medium,
			FrameLabel -> Automatic,
			PlotLegends -> Automatic
		}
	},
	Last];


interpolationOptions = {
	PlotPoints -> 6,
	MaxRecursion -> 4
};


fieldPlot2DOpts = Normal @ Merge[
	{
		Options[ListDensityPlot],
		interpolationOptions,
		{
			ImageSize -> Medium,
			AspectRatio -> Automatic,
			FrameLabel -> {
				{TraditionalForm @ RawBoxes["\"\\!\\(\\*StyleBox[\\\"z\\\",FontSlant->\\\"Italic\\\"]\\) (m)\""], None},
				{TraditionalForm @ RawBoxes["\"\\!\\(\\*StyleBox[\\\"x\\\",FontSlant->\\\"Italic\\\"]\\) (m)\""], None}},
			ColorFunction -> (Blend[{RGBColor[0, 0.36, 0.77], GrayLevel[.975], RGBColor[0.85, 0.27, 0.08]}, #]&),
			ClippingStyle -> Automatic,
			PlotRange -> Automatic,
			PlotRangePadding -> Automatic,
			MeshFunctions -> {#3 &},
			Mesh -> True,
			MeshStyle -> GrayLevel[0, .2],
			"ReconstructMesh" -> False,
			"DeviationsPlotPoints" -> Automatic,
			"DeviationsMaxRecursion" -> Automatic,
			"Deviations" -> {.01, .05},
			"DeviationsStyle" -> Automatic
		}
	},
	Last];


integrationOpts = Sequence[
	Method -> {Automatic, "SymbolicProcessing" -> 0},
	AccuracyGoal -> 4, PrecisionGoal -> 4, MaxRecursion -> 3, MaxPoints -> 10];


biotSavartPlot[integrand_, \[Rho]c_, pad_, zMax_, opts_] := Module[
	{integrandX, integrandY, integrandZ, const, data, interpolationOpts, plotOpts, xRange, yRange, zRange, temp},

	interpolationOpts = Sequence @@ FilterRules[opts, Complement[Options[Plot][[All, 1]], Options[ListLinePlot][[All, 1]]]];
	plotOpts = Sequence @@ FilterRules[opts, Options[ListLinePlot]];

	temp = Replace[
		Lookup[<|plotOpts|>, PlotRange, Automatic],
		Except[{rx_, ry_, rz_}] :> {Automatic, Automatic, Automatic}];

	MapThread[
		Function[{val, range},
			Switch[val,
				_?NumericQ,
					range = {-val, val},
				{_?NumericQ, _?NumericQ},
					range = val,
				_,
					range = {-1, 1} (1 + pad) \[Rho]c;
					zRange = {-1, 1} zMax],
			HoldRest],
		{
			temp,
			{xRange, yRange, zRange}}];
	
	integrandX = integrand /. {\[FormalY] -> 0, \[FormalZ] -> 0};
	integrandY = integrand /. {\[FormalX] -> 0, \[FormalZ] -> 0};
	integrandZ = integrand /. {\[FormalX] -> 0, \[FormalY] -> 0};

	const = QuantityMagnitude[UnitConvert[Quantity[1, "MagneticConstant"]]]/(4 Pi);
	(* Micro Tesla *)
	const = 10^6 const;

	data = MapThread[
		Function[{int, var, dom},
			Reap[
				Plot[
					Sow[{var,
						Chop[Quiet @ NIntegrate[int, {\[FormalU], 0, 1}, ##], .000001]}
					][[2]]&[integrationOpts],
					{var, dom[[1]], dom[[2]]}, PlotRange -> All, ##
				]][[-1, 1]]&[interpolationOpts]],
		{
			{integrandX, integrandY, integrandZ},
			{\[FormalX], \[FormalY], \[FormalZ]},
			{xRange, yRange, zRange}}];
	
	data = Map[{1, const}*# &] /@ data;

	data = SortBy[First] /@ data;

	MapThread[
		Function[{dim, str},
			ListLinePlot[
				Transpose[First[Outer[List, {#1}, #2]]& @@@ data[[dim]]],
				Sequence @@ Replace[{plotOpts},
					{
						(FrameLabel -> Automatic) -> (FrameLabel -> {
							{
								TraditionalForm[
									"\!\(\*StyleBox[\"B\",FontSlant->\"Italic\"]\)\[VeryThinSpace]\!\(\*SuperscriptBox[SubscriptBox[\(I\), \(\[Chi]\)], \(-1\)]\) (\[Mu]T\[VeryThinSpace]/\[VeryThinSpace]A)"],
								None},
							{
								TraditionalForm @ RawBoxes @ StringReplacePart[
									"\"\\!\\(\\*StyleBox[\\\"z\\\",FontSlant->\\\"Italic\\\"]\\) (m)\"", str,
									{19, 19}],
								None}}),
						(PlotLegends -> Automatic) -> (PlotLegends -> Placed[LineLegend[
							TraditionalForm /@ {
								"\!\(\*SubscriptBox[\(B\), \(x\)]\)\[VeryThinSpace]\!\(\*SuperscriptBox[SubscriptBox[\(I\), \(\[Chi]\)], \(-1\)]\)",
								"\!\(\*SubscriptBox[\(B\), \(y\)]\)\[VeryThinSpace]\!\(\*SuperscriptBox[SubscriptBox[\(I\), \(\[Chi]\)], \(-1\)]\)",
								"\!\(\*SubscriptBox[\(B\), \(z\)]\)\[VeryThinSpace]\!\(\*SuperscriptBox[SubscriptBox[\(I\), \(\[Chi]\)], \(-1\)]\)"},
							First @ Normal @ Merge[
								{{LabelStyle -> {"Graphics", "GraphicsLabel"}}, FilterRules[{opts}, LabelStyle]},
								Flatten[#, 2]&],
							LegendLayout -> "Column"], After]),
						(PlotRange -> _) -> (PlotRange -> All)
					},
					1]]],
		{{1, 2, 3}, {"x", "y", "z"}}]]


biotSavartPlot2D[integrand_, \[Chi]c_, i\[Chi]_, \[Phi]c_, t_, \[Rho]c_, {n_, m_}, pad_, zMax_, {interpolationOpts___}, {plotOpts___}, {otherOpts___}] := Module[
	{
		integrandXZ, contours, data, const, bzAlongZ, bxbyAlongZ, bzAlongX, bxbyAlongX,
		deviationInterpOpts, deviation, deviationStyle, regionPlots, temp, derivatives, integrandXZGrad, showMeshQ,
		derivativeLabels, lines, regionPlotLines, densityPlots, xDataRange, zDataRange, xPlotRange, zPlotRange, xReflectQ, zReflectQ, bFieldRange},

	(* Plot the xz-plane *)
	integrandXZ = integrand /. \[FormalY] -> 0;

	(* We will only calculate the field in one quadrant, and populate the remaining quadrants by transforming the field
		appropriately. Hence, given the specified PlotRange, we need to determine the x and z ranges over which to calculate
		the field, and the absolute plot range to draw. *)
	temp = Replace[
		Lookup[<|plotOpts|>, PlotRange, Automatic],
		{{rx_, rz_, ___} :> {rx, rz}, _ -> {Automatic, Automatic}}];

	MapThread[
		Function[{val, dataRange, plotRange, reflectQ},
			Switch[val,
				_?NumericQ,
					dataRange = {0, Abs[val]};
					plotRange = {-1, 1} Abs[val];
					reflectQ = True,
				{_?NumericQ, _?NumericQ} /; AllTrue[val, # >= 0 &] || AllTrue[val, # <= 0 &],
					dataRange = Sort[val];
					plotRange = Sort[val];
					reflectQ = False,
				{_?NumericQ, _?NumericQ},
					dataRange = {0, Max @ Abs[val]};
					plotRange = Sort[val];
					reflectQ = True,
				_,
					dataRange = {0, (1 + pad) \[Rho]c}; zDataRange = {0, zMax};
					plotRange = {-1, 1} (1 + pad) \[Rho]c; zPlotRange = {-1, 1} zMax;
					reflectQ = True],
			HoldRest],
		{
			temp,
			{xDataRange, zDataRange},
			{xPlotRange, zPlotRange},
			{xReflectQ, zReflectQ}}];

	(* Integrating the integrand returns a 3D field vector. Therefore, we use DensityPlot to interpolate the field looking at
		the z-component, and Sow each 3D point it calculates. We then call ListDensityPlot three times to make a plot of each
		field component. *)
	(* We only need to calculate the field in one quadrant, and can deduce the field across the others by symmetry. *)
	data = Reap[DensityPlot[

		Sow[{\[FormalX], \[FormalZ],
			Chop[Quiet @ NIntegrate[integrandXZ, {\[FormalU], 0, 1}, ##], .000001]}][[3]],

		{\[FormalX], xDataRange[[1]], xDataRange[[2]]}, {\[FormalZ], zDataRange[[1]], zDataRange[[2]]},
		interpolationOpts
	]][[-1, 1]]&[integrationOpts];

	deviation = Replace[#, Except[_List] -> {#}]&[<|otherOpts|>["Deviations"]];
	(* If "DeviationsStyle" -> Automatic, use small black dashing for the first deviation, large black for the second, then use ColorData[97]
		in which each colour is used twice, once with small dashing and once with large dashing. *)
	deviationStyle = Replace[
		<|otherOpts|>["DeviationsStyle"],
		Automatic -> Append[Thickness[.79 .00375]] /@ Transpose @ {
			PadRight[{}, Length[deviation], {Dashing[.8{.005, .009}, 0, "Square"], Dashing[.8{.025, .009}, 0, "Square"]}],
			Table[i /. {1|2 -> Black, _ -> ColorData[97][Floor[(i - 1)/2]]}, {i, Length[deviation]}]}];
	deviationInterpOpts = Sequence[
		PlotPoints -> Replace[
			<|otherOpts|>["DeviationsPlotPoints"],
			Automatic -> Round[<|interpolationOpts|>[PlotPoints] * 10/6]],
		MaxRecursion -> Replace[
			<|otherOpts|>["DeviationsMaxRecursion"],
			Automatic -> Round[<|interpolationOpts|>[MaxRecursion] * 5/4]]];

	If[MatchQ[deviation, _?NumericQ | {__?NumericQ}],

		deviationStyle = Switch[
			deviationStyle,
			_ColorFunction, Table[deviationStyle[i], {i, Length[deviation]}],
			_List, Directive /@ PadRight[{}, Length[deviation], deviationStyle],
			_, {deviationStyle}];
		
		derivatives = Map[
			Flatten @* List @* ReplaceAll[Plus -> List],
			Expand @ FullSimplify[
				bFieldHarmonicVector[{n, m}, Sqrt[\[FormalX]^2 + \[FormalZ]^2], ArcCos[\[FormalZ] / Sqrt[\[FormalX]^2 + \[FormalZ]^2]], 0],
				Assumptions -> {Element[\[FormalX], PositiveReals], Element[\[FormalZ], PositiveReals]}]];
		
		derivatives = MapThread[Function[{dim, field}, {dim, #}& /@ field], {Range[3], derivatives}];

		derivativeLabels = Map[
			Apply @ Function[{dim, field},
				With[
					{
						xe = Exponent[field, \[FormalX]],
						ze = Exponent[field, \[FormalZ]],
						bDim = Style[Subscript["B", Switch[dim, 1, "x", 2, "y", 3, "z"]], Italic]},
					Which[
						TrueQ[field == 0], None,
						TrueQ[{xe, ze} == {0, 0}], Pane[bDim, FrameMargins -> 0, BaselinePosition -> Baseline],
						True,
						ToBoxes /@ FractionBox[
							Row[{
								If[xe + ze == 1, "\[PartialD]", Superscript["\[PartialD]", xe + ze]],
								bDim}],
							Row[MapThread[
								Switch[#1,
									0, Nothing,
									1, Row[{"\[PartialD]", #2}],
									_, Row[{"\[PartialD]", Superscript[#2, #1]}]]&,
								{
									{xe, ze},
									{Style["x", Italic], Style["z", Italic]}}], "\[ThinSpace]"]] // RawBoxes]]],
			derivatives, {2}];

		derivatives = Map[
			Map @ Apply @ Function[{dim, field},
				With[{dx = Exponent[field, \[FormalX]], dz = Exponent[field, \[FormalZ]]},
					Hold[D[Slot[dim], {\[FormalX], dx}, {\[FormalZ], dz}]]]],
			derivatives];
		
		derivatives = Function[Evaluate[derivatives]] /. Hold[d_] :> d;
		
		(* Now find the symbolic expressions for the target field gradient, and the gradient of the integrand for the Biot Savart integral. *)
		(* This is commented out because we're now sampling the target field from the centre *)
		(* targetFieldGrad = Simplify[
			Apply[derivatives,
				bFieldCoilHarmonicVector[{n, m}, \[Chi]c, i\[Chi], \[Phi]c, t, \[Rho]c, {\[FormalX], 0, \[FormalZ]}] /. Abs -> (Sqrt[#^2] &)],
			Assumptions -> {Element[\[FormalX], PositiveReals], Element[\[FormalZ], PositiveReals]}]; *)
		integrandXZGrad = Apply[
			derivatives,
			integrandXZ /. Abs -> (Sqrt[#^2] &)];

		contours = Map[
			Function[{integrandi},
				(* Use RegionPlot to find the contour where the actual field deviates from the target field harmonic (in the
					top right quadrant). *)
				regionPlots = Table[
					With[
						{target = 1/(4 Pi) Quiet @ NIntegrate[
							Evaluate[integrandi /. {\[FormalX] -> 0, \[FormalZ] -> 0}],
							{\[FormalU], 0, 1}, ##]},
						Quiet @ RegionPlot[
							With[
								{actual = 1/(4 Pi) NIntegrate[integrandi, {\[FormalU], 0, 1}, ##]},
								1 - d < actual/target < 1 + d],
							{\[FormalX], xDataRange[[1]], xDataRange[[2]]}, {\[FormalZ], zDataRange[[1]], zDataRange[[2]]},
							Evaluate[deviationInterpOpts]]],
					{d, deviation}]&[integrationOpts];
				
				(* For each RegionPlot... *)
				regionPlotLines = Map[
					Function[plot,
						(* ...extract all lines from the RegionPlot. *)
						lines = Cases[
							Normal[plot],
							Line[{ls:{{_, _}..}..}] | Line[l:{{_, _}..}] :> Splice[{ls, l}],
							Infinity];
						(* Then, for each line... *)
						lines = Map[
							Function[line,
								(* ...If a point lies close to zero in either dimension, and the points either side do
									too, then remove the point and split the line into two. *)
								temp = If[
									And @@ (Apply[Or] @* Map[LessThan[.001 \[Rho]c]] /@ Abs[{##}]),
									(* Indicate a line split by replacing the point with "break". *)
									"break",
									(* Otherwise just return the point (the second of the three being compared). *)
									#2]& @@@
										Join[
											(* Split the line into groups of three consecutive points, ready for comparision. *)
											{{line[[1]], line[[1]], line[[2]]}},
											Partition[line, 3, 1],
											{{line[[-2]], line[[-1]], line[[-1]]}}];
								(* Split the list by looking for runs of "break", and then delete those runs. *)
								Splice[DeleteCases[SplitBy[temp, # =!= "break" &], {"break"...} | {_}, 1]]],
							lines]],
					regionPlots];

				(* RegionPlot may have connected up separate regions. To avoid these erronious joins, we flatten the
						points (rather than accepting the lines that RegionPlot generated) so that we may run
						ReconstructionMesh on them later, which does a better job at identifying separate regions. *)
				contours = Partition[Flatten[#], 2]& /@ regionPlotLines;
				
				(* Identify the contour by applying ReconstructionMesh to the flattened points. *)
				contours = MapIndexed[
					Function[{points, index},
						Replace[
							Replace[TrueQ[<|otherOpts|>["ReconstructMesh"]],
								True -> Quiet @ ReconstructionMesh[points, Method -> "Crust"]],
							{
								(* If the reconstruction succeeded, extract the mesh primitives. *)
								mesh_MeshRegion :> (
									temp = MeshPrimitives[mesh, 1];
									(* Filter out especially long lines (likely errors in the mesh reconstruction). *)
									temp = Pick[
										temp,
										# < 5 & /@ Abs[Standardize[Apply[EuclideanDistance] @@@ temp]]];
									temp = Flatten[temp, 1, Line];
									(* MeshPrimitives returns individual lines for each segment of the contour, e.g.:
										{..., {pt1, pt2}, {pt2, pt3}, {pt3, pt4}, ...}
										Hence we need to find runs of point pairs where the last point of one pair
										is the first point of the next. *)
									temp = Split[temp, Last[#1] === First[#2]&];
									temp = Join[#[[All, 1]], {#[[-1, -1]]}]& /@ temp),

								(* If the reconstruction failed, use the region computed by RegionPlot, transforming
									it into the remaining three quadrants. *)
								_ReconstructionMesh | False :> (temp = regionPlotLines[[First[index]]])}];
						(* Populate the remaining three quadrants by transforming the points appropriately. *)
						Map[
							Line @ {
								#,
								If[xReflectQ, {-1, 1}#& /@ #, Nothing],
								If[xReflectQ && zReflectQ, {-1, -1}#& /@ #, Nothing],
								If[zReflectQ, {1, -1}#& /@ #, Nothing]}&,
							temp]],
					contours];
				
				(* Apply the appropriate styling to each deviation contour. *)
				contours = MapThread[{CapForm["Square"], #1, #2}&, {deviationStyle, contours}]],
			
			integrandXZGrad, {2}],
		
		(* If no deviations are specified, then just return None. *)
		contours = None];

	const = QuantityMagnitude[UnitConvert[Quantity[1, "MagneticConstant"]]]/(4 Pi);
	(* Micro Tesla *)
	const = 10^6 const;

	bFieldRange = Replace[
		Lookup[<|plotOpts|>, PlotRange, Automatic],
		{
			{_, _, rb_?NumericQ} :> Table[{-1, 1} Abs[rb], 3],
			{_, _, rb:{_?NumericQ, _?NumericQ}} :> Table[rb, 3],
			_ :> const Replace[
				{-1, 1} Max[Abs[#]]& /@ Transpose[
					Replace[
						Select[data, Apply[If[Max @ Abs[xPlotRange] < .8 \[Rho]c, 0, .7] \[Rho]c < #1 <= .8 \[Rho]c &]],
						{} -> data
					][[All, -1]]],
				0 | 0. -> 1/const,
				1]}];

	(* If n+m is odd, coil is symmetric along z. Bz symmetric along z; Bx, By antisymmetric along z. *)
	If[OddQ[n + m],
		bzAlongZ = 1; bxbyAlongZ = -1,
		bzAlongZ = -1; bxbyAlongZ = 1];

	(* If m is odd, coil is antisymmetric along x. Bz is antisymmetric along x; Bx, By, are symmetric along x. *)
	If[OddQ[m],
		bzAlongX = -1; bxbyAlongX = 1,
		bzAlongX = 1; bxbyAlongX = -1];

	(* Calculate data for the remaining quadrants. *)
	data = Join[
		(* +x, +z *)
		data,
		(* -x, +z *)
		If[xReflectQ,
			{-#1, #2, {bxbyAlongX, bxbyAlongX, bzAlongX}*#3}& @@@ data,
			{}],
		(* -x, -z *)
		If[xReflectQ && zReflectQ,
			{-#1, -#2, {bxbyAlongX bxbyAlongZ, bxbyAlongX bxbyAlongZ, bzAlongX bzAlongZ}*#3}& @@@ data,
			{}],
		(* +x, -z *)
		If[zReflectQ,
			{#1, -#2, {bxbyAlongZ, bxbyAlongZ, bzAlongZ}*#3}& @@@ data,
			{}]];
	
	(* Construct the DensityPlots. *)

	(* First, create a DensityPlot for each of the field components. *)
	densityPlots = MapThread[

		Function[{dim, str},
		
			ListDensityPlot[
				{#1, #2, const #3[[dim]]}& @@@ data,
				PlotLegends -> BarLegend[
					Automatic,
					LegendLabel -> TraditionalForm @ StringReplacePart[
						"\!\(\*SubscriptBox[\(B\), \(x\)]\)\[VeryThinSpace]\!\(\*SuperscriptBox[SubscriptBox[\(I\), \(\[Chi]\)], \(-1\)]\) (\[Mu]T\[VeryThinSpace]/\[VeryThinSpace]A)",
						str, {23, 23}],
					First @ Normal @ Merge[
						{{LabelStyle -> {"Graphics", "GraphicsLabel"}}, FilterRules[{plotOpts}, LabelStyle]},
						Flatten[#, 2]&]],
				Sequence @@ Replace[{plotOpts},
					{
						(PlotRangePadding -> Automatic) -> (PlotRangePadding -> .02 Max[Abs @* Subtract @@@ {xPlotRange, zPlotRange}]),
						(PlotRange -> Automatic) -> (PlotRange -> {Full, Full, {bFieldRange[[dim, 1]], bFieldRange[[dim, 2]]}}),
						(PlotRange -> {x_, y_, Automatic..., ___}) :> (PlotRange -> {x, y, {bFieldRange[[dim, 1]], bFieldRange[[dim, 2]]}}),
						(ClippingStyle -> Automatic) :> (ClippingStyle -> Replace[
							Replace[<|plotOpts|>[ColorFunction], s_String :> ColorData[s]] /@ {0, 1},
							Except[{__?ColorQ}] -> White])
					},
					1]]],

		{{1, 2, 3}, {"x", "y", "z"}}];
	
	(* If there are no deviation contours to draw, then return {{plotBx}, {plotBy}, {plotBz}}. Otherwise combine the DensityPlots
		with the contours and labels. *)
	If[contours === None, List /@ densityPlots,
		
		(* Now create copies of the DensityPlots for each set of deviation contours. *)
		densityPlots = MapThread[
			Function[{contourSets, dim}, Table[densityPlots[[dim]], Length[contourSets]]],
			{contours, {1, 2, 3}}];
		
		showMeshQ = Not @ Or[
			MatchQ[<|plotOpts|>[Mesh], {} | False | None],
			MatchQ[<|plotOpts|>[MeshStyle], None | {} | Opacity[0]]];

		(* Finally, combine the DensityPlots, deviation contours, and plot labels. *)
		raggedMapThread[

			Function[{plot, contourSet, label},
				Module[
					{plotImageSize, dashingScaling},
					plotImageSize = ImageSize /. AbsoluteOptions[plot, ImageSize];
					(* Scale non-absolute dashings and thicknesses by the longest side of the graphic rather than the
						width (which is the defualt behaviour). This is to ensure that plots with extreme aspect ratios (such as
						tall and narrow saddle field plots) display with consistent dashing styles and thicknesses. *)
					dashingScaling = Max[plotImageSize] / First[plotImageSize];
					(* Furthermore, if the deviation style is Automatic, then negate the actual size of the plot for the dashing and
						thickness of deviation contours. We don't simply use AbsoluteDashing/Thickness because we want the dashing/thickness
						to scale with the plot if it's resized. *)
					If[<|otherOpts|>["DeviationsStyle"] === Automatic,
						(* If a symbolic (Small, Medium, etc.) or single number ImageSize is specified, then the most space-filling aspect
							ratio possible is 360/432 (which are the actual maximum width/height values for ImageSize -> Medium). If the
							aspect ratio is greater than this, then the height will decrease while the width is fixed at 360, and if the
							aspect ratio is less, then the width will decrease while the height is fixed at 432. The default dashing
							and thickness spec has been designed for the {360, 432} ImageSize, but we want the dashing and thickness to remain
							the same absolute size for smaller and larger plots. Therefore if the aspect ratio is greater than 360/432 then
							we rescale the dashing/thickness by width, and by height if it's less. *)
						dashingScaling = dashingScaling If[Divide @@ plotImageSize > 360/432., First, Last][{360., 432.} / plotImageSize]];

					If[
						label === None, plot,
						Labeled[

							Show[
								plot,
								Graphics[contourSet /. {
									Dashing[{r__?NumericQ}, spec___] :> Dashing[dashingScaling {r}, spec],
									Thickness[r_?NumericQ, spec___] :> Thickness[dashingScaling r, spec]}]],

							Grid[
								Join[
									{{Row[{label, " Deviation:"}]}},
									MapThread[
										{Row[{
											Framed[
												Graphics[
													{
														#1 /. {
															Dashing[{r__?NumericQ}, spec___] :> Dashing[dashingScaling {r}, spec],
															Thickness[r_?NumericQ, spec___] :> Thickness[dashingScaling r, spec]},
														Line[{{-1, 0}, {1, 0}}]},
													PlotRange -> 1,
													AspectRatio -> Full,
													ImageSize -> plotImageSize,
													Background -> Switch[
														GrayLevel[Last[Cases[#1, _?ColorQ, Infinity], 0]],
														GrayLevel[g_, ___] /; g >= .7,
														Black,
														_,
														None]],
												ImageSize -> {35, 8}, Alignment -> {Left, Center},
												FrameStyle -> None, FrameMargins -> None,
												BaselinePosition -> Scaled[0]],
											" ",
											Round[100 #2, .01],
											"%"}]}&,
										{deviationStyle, deviation}],
									If[showMeshQ,
										{{Row[{
											Framed[
												Graphics[
													{
														Replace[
															Lookup[<|plotOpts|>, MeshStyle, GrayLevel[0, .3]],
															Automatic -> GrayLevel[0, .3]] /. {
																Dashing[{r__?NumericQ}, spec___] :> Dashing[dashingScaling {r}, spec],
																Thickness[r_?NumericQ, spec___] :> Thickness[dashingScaling r, spec]},
														Line[{{-1, 0}, {1, 0}}]},
													PlotRange -> 1,
													AspectRatio -> Full,
													ImageSize -> plotImageSize],
												ImageSize -> {35, 8}, Alignment -> {Left, Center},
												FrameStyle -> None, FrameMargins -> None,
												BaselinePosition -> Scaled[0]],
												" ", "Flux Lines"}]}},
										{}]],
								Alignment -> Left,
								Spacings -> If[showMeshQ,
									{1, {.8, .6, {.2}, 1.2, .6}},
									{1, {.8, .6, {.2}, .6}}],
								Dividers -> If[showMeshQ,
									{{True, {False}, True}, {True, {False}, True, True}},
									{{True, {False}, True}, {True, {False}, True}}],
								FrameStyle -> Replace[
									Lookup[Association[plotOpts], FrameStyle, GrayLevel[.75]],
									Automatic | {} -> GrayLevel[.75]],
								BaseStyle -> Merge[
									{{LabelStyle -> {"Graphics", "GraphicsLabel"}}, FilterRules[{plotOpts}, {LabelStyle}]},
									Flatten[#, 2]&][LabelStyle]],

							Right]]]],

			{densityPlots, contours, derivativeLabels},
			2]]]


raggedMapThread[fn_, {lists__}, level_, currentLevel_:1] :=
	If[currentLevel === level,
		MapThread[fn, {lists}],
		MapThread[raggedMapThread[fn, {##}, level, currentLevel + 1]&, {lists}]]


(* ::Subsection::Closed:: *)
(*Loops*)


loopIntegrand[\[Chi]c_, i\[Chi]_, \[Rho]c_, nDes_] := Module[
	{sym, l, dl},

	sym = If[EvenQ[nDes], -1, 1];

	l = Map[
		Function[{\[Chi]cp},
			\[Rho]c {Cos[2 \[Pi] \[FormalU]], Sin[2 \[Pi] \[FormalU]], \[Chi]cp}],
		Join[-\[Chi]c, \[Chi]c]];

	dl = D[l, \[FormalU]];

	Total @ MapThread[
		Function[{li, dli, i},
			i dli \[Cross] ({\[FormalX], \[FormalY], \[FormalZ]} - li) / Norm[({\[FormalX], \[FormalY], \[FormalZ]} - li)]^3],
		{l, dl, Join[sym i\[Chi], i\[Chi]]}]]


Options[LoopFieldPlot] = fieldPlotOpts;


LoopFieldPlot::BadSeparations = plotMessages["BadSeparations"];
LoopFieldPlot::BadCurrents = plotMessages["BadCurrents"];
LoopFieldPlot::BadDesired = plotMessages["BadDesired"];
LoopFieldPlot::BadRadius = plotMessages["BadRadius"];


LoopFieldPlot[\[Chi]c_, i\[Chi]_, \[Rho]c_, nDes_, opts:OptionsPattern[]] /; (
	CheckArguments[LoopFieldPlot[\[Chi]c, i\[Chi], \[Rho]c, nDes, opts], 4] &&
	loopPlotChecks[LoopFieldPlot][\[Chi]c, i\[Chi], \[Rho]c, nDes]
):=
	Module[
		{plotOpts, pad = -.05, \[Chi]cVals, zMax},

		plotOpts = FilterRules[
			Normal[Merge[{Options[LoopFieldPlot], {opts}}, Last]],
			Options[Plot]];

		(* If \[Chi]c is a list of Coil\[Chi]c[index] -> val rules, then sort by index and take the vals. *)
		\[Chi]cVals = Replace[\[Chi]c, l:{__Rule} :> SortBy[
			Cases[l, (Coil\[Chi]c[i_] -> val_) :> {i, val}],
			First][[All, 2]]];
		
		zMax = \[Rho]c (Last[\[Chi]cVals] + pad);

		biotSavartPlot[
			loopIntegrand[\[Chi]cVals, i\[Chi], \[Rho]c, nDes],
			\[Rho]c, pad, zMax, plotOpts]]


Options[LoopFieldPlot2D] = fieldPlot2DOpts;


LoopFieldPlot2D::BadSeparations = plotMessages["BadSeparations"];
LoopFieldPlot2D::BadCurrents = plotMessages["BadCurrents"];
LoopFieldPlot2D::BadDesired = plotMessages["BadDesired"];
LoopFieldPlot2D::BadRadius = plotMessages["BadRadius"];


LoopFieldPlot2D[\[Chi]c_, i\[Chi]_, \[Rho]c_, nDes_, opts:OptionsPattern[]] /; (
	CheckArguments[LoopFieldPlot2D[\[Chi]c, i\[Chi], \[Rho]c, nDes, opts], 4] &&
	loopPlotChecks[LoopFieldPlot2D][\[Chi]c, i\[Chi], \[Rho]c, nDes]
):=
	Module[
		{allOpts, interpolationOpts, plotOpts, pad = .1, \[Chi]cVals, zMax},

		(* Split options into those for interpolation (fed to DensityPlot) and
			those for plotting (fed to ListDensityPlot). *)
		allOpts = Normal[Merge[{Options[LoopFieldPlot2D], {opts}}, Last]];
		interpolationOpts = FilterRules[allOpts, interpolationOptions];
		plotOpts = FilterRules[Complement[allOpts, interpolationOpts], Options[ListDensityPlot]];

		(* If \[Chi]c is a list of Coil\[Chi]c[index] -> val rules, then sort by index and take the vals. *)
		\[Chi]cVals = Replace[\[Chi]c, l:{__Rule} :> SortBy[
			Cases[l, (Coil\[Chi]c[i_] -> val_) :> {i, val}],
			First][[All, 2]]];
		
		zMax = \[Rho]c (Last[\[Chi]cVals] + pad);

		biotSavartPlot2D[
			loopIntegrand[\[Chi]cVals, i\[Chi], \[Rho]c, nDes],
			\[Chi]cVals, i\[Chi], None, None, \[Rho]c, {nDes, 0}, pad, zMax, interpolationOpts, plotOpts, allOpts]]


(* ::Subsection::Closed:: *)
(*Saddles*)


saddleIntegrand[\[Chi]c_, \[Phi]c_, i\[Chi]_, \[Rho]c_, {nDes_, mDes_}] := Module[
	{symQ, arcCentres, periodicities, \[Chi]cPairs, saddleIs, prims},

	(* Is the coil axially symmetric? *)
	symQ = OddQ[nDes + mDes];

	arcCentres = Array[Identity, 2 mDes + 1, {0, 2 Pi}][[;;-2]];
	periodicities = PadRight[{}, 2 mDes, {1, -1}];

	If[symQ,
		(* If the coil is axially symmetric, then subsequent pairs of arcs are joined together. *)

		\[Chi]cPairs = Partition[\[Chi]c, 2];
		saddleIs = i\[Chi][[;; ;; 2]];

		prims = Table[
			Function[{centre, periodicity, sepPair, i},
			
				With[
					{
						x = Cos[2 \[Phi]cp (\[FormalU] - .5) + Abs[centre]],
						xR = Cos[2 \[Phi]cp ((1 - \[FormalU]) - .5) + Abs[centre]],
						y = Sin[2 \[Phi]cp (\[FormalU] - .5) + Abs[centre]],
						yR = Sin[2 \[Phi]cp ((1 - \[FormalU]) - .5) + Abs[centre]]
					},
					{
						(* Positive z *)
						<|(* First arc *)
							"i" -> periodicity i,
							"l" -> \[Rho]c {x, y, sepPair[[1]]}|>,
						<|(* Right connector *)
							"i" -> periodicity i,
							"l" -> \[Rho]c Append[{x, y} /. \[FormalU] -> 1, sepPair[[1]] + \[FormalU] Abs[Subtract@@sepPair]]|>,
						<|(* Second arc *)
							"i" -> periodicity i,
							"l" -> \[Rho]c {xR, yR, sepPair[[2]]}|>,
						<|(* Left connector *)
							"i" -> periodicity i,
							"l" -> \[Rho]c Append[{xR, yR} /. \[FormalU] -> 1, sepPair[[2]] - \[FormalU] Abs[Subtract@@sepPair]]|>,
						
						(* Negative z *)
						<|(* First arc *)
							"i" -> periodicity i,
							"l" -> \[Rho]c {x, y, -sepPair[[1]]}|>,
						<|(* Right connector *)
							"i" -> periodicity i,
							"l" -> \[Rho]c Append[{x, y} /. \[FormalU] -> 1, -sepPair[[1]] - \[FormalU] Abs[Subtract@@sepPair]]|>,
						<|(* Second arc *)
							"i" -> periodicity i,
							"l" -> \[Rho]c {xR, yR, -sepPair[[2]]}|>,
						<|(* Left connector *)
							"i" -> periodicity i,
							"l" -> \[Rho]c Append[{xR, yR} /. \[FormalU] -> 1, -sepPair[[2]] + \[FormalU] Abs[Subtract@@sepPair]]|>

			}]] @@ Join[centrePeriod, \[Chi]cPairi\[Chi]],

			{centrePeriod, Transpose[{arcCentres, periodicities}]},
			{\[Phi]cp, \[Phi]c},
			{\[Chi]cPairi\[Chi], Transpose[{\[Chi]cPairs, saddleIs}]}];
		
		prims = Flatten[prims, 3],


		(* If the coil is axially antisymmetric, then each arc with +ve z is joined to its corresponding arc with -ve z. *)

		prims = Table[
			Function[{centre, periodicity, sep, i},
			
				With[
					{
						x = Cos[2 \[Phi]cp (\[FormalU] - .5) + Abs[centre]],
						xR = Cos[2 \[Phi]cp ((1 - \[FormalU]) - .5) + Abs[centre]],
						y = Sin[2 \[Phi]cp (\[FormalU] - .5) + Abs[centre]],
						yR = Sin[2 \[Phi]cp ((1 - \[FormalU]) - .5) + Abs[centre]]
					},
					{
						<|(* Top arc *)
							"i" -> periodicity i,
							"l" -> \[Rho]c {x, y, sep}|>,
						<|(* Right connector *)
							"i" -> periodicity i,
							"l" -> \[Rho]c Append[{x, y} /. \[FormalU] -> 1, sep(1 - 2 \[FormalU])]|>,
						<|(* Bottom arc *)
							"i" -> periodicity i,
							"l" -> \[Rho]c {xR, yR, -sep}|>,
						<|(* Left connector *)
							"i" -> periodicity i,
							"l" -> \[Rho]c Append[{xR, yR} /. \[FormalU] -> 1, sep(-1 + 2 \[FormalU])]|>

			}]] @@ Join[centrePeriod, \[Chi]ci\[Chi]],

			{centrePeriod, Transpose[{arcCentres, periodicities}]},
			{\[Phi]cp, \[Phi]c},
			{\[Chi]ci\[Chi], Transpose[{\[Chi]c, i\[Chi]}]}];

		prims = Flatten[prims, 3]
	];


	prims = Append[#, "dl" -> D[#["l"], \[FormalU]]]& /@ prims;
	
	Total @ Map[
		Function[
			#["i"] #["dl"] \[Cross] ({\[FormalX], \[FormalY], \[FormalZ]} - #["l"]) / Norm[({\[FormalX], \[FormalY], \[FormalZ]} - #["l"])]^3],
		prims]
]


Options[SaddleFieldPlot] = fieldPlotOpts;


SaddleFieldPlot::BadSeparations = plotMessages["BadSeparations"];
SaddleFieldPlot::BadExtents = plotMessages["BadExtents"];
SaddleFieldPlot::BadCurrents = plotMessages["BadCurrents"];
SaddleFieldPlot::BadCurrentRatios = plotMessages["BadCurrentRatios"];
SaddleFieldPlot::BadDesiredNM = plotMessages["BadDesiredNM"];
SaddleFieldPlot::BadRadius = plotMessages["BadRadius"];


SaddleFieldPlot[\[Chi]c_, \[Phi]c_, i\[Chi]_, \[Rho]c_, {nDes_, mDes_}, opts:OptionsPattern[]] /; (
	CheckArguments[SaddleFieldPlot[\[Chi]c, \[Phi]c, i\[Chi], \[Rho]c, {nDes, mDes}, opts], 5] &&
	saddlePlotChecks[SaddleFieldPlot][\[Chi]c, \[Phi]c, i\[Chi], \[Rho]c, {nDes, mDes}]
):=
	Module[
		{plotOpts, pad = -.05, \[Chi]cVals, \[Phi]cVals, zMax},

		plotOpts = FilterRules[
			Normal[Merge[{Options[SaddleFieldPlot], {opts}}, Last]],
			Options[Plot]];

		(* If \[Chi]c is a list of Coil\[Chi]c[index] -> val rules, then sort by index and take the vals. *)
		\[Chi]cVals = Replace[\[Chi]c, l:{__Rule} :> SortBy[
			Cases[l, (Coil\[Chi]c[i_] -> val_) :> {i, val}],
			First][[All, 2]]];
		(* If \[Phi]c is a list of Coil\[Phi]c[index] -> val rules, then sort by index and take the vals. *)
		\[Phi]cVals = Replace[\[Phi]c, l:{__Rule} :> SortBy[
			Replace[l, (Coil\[Phi]c[i_] -> val_) :> {i, val}, 1],
			First][[All, 2]]];
		
		zMax = \[Rho]c (Last[\[Chi]cVals] + pad);

		biotSavartPlot[
			saddleIntegrand[\[Chi]cVals, \[Phi]cVals, i\[Chi], \[Rho]c, {nDes, mDes}],
			\[Rho]c, pad, zMax, plotOpts]]


Options[SaddleFieldPlot2D] = fieldPlot2DOpts;


SaddleFieldPlot2D::BadSeparations = plotMessages["BadSeparations"];
SaddleFieldPlot2D::BadExtents = plotMessages["BadExtents"];
SaddleFieldPlot2D::BadCurrents = plotMessages["BadCurrents"];
SaddleFieldPlot2D::BadCurrentRatios = plotMessages["BadCurrentRatios"];
SaddleFieldPlot2D::BadDesiredNM = plotMessages["BadDesiredNM"];
SaddleFieldPlot2D::BadRadius = plotMessages["BadRadius"];


SaddleFieldPlot2D[\[Chi]c_, \[Phi]c_, i\[Chi]_, \[Rho]c_, {nDes_, mDes_}, opts:OptionsPattern[]] /; (
	CheckArguments[SaddleFieldPlot2D[\[Chi]c, \[Phi]c, i\[Chi], \[Rho]c, {nDes, mDes}, opts], 5] &&
	saddlePlotChecks[SaddleFieldPlot2D][\[Chi]c, \[Phi]c, i\[Chi], \[Rho]c, {nDes, mDes}]
):=
	Module[
		{allOpts, interpolationOpts, plotOpts, pad = .1, \[Chi]cVals, \[Phi]cVals, zMax},

		(* Split options into those for interpolation (fed to DensityPlot) and
			those for plotting (fed to ListDensityPlot). *)
		allOpts = Normal[Merge[{Options[SaddleFieldPlot2D], {opts}}, Last]];
		interpolationOpts = FilterRules[allOpts, interpolationOptions];
		plotOpts = FilterRules[Complement[allOpts, interpolationOpts], Options[ListDensityPlot]];

		(* If \[Chi]c is a list of Coil\[Chi]c[index] -> val rules, then sort by index and take the vals. *)
		\[Chi]cVals = Replace[\[Chi]c, l:{__Rule} :> SortBy[
			Cases[l, (Coil\[Chi]c[i_] -> val_) :> {i, val}],
			First][[All, 2]]];
		(* If \[Phi]c is a list of Coil\[Phi]c[index] -> val rules, then sort by index and take the vals. *)
		\[Phi]cVals = Replace[\[Phi]c, l:{__Rule} :> SortBy[
			Replace[l, (Coil\[Phi]c[i_] -> val_) :> {i, val}, 1],
			First][[All, 2]]];
		
		zMax = \[Rho]c (Last[\[Chi]cVals] + pad);

		biotSavartPlot2D[
			saddleIntegrand[\[Chi]cVals, \[Phi]cVals, i\[Chi], \[Rho]c, {nDes, mDes}],
			\[Chi]cVals, i\[Chi], \[Phi]cVals, None, \[Rho]c, {nDes, mDes}, pad, zMax, interpolationOpts, plotOpts, allOpts]]


(* ::Subsection::Closed:: *)
(*Ellipses*)


ellipseIntegrand[\[Chi]c_, \[Psi]c_, i\[Chi]_, \[Rho]c_, {nDes_, mDes_}] := Module[
	{sym, phases, periodicities, prims},

	sym = If[EvenQ[nDes + mDes], -1, 1];

	phases = Array[Identity, 2 mDes + 1, {0, 2 Pi}][[;;-2]];
	periodicities = PadRight[{}, 2 mDes, {1, -1}];

	prims = Table[
		Function[{phase, periodicity, sep, i, t}, {

			<|(* Positive z *)
				"i" -> periodicity i,
				"l" -> \[Rho]c {
					Cos[2 \[Pi] \[FormalU]],
					Sin[2 \[Pi] \[FormalU]],
					sep + t Cos[2 \[Pi] \[FormalU] + phase]}|>,
			<|(* Negative z *)
				"i" -> sym periodicity i,
				"l" -> \[Rho]c {
					Cos[2 \[Pi] \[FormalU]],
					Sin[2 \[Pi] \[FormalU]],
					-sep - t Cos[2 \[Pi] \[FormalU] + phase]}|>

		}] @@ Join[phasePeriod, \[Chi]ci\[Chi]\[Psi]c],

		{phasePeriod, Transpose[{phases, periodicities}]},
		{\[Chi]ci\[Chi]\[Psi]c, Transpose[{\[Chi]c, i\[Chi], \[Psi]c}]}];

	prims = Flatten[prims, 3];

	prims = Append[#, "dl" -> D[#["l"], \[FormalU]]]& /@ prims;
	
	Total @ Map[
		Function[
			#["i"] #["dl"] \[Cross] ({\[FormalX], \[FormalY], \[FormalZ]} - #["l"]) / Norm[({\[FormalX], \[FormalY], \[FormalZ]} - #["l"])]^3],
		prims]
]


Options[EllipseFieldPlot] = fieldPlotOpts;


EllipseFieldPlot::BadSeparations = plotMessages["BadSeparations"];
EllipseFieldPlot::BadCurrents = plotMessages["BadCurrents"];
EllipseFieldPlot::BadDesired = plotMessages["BadDesired"];
EllipseFieldPlot::BadRadius = plotMessages["BadRadius"];


EllipseFieldPlot[\[Chi]c\[Psi]c_, i\[Chi]_, \[Rho]c_, {nDes_, mDes_}, opts:OptionsPattern[]] /; (
	CheckArguments[EllipseFieldPlot[\[Chi]c\[Psi]c, i\[Chi], \[Rho]c, {nDes, mDes}, opts], 4] &&
	ellipsePlotChecks[EllipseFieldPlot][\[Chi]c\[Psi]c, i\[Chi], \[Rho]c, {nDes, mDes}]
):=
	Module[
		{plotOpts, pad = -.05, \[Chi]cVals, \[Psi]cVals, zMax},

		plotOpts = FilterRules[
			Normal[Merge[{Options[EllipseFieldPlot], {opts}}, Last]],
			Options[Plot]];

		{\[Chi]cVals, \[Psi]cVals} = Switch[
			\[Chi]c\[Psi]c,
			(* If \[Chi]c\[Psi]c is a list of {\[Chi]c, \[Psi]c} pairs, then transpose and assign. *)
			{{_, _}..},
			Transpose[\[Chi]c\[Psi]c],
			(* If \[Chi]c\[Psi]c is a list of Coil\[Chi]c[index] -> val and Coil\[Psi]c[index] -> val rules, then sort each by index and take the vals. *)
			{__Rule},
			SortBy[Cases[\[Chi]c\[Psi]c, (#[i_] -> val_) :> {i, val}], First][[All, 2]]& /@ {Coil\[Chi]c, Coil\[Psi]c}];
		
		zMax = \[Rho]c (Max[\[Chi]cVals + \[Psi]cVals] + pad);

		biotSavartPlot[
			ellipseIntegrand[\[Chi]cVals, \[Psi]cVals, i\[Chi], \[Rho]c, {nDes, mDes}],
			\[Rho]c, pad, zMax, plotOpts]]


Options[EllipseFieldPlot2D] = fieldPlot2DOpts;


EllipseFieldPlot2D::BadSeparations = plotMessages["BadSeparations"];
EllipseFieldPlot2D::BadCurrents = plotMessages["BadCurrents"];
EllipseFieldPlot2D::BadDesired = plotMessages["BadDesired"];
EllipseFieldPlot2D::BadRadius = plotMessages["BadRadius"];


EllipseFieldPlot2D[\[Chi]c\[Psi]c_, i\[Chi]_, \[Rho]c_, {nDes_, mDes_}, opts:OptionsPattern[]] /; (
	CheckArguments[EllipseFieldPlot2D[\[Chi]c\[Psi]c, i\[Chi], \[Rho]c, {nDes, mDes}, opts], 4] &&
	ellipsePlotChecks[EllipseFieldPlot2D][\[Chi]c\[Psi]c, i\[Chi], \[Rho]c, {nDes, mDes}]
):=
	Module[
		{allOpts, interpolationOpts, plotOpts, pad = .1, \[Chi]cVals, \[Psi]cVals, zMax},

		(* Split options into those for interpolation (fed to DensityPlot) and
			those for plotting (fed to ListDensityPlot). *)
		allOpts = Normal[Merge[{Options[EllipseFieldPlot2D], {opts}}, Last]];
		interpolationOpts = FilterRules[allOpts, interpolationOptions];
		plotOpts = FilterRules[Complement[allOpts, interpolationOpts], Options[ListDensityPlot]];

		{\[Chi]cVals, \[Psi]cVals} = Switch[
			\[Chi]c\[Psi]c,
			(* If \[Chi]c\[Psi]c is a list of {\[Chi]c, \[Psi]c} pairs, then transpose and assign. *)
			{{_, _}..},
			Transpose[\[Chi]c\[Psi]c],
			(* If \[Chi]c\[Psi]c is a list of Coil\[Chi]c[index] -> val and Coil\[Psi]c[index] -> val rules, then sort each by index and take the vals. *)
			{__Rule},
			SortBy[Cases[\[Chi]c\[Psi]c, (#[i_] -> val_) :> {i, val}], First][[All, 2]]& /@ {Coil\[Chi]c, Coil\[Psi]c}];
		
		zMax = \[Rho]c (Max[\[Chi]cVals + \[Psi]cVals] + pad);

		biotSavartPlot2D[
			ellipseIntegrand[\[Chi]cVals, \[Psi]cVals, i\[Chi], \[Rho]c, {nDes, mDes}],
			\[Chi]cVals, i\[Chi], None, \[Psi]cVals, \[Rho]c, {nDes, mDes}, pad, zMax, interpolationOpts, plotOpts, allOpts]]


(* ::Subsection::Closed:: *)
(*Harmonic Plot*)


harmonicPlotChecks[{n_, m_}] := Module[
	{proceed = True},
	(* Check that arguments have been specified correctly, and issue messages if not. *)

	(* nDes and mDes must be integers that satisfy n >= m >= 0 and n > 0. *)
	If[!MatchQ[{n, m}, {np_Integer, mp_Integer} /; np >= mp >= 0 && np > 0],
		Message[HarmonicFieldPlot::BadNM, n, m]; proceed = False];
	
	proceed]


Options[HarmonicFieldPlot] = Normal @ Merge[
	{
		fieldPlotOpts,
		{PlotRange -> All}
	},
	Last];


HarmonicFieldPlot::BadNM = plotMessages["BadNM"];


HarmonicFieldPlot[{n_, m_}, opts:OptionsPattern[]] /; (
	CheckArguments[HarmonicFieldPlot[{n, m}, opts], 1] &&
	harmonicPlotChecks[{n, m}]
) :=
	Module[
		{data, allOpts, interpolationOpts, plotOpts},

		(* Split options into those for interpolation (fed to Plot) and
			those for plotting (fed to ListLinePlot). *)
		allOpts = Normal[Merge[{Options[HarmonicFieldPlot], {opts}}, Last]];
		interpolationOpts = Sequence @@ FilterRules[allOpts, Complement[Options[Plot][[All, 1]], Options[ListLinePlot][[All, 1]]]];
		plotOpts = Sequence @@ FilterRules[allOpts, Options[ListLinePlot]];

		data = MapThread[
			Function[{\[Theta], \[Phi]},
				Reap[
					Plot[
						Sow[{\[FormalR],
							Quiet @ bFieldHarmonicVector[{n, m}, Abs[\[FormalR]], \[Theta], \[Phi]]}
						][[2]],
						{\[FormalR], -1, 1}, PlotRange -> All, ##
					]][[-1, 1]]&[interpolationOpts]],
			{
				{Pi/2, Pi/2, If[\[FormalR] < 0, Pi, 0]},
				{If[\[FormalR] < 0, Pi, 0], If[\[FormalR] < 0, 3 Pi/2, Pi/2], 0}}];
		
		data = SortBy[First] /@ data;

		MapThread[
			Function[{dim, str},
				ListLinePlot[
					Transpose[First[Outer[List, {#1}, #2]]& @@@ data[[dim]]],
					Sequence @@ Replace[{plotOpts},
						{
							(FrameLabel -> Automatic) -> (FrameLabel -> {
								{
									TraditionalForm[Style["B", FontSlant -> Italic]],
									None},
								{
									TraditionalForm @ RawBoxes @ StringReplacePart[
										"\"\\!\\(\\*StyleBox[\\\"z\\\",FontSlant->\\\"Italic\\\"]\\)\"", str,
										{19, 19}],
									None}}),
							(PlotLegends -> Automatic) -> (PlotLegends -> Placed[LineLegend[
								TraditionalForm /@ {
									"\!\(\*SubscriptBox[\(B\), \(x\)]\)",
									"\!\(\*SubscriptBox[\(B\), \(y\)]\)",
									"\!\(\*SubscriptBox[\(B\), \(z\)]\)"},
								First @ Normal @ Merge[
									{{LabelStyle -> {"Graphics", "GraphicsLabel"}}, FilterRules[{plotOpts}, LabelStyle]},
									Flatten[#, 2]&],
								LegendLayout -> "Column"], After])
						},
						1]]],
			{{1, 2, 3}, {"x", "y", "z"}}]]


(* ::Section::Closed:: *)
(*Package Footer*)


Protect["CreateCoil`*"];


End[];
EndPackage[];
