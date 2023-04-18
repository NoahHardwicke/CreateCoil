(* ::Package:: *)

(* ::Title:: *)
(*Discrete Coils*)


(* ::Section::Closed:: *)
(*Package Header*)


Unprotect["CreateCoil`*", "CreateCoil`Private`*"];
ClearAll["CreateCoil`*", "CreateCoil`Private`*"];


BeginPackage["CreateCoil`"];


FindLoopCoil::usage =
"\!\(\*RowBox[{\"FindLoopCoil\", \"[\", RowBox[{RowBox[{\"{\", \
RowBox[{SubscriptBox[StyleBox[\"i\[Chi]\", \"TI\"], StyleBox[\"1\", \
\"TR\"]], \",\", \" \", SubscriptBox[StyleBox[\"i\[Chi]\", \"TI\"], \
StyleBox[\"2\", \"TR\"]], \",\", \" \", StyleBox[\"\[Ellipsis]\", \
\"TR\"]}], \"}\"}], \",\", \" \", StyleBox[\"n\", \"TI\"], \",\", \" \
\", RowBox[{\"{\", RowBox[{SubscriptBox[StyleBox[\"\[Chi]c\", \
\"TI\"], StyleBox[\"min\", \"TI\"]], \",\", \" \", \
SubscriptBox[StyleBox[\"\[Chi]c\", \"TI\"], StyleBox[\"max\", \
\"TI\"]]}], \"}\"}]}], \"]\"}]\) returns, for loop pairs of turn \
ratios \!\(\*SubscriptBox[StyleBox[\"i\[Chi]\", \"TI\"], StyleBox[\"1\
\", \"TR\"]]\), \!\(\*SubscriptBox[StyleBox[\"i\[Chi]\", \"TI\"], \
StyleBox[\"2\", \"TR\"]]\), \!\(\*StyleBox[\"\[Ellipsis]\", \
\"TR\"]\), axial separations between \!\(\*SubscriptBox[StyleBox[\"\
\[Chi]c\", \"TI\"], StyleBox[\"min\", \"TI\"]]\) and \
\!\(\*SubscriptBox[StyleBox[\"\[Chi]c\", \"TI\"], StyleBox[\"max\", \
\"TI\"]]\) optimised to generate the field harmonic of order \
\!\(\*StyleBox[\"n\", \"TI\"]\).";

FindSaddleCoil::usage =
"\!\(\*RowBox[{\"FindSaddleCoil\", \"[\", RowBox[{RowBox[{\"{\", \
RowBox[{SubscriptBox[StyleBox[\"i\[Chi]\", \"TI\"], StyleBox[\"1\", \
\"TR\"]], \",\", \" \", SubscriptBox[StyleBox[\"i\[Chi]\", \"TI\"], \
StyleBox[\"2\", \"TR\"]], \",\", \" \", StyleBox[\"\[Ellipsis]\", \
\"TR\"]}], \"}\"}], \",\", \" \", RowBox[{\"{\", RowBox[{StyleBox[\"n\
\", \"TI\"], \",\", \" \", StyleBox[\"m\", \"TI\"]}], \"}\"}], \",\", \
\" \", StyleBox[\"k\[Phi]\", \"TI\"], \",\", \" \", RowBox[{\"{\", \
RowBox[{SubscriptBox[StyleBox[\"\[Chi]c\", \"TI\"], StyleBox[\"min\", \
\"TI\"]], \",\", \" \", SubscriptBox[StyleBox[\"\[Chi]c\", \"TI\"], \
StyleBox[\"max\", \"TI\"]]}], \"}\"}]}], \"]\"}]\) returns, for arc \
groups of turn ratios \!\(\*SubscriptBox[StyleBox[\"i\[Chi]\", \
\"TI\"], StyleBox[\"1\", \"TR\"]]\), \!\(\*SubscriptBox[StyleBox[\"i\
\[Chi]\", \"TI\"], StyleBox[\"2\", \"TR\"]]\), \!\(\*StyleBox[\"\
\[Ellipsis]\", \"TR\"]\), axial separations between \
\!\(\*SubscriptBox[StyleBox[\"\[Chi]c\", \"TI\"], StyleBox[\"min\", \
\"TI\"]]\) and \!\(\*SubscriptBox[StyleBox[\"\[Chi]c\", \"TI\"], \
StyleBox[\"max\", \"TI\"]]\) and azimuthal extents, optimised to \
generate the field harmonic of order \!\(\*StyleBox[\"n\", \"TI\"]\) \
and degree \!\(\*StyleBox[\"m\", \"TI\"]\), while nulling the first \
\!\(\*StyleBox[\"k\[Phi]\", \"TI\"]\) leading-order error harmonic \
degrees.";

FindSaddleCoilAxial::usage = 
"\!\(\*RowBox[{\"FindSaddleCoilAxial\", \"[\", RowBox[{RowBox[{\"{\", \
RowBox[{SubscriptBox[StyleBox[\"i\[Chi]\", \"TI\"], StyleBox[\"1\", \
\"TR\"]], \",\", \" \", SubscriptBox[StyleBox[\"i\[Chi]\", \"TI\"], \
StyleBox[\"2\", \"TR\"]], \",\", \" \", StyleBox[\"\[Ellipsis]\", \
\"TR\"]}], \"}\"}], \",\", \" \", RowBox[{\"{\", RowBox[{StyleBox[\"n\
\", \"TI\"], \",\", \" \", StyleBox[\"m\", \"TI\"]}], \"}\"}], \",\", \
\" \", StyleBox[\"k\[Phi]\", \"TI\"], \",\", \" \", RowBox[{\"{\", RowBox[{SubscriptBox[StyleBox[\"\[Chi]c\", \"TI\
\"], StyleBox[\"min\", \"TI\"]], \",\", \" \", SubscriptBox[StyleBox[\
\"\[Chi]c\", \"TI\"], StyleBox[\"max\", \"TI\"]]}], \"}\"}]}], \
\"]\"}]\) returns, for arc groups of turn ratios \
\!\(\*SubscriptBox[StyleBox[\"i\[Chi]\", \"TI\"], StyleBox[\"1\", \
\"TR\"]]\), \!\(\*SubscriptBox[StyleBox[\"i\[Chi]\", \"TI\"], \
StyleBox[\"2\", \"TR\"]]\), \!\(\*StyleBox[\"\[Ellipsis]\", \
\"TR\"]\), and \!\(\*StyleBox[\"k\[Phi]\", \"TI\"]\) azimuthal extents, axial separations between \!\(\*SubscriptBox[StyleBox[\"\
\[Chi]c\", \"TI\"], StyleBox[\"min\", \"TI\"]]\) and \
\!\(\*SubscriptBox[StyleBox[\"\[Chi]c\", \"TI\"], StyleBox[\"max\", \
\"TI\"]]\) optimised to generate the field harmonic of order \
\!\(\*StyleBox[\"n\", \"TI\"]\) and degree \!\(\*StyleBox[\"m\", \"TI\
\"]\).";

FindSaddleCoilAzimuthal::usage = 
"\!\(\*RowBox[{\"FindSaddleCoilAzimuthal\", \"[\", \
RowBox[{StyleBox[\"m\", \"TI\"], \",\", \" \", StyleBox[\"k\[Phi]\", \
\"TI\"]}], \"]\"}]\) returns azimuthal arc extents optimised to \
generate the field harmonic of degree \!\(\*StyleBox[\"m\", \
\"TI\"]\), while nulling the first \!\(\*StyleBox[\"k\[Phi]\", \
\"TI\"]\) leading-order error harmonic degrees.";

FindEllipseCoil::usage = 
"\!\(\*RowBox[{\"FindEllipseCoil\", \"[\", RowBox[{RowBox[{\"{\", \
RowBox[{SubscriptBox[StyleBox[\"i\[Chi]\", \"TI\"], StyleBox[\"1\", \
\"TR\"]], \",\", \" \", SubscriptBox[StyleBox[\"i\[Chi]\", \"TI\"], \
StyleBox[\"2\", \"TR\"]], \",\", \" \", StyleBox[\"\[Ellipsis]\", \
\"TR\"]}], \"}\"}], \",\", \" \", RowBox[{\"{\", RowBox[{StyleBox[\"n\
\", \"TI\"], \",\", \" \", StyleBox[\"m\", \"TI\"]}], \"}\"}], \",\", \
\" \", RowBox[{\"{\", RowBox[{SubscriptBox[StyleBox[\"\[Chi]c\", \"TI\
\"], StyleBox[\"min\", \"TI\"]], \",\", \" \", SubscriptBox[StyleBox[\
\"\[Chi]c\", \"TI\"], StyleBox[\"max\", \"TI\"]]}], \"}\"}], \",\", \
\" \", RowBox[{\"{\", RowBox[{SubscriptBox[StyleBox[\"\[Psi]\", \"TI\"], \
StyleBox[\"min\", \"TI\"]], \",\", \" \", \
SubscriptBox[StyleBox[\"\[Psi]\", \"TI\"], StyleBox[\"max\", \"TI\"]]}], \
\"}\"}]}], \"]\"}]\) returns, for ellipse groups of turn ratios \
\!\(\*SubscriptBox[StyleBox[\"i\[Chi]\", \"TI\"], StyleBox[\"1\", \
\"TR\"]]\), \!\(\*SubscriptBox[StyleBox[\"i\[Chi]\", \"TI\"], \
StyleBox[\"2\", \"TR\"]]\), \!\(\*StyleBox[\"\[Ellipsis]\", \
\"TR\"]\), axial separations between \!\(\*SubscriptBox[StyleBox[\"\
\[Chi]c\", \"TI\"], StyleBox[\"min\", \"TI\"]]\) and \
\!\(\*SubscriptBox[StyleBox[\"\[Chi]c\", \"TI\"], StyleBox[\"max\", \
\"TI\"]]\) and ellipse extents between \!\(\*SubscriptBox[StyleBox[\"\[Psi]\", \
\"TI\"], StyleBox[\"min\", \"TI\"]]\) and \
\!\(\*SubscriptBox[StyleBox[\"\[Psi]\", \"TI\"], StyleBox[\"max\", \
\"TI\"]]\), optimised to generate the field harmonic of order \
\!\(\*StyleBox[\"n\", \"TI\"]\) and degree \!\(\*StyleBox[\"m\", \"TI\
\"]\).";


Coil\[Chi]c::usage = "Normalized separation of loop/arc/ellipse pairs";


Coil\[Phi]::usage = "Azimuthal extent of a saddle";


Coil\[Psi]::usage = "Normalized ellipse extent";


DesToErr::usage = "Ratio of the desired-to-leading error harmonic magnitudes";


LoopCoilPlot::usage = "stub";


SaddleCoilPlot::usage = "stub";


EllipseCoilPlot::usage = "stub";


LoopFieldPlot::usage = "stub";


SaddleFieldPlot::usage = "stub";


EllipseFieldPlot::usage = "stub";


Begin["`Private`"];


(* ::Section::Closed:: *)
(*Messages*)


finderMessages = <|
	"BadCurrents" -> "Currents: `1` should be a list of two or more real numbers.",
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
	"BadSeparations" -> "Separations: `1` should either be a list of two or more ascending positive numbers, or a list of Coil\[Chi]c[i] -> \[Chi]ci rules, e.g. {Coil\[Chi]c[1] -> \[Chi]c1, Coil\[Chi]c[2] -> \[Chi]c2, \[Ellipsis]}, where \[Chi]c1 < \[Chi]c2 < \[Ellipsis]. In the latter case, the list can contain a DesToErr -> val rule (which will be ignored).",
	"BadCurrents" -> "Currents: `1` should be a list of two or more real numbers, equal in length to the number of separations.",
	"BadDesired" -> finderMessages["BadDesired"],
	"BadRadius" -> "Coil radius \[Rho]c = `1` should be a positive number.",
	"BadExtents" -> "Extents: `1` should either be a list of one or more ascending positive numbers, or a list of Coil\[Phi][i] -> \[Phi]i rules, e.g. {Coil\[Phi][1] -> \[Phi]1, Coil\[Phi][2] -> \[Phi]2, \[Ellipsis]}, where \[Phi]1 < \[Phi]2 < \[Ellipsis].",
	"BadCurrentRatios" -> finderMessages["BadCurrentRatios"],
	"BadDesiredNM" -> finderMessages["BadDesiredNM"],
	"BadChiPsi" -> "Axial separations and extents: `1` should either be a list of separation and extent pairs, i.e. {{\[Chi]c1, \[Psi]1}, {\[Chi]c2, \[Psi]2}, \[Ellipsis]}, or a flat list of Coil\[Chi]c[i] -> \[Chi]ci and Coil\[Psi][i] -> \[Psi]i rules, e.g. {Coil\[Chi]c[1] -> \[Chi]c1, Coil\[Psi][1] -> \[Psi]1, Coil\[Chi]c[2] -> \[Chi]c2, Coil\[Psi][2] -> \[Psi]2, \[Ellipsis]}, where there are as many extents as separations. In both cases, \[Chi]c1 < \[Chi]c2 < \[Ellipsis] must be satisfied. In the latter case, the list can contain a DesToErr -> val rule (which will be ignored)."
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
		pmPlus = p[{n - 1, m + 1}, Cos[\[Theta]]]},
		
		f[{n, m}] r^(n-1) {
			(* x component *)
			-(1 + kd)/2 pmPlus Cos[(m + 1)\[Phi]] +
			(1 - kd)/2 (n + m - 1)(n + m) pmMinus Cos[(m - 1)\[Phi]],
			(* y component *)
			-(1 + kd)/2 pmPlus Sin[(m + 1)\[Phi]] -
			(1 - kd)/2 (n + m - 1)(n + m) pmMinus Sin[(m - 1)\[Phi]],
			(* z component *)
			(n + m) pm Cos[m \[Phi]]}]


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
	Simplify @ N[(1 - I)/(2 Sqrt[2t]) q[m - 1/2, 0, I (t^2 - \[Chi]^2 - 1)/(2t)]]


\[Beta]["Ellipse"][{n_, m_}, t_, \[Chi]_] := \[Beta]["Ellipse"][{n, m}, t, \[Chi]] =
	Simplify @ N[
		-I^m D[
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


totalHarmonic[i\[Chi]_List, {n_Integer, m_Integer}, \[Chi]c_Symbol, t_Symbol:None, OptionsPattern[]] :=
	With[{compQ = TrueQ[OptionValue["Compile"]]},

		totalHarmonic[i\[Chi], {n, m}, \[Chi]c, t, "Compile" -> compQ] = Module[
			{term, expr, vars, fn, loopCount = Length[i\[Chi]]},

			(* The sum of harmonic contributions from however as many loop/saddle/ellipse-primitives as there are currents. *)
			(* Simplify to quicken root-finding later. *)
			term = Simplify[N[harmMag[{n, m}, \[Chi]c, t] / f[{n, m}]], Assumptions -> {\[Chi]c > 0, t > 0}];
			expr = Abs @ Sum[
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
		"ExpansionPointsPerContourDim" -> Automatic,
		"ContourMeshNN" -> Automatic,
		"ExpansionBleed" -> 1.5,
		"Seed" -> 1,
		"PrintSteps" -> False,
		"FinalSolutionChecks" -> False}];


realQ[x_] := TrueQ[Element[x, Reals]]


(* If the "ShowSteps" option is True, we want to be able to return echoed, labeled, iconized expressions. Otherwise discard the label. *)
echoFn[printQ_] := If[TrueQ[printQ], Function[label, Echo[#, label, Iconize[#, label]&]&], Identity &]


(* ::Subsection::Closed:: *)
(*Loops*)


Options[FindLoopCoil] = FilterRules[findCoilOpts, Except["MinSeparationAndExtentDifference"]];


FindLoopCoil::BadCurrents = finderMessages["BadCurrents"];
FindLoopCoil::BadDesired = finderMessages["BadDesired"];
FindLoopCoil::BadSeparations = finderMessages["BadSeparations"];
FindLoopCoil::BadLeadingError = finderMessages["BadLeadingError"];


FindLoopCoil[i\[Chi]_, nDes_, minMax\[Chi]c_, opts:OptionsPattern[]] :=
	Module[{proceed = True, nNull, nErr, autoHarms, optValNull, optValErr, allOpts},
		
		(* Check that arguments have been specified correctly, and issue messages if not. *)
		
		(* Currents must be a list of two or more reals. *)
		If[!MatchQ[i\[Chi], l:{__?realQ} /; Length[l] >= 2],
			Message[FindLoopCoil::BadCurrents, i\[Chi]]; proceed = False];
		
		(* The desired harmonic must be an integer greater than zero... *)
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
		
		If[!proceed, $Failed,
			(* Explicitly feed findSeparations all option->value pairs. This is incase the user has changed the default value of an option on FindLoopCoil,
				which needs to propagate through to findSeparations. *)
			allOpts = Sequence @@ Normal[Merge[{Options[FindLoopCoil], {opts}}, Last]];
			nNull = Replace[optValNull,
				Automatic :> (
					(* Calculate the nulled harmonics, and the leading error harmonic. *)
					autoHarms = harmonicsToNull["Loop"][Length[i\[Chi]] + 1, nDes];
					(* Only take the nulled harmonics for nNull. *)
					Drop[autoHarms, -1])];
			nErr = Replace[optValErr, Automatic :> Last[autoHarms]];
			findSeparations["Loop", i\[Chi], nDes, nNull, nErr, minMax\[Chi]c, {None, None}, allOpts]]]


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


Options[FindSaddleCoilAxial] = FilterRules[findCoilOpts, Except["MinSeparationAndExtentDifference"]];


Options[FindSaddleCoilAzimuthal] = Append[azimuthalOpts, "PrintSteps" -> False];


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


FindSaddleCoilAxial[i\[Chi]_, {nDes_, mDes_}, k\[Phi]_, minMax\[Chi]c_, opts:OptionsPattern[]] :=
	Module[{proceed = True, nmErr, autoHarms, nmNull, optValNull, optValErr, allOpts},
		
		(* Check that arguments have been specified correctly, and issue messages if not. *)
		
		(* Currents must be a list of two or more reals. *)
		If[!MatchQ[i\[Chi], l:{__?realQ} /; Length[l] >= 2],
			Message[FindSaddleCoilAxial::BadCurrents, i\[Chi]]; proceed = False];
		
		(* nDes and mDes must be integers that satisfy n >= m > 0... *)
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
		
		If[!proceed, $Failed,
			(* Ensure findSeparations inherits all option values from FindSaddleCoilAxial. *)
			allOpts = Sequence @@ Normal[Merge[{Options[FindSaddleCoilAxial], {opts}}, Last]];
			nmNull = Replace[optValNull,
				Automatic :> (
					(* Calculate the nulled harmonics, and the leading-order error harmonic. *)
					autoHarms = harmonicsToNull["Saddle"][Length[i\[Chi]] + 1, {nDes, mDes}, k\[Phi]];
					(* Only take the nulled harmonics for nmNull. *)
					Drop[autoHarms, -1])];
			nmErr = Replace[optValErr, Automatic :> Last[autoHarms]];
			findSeparations["Saddle", i\[Chi], {nDes, mDes}, nmNull, nmErr, minMax\[Chi]c, {None, None}, allOpts]]]


FindSaddleCoilAzimuthal[mDes_, k\[Phi]_, opts:OptionsPattern[]] :=
	Module[{proceed = True, allOpts},
		
		(* Check that arguments have been specified correctly, and issue messages if not. *)
		
		(* mDes must be an integer greater than zero. *)
		If[!MatchQ[mDes, m_Integer /; m > 0],
			Message[FindSaddleCoilAzimuthal::BadDesiredM, mDes]; proceed = False];
		
		(* Check that kPhi is an integer >= 1. *)
		If[!MatchQ[k\[Phi], int_Integer /; int >= 1],
			Message[FindSaddleCoilAzimuthal::BadNulledDegrees, k\[Phi]]; proceed = False];
		
		If[!proceed, $Failed,
			(* Ensure findAzimuthalExtents inherits all option values from FindSaddleCoilAzimuthal. *)
			allOpts = Sequence @@ Normal[Merge[{Options[FindSaddleCoilAzimuthal], {opts}}, Last]];
			findAzimuthalExtents[mDes, k\[Phi], allOpts]]]


FindSaddleCoil[i\[Chi]_, {nDes_, mDes_}, k\[Phi]_, minMax\[Chi]c_, opts:OptionsPattern[]] :=
	Module[{proceed = True, nNull, nmErr, autoHarms, nmNull, optValNull, optValErr, allSeparationOpts, allAzimuthalOpts, separations, azimuthalExtents},
		
		(* Check that arguments have been specified correctly, and issue messages if not. *)
		
		(* Currents must be a list of two or more reals. *)
		If[!MatchQ[i\[Chi], l:{__?realQ} /; Length[l] >= 2],
			Message[FindSaddleCoil::BadCurrents, i\[Chi]]; proceed = False];
		
		(* nDes and mDes must be integers that satisfy n >= m > 0... *)
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
		
		If[!proceed, $Failed,
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
					autoHarms = harmonicsToNull["Saddle"][Length[i\[Chi]] + 1, nDes];
					(* Only take the nulled harmonics for nNull. *)
					Drop[autoHarms, -1])];
			nmNull = {#, mDes}& /@ nNull;
			nmErr = Replace[optValErr, Automatic :> {Last[autoHarms], mDes}];
			separations = FindSaddleCoilAxial[i\[Chi], {nDes, mDes}, k\[Phi], minMax\[Chi]c, allSeparationOpts];
			azimuthalExtents = FindSaddleCoilAzimuthal[mDes, k\[Phi], allAzimuthalOpts];
			<|"AxialSeparations" -> separations, "AzimuthalExtents" -> azimuthalExtents|>]]


(* ::Subsection::Closed:: *)
(*Ellipses*)


Options[FindEllipseCoil] = Join[
	(* Use fewer mesh points for ellipses by default, given the doubled number of coil parameters. *)
	Replace[findCoilOpts, ("MeshPointsPer\[Chi]c" -> _) -> ("MeshPointsPer\[Chi]c" -> 6), 1]];


FindEllipseCoil::BadCurrents = finderMessages["BadCurrents"];
FindEllipseCoil::BadDesiredNM = finderMessages["BadDesiredNM"];
FindEllipseCoil::BadSeparations = finderMessages["BadSeparations"];
FindEllipseCoil::BadExtents = finderMessages["BadExtents"];
FindEllipseCoil::BadLeadingError = finderMessages["BadLeadingError"];


FindEllipseCoil[i\[Chi]_, {nDes_, mDes_}, minMax\[Chi]c_, minMaxT_, opts:OptionsPattern[]] :=
	Module[{proceed = True, nmNull, nmErr, autoHarms, optValNull, optValErr, allOpts},
		
		(* Check that arguments have been specified correctly, and issue messages if not. *)
		
		(* Currents must be a list of two or more reals. *)
		If[!MatchQ[i\[Chi], l:{__?realQ} /; Length[l] >= 2],
			Message[FindEllipseCoil::BadCurrents, i\[Chi]]; proceed = False];
		
		(* nDes and mDes must be integers that satisfy n >= m > 0... *)
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
		
		If[!proceed, $Failed,
			(* Explicitly feed findSeparations all option->value pairs. *)
			allOpts = Sequence @@ Normal[Merge[{Options[FindEllipseCoil], {opts}}, Last]];
			nmNull = Replace[optValNull,
				Automatic :> (
					(* Calculate the nulled harmonics, and the leading error harmonic. *)
					autoHarms = harmonicsToNull["Ellipse"][Length[i\[Chi]] + 1, {nDes, mDes}];
					(* Only take the nulled harmonics for nmNull. *)
					Drop[autoHarms, -1])];
			nmErr = Replace[optValErr, Automatic :> Last[autoHarms]];
			findSeparations["Ellipse", i\[Chi], {nDes, mDes}, nmNull, nmErr, minMax\[Chi]c, minMaxT, allOpts]]]


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
					OptionValue["ExpansionPointsPerContourDim"]];
			nearestPts = If[# === Automatic, (Length[i\[Chi]] - Length[nmNull]), #]&[
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
					totalHarmonic[i\[Chi], #, \[Chi]c, tBurn]&,
					Join[{nmDes}, nmNull, {nmErr}]]];
			totalHarmDes = echo["Total desired harmonic"][totalHarms[[1]]];
			totalHarmsNull = echo["Total nulled harmonics"][totalHarms[[2 ;; -2]]];
			totalHarmErr = echo["Total leading error harmonic"][totalHarms[[-1]]];

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
						With[
							{check = And @@ Join[
								{Less[\[Chi]cSlots], tanCheck},
								MovingMap[Apply[Abs @* Subtract, #] >= minSepDiff &, {\[Chi]cSlots}, 1]]},
							Function @ If[
								check,
								(* Quiet needed to suppress message noise from initial guesses that are far from the solution set. *)
								Quiet[With[
									{sol = FindRoot[exprs, varsAndLimits, findRootOpts]},
									If[TrueQ[Apply[check &, sol[[All, 2]]]], sol, Nothing]]],
								Nothing]]];
			
					(* Mesh the solution space. *)
					initialMesh = echo["Initial (coarse) mesh"][Flatten[
						Array[List,
							(* Number of points in each dimension *)
							Table[meshPoints, varCount],
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
						
						Switch[filteredInitialSols,
							(* Return {} if no legal solutions were found. *)
							{}, Throw[{}],
							(* If only one solution was found, expand a cloud of points around it. *)
							{_},
							contourMesh = Flatten[
								Array[First[filteredInitialSols] + List[##] &,
									Table[meshPoints, varCount],
									(* The point cloud should fit within the grain size of the coarse mesh. *)
									Catenate[{
										Table[{-1, 1}(max\[Chi]c - min\[Chi]c)/(loopCount - 1), loopCount],
										If[ellipseQ,
											Table[{-1, 1}(maxt - mint)/(loopCount - 1), loopCount],
											{}]}]],
								(* Flatten level *)
								varCount - 1],
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
							Parallelize @ Select[
								finalSolsRaw,
								Function[sol, AllTrue[
									totalHarmsNull /. MapThread[#1 -> #2 &, {Join[separations, tans], sol}],
									# < nullThresh &]]],
							finalSolsRaw];
						
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
										rules /. {\[Chi]c -> Coil\[Chi]c, t -> Coil\[Psi]},
										DesToErr -> (totalHarmDes/totalHarmErr /. rules)]] @@@ finalSols,
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
		(* Filter out solutions with extents less than minExtent. *)
		sols = Select[sols, AllTrue[# > minExtent &]];
		(* Filter out duplicates. *)
		sols = DeleteDuplicates[sols, SameQ @@ Round[{##}, duplicatesDist] &];
		If[sols === {}, Return[{}]];
		(* Sort coils by their ease of manufacture, i.e. how close they are to being evenly distributed through max\[Phi]. *)
		sols = Nearest[sols, lin * (Pi / (2 mDes)), All];
		(* Label each extent. *)
		sols = MapIndexed[Coil\[Phi][First[#2]] -> #1 &] /@ sols;
		(* Return only the requested number of solutions. *)
		partSpec = Replace[coilsReturned, Except[All] -> Span[1, UpTo[coilsReturned]]];
		sols[[partSpec]]]


(* ::Section::Closed:: *)
(*Coil Plots*)


reflectX[prim_, x_] := GeometricTransformation[prim, ReflectionTransform[{-1, 0}, {x, 0}]]
reflectX[x_] := GeometricTransformation[#, ReflectionTransform[{-1, 0}, {x, 0}]]&


reflectY[prim_, y_] := GeometricTransformation[prim, ReflectionTransform[{0, -1}, {0, y}]]
reflectY[y_] := GeometricTransformation[#, ReflectionTransform[{0, -1}, {0, y}]]&


arrowHead = Graphics[{Red, RegularPolygon[{1, 0}, 3]}];


(* Arrow heads get too large if they scale linearly with current. *)
scaleHead[s_] := .075 s^.5


(* Due to a bug in the Mathematica front end, where ImagePadding -> All does not account for arrowheads and
	thus arrowheads near the edges of plots can get clipped, we have to calculate the appropriate plot
	padding ourselves. *)
plotRangePaddingY[gWidth_, arrowheadS_, i\[Chi]_] :=
	(* Calculate the padding for the outermost primitive. Add a further 4% of padding. *)
	scaleHead[gWidth arrowheadS Last @ Abs[i\[Chi]]] + .04


coilPlotFrameLabel = {
	{
		TraditionalForm @ RawBoxes["\"\\!\\(\\*StyleBox[\\\"z\\\",FontSlant->\\\"Italic\\\"]\\) (m)\""],
		None},
	{
		TraditionalForm @ RawBoxes["\"\\!\\(\\*SubscriptBox[\\\"\[Rho]\\\", StyleBox[\\\"c\\\",FontSlant->\\\"Italic\\\"]]\\)\[ThinSpace]\[Phi] (m)\""],
		None}};


schematicOpts = Normal @ Merge[
	{
		Options[Graphics],
		{
			ImageSize -> Large,
			PlotRange -> All,
			Frame -> True,
			FrameLabel -> coilPlotFrameLabel,
			"ThicknessScaling" -> .0015,
			"ArrowheadScaling" -> .006
		}
	},
	Last];


(* ::Subsection::Closed:: *)
(*Dynamic Plot Elements*)


dynLoop[prim_, \[Chi]c\[Rho]c_, i\[Chi]_, Dynamic[tracker_]] := dynPrim[prim,
	{
		{
			"\"\\!\\(\\*SubscriptBox[\\\"\[Chi]\\\", StyleBox[\\\"c\\\",FontSlant->\\\"Italic\\\"]]\\)\[ThinSpace]\\!\\(\\*SubscriptBox[\\\"\[Rho]\\\", StyleBox[\\\"c\\\",FontSlant->\\\"Italic\\\"]]\\)\"",
			Row[{\[Chi]c\[Rho]c, " (m)"}]},
		{
			"\"\\!\\(\\*SubscriptBox[StyleBox[\\\"i\\\",FontSlant->\\\"Italic\\\"], \\\"\[Chi]\\\"]\\)\"",
			Row[{i\[Chi], " (A)"}]}
	},
	{\[Chi]c\[Rho]c, i\[Chi]},
	Dynamic[tracker]]


dynSaddle[prim_, \[Chi]c\[Rho]c:{_, _}, i\[Chi]_, extent_, Dynamic[tracker_]] := dynPrim[prim,
	{
		{
			"\"\\!\\(\\*SubscriptBox[\\\"\[Chi]\\\", StyleBox[\\\"c\\\",FontSlant->\\\"Italic\\\"]]\\)\[ThinSpace]\\!\\(\\*SubscriptBox[\\\"\[Rho]\\\", StyleBox[\\\"c\\\",FontSlant->\\\"Italic\\\"]]\\)\"",
			Row[{\[Chi]c\[Rho]c[[1]], ", ", \[Chi]c\[Rho]c[[2]], " (m)"}]},
		{
			"\"\[Phi]\[ThinSpace]\\!\\(\\*SubscriptBox[\\\"\[Rho]\\\", StyleBox[\\\"c\\\",FontSlant->\\\"Italic\\\"]]\\)\"",
			Row[{Abs[Subtract @@ extent]/2, " (m)"}]},
		{
			"\"\\!\\(\\*SubscriptBox[StyleBox[\\\"i\\\",FontSlant->\\\"Italic\\\"], \\\"\[Chi]\\\"]\\)\"",
			Row[{i\[Chi], ", ", -i\[Chi], " (A)"}]}
	},
	{\[Chi]c\[Rho]c, i\[Chi], extent},
	Dynamic[tracker]]


dynSaddle[prim_, \[Chi]c\[Rho]c_?NumberQ, i\[Chi]_, extent_, Dynamic[tracker_]] := dynPrim[prim,
	{
		{
			"\"\\!\\(\\*SubscriptBox[\\\"\[Chi]\\\", StyleBox[\\\"c\\\",FontSlant->\\\"Italic\\\"]]\\)\[ThinSpace]\\!\\(\\*SubscriptBox[\\\"\[Rho]\\\", StyleBox[\\\"c\\\",FontSlant->\\\"Italic\\\"]]\\)\"",
			Row[{\[Chi]c\[Rho]c, " (m)"}]},
		{
			"\"\[Phi]\[ThinSpace]\\!\\(\\*SubscriptBox[\\\"\[Rho]\\\", StyleBox[\\\"c\\\",FontSlant->\\\"Italic\\\"]]\\)\"",
			Row[{Abs[Subtract @@ extent]/2, " (m)"}]},
		{
			"\"\\!\\(\\*SubscriptBox[StyleBox[\\\"i\\\",FontSlant->\\\"Italic\\\"], \\\"\[Chi]\\\"]\\)\"",
			Row[{i\[Chi], " (A)"}]}
	},
	{\[Chi]c\[Rho]c, i\[Chi], extent},
	Dynamic[tracker]]


dynEllipse[prim_, \[Chi]c\[Rho]c_, \[Psi]c\[Rho]c_, i\[Chi]_, phase_, Dynamic[tracker_]] := dynPrim[prim,
	{
		{
			"\"\\!\\(\\*SubscriptBox[\\\"\[Chi]\\\", StyleBox[\\\"c\\\",FontSlant->\\\"Italic\\\"]]\\)\[ThinSpace]\\!\\(\\*SubscriptBox[\\\"\[Rho]\\\", StyleBox[\\\"c\\\",FontSlant->\\\"Italic\\\"]]\\)\"",
			Row[{\[Chi]c\[Rho]c, " (m)"}]},
		{
			"\"\[Psi]\[ThinSpace]\\!\\(\\*SubscriptBox[\\\"\[Rho]\\\", StyleBox[\\\"c\\\",FontSlant->\\\"Italic\\\"]]\\)\"",
			Row[{\[Psi]c\[Rho]c, " (m)"}]},
		{
			"\"\\!\\(\\*SubscriptBox[StyleBox[\\\"i\\\",FontSlant->\\\"Italic\\\"], \\\"\[Chi]\\\"]\\)\"",
			Row[{i\[Chi], " (A)"}]}
	},
	{\[Chi]c\[Rho]c, \[Psi]c\[Rho]c, i\[Chi], phase},
	Dynamic[tracker]]


dynPrim[prim_, label:{{_, _}..}, key_, Dynamic[tracker_]] := Tooltip[
	EventHandler[
		{Dynamic[If[tracker === key, Red, Black]], prim},
		{"MouseEntered" :> (tracker = key), "MouseExited" :> (tracker = None)}],
	Pane[
		TraditionalForm @ Grid[{RawBoxes[#1], " = ", #2}& @@@ label, Alignment -> Left],
		FrameMargins -> {5{1, 1}, 2{1, 1}}]]


(* ::Subsection::Closed:: *)
(*Loops*)


Options[LoopCoilPlot] = schematicOpts;


LoopCoilPlot::BadSeparations = plotMessages["BadSeparations"];
LoopCoilPlot::BadCurrents = plotMessages["BadCurrents"];
LoopCoilPlot::BadDesired = plotMessages["BadDesired"];
LoopCoilPlot::BadRadius = plotMessages["BadRadius"];


LoopCoilPlot[\[Chi]c_, i\[Chi]_, \[Rho]c_, nDes_, opts:OptionsPattern[]] :=
	Module[{proceed = True, \[Chi]cVals, thicknessS, arrowheadS, allOpts},
		
		(* Check that arguments have been specified correctly, and issue messages if not. *)
		
		(* Separations must either be a list of two or more positive reals in ascending order, or a list of
			Coil\[Chi]c[...] -> ... rules (can contain a DesToErr -> ... rule, which will be ignored). *)
		If[
			!MatchQ[\[Chi]c, Alternatives[

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
			Message[LoopCoilPlot::BadSeparations, \[Chi]c]; proceed = False];
		
		(* Currents must be a list of reals, equal in length to the number of separations. *)
		If[!MatchQ[i\[Chi], l:{__?realQ} /; Length[l] === Length[DeleteCases[\[Chi]c, DesToErr -> _]]],
			Message[LoopCoilPlot::BadCurrents, i\[Chi]]; proceed = False];
		
		(* The desired harmonic must be an integer greater than zero. *)
		If[!MatchQ[nDes, n_Integer /; n > 0],
			Message[LoopCoilPlot::BadDesired, nDes]; proceed = False];
		
		(* Radius must be positive. *)
		If[!Positive[\[Rho]c],
			Message[LoopCoilPlot::BadRadius, \[Rho]c]; proceed = False];
		
		If[!proceed, $Failed,
			(* Explicitly feed loopSchematic all option->value pairs. This is incase the user has changed the default value of an option on LoopCoilPlot,
				which needs to propagate through to loopSchematic. *)
			allOpts = Sequence @@ Normal[Merge[{Options[LoopCoilPlot], {opts}}, Last]];
			(* If \[Chi]c is a list of Coil\[Chi]c[index] -> val rules, then sort by index and take the vals. *)
			\[Chi]cVals = Replace[\[Chi]c, l:{__Rule} :> SortBy[
				Cases[l, (Coil\[Chi]c[i_] -> val_) :> {i, val}],
				First][[All, 2]]];
			thicknessS = OptionValue["ThicknessScaling"];
			arrowheadS = OptionValue["ArrowheadScaling"];
			loopSchematic[\[Chi]cVals, i\[Chi], \[Rho]c, nDes, thicknessS, arrowheadS, allOpts]]]


loopSchematic[\[Chi]c_, i\[Chi]_, \[Rho]c_, nDes_, thicknessS_, arrowheadS_, opts___] := Module[
	{gPrims, symTransform},

	DynamicModule[{tracker},
		(* Construct the primitives with +ve z coords. The thickness of each primitive is proportional to i\[Chi]. *)
		gPrims = MapThread[

			Function[{\[Chi]cp, i\[Chi]p, flip},
				flip @ {
					(* Thickness *)
					Thickness[thicknessS Abs[i\[Chi]p]],
					(* Arrowheads (also scaled by i\[Chi]) *)
					Arrowheads[Table[{scaleHead[arrowheadS Abs[i\[Chi]p]], pos, arrowHead}, {pos, .25, .75, .25}]],
					(* Arrow *)
					dynLoop[
						Arrow[\[Rho]c {{0, \[Chi]cp}, {2 Pi, \[Chi]cp}}],
						\[Rho]c \[Chi]cp, i\[Chi]p,
						Dynamic[tracker]]}],

			{
				\[Chi]c,
				i\[Chi],
				i\[Chi] /. {_?Negative -> reflectX[Pi \[Rho]c], _?Positive -> Identity}
			}];
		
		(* Reverse the direction of the -ve z primitives' currents if the coil is axially antisymmetric. *)
		symTransform = If[EvenQ[nDes], reflectX[Pi \[Rho]c], Identity];

		(* Now add the primitives with -ve z coords, accounting for the symmetry/antisymmetry of the coil. *)
		gPrims = Join[reflectY[0] @* symTransform /@ gPrims, gPrims];
		
		Graphics[
			{gPrims},
			PlotRange -> {{0, 2 Pi \[Rho]c}, All},
			PlotRangeClipping -> True,
			PlotRangePadding -> {0, plotRangePaddingY[2 Pi \[Rho]c, arrowheadS, i\[Chi]]},
			Sequence @@ FilterRules[{opts}, Options[Graphics]]],
		
		UnsavedVariables :> {tracker}]

	(* integrand = Total @ MapThread[
		Function @@ {#2 dl \[Cross] rp[{x, y, z}] / Norm[rp[{x, y, z}]]^3},
		{Join[-\[Chi]c, \[Chi]c], Join[sym i\[Chi], i\[Chi]]}
	] *)
]


(* ::Subsection::Closed:: *)
(*Saddles*)


Options[SaddleCoilPlot] = schematicOpts;


SaddleCoilPlot::BadSeparations = plotMessages["BadSeparations"];
SaddleCoilPlot::BadExtents = plotMessages["BadExtents"];
SaddleCoilPlot::BadCurrents = plotMessages["BadCurrents"];
SaddleCoilPlot::BadCurrentRatios = plotMessages["BadCurrentRatios"];
SaddleCoilPlot::BadDesiredNM = plotMessages["BadDesiredNM"];
SaddleCoilPlot::BadRadius = plotMessages["BadRadius"];


SaddleCoilPlot[\[Chi]c_, \[Phi]c_, i\[Chi]_, \[Rho]c_, {nDes_, mDes_}, opts:OptionsPattern[]] :=
	Module[{proceed = True, \[Chi]cVals, \[Phi]cVals, thicknessS, arrowheadS, allOpts},
		
		(* Check that arguments have been specified correctly, and issue messages if not. *)
		
		(* Separations must either be a list of two or more positive reals in ascending order, or a list of
			Coil\[Chi]c[...] -> ... rules (can contain a DesToErr -> ... rule, which will be ignored). *)
		If[
			!MatchQ[\[Chi]c, Alternatives[

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
			Message[SaddleCoilPlot::BadSeparations, \[Chi]c]; proceed = False];
		
		(* Extents must either be a list of one or more positive reals in ascending order, or a list of
			Coil\[Phi][...] -> ... rules. *)
		If[
			!MatchQ[\[Phi]c, Alternatives[

				{_?Positive},

				l:{__?Positive} /; AllTrue[Differences[l], Positive],

				l:{(Coil\[Phi][_] -> _?Positive)..} /; With[
					{indicesAndVals = Replace[l, (Coil\[Phi][i_] -> val_) :> {i, val}, 1]},
					TrueQ @ And[
						(* Coil\[Phi] indices must be consecutive ascending integers, starting from 1. *)
						Sort[indicesAndVals[[All, 1]]] == Range[Length[indicesAndVals]],
						(* Coil\[Chi]c values, as sorted by index, must be ascending positive numbers. *)
						AllTrue[Differences[SortBy[indicesAndVals, First][[All, 2]]], Positive]
				]]
			]],
			Message[SaddleCoilPlot::BadExtents, \[Phi]c]; proceed = False];
		
		(* Currents must be a list of reals, equal in length to the number of separations. *)
		If[!MatchQ[i\[Chi], l:{__?realQ} /; Length[l] === Length[DeleteCases[\[Chi]c, DesToErr -> _]]],
			Message[SaddleCoilPlot::BadCurrents, i\[Chi]]; proceed = False];
		
		(* nDes and mDes must be integers that satisfy n >= m > 0... *)
		If[!MatchQ[{nDes, mDes}, {n_Integer, m_Integer} /; n >= m > 0],
			Message[SaddleCoilPlot::BadDesiredNM, nDes, mDes]; proceed = False];
		
		(* If nDes + mDes is odd, then pairs of successive currents must be equal in magnitude and opposite in parity. *)
		If[OddQ[nDes + mDes] && (OddQ[Length[i\[Chi]]] || !AllTrue[Partition[i\[Chi], 2], Total[#] == 0 &]),
			Message[SaddleCoilPlot::BadCurrentRatios, nDes, mDes]; proceed = False];
		
		(* Radius must be positive. *)
		If[!Positive[\[Rho]c],
			Message[SaddleCoilPlot::BadRadius, \[Rho]c]; proceed = False];
		
		If[!proceed, $Failed,
			(* Explicitly feed saddleSchematic all option->value pairs. This is incase the user has changed the default value of an option on SaddleCoilPlot,
				which needs to propagate through to saddleSchematic. *)
			allOpts = Sequence @@ Normal[Merge[{Options[SaddleCoilPlot], {opts}}, Last]];
			(* If \[Chi]c is a list of Coil\[Chi]c[index] -> val rules, then sort by index and take the vals. *)
			\[Chi]cVals = Replace[\[Chi]c, l:{__Rule} :> SortBy[
				Cases[l, (Coil\[Chi]c[i_] -> val_) :> {i, val}],
				First][[All, 2]]];
			(* If \[Phi]c is a list of Coil\[Phi][index] -> val rules, then sort by index and take the vals. *)
			\[Phi]cVals = Replace[\[Phi]c, l:{__Rule} :> SortBy[
				Replace[l, (Coil\[Phi][i_] -> val_) :> {i, val}, 1],
				First][[All, 2]]];
			thicknessS = OptionValue["ThicknessScaling"];
			arrowheadS = OptionValue["ArrowheadScaling"];
			saddleSchematic[\[Chi]cVals, \[Phi]cVals, i\[Chi], \[Rho]c, {nDes, mDes}, thicknessS, arrowheadS, allOpts]]]


saddleSchematic[\[Chi]c_, \[Phi]c_, i\[Chi]_, \[Rho]c_, {nDes_, mDes_}, thicknessS_, arrowheadS_, opts___] := Module[
	{gPrims, symQ, arcCentres, arcExtents, \[Chi]cPairs},

	(* Is the coil axially symmetric? *)
	symQ = OddQ[nDes + mDes];
	
	(* Construct the primitives' coordinates. *)
	arcCentres = Array[Identity, 2 mDes + 1, {0, 2 Pi}];
	arcExtents = \[Rho]c Riffle[
		Table[{-#, #} + centre & /@ \[Phi]c, {centre, arcCentres[[;; ;; 2]]}],
		Table[{#, -#} + centre & /@ \[Phi]c, {centre, arcCentres[[2 ;; ;; 2]]}]];
	arcExtents = Flatten[arcExtents, 1];

	DynamicModule[{tracker},

		(* Connect the arcs into saddles. *)
		If[symQ,

			(* If the coil is axially symmetric, then subsequent pairs of arcs are joined together. *)
			\[Chi]cPairs = Partition[\[Chi]c, 2];
			gPrims = Reverse @ MapThread[
				Function[{zPair, i\[Chi]p, flip},
					Reverse @ Table[
						flip @ {
							(* The thickness and head size of each arrow is proportional to i\[Chi] *)
							Thickness[thicknessS Abs[i\[Chi]p]],
							Arrowheads[{{scaleHead[arrowheadS Abs[i\[Chi]p]], .5, arrowHead}}],
							(* Collection of arrows (one for each line segment). *)
							dynSaddle[
								Arrow[{
									(* All four segments of the saddle. *)
									{{extent[[1]], zPair[[1]]}, {extent[[1]], zPair[[2]]}},
									{{extent[[1]], zPair[[2]]}, {extent[[2]], zPair[[2]]}},
									{{extent[[2]], zPair[[2]]}, {extent[[2]], zPair[[1]]}},
									{{extent[[2]], zPair[[1]]}, {extent[[1]], zPair[[1]]}}
								}],
								zPair, i\[Chi]p, extent,
								Dynamic[tracker]]
						},
						{extent, arcExtents}]],
				{
					\[Rho]c \[Chi]cPairs,
					i\[Chi][[;; ;; 2]],
					i\[Chi][[;; ;; 2]] /. {_?Negative -> reflectX[Pi \[Rho]c], _?Positive -> Identity}
				}];
			(* Now add the primitives with -ve z coords. *)
			gPrims = Join[reflectY[0] /@ gPrims, gPrims],
			
			(* If the coil is axially antisymmetric, then each arc with +ve z is joined to its corresponding arc with -ve z. *)
			gPrims = Reverse @ MapThread[
				Function[{z, i\[Chi]p, flip},
					flip @ Reverse @ Table[
						{
							(* The thickness and head size of each arrow is proportional to i\[Chi] *)
							Thickness[thicknessS Abs[i\[Chi]p]],
							Arrowheads[{{scaleHead[arrowheadS Abs[i\[Chi]p]], .5, arrowHead}}],
							(* Collection of arrows (one for each line segment). *)
							dynSaddle[
								Arrow[{
									(* All four segments of the saddle. *)
									{{extent[[1]], -z}, {extent[[1]], z}},
									{{extent[[1]], z}, {extent[[2]], z}},
									{{extent[[2]], z}, {extent[[2]], -z}},
									{{extent[[2]], -z}, {extent[[1]], -z}}
								}],
								z, i\[Chi]p, extent,
								Dynamic[tracker]]
						},
						{extent, arcExtents}]],
				{
					\[Rho]c \[Chi]c,
					i\[Chi],
					i\[Chi] /. {_?Negative -> reflectX[Pi \[Rho]c], _?Positive -> Identity}
				}]
		];

		Graphics[
			{gPrims},
			PlotRange -> {{0, 2 Pi \[Rho]c}, All},
			PlotRangeClipping -> True,
			PlotRangePadding -> {0, plotRangePaddingY[2 Pi \[Rho]c, arrowheadS, i\[Chi]]},
			Sequence @@ FilterRules[{opts}, Options[Graphics]]],
		
		UnsavedVariables :> {tracker}]]


(* ::Subsection::Closed:: *)
(*Ellipses*)


Options[EllipseCoilPlot] = schematicOpts;


EllipseCoilPlot::BadChiPsi = plotMessages["BadChiPsi"];
EllipseCoilPlot::BadCurrents = plotMessages["BadCurrents"];
EllipseCoilPlot::BadDesiredNM = plotMessages["BadDesiredNM"];
EllipseCoilPlot::BadRadius = plotMessages["BadRadius"];


EllipseCoilPlot[\[Chi]c\[Psi]c_, i\[Chi]_, \[Rho]c_, {nDes_, mDes_}, opts:OptionsPattern[]] :=
	Module[{proceed = True, \[Chi]cVals, \[Psi]cVals, thicknessS, arrowheadS, allOpts},
		
		(* Check that arguments have been specified correctly, and issue messages if not. *)
		
		(* Separations and extents must either be a list of two or more paired positive reals, 
			{{\[Chi]c1, \[Psi]1}, {\[Chi]c2, \[Psi]2}, \[Ellipsis]}, 
			or a flat list of Coil\[Chi]c[i] -> \[Chi]ci and Coil\[Psi][i] -> \[Psi]i rules,
			{Coil\[Chi]c[1] -> \[Chi]c1, Coil\[Psi][1] -> \[Psi]1, Coil\[Chi]c[2] -> \[Chi]c2, Coil\[Psi][2] -> \[Psi]2, \[Ellipsis]},
			where there are as many extents as separations. In both cases, \[Chi]c1 < \[Chi]c2 < \[Ellipsis]. *)
		If[
			!MatchQ[\[Chi]c\[Psi]c, Alternatives[

				l:{{_?Positive, _?Positive}..} /; (Length[l] >= 2 && AllTrue[Differences[l[[All, 1]]], Positive]),

				l:{(Coil\[Chi]c[_] | Coil\[Psi][_] | DesToErr -> _?Positive)..} /; Module[
					{\[Chi]cIndicesAndVals, \[Psi]cIndicesAndVals},
					{\[Chi]cIndicesAndVals, \[Psi]cIndicesAndVals} = Map[
						SortBy[Cases[l, (#[i_] -> val_) :> {i, val}], First]&,
						{Coil\[Chi]c, Coil\[Psi]}];
					TrueQ @ And[
						(* Same number of separations and extents. *)
						Length[\[Chi]cIndicesAndVals] === Length[\[Psi]cIndicesAndVals],
						(* Coil\[Chi]c and Coil\[Psi] indices must be consecutive ascending integers, starting from 1. *)
						\[Chi]cIndicesAndVals[[All, 1]] == Range[Length[\[Chi]cIndicesAndVals]],
						\[Psi]cIndicesAndVals[[All, 1]] == Range[Length[\[Psi]cIndicesAndVals]],
						(* Coil\[Chi]c values, as sorted by index, must be ascending positive numbers. *)
						AllTrue[Differences[\[Chi]cIndicesAndVals[[All, 2]]], Positive]
				]]
			]],
			Message[EllipseCoilPlot::BadChiPsi, \[Chi]c\[Psi]c]; proceed = False];
		
		(* Currents must be a list of reals, equal in length to the number of separations. *)
		If[
			!MatchQ[i\[Chi], l:{__?realQ} /; Length[l] === Length[
				Cases[\[Chi]c\[Psi]c, (Coil\[Chi]c[_] -> _) | {_, _}]]],
			Message[EllipseCoilPlot::BadCurrents, i\[Chi]]; proceed = False];
		
		(* nDes and mDes must be integers that satisfy n >= m > 0... *)
		If[!MatchQ[{nDes, mDes}, {n_Integer, m_Integer} /; n >= m > 0],
			Message[EllipseCoilPlot::BadDesiredNM, nDes, mDes]; proceed = False];
		
		(* Radius must be positive. *)
		If[!Positive[\[Rho]c],
			Message[EllipseCoilPlot::BadRadius, \[Rho]c]; proceed = False];
		
		If[!proceed, $Failed,
			(* Explicitly feed ellipseSchematic all option->value pairs. This is incase the user has changed the default value of an option on EllipseCoilPlot,
				which needs to propagate through to ellipseSchematic. *)
			allOpts = Sequence @@ Normal[Merge[{Options[EllipseCoilPlot], {opts}}, Last]];
			{\[Chi]cVals, \[Psi]cVals} = Switch[
				\[Chi]c\[Psi]c,
				(* If \[Chi]c\[Psi]c is a list of {\[Chi]c, \[Psi]c} pairs, then transpose and assign. *)
				{{_, _}..},
				Transpose[\[Chi]c\[Psi]c],
				(* If \[Chi]c\[Psi]c is a list of Coil\[Chi]c[index] -> val and Coil\[Psi][index] -> val rules, then sort each by index and take the vals. *)
				{__Rule},
				SortBy[Cases[\[Chi]c\[Psi]c, (#[i_] -> val_) :> {i, val}], First][[All, 2]]& /@ {Coil\[Chi]c, Coil\[Psi]}];
			thicknessS = OptionValue["ThicknessScaling"];
			arrowheadS = OptionValue["ArrowheadScaling"];
			ellipseSchematic[\[Chi]cVals, \[Psi]cVals, i\[Chi], \[Rho]c, {nDes, mDes}, thicknessS, arrowheadS, allOpts]]]


ellipseSchematic[\[Chi]c_, \[Psi]c_, i\[Chi]_, \[Rho]c_, {nDes_, mDes_}, thicknessS_, arrowheadS_, opts___] := Module[
	{gPrims, phase, symTransform},
	
	DynamicModule[{tracker},

		(* Construct the primitives' coordinates. *)
		phase = Array[Identity, 2 mDes + 1, {0, 2 Pi}][[;; -2]];
		gPrims = MapThread[
			Function[{z, t, i\[Chi]p, flip},
				flip @ {
					(* The thickness and head size of each arrow is proportional to i\[Chi] *)
					Thickness[thicknessS Abs[i\[Chi]p]],
					Arrowheads @ Table[
						{scaleHead[arrowheadS Abs[i\[Chi]p]], Mod[pos - #/(2 Pi), 1.0001], arrowHead},
						{pos, 0, 1, .5}],
					(* Arrows *)
					dynEllipse[
						Arrow @ FirstCase[
							Plot[t Cos[\[Phi] / \[Rho]c + #] + z, {\[Phi], 0, 2 Pi \[Rho]c}],
							Line[pts_] :> pts,
							{}, Infinity],
						z, t, i\[Chi]p, #,
						Dynamic[tracker]]
				}& /@ phase],
			{
				\[Rho]c \[Chi]c,
				\[Rho]c \[Psi]c,
				i\[Chi],
				i\[Chi] /. {_?Negative -> reflectX[Pi \[Rho]c], _?Positive -> Identity}
			}];
		(* Reshape into {{dirs, Arrow[...]}, {dirs, Arrow[...]}, ...} *)
		gPrims = Flatten[gPrims, 1];

		(* Alternating periodicity so that there are mDes lines of symmetry. *)
		gPrims = Join[
			gPrims[[;; ;; 2]],
			reflectX[Pi \[Rho]c] /@ gPrims[[2 ;; ;; 2]]];
		
		(* Reverse the direction of the -ve z primitives' currents if the coil is axially antisymmetric. *)
		symTransform = If[EvenQ[nDes + mDes], reflectX[Pi \[Rho]c], Identity];

		(* Now add the primitives with -ve z coords, accounting for the symmetry/antisymmetry of the coil. *)
		gPrims = Join[reflectY[0] @* symTransform /@ gPrims, gPrims];

		Graphics[
			{gPrims},
			PlotRange -> {{0, 2 Pi \[Rho]c}, All},
			PlotRangeClipping -> True,
			PlotRangePadding -> {0, plotRangePaddingY[2 Pi \[Rho]c, arrowheadS, i\[Chi]]},
			Sequence @@ FilterRules[{opts}, Options[Graphics]]],
		
		UnsavedVariables :> {tracker}]]


(* loopPlot3D


loopPrimitive[\[Chi]c_, \[Rho]c_] := \[Rho]c {Cos[2 \[Pi] \[FormalU]], Sin[2 \[Pi] \[FormalU]], \[Chi]c}


SaddleCoilPlot[\[Chi]c_, \[Phi]c_, i\[Chi]_, \[Rho]c_, {nDes_, mDes_}] :=
	None


EllipseCoilPlot[\[Chi]c_, i\[Chi]_, \[Rho]c_, {nDes_, mDes_}] :=
	None *)


(* ::Section::Closed:: *)
(*Field Plots*)


(* ::Section::Closed:: *)
(*Package Footer*)


End[];
EndPackage[];
