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


LoopCoilPlot3D::usage = "stub";


SaddleCoilPlot::usage = "stub";


SaddleCoilPlot3D::usage = "stub";


EllipseCoilPlot::usage = "stub";


EllipseCoilPlot3D::usage = "stub";


LoopCoilFieldPlot::usage = "stub";


SaddleCoilFieldPlot::usage = "stub";


EllipseCoilFieldPlot::usage = "stub";


Begin["`Private`"];


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
		mNull = Complement[2 Range[k\[Phi] + loopCount] - If[EvenQ[mDes], 0, 1], mNullAz];
		(* Orders to null *)
		nNull = harmonicsToNull["Loop"][loopCount, nDes];
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
		"MeshPointsPer\[Chi]c" -> 10,
		"DuplicatesProximity" -> Scaled[.03],
		"NullingThreshold" -> 10.^-5,
		"MinSeparationsDifference" -> Scaled[.01],
		"MinSeparationAndExtentDifference" -> Scaled[.01],
		"ExpansionPointsPerContourDim" -> Automatic,
		"ContourMeshNN" -> Automatic,
		"ExpansionBleed" -> 1.5,
		"Seed" -> 1,
		"PrintSteps" -> False}];


realQ[x_] := TrueQ[Element[x, Reals]]


(* If the "ShowSteps" option is True, we want to be able to return echoed, labeled, iconized expressions. Otherwise discard the label. *)
echoFn[printQ_] := If[TrueQ[printQ], Function[label, Echo[#, label, Iconize[#, label]&]&], Identity &]


(* ::Subsection::Closed:: *)
(*Loops*)


Options[FindLoopCoil] = FilterRules[findCoilOpts, Except["MinSeparationAndExtentDifference"]];


FindLoopCoil::BadCurrents = "Currents: `1` should be a list of two or more real numbers.";
FindLoopCoil::BadDesired = "Desired harmonic: `1` should be an integer greater than zero.";
FindLoopCoil::BadSeparations = "Search range: `1` should be of the form {min\[Chi]c, max\[Chi]c}, where 0 < min\[Chi]c < max\[Chi]c.";
FindLoopCoil::BadLeadingError = "If \"NulledHarmonics\" is not Automatic, then \"LeadingErrorHarmonic\" must be given explicitly.";


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


saddleMessages = <|
	"BadCurrents" -> "Currents: `1` should be a list of two or more real numbers.",
	"BadDesiredNM" -> "Desired harmonic order N and degree M: `1` (N) and `2` (M) should be integers, where N \[GreaterEqual] M > 0.",
	"BadDesiredM" -> "Desired harmonic order M (`1`) should be an integer greater than zero.",
	"BadSeparations" -> "Search range: `1` should be of the form {min\[Chi]c, max\[Chi]c}, where 0 < min\[Chi]c < max\[Chi]c.",
	"BadLeadingError" -> "If \"NulledHarmonics\" is not Automatic, then \"LeadingErrorHarmonic\" must be given explicitly.",
	"BadNulledDegrees" -> "The number of degrees to null, k\[Phi] = `1`, should be an integer greater than zero."
|>;


FindSaddleCoil::BadCurrents = saddleMessages["BadCurrents"];
FindSaddleCoil::BadDesiredNM = saddleMessages["BadDesiredNM"];
FindSaddleCoil::BadSeparations = saddleMessages["BadSeparations"];
FindSaddleCoil::BadLeadingError = saddleMessages["BadLeadingError"];
FindSaddleCoil::BadNulledDegrees = saddleMessages["BadNulledDegrees"];


FindSaddleCoilAxial::BadCurrents = saddleMessages["BadCurrents"];
FindSaddleCoilAxial::BadDesiredNM = saddleMessages["BadDesiredNM"];
FindSaddleCoilAxial::BadSeparations = saddleMessages["BadSeparations"];
FindSaddleCoilAxial::BadLeadingError = saddleMessages["BadLeadingError"];
FindSaddleCoilAxial::BadNulledDegrees = saddleMessages["BadNulledDegrees"];


FindSaddleCoilAzimuthal::BadDesiredM = saddleMessages["BadDesiredM"];
FindSaddleCoilAzimuthal::BadNulledDegrees = saddleMessages["BadNulledDegrees"];


FindSaddleCoilAxial[i\[Chi]_, {nDes_, mDes_}, k\[Phi]_, minMax\[Chi]c_, opts:OptionsPattern[]] :=
	Module[{proceed = True, nmErr, autoHarms, nmNull, optValNull, optValErr, allOpts},
		
		(* Check that arguments have been specified correctly, and issue messages if not. *)
		
		(* Currents must be a list of two or more reals. *)
		If[!MatchQ[i\[Chi], l:{__?realQ} /; Length[l] >= 2],
			Message[FindSaddleCoilAxial::BadCurrents, i\[Chi]]; proceed = False];
		
		(* nDes and mDes must be integers that satisfy n >= m > 0... *)
		If[!MatchQ[{nDes, mDes}, {n_Integer, m_Integer} /; n >= m > 0],
			Message[FindSaddleCoilAxial::BadDesiredNM, nDes, mDes]; proceed = False];
		
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
	Replace[findCoilOpts, ("MeshPointsPer\[Chi]c" -> _) -> ("MeshPointsPer\[Chi]c" -> 5), 1]];


FindEllipseCoil::BadCurrents = "Currents: `1` should be a list of two or more real numbers.";
FindEllipseCoil::BadDesiredNM = "Desired harmonic order N and degree M: `1` (N) and `2` (M) should be integers, where N \[GreaterEqual] M > 0.";
FindEllipseCoil::BadSeparations = "Search range: `1` should be of the form {min\[Chi]c, max\[Chi]c}, where 0 < min\[Chi]c < max\[Chi]c.";
FindEllipseCoil::BadExtents = "Search range: `1` should be of the form {min\[Psi], max\[Psi]}, where 0 < min\[Psi] < max\[Psi].";
FindEllipseCoil::BadLeadingError = "If \"NulledHarmonics\" is not Automatic, then \"LeadingErrorHarmonic\" must be given explicitly.";


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
				loopCount, nullCount, varCount, separations, tans, meshPoints,
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
			expPoints = If[# === Automatic, Ceiling[OptionValue["MeshPointsPer\[Chi]c"]/3], #]&[
				OptionValue["ExpansionPointsPerContourDim"]];
			nearestPts = If[# === Automatic, (Length[i\[Chi]] - Length[nmNull]), #]&[
				OptionValue["ContourMeshNN"]];
			bleed = OptionValue["ExpansionBleed"];
			seed = OptionValue["Seed"];
			coilsReturned = OptionValue["CoilsReturned"];
			valsOnlyQ = OptionValue["ValuesOnly"];


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
						And @@ (#1 - #2 > minSepTanDiff &) @@@ Transpose[{{\[Chi]cSlots}, {tSlots}}],
						True]},

					(* Construct the FindRoot function. *)
					findRoot = echo["FindRoot function"][
						Function[
							(* Only operate on points which satisfy \[Chi]c[1] < \[Chi]c[2] < \[Chi]c[3] < ... *)
							If[
								Less[\[Chi]cSlots] && tanCheck,
								(* Quiet needed to suppress message noise from initial guesses that are far from the solution set. *)
								Quiet[FindRoot[exprs, varsAndLimits, findRootOpts]],
								Nothing]]];
			
					(* Mesh the solution space. *)
					initialMesh = Flatten[
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
						varCount - 1];
					
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
										True]]&]];
						
						(* Apply findRoot to the new mesh. *)
						finalSolsRaw = echo["Final solutions (unfiltered)"][
							ParallelMap[Apply[findRoot], contourMesh][[All, All, -1]]];
						
						(* Remove illegal solutions, as before. *)
						finalSols = Parallelize @ Select[
							finalSolsRaw,
							Function[sol, And[
								Less[\[Chi]cSlots] && tanCheck & @@ sol,
								DuplicateFreeQ[sol[[;;loopCount]], Abs[#2-#1] < minSepDiff &],
								AllTrue[
									totalHarmsNull /. MapThread[#1 -> #2 &, {Join[separations, tans], sol}],
									# < nullThresh &]]]];
						
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
		sols = Nearest[sols, lin, All];
		(* Label each extent. *)
		sols = MapIndexed[Coil\[Phi][First[#2]] -> #1 &] /@ sols;
		(* Return only the requested number of solutions. *)
		partSpec = Replace[coilsReturned, Except[All] -> Span[1, UpTo[coilsReturned]]];
		sols[[partSpec]]]


(* ::Section::Closed:: *)
(*Package Footer*)


End[];
EndPackage[];
