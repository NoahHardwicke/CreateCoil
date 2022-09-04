(* ::Package:: *)

(* ::Title:: *)
(*Discrete Coils*)


(* ::Section:: *)
(*Package Header*)


Unprotect["CreateCoil`*", "CreateCoil`Private`*"];
ClearAll["CreateCoil`*", "CreateCoil`Private`*"];


BeginPackage["CreateCoil`"];


FindLoopCoil::usage = "stub";

FindSaddleCoil::usage = "stub";

FindEllipseCoil::usage = "stub";

Coil\[Chi]c::usage = "Separation of loop/arc/ellipse pairs";

CoilT::usage = "Ellipse z-tangent";

DesToErr::usage = "Ratio of the desired-to-leading error harmonic magnitudes";


Begin["`Private`"];


(* ::Text:: *)
(*Setting CreateCoil`Private`$DevMode to True prints various stages of evaluation in some functions.*)


$DevMode = False;


(* ::Section:: *)
(*Magnetic Field*)


(* ::Text:: *)
(*Calculate the magnetic field as a summation over spherical harmonics.*)


(* ::Text:: *)
(*B = Sum[Ns] Sum[Ms] C[n,m] (X[n,m] x + Y[n,m] y + Z[n,m] z), where Ns and Ms are lists dictated by the coil (i.e. topology, N, and M), C[n,m] are the magnitudes of harmonics in the field and (X[n,m] x + Y[n,m] y + Z[n,m] z) are the field harmonic vector components in the magnetic field.*)


(* ::Text:: *)
(*For the loops case: m = 0, and each n is odd for symmetric coils and even for anti-symmetric coils.*)


p[{n_, m_}, x_] :=
	(-1)^m LegendreP[n, m, x] (* We do not include the Condon\[Dash]Shortley phase *)


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


(* ::Section:: *)
(*Selecting Harmonics*)


(* ::Text:: *)
(*Calculate the simultaneous equations for our desired harmonic, NDes, and the harmonics we desire to be nulled, NNull:*)


(* ::Text:: *)
(*Important: Simplify and N are used frequently at the evaluation stages of the following functions. Simplifying as we go is much faster than only applying Simplify to the final expression.*)


(* ::Text:: *)
(*Axial harmonic weighting:*)


\[Beta]["Loop"][{n_, 0}, \[Chi]_] :=
	Simplify @ N[\[Pi]/2 D[\[Chi] Sqrt[1/(1 + \[Chi]^2)], {\[Chi], n}]]


\[Beta]["Saddle"][{n_, m_}, \[Chi]_] :=
	Simplify @ N[
		(\[Pi] (2m)!)/(2^(m + 1) m!) D[
			\[Chi] (1/(1 + \[Chi]^2))^(m + 1/2) -
			Simplify @ N[m (-1)^m Sum[
				Binomial[m - 1, k] (-1)^k/(2m - 2k - 1) (\[Chi]^2/(1 + \[Chi]^2))^(m - k - 1/2),
				{k, 0, m - 1}]],
			{\[Chi], n - m}]]


q[n_, m_, x_] := (-1)^m LegendreQ[n, m, 3, x]


s[m_, t_, \[Chi]_] := Simplify @ N[(1 - I)/(2 Sqrt[2t]) q[m - 1/2, 0, I (t^2 - \[Chi]^2 - 1)/(2t)]]


\[Beta]["Ellipse"][{n_, m_}, t_, \[Chi]_] :=
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


totalHarmonic[i\[Chi]_List, {n_Integer, m_Integer}, \[Chi]c_, t_:None] :=
	(* The sum of harmonic contributions from however as many coils as there are currents. *)
	(* Simplify to quicken root-finding later. *)
	With[{term = Simplify[N[harmMag[{n, m}, \[Chi]c, t] / f[{n, m}]], Assumptions -> {\[Chi]c > 0, t > 0}]},
		Abs[Sum[
			i\[Chi][[j]] (term /. {\[Chi]c -> \[Chi]c[j], t -> t[j]}),
			{j, Length[i\[Chi]]}]]]


totalHarmonic[i\[Chi]_List, n_Integer, \[Chi]c_Symbol, t:None:None] := totalHarmonic[i\[Chi], {n, 0}, \[Chi]c]


(* ::Text:: *)
(*Determine which harmonics to null, given the coil parameters:*)


harmonicsToNull["Loop"][loopCount_, nDes_] :=
	With[{nullCount = loopCount - 1},
		(* Make a list of the first nullCount numbers with the parity of nDes, ensuring that nDes is not one of them. *)
		DeleteCases[2 Range[nullCount + 1] - If[EvenQ[nDes], 0, 1], nDes][[;; nullCount]]]


harmonicsToNull["Saddle"][loopCount_, {nDes_, mDes_}] :=
	With[{nullCount = loopCount - 1},
		(* Make a list of the odd multiples of mDes, nullCount long and ensuring that nDes is excluded. *)
		DeleteCases[mDes(2 Range[nullCount + 1] - 1), nDes][[;; nullCount]]]


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


(* ::Section:: *)
(*Nulling Code*)


findCoilOpts = Join[
	Options[FindRoot],
	{
		"CoilsReturned" -> 1,
		"ValuesOnly" -> False,
		"NulledHarmonics" -> Automatic,
		"LeadingErrorHarmonic" -> Automatic,
		"MeshPointsPer\[Chi]c" -> 10,
		"DuplicatesProximity" -> Scaled[.03],
		"NullingThreshold" -> 10.^-5,
		"MinSeparationsDifference" -> Scaled[.01],
		"ExpansionPointsPer\[Chi]c" -> Automatic,
		"ContourMeshNN" -> Automatic,
		"ExpansionBleed" -> 1.5,
		"Seed" -> 1}];


realQ[x_] := TrueQ[Element[x, Reals]]


(* If $DevMode is on, we want to be able to return echoed, labeled, iconized expressions. Otherwise discard the label. *)
echoFn[devMode_] := If[TrueQ[devMode], Function[label, Echo[#, label, Iconize]&], Identity &]


Options[FindLoopCoil] = findCoilOpts;


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
			(* Explicitly feed findSeparations all option->value pairs. *)
			allOpts = Sequence @@ Normal[Merge[{Options[FindLoopCoil], {opts}}, Last]];
			nNull = Replace[optValNull,
				Automatic :> (
					(* Calculate the nulled harmonics, and the leading error harmonic. *)
					autoHarms = harmonicsToNull["Loop"][Length[i\[Chi]] + 1, nDes];
					(* Only take the nulled harmonics for nNull. *)
					Drop[autoHarms, -1])];
			nErr = Replace[optValErr, Automatic :> Last[autoHarms]];
			findSeparations["Loop", i\[Chi], nDes, nNull, nErr, minMax\[Chi]c, {None, None}, allOpts]]]


Options[FindSaddleCoil] = findCoilOpts;


FindSaddleCoil::BadCurrents = "Currents: `1` should be a list of two or more real numbers.";
FindSaddleCoil::BadDesiredNM = "Desired harmonic order N and degree M: `1` (N) and `2` (M) should be integers, where N \[GreaterEqual] M > 0.";
FindSaddleCoil::BadSeparations = "Search range: `1` should be of the form {min\[Chi]c, max\[Chi]c}, where 0 < min\[Chi]c < max\[Chi]c.";
FindSaddleCoil::BadLeadingError = "If \"NulledHarmonics\" is not Automatic, then \"LeadingErrorHarmonic\" must be given explicitly.";


FindSaddleCoil[i\[Chi]_, {nDes_, mDes_}, minMax\[Chi]c_, opts:OptionsPattern[]] :=
	Module[{proceed = True, nNull, nmErr, autoHarms, nmNull, optValNull, optValErr, allOpts},
		
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
		
		If[!proceed, $Failed,
			(* Explicitly feed findSeparations all option->value pairs. *)
			allOpts = Sequence @@ Normal[Merge[{Options[FindSaddleCoil], {opts}}, Last]];
			nNull = Replace[optValNull,
				Automatic :> (
					(* Calculate the nulled harmonics, and the leading error harmonic. *)
					autoHarms = harmonicsToNull["Saddle"][Length[i\[Chi]] + 1, {nDes, mDes}];
					(* Only take the nulled harmonics for nNull. *)
					Drop[autoHarms, -1])];
			nmNull = {#, mDes}& /@ nNull;
			nmErr = Replace[optValErr, Automatic :> {Last[autoHarms], mDes}];
			findSeparations["Saddle", i\[Chi], {nDes, mDes}, nmNull, nmErr, minMax\[Chi]c, {None, None}, allOpts]]]


Options[FindEllipseCoil] = Replace[
	findCoilOpts,
	("MeshPointsPer\[Chi]c" -> 10) -> ("MeshPointsPer\[Chi]c" -> 5),
	1];


FindEllipseCoil::BadCurrents = "Currents: `1` should be a list of two or more real numbers.";
FindEllipseCoil::BadDesiredNM = "Desired harmonic order N and degree M: `1` (N) and `2` (M) should be integers, where N \[GreaterEqual] M > 0.";
FindEllipseCoil::BadSeparations = "Search range: `1` should be of the form {min\[Chi]c, max\[Chi]c}, where 0 < min\[Chi]c < max\[Chi]c.";
FindEllipseCoil::BadTangents = "Search range: `1` should be of the form {minT, maxT}, where 0 < minT < maxT.";
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
			Message[FindEllipseCoil::BadTangents, minMaxT]; proceed = False];
		
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

		(* Ensure Parallel kernels are running *)
		Quiet[LaunchKernels[]];
		Quiet[DistributeDefinitions["CreateCoil`"]];

		Module[
			{
				echo = echoFn[$DevMode],
				ellipseQ = topology === "Ellipse",

				(* Parameters *)
				loopCount, nullCount, varCount, separations, tans, meshPoints,
				dupProx, nullThresh, minSepDiff, expPoints, nearestPts, bleed, seed, coilsReturned, valsOnlyQ,

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
			expPoints = If[# === Automatic, Ceiling[OptionValue["MeshPointsPer\[Chi]c"]/3], #]&[
				OptionValue["ExpansionPointsPer\[Chi]c"]];
			nearestPts = If[# === Automatic, (Length[i\[Chi]] - Length[nNull]), #]&[
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
					(* Ellipses: only operate on points for which \[Chi]c[n] >= t[n] *)
					{tanCheck = If[ellipseQ,
						And @@ GreaterEqual @@@ Transpose[{{\[Chi]cSlots}, {tSlots}}],
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
										sol + Total[RandomReal[{-1,1}spread/(expPoints + 1)/2, contourDim] +  basisVs slots]&,
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
										rules /. {\[Chi]c -> Coil\[Chi]c, t -> CoilT},
										DesToErr -> (totalHarmDes/totalHarmErr /. rules)]] @@@ finalSols,
								Last][[partSpec]]];
						
						(* If "ValuesOnly" -> True, then only return the separation (and tan) values. *)
						If[valsOnlyQ,
							Function[coil,
								If[ellipseQ, Identity, Catenate][
									GatherBy[Drop[coil, -1], #[[1, 1]]&][[All, All, 2]]]] /@ coils,
							coils]]]]]


(* ::Section:: *)
(*Package Footer*)


End[];
EndPackage[];
