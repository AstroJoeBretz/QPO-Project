(* ::Package:: *)

(* ::Input::Initialization:: *)
c1=1;
c2=370;
\[Mu]1=1;
\[Mu]2=1/11;
Rs=1;
prec=64;
\[CapitalDelta]\[Omega]=4;

\[Alpha]=(c2 \[Mu]1 -c1 \[Mu]2 )/(c2 \[Mu]1 +c1 \[Mu]2)


(* ::Input::Initialization:: *)
A2reduced[l_,\[Omega]_]=((1-\[Alpha])SphericalBesselJ[l,(Rs \[Omega])/c1] ((Rs \[Omega])/ c2 SphericalHankelH2[l-1,(Rs \[Omega])/c2]-(l+2) SphericalHankelH2[l,(Rs \[Omega])/c2])-(1+\[Alpha]) SphericalHankelH2[l,(Rs \[Omega])/c2]((Rs \[Omega])/ c2 SphericalBesselJ[l-1,(Rs \[Omega])/c1]- c1/c2 (l+2)SphericalBesselJ[l,(Rs \[Omega])/c1]));


SetDirectory[NotebookDirectory[]];
Do[
xms=Import["c100XMl"<>ToString[lm]<>".Mx"];
xmsl[l_]:=DeleteDuplicatesBy[Chop[xms[[l-lm+10]]],SetPrecision[#,10]&];
xmri[lm]=Table[ReIm[xmsl[l]],{l,lm-9,lm}];,
{lm,{10,20,30}}]

xmrtot=Flatten[xmri/@{10,20,30},1];


(* ::Input::Initialization:: *)
rf[l_,n_]:=\[Omega]/.FindRoot[A2reduced[l,\[Omega]]==0,{\[Omega],37/10 (xmrtot[[l,n,1]]+I xmrtot[[l,n,2]])},WorkingPrecision->prec,PrecisionGoal->prec,MaxIterations->2000];


(* ::Input:: *)
(*scan[l_]:=DeleteDuplicates[Select[Table[rf[l,10(xmrtot[[l,n,1]]+I xmrtot[[l,n,2]])],{x,scset[l]}],Im[#]>1&]];*)


xms=Table[Table[rf[l,n],{n,Length[xmrtot[[l]]]}],{l,1,25}];


Export["c370XMl25.Mx",xms]
