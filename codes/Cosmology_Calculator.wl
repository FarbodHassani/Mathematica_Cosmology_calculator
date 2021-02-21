(* ::Package:: *)

(*Function to compute conformal time for a given redshift*)
tau[zs_,H0_,\[CapitalOmega]m_,\[CapitalOmega]\[CapitalLambda]_,w_,\[CapitalOmega]r_]:=NIntegrate[H0^-1/Sqrt[\[CapitalOmega]\[CapitalLambda] (1+z)^(3(1+w)) +\[CapitalOmega]m (1+z)^3+\[CapitalOmega]r ((1+z)^4) ],{z,zs,\[Infinity]}]


(*Function to compute Physical time for a given redshift = (a'(tau))/a*)
time[zs_,H0_,\[CapitalOmega]m_,\[CapitalOmega]\[CapitalLambda]_,w_,\[CapitalOmega]r_]:=NIntegrate[H0^-1/((1+z)Sqrt[\[CapitalOmega]\[CapitalLambda] (1+z)^(3(1+w)) +\[CapitalOmega]m (1+z)^3+\[CapitalOmega]r ((1+z)^4) ]),{z,zs,\[Infinity]}]


(*Physical Hubble Function at a given redshift = Overscript[a, .]/a *)
ConformalHubblePrime[z_,H0_,\[CapitalOmega]m_,\[CapitalOmega]d_,w_,\[CapitalOmega]r_]:=D[ConformalHubble[z,H0,\[CapitalOmega]m,\[CapitalOmega]d,w,\[CapitalOmega]r],z]*D[1/a-1,a]*ConformalHubble[z,H0,\[CapitalOmega]m,\[CapitalOmega]d,w,\[CapitalOmega]r]*1/(1+z);  (* (d HH)/dtau = dH/dz  dz/da da/dtau*)


(*Physical Hubble Function at a given redshift = Overscript[a, .]/a *)
ConformalHubblePrimePrime[z_,H0_,\[CapitalOmega]m_,\[CapitalOmega]d_,w_,\[CapitalOmega]r_]:=D[ConformalHubblePrime[z,H0,\[CapitalOmega]m,\[CapitalOmega]d,w,\[CapitalOmega]r],z]*D[1/a-1,a]*ConformalHubble[z,H0,\[CapitalOmega]m,\[CapitalOmega]d,w,\[CapitalOmega]r]*1/(1+z);  (* (d ^2HH)/dtau^2 = dH/dz  dz/da da/dtau*)


(*Physical Hubble Function at a given redshift = Overscript[a, .]/a *)
Massresolution[L_,Num_]:= 6.6 10^11 L^3/Num;(*in M_sun*)(* Critical density of the Universe is 6.6*10^11M_sun*)


(*Conformal Hubble Function at a given redshift *)
ConformalHubble[z_,H0_,\[CapitalOmega]m_,\[CapitalOmega]\[CapitalLambda]_,w_,\[CapitalOmega]r_]:=H0 Sqrt[\[CapitalOmega]\[CapitalLambda] (1+z)^(1+3 w) +\[CapitalOmega]m (1+z)+\[CapitalOmega]r ((1+z)^2) ]





(* ::Text:: *)
(*PDE part:*)


pdConv[f_]:=TraditionalForm[f/.Derivative[inds__][g_][vars__]:>Apply[Defer[D[g[vars],##]]&,
Transpose[{{vars},{inds}}]/.{{var_,0}:>Sequence[],{var_,1}:>{var}}]] (*To show the derivatives beautifully*)


odeform[Eq_,assum_,x_,y_]:=Module[{f},
  f[t_,x_]:=assum[t,x] ; Block[{f},(f[#1,#2]&)[x,y]:=assum;Eq]]


odeform[Eq_,assum_,f_,x_,y_]:=Block[{f},f:=assum;Eq]


Map[Take[#, 2] &, {{2, 1, 7}, {4, 1, 5}, {3, 1, 2}}]
