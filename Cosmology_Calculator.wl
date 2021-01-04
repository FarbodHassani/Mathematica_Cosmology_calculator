(* ::Package:: *)

(*Function to compute conformal time for a given redshift*)
tau[zs_,H0_,\[CapitalOmega]m_,\[CapitalOmega]\[CapitalLambda]_,w_,\[CapitalOmega]r_]:=NIntegrate[H0^-1/Sqrt[\[CapitalOmega]\[CapitalLambda] (1+z)^(3(1+w)) +\[CapitalOmega]m (1+z)^3+\[CapitalOmega]r ((1+z)^4) ],{z,zs,\[Infinity]}]


(*Function to compute Physical time for a given redshift = (a'(tau))/a*)
time[zs_,H0_,\[CapitalOmega]m_,\[CapitalOmega]\[CapitalLambda]_,w_,\[CapitalOmega]r_]:=NIntegrate[H0^-1/((1+z)Sqrt[\[CapitalOmega]\[CapitalLambda] (1+z)^(3(1+w)) +\[CapitalOmega]m (1+z)^3+\[CapitalOmega]r ((1+z)^4) ]),{z,zs,\[Infinity]}]


(*Conformal Hubble Function at a given redshift *)
ConformalHubble[z_,H0_,\[CapitalOmega]m_,\[CapitalOmega]\[CapitalLambda]_,w_,\[CapitalOmega]r_]:=H0 Sqrt[\[CapitalOmega]\[CapitalLambda] (1+z)^(1+3 w) +\[CapitalOmega]m (1+z)+\[CapitalOmega]r ((1+z)^2) ]


(*Physical Hubble Function at a given redshift = Overscript[a, .]/a *)
ConformalHubblePrime[z_,H0_,\[CapitalOmega]m_,\[CapitalOmega]d_,w_,\[CapitalOmega]r_]:=D[ConformalHubble[z,H0,\[CapitalOmega]m,\[CapitalOmega]d,w,\[CapitalOmega]r],z]*D[1/a-1,a]*ConformalHubble[z,H0,\[CapitalOmega]m,\[CapitalOmega]d,w,\[CapitalOmega]r]*1/(1+z);  (* (d HH)/dtau = dH/dz  dz/da da/dtau*)


(*Physical Hubble Function at a given redshift = Overscript[a, .]/a *)
ConformalHubblePrimePrime[z_,H0_,\[CapitalOmega]m_,\[CapitalOmega]d_,w_,\[CapitalOmega]r_]:=D[ConformalHubblePrime[z,H0,\[CapitalOmega]m,\[CapitalOmega]d,w,\[CapitalOmega]r],z]*D[1/a-1,a]*ConformalHubble[z,H0,\[CapitalOmega]m,\[CapitalOmega]d,w,\[CapitalOmega]r]*1/(1+z);  (* (d ^2HH)/dtau^2 = dH/dz  dz/da da/dtau*)
