! Weighted Network
! Network LIM Declaration File
! Composed with autoLIMR
! Author: Gemma Gerber
! Date: 2023-01-09

! Respiration included: Yes
! U included: Yes

! Living compartments: 3
! Non-living compartments: 1
! External compartments: 7
! Boundary flows: 10
! Internal flows: 7

! Abbreviations
! GPP = Gross Primary Production (Primary Producers only)
! Q = Consumption
! NPP = Net Primary Production (Primary Producers only)
! P = Production
! R = respiration
! U = Passive flows to non-living compartments/Unassimilated material
! AE = Assimilation Efficiency
! IM = Import flow
! EX = Export Flow
! NLNode = Non-living compartment

### COMPARTMENTS

Invertebrate = 800.00
Plant = 500.00
Vertebrate = 200.00
DetritusNLNode = 7000.00

### END COMPARTMENTS

### EXTERNALS

CO2
DetritusNLNodeImport
InvertebrateImport
PlantImport
DetritusNLNodeExport
PlantExport
VertebrateExport

### END EXTERNALS

### VARIABLES

! Consumption (Q) / Gross Primary Production (GPP) Variables

Invertebrate_Q = Flowto(Invertebrate) - Invertebrate_IM
Plant_GPP = Flowto(Plant) - Plant_IM
Vertebrate_Q = Flowto(Vertebrate)

! Production (P/NPP) Variables

Invertebrate_P = Flowfrom(Invertebrate) - Invertebrate_R - Invertebrate_U
Plant_NPP = Flowfrom(Plant) - Plant_R - Plant_U - Plant_EX
Vertebrate_P = Flowfrom(Vertebrate) - Vertebrate_R - Vertebrate_U - Vertebrate_EX

! Unused Energy/Material (U) Variables

Invertebrate_U = Flowto(Invertebrate) - Invertebrate_P - Invertebrate_R
Plant_U = Flowto(Plant) - Plant_NPP - Plant_R - Plant_EX
Vertebrate_U = Flowto(Vertebrate) - Vertebrate_P - Vertebrate_R - Vertebrate_EX

! Assimilation Efficiency (AE) Variables

Invertebrate_AE = Invertebrate_P + Invertebrate_R
Vertebrate_AE = Vertebrate_P + Vertebrate_R

### END VARIABLES

### FLOWS

! GPP flows

Plant_GPP: CO2 -> Plant

! Respiration flows

Plant_R: Plant -> CO2
Invertebrate_R: Invertebrate -> CO2
Vertebrate_R: Vertebrate -> CO2

! Import flows

DetritusNLNode_IM: DetritusNLNodeImport -> DetritusNLNode
Invertebrate_IM: InvertebrateImport -> Invertebrate
Plant_IM: PlantImport -> Plant

! Export flows

DetritusNLNode_EX: DetritusNLNode -> DetritusNLNodeExport
Plant_EX: Plant -> PlantExport
Vertebrate_EX: Vertebrate -> VertebrateExport

! Adjacency Matrix flows

DetritusNLNode_Q_Invertebrate: DetritusNLNode -> Invertebrate
DetritusNLNode_Q_Vertebrate: DetritusNLNode -> Vertebrate
Invertebrate_Q_Vertebrate: Invertebrate -> Vertebrate
Invertebrate_U_DetritusNLNode: Invertebrate -> DetritusNLNode
Plant_Q_Invertebrate: Plant -> Invertebrate
Plant_U_DetritusNLNode: Plant -> DetritusNLNode
Vertebrate_U_DetritusNLNode: Vertebrate -> DetritusNLNode

### END FLOWS

### INEQUALITIES

! Network Data Input Inequalities

Plant_GPP > 200.00
Invertebrate_Q > 500.00
Plant_GPP < 1000.00
Invertebrate_Q < 900.00
Vertebrate_Q < 350.00
Invertebrate_R > 0.75
Invertebrate_R < 0.75
Plant_U < 200.00
Plant_IM > 700.00
DetritusNLNode_IM < 1200.00
Plant_IM < 1300.00
DetritusNLNode_EX < 380.00
Plant_EX < 1300.00
Invertebrate_AE > Invertebrate_Q * 0.50
Vertebrate_AE > Vertebrate_Q * 0.20
Invertebrate_AE > Invertebrate_Q * 0.80
Vertebrate_AE > Vertebrate_Q * 0.30

! Adjacency Matrix Inequalities

DetritusNLNode_Q_Invertebrate = Invertebrate_Q * [0.01,0.6]
Plant_Q_Invertebrate < Invertebrate_Q * 0.6
Invertebrate_Q_Vertebrate = Vertebrate_Q * [0.6,1]

### END INEQUALITIES

