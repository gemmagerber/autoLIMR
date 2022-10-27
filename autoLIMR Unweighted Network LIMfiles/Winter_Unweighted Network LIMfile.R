! Unweighted Network
! Network LIM Declaration File
! Composed with autoLIMR
! Author: gemma
! Date: 2022-10-28

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
DetritusNLNodeInput
InvertebrateInput
PlantInput
DetritusNLNodeExport
PlantExport
VertebrateExport

### END EXTERNALS

### VARIABLES

! Consumption (Q) / Gross Primary Production (GPP) Variables

Invertebrate_Q = Flowto(Invertebrate) - Invertebrate_IN
Plant_GPP = Flowto(Plant) - Plant_IN
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

