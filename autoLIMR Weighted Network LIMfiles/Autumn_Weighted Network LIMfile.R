! Weighted Network
! Network LIM Declaration File
! Composed with autoLIMR
! Author: Gemma Gerber
! Date: 2022-10-28

! Respiration included: Yes
! U included: Yes

! Living compartments: 3
! Non-living compartments: 1
! External compartments: 9
! Boundary flows: 12
! Internal flows: 6

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

Invertebrate = 900.1000
Plant = 600.0000
Vertebrate = 33.0000
DetritusNLNode = 8000.0000

### END COMPARTMENTS

### EXTERNALS

CO2
DetritusNLNodeInput
InvertebrateInput
PlantInput
VertebrateInput
DetritusNLNodeExport
InvertebrateExport
PlantExport
VertebrateExport

### END EXTERNALS

### VARIABLES

! Consumption (Q) / Gross Primary Production (GPP) Variables

Invertebrate_Q = Flowto(Invertebrate) - Invertebrate_IN
Plant_GPP = Flowto(Plant) - Plant_IN
Vertebrate_Q = Flowto(Vertebrate) - Vertebrate_IN

! Production (P/NPP) Variables

Invertebrate_P = Flowfrom(Invertebrate) - Invertebrate_R - Invertebrate_U - Invertebrate_EX
Plant_NPP = Flowfrom(Plant) - Plant_R - Plant_U - Plant_EX
Vertebrate_P = Flowfrom(Vertebrate) - Vertebrate_R - Vertebrate_U - Vertebrate_EX

! Unused Energy/Material (U) Variables

Invertebrate_U = Flowto(Invertebrate) - Invertebrate_P - Invertebrate_R - Invertebrate_EX
Plant_U = Flowto(Plant) - Plant_NPP - Plant_R - Plant_EX
Vertebrate_U = Flowto(Vertebrate) - Vertebrate_P - Vertebrate_R - Vertebrate_EX

! No Assimilation Efficiency (AE) Variables defined

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
Vertebrate_IM: VertebrateImport -> Vertebrate

! Export flows

DetritusNLNode_EX: DetritusNLNode -> DetritusNLNodeExport
Plant_EX: Plant -> PlantExport
Invertebrate_EX: Invertebrate -> InvertebrateExport
Vertebrate_EX: Vertebrate -> VertebrateExport

! Adjacency Matrix flows

DetritusNLNode_Q_Invertebrate: DetritusNLNode -> Invertebrate
Invertebrate_Q_Vertebrate: Invertebrate -> Vertebrate
Invertebrate_U_DetritusNLNode: Invertebrate -> DetritusNLNode
Plant_Q_Invertebrate: Plant -> Invertebrate
Plant_U_DetritusNLNode: Plant -> DetritusNLNode
Vertebrate_U_DetritusNLNode: Vertebrate -> DetritusNLNode

### END FLOWS

### INEQUALITIES

! Network Data Input Inequalities

Plant_GPP > 0.0300
Plant_GPP < 0.4000
Plant_NPP > 0.4000
Invertebrate_P > 0.002*Invertebrate_U
Vertebrate_P > 0.0002
Plant_NPP < 0.9000
Invertebrate_P < 0.04*Invertebrate_U
Vertebrate_P < 0.0030
Plant_R > 0.2*Plant_NPP
Invertebrate_R > 0.8*Invertebrate_P
Vertebrate_R > 0.3500
Plant_R < 0.5*Plant_NPP
Invertebrate_R < 3*Invertbrate_P
Vertebrate_R < 0.3500

! Adjacency Matrix Inequalities


### END INEQUALITIES

