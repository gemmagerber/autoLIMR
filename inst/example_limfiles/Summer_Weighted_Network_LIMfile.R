! Weighted Network
! Network LIM Declaration File
! Composed with autoLIMR
! Author: Gemma Gerber
! Date: 2023-02-07

! Respiration included: Yes
! U included: Yes

! Living compartments: 3
! Non-living compartments: 1
! External compartments: 6
! Boundary flows: 9
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

Invert = 2000.00
Plant = 800.00
Vert = 55.00
DetNLNode = 10000.20

### END COMPARTMENTS

### EXTERNALS

CO2
DetNLNodeImport
PlantImport
DetNLNodeExport
PlantExport
VertExport

### END EXTERNALS

### VARIABLES

! Consumption (Q) / Gross Primary Production (GPP) Variables

Invert_Q = Flowto(Invert)
Plant_GPP = Flowto(Plant) - Plant_IM
Vert_Q = Flowto(Vert)

! Production (P/NPP) Variables

Invert_P = Flowfrom(Invert) - Invert_R - Invert_U
Plant_NPP = Flowfrom(Plant) - Plant_R - Plant_U - Plant_EX
Vert_P = Flowfrom(Vert) - Vert_R - Vert_U - Vert_EX

! Unused Energy/Material (U) Variables

Invert_U = Flowto(Invert) - Invert_P - Invert_R
Plant_U = Flowto(Plant) - Plant_NPP - Plant_R - Plant_EX
Vert_U = Flowto(Vert) - Vert_P - Vert_R - Vert_EX

! Assimilation Efficiency (AE) Variables

Invert_AE = Invert_P + Invert_R
Vert_AE = Vert_P + Vert_R

### END VARIABLES

### FLOWS

! GPP flows

Plant_GPP: CO2 -> Plant

! Respiration flows

Plant_R: Plant -> CO2
Invert_R: Invert -> CO2
Vert_R: Vert -> CO2

! Import flows

DetNLNode_IM: DetNLNodeImport -> DetNLNode
Plant_IM: PlantImport -> Plant

! Export flows

DetNLNode_EX: DetNLNode -> DetNLNodeExport
Plant_EX: Plant -> PlantExport
Vert_EX: Vert -> VertExport

! Adjacency Matrix flows

DetNLNode_Q_Invert: DetNLNode -> Invert
Invert_Q_Vert: Invert -> Vert
Invert_U_DetNLNode: Invert -> DetNLNode
Plant_Q_Invert: Plant -> Invert
Plant_U_DetNLNode: Plant -> DetNLNode
Vert_U_DetNLNode: Vert -> DetNLNode

### END FLOWS

### INEQUALITIES

! Network Data Input Inequalities

Plant_GPP > 1100.00
Invert_Q > 100.00
Plant_GPP < 2600.00
Invert_Q < 3000.00
Plant_NPP > 0.6*Plant_GPP
Invert_P > 0.4*Invert_Q
Plant_NPP < 0.8*Plant_GPP
Invert_P < 0.6*Invert_Q
Plant_R > 0.4*Plant_NPP
Invert_R > 1*Invert_P
Vert_R > 0.75
Plant_R < 0.7*Plant_NPP
Vert_R < 0.75
Vert_U < 0.05*Vert_Q
Invert_AE > 0.5*Invert_Q
Vert_AE > 0.2*Vert_Q

! Adjacency Matrix Inequalities


### END INEQUALITIES

