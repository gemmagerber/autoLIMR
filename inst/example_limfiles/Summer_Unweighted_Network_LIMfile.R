! Unweighted Network
! Network LIM Declaration File
! Composed with autoLIMR
! Author: gemma
! Date: 2023-06-17

! Respiration included: Yes
! U included: Yes

! Living compartments: 3
! Non-living compartments: 1
! External compartments: 7
! Boundary flows: 10
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

Invert = 2000.0
Plant = 800.0
Vert = 55.0
DetNLNode = 10000.2

### END COMPARTMENTS

### EXTERNALS

CO2
DetNLNodeImport
PlantImport
DetNLNodeExport
InvertExport
PlantExport
VertExport

### END EXTERNALS

### VARIABLES

! Consumption (Q) / Gross Primary Production (GPP) Variables

Invert_Q = Flowto(Invert)
Plant_GPP = Flowto(Plant) - Plant_IM
Vert_Q = Flowto(Vert)

! Production (P/NPP) Variables

Invert_P = Flowfrom(Invert) - Invert_R - Invert_U - Invert_EX
Plant_NPP = Flowfrom(Plant) - Plant_R - Plant_U - Plant_EX
Vert_P = Flowfrom(Vert) - Vert_R - Vert_U - Vert_EX

! Unused Energy/Material (U) Variables

Invert_U = Flowto(Invert) - Invert_P - Invert_R - Invert_EX
Plant_U = Flowto(Plant) - Plant_NPP - Plant_R - Plant_EX
Vert_U = Flowto(Vert) - Vert_P - Vert_R - Vert_EX

! Assimilation Efficiency (AE) variables

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
Invert_EX: Invert -> InvertExport
Vert_EX: Vert -> VertExport

! Adjacency Matrix flows

DetNLNode_Q_Invert: DetNLNode -> Invert
Invert_Q_Vert: Invert -> Vert
Invert_U_DetNLNode: Invert -> DetNLNode
Plant_Q_Invert: Plant -> Invert
Plant_U_DetNLNode: Plant -> DetNLNode
Vert_U_DetNLNode: Vert -> DetNLNode

### END FLOWS

