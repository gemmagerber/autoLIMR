! Weighted Network
! Network LIM Declaration File
! Composed with autoLIMR
! Author: Gemma Gerber
! Date: 2023-02-07

! Respiration included: Yes
! U included: Yes

! Living compartments: 3
! Non-living compartments: 1
! External compartments: 8
! Boundary flows: 11
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

Invert = 822
Plant = 500
Vert = 222
DetNLNode = 7111

### END COMPARTMENTS

### EXTERNALS

CO2
DetNLNodeImport
InvertImport
PlantImport
VertImport
DetNLNodeExport
PlantExport
VertExport

### END EXTERNALS

### VARIABLES

! Consumption (Q) / Gross Primary Production (GPP) Variables

Invert_Q = Flowto(Invert) - Invert_IM
Plant_GPP = Flowto(Plant) - Plant_IM
Vert_Q = Flowto(Vert) - Vert_IM

! Production (P/NPP) Variables

Invert_P = Flowfrom(Invert) - Invert_R - Invert_U
Plant_NPP = Flowfrom(Plant) - Plant_R - Plant_U - Plant_EX
Vert_P = Flowfrom(Vert) - Vert_R - Vert_U - Vert_EX

! Unused Energy/Material (U) Variables

Invert_U = Flowto(Invert) - Invert_P - Invert_R
Plant_U = Flowto(Plant) - Plant_NPP - Plant_R - Plant_EX
Vert_U = Flowto(Vert) - Vert_P - Vert_R - Vert_EX

! No Assimilation Efficiency (AE) Variables defined

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
Invert_IM: InvertImport -> Invert
Plant_IM: PlantImport -> Plant
Vert_IM: VertImport -> Vert

! Export flows

DetNLNode_EX: DetNLNode -> DetNLNodeExport
Plant_EX: Plant -> PlantExport
Vert_EX: Vert -> VertExport

! Adjacency Matrix flows

DetNLNode_Q_Invert: DetNLNode -> Invert
DetNLNode_Q_Vert: DetNLNode -> Vert
Invert_Q_Vert: Invert -> Vert
Invert_U_DetNLNode: Invert -> DetNLNode
Plant_Q_Invert: Plant -> Invert
Plant_U_DetNLNode: Plant -> DetNLNode
Vert_U_DetNLNode: Vert -> DetNLNode

### END FLOWS

### INEQUALITIES

! Network Data Input Inequalities

Plant_GPP > 700
Plant_GPP < 1000
DetNLNode_IM > 200
Plant_IM > 700
DetNLNode_IM < 1200
Plant_IM < 1300
DetNLNode_EX > 180
Plant_EX > 300
DetNLNode_EX < 380
Plant_EX < 1300

! Adjacency Matrix Inequalities

DetNLNode_Q_Invert = Invert_Q * [0.01,0.6]
Plant_Q_Invert < Invert_Q * 0.6
Invert_Q_Vert = Vert_Q * [0.6,1]

### END INEQUALITIES

