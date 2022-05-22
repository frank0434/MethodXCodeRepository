# Sustainable vegetable system 

This is the repositry where the magic begins

//www.plantuml.com/plantuml/png/TP5DImD138Rl-HLnBrxSWxP2Ao95AzYRQD5pt4tRqVbOPhDWBVtlPifG1z5BUSFpaZwJdVL2KGPduKGsx1WxIyQ3vPMW18nclH5iJUH6JF1mYWyXcg_WXInQuxc_mhg-ESFhkjOVZLfvuIYcAGg99wmKEce-QCrOBLmNek2SX2hS0M9uU-T6i7fI0IJVukFziaBg5PmNU15LYf9DXXS5V0x6eWkcbo4oDkh9Nd9lLFJko8TkYyQt_9dnkC2EXLeIEg0qm1wXS5z6ylqgH9SzIlrN4evuIvPymtdtPS7dPEmM_n2O0En__YFtVctR_VlMbZlBnLMckWqHG8-Nlj11GczLv56HnpbF8BtsE2ZCUGew7_ECVJiu-mq0


```
@startuml
!theme plain
left to right direction
' Horizontal lines: -->, <--, <-->
' Vertical lines: ->, <-, <->

package "Import Data" {
  object "Rain and PET" as ob1
  object "Irrigation" as ob2
  object "Soil moisture content" as ob3
  object "Canopy index" as ob4
  object "metadata" as meta
} 

package "Transformation"{
  object "Daily Canopy index \nand PET correction" as ob5
  object "Rain + Irrigation + Soil Moisture Content" as ob7

}

package "Daily Water Balance" {
  object "Soil moisture depletion" as ob8

}  


ob1 --> ob5
ob4 --> ob5: Excel adaptor
meta -> ob5: Excel adaptor

ob1 --> ob7: API
ob2 --> ob7: Excel adaptor
ob3 --> ob7: Excel adaptor

ob5 --> ob8
ob7 --> ob8
@enduml
```