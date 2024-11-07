library(tidyverse)
library(dplyr)


#Part_1
annualDemand <- 15000
costPerUnit	<- 75
holdingCost <- 	0.165 * 75
supplierCost	<- 180

#Calculating_Economic_Order_quantity_which_is_also_known_as_Double_Inventory_Level
doubleInventoryLevel <- sqrt(((2 * annualDemand * supplierCost)
                              / (holdingCost)))
print(doubleInventoryLevel)
#Calculating_Inventory_Level
inventoryLevel <- doubleInventoryLevel/2
print(inventoryLevel)

#Calculating_number_of_order_done_annually
Order <- annualDemand / doubleInventoryLevel
print(round(Order))

#Calculating_Total_Ordering_Cost
annualOrderingCost	<- (supplierCost * annualDemand)/doubleInventoryLevel
print(annualOrderingCost)

#Calculating_annual_holding_cost 
annualHoldingCost	<- inventoryLevel * holdingCost
print(annualHoldingCost)

#Calculating_Total_cost
totalCost <-	annualOrderingCost + annualHoldingCost
print(totalCost)

#Calculating_inventory_level_order
inventorySimulation <- seq(100, 4000, by = 25)
print(inventorySimulation)

#Calculating_double_inventory_level_order
doubleInventorySimulation <- inventorySimulation * 2
print(doubleInventorySimulation)

totalCostUsingSimulation <- supplierCost * (annualDemand/doubleInventorySimulation) + (holdingCost * inventorySimulation)
simulationCost <- data.frame(inventorySimulation,doubleInventorySimulation)


simulationCost['TotalCostUsingSimulation'] <- supplierCost * (annualDemand/doubleInventorySimulation) + (holdingCost * inventorySimulation)

#Printing_minimum_total_cost
print(min(simulationCost['TotalCostUsingSimulation']))

#Printing inventory_level_order, double_inventory_level_order and minimum_total_cost
print(simulationCost[(simulationCost$TotalCostUsingSimulation==min(simulationCost$TotalCostUsingSimulation)),])

#Part_2_Triangular_Probability_Distribution

#Minimum_Annual_Demand
minValue <- 10000
#Maximum_Annual_Demand
maxValue <- 18000
#Mode_Annual_Demand
modeValue <- 15000

#Create_random_values
randomValues <- (runif(10000))
randomValues <- round(randomValues ,2)
print(randomValues)

#minimum_value_of_random_variables
min(randomValues)

#maximum_value_of_random_variables
max(randomValues)

#maximum_value_of_random_variables
area <-(modeValue-minValue)/(maxValue-minValue)
print(area)

firstTerm <-(maxValue-minValue) * (modeValue-minValue)
firstTerm

secondTerm <-(maxValue-minValue) * (maxValue-modeValue)
secondTerm

x = minValue+sqrt(randomValues*firstTerm)
y = maxValue-sqrt((1-randomValues)*secondTerm)

#Calculating_Simulation_of_annual_demand
annualDemandSimulation <- round(ifelse(x <= area, x, y),0)
summary(annualDemandSimulation)

round(sd(annualDemandSimulation),0)
print(annualDemandSimulation)

#Simulation_of_Inventory_level_and_Double_Inventory_level
doubleInventoryLevelSimulation <- sqrt(((2 * annualDemandSimulation * supplierCost) / (holdingCost)))

inventoryLevelSimulation <- doubleInventoryLevelSimulation/2
print(inventoryLevelSimulation)

#Simulation_of_Order
annualOrderSimualtion <- round(annualDemandSimulation / doubleInventoryLevelSimulation,0)
print(annualOrderSimualtion)

#Simulation_of_Ordering_cost
annualOrderingCostSimulation<- supplierCost * (annualDemandSimulation/doubleInventoryLevelSimulation)

#Simulation_of_holding_cost
holdingCostSimulation<- inventoryLevelSimulation * holdingCost

#Simulation_of_Total_Inventory_Cost
totalInventoryCostSimulation<-	annualOrderingCostSimulation + holdingCostSimulation


#Data_frame_of_simulated_values
dataFrameSimulation <- data.frame(randomValues, annualDemandSimulation, inventoryLevelSimulation, 
                        doubleInventoryLevelSimulation, totalInventoryCostSimulation, annualOrderSimualtion)

#Showing the minimum annual demand, inventory_order, double_inventory_order, total_cost and number_of_order====
print(dataFrameSimulation[(dataFrameSimulation$totalInventoryCostSimulation==min(dataFrameSimulation$totalInventoryCostSimulation)),]$annualDemandSimulation[1])

print(dataFrameSimulation[(dataFrameSimulation$totalInventoryCostSimulation==min(dataFrameSimulation$totalInventoryCostSimulation)),]$inventoryLevelSimulation[1])

print(dataFrameSimulation[(dataFrameSimulation$totalInventoryCostSimulation==min(dataFrameSimulation$totalInventoryCostSimulation)),]$doubleInventoryLevelSimulation[1])

print(dataFrameSimulation[(dataFrameSimulation$totalInventoryCostSimulation==min(dataFrameSimulation$totalInventoryCostSimulation)),]$totalInventoryCostSimulation[1])

print(dataFrameSimulation[(dataFrameSimulation$totalInventoryCostSimulation==min(dataFrameSimulation$totalInventoryCostSimulation)),]$annualOrderSimualtion[1])
#===============================================================================================================

#T-test_Total_Inventory_Cost
validateSimulatedTotalCost <- t.test(dataFrameSimulation$totalInventoryCostSimulation,conf.level = 0.95,mu=totalCost, alt = 'greater')
print(validateSimulatedTotalCost)



#T-test_Number_of_Orders
validateSimulatedNumberOfOrder <- t.test(dataFrameSimulation$annualOrderSimualtion,conf.level = 0.95,mu = Order, alt = 'less')
print(validateSimulatedNumberOfOrder)


#T-test_Order_Quantity
validateSimulatedOrderQuantity <- t.test(dataFrameSimulation$doubleInventoryLevelSimulation,conf.level = 0.95, mu = doubleInventoryLevel, alt = 'less')
print(validateSimulatedOrderQuantity)


#Deleting the variables
rm(list = ls())


