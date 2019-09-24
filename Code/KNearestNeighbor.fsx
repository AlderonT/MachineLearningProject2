//----------------------------------------------------------------------------------------------------- 
// Assignment 2
// Chris Major
// 9/20/19
//
// Implementation of the K-Nearest Neighbor Algorithm for classification and regression.
// Applicable to all six datasets (abalone.data, car.data, foresfires.csv, machine.data, 
//  segmentation.data, and winequality-red.csv, winequality-white.csv)
//
//-----------------------------------------------------------------------------------------------------


// Type Definitions:
//-----------------------------------------------------------------------------------------------------

open System

// Classification types (Class)
type Class = string

// Data format for sample data (Attribute)
type Data = {
    uid         : string option     //some datasets have id values so we have this as an optional value
    cls         : string            //we are taking the strings as the classes
    attributes  : string []         //we are compiling all attributes into a list of the string values
} 

// Type alias for the training set
type DataSet = Data seq


// Two steps: assignment and Update

//luckily for us all points will be floats (damnit)
// Function to calculate Euclidean Distance D(x,y)
let EuclideanDistance (point1 : int[]) (point2 : int[]) = 
    sqrt( ( point1.[0] - point2.[0] )**2 //you can't to power operations on int (can't have fractions) 
        + ( point1.[1] - point2.[1] )**2 //also can't do sqrt on int
        )                                //until this thinks EuclideanDistance is correct you'll get the same error on 46
   

// Function to read in data points

// Implementations
let pointExample1 = [|0.; 0.|];
let pointExample2 = [|3.; 4.|];
let distance = EuclideanDistance 

printfn "%A" (distance pointExample1 pointExample1)

//-----------------------------------------------------------------------------------------------------
// END OF CODE