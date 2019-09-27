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



// Functions:
//-----------------------------------------------------------------------------------------------------

// Function to calculate Euclidean Distance D(x,y)
let EuclideanDistance (A : float seq) (B : float seq) = 

    // Separate the sequences by data point values and calculate the Euclidean distance
    Seq.zip A B
    |> Seq.sumBy (fun (a,b) -> (a - b) ** 2.0)
    |> sqrt
   



// Function to read in data points

// Implementations (Test)
let pointExample1 = [|0.0 ; 0.0; 0.0|];
let pointExample2 = [|3.0 ; 4.0; 5.0|];
let result = EuclideanDistance pointExample1 pointExample2

printfn "%A" result

//-----------------------------------------------------------------------------------------------------
// END OF CODE