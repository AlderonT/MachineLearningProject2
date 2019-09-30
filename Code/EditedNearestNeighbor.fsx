//----------------------------------------------------------------------------------------------------- 
// Assignment 2
// Chris Major
// 9/20/19
//
// Implementation of the Edited Nearest Neighbor Algorithm for classification and regression.
// Applicable to the three classification  datasets (abalone.data, car.data, foresfires.csv, 
//  machine.data) 
//
//
//-----------------------------------------------------------------------------------------------------


// Type Definitions:
//-----------------------------------------------------------------------------------------------------



// Functions:
//-----------------------------------------------------------------------------------------------------

// Function to calculate Euclidean Distance D(x,y)
// Note: this will only work on sequences of the same length
let EuclideanDistance (A : float seq) (B : float seq) = 

    // Separate the sequences by data point values and calculate the Euclidean distance
    Seq.zip A B
    |> Seq.sumBy (fun (a,b) -> (a - b) ** 2.0)
    |> sqrt
   

// Function to calculate the midpoint
// Note: this will only work on sequences of the same length
let midpoint (A : float seq) (B : float seq) = 
   
    // Separate the sequences by data point values and calculate the Euclidean distance
    Seq.zip A B
    |> Seq.sumBy (fun (a,b) -> (a - b) ** 2.0)
    |> sqrt

// Function to implement the Edited Nearest Neighbor Algorithm
let EditedNearestNeighbor (x : float seq) = 
    Seq.init D.length ( fun i -> D
        |> Seq.map(fun x' -> dist(x' , D.[i]), D.[i], x')
        |> Seq.filter( fun (d, _, _) -> d <> 0)
        |> Seq.minBy fst
        |> (fun(d, p, x') -> if p.[class] <> x'.[class] then Some p else None)      // Fill in class later
    )
    |> Seq.filler (fun p -> Some p)



// Function to read in data points


// Implementation
vs
|> Seq.map( fun x -> KNearestNeighbor ts n x)



//-----------------------------------------------------------------------------------------------------
// END OF CODE