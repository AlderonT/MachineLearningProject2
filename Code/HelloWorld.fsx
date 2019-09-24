
printfn "Hello World"

let A = [|0.1;0.2;0.9;1.4|]
let B = [|0.3;1.2;3.9;1.4|]

let distance (A :float seq) (B : float seq) = 
    Seq.zip A B 
    |> Seq.sumBy (fun (a,b) -> (a-b)*(a-b)) 
    |> sqrt

let midpoint (ps :float array seq) = 
   Array.init (ps|>Seq.head|>Array.length) (fun i -> 
      ps|>Seq.averageBy (fun x -> x.[i])
   )   

distance [|0.0;0.0;0.0;0.0|] [|1.0;1.0;1.0;1.0|]
distance A B

type Cluster = {
    centroid : float []
    points : float [] Set
}

let fetchTrainingSet filePath isCommaSeperated hasHeader =
    System.IO.File.ReadAllLines(filePath) // this give you back a set of line from the file (replace with your directory)
    |> Seq.map (fun v -> v.Trim())
    |> Seq.filter (System.String.IsNullOrWhiteSpace >> not)
    |> Seq.filter (fun line ->
        if isCommaSeperated && line.StartsWith(";") then false 
        else true
        )
    |> (if hasHeader then Seq.skip 1 else id)
    |> Seq.map (fun line -> line.Split(if isCommaSeperated then ',' else ';') |> Array.map (fun value -> value.Trim())) // this give you an array of elements from the comma seperated fields. We trim to make sure that any white space is removed.



let trainingDataSet =
    fetchTrainingSet (@"D:\Fall2019\Machine Learning\Project 2\Data\winequality-red.csv") false true // this give you back a set of line from the file (replace with your directory)
    |> Seq.map (fun fields ->  fields |> Array.map (fun x -> x |> float))   //take each individual string in each array in our sequence, and convert it to a float

midpoint trainingDataSet //midpoint: [|8.319637273; 0.5278205128; 0.2709756098; 2.538805503; 0.08746654159; 15.87492183; 46.46779237; 0.9967466792; 3.311113196; 0.658148843; 10.42298311; 5.636022514|]


////Playing with modular arithmatic (which we'll need for the forestfire dataset
//let a = 0
//let b=6
//let n=7
//let add n a b  = ((a%n)+(b%n))%n
//let sub n a b  = ((a%n)+(n-(b%n)))%n
//let dist n a b = sub n (max a b) (min a b) 
//let mid n a b  = add n ((dist n a b)/2)  (min a b)

//Array.init 14 (add n 4)
//Array.init 14 (sub n 4)

//dist n 6 14
//mid n 6 11
//                    1                   2                   3
//0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4
//0 1 2 3 4 5 6 0 1 2 3 4 5 6 0 1 2 3 4 5 6 0 1 2 3 4 5 6 0 1 2 3 4 5 6
//            ^    m    ^
//            #





