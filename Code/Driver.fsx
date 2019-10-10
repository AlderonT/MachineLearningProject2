#load "tools.fsx"
open Tools

//-------------------------------------------------------------------------------------------------------
//
//  CSCI 447 - Machine Learning      
//  Assignment #2
//  Chris Major, Farshina Nazrul-Shimim, Tysen Radovich, Allen Simpson
//
//  Implementation and demonstration of K-nearest Neighbor (KNN), Edited Nearest Neighbor (ENN),
//  Condensed Nearest Neighbor (CNN), K-Means Regression, and K-Medoids algorithms.
//
//-------------------------------------------------------------------------------------------------------

// Module Declaration for runtime
module Project2 =


    // CLASSES
    //---------------------------------------------------------------------------------------------------

    type Point = 
        abstract member realAttributes: float[]
        abstract member categoricalAttributes: string[]
        abstract member cls : string option
        abstract member regressionValue : float option
        abstract member distance: p:Point -> trainingDataSet:Point seq-> float      //sqrt((Real distance)^2+(CategoricalClassification distance)^2+(CategoricalRegression distance)^2)

 
    // Class for a Classification process output
    type Classifier =
        abstract member classify: p:Point -> string option

    // Class for a Regression process output
    type Regresser =
        abstract member regress: p:Point -> float
    ////How to get a dataset from a file
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
        
    ////DISTANCE FUNCTIONS
    let getCategoricalRegressionDistance (point:Point) (target:Point) (trainingSet:Point seq) (p:float)=
        let avgRegValue = trainingSet |> Seq.map (fun p -> match p.regressionValue with | None -> 0. | Some v -> v) |> Seq.average 
        let avgRegValueGiven x i = trainingSet |> Seq.filter (fun p -> p.categoricalAttributes.[i] = x)|> Seq.map (fun p -> match p.regressionValue with | None -> 0. | Some v -> v) |> Seq.average
        
        point.categoricalAttributes
        |> Seq.mapi (fun i a -> 
            System.Math.Pow (((((avgRegValueGiven a i)/avgRegValue) - ((avgRegValueGiven target.categoricalAttributes.[i] i)/avgRegValue))
            |>System.Math.Abs
            ),p)   
        )
        |>Seq.sum
        |> (fun v -> System.Math.Pow (v,1./p))



    let PointsWithValue_vi_and_cls (trainingSet:Point[]) (vi:string) (cls: string) (attribIndex:int)= 
        trainingSet 
        |> Seq.filter (fun cPoint -> cPoint.cls = Some cls && cPoint.categoricalAttributes.[attribIndex] = vi)
        |> Seq.length
        |> float

    let PointsWithValue_vi (trainingSet:Point[]) (vi:string) (attribIndex:int)= 
        trainingSet 
        |> Seq.filter (fun cPoint -> cPoint.categoricalAttributes.[attribIndex]=vi)
        |> Seq.length
        |> float

    let classificationPercent trainingSet ((i,vi):int*string) (vj :string) (p:float) = 
        trainingSet 
        |> Seq.choose (fun (pt:Point) -> pt.cls)
        |> Seq.map (fun cls -> 
            System.Math.Pow (
                (
                    (
                        (PointsWithValue_vi_and_cls trainingSet vi cls i)
                        /(PointsWithValue_vi trainingSet vi i)
                    )-(
                        (PointsWithValue_vi_and_cls trainingSet vj cls i )
                        /(PointsWithValue_vi trainingSet vj i)
                    )
                ),p)
        ) 
        |> Seq.sum
        |> (fun v -> System.Math.Pow (v,1./p))
        

    let getCategoricalClassificationDistance (point:Point) (target: Point) (trainingSet:Point seq) p= 
        point.categoricalAttributes
        |> Seq.mapi (fun i a -> classificationPercent (trainingSet|>Seq.toArray) (i,a) target.categoricalAttributes.[i] p)
        |> Seq.sum 
    
    
    ////GET THE DATASET
    let fullDataset filename (classIndex:int option) (regressionIndex : int option) (pValue:float)= 
        let classIndex,regressionIndex = 
            match classIndex,regressionIndex with 
            | None,None     -> -1,-1
            | None,Some a   -> -1,a 
            | Some a,None   -> a,-1
            | Some a,Some b -> a,b
        let dataSet = fetchTrainingSet filename true false

        let columns = dataSet|> Seq.transpose|> Seq.toArray
        let realIndexes,categoricalIndexes = 
            columns
            |>Seq.mapi (fun i c -> i,c)
            |>Seq.filter (fun (i,_) -> i<>regressionIndex && i<> classIndex)
            |>Seq.map (fun (i,c) ->
                
                i,
                (c
                 |> Seq.exists (fun v -> 
                    v
                    |>System.Double.tryParse 
                    |> Option.isNone
                    )
                )
            )
            |>Seq.toArray
            |>Array.partition snd
            |>(fun (c,r) -> (r|> Seq.map fst |>Set.ofSeq),(c|>Seq.map fst |>Set.ofSeq))
            

        dataSet
        |> Seq.map (fun p -> 
            {new Point with 
                member _.cls = match classIndex with | -1 -> None | i -> Some p.[i]
                member _.regressionValue = match regressionIndex with | -1 -> None | i -> (p.[i] |> System.Double.tryParse) //Needs to be able to parse ints into floats
                member _.realAttributes = p |> Seq.filterWithIndex (fun i a -> realIndexes.Contains i) |>Seq.map System.Double.Parse |> Seq.toArray
                member _.categoricalAttributes = p |> Seq.filterWithIndex (fun i a -> categoricalIndexes.Contains i) |> Seq.toArray
                member this.distance (p:Point) (trainingDataSet:Point seq)= System.Math.Sqrt((Seq.zip this.realAttributes p.realAttributes|> Seq.sumBy (fun (a,b) -> (a-b)*(a-b)))**2.+(getCategoricalClassificationDistance this p trainingDataSet pValue)**2. + (getCategoricalRegressionDistance this p trainingDataSet pValue)**2. )
            }            
        ) |> Seq.toArray
        

    let processTrainingDataset (points:Point seq) = 
        let datasetCategoricalAttributesValues= (points |> Seq.map (fun p -> p.categoricalAttributes) |> Seq.transpose |>Seq.map (fun aList -> aList |> Seq.distinct|>Seq.toArray)|>Seq.toArray) 
        let datasetRealAttributeValues= (points|> Seq.map (fun p -> p.realAttributes) |> Seq.transpose |>Seq.map (fun aList -> aList |> Seq.distinct|>Seq.toArray)|>Seq.toArray)
        let datasetClasses= (points|>Seq.map (fun p -> p.cls)|>Seq.distinct|>Seq.choose id|>Seq.toArray)
        {|datasetCategoricalAttributesValues=datasetCategoricalAttributesValues;datasetRealAttributeValues=datasetRealAttributeValues;datasetClasses=datasetClasses|}


        
        
    //// Loss Functions
        
    let zeroOneLoss (x:(string*string) seq) = //This implements 0-1 loss which we found was accurate enough for our analyses for classification problems
        x                                   //take the tuples..
        |>Seq.averageBy (function           //and take the average of the following matches 
            | a,b when a=b -> 0.            //if both parts of the tuple are the same, then there is an error of 0 (no error)
            | _ -> 1.                       //otherwise there is an error of 1 (lots of error)
        )                                   //effectively we are computing the percentage of the classifications that are wrong in the validation set

    let meanSquaredError (x:(float*float) seq) = //This implements MSE loss which we found was accurate enough for our analyses for regression problems
        x                                        //take the tuples
        |>Seq.averageBy (fun (a,b) -> (a-b)**2.)  //and average them all by squaring the difference of their parts
    ////




    // K-nearest Neighbor (KNN)
    // this makes this function only visible inside the defining module
    let private kNearestNeighborClassificationImpl k (trainingSet:Point seq) (p:Point) =
        trainingSet
        |> Seq.sortBy (fun tp -> tp.distance p trainingSet)
        |> Seq.take k
        |> Seq.map (fun tp -> tp.cls)
        |> Seq.countBy id
        |> Seq.maxBy snd
        |> fst
    
    let private kNearestNeighborRegressionImpl k (trainingSet:Point seq) (p:Point) =
        trainingSet
        |> Seq.sortBy (fun tp -> tp.distance p trainingSet)
        |> Seq.take k
        |> Seq.map (fun tp -> tp.regressionValue)
        |> Seq.map (fun v -> match v with |None -> 0. | Some v -> v)
        |> Seq.average
    

    // Function to classify and regress points via KNN
    let kNearestNeighborClassification k (trainingSet:Point seq) =
        let trainingDatasetProperties = processTrainingDataset (trainingSet |> Seq.cast)
        { new Classifier with
            member __.classify p = kNearestNeighborClassificationImpl k trainingSet p
        }

    type KNearestNeighborClassification (k,trainingSet:Point[]) =
        member __.classify (p:Point) = kNearestNeighborClassificationImpl k trainingSet p
        interface Classifier with
            member __.classify p = __.classify p


    let kNearestNeighborRegression k (trainingSet:Point seq) =
        let trainingDatasetProperties = processTrainingDataset (trainingSet |> Seq.cast)
        { new Regresser with
            member __.regress p = kNearestNeighborRegressionImpl k trainingSet p
        }

    type KNearestNeighborRegression (k,trainingSet:Point[]) =
        member __.regress (p:Point) = kNearestNeighborRegressionImpl k trainingSet p
        interface Regresser with
            member __.regress p = __.regress p


    // Condensed Nearest Neighbor (CNN)
    // Implementation of Condensed Nearest Neighbor
    let private createCondensedKNearestNeighborSet k (dataSet:Point seq) =
        let dataSet = ResizeArray(dataSet)
        let results = ResizeArray()
        let rec loop i =
            if i<dataSet.Count then 
                let p = dataSet.[i]
                let computedClass = 
                    dataSet
                    |> Seq.sortBy (fun tp -> tp.distance p dataSet)
                    |> Seq.take k
                    |> Seq.map (fun tp -> tp.cls.Value)
                    |> Seq.countBy id
                    |> Seq.maxBy snd
                    |> fst
                if computedClass <> p.cls.Value then 
                    dataSet.RemoveAt(i)
                    loop i
                else 
                    results.Add(p)
                    loop (i+1)
            else 
                results:>_ seq
        loop 0
        
    // Edited Nearest Neighbor (ENN)
    // Implementation of Edited Nearest Neighbor
    let createKEditedNearestNeighbor k (trainingSet:Point[]) =
        trainingSet                                 // take the trainingSet
        |> Seq.mapi ( fun i point ->                // take a point and the index of said point
            (kNearestNeighborClassification k (     // we are making a kNNclassifier
                trainingSet                         // using the trainingSet                        //we are taking out the point we are looking at...
                |> Seq.mapi (fun i x -> (x,i))      // make each point into a point and it's index
                |> Seq.filter (fun (_,j) -> j<>i)   // make sure the index you are looking at is not our input point
                |> Seq.map (fun (x,_) -> x)         // take the (point,index) tuple and return just the point
                |> Seq.toArray)                     // then we make this whole ugly thing into an array again
                ).classify point                    // then we classify the point in question
            ) 
        |> Seq.mapi (fun i x -> (x,i))              // make a tuple of our point and it's index (this is so we can grab the original point's class
        |> Seq.filter (fun (x,i) ->                 // take the tuple...
            x <> trainingSet.[i].cls         // filter out points that are falsely classified
        )
        |> Seq.map (fun (_,i) -> trainingSet.[i])   // return the points from the original trainingSet

       
open Project2

///////////////////////////
//// k-fold

let getRandomFolds k (dataSet:'a seq) = //k is the number of slices dataset is the unsliced dataset
    let rnd = System.Random()           //init randomnumbergenerator
    let data = ResizeArray(dataSet)     //convert our dataset to a resizable array
    let getRandomElement() =            //Get a random element out of data
        if data.Count <= 0 then None    //if our data is empty return nothing
        else
            let idx = rnd.Next(0,data.Count)    //get a random index between 0 and |data|
            let e = data.[idx]                  //get the element e from idx
            data.RemoveAt(idx) |> ignore        //remove the element e from data
            Some e                              //return e
    let folds = Array.init k (fun _ -> Seq.empty)       //resultant folds array init as an empty seq
    let rec generate  j =                               //recursively generate an array that increments on j (think like a while loop)
        match getRandomElement() with                   //match the random element with:
        | None -> folds                                 //if there is nothing there then return folds
        | Some e ->                                     // if there is something there
            let s = folds.[j%k]                         // get the (j%k)th fold  in the array
            folds.[j%k] <- seq { yield! s; yield e }    //create a new seqence containing the old sequence (at j%k) and the new element e, and put it back into slot (j%k)
            generate (j+1)                              //increment j and run again
    generate 0                                          //calls the generate function

let applyKFold (trainingSet:Point seq) (validationSet: Point seq) (preprocessFunction: (Point seq -> Point seq) option) lossFunction classification_regressionFunction =   //apply the loss function (MSE) to the kth fold
    (match preprocessFunction with |Some f -> (f validationSet) |_ -> validationSet)//preprocess our validation set if we have a preprocessing function, otherwise use our validation set
    |> Seq.map (fun x -> (classification_regressionFunction trainingSet x))         //grab each element out of it and run it as the "sample" in our classify function and pair the resultant class with the element's ACTUAL class in a tuple
    |> lossFunction                                                                 //run the loss algorithm with the sequence of class tuples
    //                                                                              //The result is a float: the % of elements that were guessed incorrectly

let doKFold k (dataSet:Point seq) (preprocessFunction: (Point seq -> Point seq) option) lossFunction classification_regressionFunction=           //This is where we do the k-folding algorithim this will return the average from all the kfolds
    let folds = getRandomFolds k dataSet    //Lets get the k folds randomly using the function above; this returns an array of Data seqences
    Seq.init k (fun k ->                    //We're initializing a new seq of size k using the lambda function "(fun k -> ...)"" to get the kth element
        let validationSet = folds.[k]       //The first part of our function we get the validation set by grabing the kth data Seq from folds
        let trainingSet =                   //The training set requires us to do a round-about filter due to the fact that seqences are never identical and we can't use a Seq.filter...
            folds                           //lets grab the array of data seqences
            |> Seq.mapi (fun i p -> (i,p))  //each seqence in the array is mapped to a tuple with the index of the sequence as "(index,sequence)"
            |> Seq.filter(fun (i,_) -> i<>k)//now we will filter out the seqence that has the index of k
            |> Seq.collect snd              //now we grab the seqence from the tuple
        applyKFold trainingSet validationSet preprocessFunction lossFunction classification_regressionFunction 
    )   //Finally lets apply our function above "applyKFold" to our training set and validation set using our preproccess function, lossfunction, and algorithm
    //|> Seq.average                          //the result is a seq of floats so we'll just get the average our % failuresto give us a result to our k-fold analysis as the accuracy of our algorithm
    //Currently an issue 
///////////////////////////



let ds = (fullDataset "D:\Fall2019\Machine Learning\Project 2\Data\machine.data" None (Some 9) 2.)

let classifier1 = kNearestNeighborClassification 2 ds
let classifier2 = KNearestNeighborClassification(2,ds)
let regressor1 = kNearestNeighborRegression 2 ds

 

let p=ds.[1]
 

classifier1.classify ds.[1]
classifier2.classify p
regressor1.regress p

// END OF CODE