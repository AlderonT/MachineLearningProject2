//--------------------------------------------------------------------------------------------------------------------------
// CSCI447 - Fall 2019
// Assignment #2
// Allen Simpson;
// 
// This file is currently being included to provide an example of how memoization *Can* be implemented
//--------------------------------------------------------------------------------------------------------------------------


////This was an experiment to see if we could speed up the preformance by using memoization
////but due to complexity we did not want to put it into the main code base
////Please do NOT grade this for the votes file
////this is only here to show an alternative approach   

////Type Definitions:
//Classification types
type Class =
    | Democrat
    | Republican

//Data format for sample data
type Data ={
    cls:Class
    handicappedinfants:int 
    waterprojectcostsharing:int 
    adoptionofthebudgetresolution:int
    physicianfeefreeze:int
    elsalvadoraid:int
    religiousgroupsinschools:int
    antisatellitetestban:int
    aidtonicaraguancontras:int
    mxmissile:int
    immigration:int
    synfuelscorporationcutback:int
    educationspending:int
    superfundrighttosue:int
    crime:int
    dutyfreeexports:int
    exportadministrationactsouthafrica:int
} 

//type alias for the training set
type DataSet = System.Guid *(Data seq)
////

////Functions:

//Implements memoization (a way to trade speed for storage)
let memoize (key: 'a -> 'key) (f: 'a -> 'b) = 
    let memo = System.Collections.Generic.Dictionary()  // make a dictionary
    fun (a:'a) ->                                       // get an arguement a
        let key = key a                                 // get the key of a
        match memo.TryGetValue key  with                // look up the key
        | false,_ ->                                    // if you can't find it compute f(a)
            let v = f a                                 // and remember it
            memo.[key]<-v 
            v 
        | true,v -> v                                   // otherwise return the remembered value
        

//#{pred} = the count of elements in the set that pred is true
//Implements #{pred}
let filteredCount pred =    //-- Now With Memoization
    memoize fst (fun (_,s) ->
        s |> Seq.filter pred |> Seq.length
    )

//Implements Q (C=ci) = #{pred}/N // Finds the percentage of elements in the data set that fall into class "cls"  -- Now with memoization
let Q =
    memoize fst (fun dataSet -> //fst takes a 2-tuple and returns the first element
        memoize id (fun cls ->  
            let classCount = filteredCount (fun x -> x.cls = cls)
            (float (classCount dataSet))/(float (dataSet|>snd|>Seq.length))  //snd takes a 2-tuple and returns the second element
        )
    )
   
    

//Implements F (Aj=ak,C=ci) = #{(xaj=ak)&(x in ci)}+1/N+d
//Finds the likeliness that a certain attribute "Aj" has the value ak and fall into class "cls" -- Now With Memoization cuts time by 99.4%
let F =
    memoize fst (fun dataSet ->
        //printfn "F dataSet key: %A" (fst dataSet)
        memoize id (fun cls ->         
            //printfn "F cls key: %A" (cls)
            let Nc = filteredCount (fun x -> x.cls = cls) dataSet //gets the number of elements that fall into class "cls"             
            let pred Aj ak (x:Data)   = (Aj x = ak) && (x.cls = cls) // determines the predicate of the F function
            memoize fst (fun (key,Aj)->
                memoize id (fun ak ->
                    //printfn "F ajak key: %A" (key)
                    let topTerm = (float ((filteredCount (pred Aj ak) dataSet)+1))
                    (fun d ->
                         topTerm/(float (Nc + d)) // executes the function F
                    )
                )
            )   
        )
    )

//Implements C(x) = Q(C=ci)*Product(F(Aj=ak,C=ci)) from j=1 to d
// Finds the likeliness that the sample data point is of the class "cls".
let C (dataSet:DataSet) (cls:Class) (sample:Data) = 
    //for more than one attribute, additional F parts will need to be added
    let d = 16   //number of attributes
    (Q dataSet cls)
    *(F dataSet cls (0,(fun x -> x.handicappedinfants)) sample.handicappedinfants d )
    *(F dataSet cls (1,(fun x -> x.waterprojectcostsharing)) sample.waterprojectcostsharing d)
    *(F dataSet cls (2,(fun x -> x.adoptionofthebudgetresolution)) sample.adoptionofthebudgetresolution d)
    *(F dataSet cls (3,(fun x -> x.physicianfeefreeze)) sample.physicianfeefreeze  d)
    *(F dataSet cls (4,(fun x -> x.elsalvadoraid)) sample.elsalvadoraid d)
    *(F dataSet cls (5,(fun x -> x.religiousgroupsinschools)) sample.religiousgroupsinschools d)
    *(F dataSet cls (6,(fun x -> x.antisatellitetestban)) sample.antisatellitetestban  d)
    *(F dataSet cls (7,(fun x -> x.aidtonicaraguancontras)) sample.aidtonicaraguancontras  d)
    *(F dataSet cls (8,(fun x -> x.mxmissile)) sample.mxmissile  d)
    *(F dataSet cls (9,(fun x -> x.immigration)) sample.immigration  d)
    *(F dataSet cls (10,(fun x -> x.synfuelscorporationcutback)) sample.synfuelscorporationcutback d)
    *(F dataSet cls (11,(fun x -> x.educationspending)) sample.educationspending  d)
    *(F dataSet cls (12,(fun x -> x.superfundrighttosue)) sample.superfundrighttosue d)
    *(F dataSet cls (13,(fun x -> x.crime)) sample.crime  d)
    *(F dataSet cls (14,(fun x -> x.dutyfreeexports)) sample.dutyfreeexports d)
    *(F dataSet cls (15,(fun x -> x.exportadministrationactsouthafrica)) sample.exportadministrationactsouthafrica  d)

    //Actually classifies a sample datapoint into a class.
let classify (dataSet:DataSet) (sample:Data) =
    [
        Class.Democrat // this should be a list of all possible classifications
        Class.Republican

    ]
    |> Seq.map (fun cls -> cls, C dataSet cls sample)   //maps the class to the likeliness
    //|> Seq.map (fun (cls,factor) -> printfn "class: %A factor: %A" cls factor; (cls,factor)) //Will print the likelihood of each class type (for debugging)
    |> Seq.maxBy (snd) // get the maximum based on the FACTOR only
    |> fst // return just the class (no factor)
////

/// //// Loss Function

let zeroOneLoss (x:(Class*Class) seq) =   //This is the MSE (Mean Square Error) loss function, it takes the number of elements (|validationSet| from below), and a sequence of (class*class) tuples
    x
    |>Seq.averageBy (function
        | a,b when a=b -> 0.
        | _ -> 1.
    )            //here we're just doing the MSE calculation 1/d*SUM((Yi-'Yi)^2; i=1; i->d)
    //in a nutshell this gets the % of classes that were guessed incorrectly therefore... ~(0 < result < 1) //You can get get 0.0 and 1.0 but the chance is incredibly low

////

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

let applyKFold (trainingSet:DataSet) (validationSet: Data seq) =   //apply the loss function (MSE) to the kth fold
    validationSet                                                   //take our validation set
    |> Seq.map (fun x -> (classify trainingSet x,x.cls))            //grab each element out of it and run it as the "sample" in our classify function and pair the resultant class with the element's ACTUAL class in a tuple
    |> zeroOneLoss                                                  //run the MSE algorithm with d = |validationSet| and the sequence of class tuples
    //                                                              //The result is a float: the % of elements that were guessed incorrectly

let doKFold k (dataSet:Data seq)=           //This is where we do the k-folding algorithim this will return the average from all the kfolds
    let folds = getRandomFolds k dataSet    //Lets get the k folds randomly using the function above; this returns an array of Data seqences
    Seq.init k (fun k ->                    //We're initializing a new seq of size k using the lambda function "(fun k -> ...)"" to get the kth element
        let validationSet = folds.[k]       //The first part of our function we get the validation set by grabing the kth data Seq from folds
        let trainingSet =                   //The training set requires us to do a round-about filter due to the fact that seqences are never identical and we can't use a Seq.filter...
            folds                           //lets grab the array of data seqences
            |> Seq.mapi (fun i f -> (i,f))  //each seqence in the array is mapped to a tuple with the index of the sequence as "(index,sequence)"
            |> Seq.filter(fun (i,_) -> i<>k)//now we will filter out the seqence that has the index of k
            |> Seq.collect snd              //now we grab the seqence from the tuple
            |> (fun s -> System.Guid.NewGuid(),s)
        applyKFold trainingSet validationSet//Finally lets apply our function above "applyKFold" to our training set and validation set
    )
    //|> Seq.mapi (fun i x -> printfn "i = %A loss: %A" i x; x)   //Just printing the % of failures for each subset (debuging code)  ////DEBUG Remove before submission
    |> Seq.average                          //the result is a seq of floats so we'll just get the average our % failuresto give us a result to our k-fold analysis as the accuracy of our algorithm

////
/// 
//Reads data and assigns to trainingDataSet:
let trainingDataSet =
    System.IO.File.ReadAllLines(@"D:\Fall2019\Machine Learning\Project 1\Data\5\house-votes-84.data") // this give you back a set of line from the file (replace with your directory)
    |> Seq.map (fun line -> line.Split(',') |> Array.map (fun value -> value.Trim())) // this give you an array of elements from the comma seperated fields. We trim to make sure that any white space is removed.
    //|> Seq.filter (Seq.exists(fun f -> f="?") >> not)   //This filters out all lines that contain a "?"
    |> Seq.map (fun fields ->   //This will map the lines to objects returning a seqence of datapoints (or a DataSet as defined above)
        {
            //id = fields.[0] |> System.Int32.Parse
            handicappedinfants = fields.[1] |> (function | "n" -> 0 | "y" -> 1| _ -> 2)
            waterprojectcostsharing = fields.[2] |> (function | "n" -> 0 | "y" -> 1| _ -> 2)
            adoptionofthebudgetresolution = fields.[3] |> (function | "n" -> 0 | "y" -> 1| _ -> 2) 
            physicianfeefreeze = fields.[4] |> (function | "n" -> 0 | "y" -> 1| _ -> 2) 
            elsalvadoraid = fields.[5] |> (function | "n" -> 0 | "y" -> 1| _ -> 2) 
            religiousgroupsinschools = fields.[6] |> (function | "n" -> 0 | "y" -> 1| _ -> 2) 
            antisatellitetestban = fields.[7] |> (function | "n" -> 0 | "y" -> 1| _ -> 2) 
            aidtonicaraguancontras = fields.[8] |> (function | "n" -> 0 | "y" -> 1| _ -> 2) 
            mxmissile = fields.[9] |> (function | "n" -> 0 | "y" -> 1| _ -> 2) 
            immigration = fields.[10] |> (function | "n" -> 0 | "y" -> 1| _ -> 2) 
            synfuelscorporationcutback = fields.[11] |> (function | "n" -> 0 | "y" -> 1| _ -> 2) 
            educationspending = fields.[12] |> (function | "n" -> 0 | "y" -> 1| _ -> 2)
            superfundrighttosue = fields.[13] |> (function | "n" -> 0 | "y" -> 1| _ -> 2) 
            crime = fields.[14] |> (function | "n" -> 0 | "y" -> 1| _ -> 2) 
            dutyfreeexports = fields.[15] |> (function | "n" -> 0 | "y" -> 1| _ -> 2) 
            exportadministrationactsouthafrica = fields.[16] |> (function | "n" -> 0 | "y" -> 1| _ -> 2)
            cls = fields.[0] |> (fun x -> 
                 match x with
                 | "democrat" -> Democrat // if democrat then Democrat
                 | "republican" -> Republican // ...
                 | _ -> Republican    // if it's anything else then make it a democrat (I need a default case)
             )
        }
    )
    |>Seq.cache //Due to the large number of datapoints we're caching the values so we don't redo the work above every time we look at trainingDataSet (saves ~1s)

let newShuffledTrainingDataSet () = 
    let shuffleAttributes () =                                                                  //This will generate a verson of the data that shuffles 10% of the attributes
        let workingData =                                                                       //We're getting the data we will work with
            System.IO.File.ReadAllLines(@"E:\Project 1\Data\5\house-votes-84.data")                      //get the data from file (yes this needs to match a directory that can read it)
            |> Seq.map (fun line -> line.Split(',') |> Array.map (fun value -> value.Trim()))   //split the lines on the commas
            |> Seq.map (fun sa ->                                                               //now we are taking each value and...
                let cls = sa.[0]                                                      //the last value gets to be a CLS
                let attribs = sa |> Seq.skip 1 |> Seq.toArray         //We take everything else, drop the first and last values and make the result into an array
                cls,attribs                                                                 //we are making a tuple of a tuple here 
            )
            |>Seq.toArray                                                                       //making the sequence into an array so we don't recalculate every time we call workingData

        let shuffle (data:string [] []) attr=                                       //this will shuffle the attribute "attr" in the string array data
            let mutable i = 0                                                       //we are doing this imperitvely as a show of force (this is how you make a mutable value)
            let attributes = ResizeArray (data |> Seq.map (fun xs -> xs.[attr]))    //we are making an array of the values from data's 'attr'th attribute array
            let rnd = System.Random()                                               //make our randomNumberGenerator
            while attributes.Count>0 do                                             //while we have attributes...
                let j = rnd.Next(0,attributes.Count)                                //get a random index in attributes
                let v = attributes.[j]                                              //assign v the value of the j'th attribute out of attributes
                attributes.RemoveAt(j)                                              //remove the j'th element from attributes
                data.[i].[attr] <- v                                                //replace the value of the i'th data point's 'attr'th attribute (this effects the actual value of data outside the function)
                i <- i+1                                                            //increment i

        
        let data = workingData |>Array.map snd                                                  //defining data as the attribute array from working Data
        let modifyCount = (workingData.[0]|> snd |> Array.length |> float)*0.1 |> ceil |> int   //this is the count of modifiable attributes (literally the length of attributes*0.1 rounded up)
        let attribsCount = (workingData.[0]|> snd |> Array.length)                              //this is the number of actual attributes
        let rnd = System.Random()                                                               //make a randomNumberGenerator
        let idxs = ResizeArray([0..(attribsCount-1)])                                           //we are making a mutable list of indicies 
        List.init modifyCount (fun _ ->                                                         //make a new list with magnitude of modify count (the number of elements we are shuffling)
            let j = rnd.Next(0,idxs.Count)                                                      //get a random index from idxs
            let i = idxs.[j]                                                                    //let i be the random index
            idxs.RemoveAt(j)                                                                    //we shall remove said index from idxs (so we don't choose it again)
            i                                                                                   //and add it to our list
        )                                                                                       ////This randomly chooses the attribute numbers we're going to shuffle
        |>Seq.iter (shuffle data)                                                               //we now iter through this list of indecies and shuffle the data at the index (This shuffling modifies the actual values of data)
        Seq.zip workingData data                                                                //then we make a tuple of the working data and the shuffled data
        |>Seq.map (fun ((cls,oldData),newData) ->                                          //Then we take the form ((string,string),string[],string[])
            seq {yield cls; yield! newData} |> String.concat "," )                     //and convert it into one long sequence of strings which we immediately concat with ','
    
    shuffleAttributes ()                                                                        //we start with the shuffled values this time
    |> Seq.map (fun line -> line.Split(',') |> Array.map (fun value -> value.Trim())) // this give you an array of elements from the comma seperated fields. We trim to make sure that any white space is removed.
    //|> Seq.filter (Seq.exists(fun f -> f="?") >> not)   //This filters out all lines that contain a "?"
    |> Seq.map (fun fields ->   //This will map the lines to objects returning a seqence of datapoints (or a DataSet as defined above)
        {
            //id = fields.[0] |> System.Int32.Parse
            handicappedinfants = fields.[1] |> (function | "n" -> 0 | "y" -> 1| _ -> 2)
            waterprojectcostsharing = fields.[2] |> (function | "n" -> 0 | "y" -> 1| _ -> 2)
            adoptionofthebudgetresolution = fields.[3] |> (function | "n" -> 0 | "y" -> 1| _ -> 2) 
            physicianfeefreeze = fields.[4] |> (function | "n" -> 0 | "y" -> 1| _ -> 2) 
            elsalvadoraid = fields.[5] |> (function | "n" -> 0 | "y" -> 1| _ -> 2) 
            religiousgroupsinschools = fields.[6] |> (function | "n" -> 0 | "y" -> 1| _ -> 2) 
            antisatellitetestban = fields.[7] |> (function | "n" -> 0 | "y" -> 1| _ -> 2) 
            aidtonicaraguancontras = fields.[8] |> (function | "n" -> 0 | "y" -> 1| _ -> 2) 
            mxmissile = fields.[9] |> (function | "n" -> 0 | "y" -> 1| _ -> 2) 
            immigration = fields.[10] |> (function | "n" -> 0 | "y" -> 1| _ -> 2) 
            synfuelscorporationcutback = fields.[11] |> (function | "n" -> 0 | "y" -> 1| _ -> 2) 
            educationspending = fields.[12] |> (function | "n" -> 0 | "y" -> 1| _ -> 2)
            superfundrighttosue = fields.[13] |> (function | "n" -> 0 | "y" -> 1| _ -> 2) 
            crime = fields.[14] |> (function | "n" -> 0 | "y" -> 1| _ -> 2) 
            dutyfreeexports = fields.[15] |> (function | "n" -> 0 | "y" -> 1| _ -> 2) 
            exportadministrationactsouthafrica = fields.[16] |> (function | "n" -> 0 | "y" -> 1| _ -> 2)
            cls = fields.[0] |> (fun x -> 
                 match x with
                 | "democrat" -> Democrat // if democrat then Democrat
                 | "republican" -> Republican // ...
                 | _ -> Republican    // if it's anything else then make it a democrat (I need a default case)
             )
        }
    )
    |>Seq.cache //Due to the large number of datapoints we're caching the values so we don't redo the work above every time we look at trainingDataSet (saves ~1s)


let sw = System.Diagnostics.Stopwatch.StartNew ()
Seq.init 100 (fun k -> printfn "Working on %d..." (k+1); doKFold 10  trainingDataSet)
|>Seq.average
|>printfn "Average Loss: %f"
sw.Stop()
printfn "%A" sw.Elapsed

//Average error: 10.0229% +/- ~0.005%
//time: 00:00:13.3191 


sw.Start ()
Seq.init 100 (fun k -> printfn "Working on %d..." (k+1); doKFold 10 (newShuffledTrainingDataSet ()))
|>Seq.average
|>printfn "Average Loss: %f"
sw.Stop()
printfn "%A" sw.Elapsed
 
//Average error: 10.0911% +/- ~0.06%
//time: 00:00:42.2608
