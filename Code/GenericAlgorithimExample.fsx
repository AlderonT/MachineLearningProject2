//--------------------------------------------------------------------------------------------------------------------------
// CSCI447 - Fall 2019
// Assignment #2
// Allen Simpson
// 
// This file is included to show an example of how to write functions generically
//--------------------------------------------------------------------------------------------------------------------------

// Type Definitions:
//--------------------------------------------------------------------------------------------------------------------------

// Classification types (Class)
type Class = string

// Data format for sample data (Attribute)
type Data = {
    uid         : string option     //some datasets have id values so we have this as an optional value
    cls         : string            //we are taking the strings as the classes
    attributes  : string []         //we are compiling all attributes into a list of the string values //This works only because our data is generally discrete
} 

// Type alias for the training set
type DataSet = Data seq

// Functions:
//--------------------------------------------------------------------------------------------------------------------------

// Implement #{pred}, the representation of the number of elements in the set where pred is true
let filteredCount pred (s:'a seq) = s |> Seq.filter pred |> Seq.length

// Implements Q (C = ci) = #{pred} / N, the percentage of elements in the data set that fall into class "cls"
let Q (dataSet:DataSet) cls = 
    (float (filteredCount (fun x -> x.cls = cls) dataSet)) / (float (dataSet|>Seq.length))

// Implements F (Aj = ak, C = ci) = #{(xaj = ak) & (x in ci)} + 1 / N + d
// Finds the likeliness that a certain attribute "Aj" has the value ak and fall into class "cls" 
let F (dataSet:DataSet) d Aj ak cls =
    let Nc = filteredCount (fun x -> x.cls = cls) dataSet           //gets the number of elements that fall into class "cls" 
    
    let pred (x:Data) = (Aj x = ak) && (x.cls = cls)                // determines the predicate of the F function

    (float ((filteredCount pred dataSet)+1)) / (float (Nc + d))     // executes the function F

// Implements C(x) = Q(C=ci)*Product(F(Aj=ak,C=ci)) from j=1 to d
// Finds the likeliness that the sample data point is of the class "cls".
let C (dataSet:DataSet) (cls:Class) (sample:Data) = 
    
    let d = sample.attributes.Length                                            //number of attributes

    let q = Q dataSet cls                                                       //q is the Q part of our function
    sample.attributes                                                           //for all attributes in the sample...
    |>Seq.mapi  (fun attributeIdx attributeValue ->                             //go through the attributes with i being the index for the attribute
        F dataSet d (fun x -> x.attributes.[attributeIdx]) attributeValue cls   //run F on the dataset. The lambda takes returns the value of the 
    )                                                                           //attribute from the dataset and we compare it with the attribute value we're looking for 
    |>Seq.fold (*) q                                                            //We are doing a product (that is the fold (*)) of the resultant sequence and starting the 
                                                                                //Product with q // q*F1*F2...*Fd

// Function to classify a data point given.
let classify classes (dataSet:DataSet) (sample:Data) =
    classes    
    |> Seq.map (fun cls -> cls, C dataSet cls sample)   //maps the class to the likeliness
    |> Seq.maxBy (snd) // get the maximum based on the FACTOR only
    |> fst // return just the class (no factor)
////    

//// Loss Function

let zeroOneLoss (x:(Class*Class) seq) = //This implements 0-1 loss which we found was accurate enough for our analyses
    x                                   //take the tuple..
    |>Seq.averageBy (function           //and take the average of the following matches 
        | a,b when a=b -> 0.            //if both parts of the tuple are the same, then there is an error of 0 (no error)
        | _ -> 1.                       //otherwise there is an error of 1 (lots of error)
    )                                   //effectively we are computing the percentage of the classifications that are wrong in the validation set

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

let applyKFold (trainingSet:Data seq) (validationSet: Data seq) =   //apply the loss function (MSE) to the kth fold
    let classes =
        Seq.append trainingSet validationSet
        |> Seq.map (fun x -> x.cls)
        |> Seq.distinct
    validationSet                                                   //take our validation set
    |> Seq.map (fun x -> (classify classes trainingSet x,x.cls))    //grab each element out of it and run it as the "sample" in our classify function and pair the resultant class with the element's ACTUAL class in a tuple
    |> zeroOneLoss                                                  //run the 0-1 loss algorithm with the sequence of class tuples
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
        applyKFold trainingSet validationSet//Finally lets apply our function above "applyKFold" to our training set and validation set
    )
    //|> Seq.mapi (fun i x -> printfn "i = %A loss: %A" i x; x)   //Just printing the % of failures for each subset (debuging code)  ////DEBUG Remove before submission
    |> Seq.average                          //the result is a seq of floats so we'll just get the average our % failuresto give us a result to our k-fold analysis as the accuracy of our algorithm



let normalizeData (idIndex:int option) (clsIndex:int) (workingLines:string seq)=                                                               
    workingLines                                                                            //get the data from file (yes this needs to match a directory that can read it)
    |> Seq.map (fun line -> line.Split(',') |> Array.map (fun value -> value.Trim()))       //split the lines on the commas
    |> Seq.map (fun sa ->                                                                   //now we are taking each value and...
        match idIndex with                                                                  //figure out if idIndex is defined
        | None ->                                                                           //if idIndex is None then there is no id column...
            let cls = sa.[clsIndex]                                                         //the clsIndex'th value gets to be a CLS
            let attribs =                                                                   //we set the attributes by...
                sa                                                                          //taking the sequence of datapoints
                |> Seq.mapi (fun i v -> (i,v) )                                             //giving each an index
                |> Seq.choose (fun (i,v) -> if i <> clsIndex then Some v else None )        //we are keeping the values whose index do not match the clsIndex 
                |> Seq.toArray                                                              //and making it a string array again
            (None,cls),attribs                                                              //we are making a tuple of a tuple here
        | Some idIndex ->                                                                   //if idIndex does have a value, then there is a column in the data that has an id
            let i = sa.[idIndex] |> Some                                                    //the idIndex'th value gets to be an ID
            let cls = sa.[clsIndex]                                                         //the clsIndex'th value gets to be a CLS
            let attribs =                                                                   //so we do as above...
                sa 
                |> Seq.mapi (fun i v -> (i,v) ) 
                |> Seq.choose (fun (i,v) -> if i <> idIndex && i <> clsIndex then Some v else None ) //but here we choose values whose index do not match either the idIndex or clsIndex
                |> Seq.toArray                                                              //convert it to an array of strings again
            (i,cls),attribs                                                                 //we are making a tuple of a tuple here          
        
         
    )
    |>Seq.toArray                                                                       //making the sequence into an array so we don't recalculate every time we call workingData


let shuffleAttributes (idIndex:int option) (clsIndex:int) (workingLines:string seq)=                                                                  //This will generate a verson of the data that shuffles 10% of the attributes
    let workingData = normalizeData idIndex clsIndex workingLines               //We're getting the data we will work with
    

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
    |>Seq.map (fun (((i, cls),oldData),newData) -> ((i,cls),newData))
    |>Seq.toArray
        




let private trainingDataSetForRealValuedAttributes f n file (filterQuestionMarks:bool) idIndex clsIndex =  //This is a variation on the trainingDataSet that we made to accomidate for continuous values for the attributes 
    let data =
        System.IO.File.ReadAllLines(file)                               // this give you back a set of line from the file (replace with your directory)
        |> f idIndex clsIndex                                           //f is either normalizeData or shuffleAttributes (this allows us to compose the functions without having to write this twice)
        |> Seq.filter (fun (_,attributes) ->                            //after applying the function, we filter out the datapoints that have questionmarks if filterQuestionMarks is true...
            if filterQuestionMarks then 
                (attributes |> Seq.exists(fun f -> f="?") |> not) 
            else true 
            )                                           
        |> Seq.map (fun ((i,cls),attrs) ->                                   //here we are taking each array of strings (the attributes)
            let attrs = attrs |> Seq.map System.Double.Parse |> Seq.toArray  //take the array, skip the first and take all but the last, parse each value as a double and make the result into an array
            i,cls,attrs                                                      //Then make a 3-tuple of the id,class,and attribute array
        )
    let splitIntoDivisions n xs =       //This creates a classifier function that takes an attribute seq and splits it into n divisions | result is a float -> int lambda
        let min = xs |> Seq.min         //get the minimum attribute value
        let max = xs |> Seq.max         //get the maximum attribute value
        let width = (max-min)/(float n) //get the width of each class
        fun v -> int ((v-min)/width)    //return a function that takes a float v and returns the division it lies in

    let p = 
        data                            //take the data (idx,cls,attribs[])
        |> Seq.map (fun (_,_,xs)->xs)   //get the attribs
        |> Seq.head                     //get the first set of attribs
        |> Seq.length                   //and find out how many there are (they should all be the same so p is a constant)

    let funcs =                                 //here we are making an array of functions for each attribute that will divide said attribute into n divisions
        Array.init p (fun i ->                  //so first make an array of size p (the number of attributes)
            data                                //take our data...
            |> Seq.map (fun (_,_,xs)-> xs.[i])  //and extract out the ith attribute from all our datapoints into a single sequence
            |> splitIntoDivisions n             //then have splitIntoDivisions generage a function for the ith attribute set of values 
        )

    data                                                        //Now take our data (idx,cls,attribs[])
    |> Seq.map (fun (i,cls,xs) ->                               //create our Data class by doing the following:
        {
            uid = i                                                         //ID is i 
            cls = cls                                                       
            attributes = xs|>Array.mapi (fun i x -> funcs.[i] x |> string)  //this converts the divisions (int[]) back into a string array
        }                                                                   //so it can be used by the normal algorithm
    )
    |>Seq.toArray                                                           //making it an array so we don't recompute the value of this function every time we reference it

    
//Composed versions of the function above with the type of data we're using:

let normaltrainingDataSetForRealValuedAttributes =  
    trainingDataSetForRealValuedAttributes normalizeData

    
let shuffledtrainingDataSetForRealValuedAttributes =  
    trainingDataSetForRealValuedAttributes shuffleAttributes

//This gives us the normal data values for non-continuous datasets
let private trainingDataSet f file (filterQuestionMarks:bool) idIndex clsIndex =                                                                          //This is a variation on the trainingDataSet that we made to accomidate for continuous values for the attributes 
    let data =
        System.IO.File.ReadAllLines(file)                              // this give you back a set of line from the file (replace with your directory)
        |> f idIndex clsIndex                                          // fun the f function
        |> Seq.filter (fun (_,attributes) ->                           // filter the question marks if we should... 
            if filterQuestionMarks then 
                (attributes |> Seq.exists(fun f -> f="?") |> not) 
            else true 
            )                                           
        
    data                                                            //Now take our data (idx,cls,attribs[])
    |> Seq.map (fun ((i,cls),xs) ->                                 //create our Data class by doing the following:
        {
            uid = i                                                 //uid is i 
            cls = cls                                               //cls is cls
            attributes = xs                                         //and attributes is just the string [] xs
        }
    )



//Composed versions of the function above with the type of data we're using:

let normaltrainingDataSet =  
    trainingDataSet normalizeData

    
let shuffledtrainingDataSet =  
    trainingDataSet shuffleAttributes
