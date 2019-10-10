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
        abstract member distance: p:Point -> trainingDataSet:Point[]-> float      //sqrt((Real distance)^2+(CategoricalClassification distance)^2+(CategoricalRegression distance)^2)


    //type Point =
    //    abstract member RealAttributes: float[]
    //    abstract member CategoricalAttributes: float[]
    //    abstract member distance: p:Point -> float

    // Interface for a point with a given value (inheirits from a Point object)
    type ValuePoint =
        inherit Point
        abstract member getValue: unit -> float

     // Interface for a point with a designated classification (inheirits from a Point object)
    type ClassifiedPoint =
        inherit Point
        abstract member getClass: unit -> string

 
    // Class for a Classification process output
    type Classifier =
        abstract member classify: p:Point -> string

    // Class for a Regression process output
    type Regresser =
        abstract member regress: p:Point -> float

    ///////////////////////////
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
        

    let getCategoricalClassificationDistance (point:Point) (target: Point) (trainingSet:Point seq) p= 
        point.categoricalAttributes
        |> Seq.mapi (fun i a -> classificationPercent (trainingSet|>Seq.toArray) (i,a) target.categoricalAttributes.[i] p)
        |> Seq.sum 
        
    let trainingDataset filename (classIndex:int option) (regressionIndex : int option) (pValue:float)= 
        let classIndex,regressionIndex = 
            match classIndex,regressionIndex with 
            | None,None     -> -1,-1
            | None,Some a   -> -1,a 
            | Some a,None   -> a,-1
            | Some a,Some b -> a,b
        fetchTrainingSet filename true false
        |> Seq.map (fun p -> 
            {new Point with 
                member _.cls = match classIndex with | -1 -> None | i -> Some p.[i]
                member _.regressionValue = match regressionIndex with | -1 -> None | i -> (p.[i] |> System.Double.tryParse) //Needs to be able to parse ints into floats
                member _.realAttributes = p |> Seq.filterWithIndex (fun i a -> i <> regressionIndex && i <> classIndex) |>Seq.choose System.Double.tryParse |> Seq.toArray
                member _.categoricalAttributes = p |> Seq.filterWithIndex (fun i a -> i <> regressionIndex && i <> classIndex && (System.Double.tryParse a)=None) |> Seq.toArray
                member this.distance (p:Point) (trainingDataSet:Point[])= System.Math.Sqrt((Seq.zip this.realAttributes p.realAttributes|> Seq.sumBy (fun (a,b) -> (a-b)*(a-b)))+(getCategoricalClassificationDistance this p trainingDataSet pValue) + (getCategoricalRegressionDistance this p trainingDataSet pValue) )
            }            
        ) |> Seq.toArray
        

    let processTrainingDataset (points:Point seq) = 
        let datasetCategoricalAttributesValues= (points |> Seq.map (fun p -> p.categoricalAttributes) |> Seq.transpose |>Seq.map (fun aList -> aList |> Seq.distinct|>Seq.toArray)|>Seq.toArray) 
        let datasetRealAttributeValues= (points|> Seq.map (fun p -> p.realAttributes) |> Seq.transpose |>Seq.map (fun aList -> aList |> Seq.distinct|>Seq.toArray)|>Seq.toArray)
        let datasetClasses= (points|>Seq.map (fun p -> p.cls)|>Seq.distinct|>Seq.choose id|>Seq.toArray)
        {|datasetCategoricalAttributesValues=datasetCategoricalAttributesValues;datasetRealAttributeValues=datasetRealAttributeValues;datasetClasses=datasetClasses|}

    (trainingDataset "D:\Fall2019\Machine Learning\Project 2\Data\machine.data" None (Some 9) 2. |> Seq.head)|> (fun x -> x.cls,x.regressionValue,x.realAttributes,x.categoricalAttributes)

    // K-nearest Neighbor (KNN)
    // this make this function only visible inside the defining module
    let private kNearestNeighborClassificationImpl k (trainingSet:ClassifiedPoint seq) (p:Point) =
        trainingSet
        |> Seq.sortBy (fun tp -> tp.distance p)
        |> Seq.take k
        |> Seq.map (fun tp -> tp.getClass())
        |> Seq.countBy id
        |> Seq.maxBy snd
        |> fst
    
    

    // Function to classify points via KNN
    let kNearestNeighborClassification k (trainingSet:ClassifiedPoint seq) =
        let trainingDatasetProperties = processTrainingDataset (trainingSet |> Seq.cast)
        { new Classifier with
            member __.classify p = kNearestNeighborClassificationImpl k trainingSet p
        }

    type KNearestNeighborClassification (k,trainingSet:ClassifiedPoint[]) =
        member __.classify (p:Point) = kNearestNeighborClassificationImpl k trainingSet p
        interface Classifier with
            member __.classify p = __.classify p

    // Edited Nearest Neighbor (ENN)
    // Implementation of Edited Nearest Neighbor
    let private condensedNearestNeighborClassificationImpl k (trainingSet:ClassifiedPoint[]) (samplePoints:ClassifiedPoint[]) =
        samplePoints
        |> Seq.map (fun p ->
            trainingSet
            |> Seq.sortBy (fun tp -> tp.distance p)
            |> Seq.take k
            |> Seq.map (fun tp -> tp.getClass())
            |> Seq.countBy id
            |> Seq.maxBy snd
            |> (fun (x,_) -> if p.getClass() = x then Some x else None)
        )
        |> Seq.filter (fun (x:string Option) ->  x.IsSome)
        |> Seq.map (fun x -> x.Value)
        |> Seq.toList

    let editedNearestNeighborClassificationImpl k (trainingSet:ClassifiedPoint[]) =
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
            x <> trainingSet.[i].getClass()         // filter out points that are falsely classified
        )
        |> Seq.map (fun (_,i) -> trainingSet.[i])   // return the points from the original trainingSet

        


        
        //|> match trainingSet with
        //    | X.[i] -> Seq.append
        //    | _ -> []
        // @TODO: Need to find a way to match the point with its actual class and KNN class, then remove it 
        // Thinking of using a match statement, syntax does not line up so I may have to try something else (Chris)

    // Function to classify points via ENN
    let editedNearestNeighborClassification k (trainingSet:ClassifiedPoint[]) =
        { new Classifier with
            member __.classify p = editedNearestNeighborClassificationImpl k trainingSet p
        }


    // Condensed Nearest Neighbor (CNN)
    // Implementation of Condensed Nearest Neighbor
    let private condensedNearestNeighborClassificationImpl k (trainingSet:ClassifiedPoint[]) (p:Point) =
        let returnArray = ResizeArray()
         trainingSet
         |> Seq.sortBy (fun tp -> tp.distance p)
         |> Seq.take k
         |> Seq.map (fun tp -> tp.getClass())
         |> Seq.countBy id
         |> Seq.maxBy snd
         |> (fun (x,_) -> if p.getClass() = x then returnArray.Add(x))

    // Function to classify points via CNN 
    let condensedNearestNeighborClassification k (trainingSet:ClassifiedPoint[]) =
        { new Classifier with
            member __.classify p = condensedNearestNeighborClassificationImpl k trainingSet p
        }


    let point rAttributes cAttributes=
        { new Point with
            member __.RealAttributes = rAttributes
            member __.CategoricalAttributes= cAttributes
            member __.distance (p:Point) =
                Seq.zip rAttributes p.RealAttributes
                |> Seq.sumBy (fun (a,b) -> (a-b)*(a-b))
                |> sqrt            
        }

 

    let classPoint cls rAttributes cAttributes =
        { new ClassifiedPoint with
            member __.getClass() = cls
            member __.RealAttributes = rAttributes
            member __.CategoricalAttributes= cAttributes
            member __.distance (p:Point) =
                Seq.zip rAttributes p.RealAttributes
                |> Seq.sumBy (fun (a,b) -> (a-b)*(a-b))
                |> sqrt            
        }

 


    type Point' =
        {
            attributes: float[]
        }
        interface Point with
            member __.Attributes = __.attributes
            member __.distance (p:Point) =
                Seq.zip __.attributes p.Attributes
                |> Seq.sumBy (fun (a,b) -> (a-b)*(a-b))
                |> sqrt
        static member New([<System.ParamArray>]attributes:float[]) = { attributes = attributes } |> unbox<Point>

 

    type ClassifiedPoint' =
        {
            point: Point'
            cls: string
        }
        interface ClassifiedPoint with
            member __.getClass() = __.cls
        interface Point with
            member __.Attributes = __.point.attributes
            member __.distance (p:Point) = p.distance(__.point)
        static member New(cls,[<System.ParamArray>]attributes:float[]) = { point = { attributes = attributes }; cls = cls } |> unbox<ClassifiedPoint>

 

open Project2

 

let trainingSet = [|ClassifiedPoint'.New("a",1.,1.);ClassifiedPoint'.New("b",0.,0.)|]
let trainingSet2 = [|classPoint "a" [|1.;1.|];classPoint "b" [|0.;0.|]|]

 

let classifier1 = kNearestNeighborClassification 2 trainingSet
let classifier2 = KNearestNeighborClassification(2,trainingSet)

 

let classifier3 = kNearestNeighborClassification 2 trainingSet2

 

let p = Point'.New(0.5,0.5)

 

classifier1.classify p
classifier2.classify p
classifier3.classify p

// END OF CODE