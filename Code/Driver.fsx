module Project2 =

 

    type Point =
        abstract member Attributes: float[]
        abstract member distance: p:Point -> float

 

    type ValuePoint =
        inherit Point
        abstract member getValue: unit -> float

 

    type ClassifiedPoint =
        inherit Point
        abstract member getClass: unit -> string

 

    type Classifier =
        abstract member classify: p:Point -> string

 

    type Regresser =
        abstract member regress: p:Point -> float

 

    // this make this function only visible inside the defining module
    let private kNearestNeighborClassificationImpl k (trainingSet:ClassifiedPoint[]) (p:Point) =
        trainingSet
        |> Seq.sortBy (fun tp -> tp.distance p)
        |> Seq.take k
        |> Seq.map (fun tp -> tp.getClass())
        |> Seq.countBy id
        |> Seq.maxBy snd
        |> fst
    

    let kNearestNeighborClassification k (trainingSet:ClassifiedPoint[]) =
        { new Classifier with
            member __.classify p = kNearestNeighborClassificationImpl k trainingSet p
        }

    // Implementation of Edited Nearest Neighbor (ENN)
    //let private editedNearestNeighborClassificationImpl k (trainingSet:ClassifiedPoint[]) (p:Point) =
        //trainingSet
        //|> Seq.sortBy (fun tp -> tp.distance p)
        //|> Seq.take k
        //|> Seq.map (fun tp -> tp.getClass())
        //|> Seq.countBy id
        //|> Seq.maxBy snd
        //|> fst


    type KNearestNeighborClassification (k,trainingSet:ClassifiedPoint[]) =
        member __.classify (p:Point) = kNearestNeighborClassificationImpl k trainingSet p
        interface Classifier with
            member __.classify p = __.classify p

 


    let point attributes =
        { new Point with
            member __.Attributes = attributes
            member __.distance (p:Point) =
                Seq.zip attributes p.Attributes
                |> Seq.sumBy (fun (a,b) -> (a-b)*(a-b))
                |> sqrt            
        }

 

    let classPoint cls attributes =
        { new ClassifiedPoint with
            member __.getClass() = cls
            member __.Attributes = attributes
            member __.distance (p:Point) =
                Seq.zip attributes p.Attributes
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