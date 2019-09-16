Identify classes and attributes of each dataset 
1. ~~Abalone~~  ; 1st "Sex" ; all other attributes
2. ~~Car~~ ; 7th attribute "Safety"; all other attributes discreate
3. ForestFires ; can't find the class; apparently all are attributes 
4. ~~Machine~~ ; 9th attribute "PRP" int ; all other attributes
5. ~~Segmentation~~ ; 1st value ; all other attributes (we'll need to clear the top 4 lines before reading data)
6. ~~WineQuality~~ ; 12th attribute "Quality" int ; all other attributes

Assign algorithms to people:
1. k-nearest neighbor
2. edited k-nearest neighbor
3. condensed k-nearest neighbor
4. k-means
5. k-means clustering (use the cluster centroids as a reduced dataset for k-NN (1))

Split datasets into the following format: ((uuid:System.Guid, cls:String),attributes[]); 
attributes may make the most sense as a float seq but we don't know yet.
1. Abalone
2. Car
3. ForestFires 
4. Machine
5. Segmentation
6. WineQuality 

Design Generic Algorithims : Recommended based on datasize: use memoization from Project1/F#Test/votesPerformance 
1. k-nearest neighbor
2. edited k-nearest neighbor
3. condensed k-nearest neighbor
4. k-means
5. k-means clustering (use the cluster centroids as a reduced dataset for k-NN (1))

Hypothesis for the final performance of each algorithm on each dataset

Test each k-NN algorithm on 5+ values for k when clustering set k to the number of points returned from both edited-NN and condensed-NN

Paper

Video We must be able to run 5 different algorithms on 6 different datasets 10+ times in 5 min. THEREFORE: Every algorithm must run in <1 sec\



