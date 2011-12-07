-- | In machine learning and pattern recognition, classification
-- refers to an algorithmic procedure for assigning a given piece of
-- input data into one of a given number of categories. An example
-- would be assigning a given email into “spam” or “non-spam” classes
-- or assigning a diagnosis to a given patient as described by
-- observed characteristics of the patient (gender, blood pressure,
-- presence or absence of certain symptoms, etc.). An algorithm that
-- implements classification, especially in a concrete implementation,
-- is known as a classifier. The term “classifier” sometimes also
-- refers to the mathematical function, implemented by a
-- classification algorithm, that maps input data to a category.
-- (<https://en.wikipedia.org/wiki/Classification_in_machine_learning>,
-- Nov 28 2011)

module Data.Mining.Classification
       ( module Data.Mining.Classification.DecisionTree
       , module Data.Mining.Classification.Class
       ) where
import Data.Mining.Classification.DecisionTree
import Data.Mining.Classification.Class