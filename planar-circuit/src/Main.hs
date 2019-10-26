module Main where


import Data.Array

docs = "Graph representation: http://citeseerx.ist.psu.edu/viewdoc/download;jsessionid=E996DC9201B4B7D77928E034FD9AB4A3?doi=10.1.1.52.6526&rep=rep1&type=pdf"


-- Datatypes
-- 1. ComponentInfo
data Component = Component
  { ref :: String, -- reference name (like variable name but in design)
    name :: String, -- i.e. LM555 or whatever
    datasheet :: String -- why not
    -- forget other stuff for now
  }
-- 2. Vertex, holds Component
type Vertex = Char -- @TODO change to component!
-- 3. Adjacency list
type Table a = Array Vertex a -- array is indexed by Vertex (like a dictionary!)
-- 4. Graph is an adjacency list indexed by vertex -> [vertex]
type Graph = Table [Vertex] -- table of arrays indexed by vertex to list of vertices
-- 5. edges
type Edge = (Vertex, Vertex)
-- to work with Array types
type Bounds = (Vertex, Vertex)

vertices :: Graph -> [Vertex]
vertices = indices

edges :: Graph -> [Edge]
edges g = [(v,w) | v <- vertices g, w <- g!v] -- v is index, w is all the stuff indexed by v

mapT :: (Vertex -> a -> b) -> (Table a) -> (Table b)
mapT f t = array (bounds t)  -- note that array :: (low bound, high bound) [] --> array
              [(v, f v (t!v)) | v <- indices t]

-- provides table of outdegrees of each vertex
outdegree :: Graph -> Table Int
outdegree g = mapT numEdges g
                where numEdges v ws = length ws

buildG :: Bounds -> [Edge] -> Graph
buildG bounds edges = accumArray (flip (:)) [] bounds edges -- accumArray makes Arrays, but the value can have many things like []

graph = buildG ('a', 'j')
        [
          ('a','j'), ('a', 'b'), ('a', 'c'),
          ('b', 'd'), ('b', 'e'),
          ('c', 'd')
        ]

-- parse netlist into graph data structure
-- adjacency list representation
-- [vertices]
-- [edges]

main :: IO ()
main = do
  putStrLn (graph ! 'a') -- show all of a's neighbors
