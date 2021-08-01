from collections import namedtuple, defaultdict

Edge = namedtuple('Edge', ['left', 'right', 'cost'])
Adjacency = namedtuple('Adjacency', ['to', 'cost'])
PrioritizedItem = namedtuple('PrioritizedItem', ['priority', 'item'])

def indices(l):
  return [i for i in range(len(l))]

def process_weighted_edges(data):
  v = []
  for edge in data.split(b'\n'):
    sa = edge.decode('utf-8').split(' ')
    if len(sa) > 2:
        v.append(Edge(left=int(sa[0]), right=int(sa[1]), cost=int(sa[2])))
  return v

def undirected_graph_of_weighted_edges(edges):
  graph = defaultdict(list)
  for r in edges:
    graph[r.left].append( Adjacency(to=r.right, cost=r.cost) )
    graph[r.right].append( Adjacency(to=r.left, cost=r.cost) )
  return graph

def flatten(t):
    return [item for sublist in t for item in sublist]

def obfuscate(value):
  with open('values.p', 'a') as fp:
    fp.write(str(value) + '\n')