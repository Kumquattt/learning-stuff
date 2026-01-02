def reduceNumbers(node):
    return list(map(lambda i:i - 1, node))

def get_graph_info(adj_list):
    #adj_list is the adjacency list of a graph. It's a list of lists, with each element being the two nodes connected by an edge.
    #For example, if adj_list=[[0,1],[2,3],[4,3]], we have three edges, one connecting nodes 0 and 1, another one connecting nodes 2 and 3 
    # and a third one connecting nodes 4 and 3.
    print(" -- ")
    print(adj_list)
    adj_list = list(map(lambda node:reduceNumbers(node), adj_list))
    print(adj_list)
    flat_list = sum(adj_list, [])
    print(flat_list)

    numberOfLoops = 0
    maxGrade = 0
    for index in range(0, len(adj_list)-1):
        node = adj_list[index]
        grade = flat_list.count(index)
        maxGrade = max(maxGrade, grade)
    print(f"MaxGrade = {maxGrade}" )

    for node in adj_list:
        if node[0] == node[1]:
            numberOfLoops += 1
    print(f"Number of loops = {numberOfLoops}")

    foundParrallelEdge = False
    i = 0
    while(not foundParrallelEdge and i < len(adj_list)):
        node = adj_list[i]
        others = adj_list.copy()
        others.remove(node)
        print(others)
        for otherNode in others:
            print(f"Node: {node} -> {otherNode}")
            if otherNode == node or otherNode == [node[1], node[0]]:
                foundParrallelEdge = True
        i += 1
    print(f"Has Parralled edges: {foundParrallelEdge}")

    #fix me: for the given graph, return: maximum grade of a node, 
    # number of loops, boolean representing if there are parallel edges.
    return maxGrade, numberOfLoops, foundParrallelEdge 