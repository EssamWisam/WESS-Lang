from anytree import Node, RenderTree


# A function to draw a tree from a string like ('expr', '-', ('Number', 7), ... provided by GPT-3

def draw_tree(tree_string):
    def create_tree(node):
        name, *children = node
        root = Node(name)
        for child in children:
            if isinstance(child, tuple):
                root.children += (create_tree(child),)
            else:
                Node(str(child), parent=root)
        return root
    
    root = create_tree(tree_string)
    for pre, _, node in RenderTree(root):
        print(f"{pre}{node.name}")
