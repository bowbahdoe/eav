struct NodeId(u64);

/// A node needs to be serializable straight into storage, including info about what
/// it links to.

struct RootNode<Data> {
    id: NodeId,
    mailbox: Vec<Change<Data>>,
    left: Option<NodeId>,
    right: Option<NodeId>
}

struct BranchNode<Data> {
    id: NodeId,
    mailbox: Vec<Change<Data>>,
    left: Option<NodeId>,
    right: Option<NodeId>
}

struct LeafNode<Data> {
    id: NodeId,
    data: Data
}

enum Node<Data> {
    Root(RootNode),
    Branch(BranchNode),
    Leaf(LeafNode)
}

enum Change<Data> {
    Upsert,
    Delete,
    Filter,
    RemoveInterval
}

struct Lookup<Data> {
    root: RootNode<Data>,
    local: HashMap<NodeId, Node<Data>>,
}

