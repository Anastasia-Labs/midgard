use super::*;

pub(super) struct CachedNode {
    prefix: Box<[u8]>,
    body: CachedBody,
}

enum CachedBody {
    Leaf { key: Box<[u8]>, value: Box<[u8]> },
    Branch(Box<CachedBranch>),
}

struct CachedBranch {
    size: u64,
    bitmap: u16,
    // Only the present children, in nibble order.
    children: Box<[Hash]>,
    // Kept so the arena's append_updated_branch rehashes one path, not 15.
    merkle: [Hash; 15],
}

impl CachedBranch {
    fn children(&self) -> [Option<Hash>; 16] {
        let mut children = [None; 16];
        let mut present = self.children.iter();
        for (index, child) in children.iter_mut().enumerate() {
            if self.bitmap & (1 << index) != 0 {
                *child = present.next().copied();
            }
        }
        children
    }
}

impl CachedNode {
    fn from_node(node: Node) -> Self {
        match node {
            Node::Leaf {
                prefix, key, value, ..
            } => Self {
                prefix: prefix.into_boxed_slice(),
                body: CachedBody::Leaf {
                    key: key.into_boxed_slice(),
                    value: value.into_boxed_slice(),
                },
            },
            Node::Branch {
                prefix,
                children,
                size,
                merkle,
                ..
            } => {
                let bitmap = children
                    .iter()
                    .enumerate()
                    .fold(0u16, |bits, (index, child)| {
                        bits | if child.is_some() { 1 << index } else { 0 }
                    });
                Self {
                    prefix: prefix.into_boxed_slice(),
                    body: CachedBody::Branch(Box::new(CachedBranch {
                        size,
                        bitmap,
                        children: children.iter().flatten().copied().collect(),
                        merkle,
                    })),
                }
            }
        }
    }

    fn to_node(&self, hash: Hash) -> Node {
        let prefix = self.prefix.to_vec();
        match &self.body {
            CachedBody::Leaf { key, value } => Node::Leaf {
                hash,
                prefix,
                key: key.to_vec(),
                value: value.to_vec(),
            },
            CachedBody::Branch(branch) => Node::Branch {
                hash,
                prefix,
                children: branch.children(),
                size: branch.size,
                merkle: branch.merkle,
            },
        }
    }

    // A branch's size comes from the store and is not bound by its hash. A
    // wrong size can only stop the arena at a node it was not given.
    fn subtree_size(&self) -> u64 {
        match &self.body {
            CachedBody::Leaf { .. } => 1,
            CachedBody::Branch(branch) => branch.size,
        }
    }
}

fn allocation_bytes(length: usize) -> usize {
    if length == 0 {
        0
    } else {
        length + 16
    }
}

// Estimated heap bytes of one entry: its map slot plus control byte counted
// twice (a power-of-two table is between 7/16 and 7/8 full), plus every boxed
// allocation's length and 16 bytes of allocator header and rounding.
fn entry_bytes(node: &CachedNode) -> usize {
    let slot = 2 * (size_of::<(Hash, CachedNode)>() + 1);
    let body = match &node.body {
        CachedBody::Leaf { key, value } => {
            allocation_bytes(key.len()) + allocation_bytes(value.len())
        }
        CachedBody::Branch(branch) => {
            allocation_bytes(size_of::<CachedBranch>())
                + allocation_bytes(branch.children.len() * size_of::<Hash>())
        }
    };
    slot + allocation_bytes(node.prefix.len()) + body
}

pub(super) enum Proof {
    Ready(Arena),
    Missing(Vec<Hash>),
}

struct Walk<'a> {
    touched: &'a [(Vec<u8>, bool)],
    selected: HashSet<Hash>,
    missing: BTreeSet<Hash>,
}

// Trie nodes keyed by content hash, so an entry is never stale whatever the
// durable root. Two generations approximate least-recently-used: a hit in
// `old` moves to `young`, and a trim over budget drops `old`.
pub(super) struct NodeCache {
    young: HashMap<Hash, CachedNode>,
    old: HashMap<Hash, CachedNode>,
    young_bytes: usize,
    old_bytes: usize,
    budget: usize,
}

impl NodeCache {
    pub(super) fn new(budget: usize) -> Self {
        Self {
            young: HashMap::new(),
            old: HashMap::new(),
            young_bytes: 0,
            old_bytes: 0,
            budget,
        }
    }

    pub(super) fn set_budget(&mut self, budget: usize) {
        self.budget = budget;
    }

    pub(super) fn len(&self) -> usize {
        self.young.len() + self.old.len()
    }

    pub(super) fn bytes(&self) -> usize {
        self.young_bytes + self.old_bytes
    }

    #[cfg(test)]
    pub(super) fn contains(&self, hash: &Hash) -> bool {
        self.young.contains_key(hash) || self.old.contains_key(hash)
    }

    fn touch(&mut self, hash: &Hash) -> Option<&CachedNode> {
        if !self.young.contains_key(hash) {
            let node = self.old.remove(hash)?;
            let bytes = entry_bytes(&node);
            self.old_bytes -= bytes;
            self.young_bytes += bytes;
            self.young.insert(*hash, node);
        }
        self.young.get(hash)
    }

    // The caller has authenticated the node against its hash.
    pub(super) fn insert(&mut self, node: Node) {
        let hash = node.hash();
        if self.touch(&hash).is_some() {
            return;
        }
        let node = CachedNode::from_node(node);
        self.young_bytes += entry_bytes(&node);
        self.young.insert(hash, node);
    }

    // Never call this between the NeedNodes rounds of one apply: with a
    // small budget it would drop what the last round fetched, forever.
    pub(super) fn trim(&mut self) {
        if self.bytes() <= self.budget {
            return;
        }
        self.old = std::mem::take(&mut self.young);
        self.old_bytes = std::mem::take(&mut self.young_bytes);
        // One generation alone over budget loses arbitrary entries.
        let mut excess = self.old_bytes.saturating_sub(self.budget);
        let mut removed = 0usize;
        self.old.retain(|_, node| {
            if excess == 0 {
                return true;
            }
            let bytes = entry_bytes(node);
            excess = excess.saturating_sub(bytes);
            removed += bytes;
            false
        });
        if removed > 0 {
            // The table keeps its capacity otherwise, which the estimate
            // does not count.
            self.old.shrink_to_fit();
            self.old_bytes -= removed;
        }
    }

    // Walks the stream's keys from `stream.base_root`. Either every node the
    // replay needs is cached and the arena is built, or the absent ones are
    // returned sorted; nothing is fetched speculatively.
    pub(super) fn proof_arena(&mut self, stream: &EventStream) -> Result<Proof, String> {
        if stream.base_root == EMPTY_ROOT {
            return Ok(Proof::Ready(Arena::new()));
        }
        let touched: Vec<(Vec<u8>, bool)> = stream
            .events
            .iter()
            .flatten()
            .map(|op| match op {
                Op::Insert { key, .. } => (path_nibbles(key), false),
                Op::Delete { key } => (path_nibbles(key), true),
            })
            .collect();
        let mut walk = Walk {
            touched: &touched,
            selected: HashSet::new(),
            missing: BTreeSet::new(),
        };
        let candidates: Vec<usize> = (0..touched.len()).collect();
        self.select(stream.base_root, 0, &candidates, &mut walk)?;
        // Every absent node ends up in the arena once provided.
        if walk.selected.len() + walk.missing.len() > ABSOLUTE_MAX_ARENA_NODES {
            return Err("Architecture G selection exceeds the arena node cap".to_owned());
        }
        if !walk.missing.is_empty() {
            return Ok(Proof::Missing(walk.missing.into_iter().collect()));
        }
        let mut hashes: Vec<Hash> = walk.selected.into_iter().collect();
        hashes.sort_unstable();
        let mut arena = Arena::new();
        for hash in hashes {
            let node = self
                .young
                .get(&hash)
                .or_else(|| self.old.get(&hash))
                .ok_or_else(|| "Architecture G selected node left the cache".to_owned())?;
            arena.append(node.to_node(hash))?;
        }
        arena.base_count = arena.nodes.len();
        arena.root = Some(arena.resolve(&stream.base_root)?);
        arena.assert_base_closure(stream.base_root)?;
        Ok(Proof::Ready(arena))
    }

    fn select(
        &mut self,
        hash: Hash,
        cursor: usize,
        candidates: &[usize],
        walk: &mut Walk<'_>,
    ) -> Result<(), String> {
        let Some(node) = self.touch(&hash) else {
            walk.missing.insert(hash);
            return Ok(());
        };
        walk.selected.insert(hash);
        let CachedBody::Branch(branch) = &node.body else {
            return Ok(());
        };
        let children = branch.children();
        let prefix = &node.prefix;
        let next_cursor = cursor + prefix.len();
        let mut by_branch: [Vec<usize>; 16] = std::array::from_fn(|_| Vec::new());
        let mut deletes_by_branch = [0u64; 16];
        for index in candidates {
            let path = &walk.touched[*index].0;
            if cursor > path.len()
                || !path[cursor..].starts_with(prefix)
                || next_cursor >= path.len()
            {
                continue;
            }
            let branch = path[next_cursor] as usize;
            by_branch[branch].push(*index);
            if walk.touched[*index].1 {
                deletes_by_branch[branch] += 1;
            }
        }
        if deletes_by_branch.iter().any(|count| *count > 0) {
            let mut guaranteed_survivors = 0usize;
            let mut undecided = false;
            for (branch, delete_count) in deletes_by_branch.iter().enumerate() {
                let Some(child) = children[branch] else {
                    continue;
                };
                // Inserts cannot remove a child, so a child without delete
                // candidates survives the whole stream.
                if *delete_count == 0 {
                    guaranteed_survivors += 1;
                    continue;
                }
                // This child is on a touched path, so the recursion below
                // reports it absent; the siblings wait for its size.
                match self.touch(&child).map(CachedNode::subtree_size) {
                    Some(size) if size > *delete_count => guaranteed_survivors += 1,
                    Some(_) => {}
                    None => undecided = true,
                }
            }
            // A delete needs an otherwise untouched child only when this
            // branch can collapse to a single child; child hashes in the
            // branch suffice for ordinary Merkle updates.
            if guaranteed_survivors < 2 && !undecided {
                for child in children.iter().flatten() {
                    if self.touch(child).is_some() {
                        walk.selected.insert(*child);
                    } else {
                        walk.missing.insert(*child);
                    }
                }
            }
        }
        for (branch, branch_candidates) in by_branch.iter().enumerate() {
            if branch_candidates.is_empty() {
                continue;
            }
            let Some(child) = children[branch] else {
                continue;
            };
            self.select(child, next_cursor + 1, branch_candidates, walk)?;
        }
        Ok(())
    }
}
