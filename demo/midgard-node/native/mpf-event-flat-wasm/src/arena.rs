use super::*;

#[derive(Clone)]
pub(super) struct Arena {
    pub(super) nodes: Vec<Node>,
    pub(super) ids: HashMap<Hash, usize>,
    pub(super) root: Option<usize>,
    pub(super) base_count: usize,
}

impl Arena {
    pub(super) fn new() -> Self {
        Self {
            nodes: Vec::new(),
            ids: HashMap::new(),
            root: None,
            base_count: 0,
        }
    }

    pub(super) fn append(&mut self, node: Node) -> Result<usize, String> {
        let hash = node.hash();
        if let Some(id) = self.ids.get(&hash) {
            if self.nodes[*id] != node {
                return Err("Architecture F content hash collision".to_owned());
            }
            return Ok(*id);
        }
        if self.nodes.len() >= ABSOLUTE_MAX_ARENA_NODES {
            return Err("Architecture F arena node cap exceeded".to_owned());
        }
        let id = self.nodes.len();
        self.nodes.push(node);
        self.ids.insert(hash, id);
        Ok(id)
    }

    pub(super) fn append_leaf(
        &mut self,
        prefix: Vec<u8>,
        key: Vec<u8>,
        value: Vec<u8>,
    ) -> Result<usize, String> {
        let hash = leaf_hash(&prefix, &value)?;
        self.append(Node::Leaf {
            hash,
            prefix,
            key,
            value,
        })
    }

    pub(super) fn append_branch(
        &mut self,
        prefix: Vec<u8>,
        children: [Option<Hash>; 16],
        size: u64,
    ) -> Result<usize, String> {
        let merkle = branch_merkle(&children);
        let hash = branch_hash(&prefix, &merkle);
        self.append(Node::Branch {
            hash,
            prefix,
            children,
            size,
            merkle,
        })
    }

    pub(super) fn append_updated_branch(
        &mut self,
        source_id: usize,
        branch: usize,
        child: Option<Hash>,
        size: u64,
    ) -> Result<usize, String> {
        let (prefix, mut children, mut merkle) = match &self.nodes[source_id] {
            Node::Branch {
                prefix,
                children,
                merkle,
                ..
            } => (prefix.clone(), *children, *merkle),
            _ => return Err("Architecture F update source is not a branch".to_owned()),
        };
        children[branch] = child;
        let mut index = 16 + branch;
        let mut current = child.unwrap_or(ZERO_HASH);
        while index > 1 {
            let sibling_index = index ^ 1;
            let sibling = if sibling_index >= 16 {
                children[sibling_index - 16].unwrap_or(ZERO_HASH)
            } else {
                merkle[sibling_index - 1]
            };
            current = if index % 2 == 0 {
                digest(&[&current, &sibling])
            } else {
                digest(&[&sibling, &current])
            };
            index >>= 1;
            merkle[index - 1] = current;
        }
        let hash = branch_hash(&prefix, &merkle);
        self.append(Node::Branch {
            hash,
            prefix,
            children,
            size,
            merkle,
        })
    }

    pub(super) fn resolve(&self, hash: &Hash) -> Result<usize, String> {
        self.ids.get(hash).copied().ok_or_else(|| {
            format!(
                "Architecture F mutation crossed unavailable frontier {}",
                hex(hash)
            )
        })
    }

    pub(super) fn insert_at(
        &mut self,
        id: Option<usize>,
        cursor: usize,
        path: &[u8],
        key: &[u8],
        value: &[u8],
    ) -> Result<usize, String> {
        let Some(id) = id else {
            return self.append_leaf(path[cursor..].to_vec(), key.to_vec(), value.to_vec());
        };
        match self.nodes[id].clone() {
            Node::Leaf {
                prefix,
                key: node_key,
                value: node_value,
                ..
            } => {
                if node_key == key {
                    return Err(format!("Architecture F key already exists: {}", hex(key)));
                }
                let remaining = &path[cursor..];
                let shared = common_prefix_length(&prefix, remaining);
                if shared >= prefix.len()
                    || shared >= remaining.len()
                    || prefix[shared] == remaining[shared]
                {
                    return Err("Architecture F leaf split did not diverge".to_owned());
                }
                let old_nibble = prefix[shared] as usize;
                let new_nibble = remaining[shared] as usize;
                let old_leaf =
                    self.append_leaf(prefix[shared + 1..].to_vec(), node_key, node_value)?;
                let new_leaf = self.append_leaf(
                    remaining[shared + 1..].to_vec(),
                    key.to_vec(),
                    value.to_vec(),
                )?;
                let mut children = [None; 16];
                children[old_nibble] = Some(self.nodes[old_leaf].hash());
                children[new_nibble] = Some(self.nodes[new_leaf].hash());
                self.append_branch(prefix[..shared].to_vec(), children, 2)
            }
            Node::Branch {
                prefix,
                children,
                size,
                ..
            } => {
                let inserted_size = size
                    .checked_add(1)
                    .filter(|value| *value <= MAX_SAFE_TRIE_SIZE)
                    .ok_or_else(|| "Architecture F trie size overflow".to_owned())?;
                let remaining = &path[cursor..];
                let shared = common_prefix_length(&prefix, remaining);
                if shared < prefix.len() {
                    if shared >= remaining.len() || prefix[shared] == remaining[shared] {
                        return Err("Architecture F branch split did not diverge".to_owned());
                    }
                    let old_nibble = prefix[shared] as usize;
                    let new_nibble = remaining[shared] as usize;
                    let old_branch =
                        self.append_branch(prefix[shared + 1..].to_vec(), children, size)?;
                    let new_leaf = self.append_leaf(
                        remaining[shared + 1..].to_vec(),
                        key.to_vec(),
                        value.to_vec(),
                    )?;
                    let mut split_children = [None; 16];
                    split_children[old_nibble] = Some(self.nodes[old_branch].hash());
                    split_children[new_nibble] = Some(self.nodes[new_leaf].hash());
                    return self.append_branch(
                        prefix[..shared].to_vec(),
                        split_children,
                        inserted_size,
                    );
                }
                let path_index = cursor + prefix.len();
                if path_index >= path.len() {
                    return Err("Architecture F path ended at a branch".to_owned());
                }
                let branch = path[path_index] as usize;
                let child_id = children[branch]
                    .as_ref()
                    .map(|child| self.resolve(child))
                    .transpose()?;
                let inserted = self.insert_at(child_id, path_index + 1, path, key, value)?;
                self.append_updated_branch(
                    id,
                    branch,
                    Some(self.nodes[inserted].hash()),
                    inserted_size,
                )
            }
        }
    }

    pub(super) fn delete_at(
        &mut self,
        id: usize,
        cursor: usize,
        path: &[u8],
        key: &[u8],
    ) -> Result<Option<usize>, String> {
        match self.nodes[id].clone() {
            Node::Leaf {
                prefix,
                key: node_key,
                ..
            } => {
                if node_key != key || prefix != path[cursor..] {
                    return Err(format!("Architecture F key is absent: {}", hex(key)));
                }
                Ok(None)
            }
            Node::Branch {
                prefix,
                mut children,
                size,
                ..
            } => {
                if !path[cursor..].starts_with(&prefix) {
                    return Err(format!("Architecture F key is absent: {}", hex(key)));
                }
                let path_index = cursor + prefix.len();
                if path_index >= path.len() {
                    return Err(format!("Architecture F key is absent: {}", hex(key)));
                }
                let branch = path[path_index] as usize;
                let child_hash = children[branch]
                    .ok_or_else(|| format!("Architecture F key is absent: {}", hex(key)))?;
                let child_id = self.resolve(&child_hash)?;
                let deleted = self.delete_at(child_id, path_index + 1, path, key)?;
                children[branch] = deleted.map(|child| self.nodes[child].hash());
                let remaining: Vec<(usize, Hash)> = children
                    .iter()
                    .enumerate()
                    .filter_map(|(index, child)| child.map(|hash| (index, hash)))
                    .collect();
                match remaining.as_slice() {
                    [] => Ok(None),
                    [(only_index, only_hash)] => {
                        let child_id = self.resolve(only_hash)?;
                        let mut collapsed_prefix = prefix;
                        collapsed_prefix.push(*only_index as u8);
                        collapsed_prefix.extend_from_slice(self.nodes[child_id].prefix());
                        match self.nodes[child_id].clone() {
                            Node::Leaf { key, value, .. } => {
                                self.append_leaf(collapsed_prefix, key, value).map(Some)
                            }
                            Node::Branch { children, size, .. } => self
                                .append_branch(collapsed_prefix, children, size)
                                .map(Some),
                        }
                    }
                    _ => self
                        .append_updated_branch(id, branch, children[branch], size - 1)
                        .map(Some),
                }
            }
        }
    }

    pub(super) fn apply_event(&mut self, ops: &[Op]) -> Result<Hash, String> {
        let mut root = self.root;
        for op in ops {
            match op {
                Op::Insert { key, value } => {
                    let path = path_nibbles(key);
                    root = Some(self.insert_at(root, 0, &path, key, value)?);
                }
                Op::Delete { key } => {
                    let root_id =
                        root.ok_or_else(|| format!("Architecture F key is absent: {}", hex(key)))?;
                    let path = path_nibbles(key);
                    root = self.delete_at(root_id, 0, &path, key)?;
                }
            }
        }
        self.root = root;
        Ok(root.map(|id| self.nodes[id].hash()).unwrap_or(EMPTY_ROOT))
    }

    pub(super) fn assert_base_closure(&self, base_root: Hash) -> Result<(), String> {
        if base_root == EMPTY_ROOT {
            if self.base_count != 0 {
                return Err("Architecture F empty base contains records".to_owned());
            }
            return Ok(());
        }
        let root = self.resolve(&base_root)?;
        let mut pending = vec![root];
        let mut reachable = HashSet::new();
        while let Some(id) = pending.pop() {
            if !reachable.insert(id) {
                continue;
            }
            if let Node::Branch { children, .. } = &self.nodes[id] {
                for child in children.iter().flatten() {
                    if let Some(child_id) = self.ids.get(child) {
                        pending.push(*child_id);
                    }
                }
            }
        }
        if reachable.len() != self.base_count {
            return Err(format!(
                "Architecture F base contains unreachable records: reachable={},records={}",
                reachable.len(),
                self.base_count
            ));
        }
        Ok(())
    }

    pub(super) fn dirty_records(&self) -> Vec<&Node> {
        let Some(root) = self.root else {
            return Vec::new();
        };
        let mut pending = vec![root];
        let mut visited = HashSet::new();
        let mut dirty = Vec::new();
        while let Some(id) = pending.pop() {
            if !visited.insert(id) || id < self.base_count {
                continue;
            }
            let node = &self.nodes[id];
            dirty.push(node);
            if let Node::Branch { children, .. } = node {
                for child in children.iter().flatten() {
                    if let Some(child_id) = self.ids.get(child) {
                        pending.push(*child_id);
                    }
                }
            }
        }
        dirty.sort_unstable_by_key(|node| node.hash());
        dirty
    }

    pub(super) fn rollback(&mut self, node_count: usize, root: Option<usize>) {
        self.nodes.truncate(node_count);
        self.ids.retain(|_, id| *id < node_count);
        self.root = root;
    }
}
