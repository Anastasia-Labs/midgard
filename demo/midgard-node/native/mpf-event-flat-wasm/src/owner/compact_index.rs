use super::*;

#[derive(Clone, Copy)]
pub(super) enum CompactKind {
    Leaf {
        key_offset: u32,
        key_length: u16,
        value_offset: u64,
        value_length: u32,
    },
    Branch {
        size: u64,
        bitmap: u16,
        children_offset: u32,
        merkle_offset: u32,
    },
}

#[derive(Clone)]
pub(super) struct CompactNode {
    pub(super) hash: Hash,
    pub(super) prefix_offset: u32,
    pub(super) prefix_length: u8,
    pub(super) kind: CompactKind,
}

pub(super) struct FullIndex {
    pub(super) nodes: Vec<CompactNode>,
    pub(super) ids: HashMap<Hash, u32>,
    pub(super) prefixes: Vec<u8>,
    pub(super) children: Vec<Hash>,
    pub(super) child_ids: Vec<u32>,
    pub(super) branch_merkle: Vec<[Hash; 15]>,
    pub(super) keys: Vec<u8>,
    pub(super) values: Vec<u8>,
    pub(super) root: Option<u32>,
    pub(super) branches: usize,
    pub(super) leaves: usize,
    pub(super) edges: usize,
}

pub(super) struct IndexDiagnostics {
    pub(super) nodes: usize,
    pub(super) branches: usize,
    pub(super) leaves: usize,
    pub(super) edges: usize,
    pub(super) compact_bytes: usize,
}

impl FullIndex {
    pub(super) fn from_payload(payload: &[u8], expected_marker: Hash) -> Result<Self, String> {
        if payload.len() < INPUT_HEADER_BYTES
            || payload.len() > FULL_INDEX_MAX_BYTES
            || &payload[..4] != INPUT_MAGIC
        {
            return Err("Architecture G full-index payload header/size is invalid".to_owned());
        }
        let mut reader = Reader::new(payload);
        reader.take(4)?;
        if reader.u16()? != ABI_VERSION || reader.u16()? != 0 {
            return Err("Architecture G full-index ABI version/flags are invalid".to_owned());
        }
        let max_records = reader.u32()? as usize;
        let max_events = reader.u32()? as usize;
        let max_ops = reader.u32()? as usize;
        let max_input = reader.u32()? as usize;
        let max_output = reader.u32()? as usize;
        if max_records > FULL_INDEX_MAX_RECORDS
            || max_events > ABSOLUTE_MAX_EVENTS
            || max_ops > ABSOLUTE_MAX_OPS
            || max_input > FULL_INDEX_MAX_BYTES
            || max_output > ABSOLUTE_MAX_OUTPUT_BYTES
            || payload.len() > max_input
        {
            return Err("Architecture G full-index caller cap is invalid".to_owned());
        }
        let record_count = reader.u32()? as usize;
        let event_count = reader.u32()? as usize;
        let op_count = reader.u32()? as usize;
        let marker = reader.hash()?;
        if marker != expected_marker
            || record_count > max_records
            || event_count != 0
            || op_count != 0
        {
            return Err("Architecture G full-index counts/marker are invalid".to_owned());
        }

        let mut index = Self {
            nodes: Vec::with_capacity(record_count),
            ids: HashMap::with_capacity(record_count),
            prefixes: Vec::new(),
            children: Vec::new(),
            child_ids: Vec::new(),
            branch_merkle: Vec::new(),
            keys: Vec::new(),
            values: Vec::new(),
            root: None,
            branches: 0,
            leaves: 0,
            edges: 0,
        };
        for _ in 0..record_count {
            let node = parse_node(&mut reader)?;
            index.append(node)?;
        }
        index.resolve_child_ids_from(0)?;
        if reader.remaining() != 0 {
            return Err("Architecture G full-index payload has trailing bytes".to_owned());
        }
        index.root = if marker == EMPTY_ROOT {
            None
        } else {
            Some(*index.ids.get(&marker).ok_or_else(|| {
                "Architecture G full index is missing the durable root".to_owned()
            })?)
        };
        index.authenticate_complete_closure()?;
        if index.estimated_bytes() > MAX_RESIDENT_BYTES {
            return Err("Architecture G full index exceeds the 2 GiB resident cap".to_owned());
        }
        Ok(index)
    }

    pub(super) fn append(&mut self, node: Node) -> Result<(), String> {
        let hash = node.hash();
        if self.ids.contains_key(&hash) {
            return Err("Architecture G full index contains a duplicate record".to_owned());
        }
        let prefix_offset = u32::try_from(self.prefixes.len())
            .map_err(|_| "Architecture G prefix arena overflow".to_owned())?;
        let prefix_length = u8::try_from(node.prefix().len())
            .map_err(|_| "Architecture G prefix length overflow".to_owned())?;
        self.prefixes.extend_from_slice(node.prefix());
        let kind = match node {
            Node::Leaf { key, value, .. } => {
                self.leaves += 1;
                let key_offset = u32::try_from(self.keys.len())
                    .map_err(|_| "Architecture G key arena overflow".to_owned())?;
                let key_length = u16::try_from(key.len())
                    .map_err(|_| "Architecture G key length overflow".to_owned())?;
                let value_offset = u64::try_from(self.values.len())
                    .map_err(|_| "Architecture G value arena overflow".to_owned())?;
                let value_length = u32::try_from(value.len())
                    .map_err(|_| "Architecture G value length overflow".to_owned())?;
                self.keys.extend_from_slice(&key);
                self.values.extend_from_slice(&value);
                CompactKind::Leaf {
                    key_offset,
                    key_length,
                    value_offset,
                    value_length,
                }
            }
            Node::Branch {
                children,
                size,
                merkle,
                ..
            } => {
                self.branches += 1;
                let children_offset = u32::try_from(self.children.len())
                    .map_err(|_| "Architecture G child arena overflow".to_owned())?;
                let bitmap = children
                    .iter()
                    .enumerate()
                    .fold(0u16, |bits, (index, child)| {
                        bits | if child.is_some() { 1 << index } else { 0 }
                    });
                for child in children.iter().flatten() {
                    self.children.push(*child);
                    self.child_ids.push(UNRESOLVED_CHILD_ID);
                    self.edges += 1;
                }
                let merkle_offset = u32::try_from(self.branch_merkle.len())
                    .map_err(|_| "Architecture G Merkle arena overflow".to_owned())?;
                self.branch_merkle.push(merkle);
                CompactKind::Branch {
                    size,
                    bitmap,
                    children_offset,
                    merkle_offset,
                }
            }
        };
        let id = u32::try_from(self.nodes.len())
            .map_err(|_| "Architecture G node id overflow".to_owned())?;
        self.nodes.push(CompactNode {
            hash,
            prefix_offset,
            prefix_length,
            kind,
        });
        self.ids.insert(hash, id);
        Ok(())
    }

    pub(super) fn reserve_promotion(
        &mut self,
        nodes: &[Node],
        max_resident_nodes: usize,
        max_resident_bytes: usize,
    ) -> Result<(), String> {
        let mut prefix_bytes = 0usize;
        let mut child_count = 0usize;
        let mut branch_count = 0usize;
        let mut key_bytes = 0usize;
        let mut value_bytes = 0usize;
        for node in nodes {
            prefix_bytes = prefix_bytes
                .checked_add(node.prefix().len())
                .ok_or_else(|| "Architecture G promotion prefix cap overflow".to_owned())?;
            match node {
                Node::Leaf { key, value, .. } => {
                    key_bytes = key_bytes
                        .checked_add(key.len())
                        .ok_or_else(|| "Architecture G promotion key cap overflow".to_owned())?;
                    value_bytes = value_bytes
                        .checked_add(value.len())
                        .ok_or_else(|| "Architecture G promotion value cap overflow".to_owned())?;
                }
                Node::Branch { children, .. } => {
                    branch_count += 1;
                    child_count = child_count
                        .checked_add(children.iter().flatten().count())
                        .ok_or_else(|| "Architecture G promotion edge cap overflow".to_owned())?;
                }
            }
        }
        let projected_nodes = self.nodes.len().saturating_add(nodes.len());
        let doubled = |value: usize| {
            value
                .checked_mul(2)
                .ok_or_else(|| "Architecture G promotion resident cap overflow".to_owned())
        };
        let projected_increment = doubled(nodes.len())?
            .checked_mul(size_of::<CompactNode>())
            .and_then(|value| {
                doubled(nodes.len()).ok().and_then(|ids| {
                    value.checked_add(ids * (size_of::<Hash>() + size_of::<u32>() + 1))
                })
            })
            .and_then(|value| value.checked_add(doubled(prefix_bytes).ok()?))
            .and_then(|value| value.checked_add(doubled(child_count).ok()? * size_of::<Hash>()))
            .and_then(|value| value.checked_add(doubled(child_count).ok()? * size_of::<u32>()))
            .and_then(|value| {
                value.checked_add(doubled(branch_count).ok()? * size_of::<[Hash; 15]>())
            })
            .and_then(|value| value.checked_add(doubled(key_bytes).ok()?))
            .and_then(|value| value.checked_add(doubled(value_bytes).ok()?))
            .ok_or_else(|| "Architecture G promotion resident cap overflow".to_owned())?;
        let projected_bytes = self
            .estimated_bytes()
            .checked_add(projected_increment)
            .unwrap_or(usize::MAX);
        if projected_nodes > max_resident_nodes || projected_bytes > max_resident_bytes {
            return Err(format!(
                "Architecture G promotion resident cap exceeded: projected_nodes={projected_nodes},projected_bytes={projected_bytes},max_nodes={max_resident_nodes},max_bytes={max_resident_bytes}"
            ));
        }
        self.nodes
            .try_reserve(nodes.len())
            .map_err(|error| format!("Architecture G promotion node reserve failed: {error}"))?;
        self.ids
            .try_reserve(nodes.len())
            .map_err(|error| format!("Architecture G promotion id reserve failed: {error}"))?;
        self.prefixes
            .try_reserve(prefix_bytes)
            .map_err(|error| format!("Architecture G promotion prefix reserve failed: {error}"))?;
        self.children
            .try_reserve(child_count)
            .map_err(|error| format!("Architecture G promotion child reserve failed: {error}"))?;
        self.child_ids.try_reserve(child_count).map_err(|error| {
            format!("Architecture G promotion child-id reserve failed: {error}")
        })?;
        self.branch_merkle
            .try_reserve(branch_count)
            .map_err(|error| format!("Architecture G promotion Merkle reserve failed: {error}"))?;
        self.keys
            .try_reserve(key_bytes)
            .map_err(|error| format!("Architecture G promotion key reserve failed: {error}"))?;
        self.values
            .try_reserve(value_bytes)
            .map_err(|error| format!("Architecture G promotion value reserve failed: {error}"))?;
        let reserved_bytes = self.estimated_bytes();
        if reserved_bytes > max_resident_bytes {
            return Err(format!(
                "Architecture G promotion resident cap exceeded after reserve: projected_nodes={projected_nodes},reserved_bytes={reserved_bytes},max_nodes={max_resident_nodes},max_bytes={max_resident_bytes}"
            ));
        }
        Ok(())
    }

    pub(super) fn prefix(&self, id: u32) -> &[u8] {
        let node = &self.nodes[id as usize];
        let start = node.prefix_offset as usize;
        &self.prefixes[start..start + node.prefix_length as usize]
    }

    pub(super) fn child_hash(&self, id: u32, branch: usize) -> Option<Hash> {
        let CompactKind::Branch {
            bitmap,
            children_offset,
            ..
        } = self.nodes[id as usize].kind
        else {
            return None;
        };
        if bitmap & (1 << branch) == 0 {
            return None;
        }
        let rank = (bitmap & ((1u16 << branch).wrapping_sub(1))).count_ones() as usize;
        Some(self.children[children_offset as usize + rank])
    }

    pub(super) fn child_id(&self, id: u32, branch: usize) -> Result<Option<u32>, String> {
        let CompactKind::Branch {
            bitmap,
            children_offset,
            ..
        } = self.nodes[id as usize].kind
        else {
            return Ok(None);
        };
        if bitmap & (1 << branch) == 0 {
            return Ok(None);
        }
        let rank = (bitmap & ((1u16 << branch).wrapping_sub(1))).count_ones() as usize;
        let child_index = children_offset as usize + rank;
        let child_id = self.child_ids[child_index];
        if child_id == UNRESOLVED_CHILD_ID {
            return Err(format!(
                "Architecture G child ID is unresolved for child {}",
                hex(&self.children[child_index])
            ));
        }
        Ok(Some(child_id))
    }

    pub(super) fn resolve_child_ids_from(&mut self, start: usize) -> Result<(), String> {
        if start > self.children.len() || self.child_ids.len() != self.children.len() {
            return Err("Architecture G child arena and child-id arena are out of sync".to_owned());
        }
        for index in start..self.children.len() {
            if self.child_ids[index] != UNRESOLVED_CHILD_ID {
                continue;
            }
            let hash = self.children[index];
            self.child_ids[index] = self.ids.get(&hash).copied().ok_or_else(|| {
                format!(
                    "Architecture G complete closure is missing child {}",
                    hex(&hash)
                )
            })?;
        }
        Ok(())
    }

    pub(super) fn authenticate_complete_closure(&self) -> Result<(), String> {
        let mut colors = vec![0u8; self.nodes.len()];
        let mut parents = vec![0u8; self.nodes.len()];
        let mut path = Vec::with_capacity(64);
        let mut visited = 0usize;
        if let Some(root) = self.root {
            self.visit(root, &mut path, &mut colors, &mut parents, &mut visited)?;
        }
        if visited != self.nodes.len() {
            return Err(format!(
                "Architecture G full index has unreachable records: reachable={},records={}",
                visited,
                self.nodes.len()
            ));
        }
        Ok(())
    }

    pub(super) fn visit(
        &self,
        id: u32,
        path: &mut Vec<u8>,
        colors: &mut [u8],
        parents: &mut [u8],
        visited: &mut usize,
    ) -> Result<(), String> {
        let index = id as usize;
        if colors[index] == 1 {
            return Err("Architecture G full index contains a cycle".to_owned());
        }
        if colors[index] == 2 {
            return Ok(());
        }
        colors[index] = 1;
        let node = &self.nodes[index];
        let checkpoint = path.len();
        path.extend_from_slice(self.prefix(id));
        match node.kind {
            CompactKind::Leaf {
                key_offset,
                key_length,
                ..
            } => {
                let start = key_offset as usize;
                let key = &self.keys[start..start + key_length as usize];
                if path_nibbles(key).as_slice() != path.as_slice() {
                    return Err(
                        "Architecture G leaf is not linked at its canonical key path".to_owned(),
                    );
                }
            }
            CompactKind::Branch { bitmap, .. } => {
                for branch in 0..16 {
                    if bitmap & (1 << branch) == 0 {
                        continue;
                    }
                    let child = self.child_id(id, branch)?.unwrap();
                    let child_index = child as usize;
                    parents[child_index] = parents[child_index].saturating_add(1);
                    if parents[child_index] > 1 {
                        return Err("Architecture G canonical closure is not a tree".to_owned());
                    }
                    path.push(branch as u8);
                    self.visit(child, path, colors, parents, visited)?;
                    path.pop();
                }
            }
        }
        path.truncate(checkpoint);
        colors[index] = 2;
        *visited += 1;
        Ok(())
    }

    pub(super) fn to_node(&self, id: u32) -> Node {
        let node = &self.nodes[id as usize];
        let prefix = self.prefix(id).to_vec();
        match node.kind {
            CompactKind::Leaf {
                key_offset,
                key_length,
                value_offset,
                value_length,
            } => Node::Leaf {
                hash: node.hash,
                prefix,
                key: self.keys[key_offset as usize..key_offset as usize + key_length as usize]
                    .to_vec(),
                value: self.values
                    [value_offset as usize..value_offset as usize + value_length as usize]
                    .to_vec(),
            },
            CompactKind::Branch {
                size,
                bitmap,
                merkle_offset,
                ..
            } => {
                let mut children = [None; 16];
                for (branch, child) in children.iter_mut().enumerate() {
                    if bitmap & (1 << branch) != 0 {
                        *child = self.child_hash(id, branch);
                    }
                }
                let merkle = self.branch_merkle[merkle_offset as usize];
                Node::Branch {
                    hash: node.hash,
                    prefix,
                    children,
                    size,
                    merkle,
                }
            }
        }
    }

    pub(super) fn proof_arena(&self, stream: &EventStream) -> Result<Arena, String> {
        let marker = self
            .root
            .map(|id| self.nodes[id as usize].hash)
            .unwrap_or(EMPTY_ROOT);
        if stream.base_root != marker {
            return Err("Architecture G replay log base root is stale".to_owned());
        }
        let Some(root) = self.root else {
            return Ok(Arena::new());
        };
        let touched: Vec<(Vec<u8>, bool)> = stream
            .events
            .iter()
            .flatten()
            .map(|op| match op {
                Op::Insert { key, .. } => (path_nibbles(key), false),
                Op::Delete { key } => (path_nibbles(key), true),
            })
            .collect();
        let mut selected = HashSet::new();
        let candidates: Vec<usize> = (0..touched.len()).collect();
        self.select_proof_union(root, 0, &candidates, &touched, &mut selected)?;
        let mut ids: Vec<u32> = selected.into_iter().collect();
        ids.sort_unstable_by_key(|id| self.nodes[*id as usize].hash);
        let mut arena = Arena::new();
        for id in ids {
            arena.append(self.to_node(id))?;
        }
        arena.base_count = arena.nodes.len();
        arena.root = Some(arena.resolve(&stream.base_root)?);
        arena.assert_base_closure(stream.base_root)?;
        Ok(arena)
    }

    pub(super) fn select_proof_union(
        &self,
        id: u32,
        cursor: usize,
        candidates: &[usize],
        touched: &[(Vec<u8>, bool)],
        selected: &mut HashSet<u32>,
    ) -> Result<(), String> {
        selected.insert(id);
        let CompactKind::Branch { bitmap, .. } = self.nodes[id as usize].kind else {
            return Ok(());
        };
        let prefix = self.prefix(id);
        let next_cursor = cursor + prefix.len();
        let mut by_branch: [Vec<usize>; 16] = std::array::from_fn(|_| Vec::new());
        let mut deletes_by_branch = [0usize; 16];
        for index in candidates {
            let path = &touched[*index].0;
            if cursor > path.len()
                || !path[cursor..].starts_with(prefix)
                || next_cursor >= path.len()
            {
                continue;
            }
            let branch = path[next_cursor] as usize;
            by_branch[branch].push(*index);
            if touched[*index].1 {
                deletes_by_branch[branch] += 1;
            }
        }
        if deletes_by_branch.iter().any(|count| *count > 0) {
            let mut guaranteed_survivors = 0usize;
            for (branch, delete_count) in deletes_by_branch.iter().enumerate() {
                let Some(child) = self.child_id(id, branch)? else {
                    continue;
                };
                // A child with no delete candidate is definitely still
                // present after the replay. Count it as a survivor too: inserts
                // cannot remove a child, so this is an authenticated lower bound.
                if by_branch[branch].is_empty() {
                    guaranteed_survivors += 1;
                    continue;
                }
                let delete_count = u64::try_from(*delete_count)
                    .map_err(|_| "Architecture G delete-count overflow".to_owned())?;
                if self.subtree_size(child) > delete_count {
                    guaranteed_survivors += 1;
                }
            }
            // A delete needs an otherwise untouched child node only when this
            // branch can collapse to a single child. Authenticated subtree
            // sizes prove that impossible once two children must survive the
            // complete replay stream. Child hashes remain in the branch and
            // are sufficient for ordinary Merkle updates.
            if guaranteed_survivors < 2 {
                for sibling in 0..16 {
                    if bitmap & (1 << sibling) != 0 {
                        selected.insert(self.child_id(id, sibling)?.unwrap());
                    }
                }
            }
        }
        for (branch, branch_candidates) in by_branch.iter().enumerate() {
            if branch_candidates.is_empty() {
                continue;
            }
            let Some(child) = self.child_id(id, branch)? else {
                continue;
            };
            self.select_proof_union(child, next_cursor + 1, branch_candidates, touched, selected)?;
        }
        Ok(())
    }

    pub(super) fn subtree_size(&self, id: u32) -> u64 {
        match self.nodes[id as usize].kind {
            CompactKind::Leaf { .. } => 1,
            CompactKind::Branch { size, .. } => size,
        }
    }

    pub(super) fn diagnostics(&self) -> IndexDiagnostics {
        IndexDiagnostics {
            nodes: self.nodes.len(),
            branches: self.branches,
            leaves: self.leaves,
            edges: self.edges,
            compact_bytes: self.estimated_bytes(),
        }
    }

    pub(super) fn estimated_bytes(&self) -> usize {
        self.nodes.capacity() * size_of::<CompactNode>()
            + self.ids.capacity() * (size_of::<Hash>() + size_of::<u32>() + 1)
            + self.prefixes.capacity()
            + self.children.capacity() * size_of::<Hash>()
            + self.child_ids.capacity() * size_of::<u32>()
            + self.branch_merkle.capacity() * size_of::<[Hash; 15]>()
            + self.keys.capacity()
            + self.values.capacity()
    }
}
