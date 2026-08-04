use std::{
    collections::{HashMap, HashSet},
    env,
    fs::{self, File},
    io::{Read, Write},
    mem::size_of,
    path::{Path, PathBuf},
    time::Instant,
};

use super::*;
use crate::rpc::{read_frame, write_frame, RpcFrame, RpcKind, RPC_MAX_CHUNK_BYTES};

const SIDECAR_MAGIC: &[u8; 4] = b"MGSI";
const SIDECAR_HEADER_BYTES: usize = 80;
const FULL_INDEX_MAX_RECORDS: usize = 2_000_000;
const FULL_INDEX_MAX_BYTES: usize = 536_870_912;
const MAX_RESIDENT_BYTES: usize = 2 * 1024 * 1024 * 1024;
const UNRESOLVED_CHILD_ID: u32 = u32::MAX;

#[derive(Clone, Copy)]
enum CompactKind {
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
struct CompactNode {
    hash: Hash,
    prefix_offset: u32,
    prefix_length: u8,
    kind: CompactKind,
}

struct FullIndex {
    nodes: Vec<CompactNode>,
    ids: HashMap<Hash, u32>,
    prefixes: Vec<u8>,
    children: Vec<Hash>,
    child_ids: Vec<u32>,
    branch_merkle: Vec<[Hash; 15]>,
    keys: Vec<u8>,
    values: Vec<u8>,
    root: u32,
    branches: usize,
    leaves: usize,
    edges: usize,
}

struct IndexDiagnostics {
    nodes: usize,
    branches: usize,
    leaves: usize,
    edges: usize,
    compact_bytes: usize,
}

struct SimulatedGeneration {
    base_root: Hash,
    arena: Arena,
}

struct SimulatedOwner {
    marker: Hash,
    next_handle: u64,
    generations: HashMap<u64, SimulatedGeneration>,
}

struct RuntimeGeneration {
    base_root: Hash,
    arena: Option<Arena>,
    candidate_root: Option<Hash>,
    prepared: bool,
}

struct RuntimeOwner {
    epoch: [u8; 16],
    marker: Option<Hash>,
    index: Option<FullIndex>,
    load_payload: Vec<u8>,
    generations: HashMap<[u8; 16], RuntimeGeneration>,
    next_generation: u64,
}

impl RuntimeOwner {
    fn new(epoch: [u8; 16]) -> Self {
        Self {
            epoch,
            marker: None,
            index: None,
            load_payload: Vec::new(),
            generations: HashMap::new(),
            next_generation: 1,
        }
    }

    fn generation_id(&mut self) -> Result<[u8; 16], String> {
        let counter = self.next_generation;
        self.next_generation = counter
            .checked_add(1)
            .ok_or_else(|| "Architecture G generation id overflow".to_owned())?;
        let hash = digest(&[
            b"MIDGARD-MPF-OWNER-GENERATION-V1",
            &self.epoch,
            &counter.to_le_bytes(),
        ]);
        Ok(hash[..16].try_into().unwrap())
    }

    fn fork(&mut self, base_root: Hash) -> Result<[u8; 16], String> {
        if self.generations.len() >= 2 {
            return Err("Architecture G active-generation cap exceeded".to_owned());
        }
        if self.marker != Some(base_root) {
            return Err("Architecture G fork base root is stale".to_owned());
        }
        let id = self.generation_id()?;
        self.generations.insert(
            id,
            RuntimeGeneration {
                base_root,
                arena: None,
                candidate_root: None,
                prepared: false,
            },
        );
        Ok(id)
    }

    fn apply(
        &mut self,
        id: [u8; 16],
        event_bytes: &[u8],
    ) -> Result<(Hash, Vec<Hash>, u64, u64), String> {
        let stream = parse_event_stream(event_bytes)?;
        let generation = self
            .generations
            .get_mut(&id)
            .ok_or_else(|| "Architecture G generation handle is stale".to_owned())?;
        if stream.base_root != generation.base_root || self.marker != Some(stream.base_root) {
            return Err("Architecture G replay log base root is stale".to_owned());
        }
        if generation.arena.is_some() {
            return Err("Architecture G generation has already been applied".to_owned());
        }
        let index = self
            .index
            .as_ref()
            .ok_or_else(|| "Architecture G owner is not ready".to_owned())?;
        let proof_started_at = Instant::now();
        let mut arena = index.proof_arena(&stream)?;
        let proof_duration_ns = u64::try_from(proof_started_at.elapsed().as_nanos())
            .map_err(|_| "Architecture G proof timing overflow".to_owned())?;
        let mutation_started_at = Instant::now();
        let mut roots = Vec::with_capacity(stream.events.len());
        for event in &stream.events {
            roots.push(arena.apply_event(event)?);
        }
        let mutation_duration_ns = u64::try_from(mutation_started_at.elapsed().as_nanos())
            .map_err(|_| "Architecture G mutation timing overflow".to_owned())?;
        let candidate = roots.last().copied().unwrap_or(stream.base_root);
        let observed_rss_bytes = (rss_kib("VmRSS:") as usize).saturating_mul(1024);
        if observed_rss_bytes > MAX_RESIDENT_BYTES {
            return Err(format!(
                "Architecture G apply exceeds observed RSS cap: rss_bytes={observed_rss_bytes},max_bytes={MAX_RESIDENT_BYTES}"
            ));
        }
        generation.arena = Some(arena);
        generation.candidate_root = Some(candidate);
        Ok((candidate, roots, proof_duration_ns, mutation_duration_ns))
    }

    fn generated_records(&mut self, id: [u8; 16]) -> Result<(Hash, Hash, Vec<Vec<u8>>), String> {
        self.generated_records_with_caps(id, FULL_INDEX_MAX_RECORDS, MAX_RESIDENT_BYTES)
    }

    fn generated_records_with_caps(
        &mut self,
        id: [u8; 16],
        max_resident_nodes: usize,
        max_resident_bytes: usize,
    ) -> Result<(Hash, Hash, Vec<Vec<u8>>), String> {
        let (base, candidate, records, new_records) = {
            let generation = self
                .generations
                .get(&id)
                .ok_or_else(|| "Architecture G generation handle is stale".to_owned())?;
            let candidate = generation
                .candidate_root
                .ok_or_else(|| "Architecture G generation has not been applied".to_owned())?;
            let arena = generation
                .arena
                .as_ref()
                .ok_or_else(|| "Architecture G generation arena is missing".to_owned())?;
            let records = generated_reachable_records(arena, candidate)?;
            let index = self
                .index
                .as_ref()
                .ok_or_else(|| "Architecture G owner is not ready".to_owned())?;
            let new_records = generated_reachable_nodes(arena, candidate)?
                .into_iter()
                .filter(|record| !index.ids.contains_key(&record.hash()))
                .collect::<Vec<_>>();
            (generation.base_root, candidate, records, new_records)
        };
        self.index
            .as_mut()
            .ok_or_else(|| "Architecture G owner is not ready".to_owned())?
            .reserve_promotion(&new_records, max_resident_nodes, max_resident_bytes)?;
        let observed_rss_bytes = (rss_kib("VmRSS:") as usize).saturating_mul(1024);
        if observed_rss_bytes > max_resident_bytes {
            return Err(format!(
                "Architecture G promotion exceeds observed RSS cap: rss_bytes={observed_rss_bytes},max_bytes={max_resident_bytes}"
            ));
        }
        self.generations
            .get_mut(&id)
            .ok_or_else(|| "Architecture G generation handle is stale".to_owned())?
            .prepared = true;
        Ok((base, candidate, records))
    }

    fn commit(&mut self, id: [u8; 16]) -> Result<Hash, String> {
        let generation = self
            .generations
            .get(&id)
            .ok_or_else(|| "Architecture G generation handle is stale".to_owned())?;
        if !generation.prepared {
            return Err("Architecture G generation was not prepared for promotion".to_owned());
        }
        if self.marker != Some(generation.base_root) {
            return Err("Architecture G promotion marker is stale".to_owned());
        }
        let candidate = generation
            .candidate_root
            .ok_or_else(|| "Architecture G generation candidate is missing".to_owned())?;
        let arena = generation
            .arena
            .as_ref()
            .ok_or_else(|| "Architecture G generation arena is missing".to_owned())?;
        let index = self
            .index
            .as_mut()
            .ok_or_else(|| "Architecture G owner is not ready".to_owned())?;
        let records = generated_reachable_nodes(arena, candidate)?;
        let new_records: Vec<Node> = records
            .into_iter()
            .filter(|record| !index.ids.contains_key(&record.hash()))
            .collect();
        let child_start = index.children.len();
        for record in new_records {
            index.append(record)?;
        }
        index.resolve_child_ids_from(child_start)?;
        index.root = *index.ids.get(&candidate).ok_or_else(|| {
            "Architecture G promoted root is absent from resident index".to_owned()
        })?;
        self.marker = Some(candidate);
        self.generations.remove(&id);
        self.generations.clear();
        Ok(candidate)
    }
}

impl SimulatedOwner {
    fn new(marker: Hash) -> Self {
        Self {
            marker,
            next_handle: 1,
            generations: HashMap::new(),
        }
    }

    fn fork(&mut self, arena: &Arena) -> Result<u64, String> {
        if self.generations.len() >= 2 {
            return Err("Architecture G simulated active-generation cap exceeded".to_owned());
        }
        let handle = self.next_handle;
        self.next_handle = self
            .next_handle
            .checked_add(1)
            .ok_or_else(|| "Architecture G simulated handle overflow".to_owned())?;
        self.generations.insert(
            handle,
            SimulatedGeneration {
                base_root: self.marker,
                arena: arena.clone(),
            },
        );
        Ok(handle)
    }

    fn apply(&mut self, handle: u64, stream: &EventStream) -> Result<Vec<Hash>, String> {
        let generation = self
            .generations
            .get_mut(&handle)
            .ok_or_else(|| "Architecture G simulated generation handle is stale".to_owned())?;
        if generation.base_root != stream.base_root {
            return Err("Architecture G simulated event stream base is stale".to_owned());
        }
        let mut roots = Vec::with_capacity(stream.events.len());
        for event in &stream.events {
            roots.push(generation.arena.apply_event(event)?);
        }
        Ok(roots)
    }

    fn discard(&mut self, handle: u64) -> Result<(), String> {
        self.generations
            .remove(&handle)
            .map(|_| ())
            .ok_or_else(|| "Architecture G simulated generation handle is stale".to_owned())
    }

    fn root(&self, handle: u64) -> Result<Hash, String> {
        let generation = self
            .generations
            .get(&handle)
            .ok_or_else(|| "Architecture G simulated generation handle is stale".to_owned())?;
        Ok(generation
            .arena
            .root
            .map(|id| generation.arena.nodes[id].hash())
            .unwrap_or(EMPTY_ROOT))
    }

    fn promote(&mut self, handle: u64) -> Result<Hash, String> {
        let generation = self
            .generations
            .get(&handle)
            .ok_or_else(|| "Architecture G simulated generation handle is stale".to_owned())?;
        if generation.base_root != self.marker {
            return Err("Architecture G simulated promotion marker is stale".to_owned());
        }
        let candidate = generation
            .arena
            .root
            .map(|id| generation.arena.nodes[id].hash())
            .unwrap_or(EMPTY_ROOT);
        self.generations.remove(&handle);
        self.marker = candidate;
        Ok(candidate)
    }
}

impl FullIndex {
    fn from_payload(payload: &[u8], expected_marker: Hash) -> Result<Self, String> {
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
            root: 0,
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
        index.root = *index
            .ids
            .get(&marker)
            .ok_or_else(|| "Architecture G full index is missing the durable root".to_owned())?;
        index.authenticate_complete_closure()?;
        if index.estimated_bytes() > MAX_RESIDENT_BYTES {
            return Err("Architecture G full index exceeds the 2 GiB resident cap".to_owned());
        }
        Ok(index)
    }

    fn append(&mut self, node: Node) -> Result<(), String> {
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

    fn reserve_promotion(
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

    fn prefix(&self, id: u32) -> &[u8] {
        let node = &self.nodes[id as usize];
        let start = node.prefix_offset as usize;
        &self.prefixes[start..start + node.prefix_length as usize]
    }

    fn child_hash(&self, id: u32, branch: usize) -> Option<Hash> {
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

    fn child_id(&self, id: u32, branch: usize) -> Result<Option<u32>, String> {
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

    fn resolve_child_ids_from(&mut self, start: usize) -> Result<(), String> {
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

    fn authenticate_complete_closure(&self) -> Result<(), String> {
        let mut colors = vec![0u8; self.nodes.len()];
        let mut parents = vec![0u8; self.nodes.len()];
        let mut path = Vec::with_capacity(64);
        let mut visited = 0usize;
        self.visit(
            self.root,
            &mut path,
            &mut colors,
            &mut parents,
            &mut visited,
        )?;
        if visited != self.nodes.len() {
            return Err(format!(
                "Architecture G full index has unreachable records: reachable={},records={}",
                visited,
                self.nodes.len()
            ));
        }
        Ok(())
    }

    fn visit(
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

    fn to_node(&self, id: u32) -> Node {
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

    fn proof_arena(&self, stream: &EventStream) -> Result<Arena, String> {
        if stream.base_root != self.nodes[self.root as usize].hash {
            return Err("Architecture G replay log base root is stale".to_owned());
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
        let mut selected = HashSet::new();
        let candidates: Vec<usize> = (0..touched.len()).collect();
        self.select_proof_union(self.root, 0, &candidates, &touched, &mut selected)?;
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

    fn select_proof_union(
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

    fn subtree_size(&self, id: u32) -> u64 {
        match self.nodes[id as usize].kind {
            CompactKind::Leaf { .. } => 1,
            CompactKind::Branch { size, .. } => size,
        }
    }

    fn diagnostics(&self) -> IndexDiagnostics {
        IndexDiagnostics {
            nodes: self.nodes.len(),
            branches: self.branches,
            leaves: self.leaves,
            edges: self.edges,
            compact_bytes: self.estimated_bytes(),
        }
    }

    fn estimated_bytes(&self) -> usize {
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

fn marker_from_payload(payload: &[u8]) -> Result<Hash, String> {
    if payload.len() < INPUT_HEADER_BYTES || &payload[..4] != INPUT_MAGIC {
        return Err("Architecture G input payload header is invalid".to_owned());
    }
    Ok(payload[40..72].try_into().unwrap())
}

fn sidecar_digest(marker: &Hash, payload: &[u8]) -> Hash {
    digest(&[b"MIDGARD-MPF-ARCH-G-SIDECAR-V1", marker, payload])
}

fn load_sidecar(path: &Path, marker: Hash) -> Result<Vec<u8>, String> {
    let bytes = fs::read(path).map_err(|error| format!("missing:{error}"))?;
    if bytes.len() < SIDECAR_HEADER_BYTES || &bytes[..4] != SIDECAR_MAGIC {
        return Err("corrupt:header".to_owned());
    }
    if u16::from_le_bytes(bytes[4..6].try_into().unwrap()) != ABI_VERSION
        || u16::from_le_bytes(bytes[6..8].try_into().unwrap()) != 0
    {
        return Err("corrupt:version".to_owned());
    }
    let sidecar_marker: Hash = bytes[8..40].try_into().unwrap();
    if sidecar_marker != marker {
        return Err("stale:marker".to_owned());
    }
    let payload_length = u64::from_le_bytes(bytes[40..48].try_into().unwrap()) as usize;
    if bytes.len() != SIDECAR_HEADER_BYTES + payload_length {
        return Err("corrupt:length".to_owned());
    }
    let expected: Hash = bytes[48..80].try_into().unwrap();
    let payload = &bytes[SIDECAR_HEADER_BYTES..];
    if sidecar_digest(&marker, payload) != expected {
        return Err("corrupt:digest".to_owned());
    }
    Ok(payload.to_vec())
}

fn write_sidecar(path: &Path, marker: Hash, payload: &[u8]) -> Result<(), String> {
    let temporary = path.with_extension("tmp");
    let mut file = File::create(&temporary).map_err(|error| error.to_string())?;
    file.write_all(SIDECAR_MAGIC)
        .map_err(|error| error.to_string())?;
    file.write_all(&ABI_VERSION.to_le_bytes())
        .map_err(|error| error.to_string())?;
    file.write_all(&0u16.to_le_bytes())
        .map_err(|error| error.to_string())?;
    file.write_all(&marker).map_err(|error| error.to_string())?;
    file.write_all(&(payload.len() as u64).to_le_bytes())
        .map_err(|error| error.to_string())?;
    file.write_all(&sidecar_digest(&marker, payload))
        .map_err(|error| error.to_string())?;
    file.write_all(payload).map_err(|error| error.to_string())?;
    file.sync_all().map_err(|error| error.to_string())?;
    fs::rename(&temporary, path).map_err(|error| error.to_string())?;
    Ok(())
}

fn rss_kib(field: &str) -> u64 {
    fs::read_to_string("/proc/self/status")
        .ok()
        .and_then(|status| {
            status.lines().find_map(|line| {
                line.strip_prefix(field)
                    .and_then(|value| value.split_whitespace().next())
                    .and_then(|value| value.parse().ok())
            })
        })
        .unwrap_or(0)
}

fn argument(name: &str) -> Result<PathBuf, String> {
    let prefix = format!("--{name}=");
    env::args()
        .find_map(|argument| argument.strip_prefix(&prefix).map(PathBuf::from))
        .ok_or_else(|| format!("Missing --{name}=..."))
}

fn mode() -> String {
    env::args()
        .find_map(|argument| argument.strip_prefix("--mode=").map(str::to_owned))
        .unwrap_or_else(|| "prepare".to_owned())
}

pub fn run_owner_cli() -> Result<(), String> {
    let input_path = argument("input")?;
    let sidecar_path = argument("sidecar")?;
    let events_path = argument("events")?;
    let run_mode = mode();
    if run_mode != "prepare" && run_mode != "recover" {
        return Err("Architecture G owner mode must be prepare or recover".to_owned());
    }
    let input_marker = {
        let mut header = [0u8; INPUT_HEADER_BYTES];
        File::open(&input_path)
            .and_then(|mut file| file.read_exact(&mut header))
            .map_err(|error| error.to_string())?;
        marker_from_payload(&header)?
    };
    let startup_started_at = Instant::now();
    let (payload, source, rebuild_reason) = match load_sidecar(&sidecar_path, input_marker) {
        Ok(payload) => (payload, "sidecar", "none".to_owned()),
        Err(reason) => {
            let payload = fs::read(&input_path).map_err(|error| error.to_string())?;
            if marker_from_payload(&payload)? != input_marker {
                return Err("Architecture G input marker changed while loading".to_owned());
            }
            (payload, "level-export", reason)
        }
    };
    let index = FullIndex::from_payload(&payload, input_marker)?;
    if source == "level-export" {
        write_sidecar(&sidecar_path, input_marker, &payload)?;
    }
    let startup_ms = startup_started_at.elapsed().as_secs_f64() * 1_000.0;
    let rss_after_startup = rss_kib("VmRSS:");
    let peak_after_startup = rss_kib("VmHWM:");
    let diagnostics = index.diagnostics();
    drop(payload);

    let event_bytes = fs::read(&events_path).map_err(|error| error.to_string())?;
    let stream = parse_event_stream(&event_bytes)?;
    let simulation_started_at = Instant::now();
    let base_arena = index.proof_arena(&stream)?;
    let proof_nodes = base_arena.nodes.len();
    let mut simulated_owner = SimulatedOwner::new(stream.base_root);
    let promoted_handle = simulated_owner.fork(&base_arena)?;
    let replay_handle = simulated_owner.fork(&base_arena)?;
    let active_generation_cap_rejected = simulated_owner.fork(&base_arena).is_err();
    if !active_generation_cap_rejected {
        return Err("Architecture G simulated generation cap did not fail closed".to_owned());
    }
    let roots = simulated_owner.apply(promoted_handle, &stream)?;
    let candidate = roots.last().copied().unwrap_or(stream.base_root);
    let generated_nodes = simulated_owner.generations[&promoted_handle]
        .arena
        .nodes
        .len()
        .saturating_sub(
            simulated_owner.generations[&promoted_handle]
                .arena
                .base_count,
        );
    let replay_roots = simulated_owner.apply(replay_handle, &stream)?;
    if replay_roots != roots {
        return Err("Architecture G crash replay roots diverged".to_owned());
    }
    simulated_owner.discard(replay_handle)?;
    let discarded_handle_rejected = simulated_owner.root(replay_handle).is_err();
    let stale_handle = simulated_owner.fork(&base_arena)?;
    if simulated_owner.promote(promoted_handle)? != candidate {
        return Err("Architecture G simulated promotion root diverged".to_owned());
    }
    let stale_generation_rejected = simulated_owner.promote(stale_handle).is_err();
    simulated_owner.discard(stale_handle)?;
    if !discarded_handle_rejected || !stale_generation_rejected {
        return Err("Architecture G simulated stale/discard rejection failed".to_owned());
    }
    let root_bytes: Vec<u8> = roots.iter().flatten().copied().collect();
    let replay_digest = digest(&[
        b"MIDGARD-MPF-ARCH-G-REPLAY-V1",
        &stream.base_root,
        &candidate,
        &root_bytes,
        &event_bytes,
    ]);
    let simulation_ms = simulation_started_at.elapsed().as_secs_f64() * 1_000.0;
    let steady_rss = rss_kib("VmRSS:");
    let peak_rss = rss_kib("VmHWM:");
    if peak_rss > 2 * 1024 * 1024 {
        return Err(format!(
            "Architecture G owner exceeded 2 GiB RSS: peak_kib={peak_rss}"
        ));
    }
    println!(
        concat!(
            "{{\"mode\":\"{}\",\"source\":\"{}\",\"rebuildReason\":\"{}\",",
            "\"marker\":\"{}\",\"candidateRoot\":\"{}\",\"replayDigest\":\"{}\",",
            "\"startupMs\":{},\"simulationMs\":{},\"nodes\":{},\"branches\":{},",
            "\"leaves\":{},\"edges\":{},\"compactBytes\":{},\"proofNodes\":{},",
            "\"eventCount\":{},\"generatedNodes\":{},\"rssAfterStartupKiB\":{},",
            "\"peakAfterStartupKiB\":{},\"steadyRssKiB\":{},\"peakRssKiB\":{},",
            "\"rootsExactOnReplay\":true,\"discardedHandleRejected\":{},",
            "\"staleGenerationRejected\":{},\"activeGenerationCapRejected\":{},",
            "\"fixtureWrites\":0}}"
        ),
        run_mode,
        source,
        rebuild_reason,
        hex(&input_marker),
        hex(&candidate),
        hex(&replay_digest),
        startup_ms,
        simulation_ms,
        diagnostics.nodes,
        diagnostics.branches,
        diagnostics.leaves,
        diagnostics.edges,
        diagnostics.compact_bytes,
        proof_nodes,
        stream.events.len(),
        generated_nodes,
        rss_after_startup,
        peak_after_startup,
        steady_rss,
        peak_rss,
        discarded_handle_rejected,
        stale_generation_rejected,
        active_generation_cap_rejected,
    );
    Ok(())
}

fn encode_node_record(node: &Node) -> Result<Vec<u8>, String> {
    let prefix = node.prefix();
    let prefix_length = u8::try_from(prefix.len())
        .map_err(|_| "Architecture G record prefix exceeds u8".to_owned())?;
    let mut output = Vec::new();
    match node {
        Node::Leaf {
            hash, key, value, ..
        } => {
            output.push(1);
            output.extend_from_slice(hash);
            output.push(prefix_length);
            output.extend_from_slice(prefix);
            output.extend_from_slice(
                &u16::try_from(key.len())
                    .map_err(|_| "Architecture G record key exceeds u16".to_owned())?
                    .to_le_bytes(),
            );
            output.extend_from_slice(
                &u32::try_from(value.len())
                    .map_err(|_| "Architecture G record value exceeds u32".to_owned())?
                    .to_le_bytes(),
            );
            output.extend_from_slice(key);
            output.extend_from_slice(value);
        }
        Node::Branch {
            hash,
            children,
            size,
            ..
        } => {
            output.push(2);
            output.extend_from_slice(hash);
            output.push(prefix_length);
            output.extend_from_slice(prefix);
            output.extend_from_slice(&size.to_le_bytes());
            let bitmap = children
                .iter()
                .enumerate()
                .fold(0u16, |bits, (index, child)| {
                    bits | if child.is_some() { 1 << index } else { 0 }
                });
            output.extend_from_slice(&bitmap.to_le_bytes());
            for child in children.iter().flatten() {
                output.extend_from_slice(child);
            }
        }
    }
    Ok(output)
}

fn generated_reachable_nodes(arena: &Arena, candidate: Hash) -> Result<Vec<Node>, String> {
    let root = arena.resolve(&candidate)?;
    let mut pending = vec![root];
    let mut visited = HashSet::new();
    let mut generated = Vec::new();
    while let Some(id) = pending.pop() {
        if !visited.insert(id) {
            continue;
        }
        let node = &arena.nodes[id];
        if let Node::Branch { children, .. } = node {
            for hash in children.iter().flatten() {
                if let Some(child) = arena.ids.get(hash) {
                    pending.push(*child);
                }
            }
        }
        if id >= arena.base_count {
            generated.push(node.clone());
        }
    }
    generated.sort_unstable_by_key(Node::hash);
    Ok(generated)
}

fn generated_reachable_records(arena: &Arena, candidate: Hash) -> Result<Vec<Vec<u8>>, String> {
    generated_reachable_nodes(arena, candidate)?
        .iter()
        .map(encode_node_record)
        .collect()
}

fn read_hash(bytes: &[u8], field: &str) -> Result<Hash, String> {
    bytes
        .try_into()
        .map_err(|_| format!("Architecture G {field} must contain exactly 32 bytes"))
}

fn read_id(bytes: &[u8], field: &str) -> Result<[u8; 16], String> {
    bytes
        .try_into()
        .map_err(|_| format!("Architecture G {field} must contain exactly 16 bytes"))
}

fn send_rpc(
    writer: &mut impl Write,
    epoch: [u8; 16],
    request_id: u64,
    kind: RpcKind,
    payload: Vec<u8>,
) -> Result<(), String> {
    write_frame(
        writer,
        &RpcFrame {
            kind,
            request_id,
            owner_epoch: epoch,
            payload,
        },
    )
}

fn handle_rpc_frame(
    owner: &mut RuntimeOwner,
    writer: &mut impl Write,
    frame: RpcFrame,
) -> Result<bool, String> {
    match frame.kind {
        RpcKind::Hello => {
            if frame.payload.len() != 32 {
                return Err(
                    "Architecture G Hello payload must contain the pinned binary SHA-256"
                        .to_owned(),
                );
            }
            let mut payload = Vec::with_capacity(82);
            payload.extend_from_slice(&1u16.to_le_bytes());
            payload.extend_from_slice(&frame.payload);
            payload.extend_from_slice(&(FULL_INDEX_MAX_RECORDS as u32).to_le_bytes());
            payload.extend_from_slice(&(ABSOLUTE_MAX_EVENTS as u32).to_le_bytes());
            payload.extend_from_slice(&(ABSOLUTE_MAX_OPS as u32).to_le_bytes());
            payload.extend_from_slice(&2u32.to_le_bytes());
            payload.extend_from_slice(&digest(&[b"MIDGARD-MPF-OWNER-BLAKE2B-SELFTEST-V1"]));
            send_rpc(
                writer,
                owner.epoch,
                frame.request_id,
                RpcKind::HelloAck,
                payload,
            )?;
        }
        RpcKind::LoadBegin => {
            if frame.payload.len() != INPUT_HEADER_BYTES {
                return Err(
                    "Architecture G LoadBegin must contain the full index header".to_owned(),
                );
            }
            marker_from_payload(&frame.payload)?;
            owner.load_payload.clear();
            owner.load_payload.extend_from_slice(&frame.payload);
            send_rpc(
                writer,
                owner.epoch,
                frame.request_id,
                RpcKind::LoadBegin,
                Vec::new(),
            )?;
        }
        RpcKind::LoadChunk => {
            if frame.payload.is_empty() || frame.payload.len() > RPC_MAX_CHUNK_BYTES {
                return Err("Architecture G LoadChunk size is invalid".to_owned());
            }
            let next = owner
                .load_payload
                .len()
                .checked_add(frame.payload.len())
                .ok_or_else(|| "Architecture G load length overflow".to_owned())?;
            if next > FULL_INDEX_MAX_BYTES {
                return Err("Architecture G full index exceeds load cap".to_owned());
            }
            owner.load_payload.extend_from_slice(&frame.payload);
            send_rpc(
                writer,
                owner.epoch,
                frame.request_id,
                RpcKind::LoadChunk,
                Vec::new(),
            )?;
        }
        RpcKind::LoadEnd => {
            if frame.payload.len() != 32 {
                return Err("Architecture G LoadEnd digest size is invalid".to_owned());
            }
            let expected = digest(&[b"MIDGARD-MPF-OWNER-LOAD-V1", owner.load_payload.as_slice()]);
            if frame.payload != expected {
                return Err("Architecture G full-index aggregate digest mismatch".to_owned());
            }
            let marker = marker_from_payload(&owner.load_payload)?;
            let index = FullIndex::from_payload(&owner.load_payload, marker)?;
            let diagnostics = index.diagnostics();
            owner.marker = Some(marker);
            owner.index = Some(index);
            owner.load_payload.clear();
            let mut payload = Vec::with_capacity(72);
            payload.extend_from_slice(&marker);
            for value in [
                diagnostics.nodes as u64,
                diagnostics.edges as u64,
                diagnostics.compact_bytes as u64,
                rss_kib("VmRSS:"),
                rss_kib("VmHWM:"),
            ] {
                payload.extend_from_slice(&value.to_le_bytes());
            }
            send_rpc(
                writer,
                owner.epoch,
                frame.request_id,
                RpcKind::Ready,
                payload,
            )?;
        }
        RpcKind::Fork => {
            let base_root = read_hash(&frame.payload, "fork base root")?;
            let id = owner.fork(base_root)?;
            let mut payload = Vec::with_capacity(48);
            payload.extend_from_slice(&id);
            payload.extend_from_slice(&base_root);
            send_rpc(
                writer,
                owner.epoch,
                frame.request_id,
                RpcKind::Forked,
                payload,
            )?;
        }
        RpcKind::ApplyEvents => {
            if frame.payload.len() <= 16 {
                return Err("Architecture G ApplyEvents payload is truncated".to_owned());
            }
            let id = read_id(&frame.payload[..16], "generation id")?;
            let event_bytes = &frame.payload[16..];
            let (candidate, roots, proof_duration_ns, mutation_duration_ns) =
                owner.apply(id, event_bytes)?;
            let event_digest = digest(&[b"MIDGARD-MPF-ARCH-G-EVENT-LOG-V1", event_bytes]);
            let mut payload = Vec::with_capacity(100 + roots.len() * 32);
            payload.extend_from_slice(&id);
            payload.extend_from_slice(&candidate);
            payload.extend_from_slice(&event_digest);
            payload.extend_from_slice(&(roots.len() as u32).to_le_bytes());
            for root in roots {
                payload.extend_from_slice(&root);
            }
            payload.extend_from_slice(&proof_duration_ns.to_le_bytes());
            payload.extend_from_slice(&mutation_duration_ns.to_le_bytes());
            send_rpc(
                writer,
                owner.epoch,
                frame.request_id,
                RpcKind::Applied,
                payload,
            )?;
        }
        RpcKind::Discard => {
            let id = read_id(&frame.payload, "generation id")?;
            owner
                .generations
                .remove(&id)
                .ok_or_else(|| "Architecture G generation handle is stale".to_owned())?;
            send_rpc(
                writer,
                owner.epoch,
                frame.request_id,
                RpcKind::Discarded,
                id.to_vec(),
            )?;
        }
        RpcKind::PreparePromotion => {
            let id = read_id(&frame.payload, "generation id")?;
            let (base, candidate, records) = owner.generated_records(id)?;
            let mut aggregate = Blake2bVar::new(32).expect("valid BLAKE2b length");
            aggregate.update(b"MIDGARD-MPF-OWNER-PROMOTION-V1");
            aggregate.update(&base);
            aggregate.update(&candidate);
            let mut chunk = Vec::new();
            for record in &records {
                aggregate.update(record);
                if chunk.len() + record.len() > RPC_MAX_CHUNK_BYTES && !chunk.is_empty() {
                    send_rpc(
                        writer,
                        owner.epoch,
                        frame.request_id,
                        RpcKind::PromotionChunk,
                        std::mem::take(&mut chunk),
                    )?;
                }
                if record.len() > RPC_MAX_CHUNK_BYTES {
                    return Err("Architecture G promotion record exceeds chunk cap".to_owned());
                }
                chunk.extend_from_slice(record);
            }
            if !chunk.is_empty() {
                send_rpc(
                    writer,
                    owner.epoch,
                    frame.request_id,
                    RpcKind::PromotionChunk,
                    chunk,
                )?;
            }
            let mut aggregate_digest = [0u8; 32];
            aggregate
                .finalize_variable(&mut aggregate_digest)
                .expect("valid BLAKE2b output");
            let mut payload = Vec::with_capacity(116);
            payload.extend_from_slice(&id);
            payload.extend_from_slice(&base);
            payload.extend_from_slice(&candidate);
            payload.extend_from_slice(&(records.len() as u32).to_le_bytes());
            payload.extend_from_slice(&aggregate_digest);
            send_rpc(
                writer,
                owner.epoch,
                frame.request_id,
                RpcKind::PromotionEnd,
                payload,
            )?;
        }
        RpcKind::PromotionCommitted => {
            let id = read_id(&frame.payload, "generation id")?;
            let candidate = owner.commit(id)?;
            send_rpc(
                writer,
                owner.epoch,
                frame.request_id,
                RpcKind::PromotionCommitted,
                candidate.to_vec(),
            )?;
        }
        RpcKind::Diagnostics => {
            if !frame.payload.is_empty() {
                return Err("Architecture G Diagnostics payload must be empty".to_owned());
            }
            let index = owner
                .index
                .as_ref()
                .ok_or_else(|| "Architecture G owner is not ready".to_owned())?;
            let diagnostics = index.diagnostics();
            let marker = owner.marker.unwrap();
            let generated_nodes = owner
                .generations
                .values()
                .filter_map(|generation| generation.arena.as_ref())
                .map(|arena| arena.nodes.len().saturating_sub(arena.base_count))
                .sum::<usize>();
            let generated_bytes = owner
                .generations
                .values()
                .filter_map(|generation| {
                    Some((generation.arena.as_ref()?, generation.candidate_root?))
                })
                .map(|(arena, candidate)| {
                    generated_reachable_records(arena, candidate)
                        .map(|records| records.iter().map(Vec::len).sum::<usize>())
                })
                .collect::<Result<Vec<_>, _>>()?
                .into_iter()
                .sum::<usize>();
            let mut payload = Vec::with_capacity(96);
            payload.extend_from_slice(&marker);
            for value in [
                diagnostics.nodes as u64,
                diagnostics.edges as u64,
                diagnostics.compact_bytes as u64,
                owner.generations.len() as u64,
                generated_nodes as u64,
                generated_bytes as u64,
                rss_kib("VmRSS:"),
                rss_kib("VmHWM:"),
            ] {
                payload.extend_from_slice(&value.to_le_bytes());
            }
            send_rpc(
                writer,
                owner.epoch,
                frame.request_id,
                RpcKind::DiagnosticsResult,
                payload,
            )?;
        }
        RpcKind::Ping => send_rpc(
            writer,
            owner.epoch,
            frame.request_id,
            RpcKind::Pong,
            frame.payload,
        )?,
        RpcKind::Shutdown => {
            if !frame.payload.is_empty() {
                return Err("Architecture G Shutdown payload must be empty".to_owned());
            }
            send_rpc(
                writer,
                owner.epoch,
                frame.request_id,
                RpcKind::ShutdownAck,
                Vec::new(),
            )?;
            return Ok(false);
        }
        _ => {
            return Err(format!(
                "Unexpected Architecture G request kind {:?}",
                frame.kind
            ))
        }
    }
    Ok(true)
}

pub fn run_owner_rpc() -> Result<(), String> {
    let mut epoch = [0u8; 16];
    File::open("/dev/urandom")
        .and_then(|mut file| file.read_exact(&mut epoch))
        .map_err(|error| format!("Architecture G owner epoch generation failed: {error}"))?;
    let stdin = std::io::stdin();
    let stdout = std::io::stdout();
    let mut reader = stdin.lock();
    let mut writer = stdout.lock();
    let mut owner = RuntimeOwner::new(epoch);
    let mut last_request_id = 0u64;
    let mut handshaken = false;
    while let Some(frame) = read_frame(&mut reader)? {
        if frame.request_id <= last_request_id {
            return Err("Native MPF RPC request id is duplicate or out of order".to_owned());
        }
        last_request_id = frame.request_id;
        if !handshaken {
            if frame.kind != RpcKind::Hello || frame.owner_epoch != [0; 16] {
                return Err(
                    "Native MPF RPC first request must be Hello with the zero epoch".to_owned(),
                );
            }
            handshaken = true;
        } else if frame.owner_epoch != owner.epoch {
            return Err("Native MPF RPC owner epoch mismatch".to_owned());
        }
        let request_id = frame.request_id;
        match handle_rpc_frame(&mut owner, &mut writer, frame) {
            Ok(true) => {}
            Ok(false) => return Ok(()),
            Err(error) => {
                send_rpc(
                    &mut writer,
                    owner.epoch,
                    request_id,
                    RpcKind::Error,
                    error.as_bytes().to_vec(),
                )?;
            }
        }
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    fn leaf(hash: Hash, key_byte: u8, value_byte: u8) -> Node {
        Node::Leaf {
            hash,
            prefix: vec![key_byte & 0x0f],
            key: vec![key_byte; 32],
            value: vec![value_byte; 64],
        }
    }

    fn empty_index() -> FullIndex {
        FullIndex {
            nodes: Vec::new(),
            ids: HashMap::new(),
            prefixes: Vec::new(),
            children: Vec::new(),
            child_ids: Vec::new(),
            branch_merkle: Vec::new(),
            keys: Vec::new(),
            values: Vec::new(),
            root: 0,
            branches: 0,
            leaves: 0,
            edges: 0,
        }
    }

    fn index_with_root(root: Node) -> FullIndex {
        let mut index = empty_index();
        index.append(root).unwrap();
        index
    }

    fn key_with_first_nibble(target: u8) -> Vec<u8> {
        for candidate in 0u32.. {
            let mut key = vec![0u8; 32];
            key[28..].copy_from_slice(&candidate.to_be_bytes());
            if path_nibbles(&key)[0] == target {
                return key;
            }
        }
        unreachable!("every nibble has a preimage")
    }

    fn base_arena(keys: &[Vec<u8>]) -> (Arena, Hash) {
        let mut arena = Arena::new();
        for (index, key) in keys.iter().enumerate() {
            arena
                .apply_event(&[Op::Insert {
                    key: key.clone(),
                    value: vec![u8::try_from(index + 1).unwrap(); 64],
                }])
                .unwrap();
        }
        let root = arena.nodes[arena.root.unwrap()].hash();
        (arena, root)
    }

    fn full_index(arena: &Arena, marker: Hash) -> FullIndex {
        let mut index = empty_index();
        for node in arena.dirty_records() {
            index.append(node.clone()).unwrap();
        }
        index.root = *index.ids.get(&marker).unwrap();
        index.resolve_child_ids_from(0).unwrap();
        index.authenticate_complete_closure().unwrap();
        index
    }

    #[test]
    fn proof_union_skips_delete_siblings_when_two_children_must_survive() {
        let keys: Vec<Vec<u8>> = (0..4).map(key_with_first_nibble).collect();
        let (base, marker) = base_arena(&keys);
        let index = full_index(&base, marker);
        let stream = EventStream {
            base_root: marker,
            events: vec![vec![Op::Delete {
                key: keys[0].clone(),
            }]],
        };

        let mut proof = index.proof_arena(&stream).unwrap();
        assert_eq!(proof.base_count, 2, "root plus the deleted leaf only");
        let observed = proof.apply_event(&stream.events[0]).unwrap();
        let mut reference = base.clone();
        let expected = reference.apply_event(&stream.events[0]).unwrap();
        assert_eq!(observed, expected);
    }

    #[test]
    fn proof_union_retains_sibling_when_delete_can_collapse_branch() {
        let keys: Vec<Vec<u8>> = (0..2).map(key_with_first_nibble).collect();
        let (base, marker) = base_arena(&keys);
        let index = full_index(&base, marker);
        let stream = EventStream {
            base_root: marker,
            events: vec![vec![Op::Delete {
                key: keys[0].clone(),
            }]],
        };

        let mut proof = index.proof_arena(&stream).unwrap();
        assert_eq!(proof.base_count, 3, "root plus both leaves");
        let observed = proof.apply_event(&stream.events[0]).unwrap();
        let mut reference = base.clone();
        let expected = reference.apply_event(&stream.events[0]).unwrap();
        assert_eq!(observed, expected);
    }

    #[test]
    fn child_id_cache_resolves_children_added_after_parent_records() {
        let child_hash = [2u8; 32];
        let parent_hash = [3u8; 32];
        let mut children = [None; 16];
        children[0] = Some(child_hash);
        let parent = Node::Branch {
            hash: parent_hash,
            prefix: Vec::new(),
            children,
            size: 1,
            merkle: [[0u8; 32]; 15],
        };
        let child = leaf(child_hash, 1, 11);
        let mut index = empty_index();
        index.append(parent).unwrap();
        assert!(index.child_id(0, 0).is_err());
        assert_eq!(index.child_ids, vec![UNRESOLVED_CHILD_ID]);

        index.append(child).unwrap();
        index.resolve_child_ids_from(0).unwrap();
        assert_eq!(index.child_id(0, 0).unwrap(), Some(1));
        assert_eq!(index.child_ids, vec![1]);
        assert!(index.estimated_bytes() >= index.children.capacity() * size_of::<Hash>());
    }

    #[test]
    fn promotion_cap_rejection_preserves_marker_and_generation_for_retry() {
        let base_hash = [1u8; 32];
        let candidate_hash = [2u8; 32];
        let base = leaf(base_hash, 1, 11);
        let candidate = leaf(candidate_hash, 2, 22);
        let mut arena = Arena::new();
        arena.append(base.clone()).unwrap();
        arena.base_count = arena.nodes.len();
        let candidate_id = arena.append(candidate).unwrap();
        arena.root = Some(candidate_id);
        let generation_id = [9u8; 16];
        let mut generations = HashMap::new();
        generations.insert(
            generation_id,
            RuntimeGeneration {
                base_root: base_hash,
                arena: Some(arena),
                candidate_root: Some(candidate_hash),
                prepared: false,
            },
        );
        let mut owner = RuntimeOwner {
            epoch: [7u8; 16],
            marker: Some(base_hash),
            index: Some(index_with_root(base)),
            load_payload: Vec::new(),
            generations,
            next_generation: 1,
        };
        let resident_bytes_before = owner.index.as_ref().unwrap().estimated_bytes();

        let error = owner
            .generated_records_with_caps(generation_id, 1, MAX_RESIDENT_BYTES)
            .unwrap_err();
        assert!(error.contains("projected_nodes=2"), "{error}");
        assert!(error.contains("projected_bytes="), "{error}");
        assert_eq!(owner.marker, Some(base_hash));
        assert_eq!(owner.index.as_ref().unwrap().nodes.len(), 1);
        assert_eq!(
            owner.index.as_ref().unwrap().estimated_bytes(),
            resident_bytes_before
        );
        assert!(!owner.generations[&generation_id].prepared);

        owner
            .generated_records_with_caps(generation_id, 2, MAX_RESIDENT_BYTES)
            .unwrap();
        assert!(owner.generations[&generation_id].prepared);
        assert_eq!(owner.commit(generation_id).unwrap(), candidate_hash);
        assert_eq!(owner.marker, Some(candidate_hash));
        assert_eq!(owner.index.as_ref().unwrap().nodes.len(), 2);
        assert!(owner.generations.is_empty());
    }
}
