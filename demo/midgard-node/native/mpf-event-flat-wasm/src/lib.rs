use std::collections::{HashMap, HashSet};

use blake2::{
    digest::{Update, VariableOutput},
    Blake2bVar,
};
use wasm_bindgen::prelude::*;

#[cfg(not(target_arch = "wasm32"))]
mod owner;

#[cfg(not(target_arch = "wasm32"))]
mod rpc;

#[cfg(not(target_arch = "wasm32"))]
pub use owner::{run_owner_cli, run_owner_rpc};

type Hash = [u8; 32];

const INPUT_MAGIC: &[u8; 4] = b"MEF6";
const OUTPUT_MAGIC: &[u8; 4] = b"MEFO";
const EVENT_STREAM_MAGIC: &[u8; 4] = b"MEGO";
const ROOT_STREAM_MAGIC: &[u8; 4] = b"MEGR";
const ABI_VERSION: u16 = 1;
const INPUT_HEADER_BYTES: usize = 72;
const OUTPUT_HEADER_BYTES: usize = 120;
const EMPTY_ROOT: Hash = [
    0x0e, 0x57, 0x51, 0xc0, 0x26, 0xe5, 0x43, 0xb2, 0xe8, 0xab, 0x2e, 0xb0, 0x60, 0x99, 0xda, 0xa1,
    0xd1, 0xe5, 0xdf, 0x47, 0x77, 0x8f, 0x77, 0x87, 0xfa, 0xab, 0x45, 0xcd, 0xf1, 0x2f, 0xe3, 0xa8,
];
const ZERO_HASH: Hash = [0; 32];
const ABSOLUTE_MAX_RECORDS: usize = 1_000_000;
const ABSOLUTE_MAX_EVENTS: usize = 100_000;
const ABSOLUTE_MAX_OPS: usize = 400_000;
const ABSOLUTE_MAX_INPUT_BYTES: usize = 536_870_912;
const ABSOLUTE_MAX_OUTPUT_BYTES: usize = 536_870_912;
const ABSOLUTE_MAX_ARENA_NODES: usize = 1_000_000;
const MAX_SAFE_TRIE_SIZE: u64 = 9_007_199_254_740_991;

#[derive(Clone, Debug, PartialEq, Eq)]
enum Node {
    Leaf {
        hash: Hash,
        prefix: Vec<u8>,
        key: Vec<u8>,
        value: Vec<u8>,
    },
    Branch {
        hash: Hash,
        prefix: Vec<u8>,
        children: [Option<Hash>; 16],
        size: u64,
        merkle: [Hash; 15],
    },
}

impl Node {
    fn hash(&self) -> Hash {
        match self {
            Self::Leaf { hash, .. } | Self::Branch { hash, .. } => *hash,
        }
    }

    fn prefix(&self) -> &[u8] {
        match self {
            Self::Leaf { prefix, .. } | Self::Branch { prefix, .. } => prefix,
        }
    }
}

#[derive(Clone, Debug)]
enum Op {
    Insert { key: Vec<u8>, value: Vec<u8> },
    Delete { key: Vec<u8> },
}

#[derive(Clone, Copy)]
struct Caps {
    records: usize,
    events: usize,
    ops: usize,
    input_bytes: usize,
    output_bytes: usize,
}

struct Reader<'a> {
    bytes: &'a [u8],
    offset: usize,
}

impl<'a> Reader<'a> {
    fn new(bytes: &'a [u8]) -> Self {
        Self { bytes, offset: 0 }
    }

    fn remaining(&self) -> usize {
        self.bytes.len().saturating_sub(self.offset)
    }

    fn take(&mut self, length: usize) -> Result<&'a [u8], String> {
        let end = self
            .offset
            .checked_add(length)
            .ok_or_else(|| "Architecture F input offset overflow".to_owned())?;
        if end > self.bytes.len() {
            return Err("Architecture F input is truncated".to_owned());
        }
        let value = &self.bytes[self.offset..end];
        self.offset = end;
        Ok(value)
    }

    fn u8(&mut self) -> Result<u8, String> {
        Ok(self.take(1)?[0])
    }

    fn u16(&mut self) -> Result<u16, String> {
        Ok(u16::from_le_bytes(self.take(2)?.try_into().unwrap()))
    }

    fn u32(&mut self) -> Result<u32, String> {
        Ok(u32::from_le_bytes(self.take(4)?.try_into().unwrap()))
    }

    fn u64(&mut self) -> Result<u64, String> {
        Ok(u64::from_le_bytes(self.take(8)?.try_into().unwrap()))
    }

    fn hash(&mut self) -> Result<Hash, String> {
        Ok(self.take(32)?.try_into().unwrap())
    }
}

fn digest(parts: &[&[u8]]) -> Hash {
    let mut hasher = Blake2bVar::new(32).expect("BLAKE2b-256 output size is valid");
    for part in parts {
        hasher.update(part);
    }
    let mut output = [0u8; 32];
    hasher
        .finalize_variable(&mut output)
        .expect("BLAKE2b-256 output buffer is valid");
    output
}

fn path_nibbles(key: &[u8]) -> Vec<u8> {
    digest(&[key])
        .into_iter()
        .flat_map(|byte| [byte >> 4, byte & 0x0f])
        .collect()
}

fn packed_nibbles(nibbles: &[u8]) -> Result<Vec<u8>, String> {
    if nibbles.iter().any(|nibble| *nibble > 0x0f) || nibbles.len() % 2 != 0 {
        return Err("Architecture F leaf tail is not canonical nibbles".to_owned());
    }
    Ok(nibbles
        .chunks_exact(2)
        .map(|pair| (pair[0] << 4) | pair[1])
        .collect())
}

fn leaf_hash(prefix: &[u8], value: &[u8]) -> Result<Hash, String> {
    let (head, tail) = if prefix.len() % 2 == 1 {
        (vec![0, prefix[0]], packed_nibbles(&prefix[1..])?)
    } else {
        (vec![0xff], packed_nibbles(prefix)?)
    };
    let value_hash = digest(&[value]);
    Ok(digest(&[&head, &tail, &value_hash]))
}

fn branch_merkle(children: &[Option<Hash>; 16]) -> [Hash; 15] {
    let mut nodes = [[0u8; 32]; 31];
    for (index, child) in children.iter().enumerate() {
        nodes[15 + index] = child.unwrap_or(ZERO_HASH);
    }
    for index in (0..15).rev() {
        nodes[index] = digest(&[&nodes[index * 2 + 1], &nodes[index * 2 + 2]]);
    }
    nodes[..15].try_into().unwrap()
}

fn branch_hash(prefix: &[u8], merkle: &[Hash; 15]) -> Hash {
    digest(&[prefix, &merkle[0]])
}

fn common_prefix_length(left: &[u8], right: &[u8]) -> usize {
    left.iter()
        .zip(right)
        .take_while(|(left, right)| left == right)
        .count()
}

#[derive(Clone)]
struct Arena {
    nodes: Vec<Node>,
    ids: HashMap<Hash, usize>,
    root: Option<usize>,
    base_count: usize,
}

impl Arena {
    fn new() -> Self {
        Self {
            nodes: Vec::new(),
            ids: HashMap::new(),
            root: None,
            base_count: 0,
        }
    }

    fn append(&mut self, node: Node) -> Result<usize, String> {
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

    fn append_leaf(
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

    fn append_branch(
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

    fn append_updated_branch(
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

    fn resolve(&self, hash: &Hash) -> Result<usize, String> {
        self.ids.get(hash).copied().ok_or_else(|| {
            format!(
                "Architecture F mutation crossed unavailable frontier {}",
                hex(hash)
            )
        })
    }

    fn insert_at(
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

    fn delete_at(
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

    fn apply_event(&mut self, ops: &[Op]) -> Result<Hash, String> {
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

    fn assert_base_closure(&self, base_root: Hash) -> Result<(), String> {
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

    fn dirty_records(&self) -> Vec<&Node> {
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

    fn rollback(&mut self, node_count: usize, root: Option<usize>) {
        self.nodes.truncate(node_count);
        self.ids.retain(|_, id| *id < node_count);
        self.root = root;
    }
}

struct EventStream {
    base_root: Hash,
    events: Vec<Vec<Op>>,
}

fn parse_event_stream(input: &[u8]) -> Result<EventStream, String> {
    if input.len() < 92 || &input[..4] != EVENT_STREAM_MAGIC {
        return Err("Architecture G event stream magic/header is invalid".to_owned());
    }
    let mut reader = Reader::new(input);
    reader.take(4)?;
    if reader.u16()? != ABI_VERSION || reader.u16()? != 0 {
        return Err("Architecture G event stream version/flags are invalid".to_owned());
    }
    let event_count = reader.u32()? as usize;
    let op_count = reader.u32()? as usize;
    let max_events = reader.u32()? as usize;
    let max_ops = reader.u32()? as usize;
    let max_input_bytes = reader.u32()? as usize;
    if max_events > ABSOLUTE_MAX_EVENTS
        || max_ops > ABSOLUTE_MAX_OPS
        || max_input_bytes > ABSOLUTE_MAX_INPUT_BYTES
        || event_count > max_events
        || op_count > max_ops
        || input.len() > max_input_bytes
    {
        return Err("Architecture G event stream exceeds a caller/absolute cap".to_owned());
    }
    let base_root = reader.hash()?;
    let expected_digest = reader.hash()?;
    let actual_digest = digest(&[
        b"MIDGARD-MPF-ARCH-G-EVENTS-V1",
        &input[8..28],
        &base_root,
        &input[92..],
    ]);
    if actual_digest != expected_digest {
        return Err("Architecture G event stream digest mismatch".to_owned());
    }
    let mut events = Vec::with_capacity(event_count);
    let mut parsed_ops = 0usize;
    for _ in 0..event_count {
        let event_ops = reader.u32()? as usize;
        parsed_ops = parsed_ops
            .checked_add(event_ops)
            .ok_or_else(|| "Architecture G op count overflow".to_owned())?;
        if parsed_ops > op_count {
            return Err("Architecture G event ops exceed declared count".to_owned());
        }
        let mut event = Vec::with_capacity(event_ops);
        for _ in 0..event_ops {
            let kind = reader.u8()?;
            let key_length = reader.u16()? as usize;
            let value_length = reader.u32()? as usize;
            let key = reader.take(key_length)?.to_vec();
            let value = reader.take(value_length)?.to_vec();
            match kind {
                1 => event.push(Op::Insert { key, value }),
                2 if value.is_empty() => event.push(Op::Delete { key }),
                _ => return Err("Architecture G op kind/value shape is invalid".to_owned()),
            }
        }
        events.push(event);
    }
    if parsed_ops != op_count || reader.remaining() != 0 {
        return Err("Architecture G declared counts/trailing bytes mismatch".to_owned());
    }
    Ok(EventStream { base_root, events })
}

fn parse_node(reader: &mut Reader<'_>) -> Result<Node, String> {
    let kind = reader.u8()?;
    let hash = reader.hash()?;
    let prefix_length = reader.u8()? as usize;
    if prefix_length > 64 {
        return Err("Architecture F prefix exceeds 64 nibbles".to_owned());
    }
    let prefix = reader.take(prefix_length)?.to_vec();
    if prefix.iter().any(|nibble| *nibble > 0x0f) {
        return Err("Architecture F prefix contains a non-nibble".to_owned());
    }
    match kind {
        1 => {
            let key_length = reader.u16()? as usize;
            let value_length = reader.u32()? as usize;
            let key = reader.take(key_length)?.to_vec();
            let value = reader.take(value_length)?.to_vec();
            let key_path = path_nibbles(&key);
            if !key_path.ends_with(&prefix) {
                return Err(format!(
                    "Architecture F leaf prefix does not extend its key path: key={},path={},prefix={}",
                    hex(&key),
                    hex(&key_path),
                    hex(&prefix)
                ));
            }
            let actual = leaf_hash(&prefix, &value)?;
            if actual != hash {
                return Err("Architecture F leaf content hash mismatch".to_owned());
            }
            Ok(Node::Leaf {
                hash,
                prefix,
                key,
                value,
            })
        }
        2 => {
            let size = reader.u64()?;
            let bitmap = reader.u16()?;
            if bitmap.count_ones() < 2 || !(2..=MAX_SAFE_TRIE_SIZE).contains(&size) {
                return Err("Architecture F branch has an invalid shape".to_owned());
            }
            let mut children = [None; 16];
            for (index, child) in children.iter_mut().enumerate() {
                if bitmap & (1 << index) != 0 {
                    *child = Some(reader.hash()?);
                }
            }
            let merkle = branch_merkle(&children);
            if branch_hash(&prefix, &merkle) != hash {
                return Err("Architecture F branch content hash mismatch".to_owned());
            }
            Ok(Node::Branch {
                hash,
                prefix,
                children,
                size,
                merkle,
            })
        }
        _ => Err("Architecture F record kind is invalid".to_owned()),
    }
}

fn parse_input(input: &[u8]) -> Result<(Caps, Hash, Arena, Vec<Vec<Op>>), String> {
    if input.len() < INPUT_HEADER_BYTES || &input[..4] != INPUT_MAGIC {
        return Err("Architecture F input magic/header is invalid".to_owned());
    }
    let mut reader = Reader::new(input);
    reader.take(4)?;
    if reader.u16()? != ABI_VERSION || reader.u16()? != 0 {
        return Err("Architecture F ABI version/flags are invalid".to_owned());
    }
    let caps = Caps {
        records: reader.u32()? as usize,
        events: reader.u32()? as usize,
        ops: reader.u32()? as usize,
        input_bytes: reader.u32()? as usize,
        output_bytes: reader.u32()? as usize,
    };
    if caps.records > ABSOLUTE_MAX_RECORDS
        || caps.events > ABSOLUTE_MAX_EVENTS
        || caps.ops > ABSOLUTE_MAX_OPS
        || caps.input_bytes > ABSOLUTE_MAX_INPUT_BYTES
        || caps.output_bytes > ABSOLUTE_MAX_OUTPUT_BYTES
        || input.len() > caps.input_bytes
    {
        return Err("Architecture F caller cap exceeds the absolute envelope".to_owned());
    }
    let record_count = reader.u32()? as usize;
    let event_count = reader.u32()? as usize;
    let op_count = reader.u32()? as usize;
    if record_count > caps.records || event_count > caps.events || op_count > caps.ops {
        return Err("Architecture F input count exceeds caller cap".to_owned());
    }
    let base_root = reader.hash()?;
    let mut arena = Arena::new();
    for _ in 0..record_count {
        let node = parse_node(&mut reader)?;
        let before = arena.nodes.len();
        arena.append(node)?;
        if arena.nodes.len() == before {
            return Err("Architecture F raw proof contains a duplicate record".to_owned());
        }
    }
    arena.base_count = arena.nodes.len();
    arena.root = if base_root == EMPTY_ROOT {
        None
    } else {
        Some(arena.resolve(&base_root)?)
    };
    arena.assert_base_closure(base_root)?;
    let mut events = Vec::with_capacity(event_count);
    let mut parsed_ops = 0usize;
    for _ in 0..event_count {
        let event_ops = reader.u32()? as usize;
        parsed_ops = parsed_ops
            .checked_add(event_ops)
            .ok_or_else(|| "Architecture F op count overflow".to_owned())?;
        if parsed_ops > op_count {
            return Err("Architecture F event ops exceed declared count".to_owned());
        }
        let mut event = Vec::with_capacity(event_ops);
        for _ in 0..event_ops {
            let kind = reader.u8()?;
            let key_length = reader.u16()? as usize;
            let value_length = reader.u32()? as usize;
            let key = reader.take(key_length)?.to_vec();
            let value = reader.take(value_length)?.to_vec();
            match kind {
                1 => event.push(Op::Insert { key, value }),
                2 if value.is_empty() => event.push(Op::Delete { key }),
                _ => return Err("Architecture F op kind/value shape is invalid".to_owned()),
            }
        }
        events.push(event);
    }
    if parsed_ops != op_count || reader.remaining() != 0 {
        return Err("Architecture F declared counts/trailing bytes mismatch".to_owned());
    }
    Ok((caps, base_root, arena, events))
}

fn push_u16(output: &mut Vec<u8>, value: u16) {
    output.extend_from_slice(&value.to_le_bytes());
}

fn push_u32(output: &mut Vec<u8>, value: usize) -> Result<(), String> {
    let value =
        u32::try_from(value).map_err(|_| "Architecture F u32 output overflow".to_owned())?;
    output.extend_from_slice(&value.to_le_bytes());
    Ok(())
}

fn push_u64(output: &mut Vec<u8>, value: u64) {
    output.extend_from_slice(&value.to_le_bytes());
}

fn encode_node(output: &mut Vec<u8>, node: &Node) -> Result<(), String> {
    match node {
        Node::Leaf {
            hash,
            prefix,
            key,
            value,
        } => {
            output.push(1);
            output.extend_from_slice(hash);
            output.push(prefix.len() as u8);
            output.extend_from_slice(prefix);
            push_u16(
                output,
                u16::try_from(key.len()).map_err(|_| "Architecture F key too large".to_owned())?,
            );
            push_u32(output, value.len())?;
            output.extend_from_slice(key);
            output.extend_from_slice(value);
        }
        Node::Branch {
            hash,
            prefix,
            children,
            size,
            ..
        } => {
            output.push(2);
            output.extend_from_slice(hash);
            output.push(prefix.len() as u8);
            output.extend_from_slice(prefix);
            push_u64(output, *size);
            let bitmap = children
                .iter()
                .enumerate()
                .fold(0u16, |bitmap, (index, child)| {
                    bitmap | if child.is_some() { 1 << index } else { 0 }
                });
            push_u16(output, bitmap);
            for child in children.iter().flatten() {
                output.extend_from_slice(child);
            }
        }
    }
    Ok(())
}

fn run_engine(input: &[u8]) -> Result<Vec<u8>, String> {
    let (caps, base_root, mut arena, events) = parse_input(input)?;
    let mut event_roots = Vec::with_capacity(events.len());
    for event in &events {
        event_roots.push(arena.apply_event(event)?);
    }
    let candidate_root = arena
        .root
        .map(|id| arena.nodes[id].hash())
        .unwrap_or(EMPTY_ROOT);
    let dirty = arena.dirty_records();
    let mut delta = Vec::new();
    for node in &dirty {
        encode_node(&mut delta, node)?;
        if OUTPUT_HEADER_BYTES + event_roots.len() * 32 + delta.len() > caps.output_bytes {
            return Err("Architecture F output byte cap exceeded".to_owned());
        }
    }
    let roots_bytes: Vec<u8> = event_roots.iter().flatten().copied().collect();
    let mut aggregate_counts = [0u8; 12];
    aggregate_counts[..4].copy_from_slice(
        &u32::try_from(event_roots.len())
            .map_err(|_| "Architecture F event count output overflow".to_owned())?
            .to_le_bytes(),
    );
    aggregate_counts[4..8].copy_from_slice(
        &u32::try_from(dirty.len())
            .map_err(|_| "Architecture F dirty count output overflow".to_owned())?
            .to_le_bytes(),
    );
    aggregate_counts[8..].copy_from_slice(
        &u32::try_from(delta.len())
            .map_err(|_| "Architecture F delta length output overflow".to_owned())?
            .to_le_bytes(),
    );
    let delta_digest = digest(&[
        b"MIDGARD-MPF-ARCH-F-DELTA-V1",
        &aggregate_counts,
        &base_root,
        &candidate_root,
        &roots_bytes,
        &delta,
    ]);
    let mut output = Vec::with_capacity(OUTPUT_HEADER_BYTES + roots_bytes.len() + delta.len());
    output.extend_from_slice(OUTPUT_MAGIC);
    push_u16(&mut output, ABI_VERSION);
    push_u16(&mut output, 0);
    push_u32(&mut output, event_roots.len())?;
    push_u32(&mut output, dirty.len())?;
    push_u32(&mut output, OUTPUT_HEADER_BYTES + roots_bytes.len())?;
    push_u32(&mut output, delta.len())?;
    output.extend_from_slice(&base_root);
    output.extend_from_slice(&candidate_root);
    output.extend_from_slice(&delta_digest);
    output.extend_from_slice(&roots_bytes);
    output.extend_from_slice(&delta);
    if output.len() > caps.output_bytes {
        return Err("Architecture F output byte cap exceeded".to_owned());
    }
    Ok(output)
}

fn encode_root_stream(base_root: Hash, roots: &[Hash]) -> Result<Vec<u8>, String> {
    let candidate_root = roots.last().copied().unwrap_or(base_root);
    let roots_bytes: Vec<u8> = roots.iter().flatten().copied().collect();
    let root_digest = digest(&[
        b"MIDGARD-MPF-ARCH-G-ROOTS-V1",
        &base_root,
        &candidate_root,
        &roots_bytes,
    ]);
    let mut output = Vec::with_capacity(108 + roots_bytes.len());
    output.extend_from_slice(ROOT_STREAM_MAGIC);
    push_u16(&mut output, ABI_VERSION);
    push_u16(&mut output, 0);
    push_u32(&mut output, roots.len())?;
    output.extend_from_slice(&base_root);
    output.extend_from_slice(&candidate_root);
    output.extend_from_slice(&root_digest);
    output.extend_from_slice(&roots_bytes);
    Ok(output)
}

#[wasm_bindgen]
pub struct ArchitectureGSession {
    base: Arena,
    base_root: Hash,
    generations: HashMap<u32, Arena>,
    next_handle: u32,
}

#[wasm_bindgen]
impl ArchitectureGSession {
    #[wasm_bindgen(constructor)]
    pub fn new(base_input: &[u8]) -> Result<ArchitectureGSession, JsValue> {
        let (_, base_root, arena, events) =
            parse_input(base_input).map_err(|error| JsValue::from_str(&error))?;
        if !events.is_empty() {
            return Err(JsValue::from_str(
                "Architecture G setup input must not contain events",
            ));
        }
        Ok(Self {
            base: arena,
            base_root,
            generations: HashMap::new(),
            next_handle: 1,
        })
    }

    pub fn fork_generation(&mut self) -> Result<u32, JsValue> {
        if self.generations.len() >= 2 {
            return Err(JsValue::from_str(
                "Architecture G active generation cap exceeded",
            ));
        }
        let handle = self.next_handle;
        self.next_handle = self
            .next_handle
            .checked_add(1)
            .ok_or_else(|| JsValue::from_str("Architecture G handle space exhausted"))?;
        self.generations.insert(handle, self.base.clone());
        Ok(handle)
    }

    pub fn generation_root(&self, handle: u32) -> Result<Vec<u8>, JsValue> {
        let arena = self
            .generations
            .get(&handle)
            .ok_or_else(|| JsValue::from_str("Architecture G generation handle is stale"))?;
        Ok(arena
            .root
            .map(|id| arena.nodes[id].hash())
            .unwrap_or(EMPTY_ROOT)
            .to_vec())
    }

    pub fn apply_events_roots_only(
        &mut self,
        handle: u32,
        event_input: &[u8],
    ) -> Result<Vec<u8>, JsValue> {
        let stream = parse_event_stream(event_input).map_err(|error| JsValue::from_str(&error))?;
        let arena = self
            .generations
            .get_mut(&handle)
            .ok_or_else(|| JsValue::from_str("Architecture G generation handle is stale"))?;
        let current_root = arena
            .root
            .map(|id| arena.nodes[id].hash())
            .unwrap_or(EMPTY_ROOT);
        if stream.base_root != current_root {
            return Err(JsValue::from_str(
                "Architecture G event stream base root is stale",
            ));
        }
        let checkpoint_nodes = arena.nodes.len();
        let checkpoint_root = arena.root;
        let mut roots = Vec::with_capacity(stream.events.len());
        for event in &stream.events {
            match arena.apply_event(event) {
                Ok(root) => roots.push(root),
                Err(error) => {
                    arena.rollback(checkpoint_nodes, checkpoint_root);
                    return Err(JsValue::from_str(&error));
                }
            }
        }
        encode_root_stream(stream.base_root, &roots).map_err(|error| {
            arena.rollback(checkpoint_nodes, checkpoint_root);
            JsValue::from_str(&error)
        })
    }

    pub fn discard_generation(&mut self, handle: u32) -> Result<(), JsValue> {
        if self.generations.remove(&handle).is_none() {
            return Err(JsValue::from_str(
                "Architecture G generation handle is stale",
            ));
        }
        Ok(())
    }

    pub fn active_generations(&self) -> usize {
        self.generations.len()
    }

    pub fn base_root(&self) -> Vec<u8> {
        self.base_root.to_vec()
    }
}

#[wasm_bindgen]
pub fn run_architecture_f(input: &[u8]) -> Result<Vec<u8>, JsValue> {
    run_engine(input).map_err(|error| JsValue::from_str(&error))
}

fn hex(bytes: &[u8]) -> String {
    const DIGITS: &[u8; 16] = b"0123456789abcdef";
    let mut output = String::with_capacity(bytes.len() * 2);
    for byte in bytes {
        output.push(DIGITS[(byte >> 4) as usize] as char);
        output.push(DIGITS[(byte & 0x0f) as usize] as char);
    }
    output
}

#[cfg(test)]
mod tests {
    use super::*;

    fn empty_input(events: &[Vec<Op>]) -> Vec<u8> {
        let op_count = events.iter().map(Vec::len).sum::<usize>();
        let mut input = Vec::new();
        input.extend_from_slice(INPUT_MAGIC);
        push_u16(&mut input, ABI_VERSION);
        push_u16(&mut input, 0);
        push_u32(&mut input, 64).unwrap();
        push_u32(&mut input, 64).unwrap();
        push_u32(&mut input, 128).unwrap();
        push_u32(&mut input, 1 << 20).unwrap();
        push_u32(&mut input, 1 << 20).unwrap();
        push_u32(&mut input, 0).unwrap();
        push_u32(&mut input, events.len()).unwrap();
        push_u32(&mut input, op_count).unwrap();
        input.extend_from_slice(&EMPTY_ROOT);
        for event in events {
            push_u32(&mut input, event.len()).unwrap();
            for op in event {
                match op {
                    Op::Insert { key, value } => {
                        input.push(1);
                        push_u16(&mut input, key.len() as u16);
                        push_u32(&mut input, value.len()).unwrap();
                        input.extend_from_slice(key);
                        input.extend_from_slice(value);
                    }
                    Op::Delete { key } => {
                        input.push(2);
                        push_u16(&mut input, key.len() as u16);
                        push_u32(&mut input, 0).unwrap();
                        input.extend_from_slice(key);
                    }
                }
            }
        }
        input
    }

    fn assert_delta_records_authenticate(output: &[u8]) {
        let dirty_count = u32::from_le_bytes(output[12..16].try_into().unwrap()) as usize;
        let delta_offset = u32::from_le_bytes(output[16..20].try_into().unwrap()) as usize;
        let mut reader = Reader::new(&output[delta_offset..]);
        for _ in 0..dirty_count {
            parse_node(&mut reader).expect("emitted dirty record must re-authenticate");
        }
        assert_eq!(reader.remaining(), 0);
    }

    #[test]
    fn canonical_empty_root_is_stable() {
        assert_eq!(hex(&digest(&[&[]])), hex(&EMPTY_ROOT));
    }

    #[test]
    fn rejects_invalid_mutation_atomically() {
        let input = empty_input(&[vec![Op::Delete { key: vec![1; 32] }]]);
        assert!(run_engine(&input).unwrap_err().contains("key is absent"));
    }

    #[test]
    fn emits_mandatory_root_for_empty_event_and_reinsert() {
        let key = vec![0x52; 32];
        let input = empty_input(&[
            vec![Op::Insert {
                key: key.clone(),
                value: vec![1],
            }],
            vec![],
            vec![
                Op::Delete { key: key.clone() },
                Op::Insert {
                    key,
                    value: vec![2],
                },
            ],
        ]);
        let output = run_engine(&input).unwrap();
        assert_eq!(&output[..4], OUTPUT_MAGIC);
        assert_eq!(u32::from_le_bytes(output[8..12].try_into().unwrap()), 3);
        assert_eq!(&output[120..152], &output[152..184]);
        assert_ne!(&output[152..184], &output[184..216]);
    }

    #[test]
    fn emitted_closure_reauthenticates_after_repeated_sibling_updates() {
        let numbered_key = |index: u32| {
            let mut key = vec![0u8; 32];
            key[28..].copy_from_slice(&index.to_be_bytes());
            key
        };
        let mut events = vec![(0..64)
            .map(|index| Op::Insert {
                key: numbered_key(index),
                value: vec![index as u8; 16],
            })
            .collect::<Vec<_>>()];
        events.extend((0..32).map(|index| {
            vec![
                Op::Delete {
                    key: numbered_key(index),
                },
                Op::Insert {
                    key: numbered_key(1_000 + index),
                    value: vec![(index + 17) as u8; 16],
                },
            ]
        }));
        let output = run_engine(&empty_input(&events)).unwrap();
        assert_delta_records_authenticate(&output);
    }
}
