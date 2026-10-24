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

mod hashing;
use hashing::*;
mod arena;
use arena::*;
mod input;
use input::*;
mod output;
use output::*;
mod engine;
use engine::*;

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
mod tests;
