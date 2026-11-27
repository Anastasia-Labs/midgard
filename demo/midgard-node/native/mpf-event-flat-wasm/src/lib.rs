use std::collections::{HashMap, HashSet};

use blake2::{
    digest::{Update, VariableOutput},
    Blake2bVar,
};

mod owner;
mod rpc;

pub use owner::run_owner_rpc;

type Hash = [u8; 32];

const EVENT_STREAM_MAGIC: &[u8; 4] = b"MEGO";
const ABI_VERSION: u16 = 1;
const EMPTY_ROOT: Hash = [
    0x0e, 0x57, 0x51, 0xc0, 0x26, 0xe5, 0x43, 0xb2, 0xe8, 0xab, 0x2e, 0xb0, 0x60, 0x99, 0xda, 0xa1,
    0xd1, 0xe5, 0xdf, 0x47, 0x77, 0x8f, 0x77, 0x87, 0xfa, 0xab, 0x45, 0xcd, 0xf1, 0x2f, 0xe3, 0xa8,
];
const ZERO_HASH: Hash = [0; 32];
const ABSOLUTE_MAX_EVENTS: usize = 100_000;
const ABSOLUTE_MAX_OPS: usize = 400_000;
const ABSOLUTE_MAX_INPUT_BYTES: usize = 536_870_912;
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
