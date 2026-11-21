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

mod compact_index;
use compact_index::*;
mod runtime_owner;
use runtime_owner::*;
mod simulated_owner;
use simulated_owner::*;
mod sidecar;
use sidecar::*;
mod records;
use records::*;
mod rpc_handler;
use rpc_handler::*;

mod cli;
use cli::rss_kib;
pub use cli::run_owner_cli;

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
mod tests;
