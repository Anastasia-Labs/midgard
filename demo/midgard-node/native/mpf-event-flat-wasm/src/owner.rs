use std::{
    collections::{BTreeSet, HashMap, HashSet},
    fs::{self, File},
    io::{Read, Write},
    mem::size_of,
    time::Instant,
};

use super::*;
use crate::rpc::{read_frame, write_frame, RpcFrame, RpcKind, RPC_MAX_CHUNK_BYTES, RPC_SCHEMA};

mod node_cache;
use node_cache::*;
mod records;
use records::*;
mod rpc_handler;
use rpc_handler::*;
mod runtime_owner;
use runtime_owner::*;

pub(super) fn rss_kib(field: &str) -> u64 {
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
