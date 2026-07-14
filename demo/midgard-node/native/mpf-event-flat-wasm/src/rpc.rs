use std::io::{Read, Write};

use blake2::{
    digest::{Update, VariableOutput},
    Blake2bVar,
};

pub const RPC_MAGIC: [u8; 4] = *b"MGRP";
pub const RPC_SCHEMA: u16 = 1;
pub const RPC_DIGEST_DOMAIN: &[u8] = b"MIDGARD-MPF-OWNER-RPC-V1";
pub const RPC_HEADER_BYTES: usize = 36;
pub const RPC_DIGEST_BYTES: usize = 32;
pub const RPC_MAX_FRAME_BYTES: usize = 64 * 1024 * 1024;
pub const RPC_MAX_CHUNK_BYTES: usize = 16 * 1024 * 1024;

#[repr(u16)]
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum RpcKind {
    Hello = 1,
    HelloAck = 2,
    LoadBegin = 3,
    LoadChunk = 4,
    LoadEnd = 5,
    Ready = 6,
    Fork = 7,
    Forked = 8,
    ApplyEvents = 9,
    Applied = 10,
    Discard = 11,
    Discarded = 12,
    PreparePromotion = 13,
    PromotionChunk = 14,
    PromotionEnd = 15,
    PromotionCommitted = 16,
    Diagnostics = 17,
    DiagnosticsResult = 18,
    Ping = 19,
    Pong = 20,
    Shutdown = 21,
    ShutdownAck = 22,
    Error = 23,
}

impl TryFrom<u16> for RpcKind {
    type Error = String;

    fn try_from(value: u16) -> Result<Self, String> {
        match value {
            1 => Ok(Self::Hello),
            2 => Ok(Self::HelloAck),
            3 => Ok(Self::LoadBegin),
            4 => Ok(Self::LoadChunk),
            5 => Ok(Self::LoadEnd),
            6 => Ok(Self::Ready),
            7 => Ok(Self::Fork),
            8 => Ok(Self::Forked),
            9 => Ok(Self::ApplyEvents),
            10 => Ok(Self::Applied),
            11 => Ok(Self::Discard),
            12 => Ok(Self::Discarded),
            13 => Ok(Self::PreparePromotion),
            14 => Ok(Self::PromotionChunk),
            15 => Ok(Self::PromotionEnd),
            16 => Ok(Self::PromotionCommitted),
            17 => Ok(Self::Diagnostics),
            18 => Ok(Self::DiagnosticsResult),
            19 => Ok(Self::Ping),
            20 => Ok(Self::Pong),
            21 => Ok(Self::Shutdown),
            22 => Ok(Self::ShutdownAck),
            23 => Ok(Self::Error),
            _ => Err(format!("Unknown native MPF RPC message kind {value}")),
        }
    }
}

pub struct RpcFrame {
    pub kind: RpcKind,
    pub request_id: u64,
    pub owner_epoch: [u8; 16],
    pub payload: Vec<u8>,
}

fn digest(parts: &[&[u8]]) -> [u8; RPC_DIGEST_BYTES] {
    let mut hasher = Blake2bVar::new(RPC_DIGEST_BYTES).expect("valid BLAKE2b length");
    for part in parts {
        hasher.update(part);
    }
    let mut output = [0u8; RPC_DIGEST_BYTES];
    hasher
        .finalize_variable(&mut output)
        .expect("valid BLAKE2b output");
    output
}

pub fn read_frame(reader: &mut impl Read) -> Result<Option<RpcFrame>, String> {
    let mut length = [0u8; 4];
    let mut read = 0usize;
    while read < length.len() {
        match reader.read(&mut length[read..]) {
            Ok(0) if read == 0 => return Ok(None),
            Ok(0) => return Err("Native MPF RPC stream ended with a truncated length".to_owned()),
            Ok(count) => read += count,
            Err(error) => return Err(format!("Native MPF RPC read failed: {error}")),
        }
    }
    let frame_bytes = u32::from_le_bytes(length) as usize;
    let minimum = RPC_HEADER_BYTES + RPC_DIGEST_BYTES;
    if !(minimum..=RPC_MAX_FRAME_BYTES).contains(&frame_bytes) {
        return Err(format!("Invalid native MPF RPC frame length {frame_bytes}"));
    }
    let mut frame = vec![0u8; frame_bytes];
    reader
        .read_exact(&mut frame)
        .map_err(|error| format!("Native MPF RPC frame read failed: {error}"))?;
    if frame[..4] != RPC_MAGIC {
        return Err("Invalid native MPF RPC frame magic".to_owned());
    }
    if u16::from_le_bytes(frame[4..6].try_into().unwrap()) != RPC_SCHEMA {
        return Err("Unsupported native MPF RPC schema".to_owned());
    }
    let kind = RpcKind::try_from(u16::from_le_bytes(frame[6..8].try_into().unwrap()))?;
    let request_id = u64::from_le_bytes(frame[8..16].try_into().unwrap());
    let owner_epoch = frame[16..32].try_into().unwrap();
    let payload_bytes = u32::from_le_bytes(frame[32..36].try_into().unwrap()) as usize;
    if RPC_HEADER_BYTES + payload_bytes + RPC_DIGEST_BYTES != frame_bytes {
        return Err("Native MPF RPC payload length does not match frame".to_owned());
    }
    let digest_offset = RPC_HEADER_BYTES + payload_bytes;
    let expected = digest(&[RPC_DIGEST_DOMAIN, &frame[..digest_offset]]);
    if frame[digest_offset..] != expected {
        return Err("Native MPF RPC frame digest mismatch".to_owned());
    }
    Ok(Some(RpcFrame {
        kind,
        request_id,
        owner_epoch,
        payload: frame[RPC_HEADER_BYTES..digest_offset].to_vec(),
    }))
}

pub fn write_frame(writer: &mut impl Write, frame: &RpcFrame) -> Result<(), String> {
    let frame_bytes = RPC_HEADER_BYTES
        .checked_add(frame.payload.len())
        .and_then(|length| length.checked_add(RPC_DIGEST_BYTES))
        .ok_or_else(|| "Native MPF RPC frame length overflow".to_owned())?;
    if frame_bytes > RPC_MAX_FRAME_BYTES {
        return Err(format!(
            "Native MPF RPC frame exceeds cap: bytes={frame_bytes},cap={RPC_MAX_FRAME_BYTES}"
        ));
    }
    let mut bytes = Vec::with_capacity(4 + frame_bytes);
    bytes.extend_from_slice(&(frame_bytes as u32).to_le_bytes());
    bytes.extend_from_slice(&RPC_MAGIC);
    bytes.extend_from_slice(&RPC_SCHEMA.to_le_bytes());
    bytes.extend_from_slice(&(frame.kind as u16).to_le_bytes());
    bytes.extend_from_slice(&frame.request_id.to_le_bytes());
    bytes.extend_from_slice(&frame.owner_epoch);
    bytes.extend_from_slice(&(frame.payload.len() as u32).to_le_bytes());
    bytes.extend_from_slice(&frame.payload);
    let checksum = digest(&[RPC_DIGEST_DOMAIN, &bytes[4..]]);
    bytes.extend_from_slice(&checksum);
    writer
        .write_all(&bytes)
        .and_then(|_| writer.flush())
        .map_err(|error| format!("Native MPF RPC write failed: {error}"))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn frame_round_trip_and_tamper_rejection() {
        let frame = RpcFrame {
            kind: RpcKind::ApplyEvents,
            request_id: 42,
            owner_epoch: [7; 16],
            payload: vec![1, 2, 3, 4],
        };
        let mut encoded = Vec::new();
        write_frame(&mut encoded, &frame).unwrap();
        let decoded = read_frame(&mut encoded.as_slice()).unwrap().unwrap();
        assert_eq!(decoded.kind, frame.kind);
        assert_eq!(decoded.request_id, frame.request_id);
        assert_eq!(decoded.owner_epoch, frame.owner_epoch);
        assert_eq!(decoded.payload, frame.payload);

        let index = encoded.len() - 33;
        encoded[index] ^= 1;
        assert!(read_frame(&mut encoded.as_slice()).is_err());
    }
}
