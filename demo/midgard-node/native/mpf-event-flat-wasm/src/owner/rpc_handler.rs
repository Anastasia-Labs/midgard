use super::*;

pub(super) fn send_rpc(
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

pub(super) fn handle_rpc_frame(
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
