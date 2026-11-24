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
            if frame.payload.len() != 40 {
                return Err(
                    "Architecture G Hello payload must contain the pinned binary SHA-256 and the cache budget"
                        .to_owned(),
                );
            }
            let budget = u64::from_le_bytes(frame.payload[32..40].try_into().unwrap());
            owner
                .cache
                .set_budget(usize::try_from(budget).unwrap_or(usize::MAX));
            let mut payload = Vec::with_capacity(86);
            payload.extend_from_slice(&RPC_SCHEMA.to_le_bytes());
            payload.extend_from_slice(&frame.payload[..32]);
            payload.extend_from_slice(&budget.to_le_bytes());
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
        RpcKind::SetRoot => {
            let root = read_hash(&frame.payload, "root")?;
            owner.set_root(root)?;
            let mut payload = Vec::with_capacity(64);
            payload.extend_from_slice(&root);
            for value in [
                owner.cache.len() as u64,
                owner.cache.bytes() as u64,
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
        RpcKind::ProvideNodes => {
            if frame.payload.is_empty() || frame.payload.len() > RPC_MAX_CHUNK_BYTES {
                return Err("Architecture G ProvideNodes size is invalid".to_owned());
            }
            let nodes = parse_node_records(&frame.payload)?;
            let count = u32::try_from(nodes.len())
                .map_err(|_| "Architecture G ProvideNodes count overflow".to_owned())?;
            for node in nodes {
                owner.cache.insert(node);
            }
            send_rpc(
                writer,
                owner.epoch,
                frame.request_id,
                RpcKind::NodesProvided,
                count.to_le_bytes().to_vec(),
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
                match owner.apply(id, event_bytes)? {
                    ApplyOutcome::NeedNodes(hashes) => {
                        let mut payload = Vec::with_capacity(20 + hashes.len() * 32);
                        payload.extend_from_slice(&id);
                        payload.extend_from_slice(&(hashes.len() as u32).to_le_bytes());
                        for hash in hashes {
                            payload.extend_from_slice(&hash);
                        }
                        send_rpc(
                            writer,
                            owner.epoch,
                            frame.request_id,
                            RpcKind::NeedNodes,
                            payload,
                        )?;
                        return Ok(true);
                    }
                    ApplyOutcome::Applied {
                        candidate,
                        roots,
                        proof_duration_ns,
                        mutation_duration_ns,
                    } => (candidate, roots, proof_duration_ns, mutation_duration_ns),
                };
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
            let diagnostics = owner.diagnostics()?;
            let mut payload = Vec::with_capacity(88);
            payload.extend_from_slice(&diagnostics.marker);
            for value in [
                diagnostics.cached_nodes,
                diagnostics.cached_bytes,
                diagnostics.generations,
                diagnostics.generated_nodes,
                diagnostics.generated_bytes,
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
