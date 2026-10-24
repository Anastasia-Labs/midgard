use super::*;

pub(super) fn run_engine(input: &[u8]) -> Result<Vec<u8>, String> {
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

pub(super) fn encode_root_stream(base_root: Hash, roots: &[Hash]) -> Result<Vec<u8>, String> {
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
