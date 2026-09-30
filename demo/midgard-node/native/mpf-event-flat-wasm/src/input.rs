use super::*;

pub(super) struct EventStream {
    pub(super) base_root: Hash,
    pub(super) events: Vec<Vec<Op>>,
}

pub(super) fn parse_event_stream(input: &[u8]) -> Result<EventStream, String> {
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

pub(super) fn parse_node(reader: &mut Reader<'_>) -> Result<Node, String> {
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

pub(super) fn parse_input(input: &[u8]) -> Result<(Caps, Hash, Arena, Vec<Vec<Op>>), String> {
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
