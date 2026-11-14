use super::*;

pub(super) fn encode_node_record(node: &Node) -> Result<Vec<u8>, String> {
    let prefix = node.prefix();
    let prefix_length = u8::try_from(prefix.len())
        .map_err(|_| "Architecture G record prefix exceeds u8".to_owned())?;
    let mut output = Vec::new();
    match node {
        Node::Leaf {
            hash, key, value, ..
        } => {
            output.push(1);
            output.extend_from_slice(hash);
            output.push(prefix_length);
            output.extend_from_slice(prefix);
            output.extend_from_slice(
                &u16::try_from(key.len())
                    .map_err(|_| "Architecture G record key exceeds u16".to_owned())?
                    .to_le_bytes(),
            );
            output.extend_from_slice(
                &u32::try_from(value.len())
                    .map_err(|_| "Architecture G record value exceeds u32".to_owned())?
                    .to_le_bytes(),
            );
            output.extend_from_slice(key);
            output.extend_from_slice(value);
        }
        Node::Branch {
            hash,
            children,
            size,
            ..
        } => {
            output.push(2);
            output.extend_from_slice(hash);
            output.push(prefix_length);
            output.extend_from_slice(prefix);
            output.extend_from_slice(&size.to_le_bytes());
            let bitmap = children
                .iter()
                .enumerate()
                .fold(0u16, |bits, (index, child)| {
                    bits | if child.is_some() { 1 << index } else { 0 }
                });
            output.extend_from_slice(&bitmap.to_le_bytes());
            for child in children.iter().flatten() {
                output.extend_from_slice(child);
            }
        }
    }
    Ok(output)
}

pub(super) fn generated_reachable_nodes(
    arena: &Arena,
    candidate: Hash,
) -> Result<Vec<Node>, String> {
    if candidate == EMPTY_ROOT {
        return Ok(Vec::new());
    }
    let root = arena.resolve(&candidate)?;
    let mut pending = vec![root];
    let mut visited = HashSet::new();
    let mut generated = Vec::new();
    while let Some(id) = pending.pop() {
        if !visited.insert(id) {
            continue;
        }
        let node = &arena.nodes[id];
        if let Node::Branch { children, .. } = node {
            for hash in children.iter().flatten() {
                if let Some(child) = arena.ids.get(hash) {
                    pending.push(*child);
                }
            }
        }
        if id >= arena.base_count {
            generated.push(node.clone());
        }
    }
    generated.sort_unstable_by_key(Node::hash);
    Ok(generated)
}

pub(super) fn generated_reachable_records(
    arena: &Arena,
    candidate: Hash,
) -> Result<Vec<Vec<u8>>, String> {
    generated_reachable_nodes(arena, candidate)?
        .iter()
        .map(encode_node_record)
        .collect()
}

pub(super) fn read_hash(bytes: &[u8], field: &str) -> Result<Hash, String> {
    bytes
        .try_into()
        .map_err(|_| format!("Architecture G {field} must contain exactly 32 bytes"))
}

pub(super) fn read_id(bytes: &[u8], field: &str) -> Result<[u8; 16], String> {
    bytes
        .try_into()
        .map_err(|_| format!("Architecture G {field} must contain exactly 16 bytes"))
}
