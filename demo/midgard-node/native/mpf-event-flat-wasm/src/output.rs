use super::*;

pub(super) fn push_u16(output: &mut Vec<u8>, value: u16) {
    output.extend_from_slice(&value.to_le_bytes());
}

pub(super) fn push_u32(output: &mut Vec<u8>, value: usize) -> Result<(), String> {
    let value =
        u32::try_from(value).map_err(|_| "Architecture F u32 output overflow".to_owned())?;
    output.extend_from_slice(&value.to_le_bytes());
    Ok(())
}

pub(super) fn push_u64(output: &mut Vec<u8>, value: u64) {
    output.extend_from_slice(&value.to_le_bytes());
}

pub(super) fn encode_node(output: &mut Vec<u8>, node: &Node) -> Result<(), String> {
    match node {
        Node::Leaf {
            hash,
            prefix,
            key,
            value,
        } => {
            output.push(1);
            output.extend_from_slice(hash);
            output.push(prefix.len() as u8);
            output.extend_from_slice(prefix);
            push_u16(
                output,
                u16::try_from(key.len()).map_err(|_| "Architecture F key too large".to_owned())?,
            );
            push_u32(output, value.len())?;
            output.extend_from_slice(key);
            output.extend_from_slice(value);
        }
        Node::Branch {
            hash,
            prefix,
            children,
            size,
            ..
        } => {
            output.push(2);
            output.extend_from_slice(hash);
            output.push(prefix.len() as u8);
            output.extend_from_slice(prefix);
            push_u64(output, *size);
            let bitmap = children
                .iter()
                .enumerate()
                .fold(0u16, |bitmap, (index, child)| {
                    bitmap | if child.is_some() { 1 << index } else { 0 }
                });
            push_u16(output, bitmap);
            for child in children.iter().flatten() {
                output.extend_from_slice(child);
            }
        }
    }
    Ok(())
}
