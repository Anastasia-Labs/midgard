use super::*;

pub(super) fn digest(parts: &[&[u8]]) -> Hash {
    let mut hasher = Blake2bVar::new(32).expect("BLAKE2b-256 output size is valid");
    for part in parts {
        hasher.update(part);
    }
    let mut output = [0u8; 32];
    hasher
        .finalize_variable(&mut output)
        .expect("BLAKE2b-256 output buffer is valid");
    output
}

pub(super) fn path_nibbles(key: &[u8]) -> Vec<u8> {
    digest(&[key])
        .into_iter()
        .flat_map(|byte| [byte >> 4, byte & 0x0f])
        .collect()
}

pub(super) fn packed_nibbles(nibbles: &[u8]) -> Result<Vec<u8>, String> {
    if nibbles.iter().any(|nibble| *nibble > 0x0f) || nibbles.len() % 2 != 0 {
        return Err("Architecture F leaf tail is not canonical nibbles".to_owned());
    }
    Ok(nibbles
        .chunks_exact(2)
        .map(|pair| (pair[0] << 4) | pair[1])
        .collect())
}

pub(super) fn leaf_hash(prefix: &[u8], value: &[u8]) -> Result<Hash, String> {
    let (head, tail) = if prefix.len() % 2 == 1 {
        (vec![0, prefix[0]], packed_nibbles(&prefix[1..])?)
    } else {
        (vec![0xff], packed_nibbles(prefix)?)
    };
    let value_hash = digest(&[value]);
    Ok(digest(&[&head, &tail, &value_hash]))
}

pub(super) fn branch_merkle(children: &[Option<Hash>; 16]) -> [Hash; 15] {
    let mut nodes = [[0u8; 32]; 31];
    for (index, child) in children.iter().enumerate() {
        nodes[15 + index] = child.unwrap_or(ZERO_HASH);
    }
    for index in (0..15).rev() {
        nodes[index] = digest(&[&nodes[index * 2 + 1], &nodes[index * 2 + 2]]);
    }
    nodes[..15].try_into().unwrap()
}

pub(super) fn branch_hash(prefix: &[u8], merkle: &[Hash; 15]) -> Hash {
    digest(&[prefix, &merkle[0]])
}

pub(super) fn common_prefix_length(left: &[u8], right: &[u8]) -> usize {
    left.iter()
        .zip(right)
        .take_while(|(left, right)| left == right)
        .count()
}
