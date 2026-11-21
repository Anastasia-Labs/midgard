use super::*;

fn leaf(hash: Hash, key_byte: u8, value_byte: u8) -> Node {
    Node::Leaf {
        hash,
        prefix: vec![key_byte & 0x0f],
        key: vec![key_byte; 32],
        value: vec![value_byte; 64],
    }
}

fn empty_index() -> FullIndex {
    FullIndex {
        nodes: Vec::new(),
        ids: HashMap::new(),
        prefixes: Vec::new(),
        children: Vec::new(),
        child_ids: Vec::new(),
        branch_merkle: Vec::new(),
        keys: Vec::new(),
        values: Vec::new(),
        root: None,
        branches: 0,
        leaves: 0,
        edges: 0,
    }
}

fn index_with_root(root: Node) -> FullIndex {
    let mut index = empty_index();
    index.append(root).unwrap();
    index.root = Some(0);
    index
}

fn key_with_first_nibble(target: u8) -> Vec<u8> {
    for candidate in 0u32.. {
        let mut key = vec![0u8; 32];
        key[28..].copy_from_slice(&candidate.to_be_bytes());
        if path_nibbles(&key)[0] == target {
            return key;
        }
    }
    unreachable!("every nibble has a preimage")
}

fn base_arena(keys: &[Vec<u8>]) -> (Arena, Hash) {
    let mut arena = Arena::new();
    for (index, key) in keys.iter().enumerate() {
        arena
            .apply_event(&[Op::Insert {
                key: key.clone(),
                value: vec![u8::try_from(index + 1).unwrap(); 64],
            }])
            .unwrap();
    }
    let root = arena.nodes[arena.root.unwrap()].hash();
    (arena, root)
}

fn full_index(arena: &Arena, marker: Hash) -> FullIndex {
    let mut index = empty_index();
    for node in arena.dirty_records() {
        index.append(node.clone()).unwrap();
    }
    index.root = Some(*index.ids.get(&marker).unwrap());
    index.resolve_child_ids_from(0).unwrap();
    index.authenticate_complete_closure().unwrap();
    index
}

#[test]
fn proof_union_skips_delete_siblings_when_two_children_must_survive() {
    let keys: Vec<Vec<u8>> = (0..4).map(key_with_first_nibble).collect();
    let (base, marker) = base_arena(&keys);
    let index = full_index(&base, marker);
    let stream = EventStream {
        base_root: marker,
        events: vec![vec![Op::Delete {
            key: keys[0].clone(),
        }]],
    };

    let mut proof = index.proof_arena(&stream).unwrap();
    assert_eq!(proof.base_count, 2, "root plus the deleted leaf only");
    let observed = proof.apply_event(&stream.events[0]).unwrap();
    let mut reference = base.clone();
    let expected = reference.apply_event(&stream.events[0]).unwrap();
    assert_eq!(observed, expected);
}

#[test]
fn proof_union_retains_sibling_when_delete_can_collapse_branch() {
    let keys: Vec<Vec<u8>> = (0..2).map(key_with_first_nibble).collect();
    let (base, marker) = base_arena(&keys);
    let index = full_index(&base, marker);
    let stream = EventStream {
        base_root: marker,
        events: vec![vec![Op::Delete {
            key: keys[0].clone(),
        }]],
    };

    let mut proof = index.proof_arena(&stream).unwrap();
    assert_eq!(proof.base_count, 3, "root plus both leaves");
    let observed = proof.apply_event(&stream.events[0]).unwrap();
    let mut reference = base.clone();
    let expected = reference.apply_event(&stream.events[0]).unwrap();
    assert_eq!(observed, expected);
}

#[test]
fn child_id_cache_resolves_children_added_after_parent_records() {
    let child_hash = [2u8; 32];
    let parent_hash = [3u8; 32];
    let mut children = [None; 16];
    children[0] = Some(child_hash);
    let parent = Node::Branch {
        hash: parent_hash,
        prefix: Vec::new(),
        children,
        size: 1,
        merkle: [[0u8; 32]; 15],
    };
    let child = leaf(child_hash, 1, 11);
    let mut index = empty_index();
    index.append(parent).unwrap();
    assert!(index.child_id(0, 0).is_err());
    assert_eq!(index.child_ids, vec![UNRESOLVED_CHILD_ID]);

    index.append(child).unwrap();
    index.resolve_child_ids_from(0).unwrap();
    assert_eq!(index.child_id(0, 0).unwrap(), Some(1));
    assert_eq!(index.child_ids, vec![1]);
    assert!(index.estimated_bytes() >= index.children.capacity() * size_of::<Hash>());
}

#[test]
fn promotion_cap_rejection_preserves_marker_and_generation_for_retry() {
    let base_hash = [1u8; 32];
    let candidate_hash = [2u8; 32];
    let base = leaf(base_hash, 1, 11);
    let candidate = leaf(candidate_hash, 2, 22);
    let mut arena = Arena::new();
    arena.append(base.clone()).unwrap();
    arena.base_count = arena.nodes.len();
    let candidate_id = arena.append(candidate).unwrap();
    arena.root = Some(candidate_id);
    let generation_id = [9u8; 16];
    let mut generations = HashMap::new();
    generations.insert(
        generation_id,
        RuntimeGeneration {
            base_root: base_hash,
            arena: Some(arena),
            candidate_root: Some(candidate_hash),
            prepared: false,
        },
    );
    let mut owner = RuntimeOwner {
        epoch: [7u8; 16],
        marker: Some(base_hash),
        index: Some(index_with_root(base)),
        load_payload: Vec::new(),
        generations,
        next_generation: 1,
    };
    let resident_bytes_before = owner.index.as_ref().unwrap().estimated_bytes();

    let error = owner
        .generated_records_with_caps(generation_id, 1, MAX_RESIDENT_BYTES)
        .unwrap_err();
    assert!(error.contains("projected_nodes=2"), "{error}");
    assert!(error.contains("projected_bytes="), "{error}");
    assert_eq!(owner.marker, Some(base_hash));
    assert_eq!(owner.index.as_ref().unwrap().nodes.len(), 1);
    assert_eq!(
        owner.index.as_ref().unwrap().estimated_bytes(),
        resident_bytes_before
    );
    assert!(!owner.generations[&generation_id].prepared);

    owner
        .generated_records_with_caps(generation_id, 2, MAX_RESIDENT_BYTES)
        .unwrap();
    assert!(owner.generations[&generation_id].prepared);
    assert_eq!(owner.commit(generation_id).unwrap(), candidate_hash);
    assert_eq!(owner.marker, Some(candidate_hash));
    assert_eq!(owner.index.as_ref().unwrap().nodes.len(), 2);
    assert!(owner.generations.is_empty());
}
