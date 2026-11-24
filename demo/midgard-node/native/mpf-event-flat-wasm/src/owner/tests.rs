use super::*;

// splitmix64: deterministic and dependency-free.
#[derive(Clone)]
struct Rng(u64);

impl Rng {
    fn next(&mut self) -> u64 {
        self.0 = self.0.wrapping_add(0x9e37_79b9_7f4a_7c15);
        let mut value = self.0;
        value = (value ^ (value >> 30)).wrapping_mul(0xbf58_476d_1ce4_e5b9);
        value = (value ^ (value >> 27)).wrapping_mul(0x94d0_49bb_1331_11eb);
        value ^ (value >> 31)
    }

    fn below(&mut self, bound: usize) -> usize {
        (self.next() % bound as u64) as usize
    }
}

fn numbered_key(index: u64) -> Vec<u8> {
    let mut key = vec![0u8; 32];
    key[24..].copy_from_slice(&index.to_be_bytes());
    key
}

#[derive(Clone)]
struct Ledger {
    live: Vec<Vec<u8>>,
    next_key: u64,
    rng: Rng,
}

impl Ledger {
    fn new(seed: u64) -> Self {
        Self {
            live: Vec::new(),
            next_key: seed << 32,
            rng: Rng(seed),
        }
    }

    fn insert(&mut self) -> Op {
        let key = numbered_key(self.next_key);
        self.next_key += 1;
        self.live.push(key.clone());
        let length = 1 + self.rng.below(40);
        Op::Insert {
            key,
            value: vec![self.rng.next() as u8; length],
        }
    }

    fn block(&mut self, events: usize) -> Vec<Vec<Op>> {
        (0..events)
            .map(|_| {
                let ops = 1 + self.rng.below(3);
                (0..ops)
                    .map(|_| {
                        if !self.live.is_empty() && self.rng.below(2) == 0 {
                            let index = self.rng.below(self.live.len());
                            Op::Delete {
                                key: self.live.swap_remove(index),
                            }
                        } else {
                            self.insert()
                        }
                    })
                    .collect()
            })
            .collect()
    }
}

fn seeded(keys: usize, seed: u64) -> (Ledger, Arena, Hash) {
    let mut ledger = Ledger::new(seed);
    let inserts: Vec<Op> = (0..keys).map(|_| ledger.insert()).collect();
    let mut arena = Arena::new();
    let root = arena.apply_event(&inserts).unwrap();
    (ledger, arena, root)
}

fn reference_roots(reference: &mut Arena, events: &[Vec<Op>]) -> Vec<Hash> {
    events
        .iter()
        .map(|event| reference.apply_event(event).unwrap())
        .collect()
}

fn encode_events(base_root: Hash, events: &[Vec<Op>]) -> Vec<u8> {
    let mut body = Vec::new();
    for event in events {
        body.extend_from_slice(&(event.len() as u32).to_le_bytes());
        for op in event {
            let (kind, key, value): (u8, &[u8], &[u8]) = match op {
                Op::Insert { key, value } => (1, key, value),
                Op::Delete { key } => (2, key, &[]),
            };
            body.push(kind);
            body.extend_from_slice(&(key.len() as u16).to_le_bytes());
            body.extend_from_slice(&(value.len() as u32).to_le_bytes());
            body.extend_from_slice(key);
            body.extend_from_slice(value);
        }
    }
    let op_count: usize = events.iter().map(Vec::len).sum();
    let mut input = Vec::new();
    input.extend_from_slice(EVENT_STREAM_MAGIC);
    input.extend_from_slice(&ABI_VERSION.to_le_bytes());
    input.extend_from_slice(&0u16.to_le_bytes());
    for value in [
        events.len(),
        op_count,
        ABSOLUTE_MAX_EVENTS,
        ABSOLUTE_MAX_OPS,
        ABSOLUTE_MAX_INPUT_BYTES,
    ] {
        input.extend_from_slice(&(value as u32).to_le_bytes());
    }
    input.extend_from_slice(&base_root);
    let stream_digest = digest(&[
        b"MIDGARD-MPF-ARCH-G-EVENTS-V1",
        &input[8..28],
        &base_root,
        &body,
    ]);
    input.extend_from_slice(&stream_digest);
    input.extend_from_slice(&body);
    input
}

fn read_hashes(bytes: &[u8]) -> Vec<Hash> {
    bytes
        .chunks_exact(32)
        .map(|chunk| chunk.try_into().unwrap())
        .collect()
}

// Drives the owner through its request handler. `store` stands in for the
// parent's LevelDB: every node of the seeded trie plus every promoted node.
struct Harness {
    owner: RuntimeOwner,
    request_id: u64,
    store: HashMap<Hash, Vec<u8>>,
    rounds: Vec<Vec<Hash>>,
}

impl Harness {
    fn new(budget: u64, seed: &Arena) -> Self {
        let mut harness = Self {
            owner: RuntimeOwner::new([7; 16]),
            request_id: 0,
            store: HashMap::new(),
            rounds: Vec::new(),
        };
        for node in seed.dirty_records() {
            harness
                .store
                .insert(node.hash(), encode_node_record(node).unwrap());
        }
        let mut hello = vec![0x11; 32];
        hello.extend_from_slice(&budget.to_le_bytes());
        let ack = harness
            .request(RpcKind::Hello, hello, RpcKind::HelloAck)
            .unwrap();
        assert_eq!(ack.len(), 86);
        assert_eq!(ack[..2], RPC_SCHEMA.to_le_bytes());
        assert_eq!(ack[34..42], budget.to_le_bytes());
        harness
    }

    fn frames(&mut self, kind: RpcKind, payload: Vec<u8>) -> Result<Vec<RpcFrame>, String> {
        self.request_id += 1;
        let mut output = Vec::new();
        let frame = RpcFrame {
            kind,
            request_id: self.request_id,
            owner_epoch: self.owner.epoch,
            payload,
        };
        handle_rpc_frame(&mut self.owner, &mut output, frame)?;
        let mut reader = output.as_slice();
        let mut frames = Vec::new();
        while let Some(frame) = read_frame(&mut reader).unwrap() {
            frames.push(frame);
        }
        Ok(frames)
    }

    fn request(
        &mut self,
        kind: RpcKind,
        payload: Vec<u8>,
        reply: RpcKind,
    ) -> Result<Vec<u8>, String> {
        let mut frames = self.frames(kind, payload)?;
        assert_eq!(frames.len(), 1);
        let frame = frames.pop().unwrap();
        assert_eq!(frame.kind, reply);
        Ok(frame.payload)
    }

    fn set_root(&mut self, root: Hash) -> Result<(), String> {
        let ready = self.request(RpcKind::SetRoot, root.to_vec(), RpcKind::Ready)?;
        assert_eq!(ready, root);
        Ok(())
    }

    fn diagnostics(&mut self) -> Vec<u8> {
        let payload = self
            .request(RpcKind::Diagnostics, Vec::new(), RpcKind::DiagnosticsResult)
            .unwrap();
        assert_eq!(payload.len(), 88);
        payload
    }

    fn cached_bytes(&mut self) -> u64 {
        u64::from_le_bytes(self.diagnostics()[40..48].try_into().unwrap())
    }

    fn fork(&mut self, base_root: Hash) -> Result<[u8; 16], String> {
        let forked = self.request(RpcKind::Fork, base_root.to_vec(), RpcKind::Forked)?;
        Ok(forked[..16].try_into().unwrap())
    }

    fn apply_once(&mut self, id: [u8; 16], events: &[u8]) -> Result<RpcFrame, String> {
        let mut payload = id.to_vec();
        payload.extend_from_slice(events);
        let mut frames = self.frames(RpcKind::ApplyEvents, payload)?;
        assert_eq!(frames.len(), 1);
        Ok(frames.pop().unwrap())
    }

    fn need_nodes(&self, id: [u8; 16], frame: &RpcFrame) -> Vec<Hash> {
        assert_eq!(frame.kind, RpcKind::NeedNodes);
        assert_eq!(frame.payload[..16], id);
        let count = u32::from_le_bytes(frame.payload[16..20].try_into().unwrap()) as usize;
        assert_eq!(frame.payload.len(), 20 + count * 32);
        read_hashes(&frame.payload[20..])
    }

    // Serves NeedNodes rounds until the owner answers Applied.
    fn apply(&mut self, id: [u8; 16], events: &[u8]) -> Result<Vec<Hash>, String> {
        self.rounds.clear();
        loop {
            let frame = self.apply_once(id, events)?;
            if frame.kind == RpcKind::Applied {
                let count = u32::from_le_bytes(frame.payload[80..84].try_into().unwrap()) as usize;
                assert_eq!(frame.payload.len(), 84 + count * 32 + 16);
                return Ok(read_hashes(&frame.payload[84..84 + count * 32]));
            }
            let hashes = self.need_nodes(id, &frame);
            assert!(!hashes.is_empty());
            assert!(hashes.windows(2).all(|pair| pair[0] < pair[1]));
            assert!(hashes.iter().all(|hash| !self.owner.cache.contains(hash)));
            assert!(self.rounds.len() < 66, "apply did not terminate");
            self.rounds.push(hashes.clone());
            self.provide(&hashes)?;
        }
    }

    fn provide(&mut self, hashes: &[Hash]) -> Result<(), String> {
        let mut chunk = Vec::new();
        let mut count = 0u32;
        for hash in hashes {
            let record = self.store[hash].clone();
            if chunk.len() + record.len() > RPC_MAX_CHUNK_BYTES {
                self.send_nodes(std::mem::take(&mut chunk), count)?;
                count = 0;
            }
            chunk.extend_from_slice(&record);
            count += 1;
        }
        self.send_nodes(chunk, count)
    }

    fn send_nodes(&mut self, chunk: Vec<u8>, count: u32) -> Result<(), String> {
        let reply = self.request(RpcKind::ProvideNodes, chunk, RpcKind::NodesProvided)?;
        assert_eq!(reply, count.to_le_bytes());
        Ok(())
    }

    fn promote(&mut self, id: [u8; 16]) -> Result<Hash, String> {
        let mut frames = self.frames(RpcKind::PreparePromotion, id.to_vec())?;
        let end = frames.pop().unwrap();
        assert_eq!(end.kind, RpcKind::PromotionEnd);
        for frame in frames {
            assert_eq!(frame.kind, RpcKind::PromotionChunk);
            for node in parse_node_records(&frame.payload)? {
                self.store
                    .insert(node.hash(), encode_node_record(&node).unwrap());
            }
        }
        let committed = self.request(
            RpcKind::PromotionCommitted,
            id.to_vec(),
            RpcKind::PromotionCommitted,
        )?;
        Ok(committed.try_into().unwrap())
    }

    fn run_block(&mut self, base_root: Hash, events: &[Vec<Op>]) -> Result<Vec<Hash>, String> {
        let id = self.fork(base_root)?;
        let roots = self.apply(id, &encode_events(base_root, events))?;
        let candidate = self.promote(id)?;
        assert_eq!(candidate, roots.last().copied().unwrap_or(base_root));
        Ok(roots)
    }
}

fn key_where(accept: impl Fn(&[u8]) -> bool) -> Vec<u8> {
    (0u64..)
        .map(numbered_key)
        .find(|key| accept(&path_nibbles(key)))
        .unwrap()
}

fn full_cache(arena: &Arena) -> NodeCache {
    let mut cache = NodeCache::new(usize::MAX);
    for node in arena.dirty_records() {
        cache.insert(node.clone());
    }
    cache
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

fn ready_proof(cache: &mut NodeCache, stream: &EventStream) -> Arena {
    match cache.proof_arena(stream).unwrap() {
        Proof::Ready(arena) => arena,
        Proof::Missing(_) => panic!("every node is cached"),
    }
}

#[test]
fn proof_union_skips_delete_siblings_when_two_children_must_survive() {
    let keys: Vec<Vec<u8>> = (0..4)
        .map(|nibble| key_where(|path| path[0] == nibble))
        .collect();
    let (base, marker) = base_arena(&keys);
    let mut cache = full_cache(&base);
    let stream = EventStream {
        base_root: marker,
        events: vec![vec![Op::Delete {
            key: keys[0].clone(),
        }]],
    };

    let mut proof = ready_proof(&mut cache, &stream);
    assert_eq!(proof.base_count, 2, "root plus the deleted leaf only");
    let observed = proof.apply_event(&stream.events[0]).unwrap();
    let mut reference = base.clone();
    let expected = reference.apply_event(&stream.events[0]).unwrap();
    assert_eq!(observed, expected);
}

#[test]
fn proof_union_retains_sibling_when_delete_can_collapse_branch() {
    let keys: Vec<Vec<u8>> = (0..2)
        .map(|nibble| key_where(|path| path[0] == nibble))
        .collect();
    let (base, marker) = base_arena(&keys);
    let mut cache = full_cache(&base);
    let stream = EventStream {
        base_root: marker,
        events: vec![vec![Op::Delete {
            key: keys[0].clone(),
        }]],
    };

    let mut proof = ready_proof(&mut cache, &stream);
    assert_eq!(proof.base_count, 3, "root plus both leaves");
    let observed = proof.apply_event(&stream.events[0]).unwrap();
    let mut reference = base.clone();
    let expected = reference.apply_event(&stream.events[0]).unwrap();
    assert_eq!(observed, expected);
}

#[test]
fn commit_moves_the_marker_caches_new_nodes_and_drops_generations() {
    let leaf = |hash: Hash, key_byte: u8, value_byte: u8| Node::Leaf {
        hash,
        prefix: vec![key_byte & 0x0f],
        key: vec![key_byte; 32],
        value: vec![value_byte; 64],
    };
    let base_hash = [1u8; 32];
    let candidate_hash = [2u8; 32];
    let mut arena = Arena::new();
    arena.append(leaf(base_hash, 1, 11)).unwrap();
    arena.base_count = arena.nodes.len();
    let candidate_id = arena.append(leaf(candidate_hash, 2, 22)).unwrap();
    arena.root = Some(candidate_id);
    let generation_id = [9u8; 16];
    let mut owner = RuntimeOwner::new([7u8; 16]);
    owner.cache.set_budget(1 << 20);
    owner.set_root(base_hash).unwrap();
    owner.generations.insert(
        generation_id,
        RuntimeGeneration {
            base_root: base_hash,
            arena: Some(arena),
            candidate_root: Some(candidate_hash),
            prepared: false,
        },
    );
    owner.generations.insert(
        [8u8; 16],
        RuntimeGeneration {
            base_root: base_hash,
            arena: None,
            candidate_root: None,
            prepared: false,
        },
    );

    let error = owner.commit(generation_id).unwrap_err();
    assert!(error.contains("not prepared"), "{error}");
    assert_eq!(owner.marker, Some(base_hash));

    let (base, candidate, records) = owner.generated_records(generation_id).unwrap();
    assert_eq!(
        (base, candidate, records.len()),
        (base_hash, candidate_hash, 1)
    );
    assert_eq!(owner.commit(generation_id).unwrap(), candidate_hash);
    assert_eq!(owner.marker, Some(candidate_hash));
    assert!(owner.cache.contains(&candidate_hash));
    assert!(
        owner.generations.is_empty(),
        "the sibling generation is stale"
    );
}

// Catches: a wrong root when the owner starts with nothing cached and has to
// fetch every node it needs from the parent, over inserts and deletes.
#[test]
fn empty_cache_blocks_match_reference_roots() {
    let (mut ledger, mut reference, mut root) = seeded(3_000, 1);
    let mut harness = Harness::new(1 << 30, &reference);
    harness.set_root(root).unwrap();
    for block in 0..6 {
        let events = ledger.block(40);
        let expected = reference_roots(&mut reference, &events);
        assert_eq!(harness.run_block(root, &events).unwrap(), expected);
        if block == 0 {
            assert!(
                !harness.rounds.is_empty(),
                "the first block reads the store"
            );
        }
        root = *expected.last().unwrap();
    }
    assert_eq!(harness.diagnostics()[..32], root);
}

// Catches: the cache growing without bound (the resident index grew until
// promotions were refused), and wrong roots when nodes are evicted between
// blocks.
#[test]
fn small_budget_evicts_every_block_and_keeps_reference_roots() {
    const BUDGET: u64 = 48 * 1024;
    let (mut ledger, mut reference, mut root) = seeded(3_000, 2);
    let mut harness = Harness::new(BUDGET, &reference);
    harness.set_root(root).unwrap();
    for _ in 0..25 {
        let events = ledger.block(40);
        let expected = reference_roots(&mut reference, &events);
        assert_eq!(harness.run_block(root, &events).unwrap(), expected);
        assert!(!harness.rounds.is_empty(), "every block misses the cache");
        let cached_bytes = harness.cached_bytes();
        assert!(
            cached_bytes <= BUDGET,
            "cached_bytes={cached_bytes} exceeds budget={BUDGET}"
        );
        root = *expected.last().unwrap();
    }
}

// Catches: a delete that collapses a branch onto a child the cache does not
// hold, without the walk asking for that child.
#[test]
fn collapse_onto_uncached_sibling_requests_it_and_matches_reference() {
    let lone = key_where(|path| path[0] == 0);
    let left = key_where(|path| path[0] == 1 && path[1] == 0);
    let right = key_where(|path| path[0] == 1 && path[1] == 1);
    let (mut reference, root) = base_arena(&[lone.clone(), left, right]);
    let Node::Branch { children, .. } = &reference.nodes[reference.root.unwrap()] else {
        panic!("root is a branch");
    };
    let sibling = children[1].unwrap();
    let mut harness = Harness::new(1 << 30, &reference);
    harness.set_root(root).unwrap();
    let events = vec![vec![Op::Delete { key: lone }]];
    let expected = reference_roots(&mut reference, &events);

    let id = harness.fork(root).unwrap();
    let result = harness.apply(id, &encode_events(root, &events));
    assert!(
        harness.rounds.iter().flatten().any(|hash| *hash == sibling),
        "the sibling was never requested: rounds={:?}, result={result:?}",
        harness.rounds.len()
    );
    assert_eq!(result.unwrap(), expected);
}

// Catches: SetRoot not moving the walk's start, and SetRoot replacing the
// root under an open generation.
#[test]
fn set_root_moves_back_to_an_earlier_root_and_refuses_while_a_generation_is_open() {
    let (mut ledger, mut reference, mut root) = seeded(500, 4);
    let mut harness = Harness::new(1 << 30, &reference);
    let error = harness.fork(root).unwrap_err();
    assert!(error.contains("owner is not ready"), "{error}");
    harness.set_root(root).unwrap();
    let mut snapshots = vec![(root, reference.clone(), ledger.clone())];
    for _ in 0..3 {
        let events = ledger.block(30);
        let expected = reference_roots(&mut reference, &events);
        assert_eq!(harness.run_block(root, &events).unwrap(), expected);
        root = *expected.last().unwrap();
        snapshots.push((root, reference.clone(), ledger.clone()));
    }

    let id = harness.fork(root).unwrap();
    let error = harness.set_root(snapshots[1].0).unwrap_err();
    assert!(error.contains("generation is active"), "{error}");
    harness
        .request(RpcKind::Discard, id.to_vec(), RpcKind::Discarded)
        .unwrap();

    let (earlier, mut earlier_reference, mut earlier_ledger) = snapshots[1].clone();
    harness.set_root(earlier).unwrap();
    assert!(harness.fork(root).unwrap_err().contains("stale"));
    let events = earlier_ledger.block(30);
    let expected = reference_roots(&mut earlier_reference, &events);
    assert_eq!(harness.run_block(earlier, &events).unwrap(), expected);

    harness.set_root(EMPTY_ROOT).unwrap();
    let mut fresh = Ledger::new(40);
    let events = fresh.block(20);
    let expected = reference_roots(&mut Arena::new(), &events);
    assert_eq!(harness.run_block(EMPTY_ROOT, &events).unwrap(), expected);
}

// Catches: caching a node whose record does not hash to its claimed hash, or
// caching the valid part of a refused frame.
#[test]
fn provide_nodes_refuses_a_forged_record_and_caches_nothing_from_its_frame() {
    let (ledger, mut reference, root) = seeded(200, 5);
    let mut harness = Harness::new(1 << 30, &reference);
    harness.set_root(root).unwrap();
    let events = vec![vec![Op::Delete {
        key: ledger.live[0].clone(),
    }]];
    let bytes = encode_events(root, &events);
    let id = harness.fork(root).unwrap();
    let frame = harness.apply_once(id, &bytes).unwrap();
    assert_eq!(harness.need_nodes(id, &frame), vec![root]);

    let mut forged = harness
        .store
        .values()
        .find(|record| record[0] == 1)
        .unwrap()
        .clone();
    *forged.last_mut().unwrap() ^= 1;
    let mut payload = harness.store[&root].clone();
    payload.extend_from_slice(&forged);
    let error = harness
        .frames(RpcKind::ProvideNodes, payload)
        .err()
        .unwrap();
    assert!(error.contains("content hash mismatch"), "{error}");
    assert_eq!(harness.owner.cache.len(), 0);

    let frame = harness.apply_once(id, &bytes).unwrap();
    assert_eq!(harness.need_nodes(id, &frame), vec![root]);
    let expected = reference_roots(&mut reference, &events);
    assert_eq!(harness.apply(id, &bytes).unwrap(), expected);
}

// Catches: an apply whose NeedNodes rounds do not converge. Each round must
// reach at least one level deeper, so a path of n nodes takes at most n
// rounds plus one for the siblings a delete at the bottom collapses onto.
#[test]
fn apply_on_a_deep_path_terminates_within_its_depth() {
    const SHARED: usize = 5;
    let target = numbered_key(0);
    let target_path = path_nibbles(&target);
    let mut levels: Vec<Option<Vec<u8>>> = vec![None; SHARED + 1];
    let mut candidate = 1u64;
    while levels.iter().any(Option::is_none) {
        let key = numbered_key(candidate);
        candidate += 1;
        let shared = common_prefix_length(&path_nibbles(&key), &target_path);
        if shared <= SHARED && levels[shared].is_none() {
            levels[shared] = Some(key);
        }
    }
    let mut keys = vec![target.clone()];
    keys.extend(levels.into_iter().flatten());
    let (mut reference, root) = base_arena(&keys);
    let mut depth = 0;
    let mut cursor = 0;
    let mut node = reference.root;
    while let Some(id) = node {
        depth += 1;
        node = match &reference.nodes[id] {
            Node::Leaf { .. } => None,
            Node::Branch {
                prefix, children, ..
            } => {
                cursor += prefix.len();
                let child = children[target_path[cursor] as usize];
                cursor += 1;
                child.map(|hash| reference.ids[&hash])
            }
        };
    }
    assert_eq!(
        depth,
        SHARED + 2,
        "one branch per shared nibble plus the leaf"
    );

    let mut harness = Harness::new(1 << 30, &reference);
    harness.set_root(root).unwrap();
    let events = vec![vec![Op::Delete { key: target }]];
    let expected = reference_roots(&mut reference, &events);
    assert_eq!(harness.run_block(root, &events).unwrap(), expected);
    assert!(
        harness.rounds.len() <= depth + 1,
        "rounds={} depth={depth}",
        harness.rounds.len()
    );
    assert!(harness.rounds.len() < 65);
}
