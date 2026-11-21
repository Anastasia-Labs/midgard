use super::*;

fn empty_input(events: &[Vec<Op>]) -> Vec<u8> {
    let op_count = events.iter().map(Vec::len).sum::<usize>();
    let mut input = Vec::new();
    input.extend_from_slice(INPUT_MAGIC);
    push_u16(&mut input, ABI_VERSION);
    push_u16(&mut input, 0);
    push_u32(&mut input, 64).unwrap();
    push_u32(&mut input, 64).unwrap();
    push_u32(&mut input, 128).unwrap();
    push_u32(&mut input, 1 << 20).unwrap();
    push_u32(&mut input, 1 << 20).unwrap();
    push_u32(&mut input, 0).unwrap();
    push_u32(&mut input, events.len()).unwrap();
    push_u32(&mut input, op_count).unwrap();
    input.extend_from_slice(&EMPTY_ROOT);
    for event in events {
        push_u32(&mut input, event.len()).unwrap();
        for op in event {
            match op {
                Op::Insert { key, value } => {
                    input.push(1);
                    push_u16(&mut input, key.len() as u16);
                    push_u32(&mut input, value.len()).unwrap();
                    input.extend_from_slice(key);
                    input.extend_from_slice(value);
                }
                Op::Delete { key } => {
                    input.push(2);
                    push_u16(&mut input, key.len() as u16);
                    push_u32(&mut input, 0).unwrap();
                    input.extend_from_slice(key);
                }
            }
        }
    }
    input
}

fn assert_delta_records_authenticate(output: &[u8]) {
    let dirty_count = u32::from_le_bytes(output[12..16].try_into().unwrap()) as usize;
    let delta_offset = u32::from_le_bytes(output[16..20].try_into().unwrap()) as usize;
    let mut reader = Reader::new(&output[delta_offset..]);
    for _ in 0..dirty_count {
        parse_node(&mut reader).expect("emitted dirty record must re-authenticate");
    }
    assert_eq!(reader.remaining(), 0);
}

#[test]
fn canonical_empty_root_is_stable() {
    assert_eq!(hex(&digest(&[&[]])), hex(&EMPTY_ROOT));
}

#[test]
fn rejects_invalid_mutation_atomically() {
    let input = empty_input(&[vec![Op::Delete { key: vec![1; 32] }]]);
    assert!(run_engine(&input).unwrap_err().contains("key is absent"));
}

#[test]
fn emits_mandatory_root_for_empty_event_and_reinsert() {
    let key = vec![0x52; 32];
    let input = empty_input(&[
        vec![Op::Insert {
            key: key.clone(),
            value: vec![1],
        }],
        vec![],
        vec![
            Op::Delete { key: key.clone() },
            Op::Insert {
                key,
                value: vec![2],
            },
        ],
    ]);
    let output = run_engine(&input).unwrap();
    assert_eq!(&output[..4], OUTPUT_MAGIC);
    assert_eq!(u32::from_le_bytes(output[8..12].try_into().unwrap()), 3);
    assert_eq!(&output[120..152], &output[152..184]);
    assert_ne!(&output[152..184], &output[184..216]);
}

#[test]
fn emitted_closure_reauthenticates_after_repeated_sibling_updates() {
    let numbered_key = |index: u32| {
        let mut key = vec![0u8; 32];
        key[28..].copy_from_slice(&index.to_be_bytes());
        key
    };
    let mut events = vec![(0..64)
        .map(|index| Op::Insert {
            key: numbered_key(index),
            value: vec![index as u8; 16],
        })
        .collect::<Vec<_>>()];
    events.extend((0..32).map(|index| {
        vec![
            Op::Delete {
                key: numbered_key(index),
            },
            Op::Insert {
                key: numbered_key(1_000 + index),
                value: vec![(index + 17) as u8; 16],
            },
        ]
    }));
    let output = run_engine(&empty_input(&events)).unwrap();
    assert_delta_records_authenticate(&output);
}
