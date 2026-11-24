use super::*;

#[test]
fn canonical_empty_root_is_stable() {
    assert_eq!(hex(&digest(&[&[]])), hex(&EMPTY_ROOT));
}

#[test]
fn refuses_to_delete_an_absent_key() {
    let mut arena = Arena::new();
    let error = arena
        .apply_event(&[Op::Delete { key: vec![1; 32] }])
        .unwrap_err();
    assert!(error.contains("key is absent"));
}

#[test]
fn empty_event_keeps_the_root_and_a_reinsert_changes_it() {
    let key = vec![0x52; 32];
    let mut arena = Arena::new();
    let inserted = arena
        .apply_event(&[Op::Insert {
            key: key.clone(),
            value: vec![1],
        }])
        .unwrap();
    let unchanged = arena.apply_event(&[]).unwrap();
    let reinserted = arena
        .apply_event(&[
            Op::Delete { key: key.clone() },
            Op::Insert {
                key,
                value: vec![2],
            },
        ])
        .unwrap();
    assert_eq!(inserted, unchanged);
    assert_ne!(unchanged, reinserted);
}
