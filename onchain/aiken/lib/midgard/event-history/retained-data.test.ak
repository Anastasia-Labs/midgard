use cardano/address.{Script, VerificationKey}
use cardano/assets
use cardano/transaction.{InlineDatum, Input, Output, OutputReference}
use midgard/event_history/retained_data.{EventData}

const retention = #"11111111111111111111111111111111111111111111111111111111"

const owner = #"22222222222222222222222222222222222222222222222222222222"

const stranger = #"33333333333333333333333333333333333333333333333333333333"

const key = #"4444444444444444444444444444444444444444444444444444444444444444"

const other_key =
  #"5555555555555555555555555555555555555555555555555555555555555555"

fn stored() -> EventData {
  let payload: Data = 42
  EventData {
    event_key: key,
    event_payload: payload,
    reclaim_auth: VerificationKey(owner),
  }
}

fn publication(data: EventData, index: Int) -> Input {
  let raw: Data = data
  Input {
    output_reference: OutputReference {
      transaction_id: other_key,
      output_index: index,
    },
    output: Output {
      address: address.from_script(retention),
      value: assets.from_lovelace(3_000_000),
      datum: InlineDatum(raw),
      reference_script: None,
    },
  }
}

fn read(
  references: List<Input>,
  spent: List<Input>,
  hash: ByteArray,
) -> EventData {
  retained_data.authenticate(
    references,
    spent,
    0,
    address.from_script(retention),
    key,
    hash,
    1024,
  )
}

test actual_existing_publication_authenticates() {
  read([publication(stored(), 0)], [], retained_data.datum_hash(stored())) == stored()
}

test identical_copy_can_replace_original_publication_reference() {
  read([publication(stored(), 1)], [], retained_data.datum_hash(stored())) == stored()
}

test missing_publication_reference_rejects() fail {
  expect _ = read([], [], retained_data.datum_hash(stored()))
  True
}

test consuming_publication_during_admission_rejects() fail {
  let data = publication(stored(), 0)
  expect _ = read([data], [data], retained_data.datum_hash(stored()))
  True
}

test wrong_storage_script_rejects_even_with_exact_datum_hash() fail {
  let data = publication(stored(), 0)
  let wrong = Input {
    ..data,
    output: Output { ..data.output, address: address.from_script(stranger) },
  }
  expect _ = read([wrong], [], retained_data.datum_hash(stored()))
  True
}

test wrong_event_key_rejects_even_with_matching_complete_hash() fail {
  let data = EventData { ..stored(), event_key: other_key }
  expect _ = read([publication(data, 0)], [], retained_data.datum_hash(data))
  True
}

test reclaim_authorization_is_bound_by_complete_datum_hash() fail {
  let changed = EventData {
    ..stored(),
    reclaim_auth: VerificationKey(stranger),
  }
  expect _ = read(
    [publication(changed, 0)],
    [],
    retained_data.datum_hash(stored()),
  )
  True
}

test payload_substitution_rejects() fail {
  let payload: Data = 43
  let changed = EventData { ..stored(), event_payload: payload }
  expect _ = read(
    [publication(changed, 0)],
    [],
    retained_data.datum_hash(stored()),
  )
  True
}

test key_reclaim_requires_exact_extra_signatory() {
  and {
    retained_data.owner_authorized(VerificationKey(owner), [owner], []),
    !retained_data.owner_authorized(VerificationKey(owner), [stranger], []),
    !retained_data.owner_authorized(
      VerificationKey(owner),
      [],
      [Pair(VerificationKey(owner), 0)],
    ),
    !retained_data.owner_authorized(
      VerificationKey(owner),
      [],
      [Pair(Script(owner), 0)],
    ),
  }
}

test script_reclaim_requires_exact_zero_rewarding_purpose() {
  and {
    retained_data.owner_authorized(Script(owner), [], [Pair(Script(owner), 0)]),
    !retained_data.owner_authorized(Script(owner), [owner], []),
    !retained_data.owner_authorized(Script(owner), [], [Pair(Script(owner), 1)]),
    !retained_data.owner_authorized(
      Script(owner),
      [],
      [Pair(Script(stranger), 0)],
    ),
    !retained_data.owner_authorized(
      Script(owner),
      [],
      [Pair(VerificationKey(owner), 0)],
    ),
  }
}
