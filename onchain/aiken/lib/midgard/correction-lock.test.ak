use cardano/address
use cardano/assets
use cardano/transaction.{InlineDatum, Input, Output, OutputReference}
use midgard/correction_lock

const hub_policy: ByteArray =
  #"11111111111111111111111111111111111111111111111111111111"

const forged_policy: ByteArray =
  #"22222222222222222222222222222222222222222222222222222222"

const lock_script: ByteArray =
  #"33333333333333333333333333333333333333333333333333333333"

const other_script: ByteArray =
  #"44444444444444444444444444444444444444444444444444444444"

const target: ByteArray =
  #"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"

const other_target: ByteArray =
  #"bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"

const proof_name: ByteArray =
  #"00000001aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"

const other_proof_name: ByteArray =
  #"00000002aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"

fn locked() -> correction_lock.Datum {
  correction_lock.Locked {
    target_header_hash: target,
    correction_identity: correction_lock.FraudProof {
      fraud_proof_asset_name: proof_name,
    },
  }
}

fn other_locked_target() -> correction_lock.Datum {
  correction_lock.Locked {
    target_header_hash: other_target,
    correction_identity: correction_lock.FraudProof {
      fraud_proof_asset_name: proof_name,
    },
  }
}

fn other_locked_identity() -> correction_lock.Datum {
  correction_lock.Locked {
    target_header_hash: target,
    correction_identity: correction_lock.FraudProof {
      fraud_proof_asset_name: other_proof_name,
    },
  }
}

fn lock_output(
  datum: correction_lock.Datum,
  policy: ByteArray,
  script: ByteArray,
) -> Output {
  Output {
    address: address.from_script(script),
    value: assets.from_lovelace(2_000_000)
      |> assets.merge(assets.from_asset(policy, correction_lock.asset_name, 1)),
    datum: InlineDatum(datum),
    reference_script: None,
  }
}

fn lock_input(datum: correction_lock.Datum, index: Int) -> Input {
  Input {
    output_reference: OutputReference {
      transaction_id: #"cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc",
      output_index: index,
    },
    output: lock_output(datum, hub_policy, lock_script),
  }
}

test correction_lock_transition_accepts_acquire_resume_and_terminal_clear() {
  and {
    correction_lock.datum_transition_is_valid(
      correction_lock.Idle,
      locked(),
      locked(),
      False,
    ),
    correction_lock.datum_transition_is_valid(
      locked(),
      locked(),
      locked(),
      False,
    ),
    correction_lock.datum_transition_is_valid(
      locked(),
      correction_lock.Idle,
      locked(),
      True,
    ),
    correction_lock.datum_transition_is_valid(
      correction_lock.Idle,
      correction_lock.Idle,
      locked(),
      True,
    ),
  }
}

test correction_lock_transition_rejects_stale_target_identity_and_unlock() {
  and {
    !correction_lock.datum_transition_is_valid(
      other_locked_target(),
      locked(),
      locked(),
      False,
    ),
    !correction_lock.datum_transition_is_valid(
      other_locked_identity(),
      locked(),
      locked(),
      False,
    ),
    !correction_lock.datum_transition_is_valid(
      locked(),
      correction_lock.Idle,
      locked(),
      False,
    ),
    !correction_lock.datum_transition_is_valid(
      locked(),
      locked(),
      locked(),
      True,
    ),
  }
}

test correction_lock_idle_reference_accepts_exact_singleton() {
  correction_lock.references_idle(
    [lock_input(correction_lock.Idle, 0)],
    hub_policy,
    address.from_script(lock_script),
  )
}

test correction_lock_idle_reference_rejects_missing() fail {
  correction_lock.references_idle(
    [],
    hub_policy,
    address.from_script(lock_script),
  )
}

test correction_lock_idle_reference_rejects_duplicate() fail {
  correction_lock.references_idle(
    [lock_input(correction_lock.Idle, 0), lock_input(correction_lock.Idle, 1)],
    hub_policy,
    address.from_script(lock_script),
  )
}

test correction_lock_idle_reference_rejects_locked() fail {
  correction_lock.references_idle(
    [lock_input(locked(), 0)],
    hub_policy,
    address.from_script(lock_script),
  )
}

test correction_lock_idle_reference_rejects_forged_policy() fail {
  let forged = Input {
    output_reference: lock_input(correction_lock.Idle, 0).output_reference,
    output: lock_output(correction_lock.Idle, forged_policy, lock_script),
  }
  correction_lock.references_idle(
    [forged],
    hub_policy,
    address.from_script(lock_script),
  )
}

test correction_lock_idle_reference_rejects_wrong_script() fail {
  let wrong_script = Input {
    output_reference: lock_input(correction_lock.Idle, 0).output_reference,
    output: lock_output(correction_lock.Idle, hub_policy, other_script),
  }
  correction_lock.references_idle(
    [wrong_script],
    hub_policy,
    address.from_script(lock_script),
  )
}

test correction_lock_correction_input_requires_unique_dedicated_singleton() {
  let authenticated = correction_lock.unique_input(
    [lock_input(locked(), 0)],
    hub_policy,
    address.from_script(lock_script),
  )
  authenticated.output_reference.output_index == 0
}

test correction_lock_correction_input_rejects_missing() fail {
  expect _authenticated = correction_lock.unique_input(
    [],
    hub_policy,
    address.from_script(lock_script),
  )
  True
}

test correction_lock_correction_input_rejects_duplicate() fail {
  expect _authenticated = correction_lock.unique_input(
    [lock_input(locked(), 0), lock_input(locked(), 1)],
    hub_policy,
    address.from_script(lock_script),
  )
  True
}

test correction_lock_correction_output_requires_unique_dedicated_singleton() {
  let authenticated = correction_lock.unique_output(
    [lock_output(locked(), hub_policy, lock_script)],
    hub_policy,
    address.from_script(lock_script),
  )
  correction_lock.decode_datum(authenticated) == locked()
}

test correction_lock_correction_output_rejects_missing() fail {
  expect _authenticated = correction_lock.unique_output(
    [],
    hub_policy,
    address.from_script(lock_script),
  )
  True
}

test correction_lock_correction_output_rejects_duplicate() fail {
  expect _authenticated = correction_lock.unique_output(
    [
      lock_output(locked(), hub_policy, lock_script),
      lock_output(locked(), hub_policy, lock_script),
    ],
    hub_policy,
    address.from_script(lock_script),
  )
  True
}

test correction_lock_rejects_non_dedicated_extra_token() {
  let with_extra = Output {
    ..lock_output(locked(), hub_policy, lock_script),
    value: lock_output(locked(), hub_policy, lock_script).value
      |> assets.merge(assets.from_asset(hub_policy, "EXTRA", 1)),
  }
  !correction_lock.output_is_dedicated(
    with_extra,
    hub_policy,
    address.from_script(lock_script),
  )
}
