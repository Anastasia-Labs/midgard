use midgard/fraud_proofs/native_tx/components.{encode_midgard_versioned_script}
use midgard/fraud_proofs/native_tx/types.{
  MidgardV1Script, MidgardVersionedScript, NativeCardanoScript, PlutusV3Script,
}
use midgard/fraud_proofs/script_integrity_hash_missing/rule

const nonzero =
  #"0101010101010101010101010101010101010101010101010101010101010101"

test wrongful_acceptance_requires_an_effectful_zero_hash() {
  and {
    rule.fault_holds_v1(rule.zero_hash32, True, False),
    rule.fault_holds_v1(rule.zero_hash32, False, True),
    !rule.fault_holds_v1(rule.zero_hash32, False, False),
  }
}

test honest_nonzero_integrity_hash_never_matches_the_missing_hash_reason() {
  and {
    !rule.fault_holds_v1(nonzero, True, False),
    !rule.fault_holds_v1(nonzero, False, True),
  }
}

test script_language_scan_distinguishes_native_from_effectful_scripts() {
  let native = encode_midgard_versioned_script(
    MidgardVersionedScript {
      language: NativeCardanoScript,
      script_bytes: #"00",
    },
  )
  let plutus = encode_midgard_versioned_script(
    MidgardVersionedScript { language: PlutusV3Script, script_bytes: #"01" },
  )
  let midgard = encode_midgard_versioned_script(
    MidgardVersionedScript { language: MidgardV1Script, script_bytes: #"02" },
  )
  and {
    !rule.contains_non_native_script_items_v1([]),
    !rule.contains_non_native_script_items_v1([native]),
    rule.contains_non_native_script_items_v1([native, plutus]),
    rule.contains_non_native_script_items_v1([midgard]),
  }
}

test short_integrity_hash_refuses_closed() fail {
  rule.fault_holds_v1(#"00", True, False)
}
