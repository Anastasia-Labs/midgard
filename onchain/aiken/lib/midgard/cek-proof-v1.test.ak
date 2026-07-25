use aiken/builtin
use aiken/primitive/bytearray
use midgard/cek_proof_v1

const h_01 = #"0101010101010101010101010101010101010101010101010101010101010101"

const h_02 = #"0202020202020202020202020202020202020202020202020202020202020202"

const h_03 = #"0303030303030303030303030303030303030303030303030303030303030303"

const h_04 = #"0404040404040404040404040404040404040404040404040404040404040404"

test cross_language_cek_commitment_vectors() {
  let term = cek_proof_v1.hash_application_term_v1(h_01, h_02)
  let value = cek_proof_v1.hash_lambda_value_v1(
    h_03,
    cek_proof_v1.empty_environment_root_v1,
  )
  let sequence = cek_proof_v1.hash_sequence_node_v1(
    term,
    cek_proof_v1.empty_sequence_root_v1,
    1,
  )
  let environment = cek_proof_v1.hash_environment_node_v1(
    value,
    cek_proof_v1.empty_environment_root_v1,
    1,
  )
  let continuation = cek_proof_v1.hash_apply_argument_continuation_v1(
    h_04,
    environment,
    cek_proof_v1.empty_continuation_root_v1,
  )
  and {
    cek_proof_v1.empty_sequence_root_v1 == #"8ab46e13655026ca6fe253b057ff678ebf9fa088097d6fa4c62276ba140f1743",
    cek_proof_v1.empty_environment_root_v1 == #"0b986961db44e461e897c3b03109b7f23a5270e9de71c608e518a153d57a24a7",
    cek_proof_v1.empty_continuation_root_v1 == #"53163c160dcec15695dabe0bccf6afc7f0e12db206392865db2feb0497ac838b",
    term == #"2a37aa5b923cf90c6f3c8849e8fe2b28adcda97ccd736af6bf35b8312035f431",
    value == #"a103975a15b084afa3e69e3f71ff66b57d3ce83f992df1325d0958245d337941",
    sequence == #"854e2610e77c03a89283632923ddb99af0f276bedae48865036e85c2ed1f23cb",
    environment == #"4c2bb324a912cff2fc99c9056faa7d8bb72ab5dd512362fbd6a7383fe9c71a5c",
    continuation == #"7082fd0df1eb9680517dd87620f9fa94e5ea4598a56676e224f20a833bea3513",
    cek_proof_v1.hash_machine_state_v1(
      0,
      2,
      term,
      environment,
      continuation,
      0,
      16000,
      100,
    ) == #"8a475241923d49a38fa1d6376dd1c40ebb8d16adc87f51bc4a6dea7d954f88d1",
    cek_proof_v1.hash_apply_value_continuation_v1(h_01, h_02) == #"facc375c9390b9503ccb80c0500bb3b88dc62c5da62c05599e6035938535fe61",
    cek_proof_v1.hash_case_select_continuation_v1(h_01, h_02, 3) == #"e71825335e324be2d099381365d5bc42c769bb334df509e5adae883842b9c1ea",
    cek_proof_v1.hash_case_apply_continuation_v1(h_01, h_02) == #"12aedf8df3ce9e44db4cc847cd52d0cff0b9b832ff4425954fa7c38c523e9888",
    cek_proof_v1.hash_machine_state_v1(
      7,
      0,
      h_01,
      h_02,
      cek_proof_v1.empty_continuation_root_v1,
      3,
      4,
      5,
    ) == #"31884c469ef240c7bad0a586ca242f45fa1728d13ab8385a97e5e0a18b806eb6",
    cek_proof_v1.hash_machine_state_v1(
      8,
      0,
      h_01,
      h_02,
      cek_proof_v1.empty_continuation_root_v1,
      3,
      4,
      5,
    ) == #"9d83a7edc397becce073970978dff4d6bae2c0334f0dc7c3525e35d901f47a13",
    cek_proof_v1.hash_program_envelope_v1(1, 1, 0, term, 3, 144) == #"e9d2696eff22d8078ae7bd71c83f6058e0db4d938a016314a2cd35feaccfdefa",
    cek_proof_v1.hash_blob_chunk_v1(#"010203") == #"0bb4f6f24cd0080e59e98d57a13c6453e07991452b80f14ff1d9791c02db1fc9",
    cek_proof_v1.hash_bls_miller_loop_expression_v1(h_01, h_02) == #"6bd7d80222a87e5a09102534274099c501f82c5d898fe75f24fdc951c2fb3cc0",
    cek_proof_v1.hash_bls_multiply_expression_v1(h_01, h_02) == #"be5de65e45ff867ca8394e227ee30e4f8cfaee0d4cb1d2834fe1af943b806082",
    cek_proof_v1.hash_bls_miller_loop_value_v1(
      cek_proof_v1.hash_bls_miller_loop_expression_v1(h_01, h_02),
    ) == #"d93762ca25b0c585073f9e97f50873b76e13de1290182f6bb4744d29d9fb3fb7",
  }
}

test unknown_builtin_fails_closed() fail {
  cek_proof_v1.hash_builtin_term_v1(87) == #""
}

test oversized_blob_chunk_fails_closed() fail {
  cek_proof_v1.hash_blob_chunk_v1(
    builtin.replicate_byte(cek_proof_v1.max_blob_chunk_bytes + 1, 0),
  ) == #""
}

test canonical_program_envelope_decodes_with_exact_bounds() {
  let envelope = cek_proof_v1.encode_program_envelope_v1(
    1,
    1,
    0,
    h_01,
    cek_proof_v1.max_program_node_count,
    cek_proof_v1.max_program_material_byte_length,
  )
  expect Some(decoded) = cek_proof_v1.inspect_program_envelope_v1(envelope)
  and {
    bytearray.length(envelope) == cek_proof_v1.max_program_envelope_cbor_bytes,
    decoded.term_root == h_01,
    decoded.node_count == cek_proof_v1.max_program_node_count,
    decoded.material_byte_length == cek_proof_v1.max_program_material_byte_length,
  }
}

test small_canonical_program_envelope_decodes() {
  cek_proof_v1.inspect_program_envelope_v1(
    cek_proof_v1.encode_program_envelope_v1(1, 1, 0, h_01, 3, 144),
  ) != None
}

test program_envelope_rejects_noncanonical_and_out_of_bounds_claims() {
  let noncanonical = #"8503830101005820"
    |> bytearray.concat(h_01)
    |> bytearray.concat(#"18031890")
  let legacy_v1 = #"8501830101005820"
    |> bytearray.concat(h_01)
    |> bytearray.concat(#"031890")
  and {
    cek_proof_v1.inspect_program_envelope_v1(noncanonical) == None,
    cek_proof_v1.inspect_program_envelope_v1(legacy_v1) == None,
    cek_proof_v1.inspect_program_envelope_v1(
      cek_proof_v1.encode_program_envelope_v1(1, 0, 0, h_01, 3, 144),
    ) == None,
    cek_proof_v1.inspect_program_envelope_v1(
      cek_proof_v1.encode_program_envelope_v1(
        1,
        1,
        0,
        h_01,
        cek_proof_v1.max_program_node_count + 1,
        144,
      ),
    ) == None,
    cek_proof_v1.inspect_program_envelope_v1(
      cek_proof_v1.encode_program_envelope_v1(
        1,
        1,
        0,
        h_01,
        3,
        cek_proof_v1.max_program_material_byte_length + 1,
      ),
    ) == None,
  }
}
