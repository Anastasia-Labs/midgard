use aiken/builtin
use midgard/cek_data_v1

fn ledger_bytes(length: Int) -> ByteArray {
  if length <= 8192 {
    builtin.replicate_byte(length, 171)
  } else {
    builtin.append_bytearray(
      builtin.replicate_byte(8192, 171),
      ledger_bytes(length - 8192),
    )
  }
}

test ledger_data_blob_9215_matches_host() {
  cek_data_v1.semantic_data_summary_v1(builtin.b_data(ledger_bytes(9215))) == cek_data_v1.DataSummaryV1 {
    root: #"c2efac0c7d78bca123484c80799200772859a97f54965d59881f06c434892988",
    cbor_length: 9505,
    memory: 9219,
  }
}

test ledger_data_blob_9216_matches_host() {
  cek_data_v1.semantic_data_summary_v1(builtin.b_data(ledger_bytes(9216))) == cek_data_v1.DataSummaryV1 {
    root: #"61705d68831234591650ebd3281d2eaf55819dfc489938644dfeaade1b4658d2",
    cbor_length: 9506,
    memory: 9220,
  }
}

test ledger_data_blob_12000_matches_host() {
  cek_data_v1.semantic_data_summary_v1(builtin.b_data(ledger_bytes(12000))) == cek_data_v1.DataSummaryV1 {
    root: #"ff21a0015d198c6ca7497145d7091c3c1f82c6189d8ac36a659338b713ae5ffd",
    cbor_length: 12378,
    memory: 12004,
  }
}

test ledger_data_blob_15841_matches_host() {
  cek_data_v1.semantic_data_summary_v1(builtin.b_data(ledger_bytes(15841))) == cek_data_v1.DataSummaryV1 {
    root: #"d120efaa40f0772065121c25fb7039ce03943bb1ea0c59e4e22b1b3f6b7c7c8c",
    cbor_length: 16339,
    memory: 15845,
  }
}

test ledger_data_blob_16384_matches_host() {
  cek_data_v1.semantic_data_summary_v1(builtin.b_data(ledger_bytes(16384))) == cek_data_v1.DataSummaryV1 {
    root: #"31a6e4176316af58f404da1d2e282e07190cd552fb6ac7f309b8ddd8b48b1f40",
    cbor_length: 16898,
    memory: 16388,
  }
}

test ledger_data_blob_over_bound_refuses() fail {
  cek_data_v1.semantic_data_summary_v1(builtin.b_data(ledger_bytes(16385))).memory == 0
}
