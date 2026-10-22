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
    root: #"03edb835e579c1d0cd6bbfa5e0b69a49c83b5044c5ed827f392863cb35fda711",
    cbor_length: 9505,
    memory: 1156,
  }
}

test ledger_data_blob_9216_matches_host() {
  cek_data_v1.semantic_data_summary_v1(builtin.b_data(ledger_bytes(9216))) == cek_data_v1.DataSummaryV1 {
    root: #"c8e3d77aa2c9b72948710bd206a0b4638f2b17128655e823081ed030ec1bbbe2",
    cbor_length: 9506,
    memory: 1156,
  }
}

test ledger_data_blob_12000_matches_host() {
  cek_data_v1.semantic_data_summary_v1(builtin.b_data(ledger_bytes(12000))) == cek_data_v1.DataSummaryV1 {
    root: #"be341a43932b98d87e088cf8c5e8e6da703812f385d5f1486eaba3a53c978f71",
    cbor_length: 12378,
    memory: 1504,
  }
}

test ledger_data_blob_15841_matches_host() {
  cek_data_v1.semantic_data_summary_v1(builtin.b_data(ledger_bytes(15841))) == cek_data_v1.DataSummaryV1 {
    root: #"1986a67ca1725f8b725146734d47e861815d5bf4a387277c01bfe7001edd1f6b",
    cbor_length: 16339,
    memory: 1985,
  }
}

test ledger_data_blob_16384_matches_host() {
  cek_data_v1.semantic_data_summary_v1(builtin.b_data(ledger_bytes(16384))) == cek_data_v1.DataSummaryV1 {
    root: #"15c362915ea9b73ac9bdc88fc056cbdc60034f221edd9e6cf9875d0c13e7f6d5",
    cbor_length: 16898,
    memory: 2052,
  }
}

test ledger_data_blob_over_bound_refuses() fail {
  cek_data_v1.semantic_data_summary_v1(builtin.b_data(ledger_bytes(16385))).memory == 0
}
