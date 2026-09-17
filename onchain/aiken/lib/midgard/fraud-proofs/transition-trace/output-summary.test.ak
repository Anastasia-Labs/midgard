use midgard/cek_data_v1
use midgard/script_context_v1

test dense_value_summary_matches_canonical_descriptor() {
  script_context_v1.ledger_output_summaries_with_value_v1(
    #"a200581d60aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa018200a0",
    Some(
      cek_data_v1.DataSummaryV1 {
        root: #"add54d30058e271fc5847cd7b979e05d234fc7395d4044a3481b716b82e3be04",
        cbor_length: 5002,
        memory: 14149,
      },
    ),
  ) == Some(
    (
      cek_data_v1.DataSummaryV1 {
        root: #"6aa793a77428e649db031f3f031958b5ad602ea5ae61fbda19aed0773038e170",
        cbor_length: 5053,
        memory: 14205,
      },
      cek_data_v1.DataSummaryV1 {
        root: #"6aa793a77428e649db031f3f031958b5ad602ea5ae61fbda19aed0773038e170",
        cbor_length: 5053,
        memory: 14205,
      },
      cek_data_v1.DataSummaryV1 {
        root: #"9525e1ea4350de9f831fc817b64355d7c3e26427effb7f4ca9bd29541d5eda39",
        cbor_length: 3,
        memory: 4,
      },
    ),
  )
}
