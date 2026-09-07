use aiken/builtin
use aiken/fuzz
use midgard/ledger_output_proof_raw as raw
use midgard/ledger_output_proof_v1 as proof

const long_terminal =
  #"91010600192bf15820a023c9459077b4fc906660cacfa81a46eea15b9ad1f21fb20fbd745d2678f9ec970107192bf10402581d78111111111111111111111111111111111111111111111111111111111a007a1200182e000000581c555555555555555555555555555555555555555555555555555555554040028182015820fdd05992e96e478560b718d45058402827072f35e5220f396e2569800a2b76fe1854191427000319147c191481191770d8799f8701020040845820bbcb3bff6f87a2005a336b6cb5fe5fbea093815716945279140f31aec8cbaba2000000845820bbcb3bff6f87a2005a336b6cb5fe5fbea093815716945279140f31aec8cbaba2000000d8799f835820f1f2c712b57f2a363ba09d8673a2cf47b8688568395388f1baf86ce7f477c23618301852ffffd8799f8a0107185419142719142740d87a80d87a80d87a80d8799f8358202aa2efa1446b0d53ad8a806d29396c82aca037037c095811d7948f5304d3896219142719138cffff02818201582028f935b37d798dd5f68f23fa40e9d9dd02037d6b1e1fa7ad7edfdcb84b63a26cd8799f8901031917711917715840634e9ca63abb532a52c53389db12d1514358f8ff155e3d82c0622098dbdd88d3a54a6646cce0bede0423668a5079fb08595004db249d66dbc8e10681056a775c40004000ffd87a80d87a80d87a80d87a80d87a80d87a80"

test raw_initial_roundtrip(
  args via fuzz.tuple(fuzz.int_between(0, 65535), fuzz.int_between(1, 16384)),
) {
  let (index, length) = args
  let cbor = proof.encode_initial_control_v1(
    index,
    length,
    #"0000000000000000000000000000000000000000000000000000000000000000",
  )
  let frame = raw.open(cbor)
  and {
    raw.encode(frame) == cbor,
    raw.integer(frame, 2) == index,
    raw.integer(frame, 3) == length,
  }
}

test raw_long_terminal_roundtrip() {
  let frame = raw.open(long_terminal)
  and {
    raw.encode(frame) == proof.encode_control_v1(
      proof.decode_control_v1(long_terminal),
    ),
    raw.integer(frame, 1) == 6,
  }
}

test raw_replace_changes_only_requested_item() {
  let frame = raw.open(long_terminal)
  let changed = raw.replace(frame, 2, builtin.i_data(5))
  let decoded = proof.decode_control_v1(raw.encode(changed))
  decoded == proof.LedgerOutputProofControlV1 {
    ..proof.decode_control_v1(long_terminal),
    output_index: 5,
  }
}

test raw_refuses_maps() fail {
  expect _ = raw.encode_item(builtin.map_data([]))
}

test raw_refuses_foreign_constructor() fail {
  expect _ = raw.encode_item(builtin.constr_data(2, []))
}

test raw_refuses_noncanonical_control() fail {
  expect _ = raw.open(#"9f01000001a0ffffff")
}

test raw_refuses_replace_out_of_range() fail {
  expect _ = raw.replace(raw.open(long_terminal), 17, builtin.i_data(0))
}
