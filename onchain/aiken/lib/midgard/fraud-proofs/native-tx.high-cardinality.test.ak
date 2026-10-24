// The high-cardinality conformance fixture's Aiken twin.
//
// The constants below are GENERATED and every other line here is written by
// hand. Both halves matter: re-running the writer and then the rebinder is the
// only supported way to move a constant, and editing the tests around them is
// expected.
//
//   seed:     demo/lucid-midgard/tests/fixtures/native-high-cardinality.ts
//   writer:   pnpm --dir demo/lucid-midgard run fixtures:native-high-cardinality:sync
//   rebinder: pnpm --dir demo/lucid-midgard run fixtures:native-compact
//
// Which constants are generated is stated exactly once, in the rebinder's
// `FIXTURE_FAMILIES` table; `fixtures:native-compact:check` fails if any of them
// is no longer what the fixture implies.

use aiken/builtin
use aiken/collection/list
use aiken/fuzz
use midgard/fraud_proofs/native_tx/compact.{
  decode_native_tx_compact_v1, encode_native_tx_body_compact,
  encode_native_tx_compact_v1, native_tx_id_for_version,
  verify_native_tx_compact_cbor_v1, verify_native_tx_compact_v1,
}
use midgard/fraud_proofs/native_tx/preimages.{
  decode_midgard_tx_address_witnesses_preimage_cbor,
  decode_midgard_tx_byte_list_preimage_cbor,
  decode_midgard_tx_inputs_preimage_cbor, decode_midgard_tx_mint_preimage_cbor,
  decode_midgard_tx_outputs_preimage_cbor,
  decode_midgard_tx_redeemer_witnesses_preimage_cbor,
  decode_midgard_tx_script_witnesses_preimage_cbor,
  encode_address_witness_preimage, encode_input_preimage, encode_mint_preimage,
  encode_output_preimage, encode_redeemer_witness_preimage,
  encode_script_witness_preimage,
}
use midgard/fraud_proofs/native_tx/transaction.{
  decode_midgard_transaction_v1, encode_midgard_transaction_v1,
  midgard_transaction_to_compact, midgard_transaction_witness_set_to_compact,
  partial_view_from_compact_and_preimages,
}
use midgard/fraud_proofs/native_tx/types.{
  MidgardRedeemerPurpose, MidgardRedeemerWitness,
  MidgardTransactionBodyPartialPreimages,
  MidgardTransactionWitnessSetPartialPreimages, MintRedeemer, NativeTxCompact,
  ReceiveRedeemer, RewardRedeemer, SpendRedeemer,
}

const high_cardinality_tx_cbor =
  #"84018c590141885826825820050505050505050505050505050505050505050505050505050505050505050519000058268258200909090909090909090909090909090909090909090909090909090909090909190000582682582011111111111111111111111111111111111111111111111111111111111111111900005826825820191919191919191919191919191919191919191919191919191919191919191919000058268258202222222222222222222222222222222222222222222222222222222222222222190000582682582027272727272727272727272727272727272727272727272727272727272727271900005826825820313131313131313131313131313131313131313131313131313131313131313119000058268258203a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a19000058a184582682582045454545454545454545454545454545454545454545454545454545454545451900005826825820525252525252525252525252525252525252525252525252525252525252525219000058268258206666666666666666666666666666666666666666666666666666666666666666190000582682582070707070707070707070707070707070707070707070707070707070707070701900005904228c586ca2005839002b218f09113f8c3900e461c3ea9985152b27c410dbc2e1d111eb32627388845fb012f3205a2f3239746b66c2b71370f2daacbbe0a3aa41ed01821a003d0900a1581cc64aea12b392b5cb2a52d9406afd989a500fe4ee17e98eae12280132a242a0000142b0000b586ca2005839002b218f09113f8c3900e461c3ea9985152b27c410dbc2e1d111eb32627388845fb012f3205a2f3239746b66c2b71370f2daacbbe0a3aa41ed01821a003d0900a1581c3a97a5f24edeeeb3fde8635588646ab11cc278e87928891a7b0abae8a242a1010242b1010c586ca2005839002b218f09113f8c3900e461c3ea9985152b27c410dbc2e1d111eb32627388845fb012f3205a2f3239746b66c2b71370f2daacbbe0a3aa41ed01821a003d0900a1581c28f9121278aee0522ba5485271a94bdad115d540b1800227df66cf78a242a2020342b2020d586ca2005839002b218f09113f8c3900e461c3ea9985152b27c410dbc2e1d111eb32627388845fb012f3205a2f3239746b66c2b71370f2daacbbe0a3aa41ed01821a003d0900a1581cf3ff62e1864ebbbd19d94173a9b6304573b932f3b021452c2d596e56a242a3030442b3030e586ca2005839002b218f09113f8c3900e461c3ea9985152b27c410dbc2e1d111eb32627388845fb012f3205a2f3239746b66c2b71370f2daacbbe0a3aa41ed01821a003d0900a1581cc5ce4ef0393b038bc2ca4df6e53cbdebfe65fa6f7ff71b8d25cc3ef5a242a4040542b4040f586ca2005839002b218f09113f8c3900e461c3ea9985152b27c410dbc2e1d111eb32627388845fb012f3205a2f3239746b66c2b71370f2daacbbe0a3aa41ed01821a003d0900a1581cdb3d19d56424e1bffd21001ef32a561dff2fe2cec741781fc9aabff6a242a5050642b505105849a3005839002b218f09113f8c3900e461c3ea9985152b27c410dbc2e1d111eb32627388845fb012f3205a2f3239746b66c2b71370f2daacbbe0a3aa41ed01821a003d0900a00242184d5850a3005839002b218f09113f8c3900e461c3ea9985152b27c410dbc2e1d111eb32627388845fb012f3205a2f3239746b66c2b71370f2daacbbe0a3aa41ed01821a003d0900a003820347460101004806815851a3005839002b218f09113f8c3900e461c3ea9985152b27c410dbc2e1d111eb32627388845fb012f3205a2f3239746b66c2b71370f2daacbbe0a3aa41ed01821a003d0900a00382188047460101004807015829a200581d78238774ef9f4b3b0f8f8de684800d8400007274590f574c3adf3b15ec01821a003d0900a05829a200581d78a1acc3b977322fb5c094a492f1b384a4569dfcda4c9a5023a3e6c48a01821a003d0900a05845a2005839002b218f09113f8c3900e461c3ea9985152b27c410dbc2e1d111eb32627388845fb012f3205a2f3239746b66c2b71370f2daacbbe0a3aa41ed01821a02255100a0002020583d82581c4d54bb358c2df3a5a181ebb6f07706fd99ffa18fe1329507fba325e2581ccb6a637ccc8555d544b48c7a96c2557a6e050a86226c09510efa6adc581f81581c2b218f09113f8c3900e461c3ea9985152b27c410dbc2e1d111eb326258fd86582882581c28f9121278aee0522ba5485271a94bdad115d540b1800227df66cf78a242a2020342b2020d582882581c3a97a5f24edeeeb3fde8635588646ab11cc278e87928891a7b0abae8a242a1010242b1010c582882581cc5ce4ef0393b038bc2ca4df6e53cbdebfe65fa6f7ff71b8d25cc3ef5a242a4040542b4040f582882581cc64aea12b392b5cb2a52d9406afd989a500fe4ee17e98eae12280132a242a0000142b0000b582882581cdb3d19d56424e1bffd21001ef32a561dff2fe2cec741781fc9aabff6a242a5050642b50510582882581cf3ff62e1864ebbbd19d94173a9b6304573b932f3b021452c2d596e56a242a3030442b3030e58204c268975c54054d0c4f5f83cab7e7f5be7493860e0af3f0f11f4fca35bdccc5c582001f4b788593d4f70de2a45c2e1e87088bfbdfa29577ae1b62aba60e095e3ab530083586881586582582020578a9a8283f754d152e41391de5cb8f9d63f8acb5d71e557f974b1173a9a965840e42e5bc4e65620a31dcaefb864dc79894170338028730e10b45f49d6f62eb931a1984fc9582d3de5105e9842d4efaf926a112f518dcdd766f6c0118e5673200c58928d4a820347460101004800014a820347460101004800814a820347460101004801014a820347460101004801814a820347460101004802014a820347460101004802814a820347460101004803014a820347460101004803814a820347460101004804014a820347460101004804814a820347460101004805014b82188047460101004805814b821880474601010048060158878d498400014218668201024984000442186582010249840007421867820102498401004218ca820102498401014218c9820102498401024218cc820102498401034218c8820102498401044218cd820102498401054218cb8201024a8403004319012c8201024a8403014319012c8201024a840600431901908201024a8406014319019182010200"

const high_cardinality_compact_cbor =
  #"84018c582099272cc37b5ef7d39d69a25f035251d47bf8b2ba5037cca8c80e83062e129795582071ccf64e13bf69772ad3d039a7b98df9df56cf72507fab67d250f4f3bfa96289582096dab6f4cd6cb769fae1eb1a7da4646223552afbf197df6cf85caecb95cba1f400202058208566008c2934e4c6d396238f9f9ba6f8b59ed0fec30a5820c42af941c66dd8785820ddc9251016435006deab0c3aeab4defff8a4fc6847122e046f6b25d529fa5bfc58205a48617d15087f86854e24532572be6fffeb8c4fdd27b8ee1df5647471da21b158204c268975c54054d0c4f5f83cab7e7f5be7493860e0af3f0f11f4fca35bdccc5c582001f4b788593d4f70de2a45c2e1e87088bfbdfa29577ae1b62aba60e095e3ab53005820b68e35e669b75ed8d0c35225607a520d52a1f007462fd0ee1bdf37d6806160e900"

const high_cardinality_tx_id =
  #"16686d38468fdacc6e067958843befbcfbe9b931b25203ba01324faccfb3f48b"

const high_cardinality_spend_inputs_preimage_cbor =
  #"885826825820050505050505050505050505050505050505050505050505050505050505050519000058268258200909090909090909090909090909090909090909090909090909090909090909190000582682582011111111111111111111111111111111111111111111111111111111111111111900005826825820191919191919191919191919191919191919191919191919191919191919191919000058268258202222222222222222222222222222222222222222222222222222222222222222190000582682582027272727272727272727272727272727272727272727272727272727272727271900005826825820313131313131313131313131313131313131313131313131313131313131313119000058268258203a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a190000"

const high_cardinality_reference_inputs_preimage_cbor =
  #"8458268258204545454545454545454545454545454545454545454545454545454545454545190000582682582052525252525252525252525252525252525252525252525252525252525252521900005826825820666666666666666666666666666666666666666666666666666666666666666619000058268258207070707070707070707070707070707070707070707070707070707070707070190000"

const high_cardinality_outputs_preimage_cbor =
  #"8c586ca2005839002b218f09113f8c3900e461c3ea9985152b27c410dbc2e1d111eb32627388845fb012f3205a2f3239746b66c2b71370f2daacbbe0a3aa41ed01821a003d0900a1581cc64aea12b392b5cb2a52d9406afd989a500fe4ee17e98eae12280132a242a0000142b0000b586ca2005839002b218f09113f8c3900e461c3ea9985152b27c410dbc2e1d111eb32627388845fb012f3205a2f3239746b66c2b71370f2daacbbe0a3aa41ed01821a003d0900a1581c3a97a5f24edeeeb3fde8635588646ab11cc278e87928891a7b0abae8a242a1010242b1010c586ca2005839002b218f09113f8c3900e461c3ea9985152b27c410dbc2e1d111eb32627388845fb012f3205a2f3239746b66c2b71370f2daacbbe0a3aa41ed01821a003d0900a1581c28f9121278aee0522ba5485271a94bdad115d540b1800227df66cf78a242a2020342b2020d586ca2005839002b218f09113f8c3900e461c3ea9985152b27c410dbc2e1d111eb32627388845fb012f3205a2f3239746b66c2b71370f2daacbbe0a3aa41ed01821a003d0900a1581cf3ff62e1864ebbbd19d94173a9b6304573b932f3b021452c2d596e56a242a3030442b3030e586ca2005839002b218f09113f8c3900e461c3ea9985152b27c410dbc2e1d111eb32627388845fb012f3205a2f3239746b66c2b71370f2daacbbe0a3aa41ed01821a003d0900a1581cc5ce4ef0393b038bc2ca4df6e53cbdebfe65fa6f7ff71b8d25cc3ef5a242a4040542b4040f586ca2005839002b218f09113f8c3900e461c3ea9985152b27c410dbc2e1d111eb32627388845fb012f3205a2f3239746b66c2b71370f2daacbbe0a3aa41ed01821a003d0900a1581cdb3d19d56424e1bffd21001ef32a561dff2fe2cec741781fc9aabff6a242a5050642b505105849a3005839002b218f09113f8c3900e461c3ea9985152b27c410dbc2e1d111eb32627388845fb012f3205a2f3239746b66c2b71370f2daacbbe0a3aa41ed01821a003d0900a00242184d5850a3005839002b218f09113f8c3900e461c3ea9985152b27c410dbc2e1d111eb32627388845fb012f3205a2f3239746b66c2b71370f2daacbbe0a3aa41ed01821a003d0900a003820347460101004806815851a3005839002b218f09113f8c3900e461c3ea9985152b27c410dbc2e1d111eb32627388845fb012f3205a2f3239746b66c2b71370f2daacbbe0a3aa41ed01821a003d0900a00382188047460101004807015829a200581d78238774ef9f4b3b0f8f8de684800d8400007274590f574c3adf3b15ec01821a003d0900a05829a200581d78a1acc3b977322fb5c094a492f1b384a4569dfcda4c9a5023a3e6c48a01821a003d0900a05845a2005839002b218f09113f8c3900e461c3ea9985152b27c410dbc2e1d111eb32627388845fb012f3205a2f3239746b66c2b71370f2daacbbe0a3aa41ed01821a02255100a0"

const high_cardinality_required_observers_preimage_cbor =
  #"82581c4d54bb358c2df3a5a181ebb6f07706fd99ffa18fe1329507fba325e2581ccb6a637ccc8555d544b48c7a96c2557a6e050a86226c09510efa6adc"

const high_cardinality_required_signers_preimage_cbor =
  #"81581c2b218f09113f8c3900e461c3ea9985152b27c410dbc2e1d111eb3262"

const high_cardinality_mint_preimage_cbor =
  #"86582882581c28f9121278aee0522ba5485271a94bdad115d540b1800227df66cf78a242a2020342b2020d582882581c3a97a5f24edeeeb3fde8635588646ab11cc278e87928891a7b0abae8a242a1010242b1010c582882581cc5ce4ef0393b038bc2ca4df6e53cbdebfe65fa6f7ff71b8d25cc3ef5a242a4040542b4040f582882581cc64aea12b392b5cb2a52d9406afd989a500fe4ee17e98eae12280132a242a0000142b0000b582882581cdb3d19d56424e1bffd21001ef32a561dff2fe2cec741781fc9aabff6a242a5050642b50510582882581cf3ff62e1864ebbbd19d94173a9b6304573b932f3b021452c2d596e56a242a3030442b3030e"

const high_cardinality_addr_wits_preimage_cbor =
  #"81586582582020578a9a8283f754d152e41391de5cb8f9d63f8acb5d71e557f974b1173a9a965840e42e5bc4e65620a31dcaefb864dc79894170338028730e10b45f49d6f62eb931a1984fc9582d3de5105e9842d4efaf926a112f518dcdd766f6c0118e5673200c"

const high_cardinality_script_wits_preimage_cbor =
  #"8d4a820347460101004800014a820347460101004800814a820347460101004801014a820347460101004801814a820347460101004802014a820347460101004802814a820347460101004803014a820347460101004803814a820347460101004804014a820347460101004804814a820347460101004805014b82188047460101004805814b8218804746010100480601"

const high_cardinality_redeemer_wits_preimage_cbor =
  #"8d498400014218668201024984000442186582010249840007421867820102498401004218ca820102498401014218c9820102498401024218cc820102498401034218c8820102498401044218cd820102498401054218cb8201024a8403004319012c8201024a8403014319012c8201024a840600431901908201024a84060143190191820102"

const high_cardinality_spend_inputs_hash =
  #"99272cc37b5ef7d39d69a25f035251d47bf8b2ba5037cca8c80e83062e129795"

const high_cardinality_reference_inputs_hash =
  #"71ccf64e13bf69772ad3d039a7b98df9df56cf72507fab67d250f4f3bfa96289"

const high_cardinality_outputs_hash =
  #"96dab6f4cd6cb769fae1eb1a7da4646223552afbf197df6cf85caecb95cba1f4"

const high_cardinality_required_observers_hash =
  #"8566008c2934e4c6d396238f9f9ba6f8b59ed0fec30a5820c42af941c66dd878"

const high_cardinality_required_signers_hash =
  #"ddc9251016435006deab0c3aeab4defff8a4fc6847122e046f6b25d529fa5bfc"

const high_cardinality_mint_hash =
  #"5a48617d15087f86854e24532572be6fffeb8c4fdd27b8ee1df5647471da21b1"

fn sample_high_cardinality_tx_cbor(_size: Int) -> Fuzzer<ByteArray> {
  fuzz.constant(high_cardinality_tx_cbor)
}

fn sample_high_cardinality_compact_cbor(_size: Int) -> Fuzzer<ByteArray> {
  fuzz.constant(high_cardinality_compact_cbor)
}

fn sample_high_cardinality_compact_fixture(
  _size: Int,
) -> Fuzzer<Pair<NativeTxCompact, ByteArray>> {
  fuzz.constant(
    Pair(
      decode_native_tx_compact_v1(high_cardinality_compact_cbor),
      high_cardinality_compact_cbor,
    ),
  )
}

fn sample_high_cardinality_spend_inputs_preimage_cbor(
  _size: Int,
) -> Fuzzer<ByteArray> {
  fuzz.constant(high_cardinality_spend_inputs_preimage_cbor)
}

fn sample_high_cardinality_reference_inputs_preimage_cbor(
  _size: Int,
) -> Fuzzer<ByteArray> {
  fuzz.constant(high_cardinality_reference_inputs_preimage_cbor)
}

fn sample_high_cardinality_outputs_preimage_cbor(
  _size: Int,
) -> Fuzzer<ByteArray> {
  fuzz.constant(high_cardinality_outputs_preimage_cbor)
}

fn sample_high_cardinality_required_observers_preimage_cbor(
  _size: Int,
) -> Fuzzer<ByteArray> {
  fuzz.constant(high_cardinality_required_observers_preimage_cbor)
}

fn sample_high_cardinality_required_signers_preimage_cbor(
  _size: Int,
) -> Fuzzer<ByteArray> {
  fuzz.constant(high_cardinality_required_signers_preimage_cbor)
}

fn sample_high_cardinality_mint_preimage_cbor(_size: Int) -> Fuzzer<ByteArray> {
  fuzz.constant(high_cardinality_mint_preimage_cbor)
}

fn sample_high_cardinality_addr_wits_preimage_cbor(
  _size: Int,
) -> Fuzzer<ByteArray> {
  fuzz.constant(high_cardinality_addr_wits_preimage_cbor)
}

fn sample_high_cardinality_script_wits_preimage_cbor(
  _size: Int,
) -> Fuzzer<ByteArray> {
  fuzz.constant(high_cardinality_script_wits_preimage_cbor)
}

fn sample_high_cardinality_redeemer_wits_preimage_cbor(
  _size: Int,
) -> Fuzzer<ByteArray> {
  fuzz.constant(high_cardinality_redeemer_wits_preimage_cbor)
}

fn redeemer_purpose_tag(purpose: MidgardRedeemerPurpose) -> Int {
  when purpose is {
    SpendRedeemer -> 0
    MintRedeemer -> 1
    RewardRedeemer -> 3
    ReceiveRedeemer -> 6
    _ -> -1
  }
}

fn redeemer_pointer_tags(
  redeemers: List<MidgardRedeemerWitness>,
) -> List<Pair<Int, Int>> {
  redeemers
    |> list.map(
        fn(redeemer) {
          Pair(redeemer_purpose_tag(redeemer.purpose), redeemer.index)
        },
      )
}

bench high_cardinality_decode_midgard_transaction_bench(
  tx_cbor: ByteArray via sample_high_cardinality_tx_cbor,
) {
  let decoded = decode_midgard_transaction_v1(tx_cbor)
  and {
    decoded.version == 1,
    list.length(decoded.body.inputs) == 8,
    list.length(decoded.body.reference_inputs) == 4,
    list.length(decoded.body.outputs) == 12,
    list.length(decoded.witness_set.redeemer_tx_wits) == 13,
  }
}

bench high_cardinality_decode_native_tx_compact_bench(
  compact_cbor: ByteArray via sample_high_cardinality_compact_cbor,
) {
  let compact = decode_native_tx_compact_v1(compact_cbor)
  compact.validity_code == 0
}

bench high_cardinality_verify_native_tx_compact_reencode_bench(
  fixture: Pair<NativeTxCompact, ByteArray> via sample_high_cardinality_compact_fixture,
) {
  let Pair(compact, compact_cbor) = fixture
  let verified = verify_native_tx_compact_v1(
    high_cardinality_tx_id,
    compact,
    compact_cbor,
  )
  verified.tx_compact.validity_code == Some(0)
}

bench high_cardinality_verify_native_tx_compact_cbor_bench(
  compact_cbor: ByteArray via sample_high_cardinality_compact_cbor,
) {
  let verified = verify_native_tx_compact_cbor_v1(
    high_cardinality_tx_id,
    compact_cbor,
  )
  verified.tx_compact.validity_code == Some(0)
}

bench high_cardinality_decode_then_verify_native_tx_compact_reencode_bench(
  compact_cbor: ByteArray via sample_high_cardinality_compact_cbor,
) {
  let compact = decode_native_tx_compact_v1(compact_cbor)
  let verified = verify_native_tx_compact_v1(
    high_cardinality_tx_id,
    compact,
    compact_cbor,
  )
  verified.tx_compact.validity_code == Some(0)
}

bench high_cardinality_decode_spend_inputs_preimage_bench(
  preimage_cbor: ByteArray via sample_high_cardinality_spend_inputs_preimage_cbor,
) {
  list.length(decode_midgard_tx_inputs_preimage_cbor(preimage_cbor)) == 8
}

bench high_cardinality_decode_reference_inputs_preimage_bench(
  preimage_cbor: ByteArray via sample_high_cardinality_reference_inputs_preimage_cbor,
) {
  list.length(decode_midgard_tx_inputs_preimage_cbor(preimage_cbor)) == 4
}

bench high_cardinality_decode_outputs_preimage_bench(
  preimage_cbor: ByteArray via sample_high_cardinality_outputs_preimage_cbor,
) {
  list.length(decode_midgard_tx_outputs_preimage_cbor(preimage_cbor)) == 12
}

bench high_cardinality_decode_required_observers_preimage_bench(
  preimage_cbor: ByteArray via sample_high_cardinality_required_observers_preimage_cbor,
) {
  list.length(decode_midgard_tx_byte_list_preimage_cbor(preimage_cbor)) == 2
}

bench high_cardinality_decode_required_signers_preimage_bench(
  preimage_cbor: ByteArray via sample_high_cardinality_required_signers_preimage_cbor,
) {
  list.length(decode_midgard_tx_byte_list_preimage_cbor(preimage_cbor)) == 1
}

bench high_cardinality_decode_mint_preimage_bench(
  preimage_cbor: ByteArray via sample_high_cardinality_mint_preimage_cbor,
) {
  let mint = decode_midgard_tx_mint_preimage_cbor(preimage_cbor)
  list.length(builtin.un_map_data(mint)) == 6
}

bench high_cardinality_decode_address_witnesses_preimage_bench(
  preimage_cbor: ByteArray via sample_high_cardinality_addr_wits_preimage_cbor,
) {
  list.length(decode_midgard_tx_address_witnesses_preimage_cbor(preimage_cbor)) == 1
}

bench high_cardinality_decode_script_witnesses_preimage_bench(
  preimage_cbor: ByteArray via sample_high_cardinality_script_wits_preimage_cbor,
) {
  list.length(decode_midgard_tx_script_witnesses_preimage_cbor(preimage_cbor)) == 13
}

bench high_cardinality_decode_redeemer_witnesses_preimage_bench(
  preimage_cbor: ByteArray via sample_high_cardinality_redeemer_wits_preimage_cbor,
) {
  list.length(decode_midgard_tx_redeemer_witnesses_preimage_cbor(preimage_cbor)) == 13
}

test high_cardinality_lucid_midgard_native_tx_decodes() {
  let tx_cbor = high_cardinality_tx_cbor
  let compact_cbor = high_cardinality_compact_cbor
  let tx_id = high_cardinality_tx_id
  let spend_inputs_preimage_cbor = high_cardinality_spend_inputs_preimage_cbor
  let reference_inputs_preimage_cbor = high_cardinality_reference_inputs_preimage_cbor
  let outputs_preimage_cbor = high_cardinality_outputs_preimage_cbor
  let required_observers_preimage_cbor = high_cardinality_required_observers_preimage_cbor
  let required_signers_preimage_cbor = high_cardinality_required_signers_preimage_cbor
  let mint_preimage_cbor = high_cardinality_mint_preimage_cbor
  let addr_wits_preimage_cbor = high_cardinality_addr_wits_preimage_cbor
  let script_wits_preimage_cbor = high_cardinality_script_wits_preimage_cbor
  let redeemer_wits_preimage_cbor = high_cardinality_redeemer_wits_preimage_cbor
  let decoded_spend_inputs = decode_midgard_tx_inputs_preimage_cbor(
    spend_inputs_preimage_cbor,
  )
  let decoded_reference_inputs = decode_midgard_tx_inputs_preimage_cbor(
    reference_inputs_preimage_cbor,
  )
  let decoded_outputs = decode_midgard_tx_outputs_preimage_cbor(
    outputs_preimage_cbor,
  )
  let decoded_mint = decode_midgard_tx_mint_preimage_cbor(mint_preimage_cbor)
  let decoded_addr_wits = decode_midgard_tx_address_witnesses_preimage_cbor(
    addr_wits_preimage_cbor,
  )
  let decoded_script_wits = decode_midgard_tx_script_witnesses_preimage_cbor(
    script_wits_preimage_cbor,
  )
  let decoded_redeemer_wits = decode_midgard_tx_redeemer_witnesses_preimage_cbor(
    redeemer_wits_preimage_cbor,
  )
  expect
    encode_input_preimage(decoded_spend_inputs) == spend_inputs_preimage_cbor
  expect
    encode_input_preimage(decoded_reference_inputs) == reference_inputs_preimage_cbor
  expect encode_output_preimage(decoded_outputs) == outputs_preimage_cbor
  expect encode_mint_preimage(decoded_mint) == mint_preimage_cbor
  expect
    encode_address_witness_preimage(decoded_addr_wits) == addr_wits_preimage_cbor
  expect
    encode_script_witness_preimage(decoded_script_wits) == script_wits_preimage_cbor
  expect
    encode_redeemer_witness_preimage(decoded_redeemer_wits) == redeemer_wits_preimage_cbor

  let decoded = decode_midgard_transaction_v1(tx_cbor)
  let compact = midgard_transaction_to_compact(decoded)
  let compact_from_cbor = decode_native_tx_compact_v1(compact_cbor)
  let verified = verify_native_tx_compact_v1(
    tx_id,
    compact_from_cbor,
    compact_cbor,
  )
  let verified_from_cbor = verify_native_tx_compact_cbor_v1(tx_id, compact_cbor)
  let witness_set_compact = midgard_transaction_witness_set_to_compact(
    decoded.witness_set,
  )
  let body_preimages = MidgardTransactionBodyPartialPreimages {
    inputs: Some(spend_inputs_preimage_cbor),
    reference_inputs: Some(reference_inputs_preimage_cbor),
    outputs: Some(outputs_preimage_cbor),
    required_observers: Some(required_observers_preimage_cbor),
    required_signers: Some(required_signers_preimage_cbor),
    mint: Some(mint_preimage_cbor),
  }
  let witness_set_preimages = MidgardTransactionWitnessSetPartialPreimages {
    addr_tx_wits: Some(addr_wits_preimage_cbor),
    script_tx_wits: Some(script_wits_preimage_cbor),
    redeemer_tx_wits: Some(redeemer_wits_preimage_cbor),
  }
  let partial = partial_view_from_compact_and_preimages(
    verified,
    body_preimages,
    Some(witness_set_compact),
    witness_set_preimages,
  )
  expect Some(partial_body) = partial.body
  expect Some(partial_witness_set) = partial.witness_set

  and {
    verified_from_cbor == verified,
    encode_midgard_transaction_v1(decoded) == tx_cbor,
    compact == compact_from_cbor,
    encode_native_tx_compact_v1(compact) == compact_cbor,
    native_tx_id_for_version(
      decoded.version,
      encode_native_tx_body_compact(compact.body),
    ) == tx_id,
    list.length(decoded.body.inputs) == 8,
    list.length(decoded.body.reference_inputs) == 4,
    list.length(decoded.body.outputs) == 12,
    list.length(builtin.un_map_data(decoded.body.mint)) == 6,
    list.length(decoded.witness_set.addr_tx_wits) == 1,
    list.length(decoded.witness_set.script_tx_wits) == 13,
    list.length(decoded.witness_set.redeemer_tx_wits) == 13,
    list.count(
      decoded.witness_set.redeemer_tx_wits,
      fn(redeemer) { redeemer.purpose == SpendRedeemer },
    ) == 3,
    list.count(
      decoded.witness_set.redeemer_tx_wits,
      fn(redeemer) { redeemer.purpose == MintRedeemer },
    ) == 6,
    list.count(
      decoded.witness_set.redeemer_tx_wits,
      fn(redeemer) { redeemer.purpose == RewardRedeemer },
    ) == 2,
    list.count(
      decoded.witness_set.redeemer_tx_wits,
      fn(redeemer) { redeemer.purpose == ReceiveRedeemer },
    ) == 2,
    redeemer_pointer_tags(decoded.witness_set.redeemer_tx_wits) == [
      Pair(0, 1), Pair(0, 4), Pair(0, 7), Pair(1, 0), Pair(1, 1), Pair(1, 2),
      Pair(1, 3), Pair(1, 4), Pair(1, 5), Pair(3, 0), Pair(3, 1), Pair(6, 0),
      Pair(6, 1),
    ],
    encode_input_preimage(decoded_spend_inputs) == spend_inputs_preimage_cbor,
    encode_input_preimage(decoded_reference_inputs) == reference_inputs_preimage_cbor,
    encode_output_preimage(decoded_outputs) == outputs_preimage_cbor,
    encode_mint_preimage(decoded_mint) == mint_preimage_cbor,
    encode_address_witness_preimage(decoded_addr_wits) == addr_wits_preimage_cbor,
    encode_script_witness_preimage(decoded_script_wits) == script_wits_preimage_cbor,
    encode_redeemer_witness_preimage(decoded_redeemer_wits) == redeemer_wits_preimage_cbor,
    list.length(decoded_spend_inputs) == 8,
    list.length(decoded_reference_inputs) == 4,
    list.length(decoded_outputs) == 12,
    list.length(
      decode_midgard_tx_byte_list_preimage_cbor(
        required_observers_preimage_cbor,
      ),
    ) == 2,
    list.length(
      decode_midgard_tx_byte_list_preimage_cbor(required_signers_preimage_cbor),
    ) == 1,
    list.length(builtin.un_map_data(decoded_mint)) == 6,
    list.length(decoded_addr_wits) == 1,
    list.length(decoded_script_wits) == 13,
    list.length(decoded_redeemer_wits) == 13,
    compact.body.spend_inputs_hash == high_cardinality_spend_inputs_hash,
    compact.body.reference_inputs_hash == high_cardinality_reference_inputs_hash,
    compact.body.outputs_hash == high_cardinality_outputs_hash,
    compact.body.required_observers_hash == high_cardinality_required_observers_hash,
    compact.body.required_signers_hash == high_cardinality_required_signers_hash,
    compact.body.mint_hash == high_cardinality_mint_hash,
    partial_body.inputs == Some(decoded.body.inputs),
    partial_body.reference_inputs == Some(decoded.body.reference_inputs),
    partial_body.outputs == Some(decoded.body.outputs),
    partial_body.required_observers == Some(decoded.body.required_observers),
    partial_body.required_signers == Some(decoded.body.required_signers),
    partial_body.mint == Some(decoded.body.mint),
    partial_witness_set.addr_tx_wits == Some(decoded.witness_set.addr_tx_wits),
    partial_witness_set.script_tx_wits == Some(
      decoded.witness_set.script_tx_wits,
    ),
    partial_witness_set.redeemer_tx_wits == Some(
      decoded.witness_set.redeemer_tx_wits,
    ),
  }
}
