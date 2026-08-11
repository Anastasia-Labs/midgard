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
  #"84018c590141885826825820050505050505050505050505050505050505050505050505050505050505050519000058268258200909090909090909090909090909090909090909090909090909090909090909190000582682582011111111111111111111111111111111111111111111111111111111111111111900005826825820191919191919191919191919191919191919191919191919191919191919191919000058268258202222222222222222222222222222222222222222222222222222222222222222190000582682582027272727272727272727272727272727272727272727272727272727272727271900005826825820313131313131313131313131313131313131313131313131313131313131313119000058268258203a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a19000058a1845826825820454545454545454545454545454545454545454545454545454545454545454519000058268258205252525252525252525252525252525252525252525252525252525252525252190000582682582066666666666666666666666666666666666666666666666666666666666666661900005826825820707070707070707070707070707070707070707070707070707070707070707019000059046c8c586ca2005839002b218f09113f8c3900e461c3ea9985152b27c410dbc2e1d111eb32627388845fb012f3205a2f3239746b66c2b71370f2daacbbe0a3aa41ed01821a003d0900a1581cc012db50e7f3e256aa5ea8806397141c7a8e571fc3abcf537e773a55a242a0000142b0000b586ca2005839002b218f09113f8c3900e461c3ea9985152b27c410dbc2e1d111eb32627388845fb012f3205a2f3239746b66c2b71370f2daacbbe0a3aa41ed01821a003d0900a1581cf9a2778be9b42cdd59e74568bbf243aca5591210e3ed2bbb90c7f0ffa242a1010242b1010c586ca2005839002b218f09113f8c3900e461c3ea9985152b27c410dbc2e1d111eb32627388845fb012f3205a2f3239746b66c2b71370f2daacbbe0a3aa41ed01821a003d0900a1581c1717e4e1e346932539ff3c8ec8ab14ae5d9ec67d640e90fba75ddf90a242a2020342b2020d586ca2005839002b218f09113f8c3900e461c3ea9985152b27c410dbc2e1d111eb32627388845fb012f3205a2f3239746b66c2b71370f2daacbbe0a3aa41ed01821a003d0900a1581c35acb16c87c65a724675f3166eec068a621ecfa4fc28c2611febf781a242a3030442b3030e586ca2005839002b218f09113f8c3900e461c3ea9985152b27c410dbc2e1d111eb32627388845fb012f3205a2f3239746b66c2b71370f2daacbbe0a3aa41ed01821a003d0900a1581c1e17a3ac6aa59edc53d143460cce20514d7ec6129fb5139b30a7f8f4a242a4040542b4040f586ca2005839002b218f09113f8c3900e461c3ea9985152b27c410dbc2e1d111eb32627388845fb012f3205a2f3239746b66c2b71370f2daacbbe0a3aa41ed01821a003d0900a1581c9df1a46687c1957165a9b13392d823de39e1fc19a9e31ec7d3a56c21a242a5050642b505105849a3005839002b218f09113f8c3900e461c3ea9985152b27c410dbc2e1d111eb32627388845fb012f3205a2f3239746b66c2b71370f2daacbbe0a3aa41ed01821a003d0900a00242184d5875a3005839002b218f09113f8c3900e461c3ea9985152b27c410dbc2e1d111eb32627388845fb012f3205a2f3239746b66c2b71370f2daacbbe0a3aa41ed01821a003d0900a0038203582b8501830101005820232f5cb642756b658af2c4b1d0cf3039225ddb95949a40b2bee2f81ced6e8b750518ba5876a3005839002b218f09113f8c3900e461c3ea9985152b27c410dbc2e1d111eb32627388845fb012f3205a2f3239746b66c2b71370f2daacbbe0a3aa41ed01821a003d0900a003821880582b85018301010058206e4b877969858fa545cd2938661e73759cdb0710f96927c1015336302d46e7800518ba5829a200581d78b558ffa52d7865766337a144efe3edf45aaf59bd1daad75d5955d25601821a003d0900a05829a200581d780b67a9e8deaa64fc23ff7d14caff584d07ea297e59d31f665fac812201821a003d0900a05845a2005839002b218f09113f8c3900e461c3ea9985152b27c410dbc2e1d111eb32627388845fb012f3205a2f3239746b66c2b71370f2daacbbe0a3aa41ed01821a02255100a0002020583d82581c077cbf2442d3c97a7d203a50e5b2179b743ed4e8edcd79c9d002e7c1581c8529c862e25edeffa0922a17859aa8355aff900f16ff176de6d44866581f81581c2b218f09113f8c3900e461c3ea9985152b27c410dbc2e1d111eb326258eba6581c1717e4e1e346932539ff3c8ec8ab14ae5d9ec67d640e90fba75ddf90a242a2020342b2020d581c1e17a3ac6aa59edc53d143460cce20514d7ec6129fb5139b30a7f8f4a242a4040542b4040f581c35acb16c87c65a724675f3166eec068a621ecfa4fc28c2611febf781a242a3030442b3030e581c9df1a46687c1957165a9b13392d823de39e1fc19a9e31ec7d3a56c21a242a5050642b50510581cc012db50e7f3e256aa5ea8806397141c7a8e571fc3abcf537e773a55a242a0000142b0000b581cf9a2778be9b42cdd59e74568bbf243aca5591210e3ed2bbb90c7f0ffa242a1010242b1010c5820b9044305942ef29099e5f8b2272b1e24a3eb8bf43f2472898a4eb4cb1fac2e77582001f4b788593d4f70de2a45c2e1e87088bfbdfa29577ae1b62aba60e095e3ab530083586881586582582020578a9a8283f754d152e41391de5cb8f9d63f8acb5d71e557f974b1173a9a965840c8789a6041735c40b0fe6d35aaa86cbeea00e65577561e0bfb2a860861982d2aedfb5b5c5a6f8d0d4370e53ec5a42e805ae8846bf975131dd3bc07b95411b4025902668d8203582b8501830101005820f8fa26a2d92eb001c2621f6e8acc06ae42e37cba03e30f7384a0fd28ebe9e9150518ba8203582b85018301010058207ce89afd89f0338bea94b40a588e218184d01a16df603834860fd845b5803f760518ba8203582b850183010100582007a7a210ce1dc9448b694fc12b59e9e4935f39e7552eed0280bb6c06a4a49cfb0518ba8203582b850183010100582076aabc4c1fbe263659a3b24081053033c6c1d9f710675f0405ff1dec7a85f1910518ba8203582b850183010100582052a77aa55efa9a822267a74abec2a28c1ca043d6f0f5362cc58930ee8b6348a00518ba8203582b8501830101005820b19ca8d03180a24967c6d94c64f157921088196d307295953be2df742a7c6f180518ba8203582b8501830101005820c8f36c4f4a07609c7a6454368812ba4fc80433b984dd3bacce206dc56bb8b6170518ba8203582b85018301010058206142581df4098c38d7aee066cae1b27cf1608547fe79e094b9d63a01ec16b2aa0518ba8203582b85018301010058205d17cc44dceded000d12d2bf3bb2d3b28a7ba51b5a3d7643475f5f55ac0040ea0518ba8203582b850183010100582056c6243178152ea9f7c2b72261fa0051c37b3340943d0cf4622546e8e29f55440518ba8203582b8501830101005820be7f07f533105cf0b614e19baffceedd366f1872966b784d8c8d5fac20fc1ba50518ba821880582b85018301010058208aa3818967ce21fcf839d01d80da982eac9f041a13aa4df6046a5c794b582f5b0518ba821880582b85018301010058204f2f00db13bc33c211421f296a81392d67d3db66850a56d1f8dfe44b821a1dda0518ba587a8d8400014218668201028400044218658201028400074218678201028401004218ca8201028401014218cc8201028401024218cb8201028401034218cd8201028401044218c88201028401054218c98201028403004319012c8201028403014319012c820102840600431901918201028406014319019082010200"

const high_cardinality_compact_cbor =
  #"84018c5820be74c3435e86da19b3fde4f464cd3d7eb64a6b520a5d341154880271c4a1303758205782d8b111e146a549ba081dc1fb099e7743497b5bce29ec72a85f2019cad0bc58206964e9026802c8033a265f2e2b5a7547dd24fb203b3c874242fcc2585fc426cd0020205820f9e0d6e641d40f55e7b4451d8d843876853c175474c09d4f61eaeabac0ff40005820453dfb0d91ede6f16748af7c53dd089e3c66b793e029cf6ee93ac71d4c4adca95820f35ee4999365b55237cbfe126a77ef8dfc525f96fb884a3cf7a9e34c629a52085820b9044305942ef29099e5f8b2272b1e24a3eb8bf43f2472898a4eb4cb1fac2e77582001f4b788593d4f70de2a45c2e1e87088bfbdfa29577ae1b62aba60e095e3ab53005820f43ffe12a2ad719d44f49dac827c73fc86ef84c752676d334addaeb760875bd800"

const high_cardinality_tx_id =
  #"9666f9109586fdd1efa280e790c481a95bb593e8fdd6ede3376e7d44661bd0bc"

const high_cardinality_spend_inputs_preimage_cbor =
  #"885826825820050505050505050505050505050505050505050505050505050505050505050519000058268258200909090909090909090909090909090909090909090909090909090909090909190000582682582011111111111111111111111111111111111111111111111111111111111111111900005826825820191919191919191919191919191919191919191919191919191919191919191919000058268258202222222222222222222222222222222222222222222222222222222222222222190000582682582027272727272727272727272727272727272727272727272727272727272727271900005826825820313131313131313131313131313131313131313131313131313131313131313119000058268258203a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a190000"

const high_cardinality_reference_inputs_preimage_cbor =
  #"8458268258204545454545454545454545454545454545454545454545454545454545454545190000582682582052525252525252525252525252525252525252525252525252525252525252521900005826825820666666666666666666666666666666666666666666666666666666666666666619000058268258207070707070707070707070707070707070707070707070707070707070707070190000"

const high_cardinality_outputs_preimage_cbor =
  #"8c586ca2005839002b218f09113f8c3900e461c3ea9985152b27c410dbc2e1d111eb32627388845fb012f3205a2f3239746b66c2b71370f2daacbbe0a3aa41ed01821a003d0900a1581cc012db50e7f3e256aa5ea8806397141c7a8e571fc3abcf537e773a55a242a0000142b0000b586ca2005839002b218f09113f8c3900e461c3ea9985152b27c410dbc2e1d111eb32627388845fb012f3205a2f3239746b66c2b71370f2daacbbe0a3aa41ed01821a003d0900a1581cf9a2778be9b42cdd59e74568bbf243aca5591210e3ed2bbb90c7f0ffa242a1010242b1010c586ca2005839002b218f09113f8c3900e461c3ea9985152b27c410dbc2e1d111eb32627388845fb012f3205a2f3239746b66c2b71370f2daacbbe0a3aa41ed01821a003d0900a1581c1717e4e1e346932539ff3c8ec8ab14ae5d9ec67d640e90fba75ddf90a242a2020342b2020d586ca2005839002b218f09113f8c3900e461c3ea9985152b27c410dbc2e1d111eb32627388845fb012f3205a2f3239746b66c2b71370f2daacbbe0a3aa41ed01821a003d0900a1581c35acb16c87c65a724675f3166eec068a621ecfa4fc28c2611febf781a242a3030442b3030e586ca2005839002b218f09113f8c3900e461c3ea9985152b27c410dbc2e1d111eb32627388845fb012f3205a2f3239746b66c2b71370f2daacbbe0a3aa41ed01821a003d0900a1581c1e17a3ac6aa59edc53d143460cce20514d7ec6129fb5139b30a7f8f4a242a4040542b4040f586ca2005839002b218f09113f8c3900e461c3ea9985152b27c410dbc2e1d111eb32627388845fb012f3205a2f3239746b66c2b71370f2daacbbe0a3aa41ed01821a003d0900a1581c9df1a46687c1957165a9b13392d823de39e1fc19a9e31ec7d3a56c21a242a5050642b505105849a3005839002b218f09113f8c3900e461c3ea9985152b27c410dbc2e1d111eb32627388845fb012f3205a2f3239746b66c2b71370f2daacbbe0a3aa41ed01821a003d0900a00242184d5875a3005839002b218f09113f8c3900e461c3ea9985152b27c410dbc2e1d111eb32627388845fb012f3205a2f3239746b66c2b71370f2daacbbe0a3aa41ed01821a003d0900a0038203582b8501830101005820232f5cb642756b658af2c4b1d0cf3039225ddb95949a40b2bee2f81ced6e8b750518ba5876a3005839002b218f09113f8c3900e461c3ea9985152b27c410dbc2e1d111eb32627388845fb012f3205a2f3239746b66c2b71370f2daacbbe0a3aa41ed01821a003d0900a003821880582b85018301010058206e4b877969858fa545cd2938661e73759cdb0710f96927c1015336302d46e7800518ba5829a200581d78b558ffa52d7865766337a144efe3edf45aaf59bd1daad75d5955d25601821a003d0900a05829a200581d780b67a9e8deaa64fc23ff7d14caff584d07ea297e59d31f665fac812201821a003d0900a05845a2005839002b218f09113f8c3900e461c3ea9985152b27c410dbc2e1d111eb32627388845fb012f3205a2f3239746b66c2b71370f2daacbbe0a3aa41ed01821a02255100a0"

const high_cardinality_required_observers_preimage_cbor =
  #"82581c077cbf2442d3c97a7d203a50e5b2179b743ed4e8edcd79c9d002e7c1581c8529c862e25edeffa0922a17859aa8355aff900f16ff176de6d44866"

const high_cardinality_required_signers_preimage_cbor =
  #"81581c2b218f09113f8c3900e461c3ea9985152b27c410dbc2e1d111eb3262"

const high_cardinality_mint_preimage_cbor =
  #"a6581c1717e4e1e346932539ff3c8ec8ab14ae5d9ec67d640e90fba75ddf90a242a2020342b2020d581c1e17a3ac6aa59edc53d143460cce20514d7ec6129fb5139b30a7f8f4a242a4040542b4040f581c35acb16c87c65a724675f3166eec068a621ecfa4fc28c2611febf781a242a3030442b3030e581c9df1a46687c1957165a9b13392d823de39e1fc19a9e31ec7d3a56c21a242a5050642b50510581cc012db50e7f3e256aa5ea8806397141c7a8e571fc3abcf537e773a55a242a0000142b0000b581cf9a2778be9b42cdd59e74568bbf243aca5591210e3ed2bbb90c7f0ffa242a1010242b1010c"

const high_cardinality_addr_wits_preimage_cbor =
  #"81586582582020578a9a8283f754d152e41391de5cb8f9d63f8acb5d71e557f974b1173a9a965840c8789a6041735c40b0fe6d35aaa86cbeea00e65577561e0bfb2a860861982d2aedfb5b5c5a6f8d0d4370e53ec5a42e805ae8846bf975131dd3bc07b95411b402"

const high_cardinality_script_wits_preimage_cbor =
  #"8d8203582b8501830101005820f8fa26a2d92eb001c2621f6e8acc06ae42e37cba03e30f7384a0fd28ebe9e9150518ba8203582b85018301010058207ce89afd89f0338bea94b40a588e218184d01a16df603834860fd845b5803f760518ba8203582b850183010100582007a7a210ce1dc9448b694fc12b59e9e4935f39e7552eed0280bb6c06a4a49cfb0518ba8203582b850183010100582076aabc4c1fbe263659a3b24081053033c6c1d9f710675f0405ff1dec7a85f1910518ba8203582b850183010100582052a77aa55efa9a822267a74abec2a28c1ca043d6f0f5362cc58930ee8b6348a00518ba8203582b8501830101005820b19ca8d03180a24967c6d94c64f157921088196d307295953be2df742a7c6f180518ba8203582b8501830101005820c8f36c4f4a07609c7a6454368812ba4fc80433b984dd3bacce206dc56bb8b6170518ba8203582b85018301010058206142581df4098c38d7aee066cae1b27cf1608547fe79e094b9d63a01ec16b2aa0518ba8203582b85018301010058205d17cc44dceded000d12d2bf3bb2d3b28a7ba51b5a3d7643475f5f55ac0040ea0518ba8203582b850183010100582056c6243178152ea9f7c2b72261fa0051c37b3340943d0cf4622546e8e29f55440518ba8203582b8501830101005820be7f07f533105cf0b614e19baffceedd366f1872966b784d8c8d5fac20fc1ba50518ba821880582b85018301010058208aa3818967ce21fcf839d01d80da982eac9f041a13aa4df6046a5c794b582f5b0518ba821880582b85018301010058204f2f00db13bc33c211421f296a81392d67d3db66850a56d1f8dfe44b821a1dda0518ba"

const high_cardinality_redeemer_wits_preimage_cbor =
  #"8d8400014218668201028400044218658201028400074218678201028401004218ca8201028401014218cc8201028401024218cb8201028401034218cd8201028401044218c88201028401054218c98201028403004319012c8201028403014319012c8201028406004319019182010284060143190190820102"

const high_cardinality_spend_inputs_hash =
  #"be74c3435e86da19b3fde4f464cd3d7eb64a6b520a5d341154880271c4a13037"

const high_cardinality_reference_inputs_hash =
  #"5782d8b111e146a549ba081dc1fb099e7743497b5bce29ec72a85f2019cad0bc"

const high_cardinality_outputs_hash =
  #"6964e9026802c8033a265f2e2b5a7547dd24fb203b3c874242fcc2585fc426cd"

const high_cardinality_required_observers_hash =
  #"f9e0d6e641d40f55e7b4451d8d843876853c175474c09d4f61eaeabac0ff4000"

const high_cardinality_required_signers_hash =
  #"453dfb0d91ede6f16748af7c53dd089e3c66b793e029cf6ee93ac71d4c4adca9"

const high_cardinality_mint_hash =
  #"f35ee4999365b55237cbfe126a77ef8dfc525f96fb884a3cf7a9e34c629a5208"

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
  verified.tx_compact.validity_code == 0
}

bench high_cardinality_verify_native_tx_compact_cbor_bench(
  compact_cbor: ByteArray via sample_high_cardinality_compact_cbor,
) {
  let verified = verify_native_tx_compact_cbor_v1(
    high_cardinality_tx_id,
    compact_cbor,
  )
  verified.tx_compact.validity_code == 0
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
  verified.tx_compact.validity_code == 0
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
