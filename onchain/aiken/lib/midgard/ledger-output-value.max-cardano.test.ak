use midgard/ledger_output_value_v1
use midgard/validation_merkle_v1

const maximum_cardano_value_asset_count = 1592

const maximum_cardano_value_lovelace = 30000000

const terminal_pre_control =
  #"87010100581c1111111111111111111111111111111111111111111111111111111184582018a9e6706a9c0f115695a7d384af88baa135d31fa409e78ee3ab09ca6fe4cf8f18e41902ab1908e8845820ae48ba80db2f915cc4653c8bfd3a3394d4fe56f340dbe7cf66225478a2a93a65061910b6193620d87a80"

const terminal_post_control =
  #"8701020040845820bbcb3bff6f87a2005a336b6cb5fe5fbea093815716945279140f31aec8cbaba2000000845820bbcb3bff6f87a2005a336b6cb5fe5fbea093815716945279140f31aec8cbaba2000000d8799f83582035df7dc7ebdd5dba96f45ab79dbf23ba6a6325fc3b51f99b7c0cf62c8a31efb419138a193f46ff"

const policy_transition_pre_control =
  #"87010018e4581c1212121212121212121212121212121212121212121212121212121284582018a9e6706a9c0f115695a7d384af88baa135d31fa409e78ee3ab09ca6fe4cf8f18e41902ab1908e8845820e6b1158ed70eadba4dd3edea999fbc8ce3f345de8a1c442c11d25f67245209b905190deb192d14d87a80"

const policy_transition_post_control =
  #"87010018e3581c11111111111111111111111111111111111111111111111111111111845820f3a575175904810deba1e2e614bb689c08dfd8797bfb1913783c8dd98ed55af501030a845820ae48ba80db2f915cc4653c8bfd3a3394d4fe56f340dbe7cf66225478a2a93a65061910b6193620d87a80"

fn maximum_cardano_value_peaks() -> List<validation_merkle_v1.FrontierPeak> {
  [
    validation_merkle_v1.FrontierPeak {
      height: 3,
      hash: #"d7186f5f4ba03f35771f5fe9a2bb1d98c1b6647fdb307a3a990674bdef9f44eb",
    },
    validation_merkle_v1.FrontierPeak {
      height: 4,
      hash: #"3b64e0dee5d4fb8dea89b2e43cec29e9d042e70855235cdf5ee5dfba032370c8",
    },
    validation_merkle_v1.FrontierPeak {
      height: 5,
      hash: #"99005b0619518750da865e276edd6ded5162e42e975b84ca5a0b39e2106d0c04",
    },
    validation_merkle_v1.FrontierPeak {
      height: 9,
      hash: #"e140a7bcc4ae85c668a78b45e94afac7712d80519d9d4e1ad42c1e09afd56c71",
    },
    validation_merkle_v1.FrontierPeak {
      height: 10,
      hash: #"d0f89d5bf0b4028db4ba39470ca2ff296bc5733936ddfef6beb77278f1003833",
    },
  ]
}

fn policy_transition_witness(
  quantity: Int,
) -> ledger_output_value_v1.LedgerOutputValueWitnessV1 {
  ledger_output_value_v1.LedgerOutputValueAsset {
    policy_id: #"11111111111111111111111111111111111111111111111111111111",
    asset_name: #"e2",
    quantity,
    siblings: [
      #"c8ff3191003d91361e84195924aba18210e02790385d36debdcbd8f70bbe30ce",
      #"e57595be0911f47b6ad9a4fc577e4dd80633ab1708ea007f1e10f2c046023271",
      #"9bab8e0b298f6ba239d749199354e3b5077454fdc4c46e9be6ee181b57620a36",
      #"ad95bca1382117e5c4150798dd59d0e50cb61f0ce24ae2cead5bb278d129863b",
      #"b741a1bc87ac41225567abf47022d28aff8bef9010b3f555dd95cc37606f19cc",
      #"4326fd84d7f7c1cb1c22a7d9be5cd7dd1e4e0cac784c895fe34a22252757e90f",
      #"e3aac35704e9bbab8073bd8db99057348328982a5d454a7af9be0891419510ef",
      #"16233275d44a0986f77f8f15153b441404e7248a0c844166968868f7d7bd996e",
      #"9f1efec8e1306f877d11aa735c34e3ff4491ee38d5b428bc5d3b03c0bd33b490",
      #"4ebf5ad0166b0ae660e33b8b4037a666ea07a55c72ffd3ec3b51e9cff53bdd1c",
    ],
  }
}

test maximum_cardano_value_terminal_fold_matches_typescript() {
  let pre = ledger_output_value_v1.decode_control_v1(terminal_pre_control)
  expect Some(post) = ledger_output_value_v1.step_v1(
    pre,
    maximum_cardano_value_asset_count,
    maximum_cardano_value_peaks(),
    maximum_cardano_value_lovelace,
    ledger_output_value_v1.LedgerOutputValueNoWitness,
  )
  and {
    pre.stage == ledger_output_value_v1.stage_finalize,
    pre.asset_remaining == 0,
    post.stage == ledger_output_value_v1.stage_terminal,
    ledger_output_value_v1.encode_control_v1(post) == terminal_post_control,
    ledger_output_value_v1.finalize_v1(post) != None,
  }
}

test maximum_cardano_value_policy_transition_matches_typescript() {
  let pre = ledger_output_value_v1.decode_control_v1(
    policy_transition_pre_control,
  )
  expect Some(post) = ledger_output_value_v1.step_v1(
    pre,
    maximum_cardano_value_asset_count,
    maximum_cardano_value_peaks(),
    maximum_cardano_value_lovelace,
    policy_transition_witness(1),
  )
  and {
    pre.asset_remaining == 228,
    post.asset_remaining == 227,
    post.current_policy == #"11111111111111111111111111111111111111111111111111111111",
    ledger_output_value_v1.encode_control_v1(post) == policy_transition_post_control,
    ledger_output_value_v1.step_v1(
      pre,
      maximum_cardano_value_asset_count,
      maximum_cardano_value_peaks(),
      maximum_cardano_value_lovelace,
      policy_transition_witness(2),
    ) == None,
  }
}
