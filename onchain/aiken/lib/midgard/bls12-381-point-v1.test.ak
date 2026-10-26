use aiken/builtin
use midgard/bls12_381_point_v1.{g1_point_is_invalid_v1, g2_point_is_invalid_v1}

// Compressed points; their verdicts are @noble/curves' (fromBytes), which the
// TypeScript twin's tests cross-check over many more points.

/// A G1 x coordinate: the 48 bytes with the three flag bits cleared.
fn g1_x(encoded: ByteArray) -> Int {
  builtin.bytearray_to_integer(
    True,
    builtin.cons_bytearray(
      builtin.index_bytearray(encoded, 0) % 32,
      builtin.slice_bytearray(1, 47, encoded),
    ),
  )
}

/// A G2 x coordinate (c0, c1): c1 is the first 48 bytes, flags cleared.
fn g2_invalid(encoded: ByteArray) -> Bool {
  g2_point_is_invalid_v1(
    builtin.bytearray_to_integer(True, builtin.slice_bytearray(48, 48, encoded)),
    g1_x(encoded),
  )
}

test g1_generator_and_a_multiple_are_valid() {
  and {
    !g1_point_is_invalid_v1(
      g1_x(
        #"97f1d3a73197d7942695638c4fa9ac0fc3688c4f9774b905a14e3a3f171bac586c55e83ff97a1aeffb3af00adb22c6bb",
      ),
    ),
    !g1_point_is_invalid_v1(
      g1_x(
        #"af95b8218cbee2f4fa48e6b6f1df4e8ee46fee73c270dba395dad523d10c9b35295ccfc92cf0a9db8a065e16dafbfaad",
      ),
    ),
  }
}

test g1_points_outside_the_subgroup_are_invalid() {
  and {
    // (4, y): on the curve, outside G1.
    g1_point_is_invalid_v1(4),
    // (0, 2) has order 3.
    g1_point_is_invalid_v1(0),
    g1_point_is_invalid_v1(
      g1_x(
        #"8a2b9fbe12d0fd644c1e951c5983f40ca78f18e62b3b55a1d8e62cddba3d8a3736719efc37f870af7f436c2671752974",
      ),
    ),
  }
}

test g1_x_off_the_curve_is_invalid() {
  // 1 + 4 = 5 is not a square mod p.
  g1_point_is_invalid_v1(1)
}

test g2_generator_and_a_multiple_are_valid() {
  and {
    !g2_invalid(
      #"93e02b6052719f607dacd3a088274f65596bd0d09920b61ab5da61bbdc7f5049334cf11213945d57e5ac7d055d042b7e024aa2b2f08f0a91260805272dc51051c6e47ad4fa403b02b4510b647ae3d1770bac0326a805bbefd48056c8c121bdb8",
    ),
    !g2_invalid(
      #"b068ad1be382009ac2dce123ec62dca8337d6b93b909b3ee52e31cb9e4098d1b56d596bf3c08166c7b46cb3aa85c23381380055ab9f1a87786f2508f3e4ce5caa5abcdae0a80141ee8ccc3626311e0a53be5d873fa964fd85ad56771f2984579",
    ),
  }
}

test g2_x_off_the_twist_is_invalid() {
  and {
    g2_point_is_invalid_v1(1, 0),
    g2_invalid(
      #"8b55de6ec1c1b9c65236f5282637f7e67bf524d7e4187af733ef92700eeb1086f4549c5049937408f31ba2e532c0bd501587ca03a248d9ad945e56a3e3ef50f225a92648ed8a647d159479e20ff13c8ed21188535039c8fec81090af5bef34c3",
    ),
  }
}

test g2_points_outside_the_subgroup_are_invalid() {
  and {
    g2_point_is_invalid_v1(2, 0),
    g2_invalid(
      #"937f9ebde4497e13a660d4e184d974218b21d18c9d578ab770360ad3fe41a4217e4ac78c554e05bfd2fc02336b84d90505974a366247ac98e953e9afec7f10dd12080a46e6f7dede942d6265721ee3c807009433d1bc69bf82d69569713244a7",
    ),
  }
}

test g2_points_whose_y_squared_is_in_fp_are_lifted() {
  // x^3 + 4(1 + u) has no u part here: its root is in Fp in the first case
  // and in Fp u in the second. Both points are outside G2.
  and {
    g2_invalid(
      #"8000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000020e31aad2f4b199f7f87e6433692648312e55a89b142b798084e1ac133c07736855bf683690d5fa5f87e90a1b49384db0",
    ),
    g2_invalid(
      #"800000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000013012ee46c892815c3ee133c0eb6ce1708f7aced12c82cb0a7404ad8ce28e77111a8fe9d10df4f22446c901e8f26165e6a",
    ),
  }
}
