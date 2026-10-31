/**
 * Builtin tags as Cardano's flat encoding numbers them (Plutus
 * `Builtins.hs`), and the name each tag has in the textual UPLC syntax the
 * pinned aiken reads (`DefaultFunction`'s `FromStr` in its uplc crate).
 *
 * Midgard's machine, its material and every tag-keyed table use Cardano's
 * numbers. A builtin's force count and arity are `midgardCekBuiltinForceCount`
 * and `midgardCekBuiltinArgumentCount`.
 */

/** Cardano's flat tags for the BLS point compress/uncompress/hash builtins. */
export const MIDGARD_CEK_CARDANO_BLS_TAGS = Object.freeze({
  g1Compress: 58,
  g1Uncompress: 59,
  g1HashToGroup: 60,
  g2Compress: 65,
  g2Uncompress: 66,
  g2HashToGroup: 67,
});

/** Each builtin's textual name, indexed by its Cardano tag (0 to 100). */
export const MIDGARD_CEK_BUILTIN_NAMES = Object.freeze([
  "addInteger",
  "subtractInteger",
  "multiplyInteger",
  "divideInteger",
  "quotientInteger",
  "remainderInteger",
  "modInteger",
  "equalsInteger",
  "lessThanInteger",
  "lessThanEqualsInteger",
  "appendByteString",
  "consByteString",
  "sliceByteString",
  "lengthOfByteString",
  "indexByteString",
  "equalsByteString",
  "lessThanByteString",
  "lessThanEqualsByteString",
  "sha2_256",
  "sha3_256",
  "blake2b_256",
  "verifyEd25519Signature",
  "appendString",
  "equalsString",
  "encodeUtf8",
  "decodeUtf8",
  "ifThenElse",
  "chooseUnit",
  "trace",
  "fstPair",
  "sndPair",
  "chooseList",
  "mkCons",
  "headList",
  "tailList",
  "nullList",
  "chooseData",
  "constrData",
  "mapData",
  "listData",
  "iData",
  "bData",
  "unConstrData",
  "unMapData",
  "unListData",
  "unIData",
  "unBData",
  "equalsData",
  "mkPairData",
  "mkNilData",
  "mkNilPairData",
  "serialiseData",
  "verifyEcdsaSecp256k1Signature",
  "verifySchnorrSecp256k1Signature",
  "bls12_381_G1_add",
  "bls12_381_G1_neg",
  "bls12_381_G1_scalarMul",
  "bls12_381_G1_equal",
  "bls12_381_G1_compress",
  "bls12_381_G1_uncompress",
  "bls12_381_G1_hashToGroup",
  "bls12_381_G2_add",
  "bls12_381_G2_neg",
  "bls12_381_G2_scalarMul",
  "bls12_381_G2_equal",
  "bls12_381_G2_compress",
  "bls12_381_G2_uncompress",
  "bls12_381_G2_hashToGroup",
  "bls12_381_millerLoop",
  "bls12_381_mulMlResult",
  "bls12_381_finalVerify",
  "keccak_256",
  "blake2b_224",
  "integerToByteString",
  "byteStringToInteger",
  "andByteString",
  "orByteString",
  "xorByteString",
  "complementByteString",
  "readBit",
  "writeBits",
  "replicateByte",
  "shiftByteString",
  "rotateByteString",
  "countSetBits",
  "findFirstSetBit",
  "ripemd_160",
  "expModInteger",
  "dropList",
  "lengthOfArray",
  "listToArray",
  "indexArray",
  "bls12_381_G1_multiScalarMul",
  "bls12_381_G2_multiScalarMul",
  "insertCoin",
  "lookupCoin",
  "unionValue",
  "valueContains",
  "valueData",
  "unValueData",
  "scaleValue",
] as const);

/** A builtin's textual name. */
export type MidgardCekBuiltinName = (typeof MIDGARD_CEK_BUILTIN_NAMES)[number];

/** The textual name of Cardano builtin `tag`; throws on an unknown tag. */
export const midgardCekBuiltinName = (tag: number): MidgardCekBuiltinName => {
  const name = MIDGARD_CEK_BUILTIN_NAMES[tag];
  if (name === undefined) throw new Error(`unknown builtin tag ${tag}`);
  return name;
};

/** The Cardano tag of the builtin named `name`; throws on an unknown name. */
export const midgardCekBuiltinTagOfName = (
  name: MidgardCekBuiltinName,
): number => {
  const tag = MIDGARD_CEK_BUILTIN_NAMES.indexOf(name);
  if (tag < 0) throw new Error(`unknown builtin ${String(name)}`);
  return tag;
};
