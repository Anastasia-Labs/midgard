/**
 * The **structured inputs** behind the `n01`–`n09` native-transaction vectors —
 * one source of truth, driven from two sides.
 *
 * `scripts/generate-native-tx-vector-v1-goldens.mjs` drives these through the
 * built `dist/` twin to emit the JSON fixture and to rebind the constants in
 * `onchain/aiken/lib/midgard/fraud-proofs/native-tx-v1.test.ak`; the vitest suite
 * (`tests/native-codec.test.ts`) drives the same definitions through `src/` and
 * checks the result against the checked-in fixture. Because both start here
 * rather than from the fixture's own bytes, a codec that drifts is caught on the
 * TypeScript side too — not only by `--check` and the Aiken producers.
 *
 * The transaction is deliberately the smallest thing that still exercises every
 * schema: all nine fields empty, so the `n0x` bytes are dominated by the *shape*
 * rather than by content, with a non-zero fee, an upper validity bound and no
 * lower one, distinct non-empty script-integrity and auxiliary-data hashes, an
 * explicit network id, and the `FailedScript` validity code so the compact form's
 * validity byte is not the zero default.
 *
 * This module imports nothing from the package: the two constants it needs
 * (`EMPTY_CBOR_LIST`, `MIDGARD_POSIX_TIME_NONE` and friends) arrive as the `codec`
 * argument, so it can be loaded by either side without deciding which build of
 * the codec is under test.
 */

/** The nine-field golden: everything empty, every scalar distinct. */
export const nativeTxVectorCanonicalV1 = (codec) => ({
  version: codec.MIDGARD_NATIVE_TX_V1_VERSION,
  validity: "FailedScript",
  body: {
    spendInputsPreimageCbor: codec.EMPTY_CBOR_LIST,
    referenceInputsPreimageCbor: codec.EMPTY_CBOR_LIST,
    outputsPreimageCbor: codec.EMPTY_CBOR_LIST,
    fee: 23n,
    validityIntervalStart: codec.MIDGARD_POSIX_TIME_NONE,
    validityIntervalEnd: 24n,
    requiredObserversPreimageCbor: codec.EMPTY_CBOR_LIST,
    requiredSignersPreimageCbor: codec.EMPTY_CBOR_LIST,
    mintPreimageCbor: codec.EMPTY_CBOR_LIST,
    scriptIntegrityHash: Buffer.alloc(32, 0x11),
    auxiliaryDataHash: Buffer.alloc(32, 0x22),
    networkId: 1n,
  },
  witnessSet: {
    addrTxWitsPreimageCbor: codec.EMPTY_CBOR_LIST,
    scriptTxWitsPreimageCbor: codec.EMPTY_CBOR_LIST,
    redeemerTxWitsPreimageCbor: codec.EMPTY_CBOR_LIST,
  },
});

/**
 * The same transaction with nine **distinct, ascending** field lengths, which is
 * the only way to prove the `n08` length tuple is positional rather than
 * accidentally symmetric. Field 5 is a one-policy mint map because a mint field
 * cannot be given an arbitrary length by padding.
 */
export const nativeTxVectorOrderedLengthCanonicalV1 = (codec) => {
  const base = nativeTxVectorCanonicalV1(codec);
  return {
    ...base,
    body: {
      ...base.body,
      spendInputsPreimageCbor: codec.encodeCbor([]),
      referenceInputsPreimageCbor: codec.encodeCbor([Buffer.alloc(0)]),
      outputsPreimageCbor: codec.encodeCbor([Buffer.alloc(1)]),
      requiredObserversPreimageCbor: codec.encodeCbor([Buffer.alloc(2)]),
      requiredSignersPreimageCbor: codec.encodeCbor([Buffer.alloc(3)]),
      mintPreimageCbor: codec.encodeCbor(
        new Map([[Buffer.alloc(28), new Map([[Buffer.alloc(0), 1n]])]]),
      ),
    },
    witnessSet: {
      addrTxWitsPreimageCbor: codec.encodeCbor([Buffer.alloc(4)]),
      scriptTxWitsPreimageCbor: codec.encodeCbor([0n, 1n, 2n, 3n, 4n, 5n]),
      redeemerTxWitsPreimageCbor: codec.encodeCbor([
        0n,
        1n,
        2n,
        3n,
        4n,
        5n,
        6n,
      ]),
    },
  };
};

/** The nine lengths the positional length-tuple encoder is pinned against. */
export const NATIVE_TX_VECTOR_ORDERED_LENGTH_TUPLE_V1 = [
  1, 2, 3, 4, 5, 6, 7, 8, 9,
];

/**
 * Everything the two vectors imply, computed through whichever build of the
 * codec is passed in.
 *
 * The keys are the vector's public names: the JSON fixture is exactly this
 * object, the vitest suite compares against it, and the generator binds a subset
 * of it into the Aiken module's `n0x` constants.
 */
export const deriveNativeTxVectorV1 = (codec) => {
  const tx = codec.materializeMidgardNativeTxFromCanonicalV1(
    nativeTxVectorCanonicalV1(codec),
  );
  const source = codec.deriveMidgardNativeTxProofSourceV1(tx);
  const orderedLengthTx = codec.materializeMidgardNativeTxFromCanonicalV1(
    nativeTxVectorOrderedLengthCanonicalV1(codec),
  );
  const hex = (bytes) => Buffer.from(bytes).toString("hex");
  return {
    bodyCanonical: hex(codec.encodeMidgardNativeTxBodyCanonicalV1(tx.body)),
    bodyCompact: hex(
      codec.encodeMidgardNativeTxBodyCompactV1(tx.compact.transactionBody),
    ),
    witnessPreimages: hex(
      codec.encodeMidgardNativeTxWitnessPreimagesV1(tx.witnessSet),
    ),
    witnessCompact: hex(
      codec.encodeMidgardNativeTxWitnessSetCompactV1(
        codec.deriveMidgardNativeTxWitnessSetCompactV1(tx.witnessSet),
      ),
    ),
    compact: hex(codec.encodeMidgardNativeTxCompactV1(tx.compact)),
    canonical: hex(codec.encodeMidgardNativeTxCanonicalV1(tx)),
    transactionId: hex(codec.computeMidgardNativeTxIdV1(tx)),
    fullHash: hex(codec.computeMidgardNativeTxFullHashV1(tx)),
    proofCompact: hex(source.compactCbor),
    proofWitnessCompact: hex(source.witnessSetCompactCbor),
    proofLengths: hex(source.fieldPreimageLengthsCbor),
    proofSource: hex(codec.encodeMidgardNativeTxProofSourceV1(source)),
    proofCommitment: hex(
      codec.computeMidgardNativeTxProofCommitmentV1(source),
    ),
    canonicalSize:
      codec.computeMidgardNativeTxCanonicalSizeFromProofSourceV1(source),
    orderedLengthTuple: hex(
      codec.encodeMidgardNativeTxProofFieldLengthsV1(
        NATIVE_TX_VECTOR_ORDERED_LENGTH_TUPLE_V1,
      ),
    ),
    derivedOrderedLengthTuple: hex(
      codec.deriveMidgardNativeTxProofSourceV1(orderedLengthTx)
        .fieldPreimageLengthsCbor,
    ),
  };
};
