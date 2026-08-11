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
 * The same transaction with nine **distinct** field lengths, which is what proves
 * the `n08` length tuple is positional rather than accidentally symmetric.
 *
 * The lengths are deliberately *not* sorted. Under §5.1 each field's minimum
 * width is fixed by its own item encoding — fields 0/1 are stride 40, fields 3/4
 * stride 30, field 7 stride 103, and §5.6's smallest mint policy item is 34 bytes
 * — so an ascending run in wire order is not constructible without padding every
 * field into a size that says nothing. An unsorted tuple is also the stronger
 * test: a sorted one survives a stable-sort bug in the encoder, an unsorted one
 * does not.
 *
 * Every field is built through its §5.3/§5.6 item encoder rather than by padding
 * bytes, so the resulting lengths are the ones the grammar actually produces.
 */
export const nativeTxVectorOrderedLengthCanonicalV1 = (codec) => {
  const base = nativeTxVectorCanonicalV1(codec);
  const input = (index) => ({
    txId: Buffer.alloc(32, 0xab),
    outputIndex: index,
  });
  return {
    ...base,
    body: {
      ...base.body,
      // 0 items — `80`.
      spendInputsPreimageCbor: codec.encodeMidgardFieldPreimageForFieldV1({
        fieldIndex: 0,
        items: [],
      }),
      // 1 item — stride 40.
      referenceInputsPreimageCbor: codec.encodeMidgardFieldPreimageForFieldV1({
        fieldIndex: 1,
        items: [input(7)],
      }),
      // 1 output with an address and a bare-lovelace value.
      outputsPreimageCbor: codec.encodeMidgardFieldPreimageForFieldV1({
        fieldIndex: 2,
        items: [
          {
            // A Midgard address payload: one header byte then a 28-byte payment
            // hash, no stake part (§5.5).
            address: Buffer.concat([
              Buffer.from([0x60]),
              Buffer.alloc(28, 0xcd),
            ]),
            value: { lovelace: 0n, assets: new Map() },
          },
        ],
      }),
      // 1 × 28-byte hash — stride 30.
      requiredObserversPreimageCbor: codec.encodeMidgardFieldPreimageForFieldV1(
        {
          fieldIndex: 3,
          items: [Buffer.alloc(28, 0x02)],
        },
      ),
      // 2 × 28-byte hashes, ascending so §5.3's order rule holds.
      requiredSignersPreimageCbor: codec.encodeMidgardFieldPreimageForFieldV1({
        fieldIndex: 4,
        items: [Buffer.alloc(28, 0x03), Buffer.alloc(28, 0x04)],
      }),
      // §5.6's smallest policy item: one policy, one empty-named asset.
      mintPreimageCbor: codec.encodeMidgardFieldPreimageForFieldV1({
        fieldIndex: 5,
        items: [
          {
            policyId: Buffer.alloc(28, 0x05),
            assets: [{ assetName: Buffer.alloc(0), quantity: 1n }],
          },
        ],
      }),
    },
    witnessSet: {
      // 1 witness — stride 103.
      addrTxWitsPreimageCbor: codec.encodeMidgardFieldPreimageForFieldV1({
        fieldIndex: 7,
        items: [
          {
            verificationKey: Buffer.alloc(32, 0x06),
            signature: Buffer.alloc(64, 0x07),
          },
        ],
      }),
      // The narrowest §5.3 field-6 item: `82 03 40`. `PlutusV3` rather than
      // `NativeCardano` because §5.3 makes the native language carry a script
      // *structure*, not a raw payload, so it has no empty form.
      scriptTxWitsPreimageCbor: codec.encodeMidgardFieldPreimageForFieldV1({
        fieldIndex: 6,
        items: [{ language: "PlutusV3", scriptBytes: Buffer.alloc(0) }],
      }),
      // The narrowest §5.3 field-8 item: `84 00 00 40 82 00 00`.
      redeemerTxWitsPreimageCbor: codec.encodeMidgardFieldPreimageForFieldV1({
        fieldIndex: 8,
        items: [
          {
            purpose: "Spend",
            index: 0n,
            redeemerCbor: Buffer.alloc(0),
            executionUnits: { memory: 0n, steps: 0n },
          },
        ],
      }),
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
    proofCommitment: hex(codec.computeMidgardNativeTxProofCommitmentV1(source)),
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
