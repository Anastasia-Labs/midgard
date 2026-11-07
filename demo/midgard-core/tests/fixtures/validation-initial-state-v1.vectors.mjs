/**
 * The **structured inputs** behind the `validation-initial-state-v1` golden —
 * one source of truth, driven from two sides.
 *
 * State 0 of a transaction's validation trace is not supplied by anyone: the
 * on-chain claim derives it (`validation_claim_v1.derive_initial_state`) from
 * the block header's context, the event key, the authenticated source triple
 * and the transition step's pre-state root, and the node derives the same state
 * through `deriveMidgardValidationInitialStateV1`. The two must agree byte for
 * byte, or an honest trace would not start where the claim says it starts.
 *
 * `scripts/generate-validation-initial-state-v1-goldens.mjs` drives these
 * through the built `dist/` twin to emit the JSON fixture and the Aiken module
 * `onchain/aiken/lib/midgard/validation-initial-state-v1-golden.test.ak`; the
 * vitest suite (`tests/validation-initial-state-v1-goldens.test.ts`) drives the
 * same definitions through `src/` and checks the result against the checked-in
 * fixture.
 *
 * Four transactions × two headers:
 *
 *   * `minimal_spend` — one key-locked input, one output, one signature;
 *   * `multi_redeemer` — two script inputs and a mint, each with its redeemer,
 *     and the Plutus scripts they run. It is **provisional**: it carries script
 *     bytes, so its compact form, transaction id and commitment move with any
 *     change to how a script's identity is committed;
 *   * `max_redeemers_field` — a field-8 (redeemer witnesses) preimage of exactly
 *     the consensus profile's `maxRedeemersPreimageBytes`, and no scripts;
 *   * `forced_spend` — a forced submission (no validity tag) with one key-locked
 *     input, one output and one signature, under a `ForcedTransactionEventKey`.
 *     Its source is the forced variant, so its start state is `Forced`.
 *
 * The first three are normal sources under an `L2TransactionEventKey`; their
 * start state is `Normal`. The source kind is never an input of the twin: it
 * follows from which source variant is passed, as in the claim.
 *
 * The two headers differ in every context entry, and within each header the
 * context entries are pairwise distinct, so a context element read from the
 * wrong header field or written to the wrong position changes the hash.
 *
 * This module imports nothing from the package: the codec arrives as the
 * `codec` argument, so it can be loaded by either side without deciding which
 * build is under test.
 */

import {
  addressWitness,
  filler,
  input,
  keyAddress,
  mintPolicy,
  asset,
  plutusV3,
  redeemer,
  value,
} from "./native-tx-field-items-v1.vectors.mjs";

export const VALIDATION_INITIAL_STATE_HEADERS_V1 = [
  {
    label: "a",
    endTimeMs: 1_700_000_020_000n,
    blockSlot: 86_400n,
    expectedNetworkId: 1n,
    minFeeA: 44n,
    minFeeB: 155_381n,
  },
  {
    label: "b",
    endTimeMs: 1_767_225_600_000n,
    blockSlot: 4_294_967_296n,
    expectedNetworkId: 0n,
    minFeeA: 77n,
    minFeeB: 1_000_000n,
  },
];

const PLUTUS_CHUNK_BYTES = 64;

/**
 * Canonical Plutus Data of exactly `length` bytes: an indefinite list of byte
 * strings, each at most 64 bytes, as Aiken serialises one.
 */
const plutusBytesListOfLength = (length) => {
  const chunks = [];
  let remaining = length - 2;
  const byteString = (size, seed) =>
    Buffer.concat([
      size < 24 ? Buffer.from([0x40 + size]) : Buffer.from([0x58, size]),
      filler(size, seed),
    ]);
  while (remaining > 0) {
    // A chunk of payload m occupies 1 + m bytes below 24 and 2 + m from 24 up,
    // so every remaining size but 25 is one chunk; 25 is 1 + 24.
    const take =
      remaining > PLUTUS_CHUNK_BYTES + 2
        ? PLUTUS_CHUNK_BYTES + 2
        : remaining === 25
          ? 1
          : remaining;
    const payload = take <= 24 ? take - 1 : take - 2;
    chunks.push(byteString(payload, chunks.length));
    remaining -= take;
  }
  const encoded = Buffer.concat([
    Buffer.from([0x9f]),
    ...chunks,
    Buffer.from([0xff]),
  ]);
  if (encoded.length !== length) {
    throw new Error(
      `plutus byte list of ${length} bytes came out at ${encoded.length}`,
    );
  }
  return encoded;
};

const field = (codec, fieldIndex, items) =>
  codec.encodeMidgardFieldPreimageForField({ fieldIndex, items });

const canonicalOf = (codec, overrides) => ({
  version: codec.MIDGARD_NATIVE_TX_VERSION,
  validity: "TxIsValid",
  body: {
    spendInputsPreimageCbor: codec.EMPTY_CBOR_LIST,
    referenceInputsPreimageCbor: codec.EMPTY_CBOR_LIST,
    outputsPreimageCbor: codec.EMPTY_CBOR_LIST,
    fee: 0n,
    validityIntervalStart: codec.MIDGARD_POSIX_TIME_NONE,
    validityIntervalEnd: codec.MIDGARD_POSIX_TIME_NONE,
    requiredObserversPreimageCbor: codec.EMPTY_CBOR_LIST,
    requiredSignersPreimageCbor: codec.EMPTY_CBOR_LIST,
    mintPreimageCbor: codec.EMPTY_CBOR_LIST,
    scriptIntegrityHash: codec.EMPTY_NULL_ROOT,
    auxiliaryDataHash: codec.EMPTY_NULL_ROOT,
    networkId: 0n,
    ...overrides.body,
  },
  witnessSet: {
    addrTxWitsPreimageCbor: codec.EMPTY_CBOR_LIST,
    scriptTxWitsPreimageCbor: codec.EMPTY_CBOR_LIST,
    redeemerTxWitsPreimageCbor: codec.EMPTY_CBOR_LIST,
    ...overrides.witnessSet,
  },
});

const minimalSpend = (codec) =>
  canonicalOf(codec, {
    body: {
      spendInputsPreimageCbor: field(codec, 0, [input(1, 0)]),
      outputsPreimageCbor: field(codec, 2, [
        { address: keyAddress(2), value: value(2_000_000n) },
      ]),
      fee: 170_000n,
    },
    witnessSet: {
      addrTxWitsPreimageCbor: field(codec, 7, [addressWitness(3)]),
    },
  });

const multiRedeemer = (codec) =>
  canonicalOf(codec, {
    body: {
      spendInputsPreimageCbor: field(codec, 0, [input(4, 0), input(5, 1)]),
      outputsPreimageCbor: field(codec, 2, [
        { address: keyAddress(6), value: value(5_000_000n) },
      ]),
      fee: 410_000n,
      validityIntervalEnd: 1_700_000_030_000n,
      mintPreimageCbor: field(codec, 5, [mintPolicy(7, [asset("4d4944", 5)])]),
      scriptIntegrityHash: filler(32, 8),
      networkId: 1n,
    },
    witnessSet: {
      scriptTxWitsPreimageCbor: field(codec, 6, [
        plutusV3(9, 120),
        plutusV3(10, 80),
      ]),
      addrTxWitsPreimageCbor: field(codec, 7, [addressWitness(11)]),
      redeemerTxWitsPreimageCbor: field(codec, 8, [
        redeemer("Spend", 0, "d87980", 1_000_000, 400_000_000),
        redeemer("Spend", 1, "d87a9f182aff", 1_200_000, 450_000_000),
        redeemer("Mint", 0, "43010203", 800_000, 300_000_000),
      ]),
    },
  });

/**
 * A single spend redeemer whose data fills field 8 to exactly the profile's
 * `maxRedeemersPreimageBytes`.
 */
const maxRedeemersField = (codec) => {
  const target = codec.MIDGARD_CONSENSUS_LIMITS.maxRedeemersPreimageBytes;
  const preimageWithData = (dataLength) =>
    field(codec, 8, [
      redeemer(
        "Spend",
        0,
        plutusBytesListOfLength(dataLength).toString("hex"),
        14_000_000,
        10_000_000_000,
      ),
    ]);
  let dataLength = target;
  for (let attempt = 0; attempt < 8; attempt += 1) {
    const length = preimageWithData(dataLength).length;
    if (length === target) {
      return canonicalOf(codec, {
        body: {
          spendInputsPreimageCbor: field(codec, 0, [input(12, 0)]),
          outputsPreimageCbor: field(codec, 2, [
            { address: keyAddress(13), value: value(3_000_000n) },
          ]),
          fee: 1_500_000n,
        },
        witnessSet: {
          addrTxWitsPreimageCbor: field(codec, 7, [addressWitness(14)]),
          redeemerTxWitsPreimageCbor: preimageWithData(dataLength),
        },
      });
    }
    dataLength += target - length;
  }
  throw new Error(`no redeemer data fills field 8 to ${target} bytes`);
};

/** A forced submission: user-authored material, so it carries no validity tag. */
const forcedSpend = (codec) => {
  const { validity: _validity, ...forced } = canonicalOf(codec, {
    body: {
      spendInputsPreimageCbor: field(codec, 0, [input(15, 2)]),
      outputsPreimageCbor: field(codec, 2, [
        { address: keyAddress(16), value: value(4_000_000n) },
      ]),
      fee: 190_000n,
    },
    witnessSet: {
      addrTxWitsPreimageCbor: field(codec, 7, [addressWitness(17)]),
    },
  });
  return forced;
};

export const VALIDATION_INITIAL_STATE_TRANSACTIONS_V1 = [
  {
    label: "minimal_spend",
    provisional: false,
    source: "normal",
    canonical: minimalSpend,
  },
  {
    label: "multi_redeemer",
    provisional: true,
    source: "normal",
    canonical: multiRedeemer,
  },
  {
    label: "max_redeemers_field",
    provisional: false,
    source: "normal",
    canonical: maxRedeemersField,
  },
  {
    label: "forced_spend",
    provisional: false,
    source: "forced",
    canonical: forcedSpend,
    txOrderId: { transactionIdSeed: 18, outputIndex: 3 },
  },
];

const hex = (bytes) => Buffer.from(bytes).toString("hex");

/**
 * `ledger_state.L2TransactionEventKey { tx_id }` as Aiken serialises it:
 * constructor 2 (tag 123) over an indefinite list holding the 32-byte id.
 */
export const l2TransactionEventKeyCbor = (transactionId) =>
  Buffer.concat([
    Buffer.from("d87b9f5820", "hex"),
    Buffer.from(transactionId),
    Buffer.from([0xff]),
  ]);

/**
 * `ledger_state.ForcedTransactionEventKey { tx_order_id }` as Aiken serialises
 * it: constructor 1 (tag 122) over an indefinite list holding the
 * `OutputReference` (constructor 0, tag 121) of the 32-byte transaction id and
 * the output index, which must be below 24 to stay a one-byte integer here.
 */
export const forcedTransactionEventKeyCbor = ({
  transactionId,
  outputIndex,
}) => {
  if (!Number.isInteger(outputIndex) || outputIndex < 0 || outputIndex > 23) {
    throw new Error(`output index ${outputIndex} is not a one-byte integer`);
  }
  return Buffer.concat([
    Buffer.from("d87a9fd8799f5820", "hex"),
    Buffer.from(transactionId),
    Buffer.from([outputIndex, 0xff, 0xff]),
  ]);
};

/** The source variant the twin takes, carrying the canonical bytes. */
export const validationSourceOf = (kind, canonicalTransactionCbor) =>
  kind === "forced"
    ? { forced: { canonicalTransactionCbor } }
    : { normal: { canonicalTransactionCbor } };

const eventKeyOf = (transaction) =>
  transaction.source === "forced"
    ? forcedTransactionEventKeyCbor({
        transactionId: Buffer.from(transaction.txOrderId.transactionId, "hex"),
        outputIndex: transaction.txOrderId.outputIndex,
      })
    : l2TransactionEventKeyCbor(Buffer.from(transaction.transactionId, "hex"));

/**
 * Everything the golden pins, computed through whichever build of the codec is
 * passed in. The JSON fixture is exactly this object.
 */
export const deriveValidationInitialStateVectorsV1 = (codec) => {
  const transactions = VALIDATION_INITIAL_STATE_TRANSACTIONS_V1.map(
    ({ label, provisional, source: kind, canonical, txOrderId }) => {
      const forced = kind === "forced";
      const tx = forced
        ? codec.materializeMidgardForcedTxFromCanonical(canonical(codec))
        : codec.materializeMidgardNativeTxFromCanonical(canonical(codec));
      const source = forced
        ? codec.deriveMidgardForcedTxProofSource(tx)
        : codec.deriveMidgardNativeTxProofSource(tx);
      return {
        label,
        provisional,
        source: kind,
        ...(txOrderId === undefined
          ? {}
          : {
              txOrderId: {
                transactionId: hex(filler(32, txOrderId.transactionIdSeed)),
                outputIndex: txOrderId.outputIndex,
              },
            }),
        canonicalTransactionCbor: hex(
          forced
            ? codec.encodeMidgardForcedTxCanonical(tx)
            : codec.encodeMidgardNativeTxCanonical(tx),
        ),
        transactionId: hex(codec.computeMidgardNativeTxId(tx.compact)),
        redeemersPreimageBytes: tx.witnessSet.redeemerTxWitsPreimageCbor.length,
        compactCbor: hex(source.compactCbor),
        witnessSetCompactCbor: hex(source.witnessSetCompactCbor),
        fieldPreimageLengthsCbor: hex(source.fieldPreimageLengthsCbor),
      };
    },
  );
  const headers = VALIDATION_INITIAL_STATE_HEADERS_V1.map(
    ({ label, ...header }) => ({
      label,
      endTimeMs: header.endTimeMs.toString(),
      blockSlot: header.blockSlot.toString(),
      expectedNetworkId: header.expectedNetworkId.toString(),
      minFeeA: header.minFeeA.toString(),
      minFeeB: header.minFeeB.toString(),
      contextCbor: hex(codec.deriveMidgardValidationContextCborV1(header)),
    }),
  );
  const vectors = transactions.flatMap((transaction, txIndex) =>
    VALIDATION_INITIAL_STATE_HEADERS_V1.map(
      ({ label, ...header }, headerIndex) => {
        const eventKeyCbor = eventKeyOf(transaction);
        const preUtxosRoot = filler(32, 40 + txIndex * 2 + headerIndex);
        const state = codec.deriveMidgardValidationInitialStateV1({
          ...header,
          eventKeyCbor,
          source: validationSourceOf(
            transaction.source,
            Buffer.from(transaction.canonicalTransactionCbor, "hex"),
          ),
          preUtxosRoot,
        });
        return {
          label: `${transaction.label}_${label}`,
          provisional: transaction.provisional,
          transaction: transaction.label,
          source: transaction.source,
          header: label,
          eventKeyCbor: hex(eventKeyCbor),
          preUtxosRoot: hex(preUtxosRoot),
          stateCbor: hex(codec.encodeMidgardValidationMachineState(state)),
          stateHash: hex(codec.hashMidgardValidationMachineState(state)),
        };
      },
    ),
  );
  return {
    schema: "midgard-validation-initial-state-v1-golden",
    version: 1,
    generator:
      "demo/midgard-core/scripts/generate-validation-initial-state-v1-goldens.mjs",
    aikenModule:
      "onchain/aiken/lib/midgard/validation-initial-state-v1-golden.test.ak",
    profileId: codec.MIDGARD_CONSENSUS_PROFILE_ID,
    headers,
    transactions,
    vectors,
  };
};
