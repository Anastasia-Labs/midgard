import { Store, Trie } from "@aiken-lang/merkle-patricia-forestry";
import {
  computeMidgardNativeTxId,
  EMPTY_CBOR_LIST,
  EMPTY_NULL_ROOT,
  encodeCbor,
  encodeMidgardNativeTxCompact,
  materializeMidgardNativeTxFromCanonical,
  MIDGARD_NATIVE_TX_VERSION,
  MIDGARD_POSIX_TIME_NONE,
  midgardFieldCarriageBounds,
  type MidgardNativeTxFull,
} from "@al-ft/midgard-core";
import {
  encodeMidgardTxInputCanonical,
  type MidgardTxInput,
  type MidgardTxOutput,
  Proof,
} from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";

import { midgardTxOutputFromCanonicalCbor } from "../../src/prepare-input-no-idx.js";
import {
  nativeTxFromCoreCompact,
  type SubmitStep01TxInclusion,
} from "../../src/step-support.js";
import {
  l2TransactionSourceCbor as l2TransactionSourceCborV1,
  makeFaultProofEmulatorHarness,
  trieRootHex,
} from "./submit-init-emulator-shared.js";

// ---------------------------------------------------------------------------
// §8.4 tier selection arithmetic — the counts below are DERIVED from the
// published bound, never asserted into being by a force flag.
// ---------------------------------------------------------------------------

/**
 * §5.1 wraps each item in a definite-bytes head. A §5.3 out-ref item is a fixed
 * 38 bytes (`82 ‖ 58 20 tx_id ‖ 19 index_be16`), so its wrapped stride is
 * `2 + 38 = 40`.
 */
export const REFERENCE_INPUT_ITEM_STRIDE_BYTES = 40;

/**
 * One canonical lovelace-only enterprise output (see `nativeOutputCbor`) is
 * 41 bytes, so its wrapped §5.1 stride is `2 + 41 = 43`.
 */
export const PRODUCING_OUTPUT_ITEM_STRIDE_BYTES = 43;

/** `definite_array_header(N)` for the 256..65,535 range these counts land in. */
const FIELD_PREIMAGE_ARRAY_HEADER_BYTES = 3;

const firstCountAboveTier1 = (strideBytes: number): number =>
  Math.floor(
    (midgardFieldCarriageBounds.maxTier1RedeemerPreimageBytes -
      FIELD_PREIMAGE_ARRAY_HEADER_BYTES) /
      strideBytes,
  ) + 1;

/**
 * The first field-1 reference-input count whose §5.1 preimage exceeds §8.4's
 * tier-1 redeemer bound: `3 + 40n > 14,336` first holds at `n = 359`
 * (14,363 bytes), which is inside the single-publication tier-2 window.
 */
export const REFERENCE_INPUT_NO_IDX_TIER2_REFERENCE_INPUT_COUNT =
  firstCountAboveTier1(REFERENCE_INPUT_ITEM_STRIDE_BYTES);

/**
 * The first field-2 output count whose §5.1 preimage exceeds the same bound:
 * `3 + 43n > 14,336` first holds at `n = 334` (14,365 bytes).
 */
export const REFERENCE_INPUT_NO_IDX_TIER2_PRODUCING_OUTPUT_COUNT =
  firstCountAboveTier1(PRODUCING_OUTPUT_ITEM_STRIDE_BYTES);

// ---------------------------------------------------------------------------
// The committed two-transaction block
// ---------------------------------------------------------------------------

/** One canonical native output: enterprise pubkey address, lovelace only. */
export const nativeOutputCbor = (
  paymentByte: number,
  lovelace: bigint,
): Buffer =>
  Buffer.concat([
    Buffer.from([0xa2, 0x00, 0x58, 0x1d, 0x60]),
    Buffer.alloc(28, paymentByte),
    Buffer.from([0x01, 0x82]),
    encodeCbor(lovelace),
    Buffer.from([0xa0]),
  ]);

const outRefItem = (outRef: MidgardTxInput): Buffer =>
  Buffer.from(encodeMidgardTxInputCanonical(outRef));

/**
 * Materializes a native-V1 transaction with caller-chosen §2.5 fields 0, 1 and
 * 2. `makeNativeTx` cannot express a canonical reference-input list, which is
 * the whole subject of this family.
 */
const makeReferenceInputNoIdxNativeTx = ({
  spendInputs,
  referenceInputs,
  outputCbors,
  fee,
}: {
  readonly spendInputs: readonly MidgardTxInput[];
  readonly referenceInputs: readonly MidgardTxInput[];
  readonly outputCbors: readonly Buffer[];
  readonly fee: bigint;
}): MidgardNativeTxFull =>
  materializeMidgardNativeTxFromCanonical({
    version: MIDGARD_NATIVE_TX_VERSION,
    validity: "TxIsValid",
    body: {
      spendInputsPreimageCbor:
        spendInputs.length === 0
          ? EMPTY_CBOR_LIST
          : encodeCbor(spendInputs.map(outRefItem)),
      referenceInputsPreimageCbor:
        referenceInputs.length === 0
          ? EMPTY_CBOR_LIST
          : encodeCbor(referenceInputs.map(outRefItem)),
      outputsPreimageCbor:
        outputCbors.length === 0
          ? EMPTY_CBOR_LIST
          : encodeCbor([...outputCbors]),
      requiredObserversPreimageCbor: EMPTY_CBOR_LIST,
      requiredSignersPreimageCbor: EMPTY_CBOR_LIST,
      mintPreimageCbor: EMPTY_CBOR_LIST,
      scriptIntegrityHash: EMPTY_NULL_ROOT,
      auxiliaryDataHash: EMPTY_NULL_ROOT,
      fee,
      validityIntervalStart: MIDGARD_POSIX_TIME_NONE,
      validityIntervalEnd: MIDGARD_POSIX_TIME_NONE,
      networkId: 0n,
    },
    witnessSet: {
      addrTxWitsPreimageCbor: EMPTY_CBOR_LIST,
      scriptTxWitsPreimageCbor: EMPTY_CBOR_LIST,
      redeemerTxWitsPreimageCbor: EMPTY_CBOR_LIST,
    },
  });

export type ReferenceInputNoIdxBlockFixture = {
  readonly transactionsRoot: string;
  readonly l2TransactionCount: bigint;
  readonly producingTxId: string;
  readonly producingOutputsCbor: readonly string[];
  readonly producingOutputs: readonly MidgardTxOutput[];
  readonly producingTxOutputsHash: string;
  readonly badTxId: string;
  readonly badTxReferenceInputsHash: string;
  /** The whole committed field-1 list, in committed order. */
  readonly referenceInputs: readonly MidgardTxInput[];
  /** The reference input the family challenges. */
  readonly challengedReferenceInput: MidgardTxInput;
  /** Its position in the committed field-1 list. */
  readonly challengedReferenceInputIndex: number;
  readonly badTxInclusion: SubmitStep01TxInclusion;
  readonly producingTxInclusion: SubmitStep01TxInclusion;
};

/**
 * Commits a producing transaction with `producingOutputCount` canonical
 * outputs and a reader whose field-1 list names
 * `(producing_tx_id, challengedOutputIndex)` as the two native-compact leaves
 * of one block's transactions MPF.
 *
 * The block carries the `reference-input-no-idx` violation exactly when
 * `challengedOutputIndex >= producingOutputCount`; the caller chooses, which is
 * what makes the honest-commitment polarity the same fixture with one number
 * moved.
 *
 * `referenceInputCount` pads field 1 with further honest out-refs so its §5.1
 * preimage can cross §8.4's tier-1 bound on its own size. The padding never
 * displaces the challenged item: the list is sorted canonically and the
 * challenged position is looked up afterwards.
 */
export const buildReferenceInputNoIdxBlockFixture = async ({
  producingOutputCount,
  challengedOutputIndex = 7n,
  referenceInputCount = 1,
}: {
  readonly producingOutputCount: number;
  readonly challengedOutputIndex?: bigint;
  readonly referenceInputCount?: number;
}): Promise<ReferenceInputNoIdxBlockFixture> => {
  if (!Number.isSafeInteger(referenceInputCount) || referenceInputCount <= 0) {
    throw new Error("referenceInputCount must be a positive safe integer");
  }
  const producingOutputsCbor = Array.from(
    { length: producingOutputCount },
    (_unused, index) =>
      nativeOutputCbor((0x40 + index) % 0x100, 5_000_000n + BigInt(index)),
  );
  const producingTx = makeReferenceInputNoIdxNativeTx({
    spendInputs: [{ tx_id: "99".repeat(32), output_index: 0n }],
    referenceInputs: [],
    outputCbors: producingOutputsCbor,
    fee: 7n,
  });
  const producingTxId = computeMidgardNativeTxId(producingTx).toString("hex");

  const challengedReferenceInput: MidgardTxInput = {
    tx_id: producingTxId,
    output_index: challengedOutputIndex,
  };
  const referenceInputs = [
    challengedReferenceInput,
    ...Array.from({ length: referenceInputCount - 1 }, (_unused, index) => ({
      tx_id: (index + 1).toString(16).padStart(64, "0"),
      output_index: 0n,
    })),
  ].sort((left, right) => Buffer.compare(outRefItem(left), outRefItem(right)));
  const challengedReferenceInputIndex = referenceInputs.findIndex(
    (input) =>
      input.tx_id === challengedReferenceInput.tx_id &&
      input.output_index === challengedReferenceInput.output_index,
  );
  if (challengedReferenceInputIndex < 0) {
    throw new Error(
      "Expected the challenged reference input in the canonical field-1 list",
    );
  }

  const badTx = makeReferenceInputNoIdxNativeTx({
    spendInputs: [{ tx_id: "33".repeat(32), output_index: 0n }],
    referenceInputs,
    outputCbors: [nativeOutputCbor(0x11, 4_000_000n)],
    fee: 11n,
  });
  const badTxId = computeMidgardNativeTxId(badTx).toString("hex");
  if (badTxId === producingTxId) {
    throw new Error("fixture producer collides with the disputed transaction");
  }

  const producingCompactCbor = encodeMidgardNativeTxCompact(
    producingTx.compact,
  );
  const badCompactCbor = encodeMidgardNativeTxCompact(badTx.compact);
  const producingSourceCbor = l2TransactionSourceCborV1(producingTx);
  const badSourceCbor = l2TransactionSourceCborV1(badTx);

  const store = new Store(undefined);
  await store.ready();
  const trie = new Trie(store);
  await trie.insert(
    Buffer.from(producingTxId, "hex"),
    Buffer.from(producingSourceCbor, "hex"),
  );
  await trie.insert(
    Buffer.from(badTxId, "hex"),
    Buffer.from(badSourceCbor, "hex"),
  );
  const producingProof = await trie.prove(Buffer.from(producingTxId, "hex"));
  const badProof = await trie.prove(Buffer.from(badTxId, "hex"));
  const transactionsRoot = trieRootHex(trie);
  const producingCompact = nativeTxFromCoreCompact(producingTx.compact);
  const badCompact = nativeTxFromCoreCompact(badTx.compact);

  const inclusionFor = (
    nativeTxId: string,
    compactCbor: Buffer,
    l2TransactionSourceCbor: string,
    proofCbor: string,
    compact: typeof producingTx.compact,
  ): SubmitStep01TxInclusion => ({
    nativeTxId,
    nativeTx: nativeTxFromCoreCompact(compact),
    nativeTxCompactCbor: compactCbor.toString("hex"),
    l2TransactionSourceCbor,
    transactionsPhasRoot: transactionsRoot,
    txMembershipProof: Data.from(proofCbor, Proof),
    txMembershipProofCbor: proofCbor,
  });

  return {
    transactionsRoot,
    l2TransactionCount: 2n,
    producingTxId,
    producingOutputsCbor: producingOutputsCbor.map((item) =>
      item.toString("hex"),
    ),
    producingOutputs: producingOutputsCbor.map(
      midgardTxOutputFromCanonicalCbor,
    ),
    producingTxOutputsHash: producingCompact.body.outputs_hash,
    badTxId,
    badTxReferenceInputsHash: badCompact.body.reference_inputs_hash,
    referenceInputs,
    challengedReferenceInput,
    challengedReferenceInputIndex,
    badTxInclusion: inclusionFor(
      badTxId,
      badCompactCbor,
      badSourceCbor,
      badProof.toCBOR().toString("hex"),
      badTx.compact,
    ),
    producingTxInclusion: inclusionFor(
      producingTxId,
      producingCompactCbor,
      producingSourceCbor,
      producingProof.toCBOR().toString("hex"),
      producingTx.compact,
    ),
  };
};

// ---------------------------------------------------------------------------
// Harness and reference-script publication
// ---------------------------------------------------------------------------

export const makeReferenceInputNoIdxEmulatorHarness = async () =>
  await makeFaultProofEmulatorHarness({
    contractOptions: {
      realReferenceInputNoIdx: true,
      alwaysFraudProofCatalogue: true,
    },
  });

export type ReferenceInputNoIdxHarness = Awaited<
  ReturnType<typeof makeReferenceInputNoIdxEmulatorHarness>
>;
