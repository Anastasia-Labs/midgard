/**
 * Shared real-contract emulator fixtures for the `reference-input-no-idx`
 * family (Goal task `Q31`).
 *
 * The fault is the reference-input mirror of `input-no-idx`: a committed
 * transaction *reads* `(producing_tx_id, N)` where `producing_tx_id` is itself
 * committed in the same block, yet `N >= |producer.outputs|`. Proving it needs
 * a **two-transaction block**, because step-01 proves membership of the bad
 * transaction and step-03 proves membership of the *producing* transaction
 * under the same counted `transactions_root`. `buildSingleTxBlockFixture` in
 * `submit-init-emulator-registered-families.test.ts` commits one leaf, so this
 * module builds the two-leaf PHAS trie and returns an inclusion proof for each
 * transaction.
 *
 * It also materializes the disputed transaction directly from its canonical
 * §5.1 preimages: `makeNativeTx` pins §2.5 field 1 to at most one opaque
 * 32-byte item, and this family's evidence needs a caller-chosen list of
 * canonical 38-byte §5.3 out-ref items with the challenged one among them.
 *
 * The raw builders at the bottom duplicate the production submitters' exact
 * transaction shapes minus their local fail-closed guards, so the adversarial
 * polarity watches the **validator** refuse rather than the builder.
 */
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
  encodeMidgardTxOutputCanonical,
  type FieldOpening,
  FraudProofComputationThreadRedeemer,
  FraudProofTokenDatum,
  FraudProofTokenMintRedeemer,
  MIDGARD_FIELD_INDEX,
  type MidgardTxInput,
  type MidgardTxOutput,
  Proof,
  ReferenceInputNoIdxStep02Datum,
  ReferenceInputNoIdxStep02SpendRedeemer,
  ReferenceInputNoIdxStep03Datum,
  referenceInputNoIdxStep03StateFromBadInput,
  ReferenceInputNoIdxStep04Datum,
  ReferenceInputNoIdxStep04SpendRedeemer,
  requireInputIndex,
  requireMintRedeemerIndex,
  requireOwnMintPurpose,
  requireOwnSpendPurpose,
  requireUniqueOutputIndex,
} from "@al-ft/midgard-sdk";
import {
  type BuildTxWithRedeemer,
  Data,
  type LucidEvolution,
  type Script,
  toUnit,
  type UTxO,
} from "@lucid-evolution/lucid";

import {
  faultProofFieldOpening,
  planFaultProofFieldOpening,
  publishFaultProofFieldCarriage,
} from "../../src/field-opening.js";
import { midgardTxOutputFromCanonicalCbor } from "../../src/prepare-input-no-idx.js";
import {
  fetchUtxoByOutRef,
  outRefLabel,
  parseOutRef,
  requireFaultProofStepReferenceScript,
  type ResolvedProverSigner,
  resolveReferenceInputNoIdxDeploymentContracts,
} from "../../src/runtime.js";
import {
  nativeTxFromCoreCompact,
  requireComputationThreadToken,
  selectFeeInput,
  type SubmitStep01TxInclusion,
} from "../../src/submit-step-01.js";
import {
  computationThreadOutputPredicate,
  outputWithDatumAndUnitPredicate,
} from "../../src/tx-layout.js";
import {
  type FaultProofWitnessReferenceScripts,
  witnessMintingPolicyCarriage,
} from "../../src/witness-reference-scripts.js";
import {
  l2TransactionSourceCbor as l2TransactionSourceCborV1,
  makeFaultProofEmulatorHarness,
  network,
  publishPlainReferenceScriptUtxo,
  trieRootHex,
} from "./submit-init-emulator-shared.js";

export { expectOnchainRefusal } from "./emulator/expect-onchain-refusal.js";

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

/**
 * Publishes all four step validators as reference scripts, the deployment
 * shape the standing owner ruling requires: a fault proof is always read from
 * a published reference script, never inline-attached.
 */
export const publishReferenceInputNoIdxReferenceScripts = async ({
  lucid,
  contracts,
}: {
  readonly lucid: Parameters<
    typeof publishPlainReferenceScriptUtxo
  >[0]["lucid"];
  readonly contracts: ReferenceInputNoIdxHarness["contracts"]["fraudProofContracts"]["referenceInputNoIdx"];
}): Promise<readonly [UTxO, UTxO, UTxO, UTxO]> => {
  const published: UTxO[] = [];
  for (const [index, step] of contracts.steps.entries()) {
    const script: Script = step.spendingScript;
    const { utxo } = await publishPlainReferenceScriptUtxo({
      lucid,
      script,
      label: `reference-input-no-idx step-0${(index + 1).toString()}`,
    });
    published.push(utxo);
  }
  const [step01, step02, step03, step04, ...unexpected] = published;
  if (
    step01 === undefined ||
    step02 === undefined ||
    step03 === undefined ||
    step04 === undefined ||
    unexpected.length !== 0
  ) {
    throw new Error(
      `Expected exactly four reference-input-no-idx step scripts, published ${published.length.toString()}.`,
    );
  }
  return [step01, step02, step03, step04];
};

// ---------------------------------------------------------------------------
// Raw builders — the honest submitters' transactions WITHOUT their local
// fail-closed guards, so the adversarial suite watches the VALIDATOR refuse.
// Production code never takes these paths.
// ---------------------------------------------------------------------------

type RawStepConfig = {
  readonly lucid: LucidEvolution;
  readonly blueprint: unknown;
  readonly deploymentInfo: unknown;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  /**
   * The published step reference script. Required here, not optional: §8.7's
   * positional carriage indices count into the transaction's COMPLETE
   * canonically-sorted reference-input set, so a raw builder that dropped the
   * step script would resolve different indices than the production one.
   */
  readonly referenceScriptUtxo: UTxO;
};

/**
 * A raw step-02: the honest submitter's field-1 opening transaction with a
 * caller-chosen `bad_reference_input_index` and a caller-chosen forwarded
 * reference input, and none of the submitter's local range check.
 *
 * The opening itself stays honest — the door authenticates it — so the only
 * thing a refusal can be attributed to is the on-chain selection rule
 * (`spend_input_at`'s §7.3 abort-never-clamp range guard).
 */
export const submitRawReferenceInputNoIdxStep02 = async ({
  lucid,
  blueprint,
  deploymentInfo,
  signer,
  threadOutRef,
  referenceInputsPreimage,
  badReferenceInputIndex,
  forwardedReferenceInput,
  nativeTxCompactCbor,
  referenceScriptUtxo,
}: RawStepConfig & {
  readonly referenceInputsPreimage: readonly MidgardTxInput[];
  readonly badReferenceInputIndex: number;
  readonly forwardedReferenceInput: MidgardTxInput;
  readonly nativeTxCompactCbor: string;
}): Promise<string> => {
  const { referenceInputNoIdxCategory, contracts } =
    await resolveReferenceInputNoIdxDeploymentContracts({
      blueprint,
      deploymentInfo,
      network,
    });
  const chain = contracts.referenceInputNoIdx;
  const threadUtxo = await fetchUtxoByOutRef({
    lucid,
    outRef: parseOutRef(threadOutRef, "--thread-out-ref"),
    label: "raw reference-input-no-idx step-02 thread UTxO",
  });
  if (threadUtxo.address !== chain.steps[1].spendingScriptAddress) {
    throw new Error(
      `Thread UTxO ${outRefLabel(threadUtxo)} is not locked at reference-input-no-idx step 02.`,
    );
  }
  const threadToken = requireComputationThreadToken({
    utxo: threadUtxo,
    computationThreadPolicyId: contracts.computationThread.policyId,
    categoryId: referenceInputNoIdxCategory.categoryId,
    categoryLabel: "reference-input-no-idx",
  });
  const inputDatum = Data.from(
    threadUtxo.datum!,
    ReferenceInputNoIdxStep02Datum,
  );
  if (inputDatum.data === null) {
    throw new Error("raw step-02 thread carries no §2.5 anchor");
  }
  const planned = planFaultProofFieldOpening({
    fieldIndex: MIDGARD_FIELD_INDEX.referenceInputs,
    anchorTxId: inputDatum.data.verified_tx_id,
    nativeTxCompactCbor,
    itemCbors: referenceInputsPreimage.map(encodeMidgardTxInputCanonical),
    owner: signer.paymentKeyHash,
    label: "Raw reference-input-no-idx step 02 reference-inputs",
  });
  signer.selectWallet(lucid);
  const published = await publishFaultProofFieldCarriage({
    lucid,
    signer,
    planned,
    publisherAddress: signer.address,
    label: "Raw reference-input-no-idx step 02 reference-inputs field",
  });
  const stepReference = requireFaultProofStepReferenceScript({
    utxo: referenceScriptUtxo,
    expectedScriptHash: chain.steps[1].spendingScriptHash,
    label: "raw reference-input-no-idx step 02",
  });
  const referenceInputs = [...published, stepReference];
  const referenceInputsOpening: FieldOpening = faultProofFieldOpening({
    planned,
    referenceInputs,
    label: "Raw reference-input-no-idx step 02 reference-inputs",
  });
  const feeInput = selectFeeInput(
    (await lucid.wallet().getUtxos()).filter(
      (utxo) => utxo.datum == null && utxo.datumHash == null,
    ),
  );
  const step03Datum = Data.to(
    {
      fraud_prover: signer.paymentKeyHash,
      data: referenceInputNoIdxStep03StateFromBadInput(forwardedReferenceInput),
    },
    ReferenceInputNoIdxStep03Datum,
  );
  const step03OutputMatches = computationThreadOutputPredicate({
    address: chain.steps[2].spendingScriptAddress,
    datum: step03Datum,
    unit: threadToken.unit,
  });
  const redeemer = ((ctx) => {
    requireOwnSpendPurpose(
      ctx,
      threadUtxo,
      "raw reference-input-no-idx step 02",
    );
    return Data.to(
      {
        Continue: [
          {
            input_index: requireInputIndex(
              ctx,
              threadUtxo,
              "raw reference-input-no-idx step 02",
            ),
            output_index: requireUniqueOutputIndex(
              ctx.outputs,
              step03OutputMatches,
              "raw reference-input-no-idx step 02 output",
            ),
            reference_inputs_opening: referenceInputsOpening,
            bad_reference_input_index: BigInt(badReferenceInputIndex),
          },
        ],
      },
      ReferenceInputNoIdxStep02SpendRedeemer,
    );
  }) satisfies BuildTxWithRedeemer;

  const unsigned = await lucid
    .newTx()
    .collectFrom([feeInput])
    .collectFrom([threadUtxo], redeemer)
    .readFrom([...referenceInputs])
    .pay.ToContract(
      chain.steps[2].spendingScriptAddress,
      { kind: "inline", value: step03Datum },
      {
        lovelace: threadUtxo.assets.lovelace ?? 0n,
        [threadToken.unit]: 1n,
      },
    )
    .addSignerKey(signer.paymentKeyHash)
    .complete({ localUPLCEval: true });
  const signed = await unsigned.sign.withWallet().complete();
  const txHash = await signed.submit();
  await lucid.awaitTx(txHash);
  return txHash;
};

/**
 * A raw step-04 finalize: the honest submitter's thread-burn/token-mint
 * transaction with the local `isReferenceInputNoIdxViolationV1` twin removed,
 * so a challenged index that genuinely EXISTS in the producing transaction
 * reaches the validator's own
 * `bad_reference_input_output_index >= field_item_count(outputs_view)`.
 */
export const submitRawReferenceInputNoIdxStep04 = async ({
  lucid,
  blueprint,
  deploymentInfo,
  signer,
  threadOutRef,
  outputsPreimage,
  nativeTxCompactCbor,
  referenceScriptUtxo,
  witnessReferenceScripts,
}: RawStepConfig & {
  readonly outputsPreimage: readonly MidgardTxOutput[];
  readonly nativeTxCompactCbor: string;
  readonly witnessReferenceScripts: FaultProofWitnessReferenceScripts;
}): Promise<string> => {
  const { referenceInputNoIdxCategory, contracts } =
    await resolveReferenceInputNoIdxDeploymentContracts({
      blueprint,
      deploymentInfo,
      network,
      requireFraudProofSpend: true,
    });
  const chain = contracts.referenceInputNoIdx;
  const threadUtxo = await fetchUtxoByOutRef({
    lucid,
    outRef: parseOutRef(threadOutRef, "--thread-out-ref"),
    label: "raw reference-input-no-idx step-04 thread UTxO",
  });
  if (threadUtxo.address !== chain.steps[3].spendingScriptAddress) {
    throw new Error(
      `Thread UTxO ${outRefLabel(threadUtxo)} is not locked at reference-input-no-idx step 04.`,
    );
  }
  const threadToken = requireComputationThreadToken({
    utxo: threadUtxo,
    computationThreadPolicyId: contracts.computationThread.policyId,
    categoryId: referenceInputNoIdxCategory.categoryId,
    categoryLabel: "reference-input-no-idx",
  });
  const inputDatum = Data.from(
    threadUtxo.datum!,
    ReferenceInputNoIdxStep04Datum,
  );
  if (inputDatum.data === null) {
    throw new Error("raw step-04 thread carries no producing-tx anchor");
  }
  const planned = planFaultProofFieldOpening({
    fieldIndex: MIDGARD_FIELD_INDEX.outputs,
    anchorTxId: inputDatum.data.producing_tx_id,
    nativeTxCompactCbor,
    itemCbors: outputsPreimage.map(encodeMidgardTxOutputCanonical),
    owner: signer.paymentKeyHash,
    label: "Raw reference-input-no-idx step 04 outputs",
  });
  signer.selectWallet(lucid);
  const published = await publishFaultProofFieldCarriage({
    lucid,
    signer,
    planned,
    publisherAddress: signer.address,
    label: "Raw reference-input-no-idx step 04 outputs field",
  });
  const stepReference = requireFaultProofStepReferenceScript({
    utxo: referenceScriptUtxo,
    expectedScriptHash: chain.steps[3].spendingScriptHash,
    label: "raw reference-input-no-idx step 04",
  });
  const referenceInputs = [...published, stepReference];
  const outputsOpening: FieldOpening = faultProofFieldOpening({
    planned,
    referenceInputs,
    label: "Raw reference-input-no-idx step 04 outputs",
  });
  const feeInput = selectFeeInput(
    (await lucid.wallet().getUtxos()).filter(
      (utxo) => utxo.datum == null && utxo.datumHash == null,
    ),
  );
  const fraudProofUnit = toUnit(
    contracts.fraudProof.policyId,
    threadToken.assetName,
  );
  const fraudProofDatum = Data.to(
    { fraud_prover: signer.paymentKeyHash },
    FraudProofTokenDatum,
  );
  const fraudProofOutputMatches = outputWithDatumAndUnitPredicate({
    address: contracts.fraudProof.spendingScriptAddress,
    datum: fraudProofDatum,
    unit: fraudProofUnit,
  });
  const spendRedeemer = ((ctx) => {
    requireOwnSpendPurpose(
      ctx,
      threadUtxo,
      "raw reference-input-no-idx step 04",
    );
    return Data.to(
      {
        Continue: [
          {
            input_index: requireInputIndex(
              ctx,
              threadUtxo,
              "raw reference-input-no-idx step 04",
            ),
            output_index: requireUniqueOutputIndex(
              ctx.outputs,
              fraudProofOutputMatches,
              "raw reference-input-no-idx step 04 fraud-proof",
            ),
            fraud_proof_mint_redeemer_index: requireMintRedeemerIndex(
              ctx,
              contracts.fraudProof.policyId,
              "raw reference-input-no-idx step 04 fraud-proof",
            ),
            outputs_opening: outputsOpening,
          },
        ],
      },
      ReferenceInputNoIdxStep04SpendRedeemer,
    );
  }) satisfies BuildTxWithRedeemer;
  const threadBurnRedeemer = ((ctx) => {
    requireOwnMintPurpose(
      ctx,
      contracts.computationThread.policyId,
      "raw reference-input-no-idx step 04 thread burn",
    );
    return Data.to(
      { Success: { burning_token_asset_name: threadToken.assetName } },
      FraudProofComputationThreadRedeemer,
    );
  }) satisfies BuildTxWithRedeemer;
  const fraudProofMintRedeemer = ((ctx) => {
    requireOwnMintPurpose(
      ctx,
      contracts.fraudProof.policyId,
      "raw reference-input-no-idx step 04 fraud-proof mint",
    );
    return Data.to(
      {
        computation_thread_token_asset_name: threadToken.assetName,
        computation_thread_mint_redeemer_index: requireMintRedeemerIndex(
          ctx,
          contracts.computationThread.policyId,
          "raw reference-input-no-idx step 04 thread burn",
        ),
      },
      FraudProofTokenMintRedeemer,
    );
  }) satisfies BuildTxWithRedeemer;

  const computationThreadCarriage = witnessMintingPolicyCarriage({
    script: contracts.computationThread.mintingScript,
    referenceUtxo: witnessReferenceScripts.computationThreadMint,
    label: "raw reference-input-no-idx step 04 computation-thread mint",
  });
  const fraudProofCarriage = witnessMintingPolicyCarriage({
    script: contracts.fraudProof.mintingScript,
    referenceUtxo: witnessReferenceScripts.fraudProofMint,
    label: "raw reference-input-no-idx step 04 fraud-proof mint",
  });

  const base = lucid
    .newTx()
    .collectFrom([feeInput])
    .collectFrom([threadUtxo], spendRedeemer)
    .readFrom([
      ...referenceInputs,
      ...computationThreadCarriage.referenceInputs,
      ...fraudProofCarriage.referenceInputs,
    ])
    .mintAssets({ [threadToken.unit]: -1n }, threadBurnRedeemer)
    .mintAssets({ [fraudProofUnit]: 1n }, fraudProofMintRedeemer)
    .pay.ToContract(
      contracts.fraudProof.spendingScriptAddress,
      { kind: "inline", value: fraudProofDatum },
      {
        lovelace: threadUtxo.assets.lovelace ?? 0n,
        [fraudProofUnit]: 1n,
      },
    )
    .addSignerKey(signer.paymentKeyHash);
  const unsigned = await fraudProofCarriage
    .attach(computationThreadCarriage.attach(base))
    .complete({ localUPLCEval: true });
  const signed = await unsigned.sign.withWallet().complete();
  const txHash = await signed.submit();
  await lucid.awaitTx(txHash);
  return txHash;
};
