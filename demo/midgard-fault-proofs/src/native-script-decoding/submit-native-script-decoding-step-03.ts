/**
 * `native-script-decoding` step-03 submitters (offchain plan §4.2) — the
 * thread's working step, one submitter per redeemer arm:
 *
 * - `submitNativeScriptDecodingStep03BindOutpoint`: opens the accused field
 *   through the §8.8 door, authenticates the ledger's own resolution of the
 *   accused outpoint under `prior_ledger_root`, and freezes the scan anchor.
 *   Tag-0 descriptors either start the frozen machine (self-loop into Scan)
 *   or close for direction A on an undecodable wrapper; a non-tag-0
 *   descriptor closes for direction B (descriptor contradiction).
 * - `submitNativeScriptDecodingStep03Scan`: one planned self-loop segment —
 *   the planner's control/window/frames/budget ride the redeemer verbatim,
 *   with chunk proofs rebuilt over the authenticated reference-script item.
 * - `submitNativeScriptDecodingStep03Verdict`: hands the classed state to
 *   step-04 — direction A exhibits the single refusing step, direction B the
 *   exact canonical terminal.
 * - `submitNativeScriptDecodingStep03BindOutOfDomain`: the #633 §7.2 closing
 *   arm — the accusation's verbatim pair names a subject the committed
 *   transaction does not have; the face decides whether a door opening is
 *   owed at all.
 *
 * Every validator abort this process can predict locally is refused before
 * anything is paid for, with the failure message naming the check.
 */
import {
  buildMidgardBoundedItemV1,
  decodeMidgardLedgerOutputCommitmentV1,
  MIDGARD_LEDGER_OUTPUT_FIELD_INDEX_V1,
  type MidgardLedgerOutputCommitmentV1,
} from "@al-ft/midgard-core";
import type {
  BoundedItemChunkProofV1,
  FieldOpeningV1,
  NativeScriptDecodingScanThreadStateV1,
  NativeScriptDecodingStep03Args,
  Proof,
} from "@al-ft/midgard-sdk";
import {
  encodeMidgardTxInputCanonicalV1,
  type MidgardTxInput,
  NATIVE_SCRIPT_DECODING_CLASS_PENDING_V1,
  NATIVE_SCRIPT_DECODING_DIRECTION_WRONGFUL_ACCEPTANCE_V1,
  NATIVE_SCRIPT_DECODING_DIRECTION_WRONGFUL_REJECTION_V1,
  NATIVE_SCRIPT_DECODING_REFUSAL_CLASS_MALFORMED_V1,
  nativeScriptDecodingBoundScanStateV1,
  NativeScriptDecodingStep03Datum,
  NativeScriptDecodingStep03SpendRedeemer,
  requireInputIndex,
  requireOwnSpendPurpose,
  requireUniqueOutputIndex,
} from "@al-ft/midgard-sdk";
import {
  type BuildTxWithRedeemer,
  Data,
  type LucidEvolution,
  type UTxO,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";

import {
  faultProofFieldOpeningV1,
  planFaultProofFieldOpeningV1,
  publishFaultProofFieldCarriageV1,
} from "../field-opening-v1.js";
import {
  DEFAULT_CONFIRMATION_POLL_MS,
  type ResolvedProverSigner,
} from "../runtime.js";
import { excludeUtxo } from "../spend-input-witness.js";
import { selectFeeInput } from "../submit-step-01.js";
import { computationThreadOutputPredicate } from "../tx-layout.js";
import type { NativeScriptDecodingContractsV1 } from "./contracts-v1.js";
import {
  buildNativeScriptDecodingChunkProofV1,
  buildNativeScriptDecodingLedgerMembershipV1,
  classifyNativeScriptDecodingOutOfDomainFaceV1,
  type NativeScriptDecodingLedgerTrieHandleV1,
  NativeScriptDecodingOutOfDomainFacesV1,
  nativeScriptDecodingOutpointKeyV1,
  nativeScriptDecodingScanArgsEvidenceV1,
  nativeScriptDecodingSubjectFieldIndexV1,
  nativeScriptDecodingWindowProofsV1,
} from "./evidence-v1.js";
import {
  NativeScriptDecodingPlanRoutesV1,
  type NativeScriptDecodingScanPlanV1,
  type NativeScriptDecodingScanSegmentPlanV1,
  type NativeScriptDecodingVerdictPlanV1,
} from "./scan-plan-v1.js";
import {
  nativeScriptDecodingStepLabelV1,
  nativeScriptDecodingSubmitError,
  requireNativeScriptDecodingReferenceScriptV1,
  requireNativeScriptDecodingStepStateV1,
  requireNativeScriptDecodingThreadUtxoV1,
} from "./submit-common-v1.js";

const STEP_LABEL = nativeScriptDecodingStepLabelV1(2);

export type SubmitNativeScriptDecodingStep03Result = {
  readonly txHash: string;
  readonly walletSource: string;
  readonly proverAddress: string;
  readonly fraudProver: string;
  readonly threadOutRef: string;
  readonly nextThreadOutRef: string;
  readonly fraudulentHeaderHash: string;
  readonly computationThreadUnit: string;
  /** Where the thread now sits: step-03's own address, or step-04's. */
  readonly destinationAddress: string;
  /** The `ScanThreadStateV1` the thread now carries. */
  readonly scanState: NativeScriptDecodingScanThreadStateV1;
  readonly inputIndex: number;
  readonly outputIndex: number;
  readonly awaitedConfirmation: boolean;
};

// ## Shared plumbing

type Step03Layout = {
  readonly inputIndex: bigint;
  readonly outputIndex: bigint;
};

const requireStep03State = ({
  threadUtxo,
  signer,
}: {
  readonly threadUtxo: UTxO;
  readonly signer: ResolvedProverSigner;
}): NativeScriptDecodingScanThreadStateV1 =>
  requireNativeScriptDecodingStepStateV1({
    threadUtxo,
    signer,
    schema: NativeScriptDecodingStep03Datum,
    stepIndex: 2,
  });

const requirePreBindState = (
  state: NativeScriptDecodingScanThreadStateV1,
): void => {
  if (
    state.machine_state_hash !== "" ||
    state.refusal_class !== NATIVE_SCRIPT_DECODING_CLASS_PENDING_V1 ||
    state.outpoint_key_hash !== ""
  ) {
    throw nativeScriptDecodingSubmitError(
      "the thread is already bound; the bind arms run exactly once, on a pre-bind state.",
    );
  }
};

const requireBoundPendingState = (
  state: NativeScriptDecodingScanThreadStateV1,
): void => {
  if (
    state.machine_state_hash === "" ||
    state.refusal_class !== NATIVE_SCRIPT_DECODING_CLASS_PENDING_V1
  ) {
    throw nativeScriptDecodingSubmitError(
      "only a bound, unclassed machine scans; the thread state disagrees.",
    );
  }
};

/**
 * Rebuilds the reference-script item's bounded-item commitment and refuses
 * bytes that are not the frozen anchor's — a substituted item would make
 * every chunk proof fail on-chain.
 */
const requireAnchoredItemBytes = ({
  itemBytes,
  itemIndex,
  totalLength,
  itemCommitmentHex,
}: {
  readonly itemBytes: Uint8Array;
  readonly itemIndex: number;
  readonly totalLength: bigint;
  readonly itemCommitmentHex: string;
}): void => {
  if (BigInt(itemBytes.length) !== totalLength) {
    throw nativeScriptDecodingSubmitError(
      `the supplied reference-script item is ${itemBytes.length.toString()} bytes, but the frozen anchor commits ${totalLength.toString()}.`,
    );
  }
  const rebuilt = buildMidgardBoundedItemV1({
    fieldIndex: MIDGARD_LEDGER_OUTPUT_FIELD_INDEX_V1,
    itemIndex,
    bytes: itemBytes,
  });
  if (rebuilt.commitment.toString("hex") !== itemCommitmentHex) {
    throw nativeScriptDecodingSubmitError(
      "the supplied reference-script item bytes do not rebuild the frozen item commitment.",
    );
  }
};

/**
 * The common thread-advancement transaction: fee input, thread spend with a
 * layout-resolving redeemer, the advanced state paid to `destinationAddress`,
 * carriage (if any) read as reference inputs, and the Q3 step-script
 * sourcing.
 */
const advanceStep03Thread = async ({
  lucid,
  contracts,
  signer,
  threadUtxo,
  threadUnit,
  destinationAddress,
  nextState,
  buildArgs,
  carriageUtxos,
  referenceScriptUtxo,
  awaitConfirmation,
}: {
  readonly lucid: LucidEvolution;
  readonly contracts: NativeScriptDecodingContractsV1;
  readonly signer: ResolvedProverSigner;
  readonly threadUtxo: UTxO;
  readonly threadUnit: string;
  readonly destinationAddress: string;
  readonly nextState: NativeScriptDecodingScanThreadStateV1;
  readonly buildArgs: (layout: Step03Layout) => NativeScriptDecodingStep03Args;
  readonly carriageUtxos: readonly UTxO[];
  readonly referenceScriptUtxo: UTxO;
  readonly awaitConfirmation: boolean;
}): Promise<{ readonly txHash: string; readonly layout: Step03Layout }> => {
  signer.selectWallet(lucid);
  const walletUtxos = await lucid.wallet().getUtxos();
  const walletUtxosSansCarriage = carriageUtxos.reduce<readonly UTxO[]>(
    (candidates, utxo) => excludeUtxo(candidates, utxo),
    walletUtxos,
  );
  const feeInput = selectFeeInput(walletUtxosSansCarriage);
  const nextDatum = Data.to(
    { fraud_prover: signer.paymentKeyHash, data: nextState },
    NativeScriptDecodingStep03Datum,
  );
  const outputMatches = computationThreadOutputPredicate({
    address: destinationAddress,
    datum: nextDatum,
    unit: threadUnit,
  });
  let resolvedLayout: Step03Layout | undefined;
  const redeemer = ((ctx) => {
    requireOwnSpendPurpose(ctx, threadUtxo, STEP_LABEL);
    const layout: Step03Layout = {
      inputIndex: requireInputIndex(ctx, threadUtxo, STEP_LABEL),
      outputIndex: requireUniqueOutputIndex(
        ctx.outputs,
        outputMatches,
        `${STEP_LABEL} output`,
      ),
    };
    resolvedLayout = layout;
    return Data.to(
      { Continue: [buildArgs(layout)] },
      NativeScriptDecodingStep03SpendRedeemer,
    );
  }) satisfies BuildTxWithRedeemer;
  const threadAssets = {
    lovelace: threadUtxo.assets.lovelace ?? 0n,
    [threadUnit]: 1n,
  };

  const referenceInputs = [
    ...carriageUtxos,
    requireNativeScriptDecodingReferenceScriptV1({
      utxo: referenceScriptUtxo,
      expectedScriptHash: contracts.steps[2].spendingScriptHash,
      stepIndex: 2,
    }),
  ];
  const withInputs = lucid
    .newTx()
    .collectFrom([feeInput])
    .collectFrom([threadUtxo], redeemer);
  const withReferences =
    referenceInputs.length === 0
      ? withInputs
      : withInputs.readFrom(referenceInputs);
  const paid = withReferences.pay
    .ToContract(
      destinationAddress,
      { kind: "inline", value: nextDatum },
      threadAssets,
    )
    .addSignerKey(signer.paymentKeyHash);
  const tx = paid;

  const unsigned = await tx.complete({
    localUPLCEval: true,
    ...(carriageUtxos.length === 0
      ? {}
      : { presetWalletInputs: walletUtxosSansCarriage as UTxO[] }),
  });
  if (resolvedLayout === undefined) {
    throw nativeScriptDecodingSubmitError(
      "BuildTxWithRedeemer did not resolve the step-03 layout.",
    );
  }
  const signed = await unsigned.sign.withWallet().complete();
  const txHash = await signed.submit();
  if (awaitConfirmation) {
    await lucid.awaitTx(txHash, DEFAULT_CONFIRMATION_POLL_MS);
  }
  return { txHash, layout: resolvedLayout };
};

const step03Result = ({
  txHash,
  layout,
  signer,
  threadOutRef,
  threadToken,
  destinationAddress,
  scanState,
  awaitConfirmation,
}: {
  readonly txHash: string;
  readonly layout: Step03Layout;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  readonly threadToken: {
    readonly unit: string;
    readonly fraudulentHeaderHash: string;
  };
  readonly destinationAddress: string;
  readonly scanState: NativeScriptDecodingScanThreadStateV1;
  readonly awaitConfirmation: boolean;
}): SubmitNativeScriptDecodingStep03Result => ({
  txHash,
  walletSource: signer.source,
  proverAddress: signer.address,
  fraudProver: signer.paymentKeyHash,
  threadOutRef,
  nextThreadOutRef: `${txHash}#${layout.outputIndex.toString()}`,
  fraudulentHeaderHash: threadToken.fraudulentHeaderHash,
  computationThreadUnit: threadToken.unit,
  destinationAddress,
  scanState,
  inputIndex: Number(layout.inputIndex),
  outputIndex: Number(layout.outputIndex),
  awaitedConfirmation: awaitConfirmation,
});

// ## BindOutpoint

export const submitNativeScriptDecodingStep03BindOutpoint = async ({
  lucid,
  contracts,
  categoryId,
  signer,
  threadOutRef,
  nativeTxCompactCbor,
  subjectFieldInputs,
  descriptorCbor,
  ledgerTrie,
  plan,
  referenceScriptItemBytes,
  publishCarriage = false,
  referenceScriptUtxo,
  awaitConfirmation = true,
}: {
  readonly lucid: LucidEvolution;
  readonly contracts: NativeScriptDecodingContractsV1;
  readonly categoryId: string;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  /** The committed transaction's compact bytes (§2.5 anchor: `verified_tx_id`). */
  readonly nativeTxCompactCbor: string;
  /** The accused field's complete §5.1 item list, in committed order. */
  readonly subjectFieldInputs: readonly MidgardTxInput[];
  /** The ledger's resolution of the accused outpoint, canonical CBOR hex. */
  readonly descriptorCbor: string;
  /** Pre-state ledger trie whose root is the thread's `prior_ledger_root`. */
  readonly ledgerTrie: NativeScriptDecodingLedgerTrieHandleV1;
  /** The staged scan plan. Required for tag-0 descriptors; unused otherwise. */
  readonly plan?: NativeScriptDecodingScanPlanV1;
  /** The reference-script item bytes. Required for tag-0 descriptors. */
  readonly referenceScriptItemBytes?: Uint8Array;
  /** Force §8 tier 2 carriage publication. */
  readonly publishCarriage?: boolean;
  readonly referenceScriptUtxo: UTxO;
  readonly awaitConfirmation?: boolean;
}): Promise<SubmitNativeScriptDecodingStep03Result> => {
  const { threadUtxo, threadToken } =
    await requireNativeScriptDecodingThreadUtxoV1({
      lucid,
      contracts,
      categoryId,
      stepIndex: 2,
      threadOutRef,
    });
  const state = requireStep03State({ threadUtxo, signer });
  requirePreBindState(state);

  // §7.3 abort-never-clamp: a pair outside the committed domain can never
  // bind — it is the closing arm's to consume.
  const fieldIndex = nativeScriptDecodingSubjectFieldIndexV1(
    state.outpoint_source_kind,
  );
  if (
    state.outpoint_cursor < 0n ||
    state.outpoint_cursor >= BigInt(subjectFieldInputs.length)
  ) {
    throw nativeScriptDecodingSubmitError(
      `accused ordinal ${state.outpoint_cursor.toString()} is outside the field's ${subjectFieldInputs.length.toString()} items — close the thread through BindOutOfDomain instead.`,
    );
  }
  const subjectOutpoint = subjectFieldInputs[Number(state.outpoint_cursor)]!;

  // The descriptor must be the accused outpoint's own resolution.
  const descriptor: MidgardLedgerOutputCommitmentV1 =
    decodeMidgardLedgerOutputCommitmentV1(Buffer.from(descriptorCbor, "hex"));
  if (BigInt(descriptor.outputIndex) !== subjectOutpoint.output_index) {
    throw nativeScriptDecodingSubmitError(
      `the descriptor resolves output index ${descriptor.outputIndex.toString()}, but the accused outpoint names ${subjectOutpoint.output_index.toString()}.`,
    );
  }
  if (descriptor.totalLength <= 0) {
    throw nativeScriptDecodingSubmitError(
      "the descriptor commits a non-positive output item length.",
    );
  }

  const outpointKey = nativeScriptDecodingOutpointKeyV1({
    txIdHex: subjectOutpoint.tx_id,
    outputIndex: Number(subjectOutpoint.output_index),
  });
  const ledgerMembershipProof: Proof =
    await buildNativeScriptDecodingLedgerMembershipV1({
      trie: ledgerTrie,
      outpointKey,
      priorLedgerRootHex: state.prior_ledger_root,
    });
  const bound = await Effect.runPromise(
    nativeScriptDecodingBoundScanStateV1({
      state,
      outpointKeyBytes: outpointKey.toString("hex"),
      referenceScriptLanguage: BigInt(descriptor.referenceScriptLanguage),
      outputIndex: BigInt(descriptor.outputIndex),
      referenceScriptTotalLength: BigInt(descriptor.referenceScriptTotalLength),
      referenceScriptItemCommitment:
        descriptor.referenceScriptItemCommitment.toString("hex"),
    }),
  );

  // Branch on the bound descriptor's language, mirroring the validator.
  let nextState: NativeScriptDecodingScanThreadStateV1;
  let destinationAddress: string;
  let firstChunkProof: BoundedItemChunkProofV1 | null;
  if (descriptor.referenceScriptLanguage === 0) {
    if (plan === undefined || referenceScriptItemBytes === undefined) {
      throw nativeScriptDecodingSubmitError(
        "a tag-0 descriptor needs the scan plan and the reference-script item bytes.",
      );
    }
    requireAnchoredItemBytes({
      itemBytes: referenceScriptItemBytes,
      itemIndex: descriptor.outputIndex,
      totalLength: BigInt(descriptor.referenceScriptTotalLength),
      itemCommitmentHex:
        descriptor.referenceScriptItemCommitment.toString("hex"),
    });
    firstChunkProof = buildNativeScriptDecodingChunkProofV1({
      fieldIndex: MIDGARD_LEDGER_OUTPUT_FIELD_INDEX_V1,
      itemIndex: descriptor.outputIndex,
      itemBytes: referenceScriptItemBytes,
      chunkIndex: 0,
    });
    if (plan.route === NativeScriptDecodingPlanRoutesV1.Machine) {
      const bindControl =
        plan.segments[0]?.controlBefore ?? plan.verdict.control;
      if (bindControl === null) {
        throw nativeScriptDecodingSubmitError(
          "the machine-route plan carries no bind control.",
        );
      }
      nextState = { ...bound, machine_state_hash: bindControl.hashHex };
      destinationAddress = contracts.steps[2].spendingScriptAddress;
    } else if (plan.route === NativeScriptDecodingPlanRoutesV1.BindMalformed) {
      if (
        state.direction !==
        NATIVE_SCRIPT_DECODING_DIRECTION_WRONGFUL_ACCEPTANCE_V1
      ) {
        throw nativeScriptDecodingSubmitError(
          "an undecodable wrapper closes the bind for direction A only; for direction B it merely corroborates the accusation.",
        );
      }
      nextState = {
        ...bound,
        refusal_class: NATIVE_SCRIPT_DECODING_REFUSAL_CLASS_MALFORMED_V1,
      };
      destinationAddress = contracts.steps[3].spendingScriptAddress;
    } else {
      throw nativeScriptDecodingSubmitError(
        "the plan claims a descriptor contradiction, but the bound descriptor is tag-0.",
      );
    }
  } else {
    if (
      state.direction !== NATIVE_SCRIPT_DECODING_DIRECTION_WRONGFUL_REJECTION_V1
    ) {
      throw nativeScriptDecodingSubmitError(
        "a non-tag-0 descriptor contradicts a decoding accusation and closes for direction B only; direction A has nothing to prove here.",
      );
    }
    firstChunkProof = null;
    nextState = {
      ...bound,
      refusal_class: NATIVE_SCRIPT_DECODING_REFUSAL_CLASS_MALFORMED_V1,
    };
    destinationAddress = contracts.steps[3].spendingScriptAddress;
  }

  // The §8.8 door: plan, publish whatever the tier demands, open.
  const planned = planFaultProofFieldOpeningV1({
    fieldIndex,
    anchorTxId: state.verified_tx_id,
    nativeTxCompactCbor,
    itemCbors: subjectFieldInputs.map(encodeMidgardTxInputCanonicalV1),
    owner: signer.paymentKeyHash,
    publish: publishCarriage,
    label: `${STEP_LABEL} subject field`,
  });
  signer.selectWallet(lucid);
  const carriageUtxos = await publishFaultProofFieldCarriageV1({
    lucid,
    signer,
    planned,
    publisherAddress: signer.address,
    label: `${STEP_LABEL} subject field`,
  });
  const subjectFieldOpening: FieldOpeningV1 = faultProofFieldOpeningV1({
    planned,
    referenceInputs: carriageUtxos,
    certificatePolicyId: contracts.fieldPreimageCertificatePolicyId,
    label: `${STEP_LABEL} subject field`,
  });

  const { txHash, layout } = await advanceStep03Thread({
    lucid,
    contracts,
    signer,
    threadUtxo,
    threadUnit: threadToken.unit,
    destinationAddress,
    nextState,
    buildArgs: (layout) => ({
      BindOutpoint: {
        input_index: layout.inputIndex,
        output_index: layout.outputIndex,
        subject_field_opening: subjectFieldOpening,
        descriptor_cbor: descriptorCbor,
        ledger_membership_proof: ledgerMembershipProof,
        first_chunk_proof: firstChunkProof,
      },
    }),
    carriageUtxos,
    referenceScriptUtxo,
    awaitConfirmation,
  });
  return step03Result({
    txHash,
    layout,
    signer,
    threadOutRef,
    threadToken,
    destinationAddress,
    scanState: nextState,
    awaitConfirmation,
  });
};

// ## Scan (self-loop)

export const submitNativeScriptDecodingStep03Scan = async ({
  lucid,
  contracts,
  categoryId,
  signer,
  threadOutRef,
  segment,
  referenceScriptItemBytes,
  referenceScriptUtxo,
  awaitConfirmation = true,
}: {
  readonly lucid: LucidEvolution;
  readonly contracts: NativeScriptDecodingContractsV1;
  readonly categoryId: string;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  /** The plan segment whose `controlBefore` is the thread's committed machine. */
  readonly segment: NativeScriptDecodingScanSegmentPlanV1;
  readonly referenceScriptItemBytes: Uint8Array;
  readonly referenceScriptUtxo: UTxO;
  readonly awaitConfirmation?: boolean;
}): Promise<SubmitNativeScriptDecodingStep03Result> => {
  const { threadUtxo, threadToken } =
    await requireNativeScriptDecodingThreadUtxoV1({
      lucid,
      contracts,
      categoryId,
      stepIndex: 2,
      threadOutRef,
    });
  const state = requireStep03State({ threadUtxo, signer });
  requireBoundPendingState(state);
  if (segment.controlBefore.hashHex !== state.machine_state_hash) {
    throw nativeScriptDecodingSubmitError(
      "the segment's control is not the thread's committed machine — resume the plan from the committed control.",
    );
  }
  requireAnchoredItemBytes({
    itemBytes: referenceScriptItemBytes,
    itemIndex: Number(state.output_index),
    totalLength: state.total_length,
    itemCommitmentHex: state.item_commitment,
  });
  const evidence = nativeScriptDecodingScanArgsEvidenceV1({
    segment,
    fieldIndex: MIDGARD_LEDGER_OUTPUT_FIELD_INDEX_V1,
    itemIndex: Number(state.output_index),
    itemBytes: referenceScriptItemBytes,
  });
  const nextState: NativeScriptDecodingScanThreadStateV1 = {
    ...state,
    machine_state_hash: segment.controlAfter.hashHex,
  };
  const destinationAddress = contracts.steps[2].spendingScriptAddress;

  const { txHash, layout } = await advanceStep03Thread({
    lucid,
    contracts,
    signer,
    threadUtxo,
    threadUnit: threadToken.unit,
    destinationAddress,
    nextState,
    buildArgs: (layout) => ({
      Scan: {
        input_index: layout.inputIndex,
        output_index: layout.outputIndex,
        control_cbor: evidence.control_cbor,
        chunk_proof: evidence.chunk_proof,
        next_chunk_proof: evidence.next_chunk_proof,
        frames: [...evidence.frames],
        step_budget: evidence.step_budget,
      },
    }),
    carriageUtxos: [],
    referenceScriptUtxo,
    awaitConfirmation,
  });
  return step03Result({
    txHash,
    layout,
    signer,
    threadOutRef,
    threadToken,
    destinationAddress,
    scanState: nextState,
    awaitConfirmation,
  });
};

// ## Verdict

export const submitNativeScriptDecodingStep03Verdict = async ({
  lucid,
  contracts,
  categoryId,
  signer,
  threadOutRef,
  verdict,
  referenceScriptItemBytes,
  referenceScriptUtxo,
  awaitConfirmation = true,
}: {
  readonly lucid: LucidEvolution;
  readonly contracts: NativeScriptDecodingContractsV1;
  readonly categoryId: string;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  /** The plan's verdict: the committed control plus, for direction A, the refusing step's window and class. */
  readonly verdict: NativeScriptDecodingVerdictPlanV1;
  /** Required whenever the verdict's refusing step reads a chunk window. */
  readonly referenceScriptItemBytes?: Uint8Array;
  readonly referenceScriptUtxo: UTxO;
  readonly awaitConfirmation?: boolean;
}): Promise<SubmitNativeScriptDecodingStep03Result> => {
  const { threadUtxo, threadToken } =
    await requireNativeScriptDecodingThreadUtxoV1({
      lucid,
      contracts,
      categoryId,
      stepIndex: 2,
      threadOutRef,
    });
  const state = requireStep03State({ threadUtxo, signer });
  requireBoundPendingState(state);
  if (verdict.control === null) {
    throw nativeScriptDecodingSubmitError(
      "the plan's verdict carries no control — a close-at-bind route never reaches Verdict.",
    );
  }
  if (verdict.control.hashHex !== state.machine_state_hash) {
    throw nativeScriptDecodingSubmitError(
      "the verdict's control is not the thread's committed machine — run the remaining Scan segments first.",
    );
  }

  let refusalClass: bigint;
  let chunkProof: BoundedItemChunkProofV1 | null = null;
  let nextChunkProof: BoundedItemChunkProofV1 | null = null;
  if (
    state.direction === NATIVE_SCRIPT_DECODING_DIRECTION_WRONGFUL_REJECTION_V1
  ) {
    // Direction B: the exact canonical terminal, no window.
    if (verdict.window !== null) {
      throw nativeScriptDecodingSubmitError(
        "direction B's verdict is the exact terminal; it reads no chunk window.",
      );
    }
    refusalClass = NATIVE_SCRIPT_DECODING_REFUSAL_CLASS_MALFORMED_V1;
  } else {
    if (
      state.direction !==
      NATIVE_SCRIPT_DECODING_DIRECTION_WRONGFUL_ACCEPTANCE_V1
    ) {
      throw nativeScriptDecodingSubmitError(
        `thread state carries direction ${state.direction.toString()}, outside {0, 1}.`,
      );
    }
    if (verdict.refusalClass === null) {
      throw nativeScriptDecodingSubmitError(
        "direction A's verdict needs the refusing step's predicted class; the plan carries none.",
      );
    }
    refusalClass = BigInt(verdict.refusalClass);
    if (verdict.window !== null) {
      if (referenceScriptItemBytes === undefined) {
        throw nativeScriptDecodingSubmitError(
          "the verdict's refusing step reads a chunk window; supply the reference-script item bytes.",
        );
      }
      requireAnchoredItemBytes({
        itemBytes: referenceScriptItemBytes,
        itemIndex: Number(state.output_index),
        totalLength: state.total_length,
        itemCommitmentHex: state.item_commitment,
      });
      const proofs = nativeScriptDecodingWindowProofsV1({
        window: verdict.window,
        fieldIndex: MIDGARD_LEDGER_OUTPUT_FIELD_INDEX_V1,
        itemIndex: Number(state.output_index),
        itemBytes: referenceScriptItemBytes,
      });
      chunkProof = proofs.chunk_proof;
      nextChunkProof = proofs.next_chunk_proof;
    }
  }
  const nextState: NativeScriptDecodingScanThreadStateV1 = {
    ...state,
    refusal_class: refusalClass,
  };
  const destinationAddress = contracts.steps[3].spendingScriptAddress;

  const controlCbor = verdict.control.cborHex;
  const { txHash, layout } = await advanceStep03Thread({
    lucid,
    contracts,
    signer,
    threadUtxo,
    threadUnit: threadToken.unit,
    destinationAddress,
    nextState,
    buildArgs: (layout) => ({
      Verdict: {
        input_index: layout.inputIndex,
        output_index: layout.outputIndex,
        control_cbor: controlCbor,
        chunk_proof: chunkProof,
        next_chunk_proof: nextChunkProof,
      },
    }),
    carriageUtxos: [],
    referenceScriptUtxo,
    awaitConfirmation,
  });
  return step03Result({
    txHash,
    layout,
    signer,
    threadOutRef,
    threadToken,
    destinationAddress,
    scanState: nextState,
    awaitConfirmation,
  });
};

// ## BindOutOfDomain (#633 §7.2 closing arm)

export const submitNativeScriptDecodingStep03BindOutOfDomain = async ({
  lucid,
  contracts,
  categoryId,
  signer,
  threadOutRef,
  nativeTxCompactCbor,
  subjectFieldInputs,
  publishCarriage = false,
  referenceScriptUtxo,
  awaitConfirmation = true,
}: {
  readonly lucid: LucidEvolution;
  readonly contracts: NativeScriptDecodingContractsV1;
  readonly categoryId: string;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  /**
   * The committed transaction's compact bytes and the named field's complete
   * item list — required for the count face only, where the contradiction is
   * proven against the door's authenticated count.
   */
  readonly nativeTxCompactCbor?: string;
  readonly subjectFieldInputs?: readonly MidgardTxInput[];
  readonly publishCarriage?: boolean;
  readonly referenceScriptUtxo: UTxO;
  readonly awaitConfirmation?: boolean;
}): Promise<SubmitNativeScriptDecodingStep03Result> => {
  const { threadUtxo, threadToken } =
    await requireNativeScriptDecodingThreadUtxoV1({
      lucid,
      contracts,
      categoryId,
      stepIndex: 2,
      threadOutRef,
    });
  const state = requireStep03State({ threadUtxo, signer });
  if (
    state.direction !== NATIVE_SCRIPT_DECODING_DIRECTION_WRONGFUL_REJECTION_V1
  ) {
    throw nativeScriptDecodingSubmitError(
      "the out-of-domain close is direction B's alone: only a committed rejection can accuse a subject the transaction does not have.",
    );
  }
  requirePreBindState(state);

  const face = classifyNativeScriptDecodingOutOfDomainFaceV1({
    outpointSourceKind: state.outpoint_source_kind,
    outpointCursor: state.outpoint_cursor,
    itemCount:
      subjectFieldInputs === undefined
        ? null
        : BigInt(subjectFieldInputs.length),
  });
  if (face === null) {
    throw nativeScriptDecodingSubmitError(
      "the accused pair is in-domain — bind it through BindOutpoint instead.",
    );
  }

  let subjectFieldOpening: FieldOpeningV1 | null = null;
  let carriageUtxos: readonly UTxO[] = [];
  if (face === NativeScriptDecodingOutOfDomainFacesV1.CountFace) {
    if (nativeTxCompactCbor === undefined || subjectFieldInputs === undefined) {
      throw nativeScriptDecodingSubmitError(
        "the count face proves against the door's authenticated count; supply the compact bytes and the named field's item list.",
      );
    }
    const fieldIndex = nativeScriptDecodingSubjectFieldIndexV1(
      state.outpoint_source_kind,
    );
    const planned = planFaultProofFieldOpeningV1({
      fieldIndex,
      anchorTxId: state.verified_tx_id,
      nativeTxCompactCbor,
      itemCbors: subjectFieldInputs.map(encodeMidgardTxInputCanonicalV1),
      owner: signer.paymentKeyHash,
      publish: publishCarriage,
      label: `${STEP_LABEL} out-of-domain subject field`,
    });
    signer.selectWallet(lucid);
    carriageUtxos = await publishFaultProofFieldCarriageV1({
      lucid,
      signer,
      planned,
      publisherAddress: signer.address,
      label: `${STEP_LABEL} out-of-domain subject field`,
    });
    subjectFieldOpening = faultProofFieldOpeningV1({
      planned,
      referenceInputs: carriageUtxos,
      certificatePolicyId: contracts.fieldPreimageCertificatePolicyId,
      label: `${STEP_LABEL} out-of-domain subject field`,
    });
  }

  const nextState: NativeScriptDecodingScanThreadStateV1 = {
    ...state,
    refusal_class: NATIVE_SCRIPT_DECODING_REFUSAL_CLASS_MALFORMED_V1,
  };
  const destinationAddress = contracts.steps[3].spendingScriptAddress;

  const opening = subjectFieldOpening;
  const { txHash, layout } = await advanceStep03Thread({
    lucid,
    contracts,
    signer,
    threadUtxo,
    threadUnit: threadToken.unit,
    destinationAddress,
    nextState,
    buildArgs: (layout) => ({
      BindOutOfDomain: {
        input_index: layout.inputIndex,
        output_index: layout.outputIndex,
        subject_field_opening: opening,
      },
    }),
    carriageUtxos,
    referenceScriptUtxo,
    awaitConfirmation,
  });
  return step03Result({
    txHash,
    layout,
    signer,
    threadOutRef,
    threadToken,
    destinationAddress,
    scanState: nextState,
    awaitConfirmation,
  });
};
