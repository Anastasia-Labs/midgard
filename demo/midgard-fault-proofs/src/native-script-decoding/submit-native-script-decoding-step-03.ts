/**
 * Submitters for the three split `native-script-decoding` step-03 spending
 * validators: OpenSubject, BindDescriptor, and AdvanceOrClose.
 *
 * Every validator abort this process can predict locally is refused before
 * anything is paid for, with the failure message naming the check.
 */
import {
  buildMidgardBoundedItem,
  decodeMidgardLedgerOutputCommitment,
  isExactMidgardNativeScriptStructureTerminal,
  MIDGARD_LEDGER_OUTPUT_FIELD_INDEX,
  type MidgardLedgerOutputCommitment,
} from "@al-ft/midgard-core";
import type {
  BoundedItemChunkProof,
  FieldOpening,
  NativeScriptDecodingScanThreadState,
  Proof,
} from "@al-ft/midgard-sdk";
import {
  encodeMidgardTxInputCanonical,
  type MidgardTxInput,
  NATIVE_SCRIPT_DECODING_CLASS_PENDING,
  NATIVE_SCRIPT_DECODING_DIRECTION_WRONGFUL_ACCEPTANCE,
  NATIVE_SCRIPT_DECODING_DIRECTION_WRONGFUL_REJECTION,
  NATIVE_SCRIPT_DECODING_REFUSAL_CLASS_MALFORMED,
  nativeScriptDecodingBoundDescriptorState,
  nativeScriptDecodingOpenedSubjectState,
  NativeScriptDecodingStep03AdvanceOrCloseSpendRedeemer,
  NativeScriptDecodingStep03BindDescriptorSpendRedeemer,
  NativeScriptDecodingStep03OpenSubjectDatum,
  NativeScriptDecodingStep03OpenSubjectSpendRedeemer,
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
  faultProofFieldOpening,
  planFaultProofFieldOpening,
  publishFaultProofFieldCarriage,
} from "../field-opening.js";
import {
  DEFAULT_CONFIRMATION_POLL_MS,
  type ResolvedProverSigner,
} from "../runtime.js";
import { excludeUtxo } from "../spend-input-witness.js";
import { selectFeeInput } from "../submit-step-01.js";
import { computationThreadOutputPredicate } from "../tx-layout.js";
import {
  type FraudProofPreSubmitBoundary,
  reachFraudProofPreSubmitBoundary,
  workflowReferenceScriptsUsedByTransaction,
} from "../workflow/transaction-boundary.js";
import type { NativeScriptDecodingContracts } from "./contracts.js";
import {
  buildNativeScriptDecodingChunkProof,
  buildNativeScriptDecodingLedgerMembership,
  classifyNativeScriptDecodingOutOfDomainFace,
  type NativeScriptDecodingLedgerTrieHandle,
  NativeScriptDecodingOutOfDomainFaces,
  nativeScriptDecodingScanArgsEvidence,
  nativeScriptDecodingSubjectFieldIndex,
  nativeScriptDecodingWindowProofs,
} from "./evidence.js";
import {
  NativeScriptDecodingPlanRoutes,
  type NativeScriptDecodingScanPlan,
  type NativeScriptDecodingScanSegmentPlan,
  type NativeScriptDecodingVerdictPlan,
} from "./scan-plan.js";
import {
  nativeScriptDecodingStepLabel,
  nativeScriptDecodingSubmitError,
  requireNativeScriptDecodingReferenceScript,
  requireNativeScriptDecodingStepState,
  requireNativeScriptDecodingThreadUtxo,
} from "./submit-common.js";

const OPEN_SUBJECT_INDEX = 2 as const;
const BIND_DESCRIPTOR_INDEX = 3 as const;
const ADVANCE_OR_CLOSE_INDEX = 4 as const;
const STEP_04_INDEX = 5 as const;
const OPEN_SUBJECT_LABEL = nativeScriptDecodingStepLabel(OPEN_SUBJECT_INDEX);

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
  readonly scanState: NativeScriptDecodingScanThreadState;
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
  stepIndex,
}: {
  readonly threadUtxo: UTxO;
  readonly signer: ResolvedProverSigner;
  readonly stepIndex: 2 | 3 | 4;
}): NativeScriptDecodingScanThreadState =>
  requireNativeScriptDecodingStepState({
    threadUtxo,
    signer,
    schema: NativeScriptDecodingStep03OpenSubjectDatum,
    stepIndex,
  });

const requirePreOpenState = (
  state: NativeScriptDecodingScanThreadState,
): void => {
  if (
    state.machine_state_hash !== "" ||
    state.refusal_class !== NATIVE_SCRIPT_DECODING_CLASS_PENDING ||
    state.outpoint_key_hash !== ""
  ) {
    throw nativeScriptDecodingSubmitError(
      "OpenSubject runs exactly once on step-02's sentinel state.",
    );
  }
};

const requireOpenedState = (
  state: NativeScriptDecodingScanThreadState,
): void => {
  if (
    state.outpoint_key_hash === "" ||
    state.output_index < 0n ||
    state.machine_state_hash !== "" ||
    state.refusal_class !== NATIVE_SCRIPT_DECODING_CLASS_PENDING
  ) {
    throw nativeScriptDecodingSubmitError(
      "BindDescriptor requires an opened, unbound subject state.",
    );
  }
};

const requireBoundPendingState = (
  state: NativeScriptDecodingScanThreadState,
): void => {
  if (
    state.machine_state_hash === "" ||
    state.refusal_class !== NATIVE_SCRIPT_DECODING_CLASS_PENDING
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
  const rebuilt = buildMidgardBoundedItem({
    fieldIndex: MIDGARD_LEDGER_OUTPUT_FIELD_INDEX,
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
  spendingStepIndex,
  buildRedeemer,
  carriageUtxos,
  referenceScriptUtxo,
  preSubmitBoundary,
  awaitConfirmation,
}: {
  readonly lucid: LucidEvolution;
  readonly contracts: NativeScriptDecodingContracts;
  readonly signer: ResolvedProverSigner;
  readonly threadUtxo: UTxO;
  readonly threadUnit: string;
  readonly destinationAddress: string;
  readonly nextState: NativeScriptDecodingScanThreadState;
  readonly spendingStepIndex: 2 | 3 | 4;
  readonly buildRedeemer: (layout: Step03Layout) => string;
  readonly carriageUtxos: readonly UTxO[];
  readonly referenceScriptUtxo: UTxO;
  readonly preSubmitBoundary?: FraudProofPreSubmitBoundary;
  readonly awaitConfirmation: boolean;
}): Promise<{ readonly txHash: string; readonly layout: Step03Layout }> => {
  const stepLabel = nativeScriptDecodingStepLabel(spendingStepIndex);
  signer.selectWallet(lucid);
  const walletUtxos = await lucid.wallet().getUtxos();
  const walletUtxosSansCarriage = carriageUtxos.reduce<readonly UTxO[]>(
    (candidates, utxo) => excludeUtxo(candidates, utxo),
    walletUtxos,
  );
  const feeInput = selectFeeInput(walletUtxosSansCarriage);
  const nextDatum = Data.to(
    { fraud_prover: signer.paymentKeyHash, data: nextState },
    NativeScriptDecodingStep03OpenSubjectDatum,
  );
  const outputMatches = computationThreadOutputPredicate({
    address: destinationAddress,
    datum: nextDatum,
    unit: threadUnit,
  });
  let resolvedLayout: Step03Layout | undefined;
  const redeemer = ((ctx) => {
    requireOwnSpendPurpose(ctx, threadUtxo, stepLabel);
    const layout: Step03Layout = {
      inputIndex: requireInputIndex(ctx, threadUtxo, stepLabel),
      outputIndex: requireUniqueOutputIndex(
        ctx.outputs,
        outputMatches,
        `${stepLabel} output`,
      ),
    };
    resolvedLayout = layout;
    return buildRedeemer(layout);
  }) satisfies BuildTxWithRedeemer;
  const threadAssets = {
    lovelace: threadUtxo.assets.lovelace ?? 0n,
    [threadUnit]: 1n,
  };

  const referenceInputs = [
    ...carriageUtxos,
    requireNativeScriptDecodingReferenceScript({
      utxo: referenceScriptUtxo,
      expectedScriptHash: contracts.steps[spendingStepIndex].spendingScriptHash,
      stepIndex: spendingStepIndex,
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
  const referenceRole =
    spendingStepIndex === OPEN_SUBJECT_INDEX
      ? "V1 fraud-proof native-script-decoding step-03 open-subject"
      : spendingStepIndex === BIND_DESCRIPTOR_INDEX
        ? "V1 fraud-proof native-script-decoding step-03 bind-descriptor"
        : "V1 fraud-proof native-script-decoding step-03 advance-or-close";
  const expectedTxHash = await reachFraudProofPreSubmitBoundary({
    signed,
    referenceScripts: workflowReferenceScriptsUsedByTransaction({
      signed,
      candidates: [
        {
          role: referenceRole,
          utxo: referenceScriptUtxo,
          expectedScript: contracts.steps[spendingStepIndex].spendingScript,
        },
      ],
    }),
    boundary: preSubmitBoundary,
  });
  const txHash = await signed.submit();
  if (txHash !== expectedTxHash) {
    throw nativeScriptDecodingSubmitError(
      `${stepLabel} provider returned ${txHash}, expected ${expectedTxHash}.`,
    );
  }
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
  readonly scanState: NativeScriptDecodingScanThreadState;
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

// ## OpenSubject

export const submitNativeScriptDecodingStep03OpenSubject = async ({
  lucid,
  contracts,
  categoryId,
  signer,
  threadOutRef,
  nativeTxCompactCbor,
  subjectFieldInputs,
  publishCarriage = false,
  publishedCarriageUtxos,
  certificateUtxo,
  referenceScriptUtxo,
  publicationPreSubmitBoundary,
  preSubmitBoundary,
  awaitConfirmation = true,
}: {
  readonly lucid: LucidEvolution;
  readonly contracts: NativeScriptDecodingContracts;
  readonly categoryId: string;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  /** Required whenever the accusation names a real field and non-negative ordinal. */
  readonly nativeTxCompactCbor?: string;
  readonly subjectFieldInputs?: readonly MidgardTxInput[];
  readonly publishCarriage?: boolean;
  readonly publishedCarriageUtxos?: readonly UTxO[];
  readonly certificateUtxo?: UTxO;
  readonly referenceScriptUtxo: UTxO;
  readonly publicationPreSubmitBoundary?: FraudProofPreSubmitBoundary;
  readonly preSubmitBoundary?: FraudProofPreSubmitBoundary;
  readonly awaitConfirmation?: boolean;
}): Promise<SubmitNativeScriptDecodingStep03Result> => {
  const { threadUtxo, threadToken } =
    await requireNativeScriptDecodingThreadUtxo({
      lucid,
      contracts,
      categoryId,
      stepIndex: OPEN_SUBJECT_INDEX,
      threadOutRef,
    });
  const state = requireStep03State({
    threadUtxo,
    signer,
    stepIndex: OPEN_SUBJECT_INDEX,
  });
  requirePreOpenState(state);

  const face = classifyNativeScriptDecodingOutOfDomainFace({
    outpointSourceKind: state.outpoint_source_kind,
    outpointCursor: state.outpoint_cursor,
    itemCount:
      subjectFieldInputs === undefined
        ? null
        : BigInt(subjectFieldInputs.length),
  });
  if (
    face !== null &&
    state.direction !== NATIVE_SCRIPT_DECODING_DIRECTION_WRONGFUL_REJECTION
  ) {
    throw nativeScriptDecodingSubmitError(
      "an out-of-domain accusation can close only for direction B.",
    );
  }

  const needsOpening =
    face === null || face === NativeScriptDecodingOutOfDomainFaces.CountFace;
  let subjectFieldOpening: FieldOpening | null = null;
  let carriageUtxos: readonly UTxO[] = [];
  if (needsOpening) {
    if (nativeTxCompactCbor === undefined || subjectFieldInputs === undefined) {
      throw nativeScriptDecodingSubmitError(
        "the accused pair names a field and non-negative ordinal; supply its compact transaction and complete field items.",
      );
    }
    const fieldIndex = nativeScriptDecodingSubjectFieldIndex(
      state.outpoint_source_kind,
    );
    const planned = planFaultProofFieldOpening({
      fieldIndex,
      anchorTxId: state.verified_tx_id,
      nativeTxCompactCbor,
      itemCbors: subjectFieldInputs.map(encodeMidgardTxInputCanonical),
      owner: signer.paymentKeyHash,
      publish: publishCarriage,
      label: `${OPEN_SUBJECT_LABEL} subject field`,
    });
    signer.selectWallet(lucid);
    const published =
      publishedCarriageUtxos ??
      (await publishFaultProofFieldCarriage({
        lucid,
        signer,
        planned,
        publisherAddress: signer.address,
        label: `${OPEN_SUBJECT_LABEL} subject field`,
        preSubmitBoundary: publicationPreSubmitBoundary,
      }));
    const stepReference = requireNativeScriptDecodingReferenceScript({
      utxo: referenceScriptUtxo,
      expectedScriptHash:
        contracts.steps[OPEN_SUBJECT_INDEX].spendingScriptHash,
      stepIndex: OPEN_SUBJECT_INDEX,
    });
    subjectFieldOpening = faultProofFieldOpening({
      planned,
      referenceInputs: [
        ...published,
        ...(certificateUtxo === undefined ? [] : [certificateUtxo]),
        stepReference,
      ],
      certificatePolicyId: contracts.fieldPreimageCertificatePolicyId,
      label: `${OPEN_SUBJECT_LABEL} subject field`,
    });
    carriageUtxos = [
      ...published,
      ...(certificateUtxo === undefined ? [] : [certificateUtxo]),
    ];
  }

  let nextState: NativeScriptDecodingScanThreadState;
  let destinationAddress: string;
  if (face === null) {
    if (subjectFieldInputs === undefined) {
      throw nativeScriptDecodingSubmitError(
        "an in-domain subject requires the complete field item list.",
      );
    }
    const subjectOutpoint = subjectFieldInputs[Number(state.outpoint_cursor)];
    if (subjectOutpoint === undefined) {
      throw nativeScriptDecodingSubmitError(
        "the accused ordinal is not present in the supplied field.",
      );
    }
    const outpointKeyCbor = Buffer.from(
      encodeMidgardTxInputCanonical(subjectOutpoint),
    ).toString("hex");
    nextState = await Effect.runPromise(
      nativeScriptDecodingOpenedSubjectState({
        state,
        outpointKeyBytes: outpointKeyCbor,
        outputIndex: subjectOutpoint.output_index,
      }),
    );
    destinationAddress =
      contracts.steps[BIND_DESCRIPTOR_INDEX].spendingScriptAddress;
  } else {
    nextState = {
      ...state,
      refusal_class: NATIVE_SCRIPT_DECODING_REFUSAL_CLASS_MALFORMED,
    };
    destinationAddress = contracts.steps[STEP_04_INDEX].spendingScriptAddress;
  }

  const opening = subjectFieldOpening;
  const { txHash, layout } = await advanceStep03Thread({
    lucid,
    contracts,
    signer,
    threadUtxo,
    threadUnit: threadToken.unit,
    destinationAddress,
    nextState,
    spendingStepIndex: OPEN_SUBJECT_INDEX,
    buildRedeemer: (resolved) =>
      Data.to(
        {
          Continue: [
            {
              input_index: resolved.inputIndex,
              output_index: resolved.outputIndex,
              subject_field_opening: opening,
            },
          ],
        },
        NativeScriptDecodingStep03OpenSubjectSpendRedeemer,
      ),
    carriageUtxos,
    referenceScriptUtxo,
    preSubmitBoundary,
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

// ## BindDescriptor

export const submitNativeScriptDecodingStep03BindDescriptor = async ({
  lucid,
  contracts,
  categoryId,
  signer,
  threadOutRef,
  outpointKeyCbor,
  descriptorCbor,
  ledgerTrie,
  plan,
  referenceScriptItemBytes,
  referenceScriptUtxo,
  preSubmitBoundary,
  awaitConfirmation = true,
}: {
  readonly lucid: LucidEvolution;
  readonly contracts: NativeScriptDecodingContracts;
  readonly categoryId: string;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  readonly outpointKeyCbor: string;
  readonly descriptorCbor: string;
  readonly ledgerTrie: NativeScriptDecodingLedgerTrieHandle;
  readonly plan?: NativeScriptDecodingScanPlan;
  readonly referenceScriptItemBytes?: Uint8Array;
  readonly referenceScriptUtxo: UTxO;
  readonly preSubmitBoundary?: FraudProofPreSubmitBoundary;
  readonly awaitConfirmation?: boolean;
}): Promise<SubmitNativeScriptDecodingStep03Result> => {
  const { threadUtxo, threadToken } =
    await requireNativeScriptDecodingThreadUtxo({
      lucid,
      contracts,
      categoryId,
      stepIndex: BIND_DESCRIPTOR_INDEX,
      threadOutRef,
    });
  const state = requireStep03State({
    threadUtxo,
    signer,
    stepIndex: BIND_DESCRIPTOR_INDEX,
  });
  requireOpenedState(state);

  const reopened = await Effect.runPromise(
    nativeScriptDecodingOpenedSubjectState({
      state,
      outpointKeyBytes: outpointKeyCbor,
      outputIndex: state.output_index,
    }),
  );
  if (reopened.outpoint_key_hash !== state.outpoint_key_hash) {
    throw nativeScriptDecodingSubmitError(
      "the supplied outpoint key is not the key committed by OpenSubject.",
    );
  }

  const descriptor: MidgardLedgerOutputCommitment =
    decodeMidgardLedgerOutputCommitment(Buffer.from(descriptorCbor, "hex"));
  if (BigInt(descriptor.outputIndex) !== state.output_index) {
    throw nativeScriptDecodingSubmitError(
      `the descriptor resolves output index ${descriptor.outputIndex.toString()}, but OpenSubject fixed ${state.output_index.toString()}.`,
    );
  }
  if (descriptor.totalLength <= 0) {
    throw nativeScriptDecodingSubmitError(
      "the descriptor commits a non-positive output item length.",
    );
  }
  const ledgerMembershipProof: Proof =
    await buildNativeScriptDecodingLedgerMembership({
      trie: ledgerTrie,
      outpointKey: Buffer.from(outpointKeyCbor, "hex"),
      priorLedgerRootHex: state.prior_ledger_root,
    });
  const bound = nativeScriptDecodingBoundDescriptorState({
    state,
    referenceScriptLanguage: BigInt(descriptor.referenceScriptLanguage),
    referenceScriptTotalLength: BigInt(descriptor.referenceScriptTotalLength),
    referenceScriptItemCommitment:
      descriptor.referenceScriptItemCommitment.toString("hex"),
  });

  let nextState: NativeScriptDecodingScanThreadState;
  let destinationAddress: string;
  let firstChunkProof: BoundedItemChunkProof | null;
  if (descriptor.referenceScriptLanguage === 0) {
    if (plan === undefined || referenceScriptItemBytes === undefined) {
      throw nativeScriptDecodingSubmitError(
        "a tag-0 descriptor needs the scan plan and reference-script item bytes.",
      );
    }
    requireAnchoredItemBytes({
      itemBytes: referenceScriptItemBytes,
      itemIndex: descriptor.outputIndex,
      totalLength: BigInt(descriptor.referenceScriptTotalLength),
      itemCommitmentHex:
        descriptor.referenceScriptItemCommitment.toString("hex"),
    });
    firstChunkProof = buildNativeScriptDecodingChunkProof({
      fieldIndex: MIDGARD_LEDGER_OUTPUT_FIELD_INDEX,
      itemIndex: descriptor.outputIndex,
      itemBytes: referenceScriptItemBytes,
      chunkIndex: 0,
    });
    if (plan.route === NativeScriptDecodingPlanRoutes.Machine) {
      const bindControl =
        plan.segments[0]?.controlBefore ?? plan.verdict.control;
      if (bindControl === null) {
        throw nativeScriptDecodingSubmitError(
          "the machine-route plan carries no bind control.",
        );
      }
      nextState = { ...bound, machine_state_hash: bindControl.hashHex };
      destinationAddress =
        contracts.steps[ADVANCE_OR_CLOSE_INDEX].spendingScriptAddress;
    } else if (plan.route === NativeScriptDecodingPlanRoutes.BindMalformed) {
      if (
        state.direction !== NATIVE_SCRIPT_DECODING_DIRECTION_WRONGFUL_ACCEPTANCE
      ) {
        throw nativeScriptDecodingSubmitError(
          "a malformed wrapper closes only a wrongful-acceptance claim.",
        );
      }
      nextState = {
        ...bound,
        refusal_class: NATIVE_SCRIPT_DECODING_REFUSAL_CLASS_MALFORMED,
      };
      destinationAddress = contracts.steps[STEP_04_INDEX].spendingScriptAddress;
    } else {
      throw nativeScriptDecodingSubmitError(
        "the plan claims a descriptor contradiction for a tag-0 descriptor.",
      );
    }
  } else {
    if (
      state.direction !== NATIVE_SCRIPT_DECODING_DIRECTION_WRONGFUL_REJECTION
    ) {
      throw nativeScriptDecodingSubmitError(
        "a non-tag-0 descriptor closes only a wrongful-rejection contradiction.",
      );
    }
    firstChunkProof = null;
    nextState = {
      ...bound,
      refusal_class: NATIVE_SCRIPT_DECODING_REFUSAL_CLASS_MALFORMED,
    };
    destinationAddress = contracts.steps[STEP_04_INDEX].spendingScriptAddress;
  }

  const proof = firstChunkProof;
  const { txHash, layout } = await advanceStep03Thread({
    lucid,
    contracts,
    signer,
    threadUtxo,
    threadUnit: threadToken.unit,
    destinationAddress,
    nextState,
    spendingStepIndex: BIND_DESCRIPTOR_INDEX,
    buildRedeemer: (resolved) =>
      Data.to(
        {
          Continue: [
            {
              input_index: resolved.inputIndex,
              output_index: resolved.outputIndex,
              outpoint_key_cbor: outpointKeyCbor,
              descriptor_cbor: descriptorCbor,
              ledger_membership_proof: ledgerMembershipProof,
              first_chunk_proof: proof,
            },
          ],
        },
        NativeScriptDecodingStep03BindDescriptorSpendRedeemer,
      ),
    carriageUtxos: [],
    referenceScriptUtxo,
    preSubmitBoundary,
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

// ## AdvanceOrClose

export const submitNativeScriptDecodingStep03AdvanceOrCloseSegment = async ({
  lucid,
  contracts,
  categoryId,
  signer,
  threadOutRef,
  segment,
  referenceScriptItemBytes,
  referenceScriptUtxo,
  preSubmitBoundary,
  awaitConfirmation = true,
}: {
  readonly lucid: LucidEvolution;
  readonly contracts: NativeScriptDecodingContracts;
  readonly categoryId: string;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  readonly segment: NativeScriptDecodingScanSegmentPlan;
  readonly referenceScriptItemBytes: Uint8Array;
  readonly referenceScriptUtxo: UTxO;
  readonly preSubmitBoundary?: FraudProofPreSubmitBoundary;
  readonly awaitConfirmation?: boolean;
}): Promise<SubmitNativeScriptDecodingStep03Result> => {
  const { threadUtxo, threadToken } =
    await requireNativeScriptDecodingThreadUtxo({
      lucid,
      contracts,
      categoryId,
      stepIndex: ADVANCE_OR_CLOSE_INDEX,
      threadOutRef,
    });
  const state = requireStep03State({
    threadUtxo,
    signer,
    stepIndex: ADVANCE_OR_CLOSE_INDEX,
  });
  requireBoundPendingState(state);
  if (segment.controlBefore.hashHex !== state.machine_state_hash) {
    throw nativeScriptDecodingSubmitError(
      "the segment's control is not the thread's committed machine.",
    );
  }
  requireAnchoredItemBytes({
    itemBytes: referenceScriptItemBytes,
    itemIndex: Number(state.output_index),
    totalLength: state.total_length,
    itemCommitmentHex: state.item_commitment,
  });
  const evidence = nativeScriptDecodingScanArgsEvidence({
    segment,
    fieldIndex: MIDGARD_LEDGER_OUTPUT_FIELD_INDEX,
    itemIndex: Number(state.output_index),
    itemBytes: referenceScriptItemBytes,
  });

  const closesTerminal =
    state.direction === NATIVE_SCRIPT_DECODING_DIRECTION_WRONGFUL_REJECTION &&
    isExactMidgardNativeScriptStructureTerminal(segment.controlAfter.control);
  const nextState: NativeScriptDecodingScanThreadState = closesTerminal
    ? {
        ...state,
        refusal_class: NATIVE_SCRIPT_DECODING_REFUSAL_CLASS_MALFORMED,
      }
    : { ...state, machine_state_hash: segment.controlAfter.hashHex };
  const destinationAddress = closesTerminal
    ? contracts.steps[STEP_04_INDEX].spendingScriptAddress
    : contracts.steps[ADVANCE_OR_CLOSE_INDEX].spendingScriptAddress;

  const { txHash, layout } = await advanceStep03Thread({
    lucid,
    contracts,
    signer,
    threadUtxo,
    threadUnit: threadToken.unit,
    destinationAddress,
    nextState,
    spendingStepIndex: ADVANCE_OR_CLOSE_INDEX,
    buildRedeemer: (resolved) =>
      Data.to(
        {
          Continue: [
            {
              input_index: resolved.inputIndex,
              output_index: resolved.outputIndex,
              control_cbor: evidence.control_cbor,
              chunk_proof: evidence.chunk_proof,
              next_chunk_proof: evidence.next_chunk_proof,
              frames: [...evidence.frames],
              step_budget: evidence.step_budget,
            },
          ],
        },
        NativeScriptDecodingStep03AdvanceOrCloseSpendRedeemer,
      ),
    carriageUtxos: [],
    referenceScriptUtxo,
    preSubmitBoundary,
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

export const submitNativeScriptDecodingStep03AdvanceOrCloseClose = async ({
  lucid,
  contracts,
  categoryId,
  signer,
  threadOutRef,
  verdict,
  referenceScriptItemBytes,
  referenceScriptUtxo,
  preSubmitBoundary,
  awaitConfirmation = true,
}: {
  readonly lucid: LucidEvolution;
  readonly contracts: NativeScriptDecodingContracts;
  readonly categoryId: string;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  readonly verdict: NativeScriptDecodingVerdictPlan;
  readonly referenceScriptItemBytes?: Uint8Array;
  readonly referenceScriptUtxo: UTxO;
  readonly preSubmitBoundary?: FraudProofPreSubmitBoundary;
  readonly awaitConfirmation?: boolean;
}): Promise<SubmitNativeScriptDecodingStep03Result> => {
  const { threadUtxo, threadToken } =
    await requireNativeScriptDecodingThreadUtxo({
      lucid,
      contracts,
      categoryId,
      stepIndex: ADVANCE_OR_CLOSE_INDEX,
      threadOutRef,
    });
  const state = requireStep03State({
    threadUtxo,
    signer,
    stepIndex: ADVANCE_OR_CLOSE_INDEX,
  });
  requireBoundPendingState(state);
  if (verdict.control === null) {
    throw nativeScriptDecodingSubmitError(
      "the close plan carries no machine control.",
    );
  }
  if (verdict.control.hashHex !== state.machine_state_hash) {
    throw nativeScriptDecodingSubmitError(
      "the close control is not the thread's committed machine.",
    );
  }

  let refusalClass: bigint;
  let stepBudget: bigint;
  let chunkProof: BoundedItemChunkProof | null = null;
  let nextChunkProof: BoundedItemChunkProof | null = null;
  if (state.direction === NATIVE_SCRIPT_DECODING_DIRECTION_WRONGFUL_REJECTION) {
    if (
      verdict.window !== null ||
      !isExactMidgardNativeScriptStructureTerminal(verdict.control.control)
    ) {
      throw nativeScriptDecodingSubmitError(
        "direction B closes only an exact, windowless terminal.",
      );
    }
    refusalClass = NATIVE_SCRIPT_DECODING_REFUSAL_CLASS_MALFORMED;
    stepBudget = 0n;
  } else {
    if (
      state.direction !==
        NATIVE_SCRIPT_DECODING_DIRECTION_WRONGFUL_ACCEPTANCE ||
      verdict.refusalClass === null
    ) {
      throw nativeScriptDecodingSubmitError(
        "direction A closes only with the planner's refusing primitive step.",
      );
    }
    refusalClass = BigInt(verdict.refusalClass);
    stepBudget = 1n;
    if (verdict.window !== null) {
      if (referenceScriptItemBytes === undefined) {
        throw nativeScriptDecodingSubmitError(
          "the refusing step reads a chunk window; supply the item bytes.",
        );
      }
      requireAnchoredItemBytes({
        itemBytes: referenceScriptItemBytes,
        itemIndex: Number(state.output_index),
        totalLength: state.total_length,
        itemCommitmentHex: state.item_commitment,
      });
      const proofs = nativeScriptDecodingWindowProofs({
        window: verdict.window,
        fieldIndex: MIDGARD_LEDGER_OUTPUT_FIELD_INDEX,
        itemIndex: Number(state.output_index),
        itemBytes: referenceScriptItemBytes,
      });
      chunkProof = proofs.chunk_proof;
      nextChunkProof = proofs.next_chunk_proof;
    }
  }

  const nextState: NativeScriptDecodingScanThreadState = {
    ...state,
    refusal_class: refusalClass,
  };
  const destinationAddress =
    contracts.steps[STEP_04_INDEX].spendingScriptAddress;
  const { txHash, layout } = await advanceStep03Thread({
    lucid,
    contracts,
    signer,
    threadUtxo,
    threadUnit: threadToken.unit,
    destinationAddress,
    nextState,
    spendingStepIndex: ADVANCE_OR_CLOSE_INDEX,
    buildRedeemer: (resolved) =>
      Data.to(
        {
          Continue: [
            {
              input_index: resolved.inputIndex,
              output_index: resolved.outputIndex,
              control_cbor: verdict.control!.cborHex,
              chunk_proof: chunkProof,
              next_chunk_proof: nextChunkProof,
              frames: [],
              step_budget: stepBudget,
            },
          ],
        },
        NativeScriptDecodingStep03AdvanceOrCloseSpendRedeemer,
      ),
    carriageUtxos: [],
    referenceScriptUtxo,
    preSubmitBoundary,
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
