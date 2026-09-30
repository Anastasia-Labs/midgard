import {
  buildMidgardBoundedItem,
  MIDGARD_LEDGER_OUTPUT_FIELD_INDEX,
} from "@al-ft/midgard-core";
import type { NativeScriptDecodingScanThreadState } from "@al-ft/midgard-sdk";
import {
  NATIVE_SCRIPT_DECODING_CLASS_PENDING,
  NativeScriptDecodingStep03OpenSubjectDatum,
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

import {
  DEFAULT_CONFIRMATION_POLL_MS,
  type ResolvedProverSigner,
} from "../runtime.js";
import { excludeUtxo } from "../spend-input-witness.js";
import { selectFeeInput } from "../step-support.js";
import { computationThreadOutputPredicate } from "../tx-layout.js";
import {
  type FraudProofPreSubmitBoundary,
  reachFraudProofPreSubmitBoundary,
  workflowReferenceScriptsUsedByTransaction,
} from "../workflow/transaction-boundary.js";
import type { NativeScriptDecodingContracts } from "./contracts.js";
import {
  nativeScriptDecodingStepLabel,
  nativeScriptDecodingSubmitError,
  requireNativeScriptDecodingReferenceScript,
  requireNativeScriptDecodingStepState,
} from "./submit-common.js";

export const OPEN_SUBJECT_INDEX = 2 as const;

export const BIND_DESCRIPTOR_INDEX = 3 as const;

export const ADVANCE_OR_CLOSE_INDEX = 4 as const;

export const STEP_04_INDEX = 5 as const;

export const OPEN_SUBJECT_LABEL =
  nativeScriptDecodingStepLabel(OPEN_SUBJECT_INDEX);

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

export const requireStep03State = ({
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

export const requirePreOpenState = (
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

export const requireOpenedState = (
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

export const requireBoundPendingState = (
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
export const requireAnchoredItemBytes = ({
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
export const advanceStep03Thread = async ({
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

export const step03Result = ({
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
