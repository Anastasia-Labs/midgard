/**
 * `native-script-decoding` step-02 submitter (offchain plan §4.2).
 *
 * Binds the thread to the committed claim. The disputed header rides the
 * redeemer (there are no reference inputs at all in this transaction), the
 * event's transition step and event→step leaf are opened from the
 * transition-trace reconstruction, and — for forced threads — the forced
 * verdict leaf is opened as the disputed authority. Every check the
 * validator makes that this process can make locally is made locally first,
 * so a doomed transaction is refused before it costs anything:
 *
 * - the reconstruction's header must hash to the thread NFT's asset-name
 *   tail (blake2b-224 of the serialised header Data);
 * - a normal-source thread must be direction A; a forced thread's leaf
 *   verdict must match the disputed direction (`ForcedTxValid` for A,
 *   `ForcedTxInvalid` with one of the three decoding arms for B);
 * - direction B's accused pair and refusal class are copied verbatim from
 *   the leaf's accusation via the `scan_accusation_of_v1` twin — the
 *   caller's chosen pair is refused there, because the validator ignores it;
 * - direction A's prover-chosen pair must be in `{spend, reference} × ℕ`.
 */
import type {
  EventKey,
  NativeScriptDecodingBindStateV1,
  NativeScriptDecodingScanThreadStateV1,
  OutputReference,
} from "@al-ft/midgard-sdk";
import {
  hashHexWithBlake2b,
  HeaderV1,
  NATIVE_SCRIPT_DECODING_CLASS_PENDING_V1,
  NATIVE_SCRIPT_DECODING_DIRECTION_WRONGFUL_ACCEPTANCE_V1,
  NATIVE_SCRIPT_DECODING_DIRECTION_WRONGFUL_REJECTION_V1,
  NATIVE_SCRIPT_DECODING_OUTPOINT_SOURCE_REFERENCE_V1,
  NATIVE_SCRIPT_DECODING_OUTPOINT_SOURCE_SPEND_V1,
  NATIVE_SCRIPT_DECODING_SOURCE_KIND_FORCED_V1,
  NATIVE_SCRIPT_DECODING_SOURCE_KIND_NORMAL_V1,
  nativeScriptDecodingPreBindScanStateV1,
  NativeScriptDecodingStep02Datum,
  NativeScriptDecodingStep02SpendRedeemer,
  NativeScriptDecodingStep03OpenSubjectDatum,
  OutputReference as OutputReferenceSchema,
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
  DEFAULT_CONFIRMATION_POLL_MS,
  type ResolvedProverSigner,
} from "../runtime.js";
import { selectFeeInput } from "../submit-step-01.js";
import type { TransitionTraceReconstruction } from "../transition-trace/reconstruct.js";
import { computationThreadOutputPredicate } from "../tx-layout.js";
import {
  type FraudProofPreSubmitBoundaryV1,
  reachFraudProofPreSubmitBoundaryV1,
  workflowReferenceScriptsUsedByTransactionV1,
} from "../workflow/transaction-boundary-v1.js";
import type { NativeScriptDecodingContractsV1 } from "./contracts-v1.js";
import {
  buildNativeScriptDecodingStep02EvidenceV1,
  nativeScriptDecodingScanAccusationOfV1,
} from "./evidence-v1.js";
import {
  nativeScriptDecodingStepLabelV1,
  nativeScriptDecodingSubmitError,
  requireNativeScriptDecodingReferenceScriptV1,
  requireNativeScriptDecodingStepStateV1,
  requireNativeScriptDecodingThreadUtxoV1,
} from "./submit-common-v1.js";

const STEP_LABEL = nativeScriptDecodingStepLabelV1(1);

/** Direction A's prover-chosen accused pair. */
export type NativeScriptDecodingChosenOutpointV1 = {
  readonly sourceKind: bigint;
  readonly cursor: bigint;
};

export type SubmitNativeScriptDecodingStep02Result = {
  readonly txHash: string;
  readonly walletSource: string;
  readonly proverAddress: string;
  readonly fraudProver: string;
  readonly threadOutRef: string;
  readonly nextThreadOutRef: string;
  readonly fraudulentHeaderHash: string;
  readonly computationThreadUnit: string;
  readonly thirdStepAddress: string;
  /** The pre-bind `ScanThreadStateV1` the thread now carries. */
  readonly scanState: NativeScriptDecodingScanThreadStateV1;
  readonly inputIndex: number;
  readonly outputIndex: number;
  readonly awaitedConfirmation: boolean;
};

export const submitNativeScriptDecodingStep02 = async ({
  lucid,
  contracts,
  categoryId,
  signer,
  threadOutRef,
  reconstruction,
  forcedOrderKey,
  chosenOutpoint,
  referenceScriptUtxo,
  preSubmitBoundary,
  awaitConfirmation = true,
}: {
  readonly lucid: LucidEvolution;
  readonly contracts: NativeScriptDecodingContractsV1;
  readonly categoryId: string;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  /** The disputed block's transition-trace reconstruction. */
  readonly reconstruction: TransitionTraceReconstruction;
  /** Forced threads: the disputed forced transaction's order key. */
  readonly forcedOrderKey?: OutputReference;
  /**
   * Direction A: the prover-chosen accused `(source_kind, ordinal)` pair.
   * Must be absent for direction B, where the accusation names the pair.
   */
  readonly chosenOutpoint?: NativeScriptDecodingChosenOutpointV1;
  /** Q3: the mandatory published step-02 reference script. */
  readonly referenceScriptUtxo: UTxO;
  readonly preSubmitBoundary?: FraudProofPreSubmitBoundaryV1;
  readonly awaitConfirmation?: boolean;
}): Promise<SubmitNativeScriptDecodingStep02Result> => {
  const { threadUtxo, threadToken } =
    await requireNativeScriptDecodingThreadUtxoV1({
      lucid,
      contracts,
      categoryId,
      stepIndex: 1,
      threadOutRef,
    });
  const bindState: NativeScriptDecodingBindStateV1 =
    requireNativeScriptDecodingStepStateV1({
      threadUtxo,
      signer,
      schema: NativeScriptDecodingStep02Datum,
      stepIndex: 1,
    });
  const { direction, source_kind: sourceKind } = bindState;

  // The header must be the thread NFT's: category id ‖ blake2b-224(header).
  const headerHash = await Effect.runPromise(
    hashHexWithBlake2b(Data.to(reconstruction.header, HeaderV1), 28),
  );
  if (headerHash !== threadToken.fraudulentHeaderHash) {
    throw nativeScriptDecodingSubmitError(
      `the reconstruction's header hashes to ${headerHash}, not the thread NFT's disputed header ${threadToken.fraudulentHeaderHash}.`,
    );
  }

  const requireChosenPair = (): NativeScriptDecodingChosenOutpointV1 => {
    if (chosenOutpoint === undefined) {
      throw nativeScriptDecodingSubmitError(
        "direction A needs the prover-chosen accused pair (--chosen-outpoint).",
      );
    }
    if (
      (chosenOutpoint.sourceKind !==
        NATIVE_SCRIPT_DECODING_OUTPOINT_SOURCE_SPEND_V1 &&
        chosenOutpoint.sourceKind !==
          NATIVE_SCRIPT_DECODING_OUTPOINT_SOURCE_REFERENCE_V1) ||
      chosenOutpoint.cursor < 0n
    ) {
      throw nativeScriptDecodingSubmitError(
        `chosen pair (${chosenOutpoint.sourceKind.toString()}, ${chosenOutpoint.cursor.toString()}) is outside {spend, reference} × non-negative ordinals.`,
      );
    }
    return chosenOutpoint;
  };

  let eventKey: EventKey;
  if (sourceKind === NATIVE_SCRIPT_DECODING_SOURCE_KIND_NORMAL_V1) {
    if (direction !== NATIVE_SCRIPT_DECODING_DIRECTION_WRONGFUL_ACCEPTANCE_V1) {
      throw nativeScriptDecodingSubmitError(
        "a normal-source thread is structurally direction A; the thread state disagrees.",
      );
    }
    if (forcedOrderKey !== undefined) {
      throw nativeScriptDecodingSubmitError(
        "a normal-source thread binds no forced leaf; drop --forced-order-key.",
      );
    }
    eventKey = { L2TransactionEventKey: { tx_id: bindState.verified_tx_id } };
  } else {
    if (sourceKind !== NATIVE_SCRIPT_DECODING_SOURCE_KIND_FORCED_V1) {
      throw nativeScriptDecodingSubmitError(
        `thread state carries source kind ${sourceKind.toString()}, outside {normal, forced}.`,
      );
    }
    if (bindState.verified_tx_id !== "") {
      throw nativeScriptDecodingSubmitError(
        "a forced thread reaches step 02 with the empty verified_tx_id sentinel; the thread state disagrees.",
      );
    }
    if (forcedOrderKey === undefined) {
      throw nativeScriptDecodingSubmitError(
        "a forced thread needs the disputed forced transaction's order key (--forced-order-key).",
      );
    }
    eventKey = { ForcedTransactionEventKey: { tx_order_id: forcedOrderKey } };
  }

  const evidence = await buildNativeScriptDecodingStep02EvidenceV1({
    reconstruction,
    eventKey,
  });
  const priorLedgerRoot =
    evidence.transitionStepMembership.value.pre_utxos_root;

  let scanState: NativeScriptDecodingScanThreadStateV1;
  if (evidence.forcedMembership === null) {
    const pair = requireChosenPair();
    scanState = nativeScriptDecodingPreBindScanStateV1({
      direction,
      sourceKind,
      verifiedTxId: bindState.verified_tx_id,
      txOrderId: "",
      scanReasonClass: NATIVE_SCRIPT_DECODING_CLASS_PENDING_V1,
      priorLedgerRoot,
      outpointSourceKind: pair.sourceKind,
      outpointCursor: pair.cursor,
    });
  } else {
    const leaf = evidence.forcedMembership.value;
    const txOrderId = Data.to(
      evidence.forcedMembership.key,
      OutputReferenceSchema,
    );
    if (direction === NATIVE_SCRIPT_DECODING_DIRECTION_WRONGFUL_ACCEPTANCE_V1) {
      if (leaf.verdict !== "ForcedTxValid") {
        throw nativeScriptDecodingSubmitError(
          "direction A disputes an explicit acceptance, but the forced leaf's verdict is a rejection.",
        );
      }
      const pair = requireChosenPair();
      scanState = nativeScriptDecodingPreBindScanStateV1({
        direction,
        sourceKind,
        verifiedTxId: leaf.tx_id,
        txOrderId,
        scanReasonClass: NATIVE_SCRIPT_DECODING_CLASS_PENDING_V1,
        priorLedgerRoot,
        outpointSourceKind: pair.sourceKind,
        outpointCursor: pair.cursor,
      });
    } else {
      if (
        direction !== NATIVE_SCRIPT_DECODING_DIRECTION_WRONGFUL_REJECTION_V1
      ) {
        throw nativeScriptDecodingSubmitError(
          `thread state carries direction ${direction.toString()}, outside {0, 1}.`,
        );
      }
      if (leaf.verdict === "ForcedTxValid") {
        throw nativeScriptDecodingSubmitError(
          "direction B disputes an explicit rejection, but the forced leaf's verdict is an acceptance.",
        );
      }
      if (chosenOutpoint !== undefined) {
        throw nativeScriptDecodingSubmitError(
          "direction B copies the accused pair verbatim from the leaf's accusation; drop --chosen-outpoint.",
        );
      }
      const accusation = nativeScriptDecodingScanAccusationOfV1(
        leaf.verdict.ForcedTxInvalid.reason,
      );
      scanState = nativeScriptDecodingPreBindScanStateV1({
        direction,
        sourceKind,
        verifiedTxId: leaf.tx_id,
        txOrderId,
        scanReasonClass: accusation.scanReasonClass,
        priorLedgerRoot,
        outpointSourceKind: accusation.outpointSourceKind,
        outpointCursor: accusation.outpointCursor,
      });
    }
  }

  signer.selectWallet(lucid);
  const feeInput = selectFeeInput(await lucid.wallet().getUtxos());
  const step03Datum = Data.to(
    { fraud_prover: signer.paymentKeyHash, data: scanState },
    NativeScriptDecodingStep03OpenSubjectDatum,
  );
  const step03OutputMatches = computationThreadOutputPredicate({
    address: contracts.steps[2].spendingScriptAddress,
    datum: step03Datum,
    unit: threadToken.unit,
  });
  let resolvedLayout:
    | { readonly inputIndex: bigint; readonly outputIndex: bigint }
    | undefined;
  const redeemer = ((ctx) => {
    requireOwnSpendPurpose(ctx, threadUtxo, STEP_LABEL);
    const layout = {
      inputIndex: requireInputIndex(ctx, threadUtxo, STEP_LABEL),
      outputIndex: requireUniqueOutputIndex(
        ctx.outputs,
        step03OutputMatches,
        `${STEP_LABEL} output`,
      ),
    };
    resolvedLayout = layout;
    return Data.to(
      {
        Continue: [
          {
            input_index: layout.inputIndex,
            output_index: layout.outputIndex,
            header: reconstruction.header,
            event_to_step_membership: evidence.eventToStepMembership,
            transition_step_membership: evidence.transitionStepMembership,
            forced_membership: evidence.forcedMembership,
            chosen_outpoint_source_kind: scanState.outpoint_source_kind,
            chosen_outpoint_cursor: scanState.outpoint_cursor,
          },
        ],
      },
      NativeScriptDecodingStep02SpendRedeemer,
    );
  }) satisfies BuildTxWithRedeemer;
  const threadAssets = {
    lovelace: threadUtxo.assets.lovelace ?? 0n,
    [threadToken.unit]: 1n,
  };

  const base = lucid
    .newTx()
    .collectFrom([feeInput])
    .collectFrom([threadUtxo], redeemer)
    .pay.ToContract(
      contracts.steps[2].spendingScriptAddress,
      { kind: "inline", value: step03Datum },
      threadAssets,
    )
    .addSignerKey(signer.paymentKeyHash);
  const tx = base.readFrom([
    requireNativeScriptDecodingReferenceScriptV1({
      utxo: referenceScriptUtxo,
      expectedScriptHash: contracts.steps[1].spendingScriptHash,
      stepIndex: 1,
    }),
  ]);

  const unsigned = await tx.complete({ localUPLCEval: true });
  if (resolvedLayout === undefined) {
    throw nativeScriptDecodingSubmitError(
      "BuildTxWithRedeemer did not resolve the step-02 layout.",
    );
  }
  const signed = await unsigned.sign.withWallet().complete();
  const expectedTxHash = await reachFraudProofPreSubmitBoundaryV1({
    signed,
    referenceScripts: workflowReferenceScriptsUsedByTransactionV1({
      signed,
      candidates: [
        {
          role: "V1 fraud-proof native-script-decoding step-02",
          utxo: referenceScriptUtxo,
          expectedScript: contracts.steps[1].spendingScript,
        },
      ],
    }),
    boundary: preSubmitBoundary,
  });
  const txHash = await signed.submit();
  if (txHash !== expectedTxHash) {
    throw nativeScriptDecodingSubmitError(
      `step-02 provider returned ${txHash}, expected ${expectedTxHash}.`,
    );
  }
  if (awaitConfirmation) {
    await lucid.awaitTx(txHash, DEFAULT_CONFIRMATION_POLL_MS);
  }

  return {
    txHash,
    walletSource: signer.source,
    proverAddress: signer.address,
    fraudProver: signer.paymentKeyHash,
    threadOutRef,
    nextThreadOutRef: `${txHash}#${resolvedLayout.outputIndex.toString()}`,
    fraudulentHeaderHash: threadToken.fraudulentHeaderHash,
    computationThreadUnit: threadToken.unit,
    thirdStepAddress: contracts.steps[2].spendingScriptAddress,
    scanState,
    inputIndex: Number(resolvedLayout.inputIndex),
    outputIndex: Number(resolvedLayout.outputIndex),
    awaitedConfirmation: awaitConfirmation,
  };
};
