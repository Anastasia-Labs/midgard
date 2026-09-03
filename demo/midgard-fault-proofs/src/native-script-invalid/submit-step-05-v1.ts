import { decodeMidgardVersionedScript } from "@al-ft/midgard-core";
import {
  NativeScriptInvalidStep05DatumSchema,
  NativeScriptInvalidStep05SpendRedeemerSchema,
  nativeScriptItemCommitment,
  type NativeScriptPushdownFrame,
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
  linearFaultStepLabel,
  requireLinearFaultReferenceScript,
  requireLinearFaultStepState,
  requireLinearFaultThreadUtxo,
} from "../linear-fault-family-v1.js";
import { submitLinearFaultFinalize } from "../linear-fault-finalize-v1.js";
import { submitLinearFaultContinue } from "../linear-fault-submit-v1.js";
import type { ResolvedProverSigner } from "../runtime.js";
import { computationThreadOutputPredicate } from "../tx-layout.js";
import type { FaultProofWitnessReferenceScripts } from "../witness-reference-scripts-v1.js";
import type { FraudProofPreSubmitBoundary } from "../workflow/transaction-boundary-v1.js";
import {
  NATIVE_SCRIPT_INVALID_CATEGORY_LABEL as FAMILY,
  type NativeScriptInvalidContracts,
} from "./contracts-v1.js";
import {
  NATIVE_SCRIPT_INVALID_NODE_BATCH,
  nativeScriptInvalidPushdownStep,
  nativeScriptInvalidSignerSet,
  resolveNativeScriptInvalidPushdownResume,
} from "./evidence-machine-v1.js";

type State = NonNullable<
  Data.Static<typeof NativeScriptInvalidStep05DatumSchema>["data"]
>;
type Datum = Data.Static<typeof NativeScriptInvalidStep05DatumSchema>;
const Datum = NativeScriptInvalidStep05DatumSchema as unknown as Datum;
type Redeemer = Data.Static<
  typeof NativeScriptInvalidStep05SpendRedeemerSchema
>;
const Redeemer =
  NativeScriptInvalidStep05SpendRedeemerSchema as unknown as Redeemer;

const samePeaks = (
  left: State["signer_peaks"],
  right: ReturnType<typeof nativeScriptInvalidSignerSet>["frontier"]["peaks"],
): boolean =>
  left.length === right.length &&
  left.every(
    (peak, index) =>
      peak.height === BigInt(right[index]!.height) &&
      peak.hash === Buffer.from(right[index]!.hash).toString("hex"),
  );

export const submitNativeScriptInvalidStep05 = async ({
  lucid,
  contracts,
  categoryId,
  signer,
  threadOutRef,
  scriptItemCbor,
  addressWitnessItems,
  cursorBytes,
  frames = [],
  nodeBudget = NATIVE_SCRIPT_INVALID_NODE_BATCH,
  referenceScriptUtxo,
  witnessReferenceScripts,
  preSubmitBoundary,
  awaitConfirmation = true,
}: {
  readonly lucid: LucidEvolution;
  readonly contracts: NativeScriptInvalidContracts;
  readonly categoryId: string;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  readonly scriptItemCbor: Uint8Array;
  readonly addressWitnessItems: readonly Uint8Array[];
  readonly cursorBytes?: Uint8Array;
  readonly frames?: readonly NativeScriptPushdownFrame[];
  readonly nodeBudget?: number;
  readonly referenceScriptUtxo: UTxO;
  readonly witnessReferenceScripts?: FaultProofWitnessReferenceScripts;
  readonly preSubmitBoundary?: FraudProofPreSubmitBoundary;
  readonly awaitConfirmation?: boolean;
}) => {
  const stepIndex = 4;
  const label = linearFaultStepLabel(FAMILY, stepIndex);
  const { threadUtxo, threadToken } = await requireLinearFaultThreadUtxo({
    lucid,
    contracts,
    categoryId,
    family: FAMILY,
    stepIndex,
    threadOutRef,
  });
  const state = requireLinearFaultStepState<State>({
    threadUtxo,
    signer,
    schema: Datum,
    family: FAMILY,
    stepIndex,
  });
  if (nativeScriptItemCommitment(scriptItemCbor) !== state.script_item_hash) {
    throw new Error(
      `${label}: script item does not match the thread commitment`,
    );
  }
  const script = decodeMidgardVersionedScript(scriptItemCbor);
  if (script.language !== "NativeCardano") {
    throw new Error(`${label}: selected witness is not a native script`);
  }
  const signerSet = nativeScriptInvalidSignerSet(addressWitnessItems);
  if (
    BigInt(signerSet.frontier.count) !== state.signer_count ||
    !samePeaks(state.signer_peaks, signerSet.frontier.peaks)
  ) {
    throw new Error(
      `${label}: signer preimage does not match the committed frontier`,
    );
  }
  const starts = state.phase === "ScriptReady";
  const committedCursorHash =
    starts || typeof state.phase !== "object" || state.phase === null
      ? undefined
      : state.phase.ScriptWalk.cursor_hash;
  const reconstructed =
    committedCursorHash === undefined || cursorBytes !== undefined
      ? undefined
      : resolveNativeScriptInvalidPushdownResume({
          scriptBytes: script.scriptBytes,
          validityIntervalStart: state.validity_interval_start,
          validityIntervalEnd: state.validity_interval_end,
          signerSet,
          committedCursorHash,
          nodeBudget,
        });
  const transition = nativeScriptInvalidPushdownStep({
    scriptBytes: script.scriptBytes,
    validityIntervalStart: state.validity_interval_start,
    validityIntervalEnd: state.validity_interval_end,
    signerSet,
    nodeBudget,
    ...(committedCursorHash === undefined
      ? {}
      : {
          committedCursorHash,
          cursorBytes: cursorBytes ?? reconstructed!.cursorBytes,
          frames: cursorBytes === undefined ? reconstructed!.frames : frames,
        }),
  });
  const signerQueries = transition.signerHashes.map((signerHash) => ({
    signer_hash: signerHash,
    proof: signerSet.proofFor(Buffer.from(signerHash, "hex")),
  }));
  if (transition.complete && transition.satisfied !== false) {
    throw new Error(`${label}: native script does not evaluate to false`);
  }
  const common = {
    script_item_cbor: Buffer.from(scriptItemCbor).toString("hex"),
    node_budget: BigInt(nodeBudget),
    signer_queries: signerQueries,
  };
  if (transition.complete) {
    const result = await submitLinearFaultFinalize({
      lucid,
      family: FAMILY,
      stepIndex,
      step: contracts.steps[stepIndex],
      computationThread: contracts.computationThread,
      fraudProof: contracts.fraudProof,
      signer,
      threadUtxo,
      threadToken,
      spendRedeemerSchema: Redeemer,
      buildFamilyArgs: (layout) =>
        starts
          ? {
              StartScriptFinalize: {
                input_index: layout.inputIndex,
                output_index: layout.outputIndex,
                fraud_proof_mint_redeemer_index:
                  layout.fraudProofMintRedeemerIndex,
                ...common,
              },
            }
          : {
              FinalizeScriptScan: {
                input_index: layout.inputIndex,
                output_index: layout.outputIndex,
                fraud_proof_mint_redeemer_index:
                  layout.fraudProofMintRedeemerIndex,
                ...common,
                cursor_bytes: transition.currentCursorBytes!,
                frames: [...transition.currentFrames],
              },
            },
      referenceScriptUtxo,
      witnessReferenceScripts,
      preSubmitBoundary,
      awaitConfirmation,
    });
    return {
      ...result,
      action: starts ? "StartScriptFinalize" : "FinalizeScriptScan",
      cursorBytes: transition.nextCursorBytes,
      cursorHash: transition.nextCursorHash,
      frames: transition.nextFrames,
    } as const;
  }

  signer.selectWallet(lucid);
  const stepReference = requireLinearFaultReferenceScript({
    utxo: referenceScriptUtxo,
    expectedScriptHash: contracts.steps[stepIndex].spendingScriptHash,
    family: FAMILY,
    stepIndex,
  });
  const nextDatum = Data.to(
    {
      fraud_prover: signer.paymentKeyHash,
      data: {
        ...state,
        phase: { ScriptWalk: { cursor_hash: transition.nextCursorHash } },
      },
    },
    Datum,
  );
  const outputMatches = computationThreadOutputPredicate({
    address: contracts.steps[stepIndex].spendingScriptAddress,
    datum: nextDatum,
    unit: threadToken.unit,
  });
  let outputIndex: bigint | undefined;
  const spendRedeemer = ((ctx) => {
    requireOwnSpendPurpose(ctx, threadUtxo, label);
    const inputIndex = requireInputIndex(ctx, threadUtxo, label);
    outputIndex = requireUniqueOutputIndex(ctx.outputs, outputMatches, label);
    return Data.to(
      {
        Continue: [
          starts
            ? {
                StartScriptScan: {
                  input_index: inputIndex,
                  output_index: outputIndex,
                  ...common,
                },
              }
            : {
                ResumeScriptScan: {
                  input_index: inputIndex,
                  output_index: outputIndex,
                  ...common,
                  cursor_bytes: transition.currentCursorBytes!,
                  frames: [...transition.currentFrames],
                },
              },
        ],
      },
      Redeemer,
    );
  }) satisfies BuildTxWithRedeemer;
  const txHash = await submitLinearFaultContinue({
    lucid,
    signerPaymentKeyHash: signer.paymentKeyHash,
    threadUtxo,
    threadUnit: threadToken.unit,
    stepReference,
    stepScript: contracts.steps[stepIndex].spendingScript,
    stepRole: label,
    nextAddress: contracts.steps[stepIndex].spendingScriptAddress,
    nextDatum,
    redeemer: spendRedeemer,
    preSubmitBoundary,
    awaitConfirmation,
  });
  if (outputIndex === undefined) throw new Error(`${label}: unresolved layout`);
  return {
    txHash,
    action: starts ? "StartScriptScan" : "ResumeScriptScan",
    nextThreadOutRef: `${txHash}#${outputIndex.toString()}`,
    cursorBytes: transition.nextCursorBytes,
    cursorHash: transition.nextCursorHash,
    frames: transition.nextFrames,
  } as const;
};
