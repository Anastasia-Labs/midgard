import {
  type FieldOpening,
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
  requireLinearFaultReferenceScript,
  requireLinearFaultStepState,
  requireLinearFaultThreadUtxo,
} from "../../src/linear-fault-family.js";
import { submitLinearFaultFinalize } from "../../src/linear-fault-finalize.js";
import { submitLinearFaultContinue } from "../../src/linear-fault-submit.js";
import type { ObserverOrderInvalidContracts } from "../../src/observer-order-invalid/contracts.js";
import { type ObserverOrderInvalidEvidence } from "../../src/observer-order-invalid/family.js";
import {
  ObserverOrderInvalidStep03DatumSchema,
  ObserverOrderInvalidStep03RedeemerSchema,
  ObserverOrderInvalidStep04DatumSchema,
  ObserverOrderInvalidStep04RedeemerSchema,
} from "../../src/observer-order-invalid/schemas.js";
import {
  encodeObserverOrderWalkCheckpoint,
  hashObserverOrderWalkCheckpoint,
  type ObserverOrderInvalidStagedPlan,
  observerOrderPrefix,
} from "../../src/observer-order-invalid/staged-plan.js";
import type { ResolvedProverSigner } from "../../src/runtime.js";
import { computationThreadOutputPredicate } from "../../src/tx-layout.js";
import type { FaultProofWitnessReferenceScripts } from "../../src/witness-reference-scripts.js";
import {
  FAMILY,
  type OpeningMutation,
  resolveObserverOpening,
} from "./observer-order-invalid-raw.resolve-observer-opening.js";
import { type ObserverOrderScanSuccessor } from "./observer-order-invalid-raw.submit-observer-order-invalid-step02-raw.js";

/**
 * Step 03 with every prover-supplied value exposed: the resumed checkpoint
 * bytes, the item budget, the successor state and the successor script.
 * Defaults reproduce the production builder for `walkOrdinal`.
 */
export const submitObserverOrderInvalidStep03Raw = async ({
  lucid,
  contracts,
  categoryId,
  signer,
  threadOutRef,
  evidence,
  nativeTxCompactCbor,
  staged,
  walkOrdinal,
  referenceScriptUtxo,
  checkpointBytesHex,
  itemBudget,
  successor,
  nextStepIndex,
  mutateOpening = (opening) => opening,
}: {
  readonly lucid: LucidEvolution;
  readonly contracts: ObserverOrderInvalidContracts;
  readonly categoryId: string;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  readonly evidence: ObserverOrderInvalidEvidence;
  readonly nativeTxCompactCbor: string;
  readonly staged: ObserverOrderInvalidStagedPlan;
  readonly walkOrdinal: number;
  readonly referenceScriptUtxo: UTxO;
  readonly checkpointBytesHex?: string;
  readonly itemBudget?: bigint;
  readonly successor?: ObserverOrderScanSuccessor;
  readonly nextStepIndex?: 2 | 3;
  readonly mutateOpening?: OpeningMutation;
}) => {
  const nextCheckpoint = staged.walk[walkOrdinal];
  if (nextCheckpoint === undefined)
    throw new Error(`${FAMILY} raw: walk ordinal is outside plan`);
  const priorCheckpoint =
    walkOrdinal === 0 ? staged.initialWalk : staged.walk[walkOrdinal - 1]!;
  const stepIndex = 2;
  const { threadUtxo, threadToken } = await requireLinearFaultThreadUtxo({
    lucid,
    contracts,
    categoryId,
    family: FAMILY,
    stepIndex,
    threadOutRef,
  });
  requireLinearFaultStepState<Record<string, unknown>>({
    threadUtxo,
    signer,
    schema: ObserverOrderInvalidStep03DatumSchema as never,
    family: FAMILY,
    stepIndex,
  });
  const stepReference = requireLinearFaultReferenceScript({
    utxo: referenceScriptUtxo,
    expectedScriptHash: contracts.steps[2].spendingScriptHash,
    family: FAMILY,
    stepIndex,
  });
  const { opening, carriageUtxos, extraReferenceInputs } =
    await resolveObserverOpening({
      lucid,
      contracts,
      signer,
      evidence,
      nativeTxCompactCbor,
      stepReference,
      extraReferenceInputs: [],
      mutateOpening,
      label: `${FAMILY} raw scan field 3`,
    });
  const terminal = walkOrdinal === staged.walk.length - 1;
  const defaultSuccessor = (): ObserverOrderScanSuccessor => {
    if (terminal) return { kind: "decision", violation: evidence.violation };
    const prefix = observerOrderPrefix({
      items: staged.items,
      nextItemIndex: nextCheckpoint.nextItemIndex,
      observerIndex: evidence.observerIndex,
    });
    return {
      kind: "scan",
      checkpointHash: hashObserverOrderWalkCheckpoint(nextCheckpoint),
      seen: BigInt(prefix.seen),
      previousObserver: prefix.previousObserver,
    };
  };
  const chosen = successor ?? defaultSuccessor();
  const nextData =
    chosen.kind === "decision"
      ? {
          subject: evidence.subject,
          observer_index: BigInt(evidence.observerIndex),
          violation: chosen.violation,
        }
      : {
          subject: evidence.subject,
          observer_index: BigInt(evidence.observerIndex),
          checkpoint_hash: chosen.checkpointHash,
          seen: chosen.seen,
          previous_observer: chosen.previousObserver,
          outcome: 0n,
        };
  const nextDatum = Data.to(
    { fraud_prover: signer.paymentKeyHash, data: nextData } as never,
    (chosen.kind === "decision"
      ? ObserverOrderInvalidStep04DatumSchema
      : ObserverOrderInvalidStep03DatumSchema) as never,
  );
  const nextStep =
    contracts.steps[nextStepIndex ?? (chosen.kind === "decision" ? 3 : 2)];
  const outputMatches = computationThreadOutputPredicate({
    address: nextStep.spendingScriptAddress,
    datum: nextDatum,
    unit: threadToken.unit,
  });
  const redeemer = ((ctx) => {
    requireOwnSpendPurpose(ctx, threadUtxo, `${FAMILY} raw step-03`);
    return Data.to(
      {
        Continue: [
          {
            input_index: requireInputIndex(ctx, threadUtxo, FAMILY),
            output_index: requireUniqueOutputIndex(
              ctx.outputs,
              outputMatches,
              `${FAMILY} raw step-03 output`,
            ),
            opening,
            checkpoint_bytes:
              checkpointBytesHex ??
              encodeObserverOrderWalkCheckpoint(priorCheckpoint).toString(
                "hex",
              ),
            item_budget:
              itemBudget ??
              BigInt(
                Math.max(
                  1,
                  nextCheckpoint.nextItemIndex - priorCheckpoint.nextItemIndex,
                ),
              ),
          },
        ],
      } as never,
      ObserverOrderInvalidStep03RedeemerSchema as never,
    );
  }) satisfies BuildTxWithRedeemer;
  return await submitLinearFaultContinue({
    lucid,
    signerPaymentKeyHash: signer.paymentKeyHash,
    threadUtxo,
    threadUnit: threadToken.unit,
    stepReference,
    stepScript: contracts.steps[2].spendingScript,
    stepRole: `${FAMILY} raw step-03 walk ${walkOrdinal.toString()}`,
    nextAddress: nextStep.spendingScriptAddress,
    nextDatum,
    redeemer,
    carriageUtxos,
    extraReferenceInputs,
    awaitConfirmation: true,
  });
};

/**
 * Step 04 without the off-chain `observerOrderInvalidEvidenceCloses` and
 * datum/evidence guards: an honest decision reaches
 * `terminal_contradiction_v1` and is refused there.
 */
export const submitObserverOrderInvalidStep04Raw = async ({
  lucid,
  contracts,
  categoryId,
  signer,
  threadOutRef,
  referenceScriptUtxo,
  witnessReferenceScripts,
}: {
  readonly lucid: LucidEvolution;
  readonly contracts: ObserverOrderInvalidContracts;
  readonly categoryId: string;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  readonly referenceScriptUtxo: UTxO;
  readonly witnessReferenceScripts: FaultProofWitnessReferenceScripts;
}) => {
  const stepIndex = 3;
  const { threadUtxo, threadToken } = await requireLinearFaultThreadUtxo({
    lucid,
    contracts,
    categoryId,
    family: FAMILY,
    stepIndex,
    threadOutRef,
  });
  requireLinearFaultStepState<Record<string, unknown>>({
    threadUtxo,
    signer,
    schema: ObserverOrderInvalidStep04DatumSchema as never,
    family: FAMILY,
    stepIndex,
  });
  return await submitLinearFaultFinalize({
    lucid,
    family: FAMILY,
    stepIndex,
    step: contracts.steps[3],
    computationThread: contracts.computationThread,
    fraudProof: contracts.fraudProof,
    signer,
    threadUtxo,
    threadToken,
    spendRedeemerSchema: ObserverOrderInvalidStep04RedeemerSchema,
    buildFamilyArgs: ({
      inputIndex,
      outputIndex,
      fraudProofMintRedeemerIndex,
    }) => ({
      input_index: inputIndex,
      output_index: outputIndex,
      fraud_proof_mint_redeemer_index: fraudProofMintRedeemerIndex,
    }),
    referenceScriptUtxo,
    witnessReferenceScripts,
    awaitConfirmation: true,
  });
};

// ---------------------------------------------------------------------------
// Opening mutations
// ---------------------------------------------------------------------------

/** Patch a certified carriage's reference-input coordinates. */
export const mutateCertifiedCarriage = (
  opening: FieldOpening,
  patch: (carriage: {
    cert_ref_input_index: bigint;
    chunk_ref_input_indices: bigint[];
  }) => {
    cert_ref_input_index: bigint;
    chunk_ref_input_indices: bigint[];
  },
): FieldOpening => {
  if (!("BodyFieldOpening" in opening))
    throw new Error("body opening expected");
  const carriage = opening.BodyFieldOpening.carriage;
  if (!("Certified" in carriage))
    throw new Error("certified carriage expected");
  return {
    BodyFieldOpening: {
      ...opening.BodyFieldOpening,
      carriage: {
        Certified: patch({
          cert_ref_input_index: carriage.Certified.cert_ref_input_index,
          chunk_ref_input_indices: [
            ...carriage.Certified.chunk_ref_input_indices,
          ],
        }),
      },
    },
  };
};

/**
 * Point a published (RawUtxo) carriage at a different reference input. A
 * published plan promotes the inline tier to RawUtxo, so a small field's
 * bytes are always read from the named reference input; naming another one
 * substitutes the bytes the door commits.
 */
export const mutateRawUtxoCarriage = (
  opening: FieldOpening,
  offset: bigint,
): FieldOpening => {
  if (!("BodyFieldOpening" in opening))
    throw new Error("body opening expected");
  const carriage = opening.BodyFieldOpening.carriage;
  if (!("RawUtxo" in carriage)) throw new Error("raw-utxo carriage expected");
  return {
    BodyFieldOpening: {
      ...opening.BodyFieldOpening,
      carriage: {
        RawUtxo: {
          ref_input_index: carriage.RawUtxo.ref_input_index + offset,
        },
      },
    },
  };
};
