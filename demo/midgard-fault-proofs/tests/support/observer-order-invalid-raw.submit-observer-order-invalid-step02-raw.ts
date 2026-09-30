import {
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
import { submitLinearFaultContinue } from "../../src/linear-fault-submit.js";
import type { ObserverOrderInvalidContracts } from "../../src/observer-order-invalid/contracts.js";
import {
  classifyObserverOrderInvalidFinding,
  type ObserverOrderInvalidEvidence,
  type ObserverOrderInvalidFinding,
} from "../../src/observer-order-invalid/family.js";
import {
  ObserverOrderInvalidStep01RedeemerSchema,
  ObserverOrderInvalidStep02DatumSchema,
  ObserverOrderInvalidStep02RedeemerSchema,
  ObserverOrderInvalidStep03DatumSchema,
} from "../../src/observer-order-invalid/schemas.js";
import {
  hashObserverOrderWalkCheckpoint,
  type ObserverOrderInvalidStagedPlan,
} from "../../src/observer-order-invalid/staged-plan.js";
import type { ResolvedProverSigner } from "../../src/runtime.js";
import { requireInitialStepDatum } from "../../src/step-support.js";
import { computationThreadOutputPredicate } from "../../src/tx-layout.js";
import {
  FAMILY,
  type OpeningMutation,
  resolveObserverOpening,
} from "./observer-order-invalid-raw.resolve-observer-opening.js";

// ---------------------------------------------------------------------------
// Raw submitters
// ---------------------------------------------------------------------------

/**
 * Forced step 01 with the successor exposed: `nextStepIndex` names a step
 * other than the one the validator was applied with, so the deterministic
 * successor check refuses the continuation on chain.
 */
export const submitObserverOrderInvalidStep01ForcedRaw = async ({
  lucid,
  contracts,
  categoryId,
  signer,
  threadOutRef,
  finding,
  forcedSource,
  referenceScriptUtxo,
  nextStepIndex = 1,
}: {
  readonly lucid: LucidEvolution;
  readonly contracts: ObserverOrderInvalidContracts;
  readonly categoryId: string;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  readonly finding: ObserverOrderInvalidFinding;
  readonly forcedSource: Readonly<Record<string, unknown>>;
  readonly referenceScriptUtxo: UTxO;
  readonly nextStepIndex?: 0 | 1 | 2 | 3;
}) => {
  const exact = classifyObserverOrderInvalidFinding(finding);
  const { threadUtxo, threadToken } = await requireLinearFaultThreadUtxo({
    lucid,
    contracts,
    categoryId,
    family: FAMILY,
    stepIndex: 0,
    threadOutRef,
  });
  requireInitialStepDatum({ threadUtxo, signer });
  signer.selectWallet(lucid);
  const stepReference = requireLinearFaultReferenceScript({
    utxo: referenceScriptUtxo,
    expectedScriptHash: contracts.steps[0].spendingScriptHash,
    family: FAMILY,
    stepIndex: 0,
  });
  const datum = Data.to(
    {
      fraud_prover: signer.paymentKeyHash,
      data: {
        Bound: {
          bound: {
            subject: exact.subject,
            observer_index: BigInt(exact.observerIndex),
          },
        },
      },
    } as never,
    ObserverOrderInvalidStep02DatumSchema as never,
  );
  const nextStep = contracts.steps[nextStepIndex];
  const outputMatches = computationThreadOutputPredicate({
    address: nextStep.spendingScriptAddress,
    datum,
    unit: threadToken.unit,
  });
  const redeemer = ((ctx) => {
    requireOwnSpendPurpose(ctx, threadUtxo, `${FAMILY} raw forced step-01`);
    const outputIndex = requireUniqueOutputIndex(
      ctx.outputs,
      outputMatches,
      `${FAMILY} raw forced output`,
    );
    return Data.to(
      {
        Continue: [
          {
            source: {
              ForcedSource: {
                ...forcedSource,
                input_index: requireInputIndex(ctx, threadUtxo, FAMILY),
                output_index: outputIndex,
              },
            },
            observer_index: BigInt(exact.observerIndex),
          },
        ],
      } as never,
      ObserverOrderInvalidStep01RedeemerSchema as never,
    );
  }) satisfies BuildTxWithRedeemer;
  return await submitLinearFaultContinue({
    lucid,
    signerPaymentKeyHash: signer.paymentKeyHash,
    threadUtxo,
    threadUnit: threadToken.unit,
    stepReference,
    stepScript: contracts.steps[0].spendingScript,
    stepRole: `${FAMILY} raw forced step-01`,
    nextAddress: nextStep.spendingScriptAddress,
    nextDatum: datum,
    redeemer,
    awaitConfirmation: true,
  });
};

/**
 * Step 02 with the opening exposed: `mutateOpening` rewrites the redeemer's
 * field opening after every off-chain check has passed. Carriage must
 * already be published (and certified when the tier requires it) exactly as
 * the production builder expects.
 */
export const submitObserverOrderInvalidStep02Raw = async ({
  lucid,
  contracts,
  categoryId,
  signer,
  threadOutRef,
  evidence,
  nativeTxCompactCbor,
  staged,
  referenceScriptUtxo,
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
  readonly referenceScriptUtxo: UTxO;
  readonly mutateOpening?: OpeningMutation;
}) => {
  const stepIndex = 1;
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
    schema: ObserverOrderInvalidStep02DatumSchema as never,
    family: FAMILY,
    stepIndex,
  });
  const stepReference = requireLinearFaultReferenceScript({
    utxo: referenceScriptUtxo,
    expectedScriptHash: contracts.steps[1].spendingScriptHash,
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
      label: `${FAMILY} raw field 3`,
    });
  const nextDatum = Data.to(
    {
      fraud_prover: signer.paymentKeyHash,
      data: {
        subject: evidence.subject,
        observer_index: BigInt(evidence.observerIndex),
        checkpoint_hash: hashObserverOrderWalkCheckpoint(staged.initialWalk),
        seen: 0n,
        previous_observer: "",
        outcome: 0n,
      },
    } as never,
    ObserverOrderInvalidStep03DatumSchema as never,
  );
  const nextStep = contracts.steps[2];
  const outputMatches = computationThreadOutputPredicate({
    address: nextStep.spendingScriptAddress,
    datum: nextDatum,
    unit: threadToken.unit,
  });
  const redeemer = ((ctx) => {
    requireOwnSpendPurpose(ctx, threadUtxo, `${FAMILY} raw step-02`);
    return Data.to(
      {
        Continue: [
          {
            Authenticate: {
              input_index: requireInputIndex(ctx, threadUtxo, FAMILY),
              output_index: requireUniqueOutputIndex(
                ctx.outputs,
                outputMatches,
                `${FAMILY} raw step-02 output`,
              ),
              opening,
            },
          },
        ],
      } as never,
      ObserverOrderInvalidStep02RedeemerSchema as never,
    );
  }) satisfies BuildTxWithRedeemer;
  return await submitLinearFaultContinue({
    lucid,
    signerPaymentKeyHash: signer.paymentKeyHash,
    threadUtxo,
    threadUnit: threadToken.unit,
    stepReference,
    stepScript: contracts.steps[1].spendingScript,
    stepRole: `${FAMILY} raw step-02`,
    nextAddress: nextStep.spendingScriptAddress,
    nextDatum,
    redeemer,
    carriageUtxos,
    extraReferenceInputs,
    awaitConfirmation: true,
  });
};

export type ObserverOrderScanSuccessor =
  | Readonly<{
      kind: "scan";
      checkpointHash: string;
      seen: bigint;
      previousObserver: string;
    }>
  | Readonly<{ kind: "decision"; violation: boolean }>;
