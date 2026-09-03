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
  faultProofFieldOpening,
  planFaultProofFieldOpening,
  resolveFaultProofFieldCarriagePublications,
  resolveFaultProofFieldPreimageCertificate,
} from "../field-opening-v1.js";
import {
  requireLinearFaultReferenceScript,
  requireLinearFaultStepState,
  requireLinearFaultThreadUtxo,
} from "../linear-fault-family-v1.js";
import { submitLinearFaultContinue } from "../linear-fault-submit-v1.js";
import type { ResolvedProverSigner } from "../runtime.js";
import { computationThreadOutputPredicate } from "../tx-layout.js";
import type { FraudProofPreSubmitBoundary } from "../workflow/transaction-boundary-v1.js";
import type { MissingRedeemerContracts } from "./contracts-v1.js";
import type { MissingRedeemerEvidence } from "./family-v1.js";
import {
  MissingRedeemerAuthenticationStateSchema,
  MissingRedeemerScanSchema,
  MissingRedeemerStep03DatumSchema,
  MissingRedeemerStep03RedeemerSchema,
  MissingRedeemerStep04DatumSchema,
  MissingRedeemerStep04RedeemerSchema,
  MissingRedeemerStep05DatumSchema,
} from "./schemas-v1.js";
import {
  encodeMissingRedeemerGrammarCheckpoint,
  encodeMissingRedeemerWalkCheckpoint,
  hashMissingRedeemerGrammarCheckpoint,
  hashMissingRedeemerWalkCheckpoint,
  type MissingRedeemerStagedPlan,
} from "./staged-plan-v1.js";

const FAMILY = "missing-redeemer";
export type MissingRedeemerStep03Action =
  | { readonly kind: "direct" }
  | { readonly kind: "grammar_start" }
  | { readonly kind: "grammar_resume"; readonly ordinal: number }
  | { readonly kind: "grammar_finish" };
type Common = Readonly<{
  lucid: LucidEvolution;
  contracts: MissingRedeemerContracts;
  categoryId: string;
  signer: ResolvedProverSigner;
  threadOutRef: string;
  evidence: MissingRedeemerEvidence;
  nativeTxCompactCbor: string;
  staged: MissingRedeemerStagedPlan;
  referenceScriptUtxo: UTxO;
  preSubmitBoundary?: FraudProofPreSubmitBoundary;
  awaitConfirmation?: boolean;
}>;

const opening = async (common: Common, stepIndex: number) => {
  const planned = planFaultProofFieldOpening({
    fieldIndex: 8,
    anchorTxId: common.evidence.subject.transaction_id,
    nativeTxCompactCbor: common.nativeTxCompactCbor,
    itemCbors: common.staged.items,
    owner: common.signer.paymentKeyHash,
    publish: true,
    label: "missingRedeemer field 8",
  });
  const carriageUtxos = await resolveFaultProofFieldCarriagePublications({
    lucid: common.lucid,
    publisherAddress: common.signer.address,
    planned,
  });
  if (carriageUtxos === undefined)
    throw new Error("missingRedeemer field carriage disappeared");
  const certificateUtxo =
    planned.plan.tier === "Certified"
      ? await resolveFaultProofFieldPreimageCertificate({
          lucid: common.lucid,
          network: common.lucid.config().network!,
          planned,
          certificatePolicyId:
            common.contracts.fieldPreimageCertificatePolicyId,
        })
      : undefined;
  if (planned.plan.tier === "Certified" && certificateUtxo === undefined)
    throw new Error("missingRedeemer field certificate disappeared");
  const stepReference = requireLinearFaultReferenceScript({
    utxo: common.referenceScriptUtxo,
    expectedScriptHash: common.contracts.steps[stepIndex]!.spendingScriptHash,
    family: FAMILY,
    stepIndex,
  });
  const value: FieldOpening = faultProofFieldOpening({
    planned,
    referenceInputs: [
      ...carriageUtxos,
      stepReference,
      ...(certificateUtxo === undefined ? [] : [certificateUtxo]),
    ],
    certificatePolicyId: common.contracts.fieldPreimageCertificatePolicyId,
    label: "missingRedeemer field 8",
  });
  return {
    value,
    carriageUtxos,
    certificateUtxo,
    stepReference,
    tier: planned.plan.tier,
  };
};

export const submitMissingRedeemerStep03 = async (
  common: Common & { action: MissingRedeemerStep03Action },
) => {
  const stepIndex = 4;
  const { threadUtxo, threadToken } = await requireLinearFaultThreadUtxo({
    lucid: common.lucid,
    contracts: common.contracts,
    categoryId: common.categoryId,
    family: FAMILY,
    stepIndex,
    threadOutRef: common.threadOutRef,
  });
  const state = requireLinearFaultStepState<
    Data.Static<typeof MissingRedeemerAuthenticationStateSchema>
  >({
    threadUtxo,
    signer: common.signer,
    schema: MissingRedeemerStep03DatumSchema as never,
    family: FAMILY,
    stepIndex,
  });
  const authenticated =
    "Ready" in state ? state.Ready.authenticated : state.Grammar.authenticated;
  const field = await opening(common, stepIndex);
  if (common.action.kind === "direct" && field.tier === "Certified")
    throw new Error("missingRedeemer certified field requires grammar");
  const advances =
    common.action.kind === "direct" || common.action.kind === "grammar_finish";
  const grammar =
    common.action.kind === "grammar_resume"
      ? common.staged.grammar[common.action.ordinal]
      : common.staged.grammar[0];
  if (!advances && grammar === undefined)
    throw new Error("missingRedeemer grammar ordinal is outside plan");
  const nextData = advances
    ? {
        authenticated,
        checkpoint_hash: hashMissingRedeemerWalkCheckpoint(
          common.staged.initialWalk,
        ),
        cursor: 0n,
        item_count: BigInt(common.evidence.itemCount),
        found: false,
      }
    : {
        Grammar: {
          authenticated,
          checkpoint_hash: hashMissingRedeemerGrammarCheckpoint(grammar!),
        },
      };
  const nextSchema = advances
    ? MissingRedeemerStep04DatumSchema
    : MissingRedeemerStep03DatumSchema;
  const nextStep = advances
    ? common.contracts.steps[5]
    : common.contracts.steps[4];
  const nextDatum = Data.to(
    { fraud_prover: common.signer.paymentKeyHash, data: nextData } as never,
    nextSchema as never,
  );
  const outputMatches = computationThreadOutputPredicate({
    address: nextStep.spendingScriptAddress,
    datum: nextDatum,
    unit: threadToken.unit,
  });
  let outputIndex: bigint | undefined;
  const redeemer = ((ctx) => {
    requireOwnSpendPurpose(ctx, threadUtxo, `${FAMILY} step 03`);
    const inputIndex = requireInputIndex(ctx, threadUtxo, FAMILY);
    outputIndex = requireUniqueOutputIndex(ctx.outputs, outputMatches, FAMILY);
    const base = {
      input_index: inputIndex,
      output_index: outputIndex,
      opening: field.value,
    };
    const args =
      common.action.kind === "direct"
        ? { AuthenticateDirect: base }
        : common.action.kind === "grammar_start"
          ? { StartGrammar: { ...base, item_budget: 16n } }
          : common.action.kind === "grammar_resume"
            ? {
                ResumeGrammar: {
                  ...base,
                  checkpoint_bytes: encodeMissingRedeemerGrammarCheckpoint(
                    common.staged.grammar[common.action.ordinal - 1]!,
                  ).toString("hex"),
                  item_budget: 16n,
                },
              }
            : {
                FinishGrammar: {
                  ...base,
                  checkpoint_bytes: encodeMissingRedeemerGrammarCheckpoint(
                    common.staged.grammar.at(-1)!,
                  ).toString("hex"),
                },
              };
    return Data.to(
      { Continue: [args] } as never,
      MissingRedeemerStep03RedeemerSchema as never,
    );
  }) satisfies BuildTxWithRedeemer;
  common.signer.selectWallet(common.lucid);
  const txHash = await submitLinearFaultContinue({
    lucid: common.lucid,
    signerPaymentKeyHash: common.signer.paymentKeyHash,
    threadUtxo,
    threadUnit: threadToken.unit,
    stepReference: field.stepReference,
    stepScript: common.contracts.steps[stepIndex].spendingScript,
    stepRole: `${FAMILY} step 03 ${common.action.kind}`,
    nextAddress: nextStep.spendingScriptAddress,
    nextDatum,
    redeemer,
    carriageUtxos: field.carriageUtxos,
    extraReferenceInputs:
      field.certificateUtxo === undefined ? [] : [field.certificateUtxo],
    preSubmitBoundary: common.preSubmitBoundary,
    awaitConfirmation: common.awaitConfirmation ?? true,
  });
  if (outputIndex === undefined)
    throw new Error(`${FAMILY}: layout unresolved`);
  return { txHash, nextThreadOutRef: `${txHash}#${outputIndex.toString()}` };
};

export const submitMissingRedeemerStep04 = async (common: Common) => {
  const stepIndex = 5;
  const { threadUtxo, threadToken } = await requireLinearFaultThreadUtxo({
    lucid: common.lucid,
    contracts: common.contracts,
    categoryId: common.categoryId,
    family: FAMILY,
    stepIndex,
    threadOutRef: common.threadOutRef,
  });
  const state = requireLinearFaultStepState<
    Data.Static<typeof MissingRedeemerScanSchema>
  >({
    threadUtxo,
    signer: common.signer,
    schema: MissingRedeemerStep04DatumSchema as never,
    family: FAMILY,
    stepIndex,
  });
  const cursor = Number(state.cursor);
  const checkpoint =
    cursor === 0
      ? common.staged.initialWalk
      : common.staged.walk.find((item) => item.nextItemIndex === cursor);
  if (
    checkpoint === undefined ||
    hashMissingRedeemerWalkCheckpoint(checkpoint) !== state.checkpoint_hash
  )
    throw new Error("missingRedeemer scan checkpoint is unreachable");
  const nextCursor = Math.min(cursor + 16, common.evidence.itemCount);
  const found =
    state.found ||
    common.evidence.scannedPointers
      .slice(cursor, nextCursor)
      .includes(
        `${state.authenticated.redeemer_tag.toString()}:${state.authenticated.bound.purpose_index.toString()}`,
      );
  const terminal = found || nextCursor === common.evidence.itemCount;
  const nextCheckpoint =
    nextCursor === 0
      ? common.staged.initialWalk
      : common.staged.walk.find((item) => item.nextItemIndex === nextCursor);
  if (nextCheckpoint === undefined)
    throw new Error("missingRedeemer next scan checkpoint is absent");
  const nextData = terminal
    ? { bound: state.authenticated.bound, redeemer_missing: !found }
    : {
        ...state,
        cursor: BigInt(nextCursor),
        found,
        checkpoint_hash: hashMissingRedeemerWalkCheckpoint(nextCheckpoint),
      };
  const nextSchema = terminal
    ? MissingRedeemerStep05DatumSchema
    : MissingRedeemerStep04DatumSchema;
  const nextStep = terminal
    ? common.contracts.steps[6]
    : common.contracts.steps[5];
  const nextDatum = Data.to(
    { fraud_prover: common.signer.paymentKeyHash, data: nextData } as never,
    nextSchema as never,
  );
  const outputMatches = computationThreadOutputPredicate({
    address: nextStep.spendingScriptAddress,
    datum: nextDatum,
    unit: threadToken.unit,
  });
  const field = await opening(common, stepIndex);
  let outputIndex: bigint | undefined;
  const redeemer = ((ctx) => {
    requireOwnSpendPurpose(ctx, threadUtxo, `${FAMILY} step 04`);
    const inputIndex = requireInputIndex(ctx, threadUtxo, FAMILY);
    outputIndex = requireUniqueOutputIndex(ctx.outputs, outputMatches, FAMILY);
    return Data.to(
      {
        Continue: [
          {
            input_index: inputIndex,
            output_index: outputIndex,
            opening: field.value,
            checkpoint_bytes:
              encodeMissingRedeemerWalkCheckpoint(checkpoint).toString("hex"),
            item_budget: 16n,
          },
        ],
      } as never,
      MissingRedeemerStep04RedeemerSchema as never,
    );
  }) satisfies BuildTxWithRedeemer;
  common.signer.selectWallet(common.lucid);
  const txHash = await submitLinearFaultContinue({
    lucid: common.lucid,
    signerPaymentKeyHash: common.signer.paymentKeyHash,
    threadUtxo,
    threadUnit: threadToken.unit,
    stepReference: field.stepReference,
    stepScript: common.contracts.steps[stepIndex].spendingScript,
    stepRole: `${FAMILY} step 04`,
    nextAddress: nextStep.spendingScriptAddress,
    nextDatum,
    redeemer,
    carriageUtxos: field.carriageUtxos,
    extraReferenceInputs:
      field.certificateUtxo === undefined ? [] : [field.certificateUtxo],
    preSubmitBoundary: common.preSubmitBoundary,
    awaitConfirmation: common.awaitConfirmation ?? true,
  });
  if (outputIndex === undefined)
    throw new Error(`${FAMILY}: layout unresolved`);
  return { txHash, nextThreadOutRef: `${txHash}#${outputIndex.toString()}` };
};
