import {
  type FieldOpeningV1,
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
  faultProofFieldOpeningV1,
  planFaultProofFieldOpeningV1,
  resolveFaultProofFieldCarriagePublicationsV1,
  resolveFaultProofFieldPreimageCertificateV1,
} from "../field-opening-v1.js";
import {
  requireLinearFaultReferenceScriptV1,
  requireLinearFaultStepStateV1,
  requireLinearFaultThreadUtxoV1,
} from "../linear-fault-family-v1.js";
import { submitLinearFaultContinueV1 } from "../linear-fault-submit-v1.js";
import type { ResolvedProverSigner } from "../runtime.js";
import { computationThreadOutputPredicate } from "../tx-layout.js";
import type { FraudProofPreSubmitBoundaryV1 } from "../workflow/transaction-boundary-v1.js";
import type { MissingRedeemerContractsV1 } from "./contracts-v1.js";
import type { MissingRedeemerEvidenceV1 } from "./family-v1.js";
import {
  MissingRedeemerAuthenticationStateV1Schema,
  MissingRedeemerScanV1Schema,
  MissingRedeemerStep03DatumV1Schema,
  MissingRedeemerStep03RedeemerV1Schema,
  MissingRedeemerStep04DatumV1Schema,
  MissingRedeemerStep04RedeemerV1Schema,
  MissingRedeemerStep05DatumV1Schema,
} from "./schemas-v1.js";
import {
  encodeMissingRedeemerGrammarCheckpointV1,
  encodeMissingRedeemerWalkCheckpointV1,
  hashMissingRedeemerGrammarCheckpointV1,
  hashMissingRedeemerWalkCheckpointV1,
  type MissingRedeemerStagedPlanV1,
} from "./staged-plan-v1.js";

const FAMILY = "missing-redeemer";
export type MissingRedeemerStep03ActionV1 =
  | { readonly kind: "direct" }
  | { readonly kind: "grammar_start" }
  | { readonly kind: "grammar_resume"; readonly ordinal: number }
  | { readonly kind: "grammar_finish" };
type Common = Readonly<{
  lucid: LucidEvolution;
  contracts: MissingRedeemerContractsV1;
  categoryId: string;
  signer: ResolvedProverSigner;
  threadOutRef: string;
  evidence: MissingRedeemerEvidenceV1;
  nativeTxCompactCbor: string;
  staged: MissingRedeemerStagedPlanV1;
  referenceScriptUtxo: UTxO;
  preSubmitBoundary?: FraudProofPreSubmitBoundaryV1;
  awaitConfirmation?: boolean;
}>;

const opening = async (common: Common, stepIndex: number) => {
  const planned = planFaultProofFieldOpeningV1({
    fieldIndex: 8,
    anchorTxId: common.evidence.subject.transaction_id,
    nativeTxCompactCbor: common.nativeTxCompactCbor,
    itemCbors: common.staged.items,
    owner: common.signer.paymentKeyHash,
    publish: true,
    label: "missingRedeemer field 8",
  });
  const carriageUtxos = await resolveFaultProofFieldCarriagePublicationsV1({
    lucid: common.lucid,
    publisherAddress: common.signer.address,
    planned,
  });
  if (carriageUtxos === undefined)
    throw new Error("missingRedeemer field carriage disappeared");
  const certificateUtxo =
    planned.plan.tier === "Certified"
      ? await resolveFaultProofFieldPreimageCertificateV1({
          lucid: common.lucid,
          network: common.lucid.config().network!,
          planned,
          certificatePolicyId:
            common.contracts.fieldPreimageCertificatePolicyId,
        })
      : undefined;
  if (planned.plan.tier === "Certified" && certificateUtxo === undefined)
    throw new Error("missingRedeemer field certificate disappeared");
  const stepReference = requireLinearFaultReferenceScriptV1({
    utxo: common.referenceScriptUtxo,
    expectedScriptHash: common.contracts.steps[stepIndex]!.spendingScriptHash,
    family: FAMILY,
    stepIndex,
  });
  const value: FieldOpeningV1 = faultProofFieldOpeningV1({
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

export const submitMissingRedeemerStep03V1 = async (
  common: Common & { action: MissingRedeemerStep03ActionV1 },
) => {
  const stepIndex = 4;
  const { threadUtxo, threadToken } = await requireLinearFaultThreadUtxoV1({
    lucid: common.lucid,
    contracts: common.contracts,
    categoryId: common.categoryId,
    family: FAMILY,
    stepIndex,
    threadOutRef: common.threadOutRef,
  });
  const state = requireLinearFaultStepStateV1<
    Data.Static<typeof MissingRedeemerAuthenticationStateV1Schema>
  >({
    threadUtxo,
    signer: common.signer,
    schema: MissingRedeemerStep03DatumV1Schema as never,
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
        checkpoint_hash: hashMissingRedeemerWalkCheckpointV1(
          common.staged.initialWalk,
        ),
        cursor: 0n,
        item_count: BigInt(common.evidence.itemCount),
        found: false,
      }
    : {
        Grammar: {
          authenticated,
          checkpoint_hash: hashMissingRedeemerGrammarCheckpointV1(grammar!),
        },
      };
  const nextSchema = advances
    ? MissingRedeemerStep04DatumV1Schema
    : MissingRedeemerStep03DatumV1Schema;
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
                  checkpoint_bytes: encodeMissingRedeemerGrammarCheckpointV1(
                    common.staged.grammar[common.action.ordinal - 1]!,
                  ).toString("hex"),
                  item_budget: 16n,
                },
              }
            : {
                FinishGrammar: {
                  ...base,
                  checkpoint_bytes: encodeMissingRedeemerGrammarCheckpointV1(
                    common.staged.grammar.at(-1)!,
                  ).toString("hex"),
                },
              };
    return Data.to(
      { Continue: [args] } as never,
      MissingRedeemerStep03RedeemerV1Schema as never,
    );
  }) satisfies BuildTxWithRedeemer;
  common.signer.selectWallet(common.lucid);
  const txHash = await submitLinearFaultContinueV1({
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

export const submitMissingRedeemerStep04V1 = async (common: Common) => {
  const stepIndex = 5;
  const { threadUtxo, threadToken } = await requireLinearFaultThreadUtxoV1({
    lucid: common.lucid,
    contracts: common.contracts,
    categoryId: common.categoryId,
    family: FAMILY,
    stepIndex,
    threadOutRef: common.threadOutRef,
  });
  const state = requireLinearFaultStepStateV1<
    Data.Static<typeof MissingRedeemerScanV1Schema>
  >({
    threadUtxo,
    signer: common.signer,
    schema: MissingRedeemerStep04DatumV1Schema as never,
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
    hashMissingRedeemerWalkCheckpointV1(checkpoint) !== state.checkpoint_hash
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
        checkpoint_hash: hashMissingRedeemerWalkCheckpointV1(nextCheckpoint),
      };
  const nextSchema = terminal
    ? MissingRedeemerStep05DatumV1Schema
    : MissingRedeemerStep04DatumV1Schema;
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
              encodeMissingRedeemerWalkCheckpointV1(checkpoint).toString("hex"),
            item_budget: 16n,
          },
        ],
      } as never,
      MissingRedeemerStep04RedeemerV1Schema as never,
    );
  }) satisfies BuildTxWithRedeemer;
  common.signer.selectWallet(common.lucid);
  const txHash = await submitLinearFaultContinueV1({
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
