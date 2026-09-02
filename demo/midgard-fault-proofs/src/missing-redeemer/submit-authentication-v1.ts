import { hashMidgardValidationEventKeyV1 } from "@al-ft/midgard-core";
import {
  EventKeySchema,
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
  requireLinearFaultReferenceScriptV1,
  requireLinearFaultStepStateV1,
  requireLinearFaultThreadUtxoV1,
} from "../linear-fault-family-v1.js";
import { submitLinearFaultContinueV1 } from "../linear-fault-submit-v1.js";
import type { ResolvedProverSigner } from "../runtime.js";
import { computationThreadOutputPredicate } from "../tx-layout.js";
import type { FraudProofPreSubmitBoundaryV1 } from "../workflow/transaction-boundary-v1.js";
import type { MissingRedeemerContractsV1 } from "./contracts-v1.js";
import type { MissingRedeemerStageTenAuthenticationV1 } from "./retained-stage-ten-v1.js";
import {
  MissingRedeemerAuthenticatedDescriptorV1Schema,
  MissingRedeemerAuthenticatedStageTenV1Schema,
  MissingRedeemerBoundPurposeV1Schema,
  MissingRedeemerStep02aDatumV1Schema,
  MissingRedeemerStep02aRedeemerV1Schema,
  MissingRedeemerStep02bDatumV1Schema,
  MissingRedeemerStep02bRedeemerV1Schema,
  MissingRedeemerStep02DatumV1Schema,
  MissingRedeemerStep02RedeemerV1Schema,
  MissingRedeemerStep03DatumV1Schema,
} from "./schemas-v1.js";

const FAMILY = "missing-redeemer";
type Common = Readonly<{
  lucid: LucidEvolution;
  contracts: MissingRedeemerContractsV1;
  categoryId: string;
  signer: ResolvedProverSigner;
  threadOutRef: string;
  authentication: MissingRedeemerStageTenAuthenticationV1;
  referenceScriptUtxo: UTxO;
  preSubmitBoundary?: FraudProofPreSubmitBoundaryV1;
  awaitConfirmation?: boolean;
}>;

const submit = async ({
  common,
  stepIndex,
  currentSchema,
  nextSchema,
  nextData,
  redeemerSchema,
  buildArgs,
}: {
  common: Common;
  stepIndex: 1 | 2 | 3;
  currentSchema: unknown;
  nextSchema: unknown;
  nextData: (state: Record<string, unknown>) => Record<string, unknown>;
  redeemerSchema: unknown;
  buildArgs: (state: Record<string, unknown>) => Record<string, unknown>;
}) => {
  const { threadUtxo, threadToken } = await requireLinearFaultThreadUtxoV1({
    lucid: common.lucid,
    contracts: common.contracts,
    categoryId: common.categoryId,
    family: FAMILY,
    stepIndex,
    threadOutRef: common.threadOutRef,
  });
  const state = requireLinearFaultStepStateV1<Record<string, unknown>>({
    threadUtxo,
    signer: common.signer,
    schema: currentSchema as never,
    family: FAMILY,
    stepIndex,
  });
  const data = nextData(state);
  const nextDatum = Data.to(
    { fraud_prover: common.signer.paymentKeyHash, data } as never,
    nextSchema as never,
  );
  const outputMatches = computationThreadOutputPredicate({
    address: common.contracts.steps[stepIndex + 1].spendingScriptAddress,
    datum: nextDatum,
    unit: threadToken.unit,
  });
  const stepReference = requireLinearFaultReferenceScriptV1({
    utxo: common.referenceScriptUtxo,
    expectedScriptHash: common.contracts.steps[stepIndex].spendingScriptHash,
    family: FAMILY,
    stepIndex,
  });
  let outputIndex: bigint | undefined;
  const redeemer = ((ctx) => {
    requireOwnSpendPurpose(ctx, threadUtxo, `${FAMILY} authentication`);
    const inputIndex = requireInputIndex(ctx, threadUtxo, FAMILY);
    outputIndex = requireUniqueOutputIndex(ctx.outputs, outputMatches, FAMILY);
    return Data.to(
      {
        Continue: [
          {
            ...buildArgs(state),
            input_index: inputIndex,
            output_index: outputIndex,
          },
        ],
      } as never,
      redeemerSchema as never,
    );
  }) satisfies BuildTxWithRedeemer;
  common.signer.selectWallet(common.lucid);
  const txHash = await submitLinearFaultContinueV1({
    lucid: common.lucid,
    signerPaymentKeyHash: common.signer.paymentKeyHash,
    threadUtxo,
    threadUnit: threadToken.unit,
    stepReference,
    stepScript: common.contracts.steps[stepIndex].spendingScript,
    stepRole: `${FAMILY} authentication ${stepIndex.toString()}`,
    nextAddress: common.contracts.steps[stepIndex + 1].spendingScriptAddress,
    nextDatum,
    redeemer,
    preSubmitBoundary: common.preSubmitBoundary,
    awaitConfirmation: common.awaitConfirmation ?? true,
  });
  if (outputIndex === undefined)
    throw new Error(`${FAMILY}: layout unresolved`);
  return { txHash, nextThreadOutRef: `${txHash}#${outputIndex.toString()}` };
};

/** Authenticates the header-root trace descriptor (physical step 02). */
export const submitMissingRedeemerStep02V1 = async (common: Common) =>
  await submit({
    common,
    stepIndex: 1,
    currentSchema: MissingRedeemerStep02DatumV1Schema,
    nextSchema: MissingRedeemerStep02aDatumV1Schema,
    redeemerSchema: MissingRedeemerStep02RedeemerV1Schema,
    nextData: (state) => {
      const bound = state as Data.Static<
        typeof MissingRedeemerBoundPurposeV1Schema
      >;
      const membership = common.authentication.traceMembership as {
        key: Data.Static<typeof EventKeySchema>;
        value: unknown;
      };
      return {
        bound,
        event_key_hash: hashMidgardValidationEventKeyV1(
          Buffer.from(Data.to(membership.key as never, EventKeySchema), "hex"),
        ).toString("hex"),
        descriptor: membership.value,
      };
    },
    buildArgs: () => ({
      trace_membership: common.authentication.traceMembership,
    }),
  });

/** Authenticates the exact ScriptSources stage-10 trace state (physical 02a). */
export const submitMissingRedeemerStep02aV1 = async (common: Common) =>
  await submit({
    common,
    stepIndex: 2,
    currentSchema: MissingRedeemerStep02aDatumV1Schema,
    nextSchema: MissingRedeemerStep02bDatumV1Schema,
    redeemerSchema: MissingRedeemerStep02aRedeemerV1Schema,
    nextData: (state) => {
      const authenticated = state as Data.Static<
        typeof MissingRedeemerAuthenticatedDescriptorV1Schema
      >;
      const control = common.authentication.control;
      return {
        bound: authenticated.bound,
        source_count: control.source_count,
        source_peaks: control.source_peaks,
        purpose_count: control.purpose_count,
        purpose_peaks: control.purpose_peaks,
        discovery: control.discovery,
      };
    },
    buildArgs: () => ({
      machine_state: common.authentication.machineState,
      trace_proof: common.authentication.traceProof,
      control: common.authentication.control,
    }),
  });

/** Authenticates selected purpose/source membership and Plutus language (02b). */
export const submitMissingRedeemerStep02bV1 = async (common: Common) =>
  await submit({
    common,
    stepIndex: 3,
    currentSchema: MissingRedeemerStep02bDatumV1Schema,
    nextSchema: MissingRedeemerStep03DatumV1Schema,
    redeemerSchema: MissingRedeemerStep02bRedeemerV1Schema,
    nextData: (state) => {
      const stage = state as Data.Static<
        typeof MissingRedeemerAuthenticatedStageTenV1Schema
      >;
      return {
        Ready: {
          authenticated: {
            bound: stage.bound,
            purpose_count: stage.purpose_count,
            redeemer_tag: [0n, 1n, 3n, 6n][Number(stage.bound.purpose_kind)]!,
            required_script_hash: common.authentication.sourceScriptHash,
            source_index: stage.discovery.matched_source_index,
            source_language_tag: common.authentication.sourceLanguageTag,
            source_leaf: stage.discovery.matched_source_leaf,
          },
        },
      };
    },
    buildArgs: () => ({
      absolute_purpose_index: common.authentication.absolutePurposeIndex,
      purpose_siblings: common.authentication.purposeSiblings,
      source_origin_kind: common.authentication.sourceOriginKind,
      source_key: common.authentication.sourceKey,
      source_language_tag: common.authentication.sourceLanguageTag,
      source_script_hash: common.authentication.sourceScriptHash,
      source_total_length: common.authentication.sourceTotalLength,
      source_item_commitment: common.authentication.sourceItemCommitment,
      source_siblings: common.authentication.sourceSiblings,
    }),
  });
