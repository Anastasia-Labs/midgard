import * as SDK from "@al-ft/midgard-sdk";
import {
  type BuildTxWithRedeemer,
  Data,
  type LucidEvolution,
  type UTxO,
} from "@lucid-evolution/lucid";

import {
  requireLinearFaultInitialDatum,
  requireLinearFaultReferenceScript,
  requireLinearFaultStepState,
  requireLinearFaultThreadUtxo,
} from "../../src/linear-fault-family.js";
import { submitLinearFaultContinue } from "../../src/linear-fault-submit.js";
import type { ResolvedProverSigner } from "../../src/runtime.js";
import { computationThreadOutputPredicate } from "../../src/tx-layout.js";
import type { UnusedScriptWitnessContracts } from "../../src/unused-script-witness/contracts.js";
import {
  UnusedScriptAuthenticatedWitnessSchema,
  UnusedScriptBoundWitnessSchema,
  UnusedScriptPurposeOpeningSchema,
  UnusedScriptReverseScanSchema,
  UnusedScriptSourceOpeningSchema,
  UnusedScriptStep01RedeemerSchema,
  UnusedScriptStep02DatumSchema,
  UnusedScriptStep02RedeemerSchema,
  UnusedScriptStep03DatumSchema,
  UnusedScriptStep03RedeemerSchema,
  UnusedScriptStep04DatumSchema,
  UnusedScriptStep04RedeemerSchema,
  UnusedScriptStep05DatumSchema,
} from "../../src/unused-script-witness/schemas.js";
import type { UnusedScriptWitnessAuthentication } from "../../src/unused-script-witness/submit-step-02.js";
import { buildUnusedScriptWitnessFixture } from "./unused-script-witness-emulator.build-unused-script-witness-fixture.js";
import { FAMILY } from "./unused-script-witness-emulator.unused-script-witness-fixture-spec.js";

export type UnusedScriptWitnessFixture = Awaited<
  ReturnType<typeof buildUnusedScriptWitnessFixture>
>;

export type Common = Readonly<{
  lucid: LucidEvolution;
  contracts: UnusedScriptWitnessContracts;
  categoryId: string;
  signer: ResolvedProverSigner;
  threadOutRef: string;
  referenceScriptUtxo: UTxO;
}>;

/**
 * A raw continuation: the exact datum and redeemer the test asks for reach
 * the validator, so every substitution is refused on chain rather than by an
 * off-chain builder guard.
 */
export const continueRaw = async ({
  common,
  stepIndex,
  nextAddress,
  nextDatum,
  redeemerSchema,
  args,
}: {
  readonly common: Common;
  readonly stepIndex: number;
  readonly nextAddress: string;
  readonly nextDatum: string;
  readonly redeemerSchema: unknown;
  readonly args: (
    inputIndex: bigint,
    outputIndex: bigint,
  ) => Record<string, unknown>;
}) => {
  const { lucid, contracts, categoryId, signer, threadOutRef } = common;
  const { threadUtxo, threadToken } = await requireLinearFaultThreadUtxo({
    lucid,
    contracts,
    categoryId,
    family: FAMILY,
    stepIndex,
    threadOutRef,
  });
  const role = `raw step ${(stepIndex + 1).toString().padStart(2, "0")}`;
  const stepReference = requireLinearFaultReferenceScript({
    utxo: common.referenceScriptUtxo,
    expectedScriptHash: contracts.steps[stepIndex]!.spendingScriptHash,
    family: FAMILY,
    stepIndex,
  });
  const outputMatches = computationThreadOutputPredicate({
    address: nextAddress,
    datum: nextDatum,
    unit: threadToken.unit,
  });
  let outputIndex: bigint | undefined;
  const redeemer = ((ctx) => {
    SDK.requireOwnSpendPurpose(ctx, threadUtxo, role);
    const inputIndex = SDK.requireInputIndex(ctx, threadUtxo, role);
    outputIndex = SDK.requireUniqueOutputIndex(
      ctx.outputs,
      outputMatches,
      role,
    );
    return Data.to(
      { Continue: [args(inputIndex, outputIndex)] } as never,
      redeemerSchema as never,
    );
  }) satisfies BuildTxWithRedeemer;
  signer.selectWallet(lucid);
  const txHash = await submitLinearFaultContinue({
    lucid,
    signerPaymentKeyHash: signer.paymentKeyHash,
    threadUtxo,
    threadUnit: threadToken.unit,
    stepReference,
    stepScript: contracts.steps[stepIndex]!.spendingScript,
    stepRole: role,
    nextAddress,
    nextDatum,
    redeemer,
    awaitConfirmation: true,
  });
  if (outputIndex === undefined) throw new Error(`${role}: no layout`);
  return { txHash, nextThreadOutRef: `${txHash}#${outputIndex.toString()}` };
};

export const stepState = <State>(
  common: Common,
  stepIndex: number,
  schema: unknown,
) =>
  requireLinearFaultThreadUtxo({
    lucid: common.lucid,
    contracts: common.contracts,
    categoryId: common.categoryId,
    family: FAMILY,
    stepIndex,
    threadOutRef: common.threadOutRef,
  }).then(({ threadUtxo }) =>
    requireLinearFaultStepState<State>({
      threadUtxo,
      signer: common.signer,
      schema: schema as never,
      family: FAMILY,
      stepIndex,
    }),
  );

export const datumOf = (common: Common, data: unknown, schema: unknown) =>
  Data.to(
    { fraud_prover: common.signer.paymentKeyHash, data } as never,
    schema as never,
  );

/** Step 01 over a forced leaf with no off-chain classification. */
export const submitUnusedStep01ForcedRaw = async ({
  header,
  membership,
  scriptIndex,
  direction,
  ...common
}: Common & {
  readonly header: SDK.Header;
  readonly membership: SDK.RootMembershipProof<
    SDK.OutputReference,
    SDK.ForcedInclusionTxV1
  >;
  readonly scriptIndex: bigint;
  readonly direction: bigint;
}) => {
  const verdict = membership.value.verdict;
  if (verdict === "ForcedTxValid")
    throw new Error("raw forced step 01 needs a rejected leaf");
  const subject = {
    ...SDK.forcedVerdictSubject({
      transactionId: membership.value.tx_id,
      sourceKey: membership.key,
      rejectionReason: verdict.ForcedTxInvalid.reason,
    }),
    direction,
  };
  const { threadUtxo } = await requireLinearFaultThreadUtxo({
    lucid: common.lucid,
    contracts: common.contracts,
    categoryId: common.categoryId,
    family: FAMILY,
    stepIndex: 0,
    threadOutRef: common.threadOutRef,
  });
  requireLinearFaultInitialDatum({
    threadUtxo,
    signer: common.signer,
    family: FAMILY,
  });
  return await continueRaw({
    common,
    stepIndex: 0,
    nextAddress: common.contracts.steps[1].spendingScriptAddress,
    nextDatum: datumOf(
      common,
      {
        subject,
        validation_traces_root: header.validationTracesRoot,
        validation_trace_count: header.validationTraceCount,
        script_index: scriptIndex,
      },
      UnusedScriptStep02DatumSchema,
    ),
    redeemerSchema: UnusedScriptStep01RedeemerSchema,
    args: (input_index, output_index) => ({
      source: {
        ForcedSource: {
          input_index,
          output_index,
          header,
          membership,
          direction,
        },
      },
      script_index: scriptIndex,
    }),
  });
};

/**
 * Step 02 with the supplied authentication handed to the validator verbatim;
 * the frontier counts and peaks of the next state come from the caller so a
 * substituted frontier is also refused on chain.
 */
export const submitUnusedStep02Raw = async ({
  authentication,
  frontiers,
  nextStepIndex = 2,
  ...common
}: Common & {
  readonly authentication: UnusedScriptWitnessAuthentication;
  readonly frontiers: Pick<
    Data.Static<typeof UnusedScriptAuthenticatedWitnessSchema>,
    "source_count" | "source_peaks" | "purpose_count" | "purpose_peaks"
  >;
  readonly nextStepIndex?: number;
}) => {
  const bound = await stepState<
    Data.Static<typeof UnusedScriptBoundWitnessSchema>
  >(common, 1, UnusedScriptStep02DatumSchema);
  const authenticated: Data.Static<
    typeof UnusedScriptAuthenticatedWitnessSchema
  > = {
    bound,
    prior_ledger_root: authentication.machine_state.prior_ledger_root,
    language_tag: authentication.language_tag,
    script_hash: authentication.script_hash,
    script_total_length: authentication.total_length,
    item_commitment: authentication.item_commitment,
    ...frontiers,
  };
  return await continueRaw({
    common,
    stepIndex: 1,
    nextAddress: common.contracts.steps[nextStepIndex]!.spendingScriptAddress,
    nextDatum: datumOf(common, authenticated, UnusedScriptStep03DatumSchema),
    redeemerSchema: UnusedScriptStep02RedeemerSchema,
    args: (input_index, output_index) => ({
      ...authentication,
      input_index,
      output_index,
    }),
  });
};

/** Step 03 with the initial scan state supplied by the caller. */
export const submitUnusedStep03Raw = async ({
  nextState,
  nextStepIndex = 3,
  ...common
}: Common & {
  readonly nextState: Data.Static<typeof UnusedScriptReverseScanSchema>;
  readonly nextStepIndex?: number;
}) =>
  await continueRaw({
    common,
    stepIndex: 2,
    nextAddress: common.contracts.steps[nextStepIndex]!.spendingScriptAddress,
    nextDatum: datumOf(common, nextState, UnusedScriptStep04DatumSchema),
    redeemerSchema: UnusedScriptStep03RedeemerSchema,
    args: (input_index, output_index) => ({ input_index, output_index }),
  });

export type ScanState = Data.Static<typeof UnusedScriptReverseScanSchema>;

type SourceOpening = Data.Static<typeof UnusedScriptSourceOpeningSchema>;

export type PurposeOpening = Data.Static<
  typeof UnusedScriptPurposeOpeningSchema
>;

/** Step 04 with the openings, budget and next state supplied verbatim. */
export const submitUnusedStep04Raw = async ({
  openings,
  itemBudget,
  nextState,
  nextStepIndex,
  ...common
}: Common & {
  readonly openings: readonly SourceOpening[];
  readonly itemBudget: bigint;
  readonly nextState: ScanState;
  /** 3 keeps the self-loop, 4 hands over to step 05. */
  readonly nextStepIndex: 3 | 4;
}) =>
  await continueRaw({
    common,
    stepIndex: 3,
    nextAddress: common.contracts.steps[nextStepIndex].spendingScriptAddress,
    nextDatum: datumOf(
      common,
      nextState,
      nextStepIndex === 4
        ? UnusedScriptStep05DatumSchema
        : UnusedScriptStep04DatumSchema,
    ),
    redeemerSchema: UnusedScriptStep04RedeemerSchema,
    args: (input_index, output_index) => ({
      input_index,
      output_index,
      openings,
      item_budget: itemBudget,
    }),
  });
