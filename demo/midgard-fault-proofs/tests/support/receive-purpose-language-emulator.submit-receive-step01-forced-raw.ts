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
import { submitLinearFaultFinalize } from "../../src/linear-fault-finalize.js";
import { submitLinearFaultContinue } from "../../src/linear-fault-submit.js";
import type { ReceivePurposeLanguageContracts } from "../../src/receive-purpose-language/contracts.js";
import {
  AuthenticatedReceiveLanguageSchema,
  ReceivePurposeBoundExecutionSchema,
  ReceivePurposeStep01RedeemerSchema,
  ReceivePurposeStep02DatumSchema,
  ReceivePurposeStep02RedeemerSchema,
  ReceivePurposeStep03DatumSchema,
  ReceivePurposeStep03RedeemerSchema,
} from "../../src/receive-purpose-language/schemas.js";
import type { ReceivePurposeLanguageAuthentication } from "../../src/receive-purpose-language/submit-step-02.js";
import type { ResolvedProverSigner } from "../../src/runtime.js";
import { computationThreadOutputPredicate } from "../../src/tx-layout.js";
import type { FaultProofWitnessReferenceScripts } from "../../src/witness-reference-scripts.js";
import { buildReceivePurposeFixture } from "./receive-purpose-language-emulator.build-receive-purpose-fixture.js";
import { FAMILY } from "./receive-purpose-language-emulator.receive-purpose-fixture-spec.js";

export type ReceivePurposeFixture = Awaited<
  ReturnType<typeof buildReceivePurposeFixture>
>;

type Common = Readonly<{
  lucid: LucidEvolution;
  contracts: ReceivePurposeLanguageContracts;
  categoryId: string;
  signer: ResolvedProverSigner;
  threadOutRef: string;
  referenceScriptUtxo: UTxO;
}>;

/**
 * Step 01 over a forced leaf with no off-chain classification: the exact
 * redeemer the test asks for reaches the validator, so reason-coordinate,
 * header, membership and direction mutations are refused on chain.
 */
export const submitReceiveStep01ForcedRaw = async ({
  lucid,
  contracts,
  categoryId,
  signer,
  threadOutRef,
  header,
  membership,
  executionIndex,
  direction,
  referenceScriptUtxo,
}: Common & {
  readonly header: SDK.Header;
  readonly membership: SDK.RootMembershipProof<
    SDK.OutputReference,
    SDK.ForcedInclusionTxV1
  >;
  readonly executionIndex: bigint;
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
  const { threadUtxo, threadToken } = await requireLinearFaultThreadUtxo({
    lucid,
    contracts,
    categoryId,
    family: FAMILY,
    stepIndex: 0,
    threadOutRef,
  });
  requireLinearFaultInitialDatum({ threadUtxo, signer, family: FAMILY });
  const nextDatum = Data.to(
    {
      fraud_prover: signer.paymentKeyHash,
      data: {
        subject,
        validation_traces_root: header.validationTracesRoot,
        validation_trace_count: header.validationTraceCount,
        execution_index: executionIndex,
      },
    } as never,
    ReceivePurposeStep02DatumSchema as never,
  );
  const stepReference = requireLinearFaultReferenceScript({
    utxo: referenceScriptUtxo,
    expectedScriptHash: contracts.steps[0].spendingScriptHash,
    family: FAMILY,
    stepIndex: 0,
  });
  const outputMatches = computationThreadOutputPredicate({
    address: contracts.steps[1].spendingScriptAddress,
    datum: nextDatum,
    unit: threadToken.unit,
  });
  let outputIndex: bigint | undefined;
  const redeemer = ((ctx) => {
    SDK.requireOwnSpendPurpose(ctx, threadUtxo, "raw step 01");
    const inputIndex = SDK.requireInputIndex(ctx, threadUtxo, "raw step 01");
    outputIndex = SDK.requireUniqueOutputIndex(
      ctx.outputs,
      outputMatches,
      "raw step 01",
    );
    return Data.to(
      {
        Continue: [
          {
            source: {
              ForcedSource: {
                input_index: inputIndex,
                output_index: outputIndex,
                header,
                membership,
                direction,
              },
            },
            execution_index: executionIndex,
          },
        ],
      } as never,
      ReceivePurposeStep01RedeemerSchema as never,
    );
  }) satisfies BuildTxWithRedeemer;
  signer.selectWallet(lucid);
  const txHash = await submitLinearFaultContinue({
    lucid,
    signerPaymentKeyHash: signer.paymentKeyHash,
    threadUtxo,
    threadUnit: threadToken.unit,
    stepReference,
    stepScript: contracts.steps[0].spendingScript,
    stepRole: "raw step 01",
    nextAddress: contracts.steps[1].spendingScriptAddress,
    nextDatum,
    redeemer,
    awaitConfirmation: true,
  });
  if (outputIndex === undefined) throw new Error("raw step 01: no layout");
  return { txHash, nextThreadOutRef: `${txHash}#${outputIndex.toString()}` };
};

/**
 * Step 02 with the supplied authentication handed to the validator verbatim
 * (no evidence cross-check), so every authentication-seam substitution is
 * refused by the script rather than by the builder.
 */
export const submitReceiveStep02Raw = async ({
  lucid,
  contracts,
  categoryId,
  signer,
  threadOutRef,
  authentication,
  referenceScriptUtxo,
}: Common & {
  readonly authentication: ReceivePurposeLanguageAuthentication;
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
  const bound = requireLinearFaultStepState<
    Data.Static<typeof ReceivePurposeBoundExecutionSchema>
  >({
    threadUtxo,
    signer,
    schema: ReceivePurposeStep02DatumSchema as never,
    family: FAMILY,
    stepIndex,
  });
  const authenticated: Data.Static<typeof AuthenticatedReceiveLanguageSchema> =
    {
      bound,
      prior_ledger_root: authentication.machine_state.prior_ledger_root,
      purpose_kind: 3n,
      purpose_index: authentication.purpose_index,
      source_index: authentication.source_index,
      origin_kind: authentication.origin_kind,
      source_key: authentication.source_key,
      language_tag: authentication.language_tag,
      script_hash: authentication.script_hash,
    };
  const nextDatum = Data.to(
    { fraud_prover: signer.paymentKeyHash, data: authenticated } as never,
    ReceivePurposeStep03DatumSchema as never,
  );
  const outputMatches = computationThreadOutputPredicate({
    address: contracts.steps[2].spendingScriptAddress,
    datum: nextDatum,
    unit: threadToken.unit,
  });
  const stepReference = requireLinearFaultReferenceScript({
    utxo: referenceScriptUtxo,
    expectedScriptHash: contracts.steps[stepIndex].spendingScriptHash,
    family: FAMILY,
    stepIndex,
  });
  let outputIndex: bigint | undefined;
  const redeemer = ((ctx) => {
    SDK.requireOwnSpendPurpose(ctx, threadUtxo, "raw step 02");
    const inputIndex = SDK.requireInputIndex(ctx, threadUtxo, "raw step 02");
    outputIndex = SDK.requireUniqueOutputIndex(
      ctx.outputs,
      outputMatches,
      "raw step 02",
    );
    return Data.to(
      {
        Continue: [
          {
            ...authentication,
            input_index: inputIndex,
            output_index: outputIndex,
          },
        ],
      } as never,
      ReceivePurposeStep02RedeemerSchema as never,
    );
  }) satisfies BuildTxWithRedeemer;
  signer.selectWallet(lucid);
  const txHash = await submitLinearFaultContinue({
    lucid,
    signerPaymentKeyHash: signer.paymentKeyHash,
    threadUtxo,
    threadUnit: threadToken.unit,
    stepReference,
    stepScript: contracts.steps[stepIndex].spendingScript,
    stepRole: "raw step 02",
    nextAddress: contracts.steps[2].spendingScriptAddress,
    nextDatum,
    redeemer,
    awaitConfirmation: true,
  });
  if (outputIndex === undefined) throw new Error("raw step 02: no layout");
  return { txHash, nextThreadOutRef: `${txHash}#${outputIndex.toString()}` };
};

/** Step 03 without the off-chain contradiction guard: the validator decides. */
export const submitReceiveStep03Raw = async ({
  lucid,
  contracts,
  categoryId,
  signer,
  threadOutRef,
  referenceScriptUtxo,
  witnessReferenceScripts,
}: Common & {
  readonly witnessReferenceScripts?: FaultProofWitnessReferenceScripts;
}) => {
  const stepIndex = 2;
  const { threadUtxo, threadToken } = await requireLinearFaultThreadUtxo({
    lucid,
    contracts,
    categoryId,
    family: FAMILY,
    stepIndex,
    threadOutRef,
  });
  return await submitLinearFaultFinalize({
    lucid,
    family: FAMILY,
    stepIndex,
    step: contracts.steps[stepIndex],
    computationThread: contracts.computationThread,
    fraudProof: contracts.fraudProof,
    signer,
    threadUtxo,
    threadToken,
    spendRedeemerSchema: ReceivePurposeStep03RedeemerSchema,
    buildFamilyArgs: (layout) => ({
      input_index: layout.inputIndex,
      output_index: layout.outputIndex,
      fraud_proof_mint_redeemer_index: layout.fraudProofMintRedeemerIndex,
    }),
    referenceScriptUtxo,
    witnessReferenceScripts,
    awaitConfirmation: true,
  });
};
