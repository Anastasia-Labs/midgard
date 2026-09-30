import {
  requireInputIndex,
  requireOwnSpendPurpose,
  requireUniqueOutputIndex,
} from "@al-ft/midgard-sdk";
import { type BuildTxWithRedeemer, Data } from "@lucid-evolution/lucid";

import { submitLinearFaultCancel } from "../linear-fault-cancel.js";
import {
  requireLinearFaultReferenceScript,
  requireLinearFaultStepState,
  requireLinearFaultThreadUtxo,
} from "../linear-fault-family.js";
import { submitLinearFaultFinalize } from "../linear-fault-finalize.js";
import { submitLinearFaultContinue } from "../linear-fault-submit.js";
import { computationThreadOutputPredicate } from "../tx-layout.js";
import type { FaultProofWitnessReferenceScripts } from "../witness-reference-scripts.js";
import type { ScriptIntegrityHashMismatchContracts } from "./contracts.js";
import {
  type ScriptIntegrityHashMismatchEvidence,
  scriptIntegrityHashMismatchEvidenceCloses,
} from "./family.js";
import type { ScriptIntegrityStageThreeAuthentication } from "./retained-stage-three.js";
import {
  AuthenticatedIntegritySchema,
  BoundIntegritySchema,
  IntegrityDecisionSchema,
  IntegrityLanguageFoldSchema,
  IntegrityStep02DatumSchema,
  IntegrityStep02RedeemerSchema,
  IntegrityStep03DatumSchema,
  IntegrityStep03RedeemerSchema,
  IntegrityStep04DatumSchema,
  IntegrityStep04RedeemerSchema,
  IntegrityStep05DatumSchema,
  IntegrityStep05RedeemerSchema,
} from "./schemas.js";
import {
  type Common,
  continueState,
  FAMILY,
} from "./submit.submit-script-integrity-hash-mismatch-step01-forced.js";

export const submitScriptIntegrityHashMismatchStep02 = async (
  args: Common & {
    evidence: ScriptIntegrityHashMismatchEvidence;
    authentication: ScriptIntegrityStageThreeAuthentication;
  },
) => {
  const stepIndex = 1;
  const { threadUtxo, threadToken } = await requireLinearFaultThreadUtxo({
    ...args,
    family: FAMILY,
    stepIndex,
  });
  const bound = requireLinearFaultStepState<
    Data.Static<typeof BoundIntegritySchema>
  >({
    threadUtxo,
    signer: args.signer,
    schema: IntegrityStep02DatumSchema as never,
    family: FAMILY,
    stepIndex,
  });
  const auth = args.authentication;
  if (
    bound.script_integrity_hash !== auth.scriptIntegrityHash ||
    auth.redeemerWitnessHash !== args.evidence.redeemerWitnessHash ||
    auth.control.language_bitmap !==
      BigInt(args.evidence.selectedLanguageBitmap) ||
    auth.control.execution_count !== args.evidence.executionCount ||
    auth.validationTracesRoot !== bound.validation_traces_root ||
    auth.validationTraceCount !== bound.validation_trace_count
  )
    throw new Error(
      `${FAMILY}: retained authentication differs from bound evidence`,
    );
  const authenticated = {
    bound,
    prior_ledger_root: auth.machineState.prior_ledger_root,
    redeemer_witness_hash: auth.redeemerWitnessHash,
    selected_language_bitmap: auth.control.language_bitmap,
    execution_count: auth.control.execution_count,
  };
  const nextDatum = Data.to(
    { fraud_prover: args.signer.paymentKeyHash, data: authenticated } as never,
    IntegrityStep03DatumSchema as never,
  );
  const stepReference = requireLinearFaultReferenceScript({
    utxo: args.referenceScriptUtxo,
    expectedScriptHash: args.contracts.steps[stepIndex].spendingScriptHash,
    family: FAMILY,
    stepIndex,
  });
  const matches = computationThreadOutputPredicate({
    address: args.contracts.steps[2].spendingScriptAddress,
    datum: nextDatum,
    unit: threadToken.unit,
  });
  let outputIndex: bigint | undefined;
  const redeemer = ((ctx) => {
    requireOwnSpendPurpose(ctx, threadUtxo, `${FAMILY} step 02`);
    const inputIndex = requireInputIndex(ctx, threadUtxo, `${FAMILY} step 02`);
    outputIndex = requireUniqueOutputIndex(
      ctx.outputs,
      matches,
      `${FAMILY} step 02`,
    );
    return Data.to(
      {
        Continue: [
          {
            input_index: inputIndex,
            output_index: outputIndex,
            trace_membership: auth.traceMembership,
            machine_state: auth.machineState,
            trace_proof: auth.traceProof,
            control: auth.control,
            redeemer_witness_hash: auth.redeemerWitnessHash,
          },
        ],
      } as never,
      IntegrityStep02RedeemerSchema as never,
    );
  }) satisfies BuildTxWithRedeemer;
  args.signer.selectWallet(args.lucid);
  const txHash = await submitLinearFaultContinue({
    lucid: args.lucid,
    signerPaymentKeyHash: args.signer.paymentKeyHash,
    threadUtxo,
    threadUnit: threadToken.unit,
    stepReference,
    stepScript: args.contracts.steps[stepIndex].spendingScript,
    stepRole: `${FAMILY} step 02`,
    nextAddress: args.contracts.steps[2].spendingScriptAddress,
    nextDatum,
    redeemer,
    preSubmitBoundary: args.preSubmitBoundary,
    awaitConfirmation: args.awaitConfirmation ?? true,
  });
  if (outputIndex === undefined)
    throw new Error(`${FAMILY}: unresolved layout`);
  return { txHash, nextThreadOutRef: `${txHash}#${outputIndex}` };
};

export const submitScriptIntegrityHashMismatchStep03 = async (args: Common) =>
  await continueState({
    args,
    stepIndex: 2,
    currentSchema: IntegrityStep03DatumSchema,
    nextSchema: IntegrityStep04DatumSchema,
    redeemerSchema: IntegrityStep03RedeemerSchema,
    nextStepIndex: 3,
    nextState: (
      authenticated: Data.Static<typeof AuthenticatedIntegritySchema>,
    ) => ({
      authenticated,
      cursor: 0n,
      rebuilt_language_bitmap: 0n,
      selected_language_count: 0n,
    }),
  });

export const submitScriptIntegrityHashMismatchStep04 = async (
  args: Common & { evidence: ScriptIntegrityHashMismatchEvidence },
) => {
  const { threadUtxo } = await requireLinearFaultThreadUtxo({
    ...args,
    family: FAMILY,
    stepIndex: 3,
  });
  const fold = requireLinearFaultStepState<
    Data.Static<typeof IntegrityLanguageFoldSchema>
  >({
    threadUtxo,
    signer: args.signer,
    schema: IntegrityStep04DatumSchema as never,
    family: FAMILY,
    stepIndex: 3,
  });
  if (fold.cursor < 0n || fold.cursor > 1n)
    throw new Error(`${FAMILY}: fold cursor changed`);
  const selected =
    fold.cursor === 0n
      ? fold.authenticated.selected_language_bitmap % 2n === 1n
      : fold.authenticated.selected_language_bitmap >= 2n;
  const nextFold = {
    ...fold,
    cursor: fold.cursor + 1n,
    rebuilt_language_bitmap:
      fold.rebuilt_language_bitmap +
      (selected ? (fold.cursor === 0n ? 1n : 2n) : 0n),
    selected_language_count:
      fold.selected_language_count + (selected ? 1n : 0n),
  };
  const terminal = nextFold.cursor === 2n;
  if (
    fold.authenticated.redeemer_witness_hash !==
      args.evidence.redeemerWitnessHash ||
    fold.authenticated.selected_language_bitmap !==
      BigInt(args.evidence.selectedLanguageBitmap)
  )
    throw new Error(
      `${FAMILY}: fold evidence differs from authenticated state`,
    );
  return {
    ...(await continueState({
      args,
      stepIndex: 3,
      currentSchema: IntegrityStep04DatumSchema,
      nextSchema: terminal
        ? IntegrityStep05DatumSchema
        : IntegrityStep04DatumSchema,
      redeemerSchema: IntegrityStep04RedeemerSchema,
      nextStepIndex: terminal ? 4 : 3,
      nextState: () =>
        terminal
          ? {
              authenticated: nextFold.authenticated,
              expected_hash: args.evidence.expectedHash,
            }
          : nextFold,
    })),
    terminal,
  };
};

export const submitScriptIntegrityHashMismatchStep05 = async (
  args: Common & {
    evidence: ScriptIntegrityHashMismatchEvidence;
    witnessReferenceScripts?: FaultProofWitnessReferenceScripts;
  },
) => {
  const stepIndex = 4;
  const { threadUtxo, threadToken } = await requireLinearFaultThreadUtxo({
    ...args,
    family: FAMILY,
    stepIndex,
  });
  const state = requireLinearFaultStepState<
    Data.Static<typeof IntegrityDecisionSchema>
  >({
    threadUtxo,
    signer: args.signer,
    schema: IntegrityStep05DatumSchema as never,
    family: FAMILY,
    stepIndex,
  });
  if (
    !scriptIntegrityHashMismatchEvidenceCloses(args.evidence) ||
    state.expected_hash !== args.evidence.expectedHash ||
    state.authenticated.bound.script_integrity_hash !==
      args.evidence.scriptIntegrityHash
  )
    throw new Error(
      `${FAMILY}: terminal state is not the retained contradiction`,
    );
  return await submitLinearFaultFinalize({
    lucid: args.lucid,
    family: FAMILY,
    stepIndex,
    step: args.contracts.steps[stepIndex],
    computationThread: args.contracts.computationThread,
    fraudProof: args.contracts.fraudProof,
    signer: args.signer,
    threadUtxo,
    threadToken,
    spendRedeemerSchema: IntegrityStep05RedeemerSchema,
    buildFamilyArgs: (layout) => ({
      input_index: layout.inputIndex,
      output_index: layout.outputIndex,
      fraud_proof_mint_redeemer_index: layout.fraudProofMintRedeemerIndex,
    }),
    referenceScriptUtxo: args.referenceScriptUtxo,
    witnessReferenceScripts: args.witnessReferenceScripts,
    preSubmitBoundary: args.preSubmitBoundary,
    awaitConfirmation: args.awaitConfirmation ?? true,
  });
};

export const submitScriptIntegrityHashMismatchCancel = async (
  args: Omit<
    Parameters<typeof submitLinearFaultCancel>[0],
    "family" | "steps" | "computationThread"
  > & { contracts: ScriptIntegrityHashMismatchContracts },
) => {
  const { contracts, ...rest } = args;
  return await submitLinearFaultCancel({
    ...rest,
    family: FAMILY,
    steps: contracts.steps,
    computationThread: contracts.computationThread,
  });
};
