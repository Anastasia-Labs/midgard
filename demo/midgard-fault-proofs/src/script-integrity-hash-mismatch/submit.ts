import {
  acceptedVerdictSubject,
  type ForcedInclusionTxV1,
  forcedVerdictSubject,
  type Header,
  type OutputReference,
  requireInputIndex,
  requireOwnSpendPurpose,
  requireUniqueOutputIndex,
  type RootMembershipProof,
} from "@al-ft/midgard-sdk";
import {
  type BuildTxWithRedeemer,
  Data,
  type LucidEvolution,
  type UTxO,
} from "@lucid-evolution/lucid";

import { submitLinearFaultCancel } from "../linear-fault-cancel.js";
import {
  requireLinearFaultInitialDatum,
  requireLinearFaultReferenceScript,
  requireLinearFaultStepState,
  requireLinearFaultThreadUtxo,
} from "../linear-fault-family.js";
import { submitLinearFaultFinalize } from "../linear-fault-finalize.js";
import { submitLinearFaultContinue } from "../linear-fault-submit.js";
import type { MissingNativeScriptTxContracts } from "../missing-native-script-tx/contracts.js";
import { submitMissingNativeScriptTxBinding } from "../missing-native-script-tx/submit-native-binding.js";
import type { ResolvedProverSigner } from "../runtime.js";
import type { SubmitStep01TxInclusion } from "../submit-step-01.js";
import { computationThreadOutputPredicate } from "../tx-layout.js";
import type { FaultProofWitnessReferenceScripts } from "../witness-reference-scripts.js";
import type { FraudProofPreSubmitBoundary } from "../workflow/transaction-boundary.js";
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
  IntegrityStep01RedeemerSchema,
  IntegrityStep02DatumSchema,
  IntegrityStep02RedeemerSchema,
  IntegrityStep03DatumSchema,
  IntegrityStep03RedeemerSchema,
  IntegrityStep04DatumSchema,
  IntegrityStep04RedeemerSchema,
  IntegrityStep05DatumSchema,
  IntegrityStep05RedeemerSchema,
} from "./schemas.js";

const FAMILY = "script-integrity-hash-mismatch";
type Common = Readonly<{
  lucid: LucidEvolution;
  contracts: ScriptIntegrityHashMismatchContracts;
  categoryId: string;
  signer: ResolvedProverSigner;
  threadOutRef: string;
  referenceScriptUtxo: UTxO;
  preSubmitBoundary?: FraudProofPreSubmitBoundary;
  awaitConfirmation?: boolean;
}>;

const boundState = (
  subject: ReturnType<typeof acceptedVerdictSubject>,
  header: Header,
  scriptIntegrityHash: string,
) => ({
  subject,
  validation_traces_root: header.validationTracesRoot,
  validation_trace_count: header.validationTraceCount,
  script_integrity_hash: scriptIntegrityHash,
});

export const submitScriptIntegrityHashMismatchStep01Accepted = async (
  args: Common & {
    blueprint: unknown;
    network: Parameters<
      typeof submitMissingNativeScriptTxBinding
    >[0]["network"];
    stateQueueBlockOutRef: string;
    txInclusion: SubmitStep01TxInclusion;
    header: Header;
    evidence: ScriptIntegrityHashMismatchEvidence;
    witnessReferenceScripts?: FaultProofWitnessReferenceScripts;
  },
) => {
  const subject = acceptedVerdictSubject(args.txInclusion.nativeTxId);
  if (
    subject.transaction_id !== args.evidence.finding.subject.transaction_id ||
    args.evidence.finding.subject.direction !== subject.direction
  )
    throw new Error(`${FAMILY}: accepted source differs from evidence`);
  const { threadUtxo, threadToken } = await requireLinearFaultThreadUtxo({
    ...args,
    family: FAMILY,
    stepIndex: 0,
  });
  requireLinearFaultInitialDatum({
    threadUtxo,
    signer: args.signer,
    family: FAMILY,
  });
  const nextDatum = Data.to(
    {
      fraud_prover: args.signer.paymentKeyHash,
      data: boundState(subject, args.header, args.evidence.scriptIntegrityHash),
    } as never,
    IntegrityStep02DatumSchema as never,
  );
  return await submitMissingNativeScriptTxBinding({
    lucid: args.lucid,
    blueprint: args.blueprint,
    network: args.network,
    contracts: args.contracts as unknown as MissingNativeScriptTxContracts,
    signer: args.signer,
    stepIndex: 0,
    threadUtxo,
    threadToken,
    stateQueueBlockOutRef: args.stateQueueBlockOutRef,
    txInclusion: args.txInclusion,
    nextDatum,
    spendRedeemerSchema: IntegrityStep01RedeemerSchema,
    wrapInclusionArgs: (sourceArgs) => ({
      source: {
        AcceptedSource: {
          inclusion: { RedeemerCarriedInclusion: [sourceArgs] },
        },
      },
    }),
    referenceScriptUtxo: args.referenceScriptUtxo,
    witnessReferenceScripts: args.witnessReferenceScripts,
    preSubmitBoundary: args.preSubmitBoundary,
    awaitConfirmation: args.awaitConfirmation ?? true,
  });
};

export const submitScriptIntegrityHashMismatchStep01Forced = async (
  args: Common & {
    header: Header;
    membership: RootMembershipProof<OutputReference, ForcedInclusionTxV1>;
    evidence: ScriptIntegrityHashMismatchEvidence;
  },
) => {
  const verdict = args.membership.value.verdict;
  if (verdict === "ForcedTxValid")
    throw new Error(`${FAMILY}: forced-valid source`);
  const subject = forcedVerdictSubject({
    transactionId: args.membership.value.tx_id,
    sourceKey: args.membership.key,
    rejectionReason: verdict.ForcedTxInvalid.reason,
  });
  if (
    subject.transaction_id !== args.evidence.finding.subject.transaction_id ||
    subject.source_key !== args.evidence.finding.subject.source_key ||
    subject.direction !== args.evidence.finding.subject.direction
  )
    throw new Error(`${FAMILY}: forced source differs from evidence`);
  const { threadUtxo, threadToken } = await requireLinearFaultThreadUtxo({
    ...args,
    family: FAMILY,
    stepIndex: 0,
  });
  requireLinearFaultInitialDatum({
    threadUtxo,
    signer: args.signer,
    family: FAMILY,
  });
  const nextDatum = Data.to(
    {
      fraud_prover: args.signer.paymentKeyHash,
      data: boundState(
        subject as never,
        args.header,
        args.evidence.scriptIntegrityHash,
      ),
    } as never,
    IntegrityStep02DatumSchema as never,
  );
  const stepReference = requireLinearFaultReferenceScript({
    utxo: args.referenceScriptUtxo,
    expectedScriptHash: args.contracts.steps[0].spendingScriptHash,
    family: FAMILY,
    stepIndex: 0,
  });
  const matches = computationThreadOutputPredicate({
    address: args.contracts.steps[1].spendingScriptAddress,
    datum: nextDatum,
    unit: threadToken.unit,
  });
  let outputIndex: bigint | undefined;
  const redeemer = ((ctx) => {
    requireOwnSpendPurpose(ctx, threadUtxo, `${FAMILY} step 01`);
    const inputIndex = requireInputIndex(ctx, threadUtxo, `${FAMILY} step 01`);
    outputIndex = requireUniqueOutputIndex(
      ctx.outputs,
      matches,
      `${FAMILY} step 01`,
    );
    return Data.to(
      {
        Continue: [
          {
            source: {
              ForcedSource: {
                input_index: inputIndex,
                output_index: outputIndex,
                header: args.header,
                membership: args.membership,
                direction: subject.direction,
              },
            },
          },
        ],
      } as never,
      IntegrityStep01RedeemerSchema as never,
    );
  }) satisfies BuildTxWithRedeemer;
  args.signer.selectWallet(args.lucid);
  const txHash = await submitLinearFaultContinue({
    lucid: args.lucid,
    signerPaymentKeyHash: args.signer.paymentKeyHash,
    threadUtxo,
    threadUnit: threadToken.unit,
    stepReference,
    stepScript: args.contracts.steps[0].spendingScript,
    stepRole: `${FAMILY} step 01`,
    nextAddress: args.contracts.steps[1].spendingScriptAddress,
    nextDatum,
    redeemer,
    preSubmitBoundary: args.preSubmitBoundary,
    awaitConfirmation: args.awaitConfirmation ?? true,
  });
  if (outputIndex === undefined)
    throw new Error(`${FAMILY}: unresolved layout`);
  return { txHash, nextThreadOutRef: `${txHash}#${outputIndex}` };
};

const continueState = async <State>({
  args,
  stepIndex,
  currentSchema,
  nextSchema,
  nextState,
  redeemerSchema,
  nextStepIndex,
}: {
  args: Common;
  stepIndex: number;
  currentSchema: unknown;
  nextSchema: unknown;
  nextState: (state: State) => unknown;
  redeemerSchema: unknown;
  nextStepIndex: number;
}) => {
  const { threadUtxo, threadToken } = await requireLinearFaultThreadUtxo({
    ...args,
    family: FAMILY,
    stepIndex,
  });
  const state = requireLinearFaultStepState<State>({
    threadUtxo,
    signer: args.signer,
    schema: currentSchema as never,
    family: FAMILY,
    stepIndex,
  });
  const nextDatum = Data.to(
    {
      fraud_prover: args.signer.paymentKeyHash,
      data: nextState(state),
    } as never,
    nextSchema as never,
  );
  const stepReference = requireLinearFaultReferenceScript({
    utxo: args.referenceScriptUtxo,
    expectedScriptHash: args.contracts.steps[stepIndex]!.spendingScriptHash,
    family: FAMILY,
    stepIndex,
  });
  const matches = computationThreadOutputPredicate({
    address: args.contracts.steps[nextStepIndex]!.spendingScriptAddress,
    datum: nextDatum,
    unit: threadToken.unit,
  });
  let outputIndex: bigint | undefined;
  const redeemer = ((ctx) => {
    requireOwnSpendPurpose(ctx, threadUtxo, `${FAMILY} step ${stepIndex + 1}`);
    const inputIndex = requireInputIndex(
      ctx,
      threadUtxo,
      `${FAMILY} step ${stepIndex + 1}`,
    );
    outputIndex = requireUniqueOutputIndex(
      ctx.outputs,
      matches,
      `${FAMILY} step ${stepIndex + 1}`,
    );
    return Data.to(
      {
        Continue: [{ input_index: inputIndex, output_index: outputIndex }],
      } as never,
      redeemerSchema as never,
    );
  }) satisfies BuildTxWithRedeemer;
  args.signer.selectWallet(args.lucid);
  const txHash = await submitLinearFaultContinue({
    lucid: args.lucid,
    signerPaymentKeyHash: args.signer.paymentKeyHash,
    threadUtxo,
    threadUnit: threadToken.unit,
    stepReference,
    stepScript: args.contracts.steps[stepIndex]!.spendingScript,
    stepRole: `${FAMILY} step ${stepIndex + 1}`,
    nextAddress: args.contracts.steps[nextStepIndex]!.spendingScriptAddress,
    nextDatum,
    redeemer,
    preSubmitBoundary: args.preSubmitBoundary,
    awaitConfirmation: args.awaitConfirmation ?? true,
  });
  if (outputIndex === undefined)
    throw new Error(`${FAMILY}: unresolved layout`);
  return { txHash, nextThreadOutRef: `${txHash}#${outputIndex}` };
};

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
