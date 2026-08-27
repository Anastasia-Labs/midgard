/** Resume-safe step submitters shared by the four non-terminal handoffs. */
import {
  requireInputIndex,
  requireOwnSpendPurpose,
  requireReferenceInputIndex,
  requireUniqueOutputIndex,
  withdrawalClaimsValidV1,
  type WithdrawalMistagPreparedEvidenceV1,
  WithdrawalMistagStep01Datum,
  WithdrawalMistagStep01SpendRedeemer,
  WithdrawalMistagStep02Datum,
  WithdrawalMistagStep02SpendRedeemer,
  WithdrawalMistagStep03Datum,
  WithdrawalMistagStep03SpendRedeemer,
  WithdrawalMistagStep04Datum,
  WithdrawalMistagStep04SpendRedeemer,
  WithdrawalMistagStep05Datum,
} from "@al-ft/midgard-sdk";
import {
  type BuildTxWithRedeemer,
  Data,
  type LucidEvolution,
  type UTxO,
} from "@lucid-evolution/lucid";

import {
  DEFAULT_CONFIRMATION_POLL_MS,
  type ResolvedProverSigner,
} from "../runtime.js";
import { selectFeeInput } from "../submit-step-01.js";
import { computationThreadOutputPredicate } from "../tx-layout.js";
import type { WithdrawalMistagContractsV1 } from "./contracts-v1.js";
import {
  requireWithdrawalMistagReferenceScriptV1,
  requireWithdrawalMistagThreadUtxoV1,
  withdrawalMistagError,
  withdrawalMistagStepLabelV1,
} from "./submit-common-v1.js";

type IntermediateStepIndex = 0 | 1 | 2 | 3;

const datumSchemas = [
  WithdrawalMistagStep01Datum,
  WithdrawalMistagStep02Datum,
  WithdrawalMistagStep03Datum,
  WithdrawalMistagStep04Datum,
  WithdrawalMistagStep05Datum,
] as const;

const redeemerSchemas = [
  WithdrawalMistagStep01SpendRedeemer,
  WithdrawalMistagStep02SpendRedeemer,
  WithdrawalMistagStep03SpendRedeemer,
  WithdrawalMistagStep04SpendRedeemer,
] as const;

export const withdrawalMistagStatesV1 = (
  prepared: WithdrawalMistagPreparedEvidenceV1,
) => {
  const info = prepared.committedWithdrawal.value;
  const claimedValid = withdrawalClaimsValidV1(info);
  const step02 = {
    challenged_header_hash: prepared.challengedHeaderHash,
    withdrawal_id: prepared.committedWithdrawal.key,
    withdrawal_info_hash: prepared.withdrawalInfoHash,
    claimed_valid: claimedValid,
    event_to_step_root: prepared.eventToStep.root,
    total_event_count: prepared.eventToStep.count,
    transition_trace_root: prepared.transitionStep.root,
    transition_step_count: prepared.transitionStep.count,
  };
  const step03 = {
    challenged_header_hash: prepared.challengedHeaderHash,
    withdrawal_id: prepared.committedWithdrawal.key,
    withdrawal_info_hash: prepared.withdrawalInfoHash,
    claimed_valid: claimedValid,
    pre_utxos_root: prepared.transitionStep.value.pre_utxos_root,
  };
  const step04 = {
    challenged_header_hash: prepared.challengedHeaderHash,
    withdrawal_id: prepared.committedWithdrawal.key,
    withdrawal_body_hash: prepared.withdrawalBodyHash,
    claimed_valid: claimedValid,
    output_present: prepared.outputPresent,
    core_valid: prepared.coreValid,
    cardano_value_size: prepared.cardanoValueSize,
  };
  const step05 = {
    challenged_header_hash: prepared.challengedHeaderHash,
    withdrawal_id: prepared.committedWithdrawal.key,
    claimed_valid: claimedValid,
    actual_valid: prepared.actualValid,
    exact_output_bytes: prepared.exactOutputBytes,
    required_lovelace: prepared.requiredLovelace,
  };
  return [null, step02, step03, step04, step05] as const;
};

const requireLiveDatum = ({
  threadUtxo,
  signer,
  stepIndex,
  expectedState,
}: {
  readonly threadUtxo: UTxO;
  readonly signer: ResolvedProverSigner;
  readonly stepIndex: IntermediateStepIndex;
  readonly expectedState: unknown;
}): void => {
  if (threadUtxo.datum == null) {
    throw withdrawalMistagError(
      `${withdrawalMistagStepLabelV1(stepIndex)} has no datum`,
    );
  }
  const decoded = Data.from(
    threadUtxo.datum,
    datumSchemas[stepIndex] as never,
  ) as {
    readonly fraud_prover: string;
    readonly data: unknown;
  };
  if (decoded.fraud_prover !== signer.paymentKeyHash) {
    throw withdrawalMistagError("live thread belongs to another fraud prover");
  }
  if (
    Data.to(decoded as never, datumSchemas[stepIndex] as never) !==
    Data.to(
      {
        fraud_prover: signer.paymentKeyHash,
        data: expectedState,
      } as never,
      datumSchemas[stepIndex] as never,
    )
  ) {
    throw withdrawalMistagError(
      `${withdrawalMistagStepLabelV1(stepIndex)} state does not match prepared evidence`,
    );
  }
};

const stepArgs = ({
  stepIndex,
  prepared,
  inputIndex,
  outputIndex,
  ctx,
  hubOracleUtxo,
  stateQueueBlockUtxo,
}: {
  readonly stepIndex: IntermediateStepIndex;
  readonly prepared: WithdrawalMistagPreparedEvidenceV1;
  readonly inputIndex: bigint;
  readonly outputIndex: bigint;
  readonly ctx: Parameters<BuildTxWithRedeemer>[0];
  readonly hubOracleUtxo?: UTxO;
  readonly stateQueueBlockUtxo?: UTxO;
}): unknown => {
  const common = { input_index: inputIndex, output_index: outputIndex };
  switch (stepIndex) {
    case 0:
      if (hubOracleUtxo === undefined || stateQueueBlockUtxo === undefined) {
        throw withdrawalMistagError(
          "step 01 requires hub and state-queue references",
        );
      }
      return {
        ...common,
        hub_ref_input_index: requireReferenceInputIndex(
          ctx,
          hubOracleUtxo,
          "withdrawal-mistag hub oracle",
        ),
        state_queue_node_ref_input_index: requireReferenceInputIndex(
          ctx,
          stateQueueBlockUtxo,
          "withdrawal-mistag state-queue node",
        ),
        committed_withdrawal: prepared.committedWithdrawal,
      };
    case 1:
      return {
        ...common,
        withdrawal_info: prepared.committedWithdrawal.value,
        event_to_step: prepared.eventToStep,
        transition_step: prepared.transitionStep,
      };
    case 2:
      return {
        ...common,
        withdrawal_info: prepared.committedWithdrawal.value,
        evidence: prepared.ledgerEvidence,
      };
    case 3:
      return {
        ...common,
        withdrawal_body: prepared.committedWithdrawal.value.body,
      };
  }
};

export type SubmitWithdrawalMistagStepResult = {
  readonly txHash: string;
  readonly threadOutRef: string;
  readonly nextThreadOutRef: string;
  readonly stepIndex: IntermediateStepIndex;
  readonly nextStepIndex: 1 | 2 | 3 | 4;
  readonly inputIndex: number;
  readonly outputIndex: number;
  readonly awaitedConfirmation: boolean;
};

export const submitWithdrawalMistagIntermediateStep = async ({
  lucid,
  contracts,
  signer,
  prepared,
  stepIndex,
  threadOutRef,
  hubOracleUtxo,
  stateQueueBlockUtxo,
  referenceScriptUtxo,
  awaitConfirmation = true,
}: {
  readonly lucid: LucidEvolution;
  readonly contracts: WithdrawalMistagContractsV1;
  readonly signer: ResolvedProverSigner;
  readonly prepared: WithdrawalMistagPreparedEvidenceV1;
  readonly stepIndex: IntermediateStepIndex;
  readonly threadOutRef: string;
  readonly hubOracleUtxo?: UTxO;
  readonly stateQueueBlockUtxo?: UTxO;
  /** Required for every step because applied step 03 is larger than the L1 envelope. */
  readonly referenceScriptUtxo: UTxO;
  readonly awaitConfirmation?: boolean;
}): Promise<SubmitWithdrawalMistagStepResult> => {
  const { threadUtxo, threadToken } = await requireWithdrawalMistagThreadUtxoV1(
    {
      lucid,
      contracts,
      stepIndex,
      threadOutRef,
    },
  );
  const states = withdrawalMistagStatesV1(prepared);
  requireLiveDatum({
    threadUtxo,
    signer,
    stepIndex,
    expectedState: states[stepIndex],
  });
  const nextState = states[stepIndex + 1];
  const nextDatum = Data.to(
    { fraud_prover: signer.paymentKeyHash, data: nextState } as never,
    datumSchemas[stepIndex + 1] as never,
  );
  const outputMatches = computationThreadOutputPredicate({
    address: contracts.steps[stepIndex + 1].spendingScriptAddress,
    datum: nextDatum,
    unit: threadToken.unit,
  });
  const references = [
    ...(hubOracleUtxo === undefined ? [] : [hubOracleUtxo]),
    ...(stateQueueBlockUtxo === undefined ? [] : [stateQueueBlockUtxo]),
    requireWithdrawalMistagReferenceScriptV1({
      utxo: referenceScriptUtxo,
      contracts,
      stepIndex,
    }),
  ];
  let resolved:
    | { readonly inputIndex: bigint; readonly outputIndex: bigint }
    | undefined;
  const redeemer = ((ctx) => {
    requireOwnSpendPurpose(
      ctx,
      threadUtxo,
      withdrawalMistagStepLabelV1(stepIndex),
    );
    const inputIndex = requireInputIndex(
      ctx,
      threadUtxo,
      withdrawalMistagStepLabelV1(stepIndex),
    );
    const outputIndex = requireUniqueOutputIndex(
      ctx.outputs,
      outputMatches,
      `${withdrawalMistagStepLabelV1(stepIndex)} output`,
    );
    resolved = { inputIndex, outputIndex };
    return Data.to(
      {
        Continue: [
          stepArgs({
            stepIndex,
            prepared,
            inputIndex,
            outputIndex,
            ctx,
            hubOracleUtxo,
            stateQueueBlockUtxo,
          }),
        ],
      } as never,
      redeemerSchemas[stepIndex] as never,
    );
  }) satisfies BuildTxWithRedeemer;

  signer.selectWallet(lucid);
  const feeInput = selectFeeInput(await lucid.wallet().getUtxos());
  const base = lucid
    .newTx()
    .collectFrom([feeInput])
    .collectFrom([threadUtxo], redeemer)
    .readFrom(references)
    .pay.ToContract(
      contracts.steps[stepIndex + 1].spendingScriptAddress,
      { kind: "inline", value: nextDatum },
      threadUtxo.assets,
    )
    .addSignerKey(signer.paymentKeyHash);
  const unsigned = await base.complete({ localUPLCEval: true });
  if (resolved === undefined) {
    throw withdrawalMistagError(
      "transaction builder did not resolve step layout",
    );
  }
  const signed = await unsigned.sign.withWallet().complete();
  const txHash = await signed.submit();
  if (awaitConfirmation)
    await lucid.awaitTx(txHash, DEFAULT_CONFIRMATION_POLL_MS);
  return {
    txHash,
    threadOutRef,
    nextThreadOutRef: `${txHash}#${resolved.outputIndex.toString()}`,
    stepIndex,
    nextStepIndex: (stepIndex + 1) as 1 | 2 | 3 | 4,
    inputIndex: Number(resolved.inputIndex),
    outputIndex: Number(resolved.outputIndex),
    awaitedConfirmation: awaitConfirmation,
  };
};

export const submitWithdrawalMistagStep01 = (
  args: Omit<
    Parameters<typeof submitWithdrawalMistagIntermediateStep>[0],
    "stepIndex"
  >,
) => submitWithdrawalMistagIntermediateStep({ ...args, stepIndex: 0 });
export const submitWithdrawalMistagStep02 = (
  args: Omit<
    Parameters<typeof submitWithdrawalMistagIntermediateStep>[0],
    "stepIndex"
  >,
) => submitWithdrawalMistagIntermediateStep({ ...args, stepIndex: 1 });
export const submitWithdrawalMistagStep03 = (
  args: Omit<
    Parameters<typeof submitWithdrawalMistagIntermediateStep>[0],
    "stepIndex"
  >,
) => submitWithdrawalMistagIntermediateStep({ ...args, stepIndex: 2 });
export const submitWithdrawalMistagStep04 = (
  args: Omit<
    Parameters<typeof submitWithdrawalMistagIntermediateStep>[0],
    "stepIndex"
  >,
) => submitWithdrawalMistagIntermediateStep({ ...args, stepIndex: 3 });
