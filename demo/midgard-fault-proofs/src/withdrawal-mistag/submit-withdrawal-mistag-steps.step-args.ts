import { aikenSerialisedPlutusDataCborPreservingMapOrder } from "@al-ft/midgard-core/plutus-data-cbor";
import {
  WithdrawalMistagStep01PayloadSchema,
  WithdrawalMistagStep02PayloadSchema,
  WithdrawalMistagStep03PayloadSchema,
  WithdrawalMistagStep04PayloadSchema,
} from "@al-ft/midgard-sdk";
import {
  requireReferenceInputIndex,
  withdrawalClaimsValid,
  type WithdrawalMistagPreparedEvidence,
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
  type UTxO,
} from "@lucid-evolution/lucid";

import { type ResolvedProverSigner } from "../runtime.js";
import {
  structuredDataPublicationPlan,
  structuredDataTreeData,
} from "../workflow/structured-data-preimage.js";
import {
  withdrawalMistagError,
  withdrawalMistagStepLabel,
} from "./submit-common.js";

export type IntermediateStepIndex = 0 | 1 | 2 | 3;

export const datumSchemas = [
  WithdrawalMistagStep01Datum,
  WithdrawalMistagStep02Datum,
  WithdrawalMistagStep03Datum,
  WithdrawalMistagStep04Datum,
  WithdrawalMistagStep05Datum,
] as const;

export const redeemerSchemas = [
  WithdrawalMistagStep01SpendRedeemer,
  WithdrawalMistagStep02SpendRedeemer,
  WithdrawalMistagStep03SpendRedeemer,
  WithdrawalMistagStep04SpendRedeemer,
] as const;

export const referenceScriptRoles = [
  "V1 fraud-proof withdrawal-mistag step-01",
  "V1 fraud-proof withdrawal-mistag step-02",
  "V1 fraud-proof withdrawal-mistag step-03",
  "V1 fraud-proof withdrawal-mistag step-04",
] as const;

export const withdrawalMistagStates = (
  prepared: WithdrawalMistagPreparedEvidence,
) => {
  const info = prepared.committedWithdrawal.value;
  const claimedValid = withdrawalClaimsValid(info);
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
    owner_signature_valid: prepared.ownerSignatureValid,
    output_lovelace: prepared.outputLovelace,
    output_asset_count: prepared.outputAssetCount,
    output_asset_frontier_commitment: prepared.outputAssetFrontierCommitment,
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

export const requireLiveDatum = ({
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
      `${withdrawalMistagStepLabel(stepIndex)} has no datum`,
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
      `${withdrawalMistagStepLabel(stepIndex)} state does not match prepared evidence`,
    );
  }
};

const inlineStepArgs = ({
  stepIndex,
  prepared,
  inputIndex,
  outputIndex,
  ctx,
  hubOracleUtxo,
  stateQueueBlockUtxo,
}: {
  readonly stepIndex: IntermediateStepIndex;
  readonly prepared: WithdrawalMistagPreparedEvidence;
  readonly inputIndex: bigint;
  readonly outputIndex: bigint;
  readonly ctx: Parameters<BuildTxWithRedeemer>[0];
  readonly hubOracleUtxo?: UTxO;
  readonly stateQueueBlockUtxo?: UTxO;
}) => {
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

const payloadSchemas = [
  WithdrawalMistagStep01PayloadSchema,
  WithdrawalMistagStep02PayloadSchema,
  WithdrawalMistagStep03PayloadSchema,
  WithdrawalMistagStep04PayloadSchema,
] as const;

export const withdrawalMistagStepPayloadCbor = (
  prepared: WithdrawalMistagPreparedEvidence,
  stepIndex: IntermediateStepIndex,
): string => {
  const payload = [
    { committed_withdrawal: prepared.committedWithdrawal },
    {
      withdrawal_info: prepared.committedWithdrawal.value,
      event_to_step: prepared.eventToStep,
      transition_step: prepared.transitionStep,
    },
    {
      withdrawal_info: prepared.committedWithdrawal.value,
      evidence: prepared.ledgerEvidence,
    },
    { withdrawal_body: prepared.committedWithdrawal.value.body },
  ][stepIndex];
  return aikenSerialisedPlutusDataCborPreservingMapOrder(
    Data.to(payload as never, payloadSchemas[stepIndex] as never),
  );
};

export const stepArgs = (
  args: Parameters<typeof inlineStepArgs>[0] & {
    evidenceReferences?: readonly UTxO[];
  },
) => {
  const inline = inlineStepArgs(args);
  const { input_index, output_index, ...rest } = inline;
  const indexes = { input_index, output_index };
  const payload = { ...rest };
  if (args.stepIndex === 0 && "hub_ref_input_index" in payload) {
    const {
      hub_ref_input_index,
      state_queue_node_ref_input_index,
      committed_withdrawal,
    } = payload;
    return {
      ...indexes,
      hub_ref_input_index,
      state_queue_node_ref_input_index,
      payload:
        args.evidenceReferences === undefined
          ? { InlineEvidence: { value: { committed_withdrawal } } }
          : {
              StructuredEvidence: {
                tree: structuredDataTreeData(
                  structuredDataPublicationPlan(
                    withdrawalMistagStepPayloadCbor(
                      args.prepared,
                      args.stepIndex,
                    ),
                  ).tree,
                  (index) =>
                    requireReferenceInputIndex(
                      args.ctx,
                      args.evidenceReferences![index]!,
                      "withdrawal evidence",
                    ),
                ),
              },
            },
    };
  }
  return {
    ...indexes,
    payload:
      args.evidenceReferences === undefined
        ? { InlineEvidence: { value: payload } }
        : {
            StructuredEvidence: {
              tree: structuredDataTreeData(
                structuredDataPublicationPlan(
                  withdrawalMistagStepPayloadCbor(
                    args.prepared,
                    args.stepIndex,
                  ),
                ).tree,
                (index) =>
                  requireReferenceInputIndex(
                    args.ctx,
                    args.evidenceReferences![index]!,
                    "withdrawal evidence",
                  ),
              ),
            },
          },
  };
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
