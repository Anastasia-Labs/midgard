/**
 * `cross-block-duplicate-event` — off-chain wire twins.
 *
 * Proves the same L1 deposit or withdrawal identity is committed by a live
 * challenged block and by a different confirmed settlement header. The family
 * Production catalogue category `crossBlockDuplicateEvent` (`00000016`).
 */
import { Data } from "@lucid-evolution/lucid";

import {
  type OutputReference,
  OutputReferenceSchema,
  ScriptHashSchema,
} from "@/common.js";
import {
  type DepositSourceMembershipProof,
  DepositSourceMembershipProofSchema,
  type ForcedTransactionSourceMembershipProof,
  ForcedTransactionSourceMembershipProofSchema,
  type WithdrawalSourceMembershipProof,
  WithdrawalSourceMembershipProofSchema,
} from "@/transition-trace.js";

import { FRAUD_PROOF_CATALOGUE_CATEGORY_IDS } from "./catalogue.js";
import {
  faultProofStepDatumSchema,
  faultProofStepRedeemerSchema,
} from "./native.js";

export const CROSS_BLOCK_DUPLICATE_EVENT_VIOLATION_ID_V1 =
  "cross-block-duplicate-event" as const;

export const CROSS_BLOCK_DUPLICATE_EVENT_FRAUD_CATEGORY_ID_V1 =
  FRAUD_PROOF_CATALOGUE_CATEGORY_IDS.crossBlockDuplicateEvent;

export const CrossBlockDuplicateEventHeaderHashSchema = Data.Bytes({
  minLength: 28,
  maxLength: 28,
});
export type CrossBlockDuplicateEventHeaderHash = Data.Static<
  typeof CrossBlockDuplicateEventHeaderHashSchema
>;

export const crossBlockDuplicateEventThreadTokenAssetNameV1 = (
  challengedHeaderHash: CrossBlockDuplicateEventHeaderHash,
): string => {
  if (!/^[0-9a-f]{56}$/u.test(challengedHeaderHash)) {
    throw new Error(
      "cross-block-duplicate-event challenged header hash must be 28 bytes of lowercase hex",
    );
  }
  return `${CROSS_BLOCK_DUPLICATE_EVENT_FRAUD_CATEGORY_ID_V1}${challengedHeaderHash}`;
};

export const CrossBlockDuplicateEventKindV1Schema = Data.Enum([
  Data.Literal("DuplicateDepositV1"),
  Data.Literal("DuplicateWithdrawalV1"),
  Data.Literal("DuplicateForcedTransactionV1"),
]);
export type CrossBlockDuplicateEventKindV1 = Data.Static<
  typeof CrossBlockDuplicateEventKindV1Schema
>;
export const CrossBlockDuplicateEventKindV1 =
  CrossBlockDuplicateEventKindV1Schema as unknown as CrossBlockDuplicateEventKindV1;

export const CommittedDuplicateEventProofV1Schema = Data.Enum([
  Data.Object({
    CommittedDuplicateDepositV1: Data.Object({
      membership: DepositSourceMembershipProofSchema,
    }),
  }),
  Data.Object({
    CommittedDuplicateWithdrawalV1: Data.Object({
      membership: WithdrawalSourceMembershipProofSchema,
    }),
  }),
  Data.Object({
    CommittedDuplicateForcedTransactionV1: Data.Object({
      membership: ForcedTransactionSourceMembershipProofSchema,
    }),
  }),
]);
export type CommittedDuplicateEventProofV1 =
  | {
      readonly CommittedDuplicateDepositV1: {
        readonly membership: DepositSourceMembershipProof;
      };
    }
  | {
      readonly CommittedDuplicateWithdrawalV1: {
        readonly membership: WithdrawalSourceMembershipProof;
      };
    }
  | {
      readonly CommittedDuplicateForcedTransactionV1: {
        readonly membership: ForcedTransactionSourceMembershipProof;
      };
    };
export const CommittedDuplicateEventProofV1 =
  CommittedDuplicateEventProofV1Schema as unknown as CommittedDuplicateEventProofV1;

export const CrossBlockDuplicateEventStep01DatumSchema =
  faultProofStepDatumSchema(Data.Any());
export type CrossBlockDuplicateEventStep01Datum = Data.Static<
  typeof CrossBlockDuplicateEventStep01DatumSchema
>;
export const CrossBlockDuplicateEventStep01Datum =
  CrossBlockDuplicateEventStep01DatumSchema as unknown as CrossBlockDuplicateEventStep01Datum;

export const CrossBlockDuplicateEventStep01ArgsSchema = Data.Object({
  input_index: Data.Integer(),
  output_index: Data.Integer(),
  hub_ref_input_index: Data.Integer(),
  state_queue_node_ref_input_index: Data.Integer(),
  committed_event: CommittedDuplicateEventProofV1Schema,
});
export type CrossBlockDuplicateEventStep01Args = Data.Static<
  typeof CrossBlockDuplicateEventStep01ArgsSchema
>;
export const CrossBlockDuplicateEventStep01Args =
  CrossBlockDuplicateEventStep01ArgsSchema as unknown as CrossBlockDuplicateEventStep01Args;

export const CrossBlockDuplicateEventStep01SpendRedeemerSchema =
  faultProofStepRedeemerSchema(CrossBlockDuplicateEventStep01ArgsSchema);
export type CrossBlockDuplicateEventStep01SpendRedeemer = Data.Static<
  typeof CrossBlockDuplicateEventStep01SpendRedeemerSchema
>;
export const CrossBlockDuplicateEventStep01SpendRedeemer =
  CrossBlockDuplicateEventStep01SpendRedeemerSchema as unknown as CrossBlockDuplicateEventStep01SpendRedeemer;

export const CrossBlockDuplicateEventStep02StateSchema = Data.Object({
  challenged_header_hash: CrossBlockDuplicateEventHeaderHashSchema,
  settlement_policy_id: ScriptHashSchema,
  event_kind: CrossBlockDuplicateEventKindV1Schema,
  event_key: OutputReferenceSchema,
});
export type CrossBlockDuplicateEventStep02State = Data.Static<
  typeof CrossBlockDuplicateEventStep02StateSchema
>;
export const CrossBlockDuplicateEventStep02State =
  CrossBlockDuplicateEventStep02StateSchema as unknown as CrossBlockDuplicateEventStep02State;

export const CrossBlockDuplicateEventStep02DatumSchema =
  faultProofStepDatumSchema(CrossBlockDuplicateEventStep02StateSchema);
export type CrossBlockDuplicateEventStep02Datum = Data.Static<
  typeof CrossBlockDuplicateEventStep02DatumSchema
>;
export const CrossBlockDuplicateEventStep02Datum =
  CrossBlockDuplicateEventStep02DatumSchema as unknown as CrossBlockDuplicateEventStep02Datum;

export const CrossBlockDuplicateEventStep02ArgsSchema = Data.Object({
  input_index: Data.Integer(),
  output_index: Data.Integer(),
  fraud_proof_mint_redeemer_index: Data.Integer(),
  settlement_ref_input_index: Data.Integer(),
  settled_event: CommittedDuplicateEventProofV1Schema,
});
export type CrossBlockDuplicateEventStep02Args = Data.Static<
  typeof CrossBlockDuplicateEventStep02ArgsSchema
>;
export const CrossBlockDuplicateEventStep02Args =
  CrossBlockDuplicateEventStep02ArgsSchema as unknown as CrossBlockDuplicateEventStep02Args;

export const CrossBlockDuplicateEventStep02SpendRedeemerSchema =
  faultProofStepRedeemerSchema(CrossBlockDuplicateEventStep02ArgsSchema);
export type CrossBlockDuplicateEventStep02SpendRedeemer = Data.Static<
  typeof CrossBlockDuplicateEventStep02SpendRedeemerSchema
>;
export const CrossBlockDuplicateEventStep02SpendRedeemer =
  CrossBlockDuplicateEventStep02SpendRedeemerSchema as unknown as CrossBlockDuplicateEventStep02SpendRedeemer;

export const CROSS_BLOCK_DUPLICATE_EVENT_STEP_NAMES_V1 = [
  "step_01",
  "step_02",
] as const;
export type CrossBlockDuplicateEventStepNameV1 =
  (typeof CROSS_BLOCK_DUPLICATE_EVENT_STEP_NAMES_V1)[number];

export const crossBlockDuplicateEventStepDatumSchemaV1 = (
  step: CrossBlockDuplicateEventStepNameV1,
) => {
  switch (step) {
    case "step_01":
      return CrossBlockDuplicateEventStep01DatumSchema;
    case "step_02":
      return CrossBlockDuplicateEventStep02DatumSchema;
  }
};

export const duplicateEventKindAndKeyV1 = (
  proof: CommittedDuplicateEventProofV1,
): {
  readonly eventKind: CrossBlockDuplicateEventKindV1;
  readonly eventKey: OutputReference;
} => {
  if ("CommittedDuplicateDepositV1" in proof) {
    return {
      eventKind: "DuplicateDepositV1",
      eventKey: proof.CommittedDuplicateDepositV1.membership.key,
    };
  }
  if ("CommittedDuplicateWithdrawalV1" in proof) {
    return {
      eventKind: "DuplicateWithdrawalV1",
      eventKey: proof.CommittedDuplicateWithdrawalV1.membership.key,
    };
  }
  return {
    eventKind: "DuplicateForcedTransactionV1",
    eventKey: proof.CommittedDuplicateForcedTransactionV1.membership.key,
  };
};

/** Step-01 → step-02 state, derived only after challenged membership passes. */
export const crossBlockDuplicateEventStep02StateV1 = ({
  challengedHeaderHash,
  settlementPolicyId,
  committedEvent,
}: {
  readonly challengedHeaderHash: CrossBlockDuplicateEventHeaderHash;
  readonly settlementPolicyId: string;
  readonly committedEvent: CommittedDuplicateEventProofV1;
}): CrossBlockDuplicateEventStep02State => {
  const { eventKind, eventKey } = duplicateEventKindAndKeyV1(committedEvent);
  return {
    challenged_header_hash: challengedHeaderHash,
    settlement_policy_id: settlementPolicyId,
    event_kind: eventKind,
    event_key: eventKey,
  };
};

/** Local finalization preflight mirroring the two-header/same-event predicate. */
export const assertConfirmedDuplicateEventV1 = ({
  state,
  settledHeaderHash,
  settledEvent,
}: {
  readonly state: CrossBlockDuplicateEventStep02State;
  readonly settledHeaderHash: string;
  readonly settledEvent: CommittedDuplicateEventProofV1;
}): void => {
  if (settledHeaderHash === state.challenged_header_hash) {
    throw new Error(
      "cross-block-duplicate-event settlement header must differ from challenged header",
    );
  }
  const { eventKind, eventKey } = duplicateEventKindAndKeyV1(settledEvent);
  if (eventKind !== state.event_kind) {
    throw new Error(
      "cross-block-duplicate-event challenged and settlement root domains differ",
    );
  }
  if (
    eventKey.transactionId !== state.event_key.transactionId ||
    eventKey.outputIndex !== state.event_key.outputIndex
  ) {
    throw new Error(
      "cross-block-duplicate-event challenged and settlement event identities differ",
    );
  }
};
