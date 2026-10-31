import { MIDGARD_TRANSITION_STEP_SCHEMA_VERSION } from "@al-ft/midgard-core/consensus-profile";
import { asDataType } from "@al-ft/midgard-core/lucid-data";
import { Data } from "@lucid-evolution/lucid";

import {
  AddressSchema,
  H32Schema,
  MerkleRootSchema,
  OutputReferenceSchema,
  ValueSchema,
} from "./common.js";
import {
  CardanoDatumSchema,
  TransitionPhaseSchema,
} from "./ledger-state.confirmed-state-next-header-protocol-version.js";

export const EventKeySchema = Data.Enum([
  Data.Object({
    WithdrawalEventKey: Data.Object({
      withdrawal_id: OutputReferenceSchema,
    }),
  }),
  Data.Object({
    ForcedTransactionEventKey: Data.Object({
      tx_order_id: OutputReferenceSchema,
    }),
  }),
  Data.Object({
    L2TransactionEventKey: Data.Object({
      tx_id: H32Schema,
    }),
  }),
  Data.Object({
    DepositEventKey: Data.Object({
      deposit_id: OutputReferenceSchema,
    }),
  }),
]);

export type EventKey = Data.Static<typeof EventKeySchema>;

export const EventKey = asDataType<EventKey>(EventKeySchema);

export const EventToStepValueSchema = Data.Object({
  step_index: Data.Integer(),
  phase: TransitionPhaseSchema,
});

export type EventToStepValue = Data.Static<typeof EventToStepValueSchema>;

export const EventToStepValue = asDataType<EventToStepValue>(
  EventToStepValueSchema,
);

export const TRANSITION_STEP_SCHEMA_VERSION = BigInt(
  MIDGARD_TRANSITION_STEP_SCHEMA_VERSION,
);

export const TransitionStepV1Schema = Data.Object({
  schema_version: Data.Integer(),
  step_index: Data.Integer(),
  event_key: EventKeySchema,
  phase: TransitionPhaseSchema,
  pre_utxos_root: MerkleRootSchema,
  post_utxos_root: MerkleRootSchema,
});

export type TransitionStepV1 = Data.Static<typeof TransitionStepV1Schema>;

export const TransitionStepV1 = asDataType<TransitionStepV1>(
  TransitionStepV1Schema,
);

// The unqualified names remain source aliases for consumers of the canonical
// schema; they do not define a second wire identity.
export type TransitionStep = TransitionStepV1;

export const TransitionStepSchema = TransitionStepV1Schema;

export const TransitionStep = TransitionStepV1;

export const ValidationVerdictSchema = Data.Enum([
  // Preserve the exact Aiken constructor indexes. Pending is not a valid
  // terminal descriptor verdict and is rejected by semantic conversion, but
  // omitting it here would encode Accepted/Rejected as constructors 0/1
  // instead of 1/2.
  Data.Literal("Pending"),
  Data.Literal("Accepted"),
  Data.Literal("Rejected"),
]);

export type ValidationVerdict = Data.Static<typeof ValidationVerdictSchema>;

export const ValidationVerdict = asDataType<ValidationVerdict>(
  ValidationVerdictSchema,
);

export const ValidationTraceDescriptorSchema = Data.Object({
  schema_version: Data.Integer(),
  machine_version: Data.Integer(),
  trace_root: H32Schema,
  step_count: Data.Integer(),
  initial_state_hash: H32Schema,
  terminal_state_hash: H32Schema,
  verdict: ValidationVerdictSchema,
  rejection_code_hash: H32Schema,
});

export type ValidationTraceDescriptor = Data.Static<
  typeof ValidationTraceDescriptorSchema
>;

export const ValidationTraceDescriptor = asDataType<ValidationTraceDescriptor>(
  ValidationTraceDescriptorSchema,
);

export const WithdrawalBodySchema = Data.Object({
  l2_outref: OutputReferenceSchema,
  l2_owner: Data.Bytes({ minLength: 28, maxLength: 28 }),
  l2_value: ValueSchema,
  l1_address: AddressSchema,
  l1_datum: CardanoDatumSchema,
});

export type WithdrawalBody = Data.Static<typeof WithdrawalBodySchema>;

export const WithdrawalBody = asDataType<WithdrawalBody>(WithdrawalBodySchema);

export const WithdrawalSignatureSchema = Data.Tuple([
  Data.Bytes(),
  Data.Bytes(),
]);

export type WithdrawalSignature = Data.Static<typeof WithdrawalSignatureSchema>;

export const WithdrawalSignature = asDataType<WithdrawalSignature>(
  WithdrawalSignatureSchema,
);

export const WithdrawalValiditySchema = Data.Enum([
  Data.Literal("WithdrawalIsValid"),
  Data.Literal("NonExistentWithdrawalUtxo"),
  Data.Object({
    SpentWithdrawalUtxo: Data.Object({
      l2_tx_id: Data.Bytes(),
    }),
  }),
  Data.Literal("IncorrectWithdrawalOwner"),
  Data.Literal("IncorrectWithdrawalValue"),
  Data.Literal("IncorrectWithdrawalSignature"),
  Data.Literal("TooManyTokensInWithdrawal"),
  Data.Literal("UnpayableWithdrawalValue"),
]);

export type WithdrawalValidity = Data.Static<typeof WithdrawalValiditySchema>;

export const WithdrawalValidity = asDataType<WithdrawalValidity>(
  WithdrawalValiditySchema,
);

export const WithdrawalInfoSchema = Data.Object({
  body: WithdrawalBodySchema,
  signature: WithdrawalSignatureSchema,
  validity: WithdrawalValiditySchema,
});

export type WithdrawalInfo = Data.Static<typeof WithdrawalInfoSchema>;

export const WithdrawalInfo = asDataType<WithdrawalInfo>(WithdrawalInfoSchema);

export const WithdrawalEventSchema = Data.Object({
  id: OutputReferenceSchema,
  info: WithdrawalInfoSchema,
});

export type WithdrawalEvent = Data.Static<typeof WithdrawalEventSchema>;

export const WithdrawalEvent = asDataType<WithdrawalEvent>(
  WithdrawalEventSchema,
);
