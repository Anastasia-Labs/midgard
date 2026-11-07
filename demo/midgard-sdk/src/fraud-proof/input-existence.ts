/** Exact SDK twins of the shared four-step per-event input-existence proof. */
import { asDataType } from "@al-ft/midgard-core/lucid-data";
import { Data } from "@lucid-evolution/lucid";

import { H32Schema, ProofSchema } from "../common.js";
import { HeaderSchema } from "../ledger-state.js";
import {
  EventToStepCarriedMembershipSchema,
  TransitionCarriedMembershipSchema,
} from "./counted-membership-carriage.js";
import { FieldOpeningSchema } from "./field-opening.js";
import { NetworkOrderContextSchema } from "./first-fault.js";
import {
  MidgardTxInputSchema,
  NativeTxBodyCompactSchema,
  NonMembershipCarriageSchema,
} from "./native.js";
import { VerdictSubjectSchema } from "./proof-thread-substrate.js";

export const InputExistenceCompactSchema = Data.Object({
  body: NativeTxBodyCompactSchema,
  witness_set_hash: H32Schema,
  validity_code: Data.Nullable(Data.Integer()),
});
export type InputExistenceCompact = Data.Static<
  typeof InputExistenceCompactSchema
>;

export const InputExistenceSourceStateSchema = Data.Object({
  subject: VerdictSubjectSchema,
  header: HeaderSchema,
  compact: InputExistenceCompactSchema,
});
export const InputExistenceSelectArgsSchema = Data.Object({
  input_index: Data.Integer(),
  output_index: Data.Integer(),
  inputs_opening: FieldOpeningSchema,
  selected_input_index: Data.Integer(),
  claimed_inputs_opening: Data.Nullable(FieldOpeningSchema),
  event_membership: EventToStepCarriedMembershipSchema,
});
export const InputExistenceSelectedStateSchema = Data.Object({
  subject: VerdictSubjectSchema,
  header: HeaderSchema,
  compact: InputExistenceCompactSchema,
  source_kind: Data.Integer(),
  input_index: Data.Integer(),
  selected_input: Data.Nullable(MidgardTxInputSchema),
  claimed_input: Data.Nullable(MidgardTxInputSchema),
  step_index: Data.Integer(),
});
export const InputExistencePreStateArgsSchema = Data.Object({
  input_index: Data.Integer(),
  output_index: Data.Integer(),
  transition_membership: TransitionCarriedMembershipSchema,
});
export const InputExistenceLedgerStateSchema = Data.Object({
  subject: VerdictSubjectSchema,
  compact: InputExistenceCompactSchema,
  network_order_context: NetworkOrderContextSchema,
  source_kind: Data.Integer(),
  input_index: Data.Integer(),
  selected_input: Data.Nullable(MidgardTxInputSchema),
  claimed_input: Data.Nullable(MidgardTxInputSchema),
  pre_utxos_root: H32Schema,
});
export const InputExistenceLedgerEvidenceSchema = Data.Enum([
  Data.Literal("OutOfDomain"),
  Data.Object({
    Present: Data.Object({ value_hash: H32Schema, proof: ProofSchema }),
  }),
  Data.Object({
    Absent: Data.Object({ non_membership: NonMembershipCarriageSchema }),
  }),
]);
export const InputExistenceTerminalArgsSchema = Data.Object({
  input_index: Data.Integer(),
  output_index: Data.Integer(),
  fraud_proof_mint_redeemer_index: Data.Integer(),
  ledger_evidence: InputExistenceLedgerEvidenceSchema,
});

export type InputExistenceSourceState = Data.Static<
  typeof InputExistenceSourceStateSchema
>;
export const InputExistenceSourceState = asDataType<InputExistenceSourceState>(
  InputExistenceSourceStateSchema,
);
export type InputExistenceSelectedState = Data.Static<
  typeof InputExistenceSelectedStateSchema
>;
export const InputExistenceSelectedState =
  asDataType<InputExistenceSelectedState>(InputExistenceSelectedStateSchema);
export type InputExistenceLedgerState = Data.Static<
  typeof InputExistenceLedgerStateSchema
>;
export const InputExistenceLedgerState = asDataType<InputExistenceLedgerState>(
  InputExistenceLedgerStateSchema,
);
export type InputExistenceLedgerEvidence = Data.Static<
  typeof InputExistenceLedgerEvidenceSchema
>;
export const InputExistenceLedgerEvidence =
  asDataType<InputExistenceLedgerEvidence>(InputExistenceLedgerEvidenceSchema);
