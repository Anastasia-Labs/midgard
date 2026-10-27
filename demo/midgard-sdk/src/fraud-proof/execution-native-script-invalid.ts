/** Direct `executionNativeScriptInvalid` wire
 * (`execution_native_script_invalid/{types,entry,signers,evaluate,spend_rank,
 * terminal}.ak`). The family entry selects one purpose through the shared
 * purpose route, or proves the recorded coordinate has no native execution;
 * the lookup starts a paged evaluation of a native source over the signer
 * frontier and the validity interval; the terminal decides. Constructor and
 * field order is wire format. */
import { asDataType } from "@al-ft/midgard-core/lucid-data";
import { Data } from "@lucid-evolution/lucid";

import { BoundedItemChunkProofSchema } from "../ledger-state.js";
import { DirectScanActionSchema } from "./direct-purpose-workers.js";
import {
  DirectFieldCursorSchema,
  DirectPurposeCoordinateSchema,
  DirectScriptBoundSchema,
} from "./direct-script-derivation.js";
import { FieldOpeningSchema } from "./field-opening.js";
import type { PurposeCoordinate } from "./first-fault.js";
import { leafAdmitsLaterFault } from "./first-fault-later-phases.js";
import {
  faultProofStepDatumSchema,
  faultProofStepRedeemerSchema,
  MembershipCarriageSchema,
} from "./native.js";
import { terminalVerdictContradiction } from "./proof-thread-substrate.js";
import { FrontierPeakSchema } from "./validation-auxiliary-witness.data-node-schema.js";
import { NativeScriptFrameSchema } from "./validation-auxiliary-witness.ledger-output-proof-witness-schema.js";

const indices = { input_index: Data.Integer(), output_index: Data.Integer() };

/** 0 NoExecution, 1 Evaluated. */
export const ExecutionNativeScriptInvalidFactSchema = Data.Enum([
  Data.Object({
    NoExecution: Data.Object({ coordinate: DirectPurposeCoordinateSchema }),
  }),
  Data.Object({
    Evaluated: Data.Object({
      coordinate: DirectPurposeCoordinateSchema,
      satisfied: Data.Boolean(),
    }),
  }),
]);
export type ExecutionNativeScriptInvalidFact = Data.Static<
  typeof ExecutionNativeScriptInvalidFactSchema
>;

export const ExecutionNativeScriptInvalidFinalStateSchema = Data.Object({
  bound: DirectScriptBoundSchema,
  fact: ExecutionNativeScriptInvalidFactSchema,
});
export type ExecutionNativeScriptInvalidFinalState = Data.Static<
  typeof ExecutionNativeScriptInvalidFinalStateSchema
>;
export const ExecutionNativeScriptInvalidFinalDatumSchema =
  faultProofStepDatumSchema(ExecutionNativeScriptInvalidFinalStateSchema);
export type ExecutionNativeScriptInvalidFinalDatum = Data.Static<
  typeof ExecutionNativeScriptInvalidFinalDatumSchema
>;
export const ExecutionNativeScriptInvalidFinalDatum =
  asDataType<ExecutionNativeScriptInvalidFinalDatum>(
    ExecutionNativeScriptInvalidFinalDatumSchema,
  );

/** A paged native evaluation (`types.EvalV1`). */
export const ExecutionNativeScriptInvalidEvalStateSchema = Data.Object({
  bound: DirectScriptBoundSchema,
  coordinate: DirectPurposeCoordinateSchema,
  item_commitment: Data.Bytes(),
  validity_interval_start: Data.Integer(),
  validity_interval_end: Data.Integer(),
  signer_count: Data.Integer(),
  signer_peaks: Data.Array(FrontierPeakSchema),
  total_length: Data.Integer(),
  cursor: Data.Integer(),
  stack_root: Data.Bytes(),
  stack_depth: Data.Integer(),
  pending: Data.Nullable(Data.Boolean()),
});
export type ExecutionNativeScriptInvalidEvalState = Data.Static<
  typeof ExecutionNativeScriptInvalidEvalStateSchema
>;
export const ExecutionNativeScriptInvalidEvalDatumSchema =
  faultProofStepDatumSchema(ExecutionNativeScriptInvalidEvalStateSchema);
export type ExecutionNativeScriptInvalidEvalDatum = Data.Static<
  typeof ExecutionNativeScriptInvalidEvalDatumSchema
>;
export const ExecutionNativeScriptInvalidEvalDatum =
  asDataType<ExecutionNativeScriptInvalidEvalDatum>(
    ExecutionNativeScriptInvalidEvalDatumSchema,
  );

/** The rank of a key-locked spend among the spent outrefs
 * (`types.SpendRankV1`). */
export const ExecutionNativeScriptInvalidSpendRankStateSchema = Data.Object({
  cursor: DirectFieldCursorSchema,
  subject: Data.Bytes(),
  rank: Data.Integer(),
  complete: Data.Boolean(),
});
export type ExecutionNativeScriptInvalidSpendRankState = Data.Static<
  typeof ExecutionNativeScriptInvalidSpendRankStateSchema
>;
export const ExecutionNativeScriptInvalidSpendRankDatumSchema =
  faultProofStepDatumSchema(ExecutionNativeScriptInvalidSpendRankStateSchema);
export type ExecutionNativeScriptInvalidSpendRankDatum = Data.Static<
  typeof ExecutionNativeScriptInvalidSpendRankDatumSchema
>;
export const ExecutionNativeScriptInvalidSpendRankDatum =
  asDataType<ExecutionNativeScriptInvalidSpendRankDatum>(
    ExecutionNativeScriptInvalidSpendRankDatumSchema,
  );

/** 0 Observe, 1 Contradict, 2 ObserveAbsent, 3 SpendAbsent, 4 SpendKey. */
export const ExecutionNativeScriptInvalidEntryActionSchema = Data.Enum([
  Data.Object({ Observe: Data.Object(indices) }),
  Data.Object({ Contradict: Data.Object(indices) }),
  Data.Object({
    ObserveAbsent: Data.Object({ ...indices, opening: FieldOpeningSchema }),
  }),
  Data.Object({
    SpendAbsent: Data.Object({ ...indices, opening: FieldOpeningSchema }),
  }),
  Data.Object({ SpendKey: Data.Object(indices) }),
]);
export const ExecutionNativeScriptInvalidEntryRedeemerSchema =
  faultProofStepRedeemerSchema(ExecutionNativeScriptInvalidEntryActionSchema);
export type ExecutionNativeScriptInvalidEntryRedeemer = Data.Static<
  typeof ExecutionNativeScriptInvalidEntryRedeemerSchema
>;
export const ExecutionNativeScriptInvalidEntryRedeemer =
  asDataType<ExecutionNativeScriptInvalidEntryRedeemer>(
    ExecutionNativeScriptInvalidEntryRedeemerSchema,
  );

/** One signer-frontier page over field 7 (`signers.PageV1`). */
export const ExecutionNativeScriptInvalidSignersPageSchema = Data.Object({
  ...indices,
  opening: FieldOpeningSchema,
  budget: Data.Integer(),
});
export const ExecutionNativeScriptInvalidSignersRedeemerSchema =
  faultProofStepRedeemerSchema(ExecutionNativeScriptInvalidSignersPageSchema);
export type ExecutionNativeScriptInvalidSignersRedeemer = Data.Static<
  typeof ExecutionNativeScriptInvalidSignersRedeemerSchema
>;
export const ExecutionNativeScriptInvalidSignersRedeemer =
  asDataType<ExecutionNativeScriptInvalidSignersRedeemer>(
    ExecutionNativeScriptInvalidSignersRedeemerSchema,
  );

export const ExecutionNativeScriptInvalidSignerWitnessSchema = Data.Object({
  key_hash: Data.Bytes(),
  siblings: Data.Array(Data.Bytes()),
});
export type ExecutionNativeScriptInvalidSignerWitness = Data.Static<
  typeof ExecutionNativeScriptInvalidSignerWitnessSchema
>;

/** 0 SignerPresent, 1 SignerAbsent. */
export const ExecutionNativeScriptInvalidSignerQuerySchema = Data.Enum([
  Data.Object({
    SignerPresent: Data.Object({
      position: Data.Integer(),
      siblings: Data.Array(Data.Bytes()),
    }),
  }),
  Data.Object({
    SignerAbsent: Data.Object({
      position: Data.Integer(),
      below: Data.Nullable(ExecutionNativeScriptInvalidSignerWitnessSchema),
      above: Data.Nullable(ExecutionNativeScriptInvalidSignerWitnessSchema),
    }),
  }),
]);
export type ExecutionNativeScriptInvalidSignerQuery = Data.Static<
  typeof ExecutionNativeScriptInvalidSignerQuerySchema
>;

/** One evaluation page (`evaluate.PageV1`). */
export const ExecutionNativeScriptInvalidEvaluatePageSchema = Data.Object({
  ...indices,
  chunks: Data.Array(BoundedItemChunkProofSchema),
  frames: Data.Array(NativeScriptFrameSchema),
  signers: Data.Array(ExecutionNativeScriptInvalidSignerQuerySchema),
  budget: Data.Integer(),
});
export type ExecutionNativeScriptInvalidEvaluatePage = Data.Static<
  typeof ExecutionNativeScriptInvalidEvaluatePageSchema
>;
export const ExecutionNativeScriptInvalidEvaluateRedeemerSchema =
  faultProofStepRedeemerSchema(ExecutionNativeScriptInvalidEvaluatePageSchema);
export type ExecutionNativeScriptInvalidEvaluateRedeemer = Data.Static<
  typeof ExecutionNativeScriptInvalidEvaluateRedeemerSchema
>;
export const ExecutionNativeScriptInvalidEvaluateRedeemer =
  asDataType<ExecutionNativeScriptInvalidEvaluateRedeemer>(
    ExecutionNativeScriptInvalidEvaluateRedeemerSchema,
  );

/** Opens the raw spend item of a key-locked spend and ranks the first page
 * (`spend_rank.OpenActionV1`). */
export const ExecutionNativeScriptInvalidSpendKeyActionSchema = Data.Object({
  ...indices,
  raw_item_index: Data.Integer(),
  opening: FieldOpeningSchema,
  descriptor_cbor: Data.Bytes(),
  membership: MembershipCarriageSchema,
  budget: Data.Integer(),
});
export const ExecutionNativeScriptInvalidSpendKeyRedeemerSchema =
  faultProofStepRedeemerSchema(
    ExecutionNativeScriptInvalidSpendKeyActionSchema,
  );
export type ExecutionNativeScriptInvalidSpendKeyRedeemer = Data.Static<
  typeof ExecutionNativeScriptInvalidSpendKeyRedeemerSchema
>;
export const ExecutionNativeScriptInvalidSpendKeyRedeemer =
  asDataType<ExecutionNativeScriptInvalidSpendKeyRedeemer>(
    ExecutionNativeScriptInvalidSpendKeyRedeemerSchema,
  );

/** The rank walk's `Scan` and `Finish` (`purpose_workers.MintActionV1`). */
export const ExecutionNativeScriptInvalidSpendRankRedeemerSchema =
  faultProofStepRedeemerSchema(DirectScanActionSchema);
export type ExecutionNativeScriptInvalidSpendRankRedeemer = Data.Static<
  typeof ExecutionNativeScriptInvalidSpendRankRedeemerSchema
>;
export const ExecutionNativeScriptInvalidSpendRankRedeemer =
  asDataType<ExecutionNativeScriptInvalidSpendRankRedeemer>(
    ExecutionNativeScriptInvalidSpendRankRedeemerSchema,
  );

/** The coordinate a fact names (`terminal.coordinate_v1`). */
export const executionNativeScriptInvalidFactCoordinate = (
  fact: ExecutionNativeScriptInvalidFact,
): PurposeCoordinate =>
  "NoExecution" in fact
    ? fact.NoExecution.coordinate
    : fact.Evaluated.coordinate;

/** Twin of `terminal.convicts_v1`. A leaf recording
 * `ExecutionNativeScriptFalse` at the fact's coordinate convicts unless the
 * native script there evaluated false. Any other leaf convicts on a native
 * script that evaluated false at a coordinate its recorded reason does not
 * precede. */
export const executionNativeScriptInvalidConvicts = (
  state: ExecutionNativeScriptInvalidFinalState,
): boolean => {
  const subject = state.bound.subject;
  const coordinate = executionNativeScriptInvalidFactCoordinate(state.fact);
  const falsified =
    "Evaluated" in state.fact && !state.fact.Evaluated.satisfied;
  const reason = subject.rejection_reason;
  const recorded =
    reason !== null &&
    typeof reason === "object" &&
    "ExecutionNativeScriptFalse" in reason
      ? reason.ExecutionNativeScriptFalse
      : null;
  if (
    recorded !== null &&
    recorded.purpose_kind === coordinate.kind &&
    recorded.purpose_index === coordinate.index
  )
    return terminalVerdictContradiction(subject, falsified);
  return (
    falsified &&
    !leafAdmitsLaterFault(subject, state.bound.network_order_context, {
      phase: "execution",
      position: { coordinate, subcheck: "NativeScriptEvaluation" },
    })
  );
};
