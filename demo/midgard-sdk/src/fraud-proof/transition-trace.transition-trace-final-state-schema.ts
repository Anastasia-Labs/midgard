import { asDataType } from "@al-ft/midgard-core/lucid-data";
import { Data } from "@lucid-evolution/lucid";

import { type Header, type HeaderHash } from "../ledger-state.js";
import {
  type AdjacentTraceProof,
  type EventToStepProof,
  type IndexedTraceProof,
} from "../transition-trace.js";
import {
  type FaultProofStepCancel,
  faultProofStepDatumSchema,
  faultProofStepRedeemerSchema,
} from "./native.js";
import {
  SourceMembershipMismatchWitness,
  TraceBoundarySide,
} from "./transition-trace.invalid-one-step-transition-witness-schema.js";
import {
  CountFaultWitness,
  InvalidOneStepTransitionWitness,
  OmittedDueL1EventWitness,
  OutOfWindowSourceEventWitness,
  TransitionFault,
  TransitionFaultProof,
  TransitionFaultProofSchema,
} from "./transition-trace.transition-fault-schema.js";
import { type ValidationClaimWitness } from "./validation-dispute.js";

export const TransitionTraceRouteArgsSchema = Data.Object({
  input_index: Data.Integer(),
  output_index: Data.Integer(),
  proof: Data.Nullable(TransitionFaultProofSchema),
  proof_ref_indices: Data.Array(Data.Integer()),
});

export type TransitionTraceRouteArgs = {
  readonly input_index: bigint;
  readonly output_index: bigint;
  readonly proof: TransitionFaultProof | null;
  readonly proof_ref_indices: bigint[];
};

export const TransitionTraceRouteArgs = asDataType<TransitionTraceRouteArgs>(
  TransitionTraceRouteArgsSchema,
);

export const TransitionTraceRouteSpendRedeemerSchema =
  faultProofStepRedeemerSchema(TransitionTraceRouteArgsSchema);

export type TransitionTraceRouteSpendRedeemer =
  | { readonly Cancel: FaultProofStepCancel }
  | { readonly Continue: readonly [TransitionTraceRouteArgs] };

export const TransitionTraceRouteSpendRedeemer =
  asDataType<TransitionTraceRouteSpendRedeemer>(
    TransitionTraceRouteSpendRedeemerSchema,
  );

export const TransitionTraceFinalArgsSchema = Data.Object({
  input_index: Data.Integer(),
  output_index: Data.Integer(),
  hub_ref_input_index: Data.Integer(),
  fraud_proof_mint_redeemer_index: Data.Integer(),
});

export type TransitionTraceFinalArgs = Data.Static<
  typeof TransitionTraceFinalArgsSchema
>;

export const TransitionTraceFinalArgs = asDataType<TransitionTraceFinalArgs>(
  TransitionTraceFinalArgsSchema,
);

export const TransitionTraceFinalSpendRedeemerSchema =
  faultProofStepRedeemerSchema(TransitionTraceFinalArgsSchema);

export type TransitionTraceFinalSpendRedeemer =
  | { readonly Cancel: FaultProofStepCancel }
  | { readonly Continue: readonly [TransitionTraceFinalArgs] };

export const TransitionTraceFinalSpendRedeemer =
  asDataType<TransitionTraceFinalSpendRedeemer>(
    TransitionTraceFinalSpendRedeemerSchema,
  );

/** Timing finals resolve current history references after route publication. */
export const TransitionTraceL1EventFinalArgsSchema = Data.Object({
  input_index: Data.Integer(),
  output_index: Data.Integer(),
  hub_ref_input_index: Data.Integer(),
  fraud_proof_mint_redeemer_index: Data.Integer(),
  event_reference: Data.Object({
    order_index: Data.Integer(),
    external_data_index: Data.Nullable(Data.Integer()),
  }),
  completed_fraud_witness: Data.Any(),
  yield_ref_input_index: Data.Integer(),
});

export type TransitionTraceL1EventFinalArgs = Data.Static<
  typeof TransitionTraceL1EventFinalArgsSchema
>;

export const TransitionTraceL1EventFinalArgs =
  asDataType<TransitionTraceL1EventFinalArgs>(
    TransitionTraceL1EventFinalArgsSchema,
  );

export const TransitionTraceL1EventFinalSpendRedeemerSchema =
  faultProofStepRedeemerSchema(TransitionTraceL1EventFinalArgsSchema);

export type TransitionTraceL1EventFinalSpendRedeemer =
  | { readonly Cancel: FaultProofStepCancel }
  | { readonly Continue: readonly [TransitionTraceL1EventFinalArgs] };

export const TransitionTraceL1EventFinalSpendRedeemer =
  asDataType<TransitionTraceL1EventFinalSpendRedeemer>(
    TransitionTraceL1EventFinalSpendRedeemerSchema,
  );

export const TransitionTraceYieldFinalArgsSchema = Data.Object({
  input_index: Data.Integer(),
  output_index: Data.Integer(),
  hub_ref_input_index: Data.Integer(),
  fraud_proof_mint_redeemer_index: Data.Integer(),
  yield_ref_input_indices: Data.Array(Data.Integer()),
  proof_ref_indices: Data.Array(Data.Integer()),
  output_ref_indices: Data.Array(Data.Integer()),
  deposit_event_ref_index: Data.Integer(),
  deposit_external_ref_index: Data.Nullable(Data.Integer()),
  deposit_opening: Data.Nullable(Data.Any()),
  completed_fraud_witness: Data.Nullable(Data.Any()),
});

export type TransitionTraceYieldFinalArgs = Data.Static<
  typeof TransitionTraceYieldFinalArgsSchema
>;

export const TransitionTraceYieldFinalSpendRedeemerSchema =
  faultProofStepRedeemerSchema(TransitionTraceYieldFinalArgsSchema);

export type TransitionTraceYieldFinalSpendRedeemer = Data.Static<
  typeof TransitionTraceYieldFinalSpendRedeemerSchema
>;

export const TransitionTraceYieldFinalSpendRedeemer =
  asDataType<TransitionTraceYieldFinalSpendRedeemer>(
    TransitionTraceYieldFinalSpendRedeemerSchema,
  );

export const TransitionTraceOpenedOutputsSchema = Data.Object({
  spend_input_keys: Data.Array(Data.Bytes()),
  output_hashes: Data.Array(Data.Bytes()),
});

export type TransitionTraceOpenedOutputs = Data.Static<
  typeof TransitionTraceOpenedOutputsSchema
>;

export const TransitionTraceOpenedOutputs =
  asDataType<TransitionTraceOpenedOutputs>(TransitionTraceOpenedOutputsSchema);

const TransitionTraceDataSummarySchema = Data.Object({
  root: Data.Bytes(),
  cbor_length: Data.Integer(),
  memory: Data.Integer(),
});

export const TransitionTraceOutputSummariesSchema = Data.Object({
  summaries: Data.Array(
    Data.Tuple([
      TransitionTraceDataSummarySchema,
      TransitionTraceDataSummarySchema,
      TransitionTraceDataSummarySchema,
    ]),
  ),
});

export type TransitionTraceOutputSummaries = Data.Static<
  typeof TransitionTraceOutputSummariesSchema
>;

export const TransitionTraceOutputSummaries =
  asDataType<TransitionTraceOutputSummaries>(
    TransitionTraceOutputSummariesSchema,
  );

export const TransitionTraceFinalStateSchema = Data.Object({
  kind: Data.Integer(),
  phase: Data.Integer(),
  proof_commitment: Data.Object({ hash: Data.Bytes() }),
  opened: TransitionTraceOpenedOutputsSchema,
  input_index: Data.Integer(),
  output_index: Data.Integer(),
  current_root: Data.Bytes(),
  summaries: TransitionTraceOutputSummariesSchema,
  scan_cbor: Data.Bytes(),
  value_cbor: Data.Bytes(),
  value_start: Data.Integer(),
  value_end: Data.Integer(),
  value_summary: Data.Nullable(TransitionTraceDataSummarySchema),
  descriptor_cbor: Data.Bytes(),
  deposit_index: Data.Integer(),
  deposit_source_cbor: Data.Bytes(),
  deposit_asset_count: Data.Integer(),
});

export type TransitionTraceFinalState = Data.Static<
  typeof TransitionTraceFinalStateSchema
>;

export const TransitionTraceProofCommitmentDatumSchema =
  faultProofStepDatumSchema(TransitionTraceFinalStateSchema);

export type TransitionTraceProofCommitmentDatum = Data.Static<
  typeof TransitionTraceProofCommitmentDatumSchema
>;

export const TransitionTraceProofCommitmentDatum =
  asDataType<TransitionTraceProofCommitmentDatum>(
    TransitionTraceProofCommitmentDatumSchema,
  );

export const makeTransitionFaultProof = ({
  challengedHeaderHash,
  header,
  fault,
}: {
  readonly challengedHeaderHash: HeaderHash;
  readonly header: Header;
  readonly fault: TransitionFault;
}): TransitionFaultProof => ({
  challenged_header_hash: challengedHeaderHash,
  header,
  fault,
});

export const transitionTraceThreadAssetName = ({
  fraudCategoryId,
  challengedHeaderHash,
}: {
  readonly fraudCategoryId: string;
  readonly challengedHeaderHash: HeaderHash;
}): string => `${fraudCategoryId}${challengedHeaderHash}`;

export const traceBoundaryFault = ({
  side,
  traceProof,
}: {
  readonly side: TraceBoundarySide;
  readonly traceProof: IndexedTraceProof;
}): TransitionFault => ({
  TraceBoundaryFault: { side, trace_proof: traceProof },
});

export const traceLinkFault = (
  adjacent: AdjacentTraceProof,
): TransitionFault => ({
  TraceLinkFault: { adjacent },
});

export const eventToStepMismatchFault = ({
  traceProof,
  eventToStep,
}: {
  readonly traceProof: IndexedTraceProof;
  readonly eventToStep: EventToStepProof;
}): TransitionFault => ({
  EventToStepMismatch: {
    trace_proof: traceProof,
    event_to_step: eventToStep,
  },
});

export const sourceMembershipMismatchFault = (
  witness: SourceMembershipMismatchWitness,
): TransitionFault => ({
  SourceMembershipMismatch: { witness },
});

/** Builds the canonical V1 invalid-one-step transition fault. */
export const invalidOneStepTransitionFault = (
  witness: InvalidOneStepTransitionWitness,
): TransitionFault => ({
  InvalidOneStepTransition: { witness },
});

export const omittedDueL1EventFault = (
  witness: OmittedDueL1EventWitness,
): TransitionFault => ({
  OmittedDueL1Event: { witness },
});

export const duplicateTraceEventFault = ({
  leftTrace,
  rightTrace,
}: {
  readonly leftTrace: IndexedTraceProof;
  readonly rightTrace: IndexedTraceProof;
}): TransitionFault => ({
  DuplicateTraceEvent: {
    left_trace: leftTrace,
    right_trace: rightTrace,
  },
});

export const outOfWindowSourceEventFault = (
  witness: OutOfWindowSourceEventWitness,
): TransitionFault => ({
  OutOfWindowSourceEvent: { witness },
});

export const countFault = (witness: CountFaultWitness): TransitionFault => ({
  CountFault: { witness },
});

export const acceptedTransactionTransitionMismatchFault = ({
  claim,
  terminalAcceptanceWitnessCbor,
}: {
  readonly claim: ValidationClaimWitness;
  readonly terminalAcceptanceWitnessCbor: string;
}): TransitionFault => ({
  AcceptedTransactionTransitionMismatch: {
    witness: {
      claim,
      terminal_acceptance_witness_cbor: terminalAcceptanceWitnessCbor,
    },
  },
});

export type TransitionTraceFaultProofFixture = {
  readonly proof: TransitionFaultProof;
  readonly routeArgs: TransitionTraceRouteArgs;
  readonly routeRedeemer: TransitionTraceRouteSpendRedeemer;
  readonly threadAssetName: string;
};
