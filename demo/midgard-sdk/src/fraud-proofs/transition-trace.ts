import { Data } from "@lucid-evolution/lucid";

import {
  type ValidationClaimWitnessV1,
  ValidationClaimWitnessV1Schema,
} from "@/fraud-proof/validation-dispute.js";
import {
  type OutputReference,
  OutputReferenceSchema,
  type Proof,
  ProofSchema,
} from "@/common.js";
import {
  type HeaderHash,
  HeaderHashSchema,
  type HeaderV1,
  HeaderV1Schema,
  type MidgardTxValidity,
  MidgardTxValiditySchema,
  type WithdrawalValidity,
  WithdrawalValiditySchema,
} from "@/ledger-state.js";
import {
  type AdjacentTraceProof,
  AdjacentTraceProofSchema,
  type DepositSourceMembershipProof,
  DepositSourceMembershipProofSchema,
  type EventToStepMembershipProof,
  EventToStepMembershipProofSchema,
  type EventToStepNonMembershipProof,
  EventToStepNonMembershipProofSchema,
  type EventToStepProof,
  EventToStepProofSchema,
  type ForcedTransactionSourceMembershipProof,
  ForcedTransactionSourceMembershipProofSchema,
  type IndexedTraceProof,
  type RawRootMembershipProof,
  RawRootMembershipProofSchema,
  type RawRootNonMembershipProof,
  RawRootNonMembershipProofSchema,
  type RootCountProof,
  RootCountProofSchema,
  type RootNonMembershipProof,
  rootNonMembershipProofSchema,
  TransitionTraceMembershipProofSchema,
  type WithdrawalSourceMembershipProof,
  WithdrawalSourceMembershipProofSchema,
} from "@/transition-trace.js";

import {
  type FaultProofStepCancel,
  faultProofStepDatumSchema,
  faultProofStepRedeemerSchema,
} from "../fraud-proof/native.js";

export const TraceBoundarySideSchema = Data.Enum([
  Data.Literal("TraceStart"),
  Data.Literal("TraceEnd"),
]);
export type TraceBoundarySide = "TraceStart" | "TraceEnd";
export const TraceBoundarySide =
  TraceBoundarySideSchema as unknown as TraceBoundarySide;

export const L2TransactionSourceMembershipProofSchema =
  RawRootMembershipProofSchema;
export type L2TransactionSourceMembershipProof = RawRootMembershipProof;
export const L2TransactionSourceMembershipProof =
  L2TransactionSourceMembershipProofSchema as unknown as L2TransactionSourceMembershipProof;

export const WithdrawalSourceNonMembershipProofSchema =
  rootNonMembershipProofSchema(OutputReferenceSchema);
export type WithdrawalSourceNonMembershipProof =
  RootNonMembershipProof<OutputReference>;
export const WithdrawalSourceNonMembershipProof =
  WithdrawalSourceNonMembershipProofSchema as unknown as WithdrawalSourceNonMembershipProof;

export const ForcedTransactionSourceNonMembershipProofSchema =
  rootNonMembershipProofSchema(OutputReferenceSchema);
export type ForcedTransactionSourceNonMembershipProof =
  RootNonMembershipProof<OutputReference>;
export const ForcedTransactionSourceNonMembershipProof =
  ForcedTransactionSourceNonMembershipProofSchema as unknown as ForcedTransactionSourceNonMembershipProof;

export const L2TransactionSourceNonMembershipProofSchema =
  RawRootNonMembershipProofSchema;
export type L2TransactionSourceNonMembershipProof = RawRootNonMembershipProof;
export const L2TransactionSourceNonMembershipProof =
  L2TransactionSourceNonMembershipProofSchema as unknown as L2TransactionSourceNonMembershipProof;

export const DepositSourceNonMembershipProofSchema =
  rootNonMembershipProofSchema(OutputReferenceSchema);
export type DepositSourceNonMembershipProof =
  RootNonMembershipProof<OutputReference>;
export const DepositSourceNonMembershipProof =
  DepositSourceNonMembershipProofSchema as unknown as DepositSourceNonMembershipProof;

export const TransitionSourceMembershipProofSchema = Data.Enum([
  Data.Object({
    WithdrawalSourceMembership: Data.Object({
      membership: WithdrawalSourceMembershipProofSchema,
    }),
  }),
  Data.Object({
    ForcedTransactionSourceMembership: Data.Object({
      membership: ForcedTransactionSourceMembershipProofSchema,
    }),
  }),
  Data.Object({
    L2TransactionSourceMembership: Data.Object({
      membership: L2TransactionSourceMembershipProofSchema,
    }),
  }),
  Data.Object({
    DepositSourceMembership: Data.Object({
      membership: DepositSourceMembershipProofSchema,
    }),
  }),
]);
export type TransitionSourceMembershipProof =
  | {
      readonly WithdrawalSourceMembership: {
        readonly membership: WithdrawalSourceMembershipProof;
      };
    }
  | {
      readonly ForcedTransactionSourceMembership: {
        readonly membership: ForcedTransactionSourceMembershipProof;
      };
    }
  | {
      readonly L2TransactionSourceMembership: {
        readonly membership: L2TransactionSourceMembershipProof;
      };
    }
  | {
      readonly DepositSourceMembership: {
        readonly membership: DepositSourceMembershipProof;
      };
    };
export const TransitionSourceMembershipProof =
  TransitionSourceMembershipProofSchema as unknown as TransitionSourceMembershipProof;

export const TransitionSourceNonMembershipProofSchema = Data.Enum([
  Data.Object({
    WithdrawalSourceNonMembership: Data.Object({
      non_membership: WithdrawalSourceNonMembershipProofSchema,
    }),
  }),
  Data.Object({
    ForcedTransactionSourceNonMembership: Data.Object({
      non_membership: ForcedTransactionSourceNonMembershipProofSchema,
    }),
  }),
  Data.Object({
    L2TransactionSourceNonMembership: Data.Object({
      non_membership: L2TransactionSourceNonMembershipProofSchema,
    }),
  }),
  Data.Object({
    DepositSourceNonMembership: Data.Object({
      non_membership: DepositSourceNonMembershipProofSchema,
    }),
  }),
]);
export type TransitionSourceNonMembershipProof =
  | {
      readonly WithdrawalSourceNonMembership: {
        readonly non_membership: WithdrawalSourceNonMembershipProof;
      };
    }
  | {
      readonly ForcedTransactionSourceNonMembership: {
        readonly non_membership: ForcedTransactionSourceNonMembershipProof;
      };
    }
  | {
      readonly L2TransactionSourceNonMembership: {
        readonly non_membership: L2TransactionSourceNonMembershipProof;
      };
    }
  | {
      readonly DepositSourceNonMembership: {
        readonly non_membership: DepositSourceNonMembershipProof;
      };
    };
export const TransitionSourceNonMembershipProof =
  TransitionSourceNonMembershipProofSchema as unknown as TransitionSourceNonMembershipProof;

export const SourceMembershipMismatchWitnessSchema = Data.Enum([
  Data.Object({
    MappedEventMissingFromSource: Data.Object({
      trace_proof: TransitionTraceMembershipProofSchema,
      event_to_step: EventToStepMembershipProofSchema,
      source_non_membership: TransitionSourceNonMembershipProofSchema,
    }),
  }),
  Data.Object({
    SourceEventMissingTrace: Data.Object({
      source_membership: TransitionSourceMembershipProofSchema,
      event_to_step_non_membership: EventToStepNonMembershipProofSchema,
    }),
  }),
  Data.Object({
    SourcePhaseMismatch: Data.Object({
      trace_proof: TransitionTraceMembershipProofSchema,
      source_membership: TransitionSourceMembershipProofSchema,
    }),
  }),
]);
export type SourceMembershipMismatchWitness =
  | {
      readonly MappedEventMissingFromSource: {
        readonly trace_proof: IndexedTraceProof;
        readonly event_to_step: EventToStepMembershipProof;
        readonly source_non_membership: TransitionSourceNonMembershipProof;
      };
    }
  | {
      readonly SourceEventMissingTrace: {
        readonly source_membership: TransitionSourceMembershipProof;
        readonly event_to_step_non_membership: EventToStepNonMembershipProof;
      };
    }
  | {
      readonly SourcePhaseMismatch: {
        readonly trace_proof: IndexedTraceProof;
        readonly source_membership: TransitionSourceMembershipProof;
      };
    };
export const SourceMembershipMismatchWitness =
  SourceMembershipMismatchWitnessSchema as unknown as SourceMembershipMismatchWitness;

export const LedgerDeleteWitnessSchema = Data.Object({
  key: Data.Bytes(),
  value: Data.Bytes(),
  membership_proof: ProofSchema,
  delete_proof: ProofSchema,
});
export type LedgerDeleteWitness = {
  readonly key: string;
  readonly value: string;
  readonly membership_proof: Proof;
  readonly delete_proof: Proof;
};
export const LedgerDeleteWitness =
  LedgerDeleteWitnessSchema as unknown as LedgerDeleteWitness;

export const LedgerInsertWitnessSchema = Data.Object({
  key: Data.Bytes(),
  value: Data.Bytes(),
  non_membership_proof: ProofSchema,
  insert_proof: ProofSchema,
});
export type LedgerInsertWitness = {
  readonly key: string;
  readonly value: string;
  readonly non_membership_proof: Proof;
  readonly insert_proof: Proof;
};
export const LedgerInsertWitness =
  LedgerInsertWitnessSchema as unknown as LedgerInsertWitness;

export const InvalidOneStepTransitionWitnessSchema = Data.Enum([
  Data.Object({
    ValidWithdrawalTransition: Data.Object({
      trace_proof: TransitionTraceMembershipProofSchema,
      event_to_step: EventToStepMembershipProofSchema,
      source_membership: WithdrawalSourceMembershipProofSchema,
      spent_utxo: LedgerDeleteWitnessSchema,
    }),
  }),
  Data.Object({
    InvalidWithdrawalNoOpTransition: Data.Object({
      trace_proof: TransitionTraceMembershipProofSchema,
      event_to_step: EventToStepMembershipProofSchema,
      source_membership: WithdrawalSourceMembershipProofSchema,
    }),
  }),
  Data.Object({
    InvalidForcedTransactionNoOpTransition: Data.Object({
      trace_proof: TransitionTraceMembershipProofSchema,
      event_to_step: EventToStepMembershipProofSchema,
      source_membership: ForcedTransactionSourceMembershipProofSchema,
    }),
  }),
  Data.Object({
    ValidDepositTransition: Data.Object({
      trace_proof: TransitionTraceMembershipProofSchema,
      event_to_step: EventToStepMembershipProofSchema,
      source_membership: DepositSourceMembershipProofSchema,
      event_ref_input_index: Data.Integer(),
      event_asset_name: Data.Bytes(),
      projected_utxo: LedgerInsertWitnessSchema,
    }),
  }),
]);
export type InvalidOneStepTransitionWitness =
  | {
      readonly ValidWithdrawalTransition: {
        readonly trace_proof: IndexedTraceProof;
        readonly event_to_step: EventToStepMembershipProof;
        readonly source_membership: WithdrawalSourceMembershipProof;
        readonly spent_utxo: LedgerDeleteWitness;
      };
    }
  | {
      readonly InvalidWithdrawalNoOpTransition: {
        readonly trace_proof: IndexedTraceProof;
        readonly event_to_step: EventToStepMembershipProof;
        readonly source_membership: WithdrawalSourceMembershipProof;
      };
    }
  | {
      readonly InvalidForcedTransactionNoOpTransition: {
        readonly trace_proof: IndexedTraceProof;
        readonly event_to_step: EventToStepMembershipProof;
        readonly source_membership: ForcedTransactionSourceMembershipProof;
      };
    }
  | {
      readonly ValidDepositTransition: {
        readonly trace_proof: IndexedTraceProof;
        readonly event_to_step: EventToStepMembershipProof;
        readonly source_membership: DepositSourceMembershipProof;
        readonly event_ref_input_index: bigint;
        readonly event_asset_name: string;
        readonly projected_utxo: LedgerInsertWitness;
      };
    };
export const InvalidOneStepTransitionWitness =
  InvalidOneStepTransitionWitnessSchema as unknown as InvalidOneStepTransitionWitness;

export const OmittedDueL1EventWitnessSchema = Data.Enum([
  Data.Object({
    OmittedDueDeposit: Data.Object({
      event_ref_input_index: Data.Integer(),
      event_asset_name: Data.Bytes(),
      source_non_membership: DepositSourceNonMembershipProofSchema,
    }),
  }),
  Data.Object({
    OmittedDueWithdrawal: Data.Object({
      event_ref_input_index: Data.Integer(),
      event_asset_name: Data.Bytes(),
      source_non_membership: WithdrawalSourceNonMembershipProofSchema,
    }),
  }),
  Data.Object({
    OmittedDueForcedTransaction: Data.Object({
      event_ref_input_index: Data.Integer(),
      event_asset_name: Data.Bytes(),
      validity_override: MidgardTxValiditySchema,
      source_non_membership: ForcedTransactionSourceNonMembershipProofSchema,
    }),
  }),
]);
export type OmittedDueL1EventWitness =
  | {
      readonly OmittedDueDeposit: {
        readonly event_ref_input_index: bigint;
        readonly event_asset_name: string;
        readonly source_non_membership: DepositSourceNonMembershipProof;
      };
    }
  | {
      readonly OmittedDueWithdrawal: {
        readonly event_ref_input_index: bigint;
        readonly event_asset_name: string;
        readonly source_non_membership: WithdrawalSourceNonMembershipProof;
      };
    }
  | {
      readonly OmittedDueForcedTransaction: {
        readonly event_ref_input_index: bigint;
        readonly event_asset_name: string;
        readonly validity_override: MidgardTxValidity;
        readonly source_non_membership: ForcedTransactionSourceNonMembershipProof;
      };
    };
export const OmittedDueL1EventWitness =
  OmittedDueL1EventWitnessSchema as unknown as OmittedDueL1EventWitness;

export const OutOfWindowSourceEventWitnessSchema = Data.Enum([
  Data.Object({
    OutOfWindowDeposit: Data.Object({
      event_ref_input_index: Data.Integer(),
      event_asset_name: Data.Bytes(),
      source_membership: DepositSourceMembershipProofSchema,
    }),
  }),
  Data.Object({
    OutOfWindowWithdrawal: Data.Object({
      event_ref_input_index: Data.Integer(),
      event_asset_name: Data.Bytes(),
      validity_override: WithdrawalValiditySchema,
      source_membership: WithdrawalSourceMembershipProofSchema,
    }),
  }),
  Data.Object({
    OutOfWindowForcedTransaction: Data.Object({
      event_ref_input_index: Data.Integer(),
      event_asset_name: Data.Bytes(),
      validity_override: MidgardTxValiditySchema,
      source_membership: ForcedTransactionSourceMembershipProofSchema,
    }),
  }),
]);
export type OutOfWindowSourceEventWitness =
  | {
      readonly OutOfWindowDeposit: {
        readonly event_ref_input_index: bigint;
        readonly event_asset_name: string;
        readonly source_membership: DepositSourceMembershipProof;
      };
    }
  | {
      readonly OutOfWindowWithdrawal: {
        readonly event_ref_input_index: bigint;
        readonly event_asset_name: string;
        readonly validity_override: WithdrawalValidity;
        readonly source_membership: WithdrawalSourceMembershipProof;
      };
    }
  | {
      readonly OutOfWindowForcedTransaction: {
        readonly event_ref_input_index: bigint;
        readonly event_asset_name: string;
        readonly validity_override: MidgardTxValidity;
        readonly source_membership: ForcedTransactionSourceMembershipProof;
      };
    };
export const OutOfWindowSourceEventWitness =
  OutOfWindowSourceEventWitnessSchema as unknown as OutOfWindowSourceEventWitness;

export const CountFaultWitnessSchema = Data.Enum([
  Data.Literal("HeaderTotalCountMismatch"),
  Data.Literal("HeaderTransitionStepCountMismatch"),
  Data.Object({
    SourceRootCountMismatch: Data.Object({ proof: RootCountProofSchema }),
  }),
  Data.Object({
    EventToStepRootCountMismatch: Data.Object({ proof: RootCountProofSchema }),
  }),
  Data.Object({
    TransitionTraceRootCountMismatch: Data.Object({
      proof: RootCountProofSchema,
    }),
  }),
]);
export type CountFaultWitness =
  | "HeaderTotalCountMismatch"
  | "HeaderTransitionStepCountMismatch"
  | { readonly SourceRootCountMismatch: { readonly proof: RootCountProof } }
  | {
      readonly EventToStepRootCountMismatch: {
        readonly proof: RootCountProof;
      };
    }
  | {
      readonly TransitionTraceRootCountMismatch: {
        readonly proof: RootCountProof;
      };
    };
export const CountFaultWitness =
  CountFaultWitnessSchema as unknown as CountFaultWitness;

export const TransitionFaultSchema = Data.Enum([
  Data.Object({
    TraceBoundaryFault: Data.Object({
      side: TraceBoundarySideSchema,
      trace_proof: TransitionTraceMembershipProofSchema,
    }),
  }),
  Data.Object({
    TraceLinkFault: Data.Object({ adjacent: AdjacentTraceProofSchema }),
  }),
  Data.Object({
    EventToStepMismatch: Data.Object({
      trace_proof: TransitionTraceMembershipProofSchema,
      event_to_step: EventToStepProofSchema,
    }),
  }),
  Data.Object({
    SourceMembershipMismatch: Data.Object({
      witness: SourceMembershipMismatchWitnessSchema,
    }),
  }),
  Data.Object({
    InvalidOneStepTransition: Data.Object({
      witness: InvalidOneStepTransitionWitnessSchema,
    }),
  }),
  Data.Object({
    OmittedDueL1Event: Data.Object({ witness: OmittedDueL1EventWitnessSchema }),
  }),
  Data.Object({
    DuplicateTraceEvent: Data.Object({
      left_trace: TransitionTraceMembershipProofSchema,
      right_trace: TransitionTraceMembershipProofSchema,
    }),
  }),
  Data.Object({
    OutOfWindowSourceEvent: Data.Object({
      witness: OutOfWindowSourceEventWitnessSchema,
    }),
  }),
  Data.Object({
    CountFault: Data.Object({ witness: CountFaultWitnessSchema }),
  }),
  Data.Object({
    AcceptedTransactionTransitionMismatch: Data.Object({
      witness: Data.Object({
        claim: ValidationClaimWitnessV1Schema,
        terminal_acceptance_witness_cbor: Data.Bytes(),
      }),
    }),
  }),
]);
export type TransitionFault =
  | {
      readonly TraceBoundaryFault: {
        readonly side: TraceBoundarySide;
        readonly trace_proof: IndexedTraceProof;
      };
    }
  | { readonly TraceLinkFault: { readonly adjacent: AdjacentTraceProof } }
  | {
      readonly EventToStepMismatch: {
        readonly trace_proof: IndexedTraceProof;
        readonly event_to_step: EventToStepProof;
      };
    }
  | {
      readonly SourceMembershipMismatch: {
        readonly witness: SourceMembershipMismatchWitness;
      };
    }
  | {
      readonly InvalidOneStepTransition: {
        readonly witness: InvalidOneStepTransitionWitness;
      };
    }
  | {
      readonly OmittedDueL1Event: {
        readonly witness: OmittedDueL1EventWitness;
      };
    }
  | {
      readonly DuplicateTraceEvent: {
        readonly left_trace: IndexedTraceProof;
        readonly right_trace: IndexedTraceProof;
      };
    }
  | {
      readonly OutOfWindowSourceEvent: {
        readonly witness: OutOfWindowSourceEventWitness;
      };
    }
  | { readonly CountFault: { readonly witness: CountFaultWitness } }
  | {
      readonly AcceptedTransactionTransitionMismatch: {
        readonly witness: {
          readonly claim: ValidationClaimWitnessV1;
          readonly terminal_acceptance_witness_cbor: string;
        };
      };
    };
export const TransitionFault =
  TransitionFaultSchema as unknown as TransitionFault;

export const TransitionFaultProofSchema = Data.Object({
  challenged_header_hash: HeaderHashSchema,
  header: HeaderV1Schema,
  fault: TransitionFaultSchema,
});
export type TransitionFaultProof = {
  readonly challenged_header_hash: HeaderHash;
  readonly header: HeaderV1;
  readonly fault: TransitionFault;
};
export const TransitionFaultProof =
  TransitionFaultProofSchema as unknown as TransitionFaultProof;

export const TransitionTraceStepDatumSchema = faultProofStepDatumSchema(
  TransitionFaultProofSchema,
);
export type TransitionTraceStepDatum = {
  readonly fraud_prover: string;
  readonly data: TransitionFaultProof | null;
};
export const TransitionTraceStepDatum =
  TransitionTraceStepDatumSchema as unknown as TransitionTraceStepDatum;

export const TransitionTraceRouteArgsSchema = Data.Object({
  input_index: Data.Integer(),
  output_index: Data.Integer(),
  proof: TransitionFaultProofSchema,
});
export type TransitionTraceRouteArgs = {
  readonly input_index: bigint;
  readonly output_index: bigint;
  readonly proof: TransitionFaultProof;
};
export const TransitionTraceRouteArgs =
  TransitionTraceRouteArgsSchema as unknown as TransitionTraceRouteArgs;

export const TransitionTraceRouteSpendRedeemerSchema =
  faultProofStepRedeemerSchema(TransitionTraceRouteArgsSchema);
export type TransitionTraceRouteSpendRedeemer =
  | { readonly Cancel: FaultProofStepCancel }
  | { readonly Continue: readonly [TransitionTraceRouteArgs] };
export const TransitionTraceRouteSpendRedeemer =
  TransitionTraceRouteSpendRedeemerSchema as unknown as TransitionTraceRouteSpendRedeemer;

export const TransitionTraceFinalArgsSchema = Data.Object({
  input_index: Data.Integer(),
  output_index: Data.Integer(),
  hub_ref_input_index: Data.Integer(),
  fraud_proof_mint_redeemer_index: Data.Integer(),
});
export type TransitionTraceFinalArgs = Data.Static<
  typeof TransitionTraceFinalArgsSchema
>;
export const TransitionTraceFinalArgs =
  TransitionTraceFinalArgsSchema as unknown as TransitionTraceFinalArgs;

export const TransitionTraceFinalSpendRedeemerSchema =
  faultProofStepRedeemerSchema(TransitionTraceFinalArgsSchema);
export type TransitionTraceFinalSpendRedeemer =
  | { readonly Cancel: FaultProofStepCancel }
  | { readonly Continue: readonly [TransitionTraceFinalArgs] };
export const TransitionTraceFinalSpendRedeemer =
  TransitionTraceFinalSpendRedeemerSchema as unknown as TransitionTraceFinalSpendRedeemer;

export const makeTransitionFaultProof = ({
  challengedHeaderHash,
  header,
  fault,
}: {
  readonly challengedHeaderHash: HeaderHash;
  readonly header: HeaderV1;
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
  readonly claim: ValidationClaimWitnessV1;
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
