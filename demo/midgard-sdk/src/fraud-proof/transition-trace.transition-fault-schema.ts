import { asDataType } from "@al-ft/midgard-core/lucid-data";
import { Data } from "@lucid-evolution/lucid";

import {
  type Header,
  type HeaderHash,
  HeaderHashSchema,
  HeaderSchema,
} from "../ledger-state.js";
import {
  type OperatorVerdict,
  OperatorVerdictSchema,
} from "../rejection-reason.js";
import {
  type AdjacentTraceProof,
  AdjacentTraceProofSchema,
  type DepositSourceMembershipProof,
  DepositSourceMembershipProofSchema,
  type EventToStepMembershipProof,
  type EventToStepProof,
  EventToStepProofSchema,
  type ForcedTransactionSourceMembershipProof,
  ForcedTransactionSourceMembershipProofSchema,
  type IndexedTraceProof,
  type RootCountProof,
  RootCountProofSchema,
  TransitionTraceMembershipProofSchema,
  type WithdrawalSourceMembershipProof,
  WithdrawalSourceMembershipProofSchema,
} from "../transition-trace.js";
import { faultProofStepDatumSchema } from "./native.js";
import {
  DepositSourceNonMembershipProof,
  DepositSourceNonMembershipProofSchema,
  ForcedTransactionSourceNonMembershipProof,
  ForcedTransactionSourceNonMembershipProofSchema,
  InvalidOneStepTransitionWitnessSchema,
  L2TransactionSourceMembershipProof,
  LedgerDeleteWitness,
  LedgerInsertWitness,
  SourceMembershipMismatchWitness,
  SourceMembershipMismatchWitnessSchema,
  TraceBoundarySide,
  TraceBoundarySideSchema,
  WithdrawalSourceNonMembershipProof,
  WithdrawalSourceNonMembershipProofSchema,
} from "./transition-trace.invalid-one-step-transition-witness-schema.js";
import {
  type ValidationClaimWitness,
  ValidationClaimWitnessSchema,
} from "./validation-dispute.js";

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
        readonly projected_utxo: LedgerInsertWitness;
      };
    }
  | {
      readonly L2TransactionTransition: {
        readonly trace_proof: IndexedTraceProof;
        readonly event_to_step: EventToStepMembershipProof;
        readonly source_membership: L2TransactionSourceMembershipProof;
        readonly spend_inputs_preimage: string;
        readonly outputs_preimage: string;
        readonly spent_utxos: readonly LedgerDeleteWitness[];
        readonly produced_utxos: readonly LedgerInsertWitness[];
      };
    };

export const InvalidOneStepTransitionWitness =
  asDataType<InvalidOneStepTransitionWitness>(
    InvalidOneStepTransitionWitnessSchema,
  );

export const OmittedDueL1EventWitnessSchema = Data.Enum([
  Data.Object({
    OmittedDueDeposit: Data.Object({
      source_non_membership: DepositSourceNonMembershipProofSchema,
    }),
  }),
  Data.Object({
    OmittedDueWithdrawal: Data.Object({
      source_non_membership: WithdrawalSourceNonMembershipProofSchema,
    }),
  }),
  Data.Object({
    OmittedDueForcedTransaction: Data.Object({
      event_ref_input_index: Data.Integer(),
      event_asset_name: Data.Bytes(),
      validity_override: OperatorVerdictSchema,
      source_non_membership: ForcedTransactionSourceNonMembershipProofSchema,
    }),
  }),
]);

export type OmittedDueL1EventWitness =
  | {
      readonly OmittedDueDeposit: {
        readonly source_non_membership: DepositSourceNonMembershipProof;
      };
    }
  | {
      readonly OmittedDueWithdrawal: {
        readonly source_non_membership: WithdrawalSourceNonMembershipProof;
      };
    }
  | {
      readonly OmittedDueForcedTransaction: {
        readonly event_ref_input_index: bigint;
        readonly event_asset_name: string;
        readonly validity_override: OperatorVerdict;
        readonly source_non_membership: ForcedTransactionSourceNonMembershipProof;
      };
    };

export const OmittedDueL1EventWitness = asDataType<OmittedDueL1EventWitness>(
  OmittedDueL1EventWitnessSchema,
);

export const OutOfWindowSourceEventWitnessSchema = Data.Enum([
  Data.Object({
    OutOfWindowDeposit: Data.Object({
      source_membership: DepositSourceMembershipProofSchema,
    }),
  }),
  Data.Object({
    OutOfWindowWithdrawal: Data.Object({
      source_membership: WithdrawalSourceMembershipProofSchema,
    }),
  }),
  Data.Object({
    OutOfWindowForcedTransaction: Data.Object({
      event_ref_input_index: Data.Integer(),
      event_asset_name: Data.Bytes(),
      validity_override: OperatorVerdictSchema,
      source_membership: ForcedTransactionSourceMembershipProofSchema,
    }),
  }),
]);

export type OutOfWindowSourceEventWitness =
  | {
      readonly OutOfWindowDeposit: {
        readonly source_membership: DepositSourceMembershipProof;
      };
    }
  | {
      readonly OutOfWindowWithdrawal: {
        readonly source_membership: WithdrawalSourceMembershipProof;
      };
    }
  | {
      readonly OutOfWindowForcedTransaction: {
        readonly event_ref_input_index: bigint;
        readonly event_asset_name: string;
        readonly validity_override: OperatorVerdict;
        readonly source_membership: ForcedTransactionSourceMembershipProof;
      };
    };

export const OutOfWindowSourceEventWitness =
  asDataType<OutOfWindowSourceEventWitness>(
    OutOfWindowSourceEventWitnessSchema,
  );

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

export const CountFaultWitness = asDataType<CountFaultWitness>(
  CountFaultWitnessSchema,
);

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
        claim: ValidationClaimWitnessSchema,
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
          readonly claim: ValidationClaimWitness;
          readonly terminal_acceptance_witness_cbor: string;
        };
      };
    };

export const TransitionFault = asDataType<TransitionFault>(
  TransitionFaultSchema,
);

export const TransitionFaultProofSchema = Data.Object({
  challenged_header_hash: HeaderHashSchema,
  header: HeaderSchema,
  fault: TransitionFaultSchema,
});

export type TransitionFaultProof = {
  readonly challenged_header_hash: HeaderHash;
  readonly header: Header;
  readonly fault: TransitionFault;
};

export const TransitionFaultProof = asDataType<TransitionFaultProof>(
  TransitionFaultProofSchema,
);

export const TransitionTraceStepDatumSchema = faultProofStepDatumSchema(
  TransitionFaultProofSchema,
);

export type TransitionTraceStepDatum = {
  readonly fraud_prover: string;
  readonly data: TransitionFaultProof | null;
};

export const TransitionTraceStepDatum = asDataType<TransitionTraceStepDatum>(
  TransitionTraceStepDatumSchema,
);
