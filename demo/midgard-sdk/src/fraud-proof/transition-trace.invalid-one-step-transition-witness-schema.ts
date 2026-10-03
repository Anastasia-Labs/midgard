import { asDataType } from "@al-ft/midgard-core/lucid-data";
import { Data } from "@lucid-evolution/lucid";

import {
  type OutputReference,
  OutputReferenceSchema,
  type Proof,
  ProofSchema,
} from "../common.js";
import {
  type DepositSourceMembershipProof,
  DepositSourceMembershipProofSchema,
  type EventToStepMembershipProof,
  EventToStepMembershipProofSchema,
  type EventToStepNonMembershipProof,
  EventToStepNonMembershipProofSchema,
  type ForcedTransactionSourceMembershipProof,
  ForcedTransactionSourceMembershipProofSchema,
  type IndexedTraceProof,
  type RawRootMembershipProof,
  RawRootMembershipProofSchema,
  type RawRootNonMembershipProof,
  RawRootNonMembershipProofSchema,
  type RootNonMembershipProof,
  rootNonMembershipProofSchema,
  TransitionTraceMembershipProofSchema,
  type WithdrawalSourceMembershipProof,
  WithdrawalSourceMembershipProofSchema,
} from "../transition-trace.js";

export const TraceBoundarySideSchema = Data.Enum([
  Data.Literal("TraceStart"),
  Data.Literal("TraceEnd"),
]);

export type TraceBoundarySide = "TraceStart" | "TraceEnd";

export const TraceBoundarySide = asDataType<TraceBoundarySide>(
  TraceBoundarySideSchema,
);

export const L2TransactionSourceMembershipProofSchema =
  RawRootMembershipProofSchema;

export type L2TransactionSourceMembershipProof = RawRootMembershipProof;

export const L2TransactionSourceMembershipProof =
  asDataType<L2TransactionSourceMembershipProof>(
    L2TransactionSourceMembershipProofSchema,
  );

export const WithdrawalSourceNonMembershipProofSchema =
  rootNonMembershipProofSchema(OutputReferenceSchema);

export type WithdrawalSourceNonMembershipProof =
  RootNonMembershipProof<OutputReference>;

export const WithdrawalSourceNonMembershipProof =
  asDataType<WithdrawalSourceNonMembershipProof>(
    WithdrawalSourceNonMembershipProofSchema,
  );

export const ForcedTransactionSourceNonMembershipProofSchema =
  rootNonMembershipProofSchema(OutputReferenceSchema);

export type ForcedTransactionSourceNonMembershipProof =
  RootNonMembershipProof<OutputReference>;

export const ForcedTransactionSourceNonMembershipProof =
  asDataType<ForcedTransactionSourceNonMembershipProof>(
    ForcedTransactionSourceNonMembershipProofSchema,
  );

export const L2TransactionSourceNonMembershipProofSchema =
  RawRootNonMembershipProofSchema;

export type L2TransactionSourceNonMembershipProof = RawRootNonMembershipProof;

export const L2TransactionSourceNonMembershipProof =
  asDataType<L2TransactionSourceNonMembershipProof>(
    L2TransactionSourceNonMembershipProofSchema,
  );

export const DepositSourceNonMembershipProofSchema =
  rootNonMembershipProofSchema(OutputReferenceSchema);

export type DepositSourceNonMembershipProof =
  RootNonMembershipProof<OutputReference>;

export const DepositSourceNonMembershipProof =
  asDataType<DepositSourceNonMembershipProof>(
    DepositSourceNonMembershipProofSchema,
  );

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
  asDataType<TransitionSourceMembershipProof>(
    TransitionSourceMembershipProofSchema,
  );

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
  asDataType<TransitionSourceNonMembershipProof>(
    TransitionSourceNonMembershipProofSchema,
  );

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
  asDataType<SourceMembershipMismatchWitness>(
    SourceMembershipMismatchWitnessSchema,
  );

export const LedgerDeleteWitnessSchema = Data.Object({
  key: Data.Bytes(),
  value: Data.Bytes(),
  opening: Data.Bytes(),
  delete_proof: ProofSchema,
});

export type LedgerDeleteWitness = {
  readonly key: string;
  readonly value: string;
  /** The terminal-Branch group opening of the delete (`#""` if unused). */
  readonly opening: string;
  readonly delete_proof: Proof;
};

export const LedgerDeleteWitness = asDataType<LedgerDeleteWitness>(
  LedgerDeleteWitnessSchema,
);

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

export const LedgerInsertWitness = asDataType<LedgerInsertWitness>(
  LedgerInsertWitnessSchema,
);

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
      projected_utxo: LedgerInsertWitnessSchema,
    }),
  }),
  Data.Object({
    L2TransactionTransition: Data.Object({
      trace_proof: TransitionTraceMembershipProofSchema,
      event_to_step: EventToStepMembershipProofSchema,
      source_membership: L2TransactionSourceMembershipProofSchema,
      spend_inputs_preimage: Data.Bytes(),
      outputs_preimage: Data.Bytes(),
      spent_utxos: Data.Array(LedgerDeleteWitnessSchema),
      produced_utxos: Data.Array(LedgerInsertWitnessSchema),
    }),
  }),
]);
