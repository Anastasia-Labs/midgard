import { Data } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import {
  hashHexWithBlake2b,
  type HashingError,
  type MerkleRoot,
  MerkleRootSchema,
  type OutputReference,
  OutputReferenceSchema,
  type Proof,
  ProofSchema,
} from "@/common.js";
import { EMPTY_MERKLE_TREE_ROOT } from "@/ledger-constants.js";
import {
  type DepositInfo,
  DepositInfoSchema,
  type EventKey,
  EventKeySchema,
  type EventToStepValue,
  EventToStepValueSchema,
  type ForcedInclusionTxV1,
  ForcedInclusionTxV1Schema,
  type TransitionStep,
  TransitionStepSchema,
  type WithdrawalInfo,
  WithdrawalInfoSchema,
} from "@/ledger-state.js";

type DataSchema = Parameters<typeof Data.Nullable>[0];

export const RootDomainSchema = Data.Enum([
  Data.Literal("WithdrawalsRootDomain"),
  Data.Literal("ForcedTransactionsV1RootDomain"),
  Data.Literal("TransactionsV1RootDomain"),
  Data.Literal("DepositsRootDomain"),
  Data.Literal("TransitionTraceRootDomain"),
  Data.Literal("EventToStepRootDomain"),
  Data.Literal("ValidationTracesRootDomain"),
]);
export type RootDomain = Data.Static<typeof RootDomainSchema>;
export const RootDomain = RootDomainSchema as unknown as RootDomain;

export const ROOT_DOMAINS = {
  withdrawals: "WithdrawalsRootDomain",
  forcedTransactionsV1: "ForcedTransactionsV1RootDomain",
  transactionsV1: "TransactionsV1RootDomain",
  deposits: "DepositsRootDomain",
  transitionTrace: "TransitionTraceRootDomain",
  eventToStep: "EventToStepRootDomain",
  validationTraces: "ValidationTracesRootDomain",
} as const satisfies Record<string, RootDomain>;

export type RootCountProof = {
  readonly domain: RootDomain;
  readonly root: MerkleRoot;
  readonly phas_root: MerkleRoot;
  readonly count: bigint;
};

export const RootCountProofSchema = Data.Object({
  domain: RootDomainSchema,
  root: MerkleRootSchema,
  phas_root: MerkleRootSchema,
  count: Data.Integer(),
});
export const RootCountProof = RootCountProofSchema as unknown as RootCountProof;

const COUNTED_ROOT_TAG_HEX = Buffer.from("MidgardRootCountV1", "utf8").toString(
  "hex",
);

export const countedRootPreimageHex = ({
  domain,
  phasRoot,
  count,
}: {
  readonly domain: RootDomain;
  readonly phasRoot: MerkleRoot;
  readonly count: bigint;
}): string =>
  [
    COUNTED_ROOT_TAG_HEX,
    Data.to(domain as never, RootDomainSchema as never),
    phasRoot,
    Data.to(count as never, Data.Integer() as never),
  ].join("");

export const commitCountedRootProgram = ({
  domain,
  phasRoot,
  count,
}: {
  readonly domain: RootDomain;
  readonly phasRoot: MerkleRoot;
  readonly count: bigint;
}): Effect.Effect<MerkleRoot, HashingError> => {
  if (count === 0n && phasRoot === EMPTY_MERKLE_TREE_ROOT) {
    return Effect.succeed(EMPTY_MERKLE_TREE_ROOT);
  }
  return hashHexWithBlake2b(
    countedRootPreimageHex({ domain, phasRoot, count }),
    32,
  );
};

export type RootMembershipProof<K, V> = RootCountProof & {
  readonly key: K;
  readonly value: V;
  readonly proof: Proof;
};

export const rootMembershipProofSchema = <
  K extends DataSchema,
  V extends DataSchema,
>(
  keySchema: K,
  valueSchema: V,
) =>
  Data.Object({
    domain: RootDomainSchema,
    root: MerkleRootSchema,
    phas_root: MerkleRootSchema,
    count: Data.Integer(),
    key: keySchema,
    value: valueSchema,
    proof: ProofSchema,
  });

export type RootNonMembershipProof<K> = RootCountProof & {
  readonly key: K;
  readonly proof: Proof;
};

export const rootNonMembershipProofSchema = <K extends DataSchema>(
  keySchema: K,
) =>
  Data.Object({
    domain: RootDomainSchema,
    root: MerkleRootSchema,
    phas_root: MerkleRootSchema,
    count: Data.Integer(),
    key: keySchema,
    proof: ProofSchema,
  });

export const RawRootMembershipProofSchema = rootMembershipProofSchema(
  Data.Bytes(),
  Data.Bytes(),
);
export type RawRootMembershipProof = RootMembershipProof<string, string>;
export const RawRootMembershipProof =
  RawRootMembershipProofSchema as unknown as RawRootMembershipProof;

export const DepositSourceMembershipProofSchema = rootMembershipProofSchema(
  OutputReferenceSchema,
  DepositInfoSchema,
);
export type DepositSourceMembershipProof = RootMembershipProof<
  OutputReference,
  DepositInfo
>;
export const DepositSourceMembershipProof =
  DepositSourceMembershipProofSchema as unknown as DepositSourceMembershipProof;

export const WithdrawalSourceMembershipProofSchema = rootMembershipProofSchema(
  OutputReferenceSchema,
  WithdrawalInfoSchema,
);
export type WithdrawalSourceMembershipProof = RootMembershipProof<
  OutputReference,
  WithdrawalInfo
>;
export const WithdrawalSourceMembershipProof =
  WithdrawalSourceMembershipProofSchema as unknown as WithdrawalSourceMembershipProof;

export const ForcedTransactionSourceMembershipProofSchema =
  rootMembershipProofSchema(OutputReferenceSchema, ForcedInclusionTxV1Schema);
export type ForcedTransactionSourceMembershipProof = RootMembershipProof<
  OutputReference,
  ForcedInclusionTxV1
>;
export const ForcedTransactionSourceMembershipProof =
  ForcedTransactionSourceMembershipProofSchema as unknown as ForcedTransactionSourceMembershipProof;

export const EventSettlementMembershipProofSchema = Data.Enum([
  Data.Object({
    DepositMembership: Data.Object({
      witness: RawRootMembershipProofSchema,
    }),
  }),
  Data.Object({
    WithdrawalMembership: Data.Object({
      witness: RawRootMembershipProofSchema,
    }),
  }),
  Data.Object({
    TxOrderMembership: Data.Object({
      witness: RawRootMembershipProofSchema,
    }),
  }),
]);
export type EventSettlementMembershipProof = Data.Static<
  typeof EventSettlementMembershipProofSchema
>;
export const EventSettlementMembershipProof =
  EventSettlementMembershipProofSchema as unknown as EventSettlementMembershipProof;

export const RawRootNonMembershipProofSchema = rootNonMembershipProofSchema(
  Data.Bytes(),
);
export type RawRootNonMembershipProof = RootNonMembershipProof<string>;
export const RawRootNonMembershipProof =
  RawRootNonMembershipProofSchema as unknown as RawRootNonMembershipProof;

export const TransitionTraceMembershipProofSchema = rootMembershipProofSchema(
  Data.Integer(),
  TransitionStepSchema,
);
export type IndexedTraceProof = RootMembershipProof<bigint, TransitionStep>;
export const IndexedTraceProof =
  TransitionTraceMembershipProofSchema as unknown as IndexedTraceProof;

export const AdjacentTraceProofSchema = Data.Object({
  lower: TransitionTraceMembershipProofSchema,
  upper: TransitionTraceMembershipProofSchema,
});
export type AdjacentTraceProof = {
  readonly lower: IndexedTraceProof;
  readonly upper: IndexedTraceProof;
};
export const AdjacentTraceProof =
  AdjacentTraceProofSchema as unknown as AdjacentTraceProof;

export const EventToStepMembershipProofSchema = rootMembershipProofSchema(
  EventKeySchema,
  EventToStepValueSchema,
);
export type EventToStepMembershipProof = RootMembershipProof<
  EventKey,
  EventToStepValue
>;
export const EventToStepMembershipProof =
  EventToStepMembershipProofSchema as unknown as EventToStepMembershipProof;

export const EventToStepNonMembershipProofSchema =
  rootNonMembershipProofSchema(EventKeySchema);
export type EventToStepNonMembershipProof = RootNonMembershipProof<EventKey>;
export const EventToStepNonMembershipProof =
  EventToStepNonMembershipProofSchema as unknown as EventToStepNonMembershipProof;

export const EventToStepProofSchema = Data.Enum([
  Data.Object({
    EventToStepMembership: Data.Object({
      membership: EventToStepMembershipProofSchema,
    }),
  }),
  Data.Object({
    EventToStepNonMembership: Data.Object({
      non_membership: EventToStepNonMembershipProofSchema,
    }),
  }),
]);
export type EventToStepProof =
  | {
      readonly EventToStepMembership: {
        readonly membership: EventToStepMembershipProof;
      };
    }
  | {
      readonly EventToStepNonMembership: {
        readonly non_membership: EventToStepNonMembershipProof;
      };
    };
export const EventToStepProof =
  EventToStepProofSchema as unknown as EventToStepProof;
