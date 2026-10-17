import { asDataType } from "@al-ft/midgard-core/lucid-data";
import { Data } from "@lucid-evolution/lucid";
import { Data as EffectData, Effect } from "effect";

import {
  GenericErrorFields,
  MerkleRoot,
  MerkleRootSchema,
  POSIXTimeSchema,
  PubKeyHashSchema,
} from "./common.js";
import { EMPTY_MERKLE_TREE_ROOT } from "./ledger-constants.js";

export const HeaderHashSchema = Data.Bytes({ minLength: 28, maxLength: 28 });

export type HeaderHash = Data.Static<typeof HeaderHashSchema>;

export const HeaderHash = asDataType<HeaderHash>(HeaderHashSchema);

/** Canonical proof-complete Midgard V1 block header. */
export const HeaderSchema = Data.Object({
  prevUtxosRoot: MerkleRootSchema,
  utxosRoot: MerkleRootSchema,
  withdrawalsRoot: MerkleRootSchema,
  forcedTransactionsRoot: MerkleRootSchema,
  transactionsRoot: MerkleRootSchema,
  depositsRoot: MerkleRootSchema,
  transitionTraceRoot: MerkleRootSchema,
  eventToStepRoot: MerkleRootSchema,
  validationTracesRoot: MerkleRootSchema,
  withdrawalCount: Data.Integer(),
  forcedTransactionCount: Data.Integer(),
  l2TransactionCount: Data.Integer(),
  depositCount: Data.Integer(),
  totalEventCount: Data.Integer(),
  transitionStepCount: Data.Integer(),
  validationTraceCount: Data.Integer(),
  startTime: POSIXTimeSchema,
  endTime: POSIXTimeSchema,
  blockSlot: Data.Integer(),
  expectedNetworkId: Data.Integer(),
  minFeeA: Data.Integer(),
  minFeeB: Data.Integer(),
  prevHeaderHash: HeaderHashSchema,
  operatorVkey: PubKeyHashSchema,
  protocolVersion: Data.Integer(),
});

export type Header = Data.Static<typeof HeaderSchema>;

export const Header = asDataType<Header>(HeaderSchema);

export const HeaderTransitionCommitmentsSchema = Data.Object({
  forcedTransactionsRoot: MerkleRootSchema,
  transitionTraceRoot: MerkleRootSchema,
  eventToStepRoot: MerkleRootSchema,
  validationTracesRoot: MerkleRootSchema,
  withdrawalCount: Data.Integer(),
  forcedTransactionCount: Data.Integer(),
  l2TransactionCount: Data.Integer(),
  depositCount: Data.Integer(),
  totalEventCount: Data.Integer(),
  transitionStepCount: Data.Integer(),
  validationTraceCount: Data.Integer(),
});

export type HeaderTransitionCommitments = Data.Static<
  typeof HeaderTransitionCommitmentsSchema
>;

export const HeaderTransitionCommitments =
  asDataType<HeaderTransitionCommitments>(HeaderTransitionCommitmentsSchema);

export const EMPTY_HEADER_TRANSITION_COMMITMENTS: HeaderTransitionCommitments =
  {
    forcedTransactionsRoot: EMPTY_MERKLE_TREE_ROOT,
    transitionTraceRoot: EMPTY_MERKLE_TREE_ROOT,
    eventToStepRoot: EMPTY_MERKLE_TREE_ROOT,
    validationTracesRoot: EMPTY_MERKLE_TREE_ROOT,
    withdrawalCount: 0n,
    forcedTransactionCount: 0n,
    l2TransactionCount: 0n,
    depositCount: 0n,
    totalEventCount: 0n,
    transitionStepCount: 0n,
    validationTraceCount: 0n,
  };

export type HeaderTransitionCommitmentSourceRoots = Pick<
  Header,
  | "withdrawalsRoot"
  | "forcedTransactionsRoot"
  | "transactionsRoot"
  | "depositsRoot"
>;

export type HeaderTransitionCommitmentCounts = Pick<
  HeaderTransitionCommitments,
  | "withdrawalCount"
  | "forcedTransactionCount"
  | "l2TransactionCount"
  | "depositCount"
>;

export type MakeHeaderTransitionCommitmentsInput =
  HeaderTransitionCommitmentSourceRoots &
    HeaderTransitionCommitmentCounts &
    Partial<
      Pick<
        HeaderTransitionCommitments,
        "transitionTraceRoot" | "eventToStepRoot" | "transitionStepCount"
      >
    > & {
      readonly validationTracesRoot: MerkleRoot;
      readonly validationTraceCount: bigint;
    };

export type ValidateHeaderTransitionCommitmentsInput =
  HeaderTransitionCommitments &
    Pick<Header, "withdrawalsRoot" | "transactionsRoot" | "depositsRoot">;

export class HeaderTransitionCommitmentsError extends EffectData.TaggedError(
  "HeaderTransitionCommitmentsError",
)<GenericErrorFields> {}

export const headerTransitionCommitmentsError = (
  message: string,
  cause: unknown,
): HeaderTransitionCommitmentsError =>
  new HeaderTransitionCommitmentsError({ message, cause });

export const validateSourceRootCount = (
  label: string,
  root: MerkleRoot,
  count: bigint,
): Effect.Effect<void, HeaderTransitionCommitmentsError> => {
  if (root === EMPTY_MERKLE_TREE_ROOT && count > 0n) {
    return Effect.fail(
      headerTransitionCommitmentsError(
        "Refusing non-empty source event count with an empty source root",
        `${label}_root=${root},${label}_count=${count.toString()}`,
      ),
    );
  }
  if (root !== EMPTY_MERKLE_TREE_ROOT && count === 0n) {
    return Effect.fail(
      headerTransitionCommitmentsError(
        "Refusing non-empty source root with a zero source event count",
        `${label}_root=${root},${label}_count=0`,
      ),
    );
  }
  return Effect.void;
};
