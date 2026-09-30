import {
  type WorkflowActuationPermit,
  type WorkflowAdapterRunner,
  type WorkflowFundingReservationPermit,
} from "@al-ft/midgard-fault-proofs";
import type { FraudProofCatalogueCategoryName } from "@al-ft/midgard-sdk";
import { type UTxO } from "@lucid-evolution/lucid";

import { type VerifiedWatcherDeploymentIdentity } from "../runtime/deployment-identity.js";
import {
  parseWatcherProverFundingReservationRecord,
  type WatcherProverFundingReservationPlan,
  type WatcherProverFundingReservationRecord,
  type WatcherProverFundingReservationStore,
} from "./prover-funding-reservation.js";

export const WATCHER_PROVER_FUNDING_AUTHORITY =
  "midgard-watcher-production-prover-funding-authority-v1" as const;

export type WatcherProverFundingAuthority = Readonly<{
  schemaVersion: typeof WATCHER_PROVER_FUNDING_AUTHORITY;
  plan: WatcherProverFundingReservationPlan;
  permit: WorkflowFundingReservationPermit;
}>;

export type WatcherProverFundingAuthorityFactory = Readonly<{
  schemaVersion: typeof WATCHER_PROVER_FUNDING_AUTHORITY;
  /** Omit scope only before startup dispatch; finalizers supply their minted permit. */
  releaseUnused(input?: {
    readonly actuationPermit: WorkflowActuationPermit;
  }): Promise<void>;
  create(input: {
    readonly category: FraudProofCatalogueCategoryName;
    readonly runner: WorkflowAdapterRunner;
    readonly actuationPermit: WorkflowActuationPermit;
    readonly rollbackGeneration: string;
    readonly decisionDigest: string;
    readonly walletAddress: string;
    readonly walletUtxos?: readonly UTxO[];
    readonly reservationMode?: "create_or_resume" | "resume_only";
    readonly readWalletUtxos: () => Promise<readonly UTxO[]>;
    readonly resolveInputs: (
      outRefs: readonly string[],
    ) => Promise<readonly UTxO[]>;
    readonly resolveProtocolInputAuthority: (input: {
      readonly deploymentIdentity: VerifiedWatcherDeploymentIdentity;
      readonly outRef: string;
      readonly semanticRole: "protocol_state";
    }) => Promise<unknown>;
  }): Promise<WorkflowFundingReservationPermit>;
}>;

export const admittedFactories = new WeakSet<object>();

export const assertWatcherProverFundingAuthorityFactory = (
  factory: WatcherProverFundingAuthorityFactory,
): void => {
  if (!admittedFactories.has(factory)) {
    throw new Error("prover funding authority factory is not admitted");
  }
};

export const readRecord = async ({
  store,
  reservationId,
}: {
  readonly store: WatcherProverFundingReservationStore;
  readonly reservationId: string;
}): Promise<WatcherProverFundingReservationRecord> => {
  const matches = (await store.readAll())
    .map(parseWatcherProverFundingReservationRecord)
    .filter((record) => record.reservationId === reservationId);
  if (matches.length !== 1) {
    throw new Error("prover funding reservation store changed exact identity");
  }
  return matches[0]!;
};

export const snapshot = ({
  plan,
  record,
  rollbackGeneration,
}: {
  readonly plan: WatcherProverFundingReservationPlan;
  readonly record: WatcherProverFundingReservationRecord;
  readonly rollbackGeneration: string;
}) =>
  Object.freeze({
    reservationId: record.reservationId,
    deploymentFingerprint: record.deploymentFingerprint,
    decisionDigest: record.decisionDigest,
    policyDigest: record.policyDigest,
    reservationBasisDigest: record.reservationBasisDigest,
    rollbackGeneration,
    revision: record.revision,
    walletAddress: plan.walletAddress,
    fundingPaymentKeyHash: plan.fundingPaymentKeyHash,
    state: record.state,
    activeInputs: record.activeInputs,
  });
