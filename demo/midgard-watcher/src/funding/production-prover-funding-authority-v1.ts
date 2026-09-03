import {
  createWorkflowFundingReservationPermit,
  type WorkflowActuationPermit,
  type WorkflowAdapterRunner,
  type WorkflowFundingRequirements,
  type WorkflowFundingReservationPermit,
  type WorkflowFundingReservationPort,
} from "@al-ft/midgard-fault-proofs";
import type { FraudProofCatalogueCategoryName } from "@al-ft/midgard-sdk";
import type { UTxO } from "@lucid-evolution/lucid";

import type { VerifiedWatcherDeploymentIdentity } from "../runtime/deployment-identity.js";
import type { WatcherProverFundingCalculation } from "./production-prover-funding-calculation-v1.js";
import { calculateWatcherProverFunding } from "./production-prover-funding-calculation-v1.js";
import {
  parseWatcherProverFundingReservationRecord,
  planWatcherProverFundingReservation,
  type WatcherProverFundingReservationPlan,
  type WatcherProverFundingReservationRecord,
  type WatcherProverFundingReservationStore,
} from "./production-prover-funding-reservation-v1.js";
import type { WatcherProtocolParameterRuntimeAuthority } from "./production-prover-funding-v1.js";

export const WATCHER_PROVER_FUNDING_AUTHORITY =
  "midgard-watcher-production-prover-funding-authority-v1" as const;

export type WatcherProverFundingAuthority = Readonly<{
  schemaVersion: typeof WATCHER_PROVER_FUNDING_AUTHORITY;
  plan: WatcherProverFundingReservationPlan;
  permit: WorkflowFundingReservationPermit;
}>;

export type WatcherProverFundingAuthorityFactory = Readonly<{
  schemaVersion: typeof WATCHER_PROVER_FUNDING_AUTHORITY;
  create(input: {
    readonly category: FraudProofCatalogueCategoryName;
    readonly runner: WorkflowAdapterRunner;
    readonly fundingRequirements: WorkflowFundingRequirements;
    readonly actuationPermit: WorkflowActuationPermit;
    readonly rollbackGeneration: string;
    readonly decisionDigest: string;
    readonly walletAddress: string;
    readonly walletUtxos: readonly UTxO[];
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

const admittedFactories = new WeakSet<object>();

export const assertWatcherProverFundingAuthorityFactory = (
  factory: WatcherProverFundingAuthorityFactory,
): void => {
  if (!admittedFactories.has(factory)) {
    throw new Error("prover funding authority factory is not admitted");
  }
};

const readRecord = async ({
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

const snapshot = ({
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
    profileDigest: record.profileDigest,
    calculationDigest: record.calculationDigest,
    rollbackGeneration,
    revision: record.revision,
    walletAddress: plan.walletAddress,
    fundingPaymentKeyHash: plan.fundingPaymentKeyHash,
    state: record.state,
    activeInputs: record.activeInputs,
  });

/**
 * Atomically reserves the exact measured wallet inputs, then mints the only
 * permit accepted by production runners. Operator config cannot provide a
 * reservation identity, revision, or body transition.
 */
export const createWatcherProverFundingAuthority = async (input: {
  readonly category: FraudProofCatalogueCategoryName;
  readonly runner: WorkflowAdapterRunner;
  readonly actuationPermit: WorkflowActuationPermit;
  readonly rollbackGeneration: string;
  readonly deploymentIdentity: VerifiedWatcherDeploymentIdentity;
  readonly calculation: WatcherProverFundingCalculation;
  readonly decisionDigest: string;
  readonly walletAddress: string;
  readonly walletUtxos: readonly UTxO[];
  readonly store: WatcherProverFundingReservationStore;
  readonly resolveInputs: (
    outRefs: readonly string[],
  ) => Promise<readonly UTxO[]>;
  readonly resolveProtocolInputAuthority: (input: {
    readonly deploymentIdentity: VerifiedWatcherDeploymentIdentity;
    readonly outRef: string;
    readonly semanticRole: "protocol_state";
  }) => Promise<unknown>;
}): Promise<WatcherProverFundingAuthority> => {
  const plan = planWatcherProverFundingReservation({
    deploymentIdentity: input.deploymentIdentity,
    calculation: input.calculation,
    decisionDigest: input.decisionDigest,
    walletAddress: input.walletAddress,
    utxos: input.walletUtxos,
  });
  await input.store.reserve(plan);

  const load = async () =>
    await readRecord({ store: input.store, reservationId: plan.reservationId });
  const port: WorkflowFundingReservationPort = Object.freeze({
    load: async () =>
      snapshot({
        plan,
        record: await load(),
        rollbackGeneration: input.rollbackGeneration,
      }),
    resolveInputs: async (outRefs: readonly string[]) =>
      await input.resolveInputs(outRefs),
    resolveConfirmedActionOutput: async ({
      sourceActionKind,
      sourceOutputIndex,
    }: Parameters<
      WorkflowFundingReservationPort["resolveConfirmedActionOutput"]
    >[0]) =>
      await input.store.readConfirmedActionOutput({
        reservationId: plan.reservationId,
        sourceActionKind,
        sourceOutputIndex,
      }),
    resolveProtocolInputAuthority: async ({
      deploymentFingerprint,
      outRef,
      semanticRole,
    }: Parameters<
      WorkflowFundingReservationPort["resolveProtocolInputAuthority"]
    >[0]) => {
      if (deploymentFingerprint !== plan.deploymentFingerprint) {
        throw new Error("prover funding protocol authority changed deployment");
      }
      return await input.resolveProtocolInputAuthority({
        deploymentIdentity: input.deploymentIdentity,
        outRef,
        semanticRole,
      });
    },
    prepare: async ({
      expectedRevision,
      transition,
    }: Parameters<WorkflowFundingReservationPort["prepare"]>[0]) => {
      const record = await input.store.prepareTransition({
        plan,
        expectedRevision,
        actionKind: transition.actionKind,
        signedTransactionCborHex: transition.signedTransactionCborHex,
        transactionHash: transition.transactionHash,
        transactionBodySha256: transition.transactionBodySha256,
        consumedOutRefs: transition.consumedOutRefs,
        producedInputs: transition.producedInputs,
      });
      return snapshot({
        plan,
        record,
        rollbackGeneration: input.rollbackGeneration,
      });
    },
    confirm: async ({
      expectedRevision,
      transactionHash,
    }: Parameters<WorkflowFundingReservationPort["confirm"]>[0]) => {
      const current = await load();
      const pending = current.pendingTransition;
      if (pending?.transactionHash !== transactionHash) {
        throw new Error("prover funding confirmation changed transaction hash");
      }
      return snapshot({
        plan,
        record: await input.store.confirmTransition({
          plan,
          expectedRevision,
          transitionDigest: pending.transitionDigest,
        }),
        rollbackGeneration: input.rollbackGeneration,
      });
    },
    abandon: async ({
      expectedRevision,
      transactionHash,
    }: Parameters<WorkflowFundingReservationPort["abandon"]>[0]) => {
      const current = await load();
      const pending = current.pendingTransition;
      if (pending?.transactionHash !== transactionHash) {
        throw new Error("prover funding abandonment changed transaction hash");
      }
      return snapshot({
        plan,
        record: await input.store.abandonPendingTransition({
          plan,
          expectedRevision,
          transitionDigest: pending.transitionDigest,
        }),
        rollbackGeneration: input.rollbackGeneration,
      });
    },
    markConflict: async ({
      expectedRevision,
      code,
    }: Parameters<WorkflowFundingReservationPort["markConflict"]>[0]) =>
      snapshot({
        plan,
        record: await input.store.markConflict({
          plan,
          expectedRevision,
          code,
        }),
        rollbackGeneration: input.rollbackGeneration,
      }),
    release: async ({
      expectedRevision,
    }: Parameters<WorkflowFundingReservationPort["release"]>[0]) =>
      snapshot({
        plan,
        record: await input.store.release({ plan, expectedRevision }),
        rollbackGeneration: input.rollbackGeneration,
      }),
  });
  const permit = await createWorkflowFundingReservationPermit({
    category: input.category,
    runner: input.runner,
    actuationPermit: input.actuationPermit,
    rollbackGeneration: input.rollbackGeneration,
    port,
  });
  return Object.freeze({
    schemaVersion: WATCHER_PROVER_FUNDING_AUTHORITY,
    plan,
    permit,
  });
};

/**
 * Runtime-owned funding authority. The application supplies only the fixed
 * category runner and its already-bound measured profile; the watcher cannot
 * choose or replace either identity.
 */
export const createWatcherProverFundingAuthorityFactory = (input: {
  readonly deploymentIdentity: VerifiedWatcherDeploymentIdentity;
  readonly protocolParameters: WatcherProtocolParameterRuntimeAuthority;
  readonly store: WatcherProverFundingReservationStore;
}): WatcherProverFundingAuthorityFactory => {
  const factory: WatcherProverFundingAuthorityFactory = Object.freeze({
    schemaVersion: WATCHER_PROVER_FUNDING_AUTHORITY,
    create: async (request) => {
      const calculation = await calculateWatcherProverFunding({
        deploymentIdentity: input.deploymentIdentity,
        protocolParameters: input.protocolParameters,
        requirements: request.fundingRequirements,
      });
      const authority = await createWatcherProverFundingAuthority({
        category: request.category,
        runner: request.runner,
        actuationPermit: request.actuationPermit,
        rollbackGeneration: request.rollbackGeneration,
        deploymentIdentity: input.deploymentIdentity,
        calculation,
        decisionDigest: request.decisionDigest,
        walletAddress: request.walletAddress,
        walletUtxos: request.walletUtxos,
        store: input.store,
        resolveInputs: request.resolveInputs,
        resolveProtocolInputAuthority: request.resolveProtocolInputAuthority,
      });
      return authority.permit;
    },
  });
  admittedFactories.add(factory);
  return factory;
};
