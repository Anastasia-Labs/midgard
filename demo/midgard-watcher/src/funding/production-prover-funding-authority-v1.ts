import {
  createProductionWorkflowFundingReservationPermitV1,
  type ProductionWorkflowActuationPermitV1,
  type ProductionWorkflowAdapterRunnerV1,
  type ProductionWorkflowFundingRequirementsV1,
  type ProductionWorkflowFundingReservationPermitV1,
  type ProductionWorkflowFundingReservationPortV1,
} from "@al-ft/midgard-fault-proofs";
import type { FraudProofCatalogueCategoryName } from "@al-ft/midgard-sdk";
import type { UTxO } from "@lucid-evolution/lucid";

import type { VerifiedWatcherDeploymentIdentityV1 } from "../runtime/deployment-identity.js";
import type { WatcherProductionProverFundingCalculationV1 } from "./production-prover-funding-calculation-v1.js";
import { calculateWatcherProductionProverFundingV1 } from "./production-prover-funding-calculation-v1.js";
import {
  parseWatcherProductionProverFundingReservationRecordV1,
  planWatcherProductionProverFundingReservationV1,
  type WatcherProductionProverFundingReservationPlanV1,
  type WatcherProductionProverFundingReservationRecordV1,
  type WatcherProductionProverFundingReservationStoreV1,
} from "./production-prover-funding-reservation-v1.js";
import type { WatcherProductionProtocolParameterRuntimeAuthorityV1 } from "./production-prover-funding-v1.js";

export const WATCHER_PRODUCTION_PROVER_FUNDING_AUTHORITY_V1 =
  "midgard-watcher-production-prover-funding-authority-v1" as const;

export type WatcherProductionProverFundingAuthorityV1 = Readonly<{
  schemaVersion: typeof WATCHER_PRODUCTION_PROVER_FUNDING_AUTHORITY_V1;
  plan: WatcherProductionProverFundingReservationPlanV1;
  permit: ProductionWorkflowFundingReservationPermitV1;
}>;

export type WatcherProductionProverFundingAuthorityFactoryV1 = Readonly<{
  schemaVersion: typeof WATCHER_PRODUCTION_PROVER_FUNDING_AUTHORITY_V1;
  create(input: {
    readonly category: FraudProofCatalogueCategoryName;
    readonly runner: ProductionWorkflowAdapterRunnerV1;
    readonly fundingRequirements: ProductionWorkflowFundingRequirementsV1;
    readonly actuationPermit: ProductionWorkflowActuationPermitV1;
    readonly rollbackGeneration: string;
    readonly decisionDigest: string;
    readonly walletAddress: string;
    readonly walletUtxos: readonly UTxO[];
    readonly resolveInputs: (
      outRefs: readonly string[],
    ) => Promise<readonly UTxO[]>;
    readonly resolveProtocolInputAuthority: (input: {
      readonly deploymentIdentity: VerifiedWatcherDeploymentIdentityV1;
      readonly outRef: string;
      readonly semanticRole: "protocol_state";
    }) => Promise<unknown>;
  }): Promise<ProductionWorkflowFundingReservationPermitV1>;
}>;

const admittedFactories = new WeakSet<object>();

export const assertWatcherProductionProverFundingAuthorityFactoryV1 = (
  factory: WatcherProductionProverFundingAuthorityFactoryV1,
): void => {
  if (!admittedFactories.has(factory)) {
    throw new Error("prover funding authority factory is not admitted");
  }
};

const readRecord = async ({
  store,
  reservationId,
}: {
  readonly store: WatcherProductionProverFundingReservationStoreV1;
  readonly reservationId: string;
}): Promise<WatcherProductionProverFundingReservationRecordV1> => {
  const matches = (await store.readAll())
    .map(parseWatcherProductionProverFundingReservationRecordV1)
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
  readonly plan: WatcherProductionProverFundingReservationPlanV1;
  readonly record: WatcherProductionProverFundingReservationRecordV1;
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
export const createWatcherProductionProverFundingAuthorityV1 = async (input: {
  readonly category: FraudProofCatalogueCategoryName;
  readonly runner: ProductionWorkflowAdapterRunnerV1;
  readonly actuationPermit: ProductionWorkflowActuationPermitV1;
  readonly rollbackGeneration: string;
  readonly deploymentIdentity: VerifiedWatcherDeploymentIdentityV1;
  readonly calculation: WatcherProductionProverFundingCalculationV1;
  readonly decisionDigest: string;
  readonly walletAddress: string;
  readonly walletUtxos: readonly UTxO[];
  readonly store: WatcherProductionProverFundingReservationStoreV1;
  readonly resolveInputs: (
    outRefs: readonly string[],
  ) => Promise<readonly UTxO[]>;
  readonly resolveProtocolInputAuthority: (input: {
    readonly deploymentIdentity: VerifiedWatcherDeploymentIdentityV1;
    readonly outRef: string;
    readonly semanticRole: "protocol_state";
  }) => Promise<unknown>;
}): Promise<WatcherProductionProverFundingAuthorityV1> => {
  const plan = planWatcherProductionProverFundingReservationV1({
    deploymentIdentity: input.deploymentIdentity,
    calculation: input.calculation,
    decisionDigest: input.decisionDigest,
    walletAddress: input.walletAddress,
    utxos: input.walletUtxos,
  });
  await input.store.reserve(plan);

  const load = async () =>
    await readRecord({ store: input.store, reservationId: plan.reservationId });
  const port: ProductionWorkflowFundingReservationPortV1 = Object.freeze({
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
      ProductionWorkflowFundingReservationPortV1["resolveConfirmedActionOutput"]
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
      ProductionWorkflowFundingReservationPortV1["resolveProtocolInputAuthority"]
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
    }: Parameters<
      ProductionWorkflowFundingReservationPortV1["prepare"]
    >[0]) => {
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
    }: Parameters<
      ProductionWorkflowFundingReservationPortV1["confirm"]
    >[0]) => {
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
    }: Parameters<
      ProductionWorkflowFundingReservationPortV1["abandon"]
    >[0]) => {
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
    }: Parameters<
      ProductionWorkflowFundingReservationPortV1["markConflict"]
    >[0]) =>
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
    }: Parameters<ProductionWorkflowFundingReservationPortV1["release"]>[0]) =>
      snapshot({
        plan,
        record: await input.store.release({ plan, expectedRevision }),
        rollbackGeneration: input.rollbackGeneration,
      }),
  });
  const permit = await createProductionWorkflowFundingReservationPermitV1({
    category: input.category,
    runner: input.runner,
    actuationPermit: input.actuationPermit,
    rollbackGeneration: input.rollbackGeneration,
    port,
  });
  return Object.freeze({
    schemaVersion: WATCHER_PRODUCTION_PROVER_FUNDING_AUTHORITY_V1,
    plan,
    permit,
  });
};

/**
 * Runtime-owned funding authority. The application supplies only the fixed
 * category runner and its already-bound measured profile; the watcher cannot
 * choose or replace either identity.
 */
export const createWatcherProductionProverFundingAuthorityFactoryV1 = (input: {
  readonly deploymentIdentity: VerifiedWatcherDeploymentIdentityV1;
  readonly protocolParameters: WatcherProductionProtocolParameterRuntimeAuthorityV1;
  readonly store: WatcherProductionProverFundingReservationStoreV1;
}): WatcherProductionProverFundingAuthorityFactoryV1 => {
  const factory: WatcherProductionProverFundingAuthorityFactoryV1 =
    Object.freeze({
      schemaVersion: WATCHER_PRODUCTION_PROVER_FUNDING_AUTHORITY_V1,
      create: async (request) => {
        const calculation = await calculateWatcherProductionProverFundingV1({
          deploymentIdentity: input.deploymentIdentity,
          protocolParameters: input.protocolParameters,
          requirements: request.fundingRequirements,
        });
        const authority = await createWatcherProductionProverFundingAuthorityV1(
          {
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
            resolveProtocolInputAuthority:
              request.resolveProtocolInputAuthority,
          },
        );
        return authority.permit;
      },
    });
  admittedFactories.add(factory);
  return factory;
};
