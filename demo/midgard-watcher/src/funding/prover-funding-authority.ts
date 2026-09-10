import {
  createWorkflowFundingReservationPermit,
  createWorkflowRuntimeFundingPolicy,
  type WorkflowActuationPermit,
  type WorkflowAdapterRunner,
  type WorkflowFundingReservationPermit,
  type WorkflowFundingReservationPort,
  type WorkflowRuntimeFundingPolicy,
} from "@al-ft/midgard-fault-proofs";
import type { FraudProofCatalogueCategoryName } from "@al-ft/midgard-sdk";
import {
  credentialToAddress,
  getAddressDetails,
  scriptHashToCredential,
  type UTxO,
} from "@lucid-evolution/lucid";

import {
  assertVerifiedWatcherDeploymentIdentity,
  type VerifiedWatcherDeploymentIdentity,
  watcherDeploymentAppliedScriptHashes,
  watcherDeploymentProtocolScriptAuthority,
  watcherDeploymentReleaseEconomicsAuthority,
} from "../runtime/deployment-identity.js";
import {
  assertWatcherProtocolParameterRuntimeAuthority,
  type WatcherProtocolParameterRuntimeAuthority,
} from "./prover-funding.js";
import type { WatcherRuntimeProverFundingCalculation } from "./prover-funding-calculation.js";
import { calculateWatcherRuntimeProverFunding } from "./prover-funding-calculation.js";
import {
  parseWatcherProverFundingReservationRecord,
  planWatcherProverFundingReservation,
  restoreWatcherProverFundingReservationPlan,
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
  create(input: {
    readonly category: FraudProofCatalogueCategoryName;
    readonly runner: WorkflowAdapterRunner;
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
    policyDigest: record.policyDigest,
    reservationBasisDigest: record.reservationBasisDigest,
    rollbackGeneration,
    revision: record.revision,
    walletAddress: plan.walletAddress,
    fundingPaymentKeyHash: plan.fundingPaymentKeyHash,
    state: record.state,
    activeInputs: record.activeInputs,
  });

/**
 * Atomically reserves the exact live wallet inputs, then mints the only
 * permit accepted by production runners. Operator config cannot provide a
 * reservation identity, revision, or body transition.
 */
export const createWatcherProverFundingAuthority = async (input: {
  readonly category: FraudProofCatalogueCategoryName;
  readonly runner: WorkflowAdapterRunner;
  readonly actuationPermit: WorkflowActuationPermit;
  readonly rollbackGeneration: string;
  readonly deploymentIdentity: VerifiedWatcherDeploymentIdentity;
  readonly calculation: WatcherRuntimeProverFundingCalculation;
  readonly policy: WorkflowRuntimeFundingPolicy;
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
  const records = (await input.store.readAll()).map(
    parseWatcherProverFundingReservationRecord,
  );
  const matching = records.filter(
    (record) =>
      record.deploymentFingerprint === input.deploymentIdentity.manifestId &&
      record.decisionDigest === input.decisionDigest,
  );
  if (matching.length > 1)
    throw new Error(
      "prover funding has multiple reservations for the same decision",
    );
  const existing = matching[0];
  const leasedElsewhere = new Set(
    records
      .filter((record) => record !== existing && record.state !== "released")
      .flatMap((record) => record.activeInputs.map(({ outRef }) => outRef)),
  );
  const plan =
    existing === undefined
      ? planWatcherProverFundingReservation({
          deploymentIdentity: input.deploymentIdentity,
          calculation: input.calculation,
          decisionDigest: input.decisionDigest,
          walletAddress: input.walletAddress,
          utxos: input.walletUtxos.filter(
            (utxo) =>
              !leasedElsewhere.has(`${utxo.txHash}#${utxo.outputIndex}`),
          ),
        })
      : restoreWatcherProverFundingReservationPlan({
          deploymentIdentity: input.deploymentIdentity,
          calculation: input.calculation,
          decisionDigest: input.decisionDigest,
          walletAddress: input.walletAddress,
          record: existing,
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
    resolveConfirmedInput: async ({
      outRef,
    }: Parameters<
      WorkflowFundingReservationPort["resolveConfirmedInput"]
    >[0]) =>
      await input.store.readConfirmedInput({
        reservationId: plan.reservationId,
        outRef,
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
    policy: input.policy,
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
 * Runtime-owned funding authority bound to the admitted runner, deployment,
 * current protocol parameters, and exact wallet leases.
 */
export const createWatcherProverFundingAuthorityFactory = (input: {
  readonly deploymentIdentity: VerifiedWatcherDeploymentIdentity;
  readonly protocolParameters: WatcherProtocolParameterRuntimeAuthority;
  readonly store: WatcherProverFundingReservationStore;
}): WatcherProverFundingAuthorityFactory => {
  assertVerifiedWatcherDeploymentIdentity(input.deploymentIdentity);
  assertWatcherProtocolParameterRuntimeAuthority(input.protocolParameters);
  const factory: WatcherProverFundingAuthorityFactory = Object.freeze({
    schemaVersion: WATCHER_PROVER_FUNDING_AUTHORITY,
    create: async (request) => {
      const credential = getAddressDetails(
        request.walletAddress,
      ).paymentCredential;
      if (credential?.type !== "Key")
        throw new Error("prover funding wallet has no payment key");
      const economics = await watcherDeploymentReleaseEconomicsAuthority(
        input.deploymentIdentity,
      ).verifyForWorkflow({
        deploymentFingerprint: input.deploymentIdentity.manifestId,
      });
      const contracts = Object.entries(
        watcherDeploymentAppliedScriptHashes(input.deploymentIdentity),
      ).flatMap(([name, scriptHash]) => {
        if (name.endsWith("Mint") || name.endsWith("Withdraw")) return [];
        const role =
          name === "correctionLockSpend"
            ? ("correction_lock" as const)
            : name === "fraudProofSpend" ||
                name === "fraudProofCatalogueSpend" ||
                name === "fieldPreimageCertificateSpend" ||
                name === "cekProgramMaterialSpend"
              ? ("field_carrier" as const)
              : (name.startsWith("fraudProof") &&
                    !name.startsWith("fraudProofCatalogue")) ||
                  name.startsWith("validationTraceDispute")
                ? ("proof_thread" as const)
                : name.endsWith("Spend")
                  ? ("protocol_state" as const)
                  : undefined;
        return role === undefined
          ? []
          : [
              {
                address: credentialToAddress(
                  input.deploymentIdentity.network,
                  scriptHashToCredential(scriptHash),
                ),
                scriptHash,
                role,
              },
            ];
      });
      const uniqueContracts = new Map<string, (typeof contracts)[number]>();
      for (const contract of contracts) {
        const prior = uniqueContracts.get(contract.address);
        if (prior !== undefined && prior.role !== contract.role) {
          throw new Error(
            `signed funding contract ${contract.scriptHash} has conflicting custody roles: ${prior.role}/${contract.role}`,
          );
        }
        uniqueContracts.set(contract.address, contract);
      }
      const referenceScripts = new Map<
        string,
        { outRef: string; scriptHash: string }
      >();
      for (const { outRef, scriptHash } of Object.values(
        watcherDeploymentProtocolScriptAuthority(input.deploymentIdentity)
          .referenceScripts,
      )) {
        const prior = referenceScripts.get(outRef);
        if (prior !== undefined && prior.scriptHash !== scriptHash)
          throw new Error(
            "signed funding reference has conflicting script identities",
          );
        referenceScripts.set(outRef, { outRef, scriptHash });
      }
      const policy = createWorkflowRuntimeFundingPolicy({
        category: request.category,
        runner: request.runner,
        deploymentFingerprint: input.deploymentIdentity.manifestId,
        fundingPaymentKeyHash: credential.hash,
        protocolParameters: input.protocolParameters.snapshot,
        economics,
        contracts: [...uniqueContracts.values()],
        referenceScripts: [...referenceScripts.values()],
      });
      const calculation = await calculateWatcherRuntimeProverFunding({
        deploymentIdentity: input.deploymentIdentity,
        protocolParameters: input.protocolParameters,
        policy,
      });
      const authority = await createWatcherProverFundingAuthority({
        category: request.category,
        runner: request.runner,
        actuationPermit: request.actuationPermit,
        rollbackGeneration: request.rollbackGeneration,
        deploymentIdentity: input.deploymentIdentity,
        calculation,
        policy,
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
