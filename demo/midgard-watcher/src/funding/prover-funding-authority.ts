import { computeDeploymentManifestJsonDigest } from "@al-ft/midgard-core/deployment-manifest-identity";
import {
  assertWorkflowActuationPermitIdentity,
  createWorkflowFundingReservationPermit,
  createWorkflowRuntimeFundingPolicy,
  parseWorkflowFundingPreparedTransition,
  readWorkflowRuntimeFundingPolicy,
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

import type { WatcherInstalledWorkflowCategory } from "../fault-proofs/fault-proof-application.js";
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
  authorizeWatcherProverFundingRecovery,
  releaseUnusedWatcherProverFundingReservations,
} from "./prover-funding-recovery.js";
import {
  parseWatcherProverFundingReservationRecord,
  planWatcherProverFundingReservation,
  restoreWatcherProverFundingReservationPlan,
  type WatcherProverFundingReservationPlan,
  type WatcherProverFundingReservationRecord,
  type WatcherProverFundingReservationStore,
  WatcherProverFundingUnavailableError,
} from "./prover-funding-reservation.js";
import { isWatcherProverFundingReservationConflict } from "./sqlite-prover-funding-reservation-store.js";

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
  readonly reservationPolicy?: WorkflowRuntimeFundingPolicy;
  readonly decisionDigest: string;
  readonly walletAddress: string;
  readonly walletUtxos: readonly UTxO[];
  readonly readWalletUtxos: () => Promise<readonly UTxO[]>;
  readonly store: WatcherProverFundingReservationStore;
  readonly onReserved?: (record: WatcherProverFundingReservationRecord) => void;
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
  const authority = assertWorkflowActuationPermitIdentity({
    permit: input.actuationPermit,
    category: input.category,
    rollbackGeneration: input.rollbackGeneration,
  });
  if (authority.authority === "reconciliation" && existing === undefined)
    throw new Error(
      "reconciliation funding requires its existing durable reservation",
    );
  const leasedElsewhere = new Set(
    records
      .filter((record) => record !== existing && record.state !== "released")
      .flatMap((record) =>
        [
          ...record.activeInputs,
          ...(record.pendingTransition?.producedInputs ?? []),
        ].map(({ outRef }) => outRef),
      ),
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
  // Released reservations cannot spend; authenticated reobservation can reopen
  // provisional completion before its terminal anchor.
  if (existing?.state !== "released") await input.store.reserve(plan);

  const load = async () =>
    await readRecord({ store: input.store, reservationId: plan.reservationId });
  input.onReserved?.(await load());
  const port: WorkflowFundingReservationPort = Object.freeze({
    reobserve: async ({
      expectedRevision,
      transactionHash,
    }: Parameters<
      NonNullable<WorkflowFundingReservationPort["reobserve"]>
    >[0]) => {
      if (
        input.store.readReobservationInputs === undefined ||
        input.store.reobserveTransition === undefined
      )
        throw new Error(
          "prover funding store cannot reobserve signed attempts",
        );
      const candidates = await input.store.readReobservationInputs({
        reservationId: plan.reservationId,
        transactionHash,
      });
      const roles = new Map(
        candidates.map(({ outRef, role }) => [outRef, role]),
      );
      const resolved = await input.resolveInputs(
        candidates.map(({ outRef }) => outRef),
      );
      const inputs = resolved.map((utxo) => {
        const outRef = `${utxo.txHash}#${utxo.outputIndex}`;
        const role = roles.get(outRef);
        if (role === undefined || utxo.address !== plan.walletAddress)
          throw new Error(
            "reobserved funding source substituted a wallet input",
          );
        return {
          outRef,
          role,
          lovelace: (utxo.assets.lovelace ?? 0n).toString(),
          assets: Object.entries(utxo.assets)
            .filter(([unit]) => unit !== "lovelace")
            .map(([unit, quantity]) => ({
              unit,
              quantity: quantity.toString(),
            }))
            .sort((a, b) => a.unit.localeCompare(b.unit)),
        };
      });
      // A chain query is asynchronous: reject a revoked generation before the
      // durable reservation changes, just as the submission path does.
      assertWorkflowActuationPermitIdentity({
        permit: input.actuationPermit,
        category: input.category,
        rollbackGeneration: input.rollbackGeneration,
      });
      try {
        return snapshot({
          plan,
          record: await input.store.reobserveTransition({
            plan,
            expectedRevision,
            transactionHash,
            inputs,
          }),
          rollbackGeneration: input.rollbackGeneration,
        });
      } catch (error) {
        if (isWatcherProverFundingReservationConflict(error)) return null;
        throw error;
      }
    },
    readAbandonmentHandoff: async () =>
      await input.store.readAbandonmentHandoff({
        reservationId: plan.reservationId,
      }),
    releaseIdle: async ({ expectedRevision }: { expectedRevision: string }) => {
      assertWorkflowActuationPermitIdentity({
        permit: input.actuationPermit,
        category: input.category,
        rollbackGeneration: input.rollbackGeneration,
      });
      if (input.store.releaseIdle === undefined)
        throw new Error("prover funding cannot release idle inputs");
      return snapshot({
        plan,
        record: await input.store.releaseIdle({ plan, expectedRevision }),
        rollbackGeneration: input.rollbackGeneration,
      });
    },
    acknowledgeAbandonment: async ({
      expectedRevision,
      handoff,
    }: Parameters<
      WorkflowFundingReservationPort["acknowledgeAbandonment"]
    >[0]) =>
      snapshot({
        plan,
        record: await input.store.acknowledgeAbandonment({
          plan,
          expectedRevision,
          handoff,
        }),
        rollbackGeneration: input.rollbackGeneration,
      }),
    readPendingHandoff: async () =>
      await input.store.readPendingHandoff({
        reservationId: plan.reservationId,
      }),
    readPendingTransition: async () =>
      await input.store.readPendingTransition({
        reservationId: plan.reservationId,
      }),
    readCompletionHandoff: async () =>
      await input.store.readCompletionHandoff({
        reservationId: plan.reservationId,
      }),
    load: async () =>
      snapshot({
        plan,
        record: await load(),
        rollbackGeneration: input.rollbackGeneration,
      }),
    refreshIdle: async ({
      expectedRevision,
      releaseStaleInputs,
    }: {
      expectedRevision: string;
      releaseStaleInputs: boolean;
    }) => {
      const assertSubmission = () => {
        const current = assertWorkflowActuationPermitIdentity({
          permit: input.actuationPermit,
          category: input.category,
          rollbackGeneration: input.rollbackGeneration,
        });
        if (current.authority === "reconciliation")
          throw new Error("reconciliation-only funding cannot refresh inputs");
      };
      assertSubmission();
      let current = await load();
      if (current.revision !== expectedRevision)
        throw new Error("prover funding refresh revision changed");
      if (
        current.state !== "active" ||
        current.pendingTransition !== null ||
        (await input.store.readAbandonmentHandoff({
          reservationId: plan.reservationId,
        })) !== null
      )
        throw new Error(
          "prover funding cannot refresh an unresolved reservation",
        );
      if (current.activeInputs.length !== 0) {
        assertSubmission();
        if (releaseStaleInputs && input.store.releaseIdle !== undefined)
          await input.store.releaseIdle({
            plan,
            expectedRevision: current.revision,
          });
        else if (
          input.store.releaseUnused === undefined ||
          !(await input.store.releaseUnused(current))
        )
          return null;
        current = await load();
      }
      const walletUtxos = await input.readWalletUtxos();
      if (walletUtxos.some((utxo) => utxo.address !== plan.walletAddress))
        throw new Error("refreshed funding wallet returned a foreign address");
      const others = (await input.store.readAll()).map(
        parseWatcherProverFundingReservationRecord,
      );
      const leased = new Set(
        others
          .filter(
            (record) =>
              record.reservationId !== plan.reservationId &&
              record.state !== "released",
          )
          .flatMap((record) =>
            [
              ...record.activeInputs,
              ...(record.pendingTransition?.producedInputs ?? []),
            ].map(({ outRef }) => outRef),
          ),
      );
      try {
        const refreshedPlan = planWatcherProverFundingReservation({
          deploymentIdentity: input.deploymentIdentity,
          calculation: input.calculation,
          decisionDigest: input.decisionDigest,
          walletAddress: input.walletAddress,
          utxos: walletUtxos.filter(
            ({ txHash, outputIndex }) =>
              !leased.has(`${txHash}#${outputIndex}`),
          ),
        });
        assertSubmission();
        await input.store.reserve(refreshedPlan, current.revision);
      } catch (error) {
        if (
          error instanceof WatcherProverFundingUnavailableError ||
          isWatcherProverFundingReservationConflict(error)
        )
          return null;
        throw error;
      }
      assertSubmission();
      return snapshot({
        plan,
        record: await load(),
        rollbackGeneration: input.rollbackGeneration,
      });
    },
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
      handoff,
    }: Parameters<WorkflowFundingReservationPort["prepare"]>[0]) => {
      const record = await input.store.prepareTransition({
        plan,
        handoff,
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
      const transitionDigest =
        current.pendingTransition?.transitionDigest ??
        current.lastConfirmedTransitionDigest;
      if (transitionDigest === null) {
        throw new Error(
          "prover funding confirmation has no recorded transition",
        );
      }
      return snapshot({
        plan,
        record: await input.store.confirmTransition({
          plan,
          expectedRevision,
          transactionHash,
          transitionDigest,
        }),
        rollbackGeneration: input.rollbackGeneration,
      });
    },
    abandon: async ({
      expectedRevision,
      transactionHash,
      handoff,
    }: Parameters<WorkflowFundingReservationPort["abandon"]>[0]) => {
      const current = await load();
      let transitionDigest: string;
      if (current.pendingTransition !== null) {
        if (current.pendingTransition.transactionHash !== transactionHash)
          throw new Error(
            "prover funding abandonment changed transaction hash",
          );
        transitionDigest = current.pendingTransition.transitionDigest;
      } else {
        const saved = await input.store.readAbandonmentHandoff({
          reservationId: plan.reservationId,
        });
        if (
          saved === null ||
          typeof saved !== "object" ||
          Array.isArray(saved) ||
          !("transition" in saved)
        )
          throw new Error(
            "prover funding abandonment lacks its durable signed transaction",
          );
        const transition = parseWorkflowFundingPreparedTransition(
          saved.transition,
        );
        if (transition.transactionHash !== transactionHash)
          throw new Error(
            "prover funding abandonment changed transaction hash",
          );
        transitionDigest = computeDeploymentManifestJsonDigest(transition);
      }
      return snapshot({
        plan,
        record: await input.store.abandonPendingTransition({
          plan,
          expectedRevision,
          transitionDigest,
          handoff,
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
      handoff,
    }: Parameters<WorkflowFundingReservationPort["release"]>[0]) =>
      snapshot({
        plan,
        record: await input.store.release({ plan, expectedRevision, handoff }),
        rollbackGeneration: input.rollbackGeneration,
      }),
  });
  const permit = await createWorkflowFundingReservationPermit({
    category: input.category,
    runner: input.runner,
    policy: input.policy,
    reservationPolicy: input.reservationPolicy,
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
  readonly journalRoot: string;
  readonly launchScope: readonly WatcherInstalledWorkflowCategory[];
  readonly deploymentIdentity: VerifiedWatcherDeploymentIdentity;
  readonly protocolParameters: WatcherProtocolParameterRuntimeAuthority;
  readonly store: WatcherProverFundingReservationStore;
}): WatcherProverFundingAuthorityFactory => {
  assertVerifiedWatcherDeploymentIdentity(input.deploymentIdentity);
  assertWatcherProtocolParameterRuntimeAuthority(input.protocolParameters);
  // One immutable policy calculation, never an authority, lease or wallet view.
  // A changed admitted policy/parameter/economics basis replaces this entry.
  let lastCalculation: WatcherRuntimeProverFundingCalculation | undefined;
  const reservationByPermit = new WeakMap<
    WorkflowActuationPermit,
    WatcherProverFundingReservationRecord
  >();
  const factory: WatcherProverFundingAuthorityFactory = Object.freeze({
    schemaVersion: WATCHER_PROVER_FUNDING_AUTHORITY,
    releaseUnused: async (scope) => {
      const reservation =
        scope === undefined
          ? undefined
          : reservationByPermit.get(scope.actuationPermit);
      if (scope !== undefined && reservation === undefined) return;
      await releaseUnusedWatcherProverFundingReservations({
        ...input,
        reservation,
      });
    },
    create: async (request) => {
      assertVerifiedWatcherDeploymentIdentity(input.deploymentIdentity);
      assertWatcherProtocolParameterRuntimeAuthority(input.protocolParameters);
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
      const priorAddresses = new Set<string>();
      const contracts = Object.entries(
        watcherDeploymentAppliedScriptHashes(input.deploymentIdentity),
      ).flatMap(([name, scriptHash]) => {
        // These deployed names describe proof subjects, not script purposes:
        // both are spending validators despite their Mint/Withdraw suffixes.
        const namedProofSpend =
          name === "fraudProofValueNotPreservedUnionMint" ||
          name === "fraudProofDoubleWithdraw";
        if (
          !namedProofSpend &&
          (name.endsWith("Mint") || name.endsWith("Withdraw"))
        )
          return [];
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
        if (role === undefined) return [];
        const address = credentialToAddress(
          input.deploymentIdentity.network,
          scriptHashToCredential(scriptHash),
        );
        if (!name.endsWith("Mint") && !name.endsWith("Withdraw"))
          priorAddresses.add(address);
        return [{ address, scriptHash, role }];
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
      const policyInput = {
        category: request.category,
        runner: request.runner,
        deploymentFingerprint: input.deploymentIdentity.manifestId,
        fundingPaymentKeyHash: credential.hash,
        protocolParameters: input.protocolParameters.snapshot,
        economics,
        contracts: [...uniqueContracts.values()],
        referenceScripts: [...referenceScripts.values()],
      };
      const policy = createWorkflowRuntimeFundingPolicy(policyInput);
      let reservationPolicy = policy;
      await authorizeWatcherProverFundingRecovery({
        journalRoot: input.journalRoot,
        deploymentIdentity: input.deploymentIdentity,
        actuationPermit: request.actuationPermit,
        category: request.category,
        rollbackGeneration: request.rollbackGeneration,
        store: input.store,
      });
      const execution = assertWorkflowActuationPermitIdentity({
        permit: request.actuationPermit,
        category: request.category,
        rollbackGeneration: request.rollbackGeneration,
      });
      if (execution.decisionDigest !== request.decisionDigest)
        throw new Error(
          "prover funding invocation changed its authorizing decision",
        );
      const existing = (await input.store.readAll())
        .map(parseWatcherProverFundingReservationRecord)
        .find(
          (record) =>
            record.deploymentFingerprint ===
              input.deploymentIdentity.manifestId &&
            record.decisionDigest === execution.executionDecisionDigest,
        );
      if (request.reservationMode === "resume_only" && existing === undefined)
        throw new Error(
          "existing-only funding requires its durable reservation",
        );
      if (
        existing !== undefined &&
        existing.policyDigest !==
          readWorkflowRuntimeFundingPolicy(policy).policyDigest
      ) {
        // Reconstruct the exact previously deployed selector. Preserve aliases
        // already admitted by that selector, and never infer authority from a
        // persisted digest alone. All other identity mismatches remain errors.
        const priorPolicy = createWorkflowRuntimeFundingPolicy({
          ...policyInput,
          contracts: policyInput.contracts.filter(({ address }) =>
            priorAddresses.has(address),
          ),
        });
        if (
          existing.policyDigest !==
          readWorkflowRuntimeFundingPolicy(priorPolicy).policyDigest
        )
          throw new Error(
            "restored prover funding reservation identity mismatch",
          );
        reservationPolicy = priorPolicy;
      }
      const selectedPolicy =
        readWorkflowRuntimeFundingPolicy(reservationPolicy);
      const calculation =
        lastCalculation?.policyDigest === selectedPolicy.policyDigest &&
        lastCalculation.protocolParametersDigest ===
          input.protocolParameters.snapshotDigest &&
        lastCalculation.economicsPolicyDigest === economics.policyDigest
          ? lastCalculation
          : await calculateWatcherRuntimeProverFunding({
              deploymentIdentity: input.deploymentIdentity,
              protocolParameters: input.protocolParameters,
              policy: reservationPolicy,
            });
      // Original durable leases remain authoritative on every generation,
      // including cache hits and selection of the earlier deployed roster.
      if (
        existing !== undefined &&
        (existing.policyDigest !== calculation.policyDigest ||
          existing.reservationBasisDigest !==
            calculation.reservationBasisDigest)
      )
        throw new Error(
          "restored prover funding reservation identity mismatch",
        );
      lastCalculation = calculation;
      const authority = await createWatcherProverFundingAuthority({
        category: request.category,
        runner: request.runner,
        actuationPermit: request.actuationPermit,
        rollbackGeneration: request.rollbackGeneration,
        deploymentIdentity: input.deploymentIdentity,
        calculation,
        policy,
        reservationPolicy,
        decisionDigest: execution.executionDecisionDigest,
        onReserved: (record) =>
          reservationByPermit.set(request.actuationPermit, record),
        walletAddress: request.walletAddress,
        walletUtxos:
          existing === undefined
            ? (request.walletUtxos ?? (await request.readWalletUtxos()))
            : [],
        readWalletUtxos: request.readWalletUtxos,
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
