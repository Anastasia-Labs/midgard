import { computeDeploymentManifestJsonDigest } from "@al-ft/midgard-core/deployment-manifest-identity";
import {
  assertWorkflowActuationPermitIdentity,
  createWorkflowFundingReservationPermit,
  parseWorkflowFundingPreparedTransition,
  type WorkflowActuationPermit,
  type WorkflowAdapterRunner,
  type WorkflowFundingReservationPort,
  type WorkflowRuntimeFundingPolicy,
} from "@al-ft/midgard-fault-proofs";
import type { FraudProofCatalogueCategoryName } from "@al-ft/midgard-sdk";
import { type UTxO } from "@lucid-evolution/lucid";

import { type VerifiedWatcherDeploymentIdentity } from "../runtime/deployment-identity.js";
import { readProverFundingSubmissionAuthority } from "./prover-funding-authority.reconciliation-only.js";
import {
  readRecord,
  snapshot,
  WATCHER_PROVER_FUNDING_AUTHORITY,
  type WatcherProverFundingAuthority,
} from "./prover-funding-authority.watcher-prover-funding-authority-factory.js";
import type { WatcherRuntimeProverFundingCalculation } from "./prover-funding-calculation.js";
import {
  parseWatcherProverFundingReservationRecord,
  planWatcherProverFundingReservation,
  restoreWatcherProverFundingReservationPlan,
  type WatcherProverFundingReservationRecord,
  type WatcherProverFundingReservationStore,
  WatcherProverFundingUnavailableError,
} from "./prover-funding-reservation.js";
import { isWatcherProverFundingReservationConflict } from "./sqlite-prover-funding-reservation-store.js";

export const createWatcherProverFundingAuthority = async (input: {
  readonly category: FraudProofCatalogueCategoryName;
  readonly runner: WorkflowAdapterRunner;
  readonly actuationPermit: WorkflowActuationPermit;
  readonly rollbackGeneration: string;
  readonly deploymentIdentity: VerifiedWatcherDeploymentIdentity;
  readonly calculation: WatcherRuntimeProverFundingCalculation;
  readonly selectionCalculation?: WatcherRuntimeProverFundingCalculation;
  readonly policy: WorkflowRuntimeFundingPolicy;
  readonly reservationPolicy?: WorkflowRuntimeFundingPolicy;
  readonly capacityPolicy?: WorkflowRuntimeFundingPolicy;
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
    await input.store.readReservedOutRefs({
      excludingReservationId: existing?.reservationId,
    }),
  );
  const plan =
    existing === undefined
      ? planWatcherProverFundingReservation({
          deploymentIdentity: input.deploymentIdentity,
          calculation: input.calculation,
          selectionCalculation: input.selectionCalculation,
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
  const { reconciliationOnly, assertSubmissionAuthority } =
    await readProverFundingSubmissionAuthority(
      input.store,
      plan.reservationId,
      existing !== undefined,
    );
  // Released reservations cannot spend; authenticated reobservation can reopen
  // provisional completion before its terminal anchor.
  if (existing?.state !== "released" && !reconciliationOnly)
    await input.store.reserve(plan);

  const load = async () =>
    await readRecord({ store: input.store, reservationId: plan.reservationId });
  input.onReserved?.(await load());
  const port: WorkflowFundingReservationPort = Object.freeze({
    assertSubmissionAuthority,
    reobserve: async ({
      expectedRevision,
      transactionHash,
      adoption,
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
      // Collateral never blocks a later action: collateral another reservation
      // now holds is not reclaimed. The exact bytes still land while it is
      // unspent; once it is spent the attempt is invalidated and re-signed.
      const elsewhere = new Set(
        await input.store.readReservedOutRefs({
          excludingReservationId: plan.reservationId,
        }),
      );
      const candidates = (
        await input.store.readReobservationInputs({
          reservationId: plan.reservationId,
          transactionHash,
        })
      ).filter(
        ({ outRef, role }) => role !== "collateral" || !elsewhere.has(outRef),
      );
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
            ...(adoption === undefined ? {} : { adoption }),
          }),
          rollbackGeneration: input.rollbackGeneration,
        });
      } catch (error) {
        if (isWatcherProverFundingReservationConflict(error)) return null;
        throw error;
      }
    },
    readSupersededExclusionOutRefs: async () =>
      (await input.store.readSupersededExclusionOutRefs?.({
        reservationId: plan.reservationId,
      })) ?? null,
    retireLegacyAbandonment: async ({
      expectedRevision,
      transactionHash,
      retirement,
    }: Parameters<
      NonNullable<WorkflowFundingReservationPort["retireLegacyAbandonment"]>
    >[0]) => {
      if (input.store.retireLegacyAbandonment === undefined)
        throw new Error("Funding store cannot authenticate legacy retirement");
      assertWorkflowActuationPermitIdentity({
        permit: input.actuationPermit,
        category: input.category,
        rollbackGeneration: input.rollbackGeneration,
      });
      return snapshot({
        plan,
        record: await input.store.retireLegacyAbandonment({
          plan,
          expectedRevision,
          transactionHash,
          retirement,
        }),
        rollbackGeneration: input.rollbackGeneration,
      });
    },
    readLegacyAbandonedTransactions: async () =>
      (await input.store.readLegacyAbandonedTransactions?.({
        reservationId: plan.reservationId,
      })) ?? [],
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
      const leased = new Set(
        await input.store.readReservedOutRefs({
          excludingReservationId: plan.reservationId,
        }),
      );
      try {
        const refreshedPlan = planWatcherProverFundingReservation({
          deploymentIdentity: input.deploymentIdentity,
          calculation: input.calculation,
          selectionCalculation: input.selectionCalculation,
          decisionDigest: input.decisionDigest,
          walletAddress: input.walletAddress,
          utxos: walletUtxos.filter(
            ({ txHash, outputIndex }) =>
              !leased.has(`${txHash}#${outputIndex}`),
          ),
          // A replacement spends a superseded attempt's input as funding.
          avoidCollateralOutRefs:
            (await input.store.readSupersededExclusionOutRefs?.({
              reservationId: plan.reservationId,
            })) ?? [],
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
    capacityPolicy: input.capacityPolicy,
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
