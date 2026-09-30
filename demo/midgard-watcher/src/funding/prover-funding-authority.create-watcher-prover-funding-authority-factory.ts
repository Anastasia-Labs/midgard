import {
  assertWorkflowActuationPermitIdentity,
  createWorkflowRuntimeFundingPolicy,
  readWorkflowRuntimeFundingPolicy,
  type WorkflowActuationPermit,
} from "@al-ft/midgard-fault-proofs";
import {
  credentialToAddress,
  getAddressDetails,
  scriptHashToCredential,
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
import { createWatcherProverFundingAuthority } from "./prover-funding-authority.create-watcher-prover-funding-authority.js";
import {
  admittedFactories,
  WATCHER_PROVER_FUNDING_AUTHORITY,
  type WatcherProverFundingAuthorityFactory,
} from "./prover-funding-authority.watcher-prover-funding-authority-factory.js";
import type { WatcherRuntimeProverFundingCalculation } from "./prover-funding-calculation.js";
import { calculateWatcherRuntimeProverFunding } from "./prover-funding-calculation.js";
import {
  authorizeWatcherProverFundingRecovery,
  releaseUnusedWatcherProverFundingReservations,
} from "./prover-funding-recovery.js";
import {
  parseWatcherProverFundingReservationRecord,
  type WatcherProverFundingReservationRecord,
  type WatcherProverFundingReservationStore,
} from "./prover-funding-reservation.js";

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
