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
  assertWatcherProtocolParameterHistory,
  assertWatcherProtocolParameterRuntimeAuthority,
  refreshWatcherProtocolParameterRuntimeAuthority,
  type WatcherProtocolParameterHistory,
  type WatcherProtocolParameterRuntimeAuthority,
  watcherSignedDeploymentProtocolParameterRecoveryAuthority,
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
  WatcherProverFundingUnavailableError,
} from "./prover-funding-reservation.js";

/**
 * Runtime-owned funding authority bound to the admitted runner, deployment,
 * current protocol parameters, and exact wallet leases.
 */
export const createWatcherProverFundingAuthorityFactory = (input: {
  readonly journalRoot: string;
  /** The key the fault-proof journals' rows are authenticated with. */
  readonly journalAuthenticationKey: Uint8Array;
  readonly launchScope: readonly WatcherInstalledWorkflowCategory[];
  readonly deploymentIdentity: VerifiedWatcherDeploymentIdentity;
  readonly protocolParameters: WatcherProtocolParameterRuntimeAuthority;
  readonly store: WatcherProverFundingReservationStore;
  readonly protocolParameterHistory?: WatcherProtocolParameterHistory;
}): WatcherProverFundingAuthorityFactory => {
  assertVerifiedWatcherDeploymentIdentity(input.deploymentIdentity);
  assertWatcherProtocolParameterRuntimeAuthority(input.protocolParameters);
  if (input.protocolParameterHistory !== undefined)
    assertWatcherProtocolParameterHistory(input.protocolParameterHistory);
  // One immutable policy calculation, never an authority, lease or wallet view.
  // A changed admitted policy/parameter/economics basis replaces this entry.
  const signedParameters =
    watcherSignedDeploymentProtocolParameterRecoveryAuthority(
      input.deploymentIdentity,
    );
  let lastCalculation: WatcherRuntimeProverFundingCalculation | undefined;
  let lastSelectionCalculation:
    | WatcherRuntimeProverFundingCalculation
    | undefined;
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
      let protocolParameters =
        await refreshWatcherProtocolParameterRuntimeAuthority(
          input.protocolParameters,
        );
      const currentParameters = protocolParameters;
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
                name === "cekProgramMaterialSpend" ||
                // Append-only proof-item publication uses the same immutable
                // spending validator as the fraud-proof certificate carriers.
                name === "validationTraceDisputeProofItem"
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
        protocolParameters: protocolParameters.snapshot,
        economics,
        contracts: [...uniqueContracts.values()],
        referenceScripts: [...referenceScripts.values()],
      };
      const policy = createWorkflowRuntimeFundingPolicy(policyInput);
      let reservationPolicy = policy;
      await authorizeWatcherProverFundingRecovery({
        journalRoot: input.journalRoot,
        journalAuthenticationKey: input.journalAuthenticationKey,
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
        // Recover only an actually admitted historical snapshot, never a digest
        // supplied by the durable record. Signed attempts retain their leases.
        let recovered = false;
        for (const historical of [
          input.protocolParameterHistory?.read(existing),
          signedParameters,
          input.protocolParameters,
        ]) {
          if (historical == null) continue;
          const historicalInput = {
            ...policyInput,
            protocolParameters: historical.snapshot,
          };
          const historicalPolicy =
            createWorkflowRuntimeFundingPolicy(historicalInput);
          const priorPolicy = createWorkflowRuntimeFundingPolicy({
            ...historicalInput,
            contracts: policyInput.contracts.filter(({ address }) =>
              priorAddresses.has(address),
            ),
          });
          for (const candidate of [historicalPolicy, priorPolicy]) {
            if (
              existing.policyDigest !==
              readWorkflowRuntimeFundingPolicy(candidate).policyDigest
            )
              continue;
            protocolParameters = historical;
            reservationPolicy = candidate;
            recovered = true;
            break;
          }
          if (recovered) break;
        }
        if (!recovered) {
          throw new WatcherProverFundingUnavailableError(
            "restored prover funding reservation identity mismatch: original protocol parameters are unavailable; signed attempts must reconcile before repricing",
          );
        }
      }
      const selectedPolicy =
        readWorkflowRuntimeFundingPolicy(reservationPolicy);
      const calculation =
        lastCalculation?.policyDigest === selectedPolicy.policyDigest &&
        lastCalculation.protocolParametersDigest ===
          protocolParameters.snapshotDigest &&
        lastCalculation.economicsPolicyDigest === economics.policyDigest
          ? lastCalculation
          : await calculateWatcherRuntimeProverFunding({
              deploymentIdentity: input.deploymentIdentity,
              protocolParameters,
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
      const currentPolicy = readWorkflowRuntimeFundingPolicy(policy);
      const selectionCalculation =
        currentParameters.snapshotDigest ===
        calculation.protocolParametersDigest
          ? calculation
          : lastSelectionCalculation?.policyDigest ===
              currentPolicy.policyDigest
            ? lastSelectionCalculation
            : await calculateWatcherRuntimeProverFunding({
                deploymentIdentity: input.deploymentIdentity,
                protocolParameters: currentParameters,
                policy,
              });
      lastSelectionCalculation = selectionCalculation;
      const authority = await createWatcherProverFundingAuthority({
        category: request.category,
        runner: request.runner,
        actuationPermit: request.actuationPermit,
        rollbackGeneration: request.rollbackGeneration,
        deploymentIdentity: input.deploymentIdentity,
        calculation,
        selectionCalculation,
        policy,
        capacityPolicy:
          existing === undefined
            ? policy
            : createWorkflowRuntimeFundingPolicy({
                ...policyInput,
                protocolParameters:
                  input.protocolParameterHistory?.readCapacity(existing)
                    ?.snapshot ?? policyInput.protocolParameters,
              }),
        reservationPolicy,
        decisionDigest: execution.executionDecisionDigest,
        onReserved: (record) => {
          // Persist the original admitted basis before any signed intent exists.
          input.protocolParameterHistory?.remember(record, protocolParameters);
          input.protocolParameterHistory?.rememberCapacity(
            record,
            currentParameters,
          );
          reservationByPermit.set(request.actuationPermit, record);
        },
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
