import { VALIDATION_TRACE_DISPUTE_STEP_COUNT } from "@al-ft/midgard-sdk";
import { type LucidEvolution, type UTxO } from "@lucid-evolution/lucid";

import type { StateQueueMutationLeaseCoordinator } from "../remove-fraudulent-block.js";
import type { ResolvedProverSigner } from "../runtime.js";
import {
  requireValidationTraceChallenge,
  type ValidationTraceChallenge,
  validationTraceMaterial,
} from "../workflow/challenge-authority.js";
import {
  assertManifestBoundWorkflowSigner,
  releaseFinalityAuthorityFromDeploymentBinding,
  requireManifestBoundReferenceScriptUtxo,
} from "../workflow/deployment-manifest-binding.js";
import {
  createFraudProofFamilyAuthenticatedL1TerminalVerifier,
  createFraudProofFamilyLocalKupmiosL1ObservationPort,
  type FraudProofFamilyL1ObservationPort,
} from "../workflow/family-l1-observation.js";
import type { LocalKupmiosHttpOgmiosSourceConfig } from "../workflow/local-kupmios-http-ogmios-source.js";
import type {
  FraudProofFamilyWorkflowAdapter,
  FraudProofWorkflowTerminalVerifier,
} from "../workflow/orchestrator.js";
import { fraudProofRawL1SnapshotRequestForFamily } from "../workflow/raw-l1-family-derivation.js";
import { admitFraudProofRawL1Snapshot } from "../workflow/raw-l1-snapshot.js";
import type { FraudProofReleaseFinalityAuthority } from "../workflow/release-finality-policy.js";
import {
  bindValidationTraceDisputeWorkflowDeployment,
  type ValidationTraceDisputeWorkflowDeploymentBinding,
} from "./workflow-binding.js";
import {
  deriveValidationTraceDisputeChainStage,
  type ValidationTraceDisputeChainStage,
} from "./workflow-chain-state.js";
import {
  createValidationTraceDisputeActuator,
  decodeOperatorRevealProofsFromWitnessSet,
  type ValidationTraceDisputeActuationMaterial,
} from "./workflow-engine.js";
import {
  assertValidationTraceDisputeRosterIsManifestBound,
  VALIDATION_TRACE_DISPUTE_CATEGORY_ID,
  VALIDATION_TRACE_DISPUTE_CONTROL_CONTRACT_NAMES,
  VALIDATION_TRACE_DISPUTE_REMOVAL_CONTRACT_NAMES,
  VALIDATION_TRACE_DISPUTE_WITNESS_CONTRACT_NAMES,
} from "./workflow-family.js";
import { createValidationTraceFieldCarriageProvider } from "./workflow-field-carriage.js";
import { createValidationTraceDisputeRecoveryAdapter } from "./workflow-v1.recovery-adapter.js";

export const VALIDATION_TRACE_DISPUTE_WORKFLOW =
  "midgard-validation-trace-dispute-production-workflow-v1" as const;

export type ValidationTraceDisputeControlReferences = Readonly<{
  opener: UTxO;
  source: UTxO;
  game: UTxO;
  boundary: UTxO;
  timeout: UTxO;
  award: UTxO;
}>;

export type ValidationTraceDisputeRemovalReferences = Readonly<
  Record<keyof typeof VALIDATION_TRACE_DISPUTE_REMOVAL_CONTRACT_NAMES, UTxO>
>;

export type ValidationTraceDisputeReferences = Readonly<{
  control: ValidationTraceDisputeControlReferences;
  witnesses: Readonly<{
    computationThreadMint: UTxO;
    fraudProofMint: UTxO;
    phasMembershipWithdraw: UTxO;
  }>;
  removal: ValidationTraceDisputeRemovalReferences;
}>;

export const VALIDATION_TRACE_DISPUTE_CONFIG_KEYS = Object.freeze([
  "manifest",
  "blueprintJson",
  "deploymentInfo",
  "headerHash",
  "lucid",
  "signer",
  "source",
  "decisionDigest",
  "referenceScripts",
  "stateQueueMutationLeaseCoordinator",
] as const);

const configKeyJoin = (keys: readonly string[]): string =>
  [...keys].sort().join("\0");

const REQUIRED_CONFIG_KEY_JOIN = configKeyJoin(
  VALIDATION_TRACE_DISPUTE_CONFIG_KEYS,
);

const EXECUTION_CONFIG_KEY_JOIN = configKeyJoin([
  ...VALIDATION_TRACE_DISPUTE_CONFIG_KEYS,
  "challenge",
]);

export type ManifestBoundValidationTraceDisputeWorkflowConfig = Readonly<{
  manifest: unknown;
  blueprintJson: string;
  deploymentInfo: unknown;
  headerHash: string;
  lucid: LucidEvolution;
  signer: ResolvedProverSigner;
  source: Omit<LocalKupmiosHttpOgmiosSourceConfig, "releaseFinality">;
  decisionDigest: string;
  /**
   * The freshly admitted W25 validation-trace challenge: the operator's
   * root-bound claim witness plus the challenger's deterministic replay,
   * rebuilt in this process by `admitValidationTraceChallenge`. A journal
   * copy or caller-authored object is refused by the admission registry.
   *
   * Optional at CONSTRUCTION so startup readiness can bind the manifest,
   * signer, and reference roster before any dispute exists — the same
   * contract as the other production families. EXECUTION fail-closes
   * without it: no transaction is planned or actuated challenge-free.
   */
  challenge?: ValidationTraceChallenge;
  referenceScripts: ValidationTraceDisputeReferences;
  stateQueueMutationLeaseCoordinator: StateQueueMutationLeaseCoordinator;
}>;

export type ManifestBoundValidationTraceDisputeWorkflow = Readonly<{
  binding: ValidationTraceDisputeWorkflowDeploymentBinding;
  lucid: LucidEvolution;
  signer: ResolvedProverSigner;
  decisionDigest: string;
  challenge: ValidationTraceChallenge | undefined;
  material: ValidationTraceDisputeActuationMaterial | undefined;
  l1: FraudProofFamilyL1ObservationPort<"validationTraceDispute">;
  actuator: ReturnType<typeof createValidationTraceDisputeActuator>;
  fieldCarriage: ReturnType<typeof createValidationTraceFieldCarriageProvider>;
  adapter: FraudProofFamilyWorkflowAdapter;
  terminalVerifier: FraudProofWorkflowTerminalVerifier;
  releaseFinalityAuthority: FraudProofReleaseFinalityAuthority;
  deriveStage: (
    currentTime: number,
  ) => Promise<ValidationTraceDisputeChainStage>;
}>;

/**
 * Manifest/reference/signer-bound construction for the sole interactive
 * family (ruling R6, "installed" semantics): from every derived chain stage
 * the workflow owns exactly one legal transaction, is waiting on the
 * operator's response clock with the timeout claim armed at its lapse, or
 * the journey is complete — detect, initiate, play every honest response,
 * claim timeout when the operator stalls, take the award, and remove the
 * fraudulent block. The dispute cursor is re-derived exclusively from live
 * chain state on every invocation, so an interrupted runner resumes from
 * where the chain — not local memory — says the dispute stands.
 */
export const createManifestBoundValidationTraceDisputeWorkflow = async (
  config: ManifestBoundValidationTraceDisputeWorkflowConfig,
): Promise<ManifestBoundValidationTraceDisputeWorkflow> => {
  const keyJoin = configKeyJoin(Object.keys(config));
  if (
    keyJoin !== REQUIRED_CONFIG_KEY_JOIN &&
    keyJoin !== EXECUTION_CONFIG_KEY_JOIN
  )
    throw new Error(
      "validationTraceDispute production config contains callback authority",
    );
  if (!/^[0-9a-f]{64}$/u.test(config.decisionDigest))
    throw new Error("validationTraceDispute decision digest is malformed");
  assertValidationTraceDisputeRosterIsManifestBound();
  const challenge =
    config.challenge === undefined
      ? undefined
      : requireValidationTraceChallenge(config.challenge);
  if (
    challenge !== undefined &&
    challenge.coordinate.headerHash !== config.headerHash
  )
    throw new Error(
      "validationTraceDispute challenge targets a different header",
    );
  const binding = await bindValidationTraceDisputeWorkflowDeployment({
    manifest: config.manifest,
    blueprintJson: config.blueprintJson,
    deploymentInfo: config.deploymentInfo,
    headerHash: config.headerHash,
    proverCredential: config.signer.paymentKeyHash,
  });
  if (
    challenge !== undefined &&
    challenge.coordinate.deploymentFingerprint !== binding.deploymentFingerprint
  )
    throw new Error(
      "validationTraceDispute challenge was admitted against a different deployment",
    );
  assertManifestBoundWorkflowSigner({
    network: binding.network,
    address: config.signer.address,
    paymentKeyHash: config.signer.paymentKeyHash,
  });
  const chain = binding.resolvedContracts.contracts.validationTraceDispute;
  if (chain.steps.length !== VALIDATION_TRACE_DISPUTE_STEP_COUNT)
    throw new Error(
      "validationTraceDispute manifest omitted required deployed steps",
    );
  const bind = (name: string, utxo: UTxO) =>
    requireManifestBoundReferenceScriptUtxo({
      binding,
      contractName: name,
      utxo,
    });
  const control = Object.fromEntries(
    Object.entries(VALIDATION_TRACE_DISPUTE_CONTROL_CONTRACT_NAMES).map(
      ([role, name]) => [
        role,
        bind(
          name,
          config.referenceScripts.control[
            role as keyof ValidationTraceDisputeControlReferences
          ],
        ),
      ],
    ),
  ) as ValidationTraceDisputeControlReferences;
  const witnesses = Object.freeze({
    computationThreadMint: bind(
      VALIDATION_TRACE_DISPUTE_WITNESS_CONTRACT_NAMES.computationThreadMint,
      config.referenceScripts.witnesses.computationThreadMint,
    ),
    fraudProofMint: bind(
      VALIDATION_TRACE_DISPUTE_WITNESS_CONTRACT_NAMES.fraudProofMint,
      config.referenceScripts.witnesses.fraudProofMint,
    ),
    phasMembershipWithdraw: bind(
      VALIDATION_TRACE_DISPUTE_WITNESS_CONTRACT_NAMES.phasMembershipWithdraw,
      config.referenceScripts.witnesses.phasMembershipWithdraw,
    ),
  });
  for (const [role, name] of Object.entries(
    VALIDATION_TRACE_DISPUTE_REMOVAL_CONTRACT_NAMES,
  ))
    bind(
      name,
      config.referenceScripts.removal[
        role as keyof ValidationTraceDisputeRemovalReferences
      ],
    );
  const l1 = createFraudProofFamilyLocalKupmiosL1ObservationPort({
    source: config.source,
    releaseFinality: binding.releaseFinality,
    releaseEconomics: binding.releaseEconomics,
    definition: binding.definition,
  });
  const material: ValidationTraceDisputeActuationMaterial | undefined =
    challenge === undefined
      ? undefined
      : Object.freeze({
          headerHash: config.headerHash,
          ...validationTraceMaterial(challenge),
        });
  const rawL1 = l1.rawL1;
  if (rawL1 === undefined)
    throw new Error(
      "validationTraceDispute requires the raw L1 snapshot authority",
    );
  const snapshotRequest = fraudProofRawL1SnapshotRequestForFamily({
    definition: binding.definition,
    releaseFinality: binding.releaseFinality,
  });
  const operatorProofs = Object.freeze({
    /**
     * Operator bisection reveals harvested from the admitted raw thread-unit
     * transaction history: every `Continue(RevealOperator)` redeemer the
     * operator has published on-chain for this dispute.
     */
    collect: async () => {
      const snapshot = admitFraudProofRawL1Snapshot({
        value: await rawL1.capture(snapshotRequest),
        request: snapshotRequest,
        releaseFinality: binding.releaseFinality,
        observationDepth: "inclusion",
      });
      return snapshot.transactions.flatMap((transaction) =>
        decodeOperatorRevealProofsFromWitnessSet(transaction.witnessSetCbor),
      );
    },
  });
  const fieldCarriage = createValidationTraceFieldCarriageProvider({
    binding,
    lucid: config.lucid,
    signer: config.signer,
    l1,
    material,
  });
  const actuator = createValidationTraceDisputeActuator({
    fieldCarriage,
    lucid: config.lucid,
    blueprint: binding.blueprint,
    deploymentInfo: binding.deploymentInfo,
    network: binding.network,
    signer: config.signer,
    categoryId: VALIDATION_TRACE_DISPUTE_CATEGORY_ID,
    resolved: binding.resolvedContracts,
    references: {
      control: {
        source: control.source,
        game: control.game,
        boundary: control.boundary,
        timeout: control.timeout,
        award: control.award,
      },
      witnesses,
    },
    operatorProofs,
    stateQueueMutationLeaseCoordinator:
      config.stateQueueMutationLeaseCoordinator,
    fraudProverRewardLovelace: BigInt(
      binding.releaseEconomics.policy.fraudProverRewardLovelace,
    ),
  });
  const deriveStage = async (currentTime: number) =>
    await deriveValidationTraceDisputeChainStage({
      lucid: config.lucid,
      chain,
      computationThreadPolicyId:
        binding.resolvedContracts.contracts.computationThread.policyId,
      fraudProofPolicyId:
        binding.resolvedContracts.contracts.fraudProof.policyId,
      fraudProofSpendingScriptAddress:
        binding.resolvedContracts.contracts.fraudProof.spendingScriptAddress,
      stateQueue: binding.definition.stateQueue,
      categoryId: VALIDATION_TRACE_DISPUTE_CATEGORY_ID,
      headerHash: config.headerHash,
      currentTime,
    });
  const mechanics = {
    binding,
    lucid: config.lucid,
    signer: config.signer,
    decisionDigest: config.decisionDigest,
    challenge,
    material,
    l1,
    actuator,
    fieldCarriage,
    deriveStage,
  };
  return Object.freeze({
    ...mechanics,
    adapter: createValidationTraceDisputeRecoveryAdapter({
      workflow: mechanics,
      stateQueueMutationLeaseCoordinator:
        config.stateQueueMutationLeaseCoordinator,
    }),
    terminalVerifier: createFraudProofFamilyAuthenticatedL1TerminalVerifier(l1),
    releaseFinalityAuthority:
      releaseFinalityAuthorityFromDeploymentBinding(binding),
  });
};
