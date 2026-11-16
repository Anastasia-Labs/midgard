import type {
  FamilyCommonInfrastructure,
  FamilyReferenceScriptResolver,
} from "../../src/workflow/family-application.js";
import type {
  FraudProofFamilyWorkflowAdapter,
  FraudProofWorkflowTerminalVerifier,
} from "../../src/workflow/orchestrator.js";
import type { FraudProofReleaseFinalityAuthority } from "../../src/workflow/release-finality-policy.js";

/**
 * The common infrastructure a host loads for one invocation, as a runtime
 * boundary test needs it: the identity fields are real, every Cardano-facing
 * part is an inert placeholder no test may touch.
 */
export const familyCommonInfrastructureForTest = (
  actuation: Readonly<{ headerHash: string; decisionDigest: string }>,
  overrides: Partial<FamilyCommonInfrastructure> = {},
): FamilyCommonInfrastructure => ({
  manifest: {},
  blueprintJson: "{}",
  deploymentInfo: {},
  headerHash: actuation.headerHash,
  decisionDigest: actuation.decisionDigest,
  lucid: {} as never,
  signer: {} as never,
  l1Source: {} as never,
  stateQueueMutationLeaseCoordinator: {} as never,
  ...overrides,
});

/** The resolver a record with an empty roster never reaches. */
export const emptyRosterReferenceScriptResolver: FamilyReferenceScriptResolver =
  async () => {
    throw new Error("an empty roster resolves no reference script");
  };

/**
 * The recovery surface of a test workflow that never reconciles a recorded
 * chain: inert placeholders no test may touch.
 */
export const unreachableRecoverySurfaceForTest = Object.freeze({
  adapter: {} as FraudProofFamilyWorkflowAdapter,
  terminalVerifier: {} as FraudProofWorkflowTerminalVerifier,
  releaseFinalityAuthority: {} as FraudProofReleaseFinalityAuthority,
});
