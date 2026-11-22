import { DEPLOYMENT_MANIFEST_L1_FINALITY } from "@al-ft/midgard-core/deployment-manifest-identity";
import { expect, it, vi } from "vitest";

import { FIELD_ITEM_WIDTH_ILLEGAL_CURSOR_SPEC } from "../src/field-item-width-illegal/workflow-spec.js";
import { createWorkflowReconciliationPermitController } from "../src/workflow/actuation-permit.create-workflow-reconciliation-permit-controller.js";
import { COMPLETE_CANONICAL_REPLAY } from "../src/workflow/complete-replay.replay-context-identity.js";
import {
  HEADER_CLASSIFIER,
  HEADER_DECISION,
  type HeaderFaultDecision,
} from "../src/workflow/header-classifier.authenticated-state-queue-observation-digest.js";
import {
  journalJsonDigest,
  type JournalJsonObject,
  normalizeJournalJson,
} from "../src/workflow/journal.fraud-proof-workflow-terminal.js";
import { createCanonicalFamilyArtifactPort } from "../src/workflow/manifest-bound-family-recovery.js";
import {
  FRAUD_PROOF_WORKFLOW_ADAPTER,
  FRAUD_PROOF_WORKFLOW_SAFETY,
  type FraudProofFamilyWorkflowAdapter,
} from "../src/workflow/orchestrator.fraud-proof-family-workflow-adapter.js";
import {
  computeFraudProofReleaseFinalityPolicyDigest,
  FRAUD_PROOF_RELEASE_FINALITY_AUTHORITY,
  FRAUD_PROOF_RELEASE_FINALITY_POLICY_SCHEMA_VERSION,
} from "../src/workflow/release-finality-policy.js";
import { runRegisteredFamilyReconciliation } from "./support/bound-family-recovery-fixture.js";
import { customWorkflowRecoveryFixture } from "./support/custom-workflow-recovery.js";
import { prepareReconcileThroughRunner } from "./support/reconcile-through-runner.js";
import { terminalVerifierOf } from "./support/terminal-verifier.js";

const hash = (byte: string) => byte.repeat(32);
const category = "fieldItemWidthIllegal";
const headerHash = "11".repeat(28);
const deploymentFingerprint = hash("22");
const policy = { ...DEPLOYMENT_MANIFEST_L1_FINALITY };
const releaseFinality = {
  schemaVersion: FRAUD_PROOF_RELEASE_FINALITY_POLICY_SCHEMA_VERSION,
  deploymentIdentityDigest: deploymentFingerprint,
  blueprintHash: hash("44"),
  policyDigest: computeFraudProofReleaseFinalityPolicyDigest(policy),
  policy,
};

// Local decision fixture. This test exercises the real opaque reconciliation
// permit and runner; it does not claim native L1 evidence.
const recovery = () => {
  const unsealed: Omit<HeaderFaultDecision, "decisionDigest"> = {
    schemaVersion: HEADER_DECISION,
    classifierVersion: HEADER_CLASSIFIER,
    deploymentFingerprint,
    headerHash,
    authenticatedObservationDigest: hash("55"),
    payloadEnvelopeSha256: hash("66"),
    payloadSha256: hash("77"),
    replayVersion: COMPLETE_CANONICAL_REPLAY,
    replayDigest: hash("88"),
    launchScope: [category],
    launchScopeDigest: hash("99"),
    classificationDigest: hash("aa"),
    decision: "fault_detected",
    category,
    violationId: "ScriptIntegrityHashMissing",
    detectionId: "retained-local-fault",
    position: "0",
  };
  const decision = {
    ...unsealed,
    decisionDigest: journalJsonDigest(normalizeJournalJson(unsealed)),
  };
  const controller = createWorkflowReconciliationPermitController({
    decision,
    deploymentFingerprint,
    rollbackGeneration: "0",
  });
  const forbidden = vi.fn(async (): Promise<never> => {
    throw new Error("read-only recovery entered construction or submission");
  });
  const adapter: FraudProofFamilyWorkflowAdapter = {
    adapterVersion: FRAUD_PROOF_WORKFLOW_ADAPTER,
    category,
    safety: FRAUD_PROOF_WORKFLOW_SAFETY,
    prepare: forbidden,
    preflight: forbidden,
    submit: forbidden,
    observe: forbidden,
  };
  const prepare = () =>
    prepareReconcileThroughRunner({
      category,
      deploymentFingerprint,
      headerHash,
      decisionDigest: decision.decisionDigest,
      permit: controller.permit,
      surface: {
        adapter,
        terminalVerifier: terminalVerifierOf(forbidden),
        releaseFinalityAuthority: {
          authorityVersion: FRAUD_PROOF_RELEASE_FINALITY_AUTHORITY,
          verifyForWorkflow: async () => releaseFinality,
        },
      },
    });
  return { prepare, controller, forbidden };
};

it("refuses reconciliation after actual rollback revokes the permit", async () => {
  const fixture = recovery();
  // The invocation is ready before the rollback lands, so the runner is the
  // one that refuses it.
  const resume = fixture.prepare();
  fixture.controller.revoke("native_chain_rollback");
  await expect(resume()).rejects.toThrow(
    "production workflow actuation revoked before runner_start: native_chain_rollback",
  );
  expect(fixture.forbidden).not.toHaveBeenCalled();
});

it("refuses a different decision before opening the target", async () => {
  // The family's registered runner, resumed under a decision other than the
  // one its reconciliation permit admitted.
  const fixture = await customWorkflowRecoveryFixture(
    FIELD_ITEM_WIDTH_ILLEGAL_CURSOR_SPEC,
  );
  fixture.advance();
  const refusal = await runRegisteredFamilyReconciliation(fixture, {
    decisionDigest: hash("ef"),
  }).then(
    () => undefined,
    (error: unknown) => error,
  );
  // Nothing was observed or built.
  expect(fixture.observeHeader).not.toHaveBeenCalled();
  expect(fixture.capture).not.toHaveBeenCalled();
  expect(refusal).toBeInstanceOf(Error);
  expect((refusal as Error).message).toBe(
    "production workflow actuation permit identity mismatch at runner_start",
  );
});

it("re-admits exact typed material after restart without deserializing saved JSON", async () => {
  const fresh = () => ({
    value: 9_007_199_254_740_993n,
    bytes: Buffer.from("cafe", "hex"),
  });
  // This isolated port test supplies the derivation result; family tests own canonical replay.
  const input = {} as Parameters<
    ReturnType<typeof createCanonicalFamilyArtifactPort>["prepare"]
  >[0];
  const first = createCanonicalFamilyArtifactPort(async () => fresh());
  const artifact = await first.prepare(input);
  const saved = JSON.parse(JSON.stringify(artifact)) as JournalJsonObject;
  const restarted = createCanonicalFamilyArtifactPort(async () => fresh());
  expect(() => restarted.require(saved)).toThrow("was not admitted");
  await restarted.validatePreparedArtifact({ ...input, artifact: saved });
  expect(restarted.require(saved)).toEqual(fresh());
  expect(Buffer.isBuffer(restarted.require(saved).bytes)).toBe(true);
});

it("refuses changed replay material and discards prior admission after a failed recheck", async () => {
  let value = 1n;
  const port = createCanonicalFamilyArtifactPort(async () => ({ value }));
  const input = {} as Parameters<typeof port.prepare>[0];
  const artifact = await port.prepare(input);
  value = 2n;
  await expect(
    port.validatePreparedArtifact({ ...input, artifact }),
  ).rejects.toThrow("freshly authenticated material");
  expect(() => port.require(artifact)).toThrow("was not admitted");
});

it("compares an explicit durable projection while retaining only newly derived typed material", async () => {
  class Authentication {
    constructor(readonly bytes: Buffer) {}
  }
  const derive = async () => ({
    authentication: new Authentication(Buffer.from("cafe", "hex")),
  });
  const port = () =>
    createCanonicalFamilyArtifactPort(derive, (material) => ({
      authentication: material.authentication.bytes.toString("hex"),
    }));
  const first = port();
  const input = {} as Parameters<typeof first.prepare>[0];
  const saved = JSON.parse(JSON.stringify(await first.prepare(input)));
  const restarted = port();
  await restarted.validatePreparedArtifact({ ...input, artifact: saved });
  expect(restarted.require(saved).authentication).toBeInstanceOf(
    Authentication,
  );
  expect(restarted.require(saved).authentication.bytes.toString("hex")).toBe(
    "cafe",
  );
});
