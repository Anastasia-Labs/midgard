import type { WorkflowAdapterRunnerInput } from "@al-ft/midgard-fault-proofs/workflow/adapters";
import { describe, expect, it, vi } from "vitest";

import { unsafeCreateWatcherFaultProofApplicationForTest } from "../../src/fault-proofs/fault-proof-application.create-watcher-fault-proof-readiness-application.js";
import { fundingTerminal } from "../funding/funding-handoff-fixture.js";
import {
  HEADER_HASH,
  observation,
  OBSERVATION_DIGEST,
  queueHeader,
  queueObservation,
} from "./fault-proof-application.queue-header-fixture.js";
import {
  admittedWatcherConfig,
  AUTHORITY,
  dependencies,
  DEPLOYMENT,
  FORWARDED_RUNTIME_CONFIG_PATH,
  infrastructure,
  MANIFEST_PATH,
  testFaultProofL1,
  transportFactory,
} from "./fault-proof-application.raw-config.js";

/**
 * The watcher reads the deployment manifest from its configured file, apart
 * from the signed release it verified at start-up. Every path that binds a
 * workflow from that file refuses it unless its content is the signed
 * manifest's, even when it names the same hub-oracle policy id.
 */

/** Hand-built queue observations stand in for the production source's admission. */
vi.mock(
  "../../src/indexers/authenticated-state-queue-observation.js",
  async (load) => ({
    ...(await load<
      typeof import("../../src/indexers/authenticated-state-queue-observation.js")
    >()),
    assertWatcherStateQueueObservation: (value: object) => {
      if (value !== queueObservation)
        throw new Error("test queue observation was not admitted");
    },
    assertWatcherStateQueueHeaderObservation: (value: object) => {
      if (value !== queueHeader)
        throw new Error("test queue header was not admitted");
    },
  }),
);

/** The signed manifest with one field changed under the same hub-oracle policy id. */
const ALTERED_MANIFEST = JSON.stringify({
  ...AUTHORITY.signedIdentity.manifest,
  updatedAt: "2026-07-29T00:00:00.000Z",
});

const startWithAlteredManifest = () => {
  const deps = dependencies();
  const readText = vi.mocked(deps.readText).getMockImplementation()!;
  vi.mocked(deps.readText).mockImplementation(async (path) =>
    path === MANIFEST_PATH ? ALTERED_MANIFEST : await readText(path),
  );
  const application = unsafeCreateWatcherFaultProofApplicationForTest(
    {
      l1: testFaultProofL1(),
      watcherConfig: admittedWatcherConfig(),
      deploymentIdentity: AUTHORITY.result,
      infrastructure: infrastructure(),
      unsafeTransportFactoryForTest: transportFactory().factory,
    },
    deps,
    { MIDGARD_WATCHER_PROVER_KEY: "word ".repeat(24).trim() },
  );
  return { deps, application };
};

describe("watcher fault-proof manifest file", () => {
  it("refuses to load the classifier from a manifest that differs from the signed one in one field", async () => {
    const { deps, application } = startWithAlteredManifest();
    try {
      await expect(
        application.classifyHeader({
          observation,
          stateQueueObservation: queueObservation,
          header: queueHeader,
          authenticatedObservationDigest: OBSERVATION_DIGEST,
        }),
      ).rejects.toThrow(
        "classifier manifest differs from the signed deployment manifest",
      );
      expect(deps.resolveSigner).not.toHaveBeenCalled();
    } finally {
      await application.close();
    }
  });

  it("refuses to verify a completion against a manifest that differs from the signed one in one field", async () => {
    const { application } = startWithAlteredManifest();
    try {
      await expect(
        application.verifyCompleted({
          category: "doubleSpend",
          headerHash: HEADER_HASH,
          decisionDigest: "cd".repeat(32),
          terminal: fundingTerminal(
            HEADER_HASH,
            "aa".repeat(32),
            "bb".repeat(32),
          ),
        }),
      ).rejects.toThrow(
        "completed workflow manifest differs from the signed deployment manifest",
      );
    } finally {
      await application.close();
    }
  });

  it("refuses to load a family runtime from a manifest that differs from the signed one in one field", async () => {
    const { deps, application } = startWithAlteredManifest();
    try {
      await expect(
        application.unsafeLoadRuntimeForTest({
          runtimeConfigPath: FORWARDED_RUNTIME_CONFIG_PATH,
          invocation: {
            category: "doubleSpend",
            hubOraclePolicyId: DEPLOYMENT,
            headerHash: HEADER_HASH,
            runtimeConfigPath: FORWARDED_RUNTIME_CONFIG_PATH,
            decisionDigest: "cd".repeat(32),
            actuationPermit: Object.freeze({
              permitVersion: "midgard-production-workflow-actuation-permit-v1",
            }),
          } as unknown as WorkflowAdapterRunnerInput,
        }),
      ).rejects.toThrow(
        "fault-proof runtime manifest differs from the signed deployment manifest",
      );
      expect(deps.resolveSigner).not.toHaveBeenCalled();
    } finally {
      await application.close();
    }
  });
});
