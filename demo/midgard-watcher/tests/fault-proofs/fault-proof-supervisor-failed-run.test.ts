import { mkdir, mkdtemp, rm } from "node:fs/promises";
import { join } from "node:path";

import {
  authenticatedStateQueueObservationDigest,
  classifyHeader,
  createHeaderClassifier,
  createWorkflowActuationPermitController,
  DOUBLE_SPEND_COMPLETE_CANONICAL_REPLAY,
} from "@al-ft/midgard-fault-proofs";
import {
  authenticatedHeaderObservation,
  buildCanonicalBlockFixture,
  buildFixtureTransaction,
  outRefCbor,
} from "@al-ft/midgard-fault-proofs/test-support/canonical-block-evidence-fixture";
import { afterEach, describe, expect, it, vi } from "vitest";

import {
  openWatcherFaultProofQueueJournal,
  watcherFaultProofQueueIdentityDigest,
} from "../../src/fault-proofs/fault-proof-queue-journal.js";
import { unsafeCreateWatcherFaultProofSupervisorForTest } from "../../src/fault-proofs/fault-proof-supervisor.js";
import { watcherDeploymentReleaseFinalityAuthority } from "../../src/runtime/deployment-identity.js";
import { makeWatcherDeploymentAuthorityFixture } from "../support/deployment-authority-fixture.js";

// The double-spend classifier fixture launches one family, so the installed
// scope is narrowed to that family for this file only.
vi.mock("../../src/fault-proofs/fault-proof-application.js", async (load) => {
  const actual =
    await load<
      typeof import("../../src/fault-proofs/fault-proof-application.js")
    >();
  return {
    ...actual,
    WATCHER_INSTALLED_WORKFLOW_CATEGORIES: Object.freeze(["doubleSpend"]),
  };
});

const directories: string[] = [];
afterEach(async () => {
  await Promise.all(
    directories
      .splice(0)
      .map(async (path) => rm(path, { recursive: true, force: true })),
  );
});

const deploymentIdentity = makeWatcherDeploymentAuthorityFixture().result;
const QUEUE_KEY = Uint8Array.from({ length: 32 }, () => 0x5a);
const ROLLBACK_GENERATION = "3";
const deadline = Object.freeze({
  headerEndTimeMs: "0",
  maturityAtMs: "604800000",
  latestSafeStartAtMs: "302400000",
});

const classifyDoubleSpend = async () => {
  const fixture = await buildCanonicalBlockFixture({
    transactions: [
      buildFixtureTransaction({ spendInputs: [outRefCbor(91, 0n)], fee: 1n }),
      buildFixtureTransaction({ spendInputs: [outRefCbor(91, 0n)], fee: 2n }),
    ],
  });
  const observation = {
    ...authenticatedHeaderObservation(fixture),
    confirmationDepth: 30,
  };
  const classifier = await createHeaderClassifier({
    deploymentFingerprint: deploymentIdentity.manifestId,
    replayer: DOUBLE_SPEND_COMPLETE_CANONICAL_REPLAY,
    releaseFinalityAuthority:
      watcherDeploymentReleaseFinalityAuthority(deploymentIdentity),
  });
  const decision = await classifyHeader({
    classifier,
    observation,
    authenticatedObservationDigest:
      await authenticatedStateQueueObservationDigest({
        observation,
        minimumConfirmationDepth: 30,
      }),
    sources: [
      {
        sourceId: "libp2p-test",
        fetchPayloadByHeaderHash: async () => ({
          ok: true as const,
          provenance: {
            trustClass: "public_or_permissionless_da" as const,
            sourceId: "libp2p-test/peer-a",
            grade: "security" as const,
          },
          sourceId: "libp2p-test",
          sourcePeerId: "peer-a",
          payloadEnvelopeCbor: fixture.payloadEnvelopeCbor,
          attempts: [],
        }),
      },
    ],
  });
  if (decision.decision !== "fault_detected")
    throw new Error("fixture must classify a double spend");
  return decision;
};

const createProcess = (
  root: string,
  run: (mode: "run" | "resume") => Promise<unknown>,
) =>
  unsafeCreateWatcherFaultProofSupervisorForTest({
    journalRoot: root,
    deploymentFingerprint: deploymentIdentity.manifestId,
    unsafeQueueAuthenticationKeyForTest: QUEUE_KEY,
    run: async (job) => await run(job.mode),
  });

describe("fault-proof supervisor over a run that failed before its journal", () => {
  it("requeues the job in the next process instead of reading a durable finish", async () => {
    const root = await mkdtemp("/var/tmp/midgard-fault-supervisor-failed-");
    directories.push(root);
    const job = Object.freeze({
      mode: "run" as const,
      category: "doubleSpend" as const,
      headerHash: "11".repeat(28),
      decisionDigest: "22".repeat(32),
      rollbackGeneration: ROLLBACK_GENERATION,
    });
    const failed = createProcess(root, async () => {
      throw new Error("workflow failed before its first journal entry");
    });
    await failed.recoverExisting(null);
    await expect(failed.unsafeRunOrResumeForTest(job)).rejects.toThrow(
      "workflow failed before its first journal entry",
    );
    expect(failed.status().phase).toBe("blocked");
    await failed.close();

    const modes: string[] = [];
    const next = createProcess(root, async (mode) => {
      modes.push(mode);
      return mode;
    });
    await next.recoverExisting(null);
    await expect(next.unsafeRunOrResumeForTest(job)).resolves.toBe("run");
    await next.close();
    expect(modes).toEqual(["run"]);
  }, 60_000);

  it("reopens a finished registration whose workflow directory has no execution", async () => {
    const root = await mkdtemp("/var/tmp/midgard-fault-supervisor-failed-");
    directories.push(root);
    const decision = await classifyDoubleSpend();
    // An earlier process recorded the job as finished after a run that never
    // reached its workflow journal.
    const journal = await openWatcherFaultProofQueueJournal({
      journalRoot: root,
      deploymentFingerprint: deploymentIdentity.manifestId,
      authenticationKey: QUEUE_KEY,
    });
    const identity = Object.freeze({
      category: "doubleSpend",
      headerHash: decision.headerHash,
      decisionDigest: decision.decisionDigest,
      rollbackGeneration: ROLLBACK_GENERATION,
    });
    const digest = watcherFaultProofQueueIdentityDigest({
      deploymentFingerprint: deploymentIdentity.manifestId,
      identity,
    });
    await journal.register(identity, "0");
    await journal.markStarted(digest, "0");
    await journal.markFinished(digest, "0");
    await mkdir(
      join(root, "fault-proofs", "doubleSpend", decision.headerHash),
      {
        recursive: true,
        mode: 0o700,
      },
    );

    const modes: string[] = [];
    const supervisor = createProcess(root, async (mode) => {
      modes.push(mode);
      return mode;
    });
    const controller = createWorkflowActuationPermitController({
      decision,
      rollbackGeneration: ROLLBACK_GENERATION,
    });
    const recovered = await supervisor.recoverExisting(
      decision,
      controller.permit,
      Object.freeze({ ...deadline, headerHash: decision.headerHash }),
      ROLLBACK_GENERATION,
    );
    await supervisor.close();
    expect({ recovered, modes }).toEqual({ recovered: 1, modes: ["resume"] });
  }, 60_000);
});
