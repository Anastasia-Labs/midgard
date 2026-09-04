import { Deferred, Effect, Fiber, Option } from "effect";
import { describe, expect, it } from "vitest";

import { evaluateReadiness } from "../src/commands/readiness.js";
import { runAfterL1ControlPlaneRelease } from "../src/fibers/da-publication-trigger.js";

const healthyReadiness = () =>
  evaluateReadiness({
    nowMillis: 1_000,
    maxHeartbeatAgeMs: 10_000,
    maxQueueDepth: 100,
    queueDepth: 0,
    workerHeartbeats: {
      blockCommitment: 1_000,
      blockConfirmation: 1_000,
      merge: 1_000,
      depositFetch: 1_000,
      withdrawalFetch: 1_000,
      txQueueProcessor: 1_000,
    },
    localFinalizationPending: false,
    unresolvedBlockSubmissionAgeMs: 0,
    maxUnresolvedBlockSubmissionAgeMs: 60_000,
    dbHealthy: true,
    awaitingForeignTipReconciliations: 0,
  });

describe("post-finalization DA publication ordering", () => {
  for (const path of ["legacy", "speculative"] as const) {
    it(`${path} releases the L1 permit and completes mutation before a dead-peer publication wait`, async () => {
      let unfinishedLocalMutationJobs = 1;
      let durablePublicationBacklog = 0;
      let publicationAttempts = 0;
      const publicationStarted = await Effect.runPromise(Deferred.make<void>());
      const releaseDeadPeer = await Effect.runPromise(Deferred.make<void>());
      const l1ControlPlane = await Effect.runPromise(Effect.makeSemaphore(1));

      const program = runAfterL1ControlPlaneRelease(
        l1ControlPlane.withPermits(1)(
          Effect.sync(() => {
            durablePublicationBacklog = 4;
            unfinishedLocalMutationJobs = 0;
            return { finalizedHeaderHash: "ab".repeat(28) };
          }),
        ),
        (result) => result.finalizedHeaderHash,
        () =>
          Effect.gen(function* () {
            publicationAttempts += 1;
            yield* Deferred.succeed(publicationStarted, undefined);
            yield* Deferred.await(releaseDeadPeer);
            yield* Effect.fail(new Error("dead committee peer timeout"));
          }).pipe(Effect.either, Effect.asVoid),
      );

      const fiber = Effect.runFork(program);
      await Effect.runPromise(Deferred.await(publicationStarted));

      const permit = await Effect.runPromise(
        l1ControlPlane.withPermitsIfAvailable(1)(Effect.succeed("reacquired")),
      );
      expect(Option.getOrUndefined(permit)).toBe("reacquired");

      const readiness = healthyReadiness();
      const reasons = [...readiness.reasons];
      if (unfinishedLocalMutationJobs > 0) {
        reasons.push(
          `unfinished_local_mutation_jobs:${unfinishedLocalMutationJobs.toString()}`,
        );
      }
      expect({ ready: reasons.length === 0, reasons }).toEqual({
        ready: true,
        reasons: [],
      });
      expect(durablePublicationBacklog).toBe(4);
      expect(publicationAttempts).toBe(1);
      expect(Option.isNone(await Effect.runPromise(Fiber.poll(fiber)))).toBe(
        true,
      );

      await Effect.runPromise(Deferred.succeed(releaseDeadPeer, undefined));
      await expect(Effect.runPromise(Fiber.join(fiber))).resolves.toEqual({
        finalizedHeaderHash: "ab".repeat(28),
      });
    });
  }

  it("wires both parent submit paths to publish only after the outer L1 effect", async () => {
    for (const relativePath of [
      "../src/fibers/block-commitment.ts",
      "../src/fibers/speculative-commit-builder.ts",
    ]) {
      const source = await readFile(
        new URL(relativePath, import.meta.url),
        "utf8",
      );
      const ordering = source.slice(
        source.indexOf("runAfterL1ControlPlaneRelease("),
        source.indexOf(
          "publishFinalizedDaPayloadBestEffort",
          source.indexOf("runAfterL1ControlPlaneRelease("),
        ) + "publishFinalizedDaPayloadBestEffort".length,
      );
      expect(ordering).toContain("withL1ControlPlane(");
      expect(ordering.indexOf("withL1ControlPlane(")).toBeGreaterThan(0);
      expect(
        ordering.indexOf("publishFinalizedDaPayloadBestEffort"),
      ).toBeGreaterThan(ordering.indexOf("withL1ControlPlane("));
    }

    const workerSource = await readFile(
      new URL("../src/workers/utils/commit-submission.ts", import.meta.url),
      "utf8",
    );
    expect(workerSource).not.toContain("publishDaPayloadInsertFromEnv");
    expect(workerSource).toContain("seedDaPayloadPublicationOutboxFromEnv");
  });
});
import { readFile } from "node:fs/promises";
