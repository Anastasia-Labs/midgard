import { describe, expect, it } from "vitest";

import {
  evaluateWatcherFinality,
  makeWatcherFinalityPolicy,
} from "../../src/l1/finality-engine.js";
import type { WatcherRollbackDurableTrustedHead } from "../../src/l1/rollback-engine.js";
import { createWatcherDurableRuntime } from "../../src/storage/durable-runtime.js";
import { createSyntheticStateQueueObservationFixture } from "../support/state-queue-observation-fixture.js";
import {
  hex32,
  MemoryRollbackAuthorityBackend,
  rollbackAuthorityKey,
} from "./rollback-engine.test-tls-identities.js";

describe("native reconnect durable evidence", () => {
  it("bounds same-frontier native reconnect evidence across published durable heads", async () => {
    const fixture = await createSyntheticStateQueueObservationFixture();
    try {
      const first = await fixture.observeFresh();
      const finalityPolicy = makeWatcherFinalityPolicy(
        fixture.transport.watcherConfig,
        fixture.transport.deploymentIdentity,
      );
      if (finalityPolicy === null) throw new Error("Expected local policy");
      const backend = new MemoryRollbackAuthorityBackend();
      let head: WatcherRollbackDurableTrustedHead | null = null;
      const runtime = await createWatcherDurableRuntime({
        backend,
        policy: finalityPolicy,
        authenticationKey: rollbackAuthorityKey,
        client: {
          readRecordAuthenticationKeyId: async () => hex32("99"),
          readCurrent: async () => head,
          compareAndSwap: async ({ expectedTrustedHead, nextTrustedHead }) => {
            if (JSON.stringify(head) !== JSON.stringify(expectedTrustedHead))
              return false;
            head = nextTrustedHead;
            return true;
          },
        },
      });
      await runtime.persistCanonicalProgress(first.localObservation);
      const second = await fixture.observeFresh();
      await runtime.persistCanonicalProgress(second.localObservation);
      expect(runtime.readFinality().phase).toBe("finalized");
      const finalized = runtime.readFinality();
      const depths = new Set<string>();
      for (let i = 0; i < 5; i += 1) {
        const replay = await fixture.observeFresh();
        depths.add(replay.localObservation.block.chainPoint.depth);
        await runtime.persistObservation(replay.localObservation);
        const result = await runtime.persistRollback({
          previousFinalityState: finalized,
          consistency: replay.localObservation.consistency,
          finalityResult: evaluateWatcherFinality(
            finalityPolicy,
            finalized,
            replay.localObservation.consistency,
          ),
          transportAttestations: replay.localObservation.transportAttestations,
        });
        if (result.persistence === "conflict")
          throw new Error("Unexpected reconnect CAS conflict");
        expect(result.result.action).toBe("duplicate_rewind");
        expect(
          runtime.read().authenticatedConsistencyHistory.length,
        ).toBeLessThanOrEqual(3);
        expect(
          runtime.read().currentStore.l1Observations.length,
        ).toBeLessThanOrEqual(9);
      }
      expect(depths.size).toBe(5);
      const reopened = await createWatcherDurableRuntime({
        backend,
        policy: finalityPolicy,
        authenticationKey: rollbackAuthorityKey,
        client: {
          readRecordAuthenticationKeyId: async () => hex32("99"),
          readCurrent: async () => head,
          compareAndSwap: async () => false,
        },
      });
      expect(reopened.read()).toEqual(runtime.read());
    } finally {
      await fixture.close();
    }
  }, 60_000);
});
