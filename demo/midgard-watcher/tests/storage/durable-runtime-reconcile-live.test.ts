import { describe, expect, it } from "vitest";

import { makeWatcherFinalityPolicy } from "../../src/l1/finality-engine.js";
import { createWatcherDurableRuntime } from "../../src/storage/durable-runtime.js";
import { createSyntheticStateQueueObservationFixture } from "../support/state-queue-observation-fixture.js";
import { client, key, MemoryBackend } from "./durable-runtime.policy.js";

describe("live durable authority reconciliation", () => {
  it("reauthenticates a committed direct successor and rechecks the native generation at actual CAS", async () => {
    const fixture = await createSyntheticStateQueueObservationFixture();
    try {
      const capture = await fixture.observeFresh();
      const policy = makeWatcherFinalityPolicy(
        fixture.transport.watcherConfig,
        fixture.transport.deploymentIdentity,
      );
      if (policy === null) throw new Error("signed policy rejected");
      const backend = new MemoryBackend();
      const options = { refuseCas: false };
      const sidecar = client(options);
      const sourceGeneration = { checked: false, generation: 0 };
      const runtime = await createWatcherDurableRuntime({
        backend,
        policy,
        authenticationKey: key,
        client: {
          ...sidecar.value,
          readCurrent: async () => {
            const head = await sidecar.value.readCurrent();
            if (sourceGeneration.checked) sourceGeneration.generation = 1;
            return head;
          },
        },
      });
      options.refuseCas = true;
      await expect(
        runtime.persistCanonicalProgress(capture.localObservation),
      ).rejects.toThrow("CAS conflicted");
      expect(runtime.readFinality().phase).toBe("unobserved");
      expect(sidecar.current()?.revision).toBe("0");
      options.refuseCas = false;
      await runtime.reconcile!();
      expect(runtime.readFinality().phase).toBe("pending");
      expect(sidecar.current()?.revision).toBe("1");
      const fresh = await fixture.observeFresh();
      const before = await backend.read();
      const protectedHead = sidecar.current();
      await expect(
        runtime.persistCanonicalProgress({
          ...fresh.localObservation,
          assertCurrent: () => {
            if (sourceGeneration.generation !== 0)
              throw new Error("native generation revoked");
            sourceGeneration.checked = true;
          },
        }),
      ).rejects.toThrow("native generation revoked");
      expect(await backend.read()).toEqual(before);
      expect(sidecar.current()).toEqual(protectedHead);
      expect(runtime.readFinality().phase).toBe("pending");
    } finally {
      await fixture.close();
    }
  }, 60_000);
});
