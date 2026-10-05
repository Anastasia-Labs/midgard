import { describe, expect, it } from "vitest";

import {
  createWatcherLocalUserEventHistory,
  prepareWatcherLocalUserEventTransition,
  readWatcherLocalUserEventTransition,
} from "../../src/indexers/user-event-indexer.js";
import { readWatcherLocalBackfillFinality } from "../../src/l1/finality-engine.js";
import {
  createWatcherDurableRuntime,
  persistWatcherUserEventCheckpoint,
  readWatcherProtectedUserEventCheckpoint,
} from "../../src/storage/durable-runtime.js";
import {
  durableFixture,
  openOrigin,
} from "../support/local-user-event-authority-fixture.js";
import { createSyntheticUserEventOriginFixture } from "../support/user-event-origin-fixture.js";

describe("semantic checkpoint generation at CAS", () => {
  it("refuses a real candidate whose native lease expires during independent owner revalidation", async () => {
    const fixture = await createSyntheticUserEventOriginFixture();
    try {
      const { pair, input, origin } = await openOrigin(fixture);
      const durable = await durableFixture(
        readWatcherLocalBackfillFinality(pair.finality).policy,
      );
      let armed = false;
      let reads = 0;
      const runtime = await createWatcherDurableRuntime({
        ...durable.runtimeInput,
        client: {
          ...durable.runtimeInput.client,
          readCurrent: async () => {
            const head = await durable.runtimeInput.client.readCurrent();
            if (armed && ++reads === 3) await pair.close();
            return head;
          },
        },
      });
      const publication =
        await readWatcherProtectedUserEventCheckpoint(runtime);
      const history = createWatcherLocalUserEventHistory({
        ...input,
        origin,
        publication,
      });
      const transition = prepareWatcherLocalUserEventTransition({
        history,
        ...pair,
        publication,
      });
      const prepared = readWatcherLocalUserEventTransition(transition);
      for (const object of prepared.archiveObjects)
        await durable.archive.put(Buffer.from(object.bytesHex, "hex"));
      const before = await durable.runtimeInput.backend.read();
      const head = await durable.runtimeInput.client.readCurrent();
      armed = true;
      await expect(
        persistWatcherUserEventCheckpoint(runtime, {
          ...prepared,
          validationCandidate: transition,
        }),
      ).rejects.toThrow();
      expect(reads).toBeGreaterThanOrEqual(3);
      expect(() => readWatcherLocalUserEventTransition(transition)).toThrow();
      expect(await durable.runtimeInput.backend.read()).toEqual(before);
      expect(await durable.runtimeInput.client.readCurrent()).toEqual(head);
      await pair.close();
    } finally {
      await fixture.close();
    }
  }, 60_000);
});
