import { randomUUID } from "node:crypto";
import { mkdtemp, rm } from "node:fs/promises";
import { join } from "node:path";

import { afterEach, expect, it } from "vitest";

import {
  initializeSelectedAuthorityStore,
  openWatcherTrustedHeadAuthorityStore,
} from "../../src/runtime/trusted-head-authority.js";
import {
  head,
  policy,
  recordAuthenticationKey,
} from "./trusted-head-authority.policy.js";
const cleanup: string[] = [];
afterEach(async () => {
  for (const p of cleanup.splice(0))
    await rm(p, { recursive: true, force: true });
});
it.each([1, 2, 8, 64])(
  "persists selected K=%s and returns atomic exact CAS acknowledgements through repeated retirement",
  async (liveRecordLimit) => {
    const parent = await mkdtemp("/var/tmp/midgard-sqlite-authority-");
    cleanup.push(parent);
    const input = {
      directory: join(parent, "selected"),
      policy: policy(),
      recordAuthenticationKey,
      liveRecordLimit,
      generation: `generation-${randomUUID()}`,
    };
    await expect(openWatcherTrustedHeadAuthorityStore(input)).rejects.toThrow();
    await initializeSelectedAuthorityStore(input);
    const store = await openWatcherTrustedHeadAuthorityStore(input);
    try {
      expect(await store.readCurrent()).toBeNull();
      let previous = null as ReturnType<typeof head> | null;
      for (let i = 0; i < liveRecordLimit + 3; i++) {
        const next = head(input.policy, i, "77");
        expect(
          await store.compareAndSwap({
            expectedTrustedHead: previous,
            nextTrustedHead: next,
          }),
        ).toEqual({ committed: true, head: next });
        expect(await store.readCurrent()).toEqual(next);
        previous = next;
      }
      const stale = head(input.policy, 0, "77");
      expect(
        await store.compareAndSwap({
          expectedTrustedHead: stale,
          nextTrustedHead: head(input.policy, 1, "77"),
        }),
      ).toEqual({ committed: false, head: previous });
    } finally {
      store.close();
    }
    const reopened = await openWatcherTrustedHeadAuthorityStore(input);
    reopened.close();
  },
);
