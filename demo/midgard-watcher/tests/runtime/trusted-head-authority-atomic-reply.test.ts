import { expect, it } from "vitest";

import {
  createWatcherTrustedHeadAuthorityClient,
  openWatcherTrustedHeadAuthorityStore,
  startWatcherTrustedHeadAuthorityServer,
} from "../../src/runtime/trusted-head-authority.js";
import { authenticationKey, head } from "./trusted-head-authority.policy.js";
import { sqliteScene } from "./trusted-head-authority.sqlite-fixture.js";

it("returns the exact committed transactional head when a real second SQLite writer advances before the HTTP reply", async () => {
  const scene = await sqliteScene(1),
    second = await openWatcherTrustedHeadAuthorityStore(scene.input);
  const firstHead = head(scene.input.policy, 0, "77"),
    next = head(scene.input.policy, 1, "88");
  let secondAdvanced = false;
  const server = await startWatcherTrustedHeadAuthorityServer({
    endpoint: "http://127.0.0.1:0",
    httpSecret: "synthetic-atomic-authority-secret-only",
    unsafeAllowEphemeralPortForTest: true,
    store: {
      readRecordAuthenticationKeyId: scene.store.readRecordAuthenticationKeyId,
      readCurrent: scene.store.readCurrent,
      compareAndSwap: async (input) => {
        const original = await scene.store.compareAndSwap(input);
        if (original.committed) {
          expect(
            await second.compareAndSwap({
              expectedTrustedHead: firstHead,
              nextTrustedHead: next,
            }),
          ).toEqual({ committed: true, head: next });
          secondAdvanced = true;
        }
        return original;
      },
    },
  });
  try {
    const client = createWatcherTrustedHeadAuthorityClient({
      endpoint: server.endpoint,
      httpSecret: "synthetic-atomic-authority-secret-only",
      policy: scene.input.policy,
      authenticationKey,
      requestTimeoutMs: 1000,
    });
    expect(
      await client.compareAndSwap({
        expectedTrustedHead: null,
        nextTrustedHead: firstHead,
      }),
    ).toBe(true);
    expect(secondAdvanced).toBe(true);
    expect(await client.readCurrent()).toEqual(next);
  } finally {
    await server.close();
    second.close();
    scene.store.close();
  }
});
