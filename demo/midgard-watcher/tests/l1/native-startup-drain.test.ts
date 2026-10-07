import { startWatcherNativeChainSync } from "midgard-watcher/native-chain-sync";
import { expect, it } from "vitest";

import {
  config,
  INTERSECTION,
  readIdentityFixture,
  waitFor,
} from "./native-chain-sync.config.js";
import { fakeNodeTransport } from "./native-chain-sync.fake-transport.js";

// The node never answers the intersection, so each start is still pending
// when it is cancelled. A cancelled start must release its stream at the
// node: nothing is left open behind the rejection.

it("joins an aborted startup and preserves cancellation without an orphan stream", async () => {
  const transport = await fakeNodeTransport("no_ready");
  const controller = new AbortController();
  const started = startWatcherNativeChainSync({
    binaryPath: transport.binaryPath,
    watcherConfig: config(),
    intersection: INTERSECTION,
    startupTimeoutMs: 2000,
    signal: controller.signal,
    onEvent: async () => {},
    unsafeReadIdentityFileForTest: readIdentityFixture,
  });
  const reason = new Error("owned startup retired");
  const rejected = expect(started).rejects.toBe(reason);
  await waitFor(() => transport.journal().includes("step no_ready"));
  controller.abort(reason);
  await rejected;
  await waitFor(() => transport.journal().includes("closed"));
});

it("preserves an actual revocation failure over concurrent startup cancellation after drainage", async () => {
  const transport = await fakeNodeTransport("no_ready");
  const controller = new AbortController();
  const failure = new Error("owned revocation failure");
  const started = startWatcherNativeChainSync({
    binaryPath: transport.binaryPath,
    watcherConfig: config(),
    intersection: INTERSECTION,
    startupTimeoutMs: 2000,
    signal: controller.signal,
    onEvent: async () => {},
    onAuthorityRevoked: () => {
      throw failure;
    },
    unsafeReadIdentityFileForTest: readIdentityFixture,
  });
  const rejected = expect(started).rejects.toMatchObject({
    message: "native read lifetime revocation failed",
    cause: failure,
  });
  await waitFor(() => transport.journal().includes("step no_ready"));
  controller.abort(new Error("owned stop"));
  await rejected;
  await waitFor(() => transport.journal().includes("closed"));
});

it("releases the stream at the node when a started read closes", async () => {
  const transport = await fakeNodeTransport("honest");
  const runtime = await startWatcherNativeChainSync({
    binaryPath: transport.binaryPath,
    watcherConfig: config(),
    intersection: INTERSECTION,
    startupTimeoutMs: 2000,
    onEvent: async () => {},
    unsafeReadIdentityFileForTest: readIdentityFixture,
  });
  await runtime.close();
  await runtime.done;
  await waitFor(() => transport.journal().includes("closed"));
});
