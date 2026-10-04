import { strict as assert } from "node:assert";
import { randomUUID } from "node:crypto";

import {
  startWatcherNativeChainSync,
  type WatcherNativeChainSyncEvent,
  type WatcherNativeChainSyncRuntime,
} from "midgard-watcher/native-chain-sync";

import { createHistoryWindowSealer } from "../../src/devnet-stack/history-native-window-proof.js";
import { makeLayout } from "../../src/devnet-stack/layout.js";
import { loadWatcherModule } from "../../src/devnet-stack/watcher-release.js";
import { windowFixture } from "./history-native-window-fixture.js";

const run = async () => {
  const f = windowFixture(0);
  let native: WatcherNativeChainSyncRuntime | undefined;
  try {
    const watcher = await loadWatcherModule(makeLayout(f.root));
    let resolveOpening!: (event: WatcherNativeChainSyncEvent) => void;
    const opening = new Promise<WatcherNativeChainSyncEvent>((resolve) => {
      resolveOpening = resolve;
    });
    const target = f.points[0];
    if (target === undefined) throw Error("synthetic compiled point absent");
    native = await watcher.startWatcherNativeChainSync({
      binaryPath: f.binaryPath,
      watcherConfig: watcher.parseWatcherConfig(f.watcherConfig),
      intersection: {
        kind: "point",
        blockHash: target.blockHash,
        slot: target.slot,
      },
      startupTimeoutMs: 10000,
      onEvent: async (event) => {
        resolveOpening(event);
      },
    });
    const event = await opening;
    const sealer = createHistoryWindowSealer({
      actor: {
        role: "history-recorder",
        runId: randomUUID(),
        deploymentFingerprint: "11".repeat(32),
        codeStamp: "22".repeat(32),
        serviceSpecsDigest: "33".repeat(32),
        attemptId: randomUUID(),
      },
      directories: f.directories,
      watcherConfig: f.watcherConfig,
      binaryPath: f.binaryPath,
    });
    await sealer.capture(event, target, 10000);
    assert.notEqual(
      sealer.seal(1000),
      null,
      "compiled helper accepts actual dynamically loaded watcher callback receipt",
    );
    assert.equal(
      startWatcherNativeChainSync,
      watcher.startWatcherNativeChainSync,
      "compiled static public native and actual recorder dynamic loader share native module identity",
    );
    console.log(
      "PASS compiled actual callback receipt + shared native module identity",
    );
  } finally {
    await native?.close();
    await f.close();
  }
};
void run().catch((error) => {
  console.error(error);
  process.exitCode = 1;
});
