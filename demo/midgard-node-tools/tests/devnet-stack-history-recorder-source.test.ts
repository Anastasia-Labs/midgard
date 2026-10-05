import { randomUUID } from "node:crypto";
import { mkdirSync, readFileSync } from "node:fs";
import { join } from "node:path";

import { admitWatcherNativeRollForwardBlock } from "midgard-watcher";
import { expect, it } from "vitest";

import type { HistoryChildActor } from "../src/devnet-stack/history-child-evidence.js";
import { startHistoryRecorderSource } from "../src/devnet-stack/history-recorder-source.js";
import { createHistoryChainFollower } from "../src/devnet-stack/watcher-history-chain.js";
import { windowFixture } from "./helpers/history-native-window-fixture.js";

const setup = (last: number) => {
  const fixture = windowFixture(last);
  const directories = ["actual-writer-a", "actual-writer-b"].map((name) => {
    const directory = join(fixture.root, name);
    mkdirSync(join(directory, "canonical"), { recursive: true });
    return directory;
  });
  const input = {
    directories,
    commitsDirectory: join(fixture.root, "actual-writer-commits"),
    stateQueuePolicyId: "11".repeat(28),
    admit: admitWatcherNativeRollForwardBlock,
  };
  const actor: HistoryChildActor = {
    role: "history-recorder",
    runId: "actual-source-fixture",
    deploymentFingerprint: "1".repeat(64),
    codeStamp: "2".repeat(64),
    serviceSpecsDigest: "3".repeat(64),
    attemptId: randomUUID(),
    childPid: process.pid,
  };
  const start = () => {
    const chain = createHistoryChainFollower(input);
    return {
      chain,
      native: startHistoryRecorderSource({
        actor,
        directories,
        chain,
        watcherConfig: fixture.watcherConfig,
        binaryPath: fixture.binaryPath,
      }),
    };
  };
  return { fixture, directories, start };
};

it("uses the actual native producer, bounded full-window source and strict retained restart", async () => {
  const f = setup(2161);
  let source:
    | Awaited<ReturnType<typeof startHistoryRecorderSource>>
    | undefined;
  try {
    const setupStarted = performance.now();
    const started = f.start();
    source = await started.native;
    await expect
      .poll(() => started.chain.latestBlockNo(), {
        timeout: 180000,
        interval: 100,
      })
      .toBe(2161n);
    console.info(
      "actual full-window producer setup ms",
      Math.round(performance.now() - setupStarted),
    );
    const initial = source.sealer.seal(5000);
    expect(initial?.rowCount).toBe(2160);
    expect(initial?.first.blockNo).toBe("2");
    expect(initial?.last.blockNo).toBe("2161");
    await source.close();
    expect(source.sealer.seal(5000)).toBeNull();
    const physical = f.directories.map((directory) =>
      readFileSync(join(directory, "canonical", "2161.json"), "utf8"),
    );
    const restartStarted = performance.now();
    const restarted = f.start();
    expect(restarted.chain.intersectionCandidates[0]).toMatchObject({
      kind: "point",
      blockHash: f.fixture.points[2161]?.blockHash,
    });
    source = await restarted.native;
    await expect
      .poll(() => source?.sealer.seal(5000)?.rowCount, { timeout: 10000 })
      .toBe(2160);
    console.info(
      "actual full-window retained restart proof ms",
      Math.round(performance.now() - restartStarted),
    );
    expect(
      f.directories.map((directory) =>
        readFileSync(join(directory, "canonical", "2161.json"), "utf8"),
      ),
    ).toEqual(physical);
  } finally {
    await source?.close().catch(() => undefined);
    await f.fixture.close();
  }
}, 210000);

it("revokes before a live rewind and reacquires from the actual fork roll-forward without an Origin reset", async () => {
  const f = setup(2);
  let source:
    | Awaited<ReturnType<typeof startHistoryRecorderSource>>
    | undefined;
  try {
    const started = f.start();
    source = await started.native;
    await expect
      .poll(() => started.chain.latestBlockNo(), { timeout: 10000 })
      .toBe(2n);
    const old = source.sealer.seal(5000);
    expect(old).not.toBeNull();
    if (old === null) throw Error("actual native source did not seal");
    const parent = f.fixture.points[1];
    const replay = f.fixture.events[2];
    if (parent === undefined || replay === undefined)
      throw Error("synthetic native fork absent");
    f.fixture.send({
      schemaVersion: replay.schemaVersion,
      kind: "roll_backward",
      point: { kind: "point", blockHash: parent.blockHash, slot: parent.slot },
      tip: replay.tip,
    });
    await expect
      .poll(() => source?.sealer.revalidate(old.sealId, old.generation), {
        timeout: 10000,
      })
      .toBeNull();
    await expect
      .poll(() => started.chain.latestBlockNo(), { timeout: 10000 })
      .toBeUndefined();
    f.fixture.send(replay);
    await expect
      .poll(() => source?.sealer.seal(5000)?.last.blockNo, { timeout: 10000 })
      .toBe("2");
    expect(source.sealer.seal(5000)?.sourceEpoch).not.toBe(old.sourceEpoch);
    expect(source.sealer.revalidate(old.sealId, old.generation)).toBeNull();
  } finally {
    await source?.close().catch(() => undefined);
    await f.fixture.close();
  }
}, 30000);
