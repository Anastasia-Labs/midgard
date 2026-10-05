import { mkdirSync, readFileSync, renameSync, writeFileSync } from "node:fs";
import { join } from "node:path";

import { computeFraudProofRawL1PointId } from "@al-ft/midgard-fault-proofs";
import { admitWatcherNativeRollForwardBlock } from "midgard-watcher";
import { expect, it } from "vitest";

import { HistoryConfigurationRefusal } from "../src/devnet-stack/history-configuration-refusal.js";
import { createHistoryChainFollower } from "../src/devnet-stack/watcher-history-chain.js";
import { windowFixture } from "./helpers/history-native-window-fixture.js";

it("restarts the strict production follower from its actual native genesis-short archive without resetting retained state", async () => {
  const source = windowFixture(0);
  try {
    const directories = ["writer-a", "writer-b"].map((role) => {
      const directory = join(source.root, role);
      mkdirSync(join(directory, "canonical"), { recursive: true });
      return directory;
    });
    const input = {
      directories,
      commitsDirectory: join(source.root, "writer-commits"),
      stateQueuePolicyId: "11".repeat(28),
      admit: admitWatcherNativeRollForwardBlock,
    };
    const writer = createHistoryChainFollower(input);
    expect(writer.intersectionCandidates).toEqual([{ kind: "origin" }]);
    let written = false;
    let failure: unknown;
    await source.startMain(async (event) => {
      try {
        await writer.onEvent(event);
        if (event.kind === "roll_forward") {
          expect(event.blockNo).toBe("0");
          expect(event.prevHash).toBe("");
          written = true;
        }
      } catch (error) {
        failure = error;
      }
    }, true);
    await expect
      .poll(() => written || failure !== undefined, { timeout: 10000 })
      .toBe(true);
    expect(failure).toBeUndefined();
    const persisted = directories.map((directory) =>
      readFileSync(join(directory, "canonical", "0.json"), "utf8"),
    );
    expect(persisted[0]).toBe(persisted[1]);
    const target = source.points[0];
    if (target === undefined) throw Error("actual native genesis point absent");
    expect(persisted[0]).toBe(JSON.stringify({ point: target, prevHash: "" }));
    const restarted = createHistoryChainFollower(input);
    expect(restarted.intersectionCandidates).toEqual([
      { kind: "point", blockHash: target.blockHash, slot: target.slot },
    ]);
    expect(
      directories.map((directory) =>
        readFileSync(join(directory, "canonical", "0.json"), "utf8"),
      ),
    ).toEqual(persisted);
    // The source protocol's empty-parent sentinel is specific to blockNo0.
    const wrong = {
      blockHash: target.blockHash,
      blockNo: "1",
      slot: target.slot,
    };
    for (const directory of directories) {
      const path = join(directory, "canonical", "1.json");
      renameSync(join(directory, "canonical", "0.json"), path);
      writeFileSync(
        path,
        JSON.stringify({
          point: { ...wrong, pointId: computeFraudProofRawL1PointId(wrong) },
          prevHash: "",
        }),
      );
    }
    expect(() => createHistoryChainFollower(input)).toThrow(
      HistoryConfigurationRefusal,
    );
  } finally {
    await source.close();
  }
}, 30000);
