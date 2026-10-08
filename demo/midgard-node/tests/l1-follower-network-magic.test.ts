import { mkdtemp, rm, writeFile } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";

import { Duration, Effect, Fiber, Ref, Schedule } from "effect";
import { afterEach, describe, expect, it } from "vitest";

import { awaitNodeNetworkMagic } from "../src/services/l1-follower.network-magic.js";
import {
  L1_FOLLOWER_NOT_STARTED,
  L1_NODE_CONFIG_UNREADABLE,
  l1FollowerReadiness,
  type L1FollowerState,
} from "../src/services/l1-follower.readiness.js";
import { nativeLedgerNetworkMagic } from "../src/services/native-ledger.js";

const MAGIC = 4242;
const directories: string[] = [];

afterEach(async () => {
  await Promise.all(
    directories
      .splice(0)
      .map((directory) => rm(directory, { recursive: true, force: true })),
  );
});

/** A node directory holding nothing yet: no config, no genesis, no socket. */
const nodeDirectory = async () => {
  const directory = await mkdtemp(join(tmpdir(), "midgard-node-magic-"));
  directories.push(directory);
  const configPath = join(directory, "config.json");
  return {
    configPath,
    socketPath: join(directory, "node.socket"),
    /** Writes the node config and the Shelley genesis it names. */
    writeConfig: async () => {
      await writeFile(
        join(directory, "shelley-genesis.json"),
        JSON.stringify({ networkMagic: MAGIC }),
      );
      await writeFile(
        configPath,
        JSON.stringify({ ShelleyGenesisFile: "shelley-genesis.json" }),
      );
    },
  };
};

describe("the node's network magic", () => {
  it("is read from the config files while the node's socket does not exist yet", async () => {
    const node = await nodeDirectory();
    await node.writeConfig();
    await expect(
      nativeLedgerNetworkMagic({ nodeConfigPath: node.configPath }, "Custom"),
    ).resolves.toBe(MAGIC);
  });

  it("holds l1_node_config_unreadable while the config is missing and reads it once it appears", async () => {
    const node = await nodeDirectory();
    const result = await Effect.runPromise(
      Effect.gen(function* () {
        const L1_FOLLOWER = yield* Ref.make<L1FollowerState>(
          L1_FOLLOWER_NOT_STARTED,
        );
        const waiting = yield* Effect.fork(
          awaitNodeNetworkMagic({
            globals: { L1_FOLLOWER },
            nodeConfigPath: node.configPath,
            network: "Custom",
            schedule: Schedule.spaced(Duration.millis(10)),
          }),
        );
        yield* Effect.sleep(Duration.millis(100));
        const held = yield* Ref.get(L1_FOLLOWER);
        yield* Effect.promise(() => node.writeConfig());
        const magic = yield* Fiber.join(waiting);
        return { held, magic };
      }),
    );
    expect(result.held).toMatchObject({
      kind: "waiting",
      reason: L1_NODE_CONFIG_UNREADABLE,
    });
    expect(l1FollowerReadiness(result.held).reasons).toEqual([
      L1_NODE_CONFIG_UNREADABLE,
    ]);
    expect(result.magic).toBe(MAGIC);
  });
});
