import { mkdtemp, rm, writeFile } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";

import { Duration, Effect, Either, Fiber, Ref, Schedule } from "effect";
import { afterEach, describe, expect, it } from "vitest";

import {
  awaitNodeNetworkMagic,
  withNodeNetworkMagic,
} from "../src/services/l1-follower.network-magic.js";
import {
  L1_FOLLOWER_NOT_STARTED,
  L1_NODE_CONFIG_FAILED,
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
    /** Writes a node config that does not parse. */
    writeBrokenConfig: async () => {
      await writeFile(configPath, "{ not json");
    },
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

  /** Runs `read` against a fresh follower state; returns the outcome and the state. */
  const withState = <A, E>(
    read: (L1_FOLLOWER: Ref.Ref<L1FollowerState>) => Effect.Effect<A, E, never>,
  ) =>
    Effect.runPromise(
      Effect.gen(function* () {
        const L1_FOLLOWER = yield* Ref.make<L1FollowerState>(
          L1_FOLLOWER_NOT_STARTED,
        );
        const outcome = yield* Effect.either(read(L1_FOLLOWER));
        return { outcome, state: yield* Ref.get(L1_FOLLOWER) };
      }),
    );

  it("fails under l1_node_config_failed once the config stays missing past the budget", async () => {
    const node = await nodeDirectory();
    const { outcome, state } = await withState((L1_FOLLOWER) =>
      awaitNodeNetworkMagic({
        globals: { L1_FOLLOWER },
        nodeConfigPath: node.configPath,
        network: "Custom",
        schedule: Schedule.spaced(Duration.millis(10)),
        budget: Duration.millis(80),
      }),
    );
    expect(Either.isLeft(outcome)).toBe(true);
    expect(state).toMatchObject({
      kind: "failed",
      reason: L1_NODE_CONFIG_FAILED,
    });
    expect(state.kind === "failed" && state.detail).toContain(
      "did not appear within the L1 node budget",
    );
    expect(l1FollowerReadiness(state).reasons).toEqual([L1_NODE_CONFIG_FAILED]);
  });

  it("fails at once, with no retry, on a config that does not parse", async () => {
    const node = await nodeDirectory();
    await node.writeBrokenConfig();
    const { outcome, state } = await withState((L1_FOLLOWER) =>
      awaitNodeNetworkMagic({
        globals: { L1_FOLLOWER },
        nodeConfigPath: node.configPath,
        network: "Custom",
        schedule: Schedule.spaced(Duration.millis(10)),
        budget: Duration.minutes(10),
      }).pipe(
        Effect.timeoutFail({
          duration: Duration.seconds(2),
          onTimeout: () => new Error("retried without bound"),
        }),
      ),
    );
    expect(
      Either.isLeft(outcome) && String(outcome.left).includes("without bound"),
    ).toBe(false);
    expect(Either.isLeft(outcome)).toBe(true);
    expect(state).toMatchObject({
      kind: "failed",
      reason: L1_NODE_CONFIG_FAILED,
    });
    expect(state.kind === "failed" && state.detail).toContain(
      "a failure waiting does not fix",
    );
  });

  it("does not start the follower on a config that does not parse, and names why", async () => {
    const node = await nodeDirectory();
    await node.writeBrokenConfig();
    let started = 0;
    const { outcome, state } = await withState((L1_FOLLOWER) =>
      Effect.scoped(
        withNodeNetworkMagic(
          {
            globals: { L1_FOLLOWER },
            nodeConfigPath: node.configPath,
            network: "Custom",
          },
          () =>
            Effect.sync(() => {
              started += 1;
            }),
        ),
      ),
    );
    expect(Either.isRight(outcome)).toBe(true);
    expect(started).toBe(0);
    expect(state).toMatchObject({
      kind: "failed",
      reason: L1_NODE_CONFIG_FAILED,
    });
  });
});
