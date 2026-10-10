/**
 * A command's commit program builds with the command's own Lucid (option E):
 * `reconcile local-finalization` replays the commit worker program in the
 * CLI process, and passes it `environmentCommitLucidFactory`, the Lucid the
 * command's layers provide. Under `L1_ACCESS=node` that is the node-ledger
 * tool adapter, and the follower store is never opened. The role factory
 * over the follower adapter is the negative control: it does reach the
 * follower opener, so the spy can see an open.
 */
import * as LE from "@lucid-evolution/lucid";
import { Effect, Layer } from "effect";
import { afterEach, describe, expect, it, vi } from "vitest";

import { ToolL1AdapterLive } from "../src/commands/l1-tool-adapter.js";
import { l1AccessOf } from "../src/l1-access.js";
import { NodeConfig } from "../src/services/config.js";
import type { NodeConfigDep } from "../src/services/config.node-config-dep.js";
import { FollowerL1AdapterLive } from "../src/services/l1-adapter.js";
import { Lucid } from "../src/services/lucid.js";
import { environmentCommitLucidFactory } from "../src/workers/commit-block-header.js";

const opened = vi.hoisted(() => ({
  follower: 0,
  nodeLedger: [] as string[],
}));

vi.mock("../src/services/l1-provider.js", async (importOriginal) => ({
  ...(await importOriginal<typeof import("../src/services/l1-provider.js")>()),
  openNodeL1AccessFromConfig: async () => {
    opened.follower += 1;
    throw new Error("the follower store was opened");
  },
}));

vi.mock("../src/services/l1-node-ledger-access.js", async (importOriginal) => {
  const actual =
    await importOriginal<
      typeof import("../src/services/l1-node-ledger-access.js")
    >();
  const { openL1Access } = await import("../src/l1-access.js");
  const Lucid = await import("@lucid-evolution/lucid");
  return {
    ...actual,
    openNodeLedgerAccess: async (input: {
      nativeLedger: { socketPath: string };
    }) => {
      opened.nodeLedger.push(input.nativeLedger.socketPath);
      const point = { slot: 0, id: "00".repeat(32) };
      return openL1Access({
        kind: "node",
        provider: new Lucid.Emulator([]),
        endpoint: input.nativeLedger.socketPath,
        slotConfig: async () => Lucid.SLOT_CONFIG_NETWORK.Preview,
        tipSlot: async () => 0,
        viewPoint: async () => point,
        synchronizedViewPoint: async () => point,
        submitSlotSnapshot: async () => {
          throw new Error("no submit slot in this test");
        },
        close: async () => undefined,
      });
    },
  };
});

const seeds = [0, 1, 2].map(() => LE.generateSeedPhrase());
const address = (seed: string) =>
  LE.walletFromSeed(seed, { network: "Custom" }).address;

const config = {
  NETWORK: "Custom",
  L1_NATIVE_LEDGER: {
    socketPath: "/ipc/node.socket",
    nodeConfigPath: "/ipc/config.json",
    binaryPath: "/bin/l1-node-transport",
  },
  L1_NODE_BEHIND_MAX_MS: 60_000,
  L1_OPERATOR_SEED_PHRASE: seeds[0],
  L1_OPERATOR_SEED_PHRASE_FOR_MERGE_TX: seeds[1],
  L1_REFERENCE_SCRIPT_SEED_PHRASE: seeds[2],
  L1_REFERENCE_SCRIPT_ADDRESS: address(seeds[2]!),
  L1_REFERENCE_SCRIPT_DEPLOY_ADDRESS: address(seeds[2]!),
} as unknown as NodeConfigDep;

const lucidOver = (adapter: typeof ToolL1AdapterLive) =>
  Lucid.DefaultWithoutDependencies.pipe(
    Layer.provide(adapter),
    Layer.provide(Layer.succeed(NodeConfig, config)),
  );

describe("a command's commit Lucid", () => {
  afterEach(() => {
    vi.unstubAllEnvs();
    opened.follower = 0;
    opened.nodeLedger = [];
  });

  it("is the command's tool Lucid under L1_ACCESS=node, and opens no follower store", async () => {
    vi.stubEnv("L1_ACCESS", "node");
    const { commit, service } = await Effect.runPromise(
      Effect.gen(function* () {
        const factory = yield* environmentCommitLucidFactory;
        return { commit: yield* factory(), service: yield* Lucid };
      }).pipe(Effect.provide(lucidOver(ToolL1AdapterLive)), Effect.scoped),
    );
    expect(commit).toBe(service);
    expect(l1AccessOf(commit.api)?.kind).toBe("node");
    expect(opened.nodeLedger).toEqual(["/ipc/node.socket"]);
    expect(opened.follower).toBe(0);
  });

  it("the role adapter does open the follower store (the spy's control)", async () => {
    for (const key of [
      "L1_ACCESS",
      "L1_PROVIDER",
      "L1_KUPO_URL",
      "L1_OGMIOS_URL",
      "L1_BLOCKFROST_URL",
      "L1_BLOCKFROST_PROJECT_ID",
    ])
      vi.stubEnv(key, "");
    const failure = await Effect.runPromise(
      Lucid.pipe(
        Effect.provide(lucidOver(FollowerL1AdapterLive)),
        Effect.scoped,
        Effect.flip,
      ),
    );
    expect(failure.message).toMatch(/the follower store was opened/);
    expect(opened.follower).toBe(1);
    expect(opened.nodeLedger).toEqual([]);
  });
});
