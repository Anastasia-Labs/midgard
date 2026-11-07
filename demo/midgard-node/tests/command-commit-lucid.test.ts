/**
 * A command's commit program builds with the command's own Lucid (option E):
 * `reconcile local-finalization` replays the commit worker program in the
 * CLI process, and passes it `environmentCommitLucidFactory`, the Lucid the
 * command's layers provide; here the node-ledger tool adapter over the local
 * node stands for them. The role Lucid over the same node is the control: it
 * is the follower adapter, so the access kind tells them apart.
 * The commit worker's role factory builds its Lucid in the worker run's
 * scope, so the access stays open until the run ends.
 */
import { MIDGARD_CONSENSUS_PROFILE } from "@al-ft/midgard-core/consensus-profile";
import * as LE from "@lucid-evolution/lucid";
import { Effect, Layer } from "effect";
import {
  afterAll,
  afterEach,
  beforeAll,
  describe,
  expect,
  it,
  vi,
} from "vitest";

import { ToolL1AdapterLive } from "../src/commands/l1-tool-adapter.js";
import { l1AccessOf, openL1Access } from "../src/l1-access.js";
import { l1SlotNow } from "../src/l1-heads.js";
import { NodeConfig } from "../src/services/config.js";
import type { NodeConfigDep } from "../src/services/config.node-config-dep.js";
import { FollowerL1AdapterLive } from "../src/services/l1-adapter.follower.js";
import { L1Adapter } from "../src/services/l1-adapter.js";
import { Lucid } from "../src/services/lucid.js";
import {
  ContractDeploymentIdentity,
  MidgardContracts,
} from "../src/services/midgard-contracts.js";
import { environmentCommitLucidFactory } from "../src/workers/commit-block-header.js";
import { scopedCommitLucidFactory } from "../src/workers/commit-block-header.pending-user-event-counts-up-to.js";
import {
  type FakeLocalNode,
  fakeLocalNode,
} from "./helpers/fake-local-node.js";
import { loadRealMidgardContractsForTest } from "./helpers/real-midgard-contracts.js";

const seeds = [0, 1, 2].map(() => LE.generateSeedPhrase());
const address = (seed: string) =>
  LE.walletFromSeed(seed, { network: "Custom" }).address;

let node: FakeLocalNode;
let config: NodeConfigDep;
let contracts: Layer.Layer<MidgardContracts | ContractDeploymentIdentity>;

beforeAll(async () => {
  node = await fakeLocalNode();
  await node.answer("answer");
  config = {
    NETWORK: "Custom",
    L1_NATIVE_LEDGER: node.nativeLedger,
    L1_NODE_BEHIND_MAX_MS: 60_000,
    L1_OPERATOR_SEED_PHRASE: seeds[0],
    L1_OPERATOR_SEED_PHRASE_FOR_MERGE_TX: seeds[1],
    L1_REFERENCE_SCRIPT_SEED_PHRASE: seeds[2],
    L1_REFERENCE_SCRIPT_ADDRESS: address(seeds[2]!),
    L1_REFERENCE_SCRIPT_DEPLOY_ADDRESS: address(seeds[2]!),
    // The follower store is opened lazily; nothing here reads it.
    POSTGRES_HOST: "127.0.0.1",
    POSTGRES_PORT: 1,
    POSTGRES_USER: "unused",
    POSTGRES_PASSWORD: "unused",
    POSTGRES_DB: "unused",
  } as unknown as NodeConfigDep;
  const deployed = await loadRealMidgardContractsForTest({
    txHash: "11".repeat(32),
    outputIndex: 0,
  });
  contracts = Layer.mergeAll(
    Layer.succeed(
      MidgardContracts,
      MidgardContracts.make({
        ...deployed,
        consensusProfile: MIDGARD_CONSENSUS_PROFILE,
      }),
    ),
    Layer.succeed(
      ContractDeploymentIdentity,
      ContractDeploymentIdentity.make({
        kind: "derived",
        consensusProfile: MIDGARD_CONSENSUS_PROFILE,
      }),
    ),
  );
});

afterAll(async () => {
  await node.remove();
});

/** A role adapter whose access counts its closes. */
const closingAdapter = (closes: { count: number }) =>
  Layer.succeed(L1Adapter, {
    role: "follower",
    open: () =>
      Effect.acquireRelease(
        Effect.sync(() => {
          const point = { slot: 5, id: "00".repeat(32) };
          return openL1Access({
            kind: "follower",
            provider: new LE.Emulator([]),
            endpoint: "/ipc/node.socket",
            slotConfig: async () => LE.SLOT_CONFIG_NETWORK.Preview,
            tipSlot: async () => {
              if (closes.count > 0) throw new Error("the access is closed");
              return 5;
            },
            viewPoint: async () => point,
            synchronizedViewPoint: async () => point,
            submitSlotSnapshot: async () => {
              throw new Error("no submit slot in this test");
            },
            close: async () => {
              closes.count += 1;
            },
          });
        }),
        (access) => Effect.promise(access.close),
      ),
  });

const lucidOver = (adapter: Layer.Layer<L1Adapter>) =>
  Lucid.DefaultWithoutDependencies.pipe(
    Layer.provide(adapter),
    Layer.provide(Layer.succeed(NodeConfig, config)),
  );

describe("a command's commit Lucid", () => {
  afterEach(() => {
    vi.unstubAllEnvs();
  });

  it("is the command's tool Lucid under L1_ACCESS=node, over the local node's ledger", async () => {
    vi.stubEnv("L1_ACCESS", "node");
    const { commit, service } = await Effect.runPromise(
      Effect.gen(function* () {
        const factory = yield* environmentCommitLucidFactory;
        return { commit: yield* factory(), service: yield* Lucid };
      }).pipe(Effect.provide(lucidOver(ToolL1AdapterLive)), Effect.scoped),
    );
    expect(commit).toBe(service);
    expect(l1AccessOf(commit.api)?.kind).toBe("node");
    expect(l1AccessOf(commit.api)?.endpoint).toBe(node.nativeLedger.socketPath);
  });

  it("the role Lucid over the same node is the follower adapter (the kind's control)", async () => {
    for (const key of [
      "L1_PROVIDER",
      "L1_KUPO_URL",
      "L1_OGMIOS_URL",
      "L1_BLOCKFROST_URL",
      "L1_BLOCKFROST_PROJECT_ID",
    ])
      vi.stubEnv(key, "");
    vi.stubEnv("L1_ACCESS", "follower");
    const kind = await Effect.runPromise(
      Effect.map(Lucid, (lucid) => l1AccessOf(lucid.api)?.kind).pipe(
        Effect.provide(
          lucidOver(FollowerL1AdapterLive.pipe(Layer.provide(contracts))),
        ),
        Effect.scoped,
      ),
    );
    expect(kind).toBe("follower");
  });

  it("the commit worker's factory keeps the follower access open for the run", async () => {
    const closes = { count: 0 };
    const slot = await Effect.runPromise(
      Effect.gen(function* () {
        const factory = yield* scopedCommitLucidFactory(
          lucidOver(closingAdapter(closes)),
        );
        const lucid = yield* factory();
        return yield* l1SlotNow(lucid.api);
      }).pipe(Effect.scoped),
    );
    expect(slot).toBe(5);
    expect(closes.count).toBe(1);
  });

  it("a Lucid provided out of its own scope reads through a closed access (the control)", async () => {
    const closes = { count: 0 };
    const lucid = await Effect.runPromise(
      Effect.provide(Lucid, lucidOver(closingAdapter(closes))),
    );
    expect(closes.count).toBe(1);
    const slot = await Effect.runPromise(Effect.either(l1SlotNow(lucid.api)));
    expect(slot._tag).toBe("Left");
  });
});
