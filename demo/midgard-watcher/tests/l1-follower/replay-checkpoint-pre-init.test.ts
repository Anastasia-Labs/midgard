import { mkdtemp, rm } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";

import { NAMED_NETWORK_L1_SLOT_CONFIGS } from "@al-ft/midgard-core/l1-slot-config";
import type { LedgerCheckpoint } from "@al-ft/midgard-fault-proofs";
import type { OutRef } from "@al-ft/midgard-l1-follower";
import {
  SIM_ORIGIN,
  SimChain,
  type SimTx,
  simUniverse,
} from "@al-ft/midgard-l1-follower/testing";
import * as SDK from "@al-ft/midgard-sdk";
import { afterEach, describe, expect, it } from "vitest";

import { createWatcherReplayedLedger } from "../../src/fault-proofs/replayed-ledger.js";
import { readWatcherObservation } from "../../src/l1-follower/observation.js";
import { watcherReplayCheckpointPrecedesInit } from "../../src/l1-follower/replay-source.js";
import { advanceWatcherReplayCheckpoint } from "../../src/runtime/watcher-runtime.advance-replayed-ledger.js";
import {
  queueState,
  SIM_HUB_ORACLE_ONE_SHOT,
} from "../support/l1-follower-state-queue-traffic.js";
import {
  applyAll,
  AUTHORITY,
  D,
  openStore,
  RECOVERY_DEPTH,
  RELEASE_DEPTH,
  SOURCE_ID,
} from "../support/l1-follower-store-reset.js";
import {
  GENESIS,
  genesisInitTx,
  outRefLabel,
  replaySource,
} from "../support/replay-source-fixture.js";

/** The production replay checkpoint depth: automaticRecoveryMaxDepth + 2. */
const CHECKPOINT_DEPTH = RECOVERY_DEPTH + 2;

// Synthetic chain framing and the real follower projection; no Plutus execution.
const directories: string[] = [];
afterEach(async () => {
  for (const directory of directories.splice(0))
    await rm(directory, { recursive: true, force: true });
});

/**
 * A store following the deployment (watching its hub-oracle one-shot, as
 * `followChain` does), fed one block per `forward` call.
 */
const followed = async () => {
  const store = openStore(":memory:");
  await store.start();
  await store.initialize(SIM_ORIGIN);
  store.watchProtocolInit(SIM_HUB_ORACLE_ONE_SHOT);
  const chain = new SimChain(simUniverse(), SIM_ORIGIN);
  const forward = async (...txs: SimTx[]) =>
    applyAll(store, [chain.forward(txs).event]);
  const empty = async (blocks: number) => {
    for (let index = 0; index < blocks; index += 1) await forward();
  };
  const directory = await mkdtemp(join(tmpdir(), "watcher-preinit-"));
  directories.push(directory);
  const source = replaySource(store, CHECKPOINT_DEPTH);
  let stored: LedgerCheckpoint | undefined;
  // A chain event landing once, right after the keeper's next root read.
  let afterRead: (() => Promise<void>) | undefined;
  const keeper = createWatcherReplayedLedger({
    follower: {
      ...source,
      confirmed: async (observation) => {
        const read = await source.confirmed(observation);
        const landing = afterRead;
        afterRead = undefined;
        await landing?.();
        return read;
      },
    },
    l1SlotConfig: NAMED_NETWORK_L1_SLOT_CONFIGS.Preprod,
    availabilityAuthority: () => ({
      deploymentIdentity: D.hubOracleMint,
      responseGeometry: SDK.availabilityResponseGeometry({
        chunkByteLength: 4096,
        trancheByteLength: 64 * 1024,
        maxTrancheCount: 1024,
      }),
    }),
    checkpointPath: join(directory, "ledger.json"),
    unsafeCheckpointStoreForTest: {
      read: async () => stored,
      write: async (checkpoint) => {
        stored = checkpoint;
      },
    },
  });
  const observe = (depth: number) =>
    readWatcherObservation(store, {
      authority: AUTHORITY,
      sourceId: SOURCE_ID,
      depth,
      releaseDepth: RELEASE_DEPTH,
    });
  const precedesInit = (depth: number) =>
    watcherReplayCheckpointPrecedesInit(store, {
      hubOracleOneShotOutRef: outRefLabel(SIM_HUB_ORACLE_ONE_SHOT),
      depth,
    });
  /** The replay checkpoint's readiness reasons, as the decision driver reads them. */
  const readiness = async () =>
    advanceWatcherReplayCheckpoint(
      {
        releaseDepth: RELEASE_DEPTH,
        replayedLedger: {
          depth: CHECKPOINT_DEPTH,
          advance: (observation) => keeper.advance(observation, []),
          checkpointPrecedesInit: precedesInit,
        },
      },
      await observe(RELEASE_DEPTH),
      observe,
      () => undefined,
    );
  const checkpointObservation = async () => {
    const read = await observe(CHECKPOINT_DEPTH);
    if (read.kind !== "ok")
      throw new Error(`observation: ${read.reason}: ${read.detail}`);
    return read.observation;
  };
  const close = async () => {
    source.close();
    await store.close();
  };
  return {
    store,
    chain,
    forward,
    empty,
    source,
    readiness,
    checkpointObservation,
    /** The ledger checkpoint the keeper persisted, if any. */
    persisted: () => stored,
    precedesInit,
    afterNextRead: (landing: () => Promise<void>) => {
      afterRead = landing;
    },
    close,
  };
};

/** A plain payment spending `input`, creating nothing the projection reads. */
const spend = (chain: SimChain, input: OutRef): SimTx => ({
  inputs: [input],
  outputs: [{ address: simUniverse().untrackedAddress, lovelace: 1_000_000n }],
  nonce: chain.nonce(),
});

describe("a replay checkpoint point below the deployment's protocol init", () => {
  it("is ready from genesis while the init is shallower than the checkpoint, then reads the root once the init is deep", async () => {
    const f = await followed();
    try {
      // The checkpoint point is the first block, three below the init block.
      await f.empty(3);
      await f.forward(genesisInitTx());
      await f.empty(CHECKPOINT_DEPTH - 4);
      const observation = await f.checkpointObservation();
      expect(observation.finalizedQueue).toEqual([]);
      const confirmed = await f.source.confirmed(observation);
      expect(confirmed.state).toBeNull();
      expect(confirmed.current()).toBe(true);
      expect(await f.readiness()).toEqual([]);
      // Once the init block is the checkpoint point, the root is read as before.
      await f.empty(3);
      const deep = await f.source.confirmed(await f.checkpointObservation());
      expect(deep.state).toEqual(GENESIS);
      expect(await f.readiness()).toEqual([]);
    } finally {
      await f.close();
    }
  });

  it.each([
    {
      name: "no root anywhere",
      build: async (f: Awaited<ReturnType<typeof followed>>) => {
        await f.empty(CHECKPOINT_DEPTH + 2);
      },
    },
    {
      name: "a root after the point whose tx does not spend the deployment's one-shot",
      build: async (f: Awaited<ReturnType<typeof followed>>) => {
        await f.empty(3);
        await f.forward(genesisInitTx(f.chain.outsideInput()));
        await f.empty(CHECKPOINT_DEPTH - 4);
      },
    },
    {
      name: "the deployment's one-shot spent after the point by a tx that creates no root",
      build: async (f: Awaited<ReturnType<typeof followed>>) => {
        await f.empty(3);
        await f.forward(spend(f.chain, SIM_HUB_ORACLE_ONE_SHOT));
        await f.empty(CHECKPOINT_DEPTH - 4);
      },
    },
    {
      name: "the deployment's one-shot spent at or before the point, and a root after it",
      build: async (f: Awaited<ReturnType<typeof followed>>) => {
        // The spend lands in the checkpoint block itself.
        await f.forward(spend(f.chain, SIM_HUB_ORACLE_ONE_SHOT));
        await f.empty(2);
        await f.forward(genesisInitTx(f.chain.outsideInput()));
        await f.empty(CHECKPOINT_DEPTH - 4);
      },
    },
    {
      name: "the deployment's queue deinitialised before the point",
      build: async (f: Awaited<ReturnType<typeof followed>>) => {
        await f.forward(genesisInitTx());
        const live = queueState(f.chain, D);
        if (live === null)
          throw new Error("the model queue is not initialized");
        // Root and CorrectionLock spent, as a Deinit leaves the queue.
        await f.forward({
          inputs: [live.ordered[0]!.outRef, live.lock.outRef],
          outputs: [
            { address: simUniverse().untrackedAddress, lovelace: 1_000_000n },
          ],
          nonce: f.chain.nonce(),
        });
        // The checkpoint point is the block after that spend.
        await f.empty(CHECKPOINT_DEPTH);
      },
    },
    {
      name: "a CorrectionLock live at the point",
      build: async (f: Awaited<ReturnType<typeof followed>>) => {
        const stray = genesisInitTx(f.chain.outsideInput());
        await f.forward({ ...stray, outputs: [stray.outputs[1]!] });
        // The checkpoint point is the block after the stray lock.
        await f.empty(2);
        await f.forward(genesisInitTx());
        await f.empty(CHECKPOINT_DEPTH - 3);
      },
    },
  ])(
    "stays pending with zero roots at the point and $name",
    async ({ build }) => {
      const f = await followed();
      try {
        await build(f);
        expect((await f.checkpointObservation()).finalizedQueue).toEqual([]);
        await expect(
          f.source.confirmed(await f.checkpointObservation()),
        ).rejects.toThrow(
          "replay checkpoint has no unique confirmed-state root",
        );
        expect(await f.readiness()).toEqual([
          {
            reason: "replayed_ledger_source_pending",
            detail: "replay checkpoint has no unique confirmed-state root",
          },
        ]);
      } finally {
        await f.close();
      }
    },
  );

  it("stays pending with two roots at the point", async () => {
    const f = await followed();
    try {
      await f.forward(genesisInitTx());
      // A second root output, alone: no second CorrectionLock or hub oracle.
      const second = genesisInitTx(f.chain.outsideInput());
      await f.forward({ ...second, outputs: [second.outputs[2]!] });
      await f.empty(CHECKPOINT_DEPTH);
      expect(await f.readiness()).toEqual([
        {
          reason: "replayed_ledger_stable_observation_pending",
          detail: expect.stringMatching(/^duplicate_root: /u),
        },
      ]);
    } finally {
      await f.close();
    }
  });

  it("stays pending when a rewind lands while a pre-init checkpoint is read", async () => {
    const f = await followed();
    try {
      await f.empty(3);
      await f.forward(genesisInitTx());
      await f.empty(CHECKPOINT_DEPTH - 5);
      const target = f.chain.tip.point;
      await f.empty(1);
      f.afterNextRead(async () => {
        expect((await f.store.rewind(target)).kind).toBe("rewound");
      });
      expect(await f.readiness()).toEqual([
        {
          reason: "replayed_ledger_source_pending",
          detail: "replay checkpoint changed while it was read",
        },
      ]);
    } finally {
      await f.close();
    }
  });
});

describe("a replay checkpoint deeper than the follower's origin", () => {
  it("is ready from genesis while the deployment's init followed the origin, then reads the root once the init is deep", async () => {
    const f = await followed();
    try {
      // The init is the first block after the origin, so every checkpoint
      // point until it is CHECKPOINT_DEPTH deep lies at or below the origin.
      await f.forward(genesisInitTx());
      for (let blocks = 1; blocks < CHECKPOINT_DEPTH; blocks += 1) {
        await expect(f.checkpointObservation()).rejects.toThrow(
          "l1_follower_short_chain",
        );
        expect(await f.precedesInit(CHECKPOINT_DEPTH)).toBe(true);
        expect(await f.readiness()).toEqual([]);
        await f.empty(1);
      }
      expect(f.persisted()).toBeUndefined();
      // The init block is now the checkpoint point, above the origin.
      expect(await f.precedesInit(CHECKPOINT_DEPTH)).toBe(false);
      const deep = await f.source.confirmed(await f.checkpointObservation());
      expect(deep.state).toEqual(GENESIS);
      expect(await f.readiness()).toEqual([]);
    } finally {
      await f.close();
    }
  });

  it.each([
    {
      name: "no init after the origin",
      build: async (f: Awaited<ReturnType<typeof followed>>) => {
        await f.empty(CHECKPOINT_DEPTH - 1);
      },
    },
    {
      name: "a root after the origin whose tx does not spend the deployment's one-shot",
      build: async (f: Awaited<ReturnType<typeof followed>>) => {
        await f.forward(genesisInitTx(f.chain.outsideInput()));
        await f.empty(CHECKPOINT_DEPTH - 2);
      },
    },
    {
      name: "the deployment's one-shot spent after the origin by a tx that creates no root",
      build: async (f: Awaited<ReturnType<typeof followed>>) => {
        await f.forward(spend(f.chain, SIM_HUB_ORACLE_ONE_SHOT));
        await f.empty(1);
        await f.forward(genesisInitTx(f.chain.outsideInput()));
        await f.empty(CHECKPOINT_DEPTH - 4);
      },
    },
  ])("stays pending with $name", async ({ build }) => {
    const f = await followed();
    try {
      await build(f);
      expect(await f.readiness()).toEqual([
        {
          reason: "replayed_ledger_stable_observation_pending",
          detail: `l1_follower_short_chain: the follower holds fewer than ${CHECKPOINT_DEPTH.toString()} blocks after its origin`,
        },
      ]);
    } finally {
      await f.close();
    }
  });
});
