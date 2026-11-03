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
import { Data } from "@lucid-evolution/lucid";
import { afterEach, describe, expect, it } from "vitest";

import { createWatcherReplayedLedger } from "../../src/fault-proofs/replayed-ledger.js";
import { readWatcherObservation } from "../../src/l1-follower/observation.js";
import { createWatcherReplayRetention } from "../../src/l1-follower/replay-retention.js";
import { createWatcherFollowerReplaySource } from "../../src/l1-follower/replay-source.js";
import { advanceWatcherReplayCheckpoint } from "../../src/runtime/watcher-runtime.advance-replayed-ledger.js";
import {
  applyTx,
  attestTx,
  commitTx,
  initTx,
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

/** The production replay checkpoint depth: automaticRecoveryMaxDepth + 2. */
const CHECKPOINT_DEPTH = RECOVERY_DEPTH + 2;

const outRefLabel = (outRef: OutRef) =>
  `${outRef.txHash.toString("hex")}#${outRef.index.toString()}`;

/** The follower's replay port over `store`, for its queue reads only. */
const replaySource = (
  store: ReturnType<typeof openStore>,
  minimumCheckpointDepth = RELEASE_DEPTH,
) =>
  createWatcherFollowerReplaySource({
    store,
    identity: {
      deploymentManifestId: AUTHORITY.deploymentFingerprint,
      blueprintHash: "00".repeat(32),
      network: "Preprod",
    },
    scripts: {
      depositPolicyId: "01".repeat(28),
      withdrawalPolicyId: "02".repeat(28),
      forcedOrderPolicyId: "03".repeat(28),
      forcedOrderAddressHex: "70" + "04".repeat(28),
    },
    userEvents: {
      deploymentManifestId: AUTHORITY.deploymentFingerprint,
      blueprintHash: "00".repeat(32),
      eventAuthority: async () => {
        throw new Error("checkpoint read does not read events");
      },
      headerFence: async () => {
        throw new Error("checkpoint read does not capture events");
      },
      close: () => undefined,
    },
    minimumCheckpointDepth,
    observationAuthority: {
      ...AUTHORITY,
      hubOracleOneShotOutRef: outRefLabel(SIM_HUB_ORACLE_ONE_SHOT),
    },
    retention: createWatcherReplayRetention({
      slotConfig: { zeroTime: 0, zeroSlot: 0, slotLength: 1000 },
      eventWaitMs: 1n,
      maximumAdmissionRangeMs: 1n,
      maximumProtectionWaitMs: 0n,
    }),
  });

const GENESIS = SDK.makeGenesisConfirmedState(1000n);

/** The protocol init whose root carries the genesis ConfirmedState. */
const genesisInitTx = (oneShot: OutRef = SIM_HUB_ORACLE_ONE_SHOT): SimTx => {
  const initial = initTx(D, undefined, oneShot);
  return {
    ...initial,
    outputs: initial.outputs.map((output, index) =>
      index === 2
        ? {
            ...output,
            datum: Buffer.from(
              Data.to(
                SDK.nodeViewToLinkedListDatum({
                  key: "Empty",
                  next: "Empty",
                  data: Data.from(Data.to(GENESIS, SDK.ConfirmedState)),
                }),
                SDK.LinkedListDatum,
              ),
              "hex",
            ),
          }
        : output,
    ),
  };
};

// Synthetic chain framing and the real follower projection; no Plutus execution.
it("reads the confirmed root from the admitted queue point and revokes it on a rewind", async () => {
  const store = openStore(":memory:");
  await store.start();
  await store.initialize(SIM_ORIGIN);
  const chain = new SimChain(simUniverse(), SIM_ORIGIN);
  const event = chain.forward([genesisInitTx()]).event;
  let rewindPoint = chain.tip.point;
  const events = [event];
  for (let index = 0; index < 8; index += 1) {
    events.push(chain.forward([]).event);
    if (index === 5) rewindPoint = chain.tip.point;
  }
  await applyAll(store, events);
  const input = {
    authority: AUTHORITY,
    sourceId: SOURCE_ID,
    depth: RELEASE_DEPTH,
    releaseDepth: RELEASE_DEPTH,
  };
  const observed = await readWatcherObservation(store, input);
  if (observed.kind !== "ok")
    throw new Error(`observation: ${observed.reason}`);
  const source = replaySource(store);
  try {
    const confirmed = await source.confirmed(observed.observation);
    expect(confirmed.state).toEqual(GENESIS);
    expect(confirmed.current()).toBe(true);
    expect((await store.rewind(rewindPoint)).kind).toBe("rewound");
    expect(confirmed.current()).toBe(false);
    await expect(source.confirmed(observed.observation)).rejects.toThrow(
      "checkpoint observation left the canonical chain",
    );
  } finally {
    source.close();
    await store.close();
  }
});

it("reads a header's availability status from its latest queue node", async () => {
  const store = openStore(":memory:");
  await store.start();
  await store.initialize(SIM_ORIGIN);
  const chain = new SimChain(simUniverse(), SIM_ORIGIN);
  const state = () => {
    const live = queueState(chain, D);
    if (live === null) throw new Error("the model queue is not initialized");
    return live;
  };
  await applyAll(store, [chain.forward([initTx(D)]).event]);
  await applyAll(store, [chain.forward([commitTx(state(), D)]).event]);
  const header = state().tailHeaderHash as string;
  const source = replaySource(store);
  try {
    expect(await source.availabilityStatus(header)).toBe("Unattested");
    await applyAll(store, [chain.forward([attestTx(state(), D)]).event]);
    const attestation = state().attestations.get(header);
    if (attestation === undefined)
      throw new Error("the header is not attested");
    await applyAll(store, [
      chain.forward([applyTx(state().tail, attestation, header, D, true)])
        .event,
    ]);
    expect(await source.availabilityStatus(header)).toMatchObject({
      Attested: { commitment_hash: expect.stringMatching(/^[0-9a-f]{64}$/) },
    });
    await expect(source.availabilityStatus("ee".repeat(28))).rejects.toThrow(
      "authenticated admission is unavailable",
    );
    source.close();
    await expect(source.availabilityStatus(header)).rejects.toThrow(
      "the follower replay source is closed",
    );
  } finally {
    source.close();
    await store.close();
  }
});

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
  const keeper = createWatcherReplayedLedger({
    follower: source,
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
  /** The replay checkpoint's readiness reasons, as the decision driver reads them. */
  const readiness = async () =>
    advanceWatcherReplayCheckpoint(
      {
        releaseDepth: RELEASE_DEPTH,
        replayedLedger: {
          depth: CHECKPOINT_DEPTH,
          advance: (observation) => keeper.advance(observation, []),
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
});
