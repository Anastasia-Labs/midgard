/**
 * The replayed ledger's advance holds a local replay fault under
 * `replayed_ledger_local_fault` and replays again on the next pass, but not
 * without bound: the `LOCAL_FAULT_RUN_LIMIT`th advance in a row toward one
 * confirmed head that fails that way, with no advance toward it completing
 * in between, fails with `WatcherReplayLocalFaultExhaustedError`, which ends
 * the watcher. An advance that completes starts the count over.
 */
import { mkdtemp, rm } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";

import { NAMED_NETWORK_L1_SLOT_CONFIGS } from "@al-ft/midgard-core/l1-slot-config";
import {
  type LedgerCheckpoint,
  LOCAL_FAULT_RUN_LIMIT,
  ReplayLocalFault,
  type RetainedDaPayloadSource,
} from "@al-ft/midgard-fault-proofs";
import { buildCanonicalBlockFixture } from "@al-ft/midgard-fault-proofs/test-support/canonical-block-evidence-fixture";
import * as SDK from "@al-ft/midgard-sdk";
import { Effect } from "effect";
import { afterEach, describe, expect, it } from "vitest";

import { WatcherReplayLocalFaultExhaustedError } from "../../src/fault-proofs/replay-local-fault-bound.js";
import { createWatcherReplayedLedger } from "../../src/fault-proofs/replayed-ledger.js";
import { observation } from "./fault-decision-bridge.observation.js";

const availabilityAuthority = () => ({
  deploymentIdentity: "c7".repeat(28),
  responseGeometry: SDK.availabilityResponseGeometry({
    chunkByteLength: 4096,
    trancheByteLength: 64 * 1024,
    maxTrancheCount: 1024,
  }),
});

const directories: string[] = [];
afterEach(async () => {
  for (const directory of directories.splice(0))
    await rm(directory, { recursive: true, force: true });
});

/**
 * A keeper whose confirmed head is one block on genesis, whose replay fails
 * locally while `faulting` holds. Its checkpoint store never restores, so a
 * new follower generation replays the head again.
 */
const scene = async () => {
  const directory = await mkdtemp(join(tmpdir(), "watcher-local-fault-"));
  directories.push(directory);
  const block = await buildCanonicalBlockFixture({
    transactions: [],
    utxos: [],
    prevHeaderHash: SDK.GENESIS_HEADER_HASH,
  });
  const state = { faulting: true, epoch: 0 };
  const written: LedgerCheckpoint[] = [];
  const sources: RetainedDaPayloadSource[] = [
    {
      sourceId: "local-fault",
      fetchPayloadByHeaderHash: async () => ({
        ok: true,
        sourceId: "local-fault",
        sourcePeerId: "peer",
        payloadEnvelopeCbor: block.payloadEnvelopeCbor,
        provenance: {
          trustClass: "public_or_permissionless_da",
          sourceId: "local-fault/peer",
          grade: "security",
        },
        attempts: [],
      }),
    },
  ];
  const keeper = createWatcherReplayedLedger({
    follower: {
      epoch: () => state.epoch,
      confirmed: async () => ({
        state: {
          ...SDK.makeGenesisConfirmedState(1000n),
          headerHash: block.headerHash,
          utxoRoot: block.header.utxosRoot,
        },
        current: () => true,
      }),
      materialFor: async (payload) => {
        if (state.faulting)
          throw new ReplayLocalFault({
            message: "the script evaluator failed locally",
            cause: undefined,
          });
        return {
          blockTimeMs: Number(payload.block_body.header.endTime),
          replayUserEvent: () => Effect.die("the block has no user event"),
          verifyForcedSource: () => Effect.die("the block has no forced order"),
        };
      },
      availabilityStatus: async () => "Unattested",
      checkpointDurable: () => undefined,
      close: () => undefined,
    },
    l1SlotConfig: NAMED_NETWORK_L1_SLOT_CONFIGS.Preprod,
    availabilityAuthority,
    checkpointPath: join(directory, "ledger.json"),
    unsafeCheckpointStoreForTest: {
      read: async () => undefined,
      write: async (checkpoint) => {
        written.push(checkpoint);
      },
    },
  });
  const advance = () => keeper.advance(observation([]), sources);
  /** `count` advances, each held on the local fault. */
  const held = async (count: number) => {
    for (let attempt = 0; attempt < count; attempt += 1)
      expect(await advance()).toEqual([
        {
          reason: "replayed_ledger_local_fault",
          detail: "the script evaluator failed locally",
        },
      ]);
  };
  return { block, state, written, advance, held };
};

describe("the replayed ledger's local-fault bound", () => {
  it("exits on the limit-th local fault in a row toward one confirmed head", async () => {
    const s = await scene();
    await s.held(LOCAL_FAULT_RUN_LIMIT - 1);
    const outcome = s.advance();
    await expect(outcome).rejects.toBeInstanceOf(
      WatcherReplayLocalFaultExhaustedError,
    );
    await expect(outcome).rejects.toThrow(
      `replayed_ledger_local_fault: ${s.block.headerHash}: the script evaluator failed locally`,
    );
    expect(s.written).toEqual([]);
  });

  it("starts the count over once an advance toward the head completes", async () => {
    const s = await scene();
    await s.held(LOCAL_FAULT_RUN_LIMIT - 1);
    s.state.faulting = false;
    expect(await s.advance()).toEqual([]);
    expect(s.written.map(({ headerHash }) => headerHash)).toEqual([
      s.block.headerHash,
    ]);
    // A new follower generation replays the head again.
    s.state.epoch += 1;
    s.state.faulting = true;
    await s.held(LOCAL_FAULT_RUN_LIMIT - 1);
  });
});
