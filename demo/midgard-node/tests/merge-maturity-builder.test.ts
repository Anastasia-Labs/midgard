import "./utils.js";

import { SELECTED_DEPLOYMENT_PROFILE } from "@al-ft/midgard-core/deployment-profile";
import type { SubmitSlotSnapshot } from "@al-ft/midgard-core/ogmios-slot";
import * as SDK from "@al-ft/midgard-sdk";
import { Effect, Layer } from "effect";
import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";

import { Globals, NodeConfig } from "../src/services/index.js";
import {
  attachTestL1Access,
  TEN_MINUTES_MS,
  type TestL1Tip,
} from "./helpers/l1-tip.js";

const requireLandedStateQueueMock = vi.hoisted(() => vi.fn());
const getStateQueueNodeFromStateQueueDatumMock = vi.hoisted(() => vi.fn());
const getHeaderFromStateQueueDatumMock = vi.hoisted(() => vi.fn());
const hashBlockHeaderMock = vi.hoisted(() => vi.fn());
const fetchFirstBlockTxsMock = vi.hoisted(() => vi.fn());
const breakDownTxMock = vi.hoisted(() => vi.fn());
const localSlotSnapshotProviderMock = vi.hoisted(() => vi.fn());
const buildMergeTxMock = vi.hoisted(() => vi.fn());
// The poisoned own headers the readiness read returns (none by default).
const poisonedHeaders = vi.hoisted(() => ({ value: new Set<string>() }));

import { withoutFollowerJournal } from "./helpers/intent-journal.js";

vi.mock("@al-ft/midgard-sdk", async (importOriginal) => {
  const actual = await importOriginal<typeof import("@al-ft/midgard-sdk")>();
  return {
    ...actual,
    getStateQueueNodeFromStateQueueDatum:
      getStateQueueNodeFromStateQueueDatumMock,
    getHeaderFromStateQueueDatum: getHeaderFromStateQueueDatumMock,
    hashBlockHeader: hashBlockHeaderMock,
    // What a ready merge reads before its wallet view, and its build.
    fetchCorrectionLockUTxOProgram: () =>
      Effect.succeed({ txHash: "dd".repeat(32), outputIndex: 0 }),
    buildMergeToConfirmedStateTxProgram: buildMergeTxMock,
  };
});

// The landed queue (P1) the builder reads its length, root and link from.
vi.mock("../src/services/landed-state-queue.js", async (importOriginal) => {
  const actual =
    await importOriginal<
      typeof import("../src/services/landed-state-queue.js")
    >();
  return {
    ...actual,
    requireLandedStateQueue: requireLandedStateQueueMock,
  };
});

// Poisoned own-header readiness precedes the block lookup.
vi.mock("../src/database/poisoned-own-headers.js", async (importOriginal) => {
  const actual =
    await importOriginal<
      typeof import("../src/database/poisoned-own-headers.js")
    >();
  const { Effect: EffectModule } = await import("effect");
  return {
    ...actual,
    readPoisonedOwnHeaders: EffectModule.sync(() => ({
      orphaned: [...poisonedHeaders.value].map((hash) => ({
        headerHash: hash,
        orphans: 1,
      })),
      headers: poisonedHeaders.value,
    })),
  };
});

vi.mock("../src/transactions/utils.js", async (importOriginal) => {
  const actual =
    await importOriginal<typeof import("../src/transactions/utils.js")>();
  return {
    ...actual,
    fetchFirstBlockTxs: fetchFirstBlockTxsMock,
  };
});

// The merge withdrawal's reference script a ready merge resolves.
vi.mock("../src/transactions/reference-scripts.js", async (importOriginal) => {
  const actual =
    await importOriginal<
      typeof import("../src/transactions/reference-scripts.js")
    >();
  return {
    ...actual,
    referenceScriptByName: () => ({ txHash: "ab".repeat(32), outputIndex: 0 }),
  };
});

vi.mock("../src/utils.js", async (importOriginal) => {
  const actual = await importOriginal<typeof import("../src/utils.js")>();
  return {
    ...actual,
    breakDownTx: breakDownTxMock,
  };
});

import {
  listSlotAwareDueWork,
  slotAwareDueWorkRegistry,
} from "../src/fibers/slot-aware-due-work.js";
import {
  IntentJournal,
  IntentJournalWithoutFollower,
} from "../src/services/intent-journal.js";
import {
  buildAndSubmitMergeTx,
  mergeNoInlineSubmitDueWorkFromDefer,
  type MergeTxResult,
} from "../src/transactions/state-queue/merge-to-confirmed-state.js";
import { NoInlineSubmitDefer } from "../src/transactions/utils.js";

const fetchConfig: SDK.StateQueueFetchConfig = {
  stateQueueAddress: "addr_test1statequeue",
  stateQueuePolicyId: "00".repeat(28),
};

const contracts = {
  daAttestation: {
    policyId: "22".repeat(28),
  },
  stateQueue: {
    mintingScriptCBOR: "",
  },
  settlement: { policyId: "33".repeat(28) },
  hubOracle: { policyId: "44".repeat(28) },
  correctionLock: { spendingScriptAddress: "addr_test1correctionlock" },
} as unknown as SDK.MidgardValidators;

const headerHash = "aa".repeat(28);

const fakeLucidUtxosAt = vi.fn();

const fakeLucid = {
  config: () => ({
    network: "Custom",
    provider: {},
  }),
  slotToUnixTime: (slot: number) => slot * 1_000,
  unixTimeToSlot: (unixTime: number) => Math.floor(unixTime / 1_000),
  utxosAt: fakeLucidUtxosAt,
  utxosAtWithUnit: async () => [{ txHash: "ee".repeat(32), outputIndex: 0 }],
  wallet: () => ({ address: async () => "addr_test1mergewallet" }),
} as never;

const makeLink = () =>
  ({
    utxo: {
      txHash: "bb".repeat(32),
      outputIndex: 0,
    },
    datum: {
      key: {
        Key: {
          key: headerHash,
        },
      },
      data: {},
    },
    assetName: "",
  }) as SDK.StateQueueUTxO;

// The root node at genesis: no earlier merge exists for the builder to
// finalize before it builds.
const genesisConfirmed = {
  utxo: {
    txHash: "cc".repeat(32),
    outputIndex: 0,
  },
  datum: {
    key: "Empty",
    next: { Key: { key: headerHash } },
    data: SDK.castConfirmedStateToData({
      headerHash: SDK.GENESIS_HEADER_HASH,
      prevHeaderHash: SDK.GENESIS_HEADER_HASH,
      utxoRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
      startTime: 0n,
      endTime: 0n,
      protocolVersion: 0n,
    }),
  },
  assetName: SDK.STATE_QUEUE_ROOT_ASSET_NAME,
} as SDK.StateQueueUTxO;

const configureCandidate = ({
  daAttestation,
  endTime,
  blockTime,
}: {
  readonly daAttestation: SDK.DaAvailabilityStateQueueStatus;
  readonly endTime: bigint;
  readonly blockTime?: bigint;
}) => {
  const blockHeader = {
    endTime,
  } as SDK.Header;
  // Eight queued blocks, above the default minimum queue length.
  requireLandedStateQueueMock.mockImplementation(() =>
    Effect.succeed({
      healthy: true,
      root: { element: genesisConfirmed },
      nodes: Array.from({ length: 8 }, () => ({ element: makeLink() })),
    }),
  );
  getStateQueueNodeFromStateQueueDatumMock.mockImplementation(() =>
    Effect.succeed({
      proven_fraud: null,
      header: blockHeader,
      // The authenticated commit clock is separate from the payload end time.
      block_time: blockTime ?? endTime,
      da_attestation: daAttestation,
    } as SDK.StateQueueNode),
  );
  getHeaderFromStateQueueDatumMock.mockImplementation(() =>
    Effect.succeed(blockHeader),
  );
  hashBlockHeaderMock.mockImplementation(() => Effect.succeed(headerHash));
};

// A follower journal whose wallet view holds `held` (live own intents' inputs).
const holdingJournal = (held: readonly string[]) =>
  Layer.succeed(IntentJournal, {
    ...Effect.runSync(
      Effect.provide(IntentJournal, IntentJournalWithoutFollower),
    ),
    walletView: (_lucid, address) =>
      Effect.succeed({
        address,
        source: "follower" as const,
        utxos: [{ txHash: "ff".repeat(32), outputIndex: 1 } as never],
        predictedBy: new Map<string, string>(),
        held: new Set(held),
      }),
  });

const runBuilder = (held?: readonly string[]) =>
  Effect.runPromise(
    buildAndSubmitMergeTx(fakeLucid, fetchConfig, contracts).pipe(
      held === undefined
        ? withoutFollowerJournal
        : Effect.provide(holdingJournal(held)),
      Effect.provide(Globals.Default),
      Effect.provide(NodeConfig.layer),
    ) as Effect.Effect<MergeTxResult, unknown, never>,
  );

const readyAfterUnixTime =
  520_000 + SELECTED_DEPLOYMENT_PROFILE.timing.block_maturity_ms;

describe("merge builder maturity preflight", () => {
  // Maturity is judged at the L1 `slotNow`; the local clock keeps driving
  // only the submit-ledger timing.
  let l1Tip: TestL1Tip;
  const setNow = (unixTimeMs: number) => {
    vi.setSystemTime(unixTimeMs);
    l1Tip.setTipSlot(unixTimeMs / 1_000);
  };
  beforeEach(() => {
    vi.useFakeTimers();
    vi.setSystemTime(510_000);
    // The client's submit-slot reader: the local node's ledger tip.
    l1Tip = attachTestL1Access(fakeLucid, 510, {
      submitSlotSnapshot: () =>
        Effect.runPromise(
          localSlotSnapshotProviderMock() as Effect.Effect<SubmitSlotSnapshot>,
        ),
    });
    requireLandedStateQueueMock.mockReset();
    getStateQueueNodeFromStateQueueDatumMock.mockReset();
    getHeaderFromStateQueueDatumMock.mockReset();
    hashBlockHeaderMock.mockReset();
    fetchFirstBlockTxsMock.mockReset();
    breakDownTxMock.mockReset();
    localSlotSnapshotProviderMock.mockReset();
    buildMergeTxMock.mockReset();
    slotAwareDueWorkRegistry.clearAll();
    fakeLucidUtxosAt.mockReset();
    poisonedHeaders.value = new Set();

    configureCandidate({
      // Attested permits merging; the default isolates its maturity check.
      daAttestation: { Attested: { commitment_hash: "22".repeat(32) } },
      endTime: 500_000n,
    });
    fetchFirstBlockTxsMock.mockImplementation(() =>
      Effect.succeed({
        txs: [],
        txHashes: [],
        headerHash: Buffer.from(headerHash, "hex"),
      }),
    );
    localSlotSnapshotProviderMock.mockImplementation(() =>
      Effect.succeed({
        source: "test",
        currentSlot: 0,
        observedAtMs: 0,
        slotLengthMs: 1_000,
      }),
    );
  });

  afterEach(() => {
    vi.useRealTimers();
  });

  it.each([500_000n, 100_000n])(
    "holds the immature commit clock before decode with payload end %s",
    async (endTime) => {
      configureCandidate({
        daAttestation: { Attested: { commitment_hash: "22".repeat(32) } },
        endTime,
        blockTime: 500_000n,
      });
      expect(await runBuilder()).toMatchObject({
        status: "skipped_oldest_block_not_mature",
        headerHash,
        readyAfterUnixTime,
        nowUnixTime: 510_000,
      });
      expect(fetchFirstBlockTxsMock).not.toHaveBeenCalled();
      expect(breakDownTxMock).not.toHaveBeenCalled();
      expect(localSlotSnapshotProviderMock).not.toHaveBeenCalled();
    },
  );

  it("returns DA-unattested before BlocksDB lookup or decode", async () => {
    configureCandidate({
      daAttestation: SDK.NO_DA_ATTESTATION,
      endTime: 500_000n,
    });
    setNow(530_000 + SELECTED_DEPLOYMENT_PROFILE.timing.block_maturity_ms);

    const result = await runBuilder();

    expect(result).toMatchObject({
      status: "skipped_oldest_block_unattested",
      headerHash,
      reason: expect.stringContaining("current_da_availability=Unattested"),
    });
    expect(fetchFirstBlockTxsMock).not.toHaveBeenCalled();
    expect(breakDownTxMock).not.toHaveBeenCalled();
    expect(localSlotSnapshotProviderMock).not.toHaveBeenCalled();
  });

  it("returns event-orphaned for a poisoned own header before BlocksDB lookup or decode", async () => {
    poisonedHeaders.value = new Set([headerHash]);
    setNow(530_000 + SELECTED_DEPLOYMENT_PROFILE.timing.block_maturity_ms);

    const result = await runBuilder();

    expect(result).toMatchObject({
      status: "skipped_oldest_block_event_orphaned",
      headerHash,
      reason: expect.stringContaining(`header=${headerHash}`),
    });
    expect(fetchFirstBlockTxsMock).not.toHaveBeenCalled();
    expect(breakDownTxMock).not.toHaveBeenCalled();
  });

  it("calls nothing mature on a local clock 10 minutes fast", async () => {
    // L1 now is one slot before maturity; the local clock is 10 minutes past
    // it.
    setNow(readyAfterUnixTime - 1_000);
    vi.setSystemTime(readyAfterUnixTime + TEN_MINUTES_MS);

    const result = await runBuilder();

    expect(result).toMatchObject({
      status: "skipped_oldest_block_not_mature",
      headerHash,
      readyAfterUnixTime,
      nowUnixTime: readyAfterUnixTime - 1_000,
    });
    expect(fetchFirstBlockTxsMock).not.toHaveBeenCalled();
  });

  it("lets a semantically ready candidate proceed to the local submit-ledger gate", async () => {
    setNow(530_000 + SELECTED_DEPLOYMENT_PROFILE.timing.block_maturity_ms);

    const result = await runBuilder();

    expect(result).toMatchObject({
      status: "skipped_oldest_block_local_ledger_not_ready",
      headerHash,
      reason: expect.stringContaining("local_ledger_slot=0"),
      readyAfterUnixTime:
        520_000 + SELECTED_DEPLOYMENT_PROFILE.timing.block_maturity_ms,
    });
    expect(fetchFirstBlockTxsMock).toHaveBeenCalledTimes(1);
    expect(breakDownTxMock).not.toHaveBeenCalled();
    expect(localSlotSnapshotProviderMock).toHaveBeenCalledTimes(1);
  });

  // A local node between blocks: the wall-clock slot is past the merge's lower
  // bound, its latest block is not (devnet r4: tip 20 short, wall 8 past).
  const validFromSlot =
    (500_000 + SELECTED_DEPLOYMENT_PROFILE.timing.block_maturity_ms) / 1_000;
  const trailingTip = (ledgerTipSlot: number, currentSlot: number) =>
    localSlotSnapshotProviderMock.mockImplementation(() =>
      Effect.succeed({
        source: "l1_node_tip",
        currentSlot,
        ledgerTipSlot,
        observedAtMs: currentSlot * 1_000,
        slotLengthMs: 1_000,
      }),
    );

  it("defers a merge whose lower bound the ledger tip has not reached, though the wall-clock slot has, and builds nothing", async () => {
    setNow(530_000 + SELECTED_DEPLOYMENT_PROFILE.timing.block_maturity_ms);
    trailingTip(validFromSlot - 20, validFromSlot + 8);

    expect(await runBuilder()).toMatchObject({
      status: "skipped_oldest_block_local_ledger_not_ready",
      headerHash,
      reason: expect.stringContaining(
        `local_ledger_slot=${(validFromSlot - 20).toString()},target_slot=${(validFromSlot + 2).toString()}`,
      ),
    });
    expect(buildMergeTxMock).not.toHaveBeenCalled();
    expect(listSlotAwareDueWork()).toMatchObject([
      {
        kind: "merge_submit_validity",
        observedSlot: validFromSlot - 20,
        dueSlot: validFromSlot + 2,
      },
    ]);
  });

  it("builds a merge once the ledger tip reaches its bound, unless an own live merge intent spends its inputs", async () => {
    // Devnet r4: a rebuild beside the refused, still-live merge spent its
    // state-queue inputs and its predicted change again (BadInputsUTxO).
    setNow(530_000 + SELECTED_DEPLOYMENT_PROFILE.timing.block_maturity_ms);
    trailingTip(validFromSlot + 2, validFromSlot + 8);
    buildMergeTxMock.mockImplementation(() =>
      Effect.fail(new Error("merge build reached")),
    );
    const held = [`${"cc".repeat(32)}#0`, `${"bb".repeat(32)}#0`];

    expect(await runBuilder(held)).toMatchObject({
      status: "skipped_own_merge_in_flight",
      headerHash,
      reason: `held_merge_inputs=${held.join("|")}`,
    });
    expect(buildMergeTxMock).not.toHaveBeenCalled();

    await expect(runBuilder([])).rejects.toThrow("merge build reached");
    expect(buildMergeTxMock).toHaveBeenCalledTimes(1);
    expect(buildMergeTxMock.mock.calls[0]![0]).toMatchObject({
      validFrom: validFromSlot * 1_000,
    });
  });

  it("anchors provider-slot submit defers to local future slots", () => {
    const defer = new NoInlineSubmitDefer({
      callerLabel: "merge",
      kind: "provider_slot_wait",
      key: "merge:candidate:10:12",
      txHash: "aa".repeat(32),
      currentSlot: 7,
      targetSlot: 12,
      dueSlot: 12,
      waitMs: 5_000,
      slotSource: "provider",
      dependencyKey: "merge:candidate:10:12",
      invalidationKey: "merge:candidate:10:12",
      invalidBeforeSlot: 10,
      invalidHereafterSlot: 20,
    });

    expect(
      mergeNoInlineSubmitDueWorkFromDefer({
        defer,
        localSubmitSlotSnapshot: {
          currentSlot: 12,
          slotLengthMs: 1_000,
          source: "l1_node_tip",
        },
        nowMs: 1_000,
      }),
    ).toMatchObject({
      kind: "merge_submit_validity",
      key: "merge:candidate:10:12",
      observedSlot: 12,
      dueSlot: 17,
      dueAtMs: 6_000,
      waitMs: 5_000,
      slotSource: "l1_node_tip",
      reason: expect.stringContaining("provider_current_slot=7"),
      dependencyKey: "merge:candidate:10:12",
      invalidationKey: "merge:candidate:10:12",
    });
  });
});
