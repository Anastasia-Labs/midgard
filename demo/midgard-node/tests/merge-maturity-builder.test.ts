import "./utils.js";

import { SELECTED_DEPLOYMENT_PROFILE } from "@al-ft/midgard-core/deployment-profile";
import * as SDK from "@al-ft/midgard-sdk";
import { Effect, Ref } from "effect";
import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";

import { Globals, NodeConfig } from "../src/services/index.js";
import {
  registerTestL1Tip,
  TEN_MINUTES_MS,
  type TestL1Tip,
} from "./helpers/l1-tip.js";

const fetchConfirmedStateAndItsLinkProgramMock = vi.hoisted(() => vi.fn());
const getStateQueueNodeFromStateQueueDatumMock = vi.hoisted(() => vi.fn());
const getHeaderFromStateQueueDatumMock = vi.hoisted(() => vi.fn());
const hashBlockHeaderMock = vi.hoisted(() => vi.fn());
const fetchFirstBlockTxsMock = vi.hoisted(() => vi.fn());
const breakDownTxMock = vi.hoisted(() => vi.fn());
const makeLocalOgmiosSubmitSlotSnapshotProviderMock = vi.hoisted(() => vi.fn());
const localSlotSnapshotProviderMock = vi.hoisted(() => vi.fn());

vi.mock("@al-ft/midgard-sdk", async (importOriginal) => {
  const actual = await importOriginal<typeof import("@al-ft/midgard-sdk")>();
  return {
    ...actual,
    fetchConfirmedStateAndItsLinkProgram:
      fetchConfirmedStateAndItsLinkProgramMock,
    getStateQueueNodeFromStateQueueDatum:
      getStateQueueNodeFromStateQueueDatumMock,
    getHeaderFromStateQueueDatum: getHeaderFromStateQueueDatumMock,
    hashBlockHeader: hashBlockHeaderMock,
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

vi.mock("../src/utils.js", async (importOriginal) => {
  const actual = await importOriginal<typeof import("../src/utils.js")>();
  return {
    ...actual,
    breakDownTx: breakDownTxMock,
  };
});

vi.mock("../src/l1-heads.js", async (importOriginal) => {
  const actual = await importOriginal<typeof import("../src/l1-heads.js")>();
  return {
    ...actual,
    makeLocalOgmiosSubmitSlotSnapshotProvider:
      makeLocalOgmiosSubmitSlotSnapshotProviderMock,
  };
});

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
} as SDK.MidgardValidators;

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
}: {
  readonly daAttestation: SDK.DaAvailabilityStateQueueStatus;
  readonly endTime: bigint;
}) => {
  const blockHeader = {
    endTime,
  } as SDK.Header;
  fetchConfirmedStateAndItsLinkProgramMock.mockImplementation(() =>
    Effect.succeed({
      confirmed: genesisConfirmed,
      link: makeLink(),
    }),
  );
  getStateQueueNodeFromStateQueueDatumMock.mockImplementation(() =>
    Effect.succeed({
      proven_fraud: null,
      header: blockHeader,
      da_attestation: daAttestation,
    } as SDK.StateQueueNode),
  );
  getHeaderFromStateQueueDatumMock.mockImplementation(() =>
    Effect.succeed(blockHeader),
  );
  hashBlockHeaderMock.mockImplementation(() => Effect.succeed(headerHash));
};

const runBuilder = () =>
  Effect.runPromise(
    Effect.gen(function* () {
      const globals = yield* Globals;
      yield* Ref.set(globals.BLOCKS_IN_QUEUE, 8);
      yield* Ref.set(
        globals.LATEST_SYNC_TIME_OF_STATE_QUEUE_LENGTH,
        Date.now(),
      );
      return yield* buildAndSubmitMergeTx(fakeLucid, fetchConfig, contracts);
    }).pipe(
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
    l1Tip = registerTestL1Tip(fakeLucid, 510);
    fetchConfirmedStateAndItsLinkProgramMock.mockReset();
    getStateQueueNodeFromStateQueueDatumMock.mockReset();
    getHeaderFromStateQueueDatumMock.mockReset();
    hashBlockHeaderMock.mockReset();
    fetchFirstBlockTxsMock.mockReset();
    breakDownTxMock.mockReset();
    makeLocalOgmiosSubmitSlotSnapshotProviderMock.mockReset();
    localSlotSnapshotProviderMock.mockReset();
    fakeLucidUtxosAt.mockReset();

    configureCandidate({
      // A state-queue node's `da_attestation` is the
      // `DaAvailabilityStateQueueStatus` enum, not a raw policy-id string;
      // `Attested` is one of the two merge-permitting kinds, so the maturity
      // window (not availability) is what this default exercises.
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
    makeLocalOgmiosSubmitSlotSnapshotProviderMock.mockImplementation(
      () => localSlotSnapshotProviderMock,
    );
  });

  afterEach(() => {
    vi.useRealTimers();
  });

  it("returns not-mature before BlocksDB lookup or decode", async () => {
    const result = await runBuilder();

    expect(result).toMatchObject({
      status: "skipped_oldest_block_not_mature",
      headerHash,
      readyAfterUnixTime:
        520_000 + SELECTED_DEPLOYMENT_PROFILE.timing.block_maturity_ms,
      nowUnixTime: 510_000,
    });
    expect(fetchFirstBlockTxsMock).not.toHaveBeenCalled();
    expect(breakDownTxMock).not.toHaveBeenCalled();
    expect(localSlotSnapshotProviderMock).not.toHaveBeenCalled();
  });

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
          source: "local_ogmios_tip",
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
      slotSource: "local_ogmios_tip",
      reason: expect.stringContaining("provider_current_slot=7"),
      dependencyKey: "merge:candidate:10:12",
      invalidationKey: "merge:candidate:10:12",
    });
  });
});
