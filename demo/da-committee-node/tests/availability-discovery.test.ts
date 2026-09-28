import * as SDK from "@al-ft/midgard-sdk";
import type { LucidEvolution, UTxO } from "@lucid-evolution/lucid";
import { beforeEach, describe, expect, it, vi } from "vitest";

import {
  type AvailabilityResponderSkippedRecord,
  discoverAvailabilityResponderChallenges,
} from "../src/availability/factory.js";

// Wrapped, not replaced: the live-state authentication is the SDK's own and is
// exercised there. These tests pin how the responder finds challenge records
// and what it requires of the authenticated snapshot.
vi.mock("@al-ft/midgard-sdk", async (importOriginal) => {
  const actual = await importOriginal<typeof import("@al-ft/midgard-sdk")>();
  return {
    ...actual,
    daAvailabilityChallengeSnapshotFromUtxos: vi.fn(
      actual.daAvailabilityChallengeSnapshotFromUtxos,
    ),
  };
});

const AVAILABILITY_POLICY = "a1".repeat(28);
const AVAILABILITY_ADDRESS = "addr_test1availability";
const STATE_QUEUE_ADDRESS = "addr_test1statequeue";
const CORRECTION_LOCK_ADDRESS = "addr_test1correctionlock";

const geometry = SDK.availabilityResponseGeometry({
  chunkByteLength: 4096,
  trancheByteLength: 4 * 1024 * 1024,
  maxTrancheCount: 16,
});

const parameters = SDK.daAvailabilityParameters({
  responseGeometry: geometry,
  ...SDK.DA_AVAILABILITY_PROFILE_BOND_AMOUNTS,
  challengerBondLovelace: 12_000_000_000n,
  maxOpenFeeLovelace: 500_000n,
  maxPublicationFeeLovelace: 500_000n,
  maxSettlementFeeLovelace: 500_000n,
  maxCloseFeeLovelace: 1_000_000n,
  maxTimeoutFeeLovelace: 1_200_000n,
});

const deployment = {
  contracts: {
    availabilityChallenge: {
      policyId: AVAILABILITY_POLICY,
      spendingScriptAddress: AVAILABILITY_ADDRESS,
    },
    stateQueue: { spendingScriptAddress: STATE_QUEUE_ADDRESS },
    correctionLock: { spendingScriptAddress: CORRECTION_LOCK_ADDRESS },
  },
  parameters,
} as unknown as SDK.DaAvailabilityDeployment;

/** A challenge record over `headerByte`'s header, opened at `openedAt`. */
const challengeRecord = (headerByte: string, openedAt: bigint) =>
  SDK.buildDaAvailabilityChallengeDatumPlan({
    commitment: SDK.buildDaAvailabilityCommitment({
      deploymentIdentity: "11".repeat(28),
      headerHash: headerByte.repeat(28),
      payload: Uint8Array.from([1, 2, 3, 4]),
      responseGeometry: geometry,
    }),
    challengerFundingOutRef: {
      transactionId: headerByte.repeat(32),
      outputIndex: 0n,
    },
    challenger: "aa".repeat(28),
    openedAt,
    parameters,
  }).record;

const recordUtxo = (
  record: SDK.DaAvailabilityChallengeRecord,
  txByte: string,
  assetName = record.challenge_asset_name,
): UTxO => ({
  txHash: txByte.repeat(32),
  outputIndex: 0,
  address: AVAILABILITY_ADDRESS,
  assets: {
    lovelace: parameters.challenge_record_lovelace,
    [AVAILABILITY_POLICY + assetName]: 1n,
  },
  datum: SDK.encodeDaAvailabilityChallengeRecord(record),
});

/** A UTxO at the availability address that is not a challenge record. */
const otherUtxo = (unitSuffix: string, txByte: string): UTxO => ({
  txHash: txByte.repeat(32),
  outputIndex: 1,
  address: AVAILABILITY_ADDRESS,
  assets: { lovelace: 2_000_000n, [AVAILABILITY_POLICY + unitSuffix]: 1n },
  datum: null,
});

const lucidWith = (availabilityUtxos: readonly UTxO[]) =>
  ({
    utxosAt: async (address: string) =>
      address === AVAILABILITY_ADDRESS ? availabilityUtxos : [],
  }) as unknown as LucidEvolution;

/** An authenticated snapshot whose record is `utxo`, as the SDK would return. */
const snapshotOf = (utxo: UTxO, record: SDK.DaAvailabilityChallengeRecord) =>
  ({
    record: utxo,
    recordDatum: record,
    terminal: { ...utxo, outputIndex: 5 },
    terminalDatum: {},
    queue: { utxo: { ...utxo, outputIndex: 6 } },
    tranches: [],
  }) as unknown as Awaited<
    ReturnType<typeof SDK.daAvailabilityChallengeSnapshotFromUtxos>
  >;

const snapshotMock = vi.mocked(SDK.daAvailabilityChallengeSnapshotFromUtxos);

describe("availability responder challenge discovery", () => {
  beforeEach(() => {
    snapshotMock.mockReset();
  });

  it("discovers each challenge record by its DACH token and orders them by response deadline", async () => {
    const later = challengeRecord("41", 9_000n);
    const earlier = challengeRecord("42", 1_000n);
    const laterUtxo = recordUtxo(later, "51");
    const earlierUtxo = recordUtxo(earlier, "52");
    const byHeader = new Map([
      [later.commitment.header_hash, snapshotOf(laterUtxo, later)],
      [earlier.commitment.header_hash, snapshotOf(earlierUtxo, earlier)],
    ]);
    snapshotMock.mockImplementation(async (_, headerHash) => {
      const snapshot = byHeader.get(headerHash);
      if (snapshot === undefined) throw new Error("unexpected header");
      return snapshot;
    });

    const challenges = await discoverAvailabilityResponderChallenges(
      lucidWith([
        laterUtxo,
        // A terminal accumulator (DACT) and a tranche thread (DT) share the
        // address and policy; neither is a challenge record.
        otherUtxo("44414354" + "00".repeat(28), "61"),
        otherUtxo("4454" + "00".repeat(30), "62"),
        earlierUtxo,
      ]),
      deployment,
    );

    expect(challenges.map(({ record }) => record.utxo.txHash)).toEqual([
      earlierUtxo.txHash,
      laterUtxo.txHash,
    ]);
    expect(challenges.map(({ record }) => record.datum)).toEqual([
      earlier,
      later,
    ]);
    expect(earlier.response_deadline).toBeLessThan(later.response_deadline);
    expect(snapshotMock.mock.calls.map(([, headerHash]) => headerHash)).toEqual(
      [later.commitment.header_hash, earlier.commitment.header_hash],
    );
  });

  it("finds nothing to answer when no DACH record is live", async () => {
    await expect(
      discoverAvailabilityResponderChallenges(
        lucidWith([otherUtxo("44414354" + "00".repeat(28), "61")]),
        deployment,
      ),
    ).resolves.toEqual([]);
    expect(snapshotMock).not.toHaveBeenCalled();
  });

  /** Runs discovery and collects what it left out. */
  const discover = async (utxos: readonly UTxO[]) => {
    const skipped: AvailabilityResponderSkippedRecord[] = [];
    const challenges = await discoverAvailabilityResponderChallenges(
      lucidWith(utxos),
      deployment,
      (skip) => skipped.push(skip),
    );
    return { challenges, skipped };
  };
  const outRefOf = (utxo: UTxO) => `${utxo.txHash}#${utxo.outputIndex}`;

  it("skips a record whose datum names a different challenge than its token", async () => {
    const record = challengeRecord("41", 1_000n);
    const other = challengeRecord("43", 1_000n);
    const utxo = recordUtxo(record, "51", other.challenge_asset_name);
    snapshotMock.mockResolvedValue(snapshotOf(utxo, record));

    await expect(discover([utxo])).resolves.toEqual({
      challenges: [],
      skipped: [
        {
          outRef: outRefOf(utxo),
          stranded: false,
          reason:
            "Availability challenge record datum names a different challenge than its token",
        },
      ],
    });
    expect(snapshotMock).not.toHaveBeenCalled();
  });

  // A timeout or fraud removal can prune a Challenged descendant; its record
  // then stays at the availability address for good with no node to answer.
  it("skips a stranded record whose state-queue node is gone and still finds the live challenge", async () => {
    const stranded = challengeRecord("41", 1_000n);
    const live = challengeRecord("42", 9_000n);
    const strandedUtxo = recordUtxo(stranded, "51");
    const liveUtxo = recordUtxo(live, "52");
    snapshotMock.mockImplementation(async (_, headerHash) =>
      headerHash === live.commitment.header_hash
        ? snapshotOf(liveUtxo, live)
        : ({ tranches: [] } as never),
    );

    const { challenges, skipped } = await discover([strandedUtxo, liveUtxo]);

    expect(challenges.map(({ record }) => record.utxo)).toEqual([liveUtxo]);
    expect(skipped).toEqual([
      {
        outRef: outRefOf(strandedUtxo),
        stranded: true,
        reason:
          "Availability challenge record is not the one its state-queue node is challenged by",
      },
    ]);
  });

  it.each([
    ["another transaction", { txHash: "59".repeat(32) }],
    ["another output of the same transaction", { outputIndex: 3 }],
  ])(
    "skips a record when its state-queue node is challenged by a record in %s",
    async (_, moved) => {
      const record = challengeRecord("41", 1_000n);
      const utxo = recordUtxo(record, "51");
      snapshotMock.mockResolvedValue(snapshotOf({ ...utxo, ...moved }, record));

      await expect(discover([utxo])).resolves.toEqual({
        challenges: [],
        skipped: [
          {
            outRef: outRefOf(utxo),
            stranded: true,
            reason:
              "Availability challenge record is not the one its state-queue node is challenged by",
          },
        ],
      });
    },
  );

  it("skips a challenge whose authenticated live state is incomplete and one whose snapshot fails, without stopping discovery", async () => {
    const incomplete = challengeRecord("41", 1_000n);
    const failing = challengeRecord("43", 1_000n);
    const live = challengeRecord("42", 9_000n);
    const incompleteUtxo = recordUtxo(incomplete, "51");
    const failingUtxo = recordUtxo(failing, "53");
    const liveUtxo = recordUtxo(live, "52");
    snapshotMock.mockImplementation(async (_, headerHash) => {
      if (headerHash === failing.commitment.header_hash)
        throw new Error("Nonunique authenticated unit");
      return headerHash === live.commitment.header_hash
        ? snapshotOf(liveUtxo, live)
        : ({
            ...snapshotOf(incompleteUtxo, incomplete),
            terminal: undefined,
          } as never);
    });

    const { challenges, skipped } = await discover([
      incompleteUtxo,
      failingUtxo,
      liveUtxo,
    ]);

    expect(challenges.map(({ record }) => record.utxo)).toEqual([liveUtxo]);
    expect(skipped).toEqual([
      {
        outRef: outRefOf(incompleteUtxo),
        stranded: false,
        reason:
          "Authenticated availability challenge has incomplete live state",
      },
      {
        outRef: outRefOf(failingUtxo),
        stranded: false,
        reason: "Nonunique authenticated unit",
      },
    ]);
  });
});
