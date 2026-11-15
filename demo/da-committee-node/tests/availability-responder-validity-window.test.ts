import { mkdtempSync, rmSync } from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";

import { openAvailabilityOperationJournal } from "@al-ft/midgard-core/availability-operation-journal";
import * as SDK from "@al-ft/midgard-sdk";
import type { LucidEvolution, UTxO } from "@lucid-evolution/lucid";
import { afterEach, describe, expect, it, vi } from "vitest";

import {
  availabilityResponderL1BuildTime,
  availabilityResponderTransactionOperation,
  buildAvailabilityResponderTransaction,
} from "../src/availability/factory.discover-availability-responder-challenges.js";
import { runPlainAvailabilityResponderOperation } from "../src/availability/plain-response-run.js";
import { committeePromiseNetworkClock } from "../src/availability/promise-network-clock.js";
import {
  promiseResponseBuildTime,
  promiseResponseTtl,
} from "../src/availability/promise-response-build.js";
import {
  type AvailabilityResponderAction,
  type AvailabilityResponderChallenge,
} from "../src/availability/responder.js";
import { AvailabilityResponderValidityDeferredError } from "../src/availability/validity-deferred-error.js";
import {
  payload,
  withResponseDeadline,
} from "./helpers/availability-challenge.js";
import { l1ClockAt } from "./helpers/l1-clock.js";

/**
 * The responder's validity interval and deadline decisions are taken on the
 * committee's L1 clock (the follower tip plus monotonic elapsed time) through
 * `SDK.daAvailabilityValidityWindow`, never on the wall clock. The builders
 * are wrapped so the interval each one is handed can be read; nothing here
 * needs a deployed protocol.
 */
vi.mock("@al-ft/midgard-sdk", async (importOriginal) => {
  const actual = await importOriginal<typeof import("@al-ft/midgard-sdk")>();
  const { Effect } = await import("effect");
  // As Lucid's builder does, the exclusive TTL is the slot of `validTo`
  // (the fixture's 1 s slots from ZERO_TIME, inlined: this factory is hoisted).
  const built = (
    _lucid: unknown,
    _deployment: unknown,
    input: { validTo: bigint },
  ) =>
    Effect.succeed({
      tx: {
        input,
        toTransaction: () => ({
          body: () => ({
            ttl: () => (input.validTo - 1_700_000_000_000n) / 1_000n,
          }),
        }),
      },
    });
  return {
    ...actual,
    daAvailabilityValidityWindow: vi.fn(actual.daAvailabilityValidityWindow),
    buildPublishDaAvailabilityChunkTxProgram: vi.fn(built),
    buildSettleDaAvailabilityTrancheTxProgram: vi.fn(built),
    buildCloseDaAvailabilityChallengeTxProgram: vi.fn(built),
  };
});

const ZERO_TIME = 1_700_000_000_000;
const slotStart = (slot: number) => ZERO_TIME + slot * 1_000;
const TIP_SLOT = 10_000;
const TIP_MS = slotStart(TIP_SLOT);

const lucid = {
  config: () => ({
    slotConfig: { zeroTime: ZERO_TIME, zeroSlot: 0, slotLength: 1_000 },
    protocolParameters: { collateralPercentage: 150 },
  }),
  unixTimeToSlot: (ms: number) => Math.floor((ms - ZERO_TIME) / 1_000),
  slotToUnixTime: (slot: number) => slotStart(slot),
  wallet: () => ({ address: async () => "addr_test1responder" }),
  utxosAt: async (): Promise<UTxO[]> => [
    {
      txHash: "77".repeat(32),
      outputIndex: 0,
      address: "addr_test1responder",
      assets: { lovelace: 50_000_000n },
    },
  ],
} as unknown as LucidEvolution;

const deployment = {
  parameters: {
    max_publication_fee_lovelace: 500_000n,
    max_settlement_fee_lovelace: 500_000n,
    max_close_fee_lovelace: 1_000_000n,
  },
} as unknown as SDK.DaAvailabilityDeployment;

/** The follower tip at `TIP_SLOT`, observed `elapsedMs` ago on the monotonic clock. */
const clockAt = (elapsedMs: number) =>
  l1ClockAt({
    slotConfig: lucid.config().slotConfig!,
    tipSlot: TIP_SLOT,
    elapsedMs,
  });
const withDeadline = withResponseDeadline;

const publishOf = (
  challenge: AvailabilityResponderChallenge,
): AvailabilityResponderAction => ({
  kind: "publish",
  challenge,
  tranche: challenge.tranches[0]!,
  publication: SDK.planDaAvailabilityPublications({
    commitment: challenge.record.datum.commitment,
    challengeAssetName: challenge.record.datum.challenge_asset_name,
    payload,
  })[0]!.publications[0]!,
});

/** The challenge with its first tranche published (a Receipt). */
const published = (
  challenge: AvailabilityResponderChallenge,
): AvailabilityResponderChallenge => {
  const tranche = challenge.tranches[0]!;
  if (!("Active" in tranche.datum))
    throw new Error("The fixture's first tranche is active");
  return {
    ...challenge,
    tranches: [
      {
        ...tranche,
        datum: {
          Receipt: tranche.datum.Active,
        } as unknown as SDK.DaAvailabilityTrancheDatum,
      },
    ],
  };
};

const interval = (
  builder:
    | typeof SDK.buildPublishDaAvailabilityChunkTxProgram
    | typeof SDK.buildSettleDaAvailabilityTrancheTxProgram
    | typeof SDK.buildCloseDaAvailabilityChallengeTxProgram,
) => {
  const calls = vi.mocked(builder).mock.calls;
  const input = calls[calls.length - 1]![2] as {
    validFrom: bigint;
    validTo: bigint;
  };
  return { validFrom: input.validFrom, validTo: input.validTo };
};

const windowLimits = () => {
  const calls = vi.mocked(SDK.daAvailabilityValidityWindow).mock.calls;
  return calls[calls.length - 1]![0].limits;
};

const dirs: string[] = [];
afterEach(() => {
  dirs
    .splice(0)
    .forEach((dir) => rmSync(dir, { recursive: true, force: true }));
  vi.useRealTimers();
});

describe("responder publication validity on the committee L1 clock", () => {
  it("never signs a lower bound past the tip: a tip 117 s behind the L1 clock defers the publish", async () => {
    // The deadline is 30 s ahead of the L1 clock. The longest range the tip
    // admits ends 1 s after the L1 clock's slot, too soon for any block
    // after it, so nothing is built.
    const { clock } = clockAt(117_000);
    const action = publishOf(withDeadline(TIP_MS + 147_000));
    const operation = availabilityResponderTransactionOperation(
      lucid,
      deployment,
      action,
      () => availabilityResponderL1BuildTime(clock),
    );
    await expect(
      operation.build(new AbortController().signal),
    ).rejects.toMatchObject({
      name: "AvailabilityResponderValidityDeferredError",
      reason: "ledger_tip_behind",
    });
    expect(SDK.buildPublishDaAvailabilityChunkTxProgram).not.toHaveBeenCalled();
  });

  it("builds the publish with validFrom <= tip and validTo <= deadline + 1 once the tip is within reach", async () => {
    const deadlineMs = TIP_MS + 80_000;
    const { clock } = clockAt(50_000);
    await buildAvailabilityResponderTransaction(
      lucid,
      deployment,
      publishOf(withDeadline(deadlineMs)),
      availabilityResponderL1BuildTime(clock),
    );
    const { validFrom, validTo } = interval(
      SDK.buildPublishDaAvailabilityChunkTxProgram,
    );
    expect(validFrom).toBeLessThanOrEqual(BigInt(TIP_MS));
    expect(validTo).toBeLessThanOrEqual(BigInt(deadlineMs) + 1n);
    expect(validTo - validFrom).toBeLessThanOrEqual(
      SDK.DA_AVAILABILITY_MAX_VALIDITY_RANGE_MS,
    );
    expect(windowLimits()).toEqual({ upperLimitMs: BigInt(deadlineMs) + 1n });
  });

  it("builds the deferred publish from the next block's tip, inside the deadline", async () => {
    const deadlineMs = TIP_MS + 150_000;
    const { clock } = clockAt(117_000);
    const action = publishOf(withDeadline(deadlineMs));
    await expect(
      buildAvailabilityResponderTransaction(
        lucid,
        deployment,
        action,
        availabilityResponderL1BuildTime(clock),
      ),
    ).rejects.toBeInstanceOf(AvailabilityResponderValidityDeferredError);
    // A block lands at slot TIP + 116 and the next boundary read observes it.
    clock.observeTipSlot(TIP_SLOT + 116);
    await buildAvailabilityResponderTransaction(
      lucid,
      deployment,
      action,
      availabilityResponderL1BuildTime(clock),
    );
    const { validFrom, validTo } = interval(
      SDK.buildPublishDaAvailabilityChunkTxProgram,
    );
    expect(validFrom).toBeLessThanOrEqual(BigInt(slotStart(TIP_SLOT + 116)));
    expect(validTo).toBeLessThanOrEqual(BigInt(deadlineMs) + 1n);
  });

  it("is unmoved by a wall clock skewed 120 s either way", async () => {
    vi.useFakeTimers({ toFake: ["Date"] });
    const deadlineMs = TIP_MS + 80_000;
    const action = publishOf(withDeadline(deadlineMs));
    const intervals: unknown[] = [];
    for (const skewMs of [0, -120_000, 120_000]) {
      vi.setSystemTime(TIP_MS + 50_000 + skewMs);
      const { clock } = clockAt(50_000);
      await availabilityResponderTransactionOperation(
        lucid,
        deployment,
        action,
        () => availabilityResponderL1BuildTime(clock),
      ).build(new AbortController().signal);
      intervals.push(interval(SDK.buildPublishDaAvailabilityChunkTxProgram));
    }
    expect(intervals[1]).toEqual(intervals[0]);
    expect(intervals[2]).toEqual(intervals[0]);
  });

  it("reads a closed publication window as the deadline passed", async () => {
    const { clock } = clockAt(30_000);
    await expect(
      buildAvailabilityResponderTransaction(
        lucid,
        deployment,
        publishOf(withDeadline(TIP_MS + 20_000)),
        availabilityResponderL1BuildTime(clock),
      ),
    ).rejects.toMatchObject({
      name: "AvailabilityResponderValidityDeferredError",
      reason: "deadline_passed",
    });
  });
});

describe("responder completion validity", () => {
  it("bounds neither a settle of a published tranche nor a close by the response deadline", async () => {
    // The deadline is long past: completion work is never bounded by it.
    const challenge = published(withDeadline(TIP_MS - 600_000));
    const { clock } = clockAt(5_000);
    const time = availabilityResponderL1BuildTime(clock);
    await buildAvailabilityResponderTransaction(
      lucid,
      deployment,
      { kind: "settle", challenge, tranche: challenge.tranches[0]! },
      time,
    );
    expect(windowLimits()).toBeUndefined();
    const settle = interval(SDK.buildSettleDaAvailabilityTrancheTxProgram);
    await buildAvailabilityResponderTransaction(
      lucid,
      deployment,
      { kind: "close", challenge },
      time,
    );
    expect(windowLimits()).toBeUndefined();
    const close = interval(SDK.buildCloseDaAvailabilityChallengeTxProgram);
    // The lookahead past the L1 clock, and the lower bound the longest
    // range allows (it would otherwise be backdated 60 s from the tip).
    for (const { validFrom, validTo } of [settle, close]) {
      expect(validTo).toBe(BigInt(TIP_MS + 5_000 + 60_000));
      expect(validFrom).toBe(BigInt(TIP_MS + 5_000 + 60_000 - 120_000));
    }
  });

  it("starts a settle of an expired active tranche at or after the response deadline", async () => {
    const deadlineMs = TIP_MS - 10_500;
    const challenge = withDeadline(deadlineMs);
    const { clock } = clockAt(5_000);
    await buildAvailabilityResponderTransaction(
      lucid,
      deployment,
      { kind: "settle", challenge, tranche: challenge.tranches[0]! },
      availabilityResponderL1BuildTime(clock),
    );
    expect(windowLimits()).toEqual({ lowerLimitMs: BigInt(deadlineMs) });
    const { validFrom } = interval(
      SDK.buildSettleDaAvailabilityTrancheTxProgram,
    );
    expect(validFrom).toBe(BigInt(TIP_MS - 10_000));
    expect(validFrom).toBeGreaterThanOrEqual(BigInt(deadlineMs));
  });
});

/** The plain responder's SDK run over a real SQLite journal, its context as
 * production's: leases on the wall clock, which the caller sets. */
const runnerScene = () => {
  const dir = mkdtempSync(join(tmpdir(), "availability-validity-"));
  dirs.push(dir);
  const journal = openAvailabilityOperationJournal(join(dir, "ops.sqlite"));
  const context: SDK.DaAvailabilityOperationContext = {
    actor: "44".repeat(28),
    deploymentIdentity: "55".repeat(32),
    stateQueuePolicyId: "66".repeat(28),
    journal,
    minimumConfirmationDepth: 10,
    transactionLimits: {
      maxTxSize: 16_384,
      maxTxExMem: 14_000_000n,
      maxTxExSteps: 10_000_000_000n,
      coinsPerUtxoByte: 4310n,
      feeCeilings: {},
    },
    assertActuationCurrent: vi.fn(async () => {}),
    observe: vi.fn(async () => ({ status: "unknown" as const, reason: "-" })),
    submit: vi.fn(async () => "00".repeat(32)),
  };
  return {
    context,
    pending: () => journal.pending(context.deploymentIdentity, context.actor),
    close: () => journal.close(),
  };
};

describe("an inadmissible window defers before signing", () => {
  it.each([
    [
      "not due: the tip trails the L1 clock by 117 s, past the anchored range",
      "ledger_tip_behind",
      () => publishOf(withDeadline(TIP_MS + 600_000)),
    ],
    [
      "refused: an active tranche settles only past a deadline beyond the lookahead",
      "no_validity_interval",
      () => {
        const challenge = withDeadline(TIP_MS + 600_000);
        return {
          kind: "settle" as const,
          challenge,
          tranche: challenge.tranches[0]!,
        };
      },
    ],
  ])("%s", async (_name, reason, actionOf) => {
    const { clock } = clockAt(117_000);
    vi.useFakeTimers({ toFake: ["Date"] });
    vi.setSystemTime(TIP_MS + 117_000);
    const s = runnerScene();
    try {
      await expect(
        runPlainAvailabilityResponderOperation(
          s.context,
          lucid,
          deployment,
          actionOf(),
          clock,
        ),
      ).rejects.toMatchObject({
        name: "AvailabilityResponderValidityDeferredError",
        reason,
      });
      expect(s.pending()).toEqual([]);
      expect(s.context.submit).not.toHaveBeenCalled();
      for (const builder of [
        SDK.buildPublishDaAvailabilityChunkTxProgram,
        SDK.buildSettleDaAvailabilityTrancheTxProgram,
      ])
        expect(builder).not.toHaveBeenCalled();
    } finally {
      s.close();
    }
  });
});

describe("the L1 deadline is checked again before the build returns", () => {
  it("never hands the runner a publication whose deadline passed on the L1 clock during the build", async () => {
    const deadlineMs = TIP_MS + 30_000;
    const { clock, advance } = clockAt(5_000);
    // The wall clock stays before the deadline: only the L1 check refuses.
    vi.useFakeTimers({ toFake: ["Date"] });
    vi.setSystemTime(TIP_MS + 5_000);
    const build = vi.mocked(SDK.buildPublishDaAvailabilityChunkTxProgram);
    const original = build.getMockImplementation()!;
    build.mockImplementationOnce((...args) => {
      advance(30_000);
      return original(...args);
    });
    const s = runnerScene();
    try {
      await expect(
        runPlainAvailabilityResponderOperation(
          s.context,
          lucid,
          deployment,
          publishOf(withDeadline(deadlineMs)),
          clock,
        ),
      ).rejects.toMatchObject({
        name: "AvailabilityResponderValidityDeferredError",
        reason: "deadline_passed",
      });
      expect(build).toHaveBeenCalledTimes(1);
      expect(s.pending()).toEqual([]);
      expect(s.context.submit).not.toHaveBeenCalled();
    } finally {
      s.close();
    }
  });
});

describe("promise-mode response validity", () => {
  // The tip was observed 5 s after it landed; the wall clock reads 25 s past
  // it, so the L1 clock trails the wall by 20 s.
  const wallMs = TIP_MS + 25_000;
  const network = () =>
    committeePromiseNetworkClock({
      slotConfig: lucid.config().slotConfig!,
      initialProducerMemberSkewMs: 2_000,
      maxWallMonotonicDriftMs: 2_000,
      nowMs: () => wallMs,
      monotonicMs: () => 0,
    });
  const promiseBuild = async (
    action: AvailabilityResponderAction,
    useNetworkClock: boolean,
  ) => {
    const { clock } = clockAt(5_000);
    const tx = await availabilityResponderTransactionOperation(
      lucid,
      deployment,
      action,
      useNetworkClock
        ? promiseResponseBuildTime(clock, network())
        : () => availabilityResponderL1BuildTime(clock),
    ).build(new AbortController().signal);
    return promiseResponseTtl(tx, network());
  };

  it("passes the fifty-slot TTL rule with the L1 clock 20 s behind the wall", async () => {
    const deadlineMs = TIP_MS + 600_000;
    const ttl = await promiseBuild(publishOf(withDeadline(deadlineMs)), true);
    expect(network().eligibleFutureSlots(ttl, 3_000)).toBeGreaterThanOrEqual(
      50,
    );
    const { validFrom, validTo } = interval(
      SDK.buildPublishDaAvailabilityChunkTxProgram,
    );
    expect(validFrom).toBeLessThanOrEqual(BigInt(TIP_MS));
    expect(validTo).toBe(BigInt(wallMs + 2_000 + 60_000));
    expect(windowLimits()).toEqual({ upperLimitMs: BigInt(deadlineMs) + 1n });
  });

  it("is refused by the TTL rule when the L1 clock picks the upper bound", async () => {
    await expect(
      promiseBuild(publishOf(withDeadline(TIP_MS + 600_000)), false),
    ).rejects.toThrow("Built response TTL has insufficient future slots");
  });

  it("decides the deadline on the L1 clock: a network clock past it defers, never misses", async () => {
    // The network clock's upper time is past the deadline; the L1 clock is not.
    await expect(
      promiseBuild(publishOf(withDeadline(TIP_MS + 20_000)), true),
    ).rejects.toMatchObject({
      name: "AvailabilityResponderValidityDeferredError",
      reason: "no_validity_interval",
    });
  });
});
