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
} from "../src/availability/factory.discover-availability-responder-challenges.js";
import { runPlainAvailabilityResponderOperation } from "../src/availability/plain-response-run.js";
import { committeePromiseNetworkClock } from "../src/availability/promise-network-clock.js";
import {
  PROMISE_RESPONSE_BUILD_STAGE_MS,
  PROMISE_RESPONSE_MIN_FUTURE_SLOTS,
  PROMISE_RESPONSE_PROPAGATION_MS,
  promiseResponseBuildTime,
  promiseResponseTtl,
} from "../src/availability/promise-response-build.js";
import {
  AvailabilityResponder,
  type AvailabilityResponderAction,
  type AvailabilityResponderReport,
} from "../src/availability/responder.js";
import {
  AVAILABILITY_RESPONDER_RETRY_POLICY,
  createAvailabilityResponseLoop,
} from "../src/availability-response-loop.js";
import { committeeL1NowMs } from "../src/l1/follower/l1-clock.js";
import {
  deploymentFingerprint,
  deploymentIdentity,
  payload,
  record,
  withResponseDeadline,
} from "./helpers/availability-challenge.js";
import { l1ClockAt } from "./helpers/l1-clock.js";

/**
 * A plain responder whose observed tip trails its L1 clock past the
 * backdate: the interval ending at the deadline within the validator's
 * 120 s range would start past the tip, so it is anchored on the tip
 * instead, and the publication is built and signed now rather than after the
 * next block. The builder hands back a real unsigned body and records the
 * interval it is given; signing rejects with a marker, so the run stops
 * exactly where signing starts.
 */
vi.mock("@al-ft/midgard-sdk", async (importOriginal) => {
  const actual = await importOriginal<typeof import("@al-ft/midgard-sdk")>();
  const { Effect } = await import("effect");
  const { CML } = await import("@lucid-evolution/lucid");
  const complete = vi.fn(async () => {
    throw new Error("signing reached");
  });
  return {
    ...actual,
    signingCompleted: complete,
    buildPublishDaAvailabilityChunkTxProgram: vi.fn(
      (_lucid: unknown, _deployment: unknown, input: { validTo: bigint }) =>
        Effect.succeed({
          tx: {
            toTransaction: () => ({
              body: () => {
                const body = CML.TransactionBody.new(
                  CML.TransactionInputList.new(),
                  CML.TransactionOutputList.new(),
                  0n,
                );
                // The exclusive TTL is the upper bound's slot.
                body.set_ttl((input.validTo - BigInt(ZERO_TIME)) / 1_000n);
                return body;
              },
            }),
            sign: { withWallet: () => ({ complete }) },
          },
        }),
    ),
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

/** The follower tip at `TIP_SLOT`, observed `lagMs` ago on the monotonic clock. */
const clockAt = (lagMs: number) =>
  l1ClockAt({
    slotConfig: lucid.config().slotConfig!,
    tipSlot: TIP_SLOT,
    elapsedMs: lagMs,
  });

const publishOf = (deadlineMs: number): AvailabilityResponderAction => {
  const challenge = withResponseDeadline(deadlineMs);
  return {
    kind: "publish",
    challenge,
    tranche: challenge.tranches[0]!,
    publication: SDK.planDaAvailabilityPublications({
      commitment: challenge.record.datum.commitment,
      challengeAssetName: challenge.record.datum.challenge_asset_name,
      payload,
    })[0]!.publications[0]!,
  };
};

/** The interval the publication builder was last handed. */
const builtInterval = () => {
  const calls = vi.mocked(SDK.buildPublishDaAvailabilityChunkTxProgram).mock
    .calls;
  const input = calls[calls.length - 1]![2] as {
    validFrom: bigint;
    validTo: bigint;
  };
  return { validFrom: input.validFrom, validTo: input.validTo };
};

const dirs: string[] = [];
afterEach(() => {
  vi.useRealTimers();
  for (const dir of dirs.splice(0)) rmSync(dir, { recursive: true });
});

describe("a tip trailing the L1 clock anchors the publication window on it", () => {
  it.each([
    [61_000, 59_000],
    [90_000, 30_000],
    [116_000, 30_000],
  ])(
    "builds the publish with the tip %d ms behind the L1 clock and the deadline %d ms past it",
    async (lagMs, deadlineAfterMs) => {
      const deadlineMs = TIP_MS + lagMs + deadlineAfterMs;
      const { clock } = clockAt(lagMs);
      await availabilityResponderTransactionOperation(
        lucid,
        deployment,
        publishOf(deadlineMs),
        () => availabilityResponderL1BuildTime(clock),
      ).build(new AbortController().signal);
      const { validFrom, validTo } = builtInterval();
      // The submit margin before the tip, and the longest range from it.
      expect(validFrom).toBe(BigInt(slotStart(TIP_SLOT - 2)));
      expect(validTo).toBe(BigInt(slotStart(TIP_SLOT - 2 + 120)));
      expect(validTo).toBeLessThanOrEqual(BigInt(deadlineMs) + 1n);
      // Still includable after the L1 clock's slot.
      expect(validTo).toBeGreaterThanOrEqual(BigInt(TIP_MS + lagMs + 2_000));
      expect(validTo - validFrom).toBeLessThanOrEqual(
        SDK.DA_AVAILABILITY_MAX_VALIDITY_RANGE_MS,
      );
    },
  );

  it("signs the publish now: a tip 61 s behind the L1 clock, the deadline 59 s ahead of it", async () => {
    const l1NowMs = TIP_MS + 61_000;
    const deadlineMs = l1NowMs + 59_000;
    const { clock } = clockAt(61_000);
    vi.useFakeTimers({ toFake: ["Date"] });
    vi.setSystemTime(l1NowMs);
    const dir = mkdtempSync(join(tmpdir(), "availability-lagging-tip-"));
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
    const signing = (
      SDK as unknown as { signingCompleted: ReturnType<typeof vi.fn> }
    ).signingCompleted;
    try {
      await expect(
        runPlainAvailabilityResponderOperation(
          context,
          lucid,
          deployment,
          publishOf(deadlineMs),
          clock,
        ),
      ).rejects.toThrow("signing reached");
      expect(
        SDK.buildPublishDaAvailabilityChunkTxProgram,
      ).toHaveBeenCalledOnce();
      expect(signing).toHaveBeenCalledOnce();
      expect(context.submit).not.toHaveBeenCalled();
      const { validFrom, validTo } = builtInterval();
      expect(validFrom).toBeLessThanOrEqual(BigInt(slotStart(TIP_SLOT - 2)));
      expect(validTo).toBeGreaterThan(BigInt(l1NowMs));
      expect(validTo).toBeLessThanOrEqual(BigInt(deadlineMs) + 1n);
      expect(validTo - validFrom).toBeLessThanOrEqual(
        SDK.DA_AVAILABILITY_MAX_VALIDITY_RANGE_MS,
      );
    } finally {
      journal.close();
    }
  });
});

/**
 * Promise mode anchors on a lagging tip only while the anchored TTL still
 * clears the fifty-slot rule at the build stage's end, the wall stepped as
 * far as the drift bound allows; short of that the step stays pending until
 * the next block, as before, and never fails. The step builds the response
 * as the promise runtime does and checks its TTL on the adopted network
 * clock, whose upper bound (the wall plus the adopted skew) is the L1
 * clock's now, `lagMs` past the tip. With the 3 s propagation, 2 s stage
 * and 2 s drift allowances, the anchored TTL (118 slots past the tip) is
 * taken up to a lag of 60 s.
 */
describe("a promise response on a tip trailing the L1 clock", () => {
  /** With `wallStepMs`, the TTL is checked at the build stage's end, the
   * wall stepped that far past the monotonic clock. */
  const promiseResponderAt = (lagMs: number, wallStepMs?: number) => {
    const { clock: l1Clock } = clockAt(lagMs);
    let wallMs = TIP_MS + lagMs - 2_000;
    let monotonicMs = 0;
    const network = committeePromiseNetworkClock({
      slotConfig: lucid.config().slotConfig!,
      initialProducerMemberSkewMs: 2_000,
      maxWallMonotonicDriftMs: 2_000,
      nowMs: () => wallMs,
      monotonicMs: () => monotonicMs,
    });
    const ttls: number[] = [];
    const reports: AvailabilityResponderReport[] = [];
    const lines: string[] = [];
    const responder = new AvailabilityResponder({
      deploymentFingerprint,
      deploymentIdentity,
      store: { getDaPayload: async () => record },
      reconcile: async () => "ready",
      discover: async () => [withResponseDeadline(TIP_MS + 180_000)],
      execute: async (action) => {
        const tx = await availabilityResponderTransactionOperation(
          lucid,
          deployment,
          action,
          promiseResponseBuildTime(l1Clock, network),
        ).build(new AbortController().signal);
        if (wallStepMs !== undefined) {
          monotonicMs += PROMISE_RESPONSE_BUILD_STAGE_MS;
          wallMs += PROMISE_RESPONSE_BUILD_STAGE_MS + wallStepMs;
        }
        ttls.push(promiseResponseTtl(tx, network));
        return "pending";
      },
      now: () => committeeL1NowMs(l1Clock),
    });
    const loop = createAvailabilityResponseLoop({
      drain: async () => {
        const report = await responder.drain();
        reports.push(report);
        return report;
      },
      write: (_stream, line) => lines.push(line),
      pollIntervalMs: 15_000,
    });
    /** Past the retry burst: enough drains to exhaust it were they failing. */
    const runPastRetryBurst = async () => {
      for (let i = 0; i < AVAILABILITY_RESPONDER_RETRY_POLICY.retries + 2; i++)
        await loop.run();
    };
    return { network, ttls, reports, lines, loop, runPastRetryBurst };
  };

  it.each([59_000, 60_000])(
    "builds the response now with a tip %d ms behind, its TTL clearing the fifty-slot rule to the build stage's end",
    async (lagMs) => {
      const step = promiseResponderAt(lagMs);
      await step.runPastRetryBurst();
      expect(step.reports.map((report) => report.status)).toEqual(
        Array(AVAILABILITY_RESPONDER_RETRY_POLICY.retries + 2).fill("pending"),
      );
      // Built on every drain, not deferred to the next block.
      expect(
        SDK.buildPublishDaAvailabilityChunkTxProgram,
      ).toHaveBeenCalledTimes(AVAILABILITY_RESPONDER_RETRY_POLICY.retries + 2);
      expect(step.reports[0]).toMatchObject({ action: "publish" });
      const { validFrom, validTo } = builtInterval();
      expect(validFrom).toBe(BigInt(slotStart(TIP_SLOT - 2)));
      expect(validTo).toBe(BigInt(slotStart(TIP_SLOT - 2 + 120)));
      const ttl = step.ttls[0]!;
      expect(ttl).toBe(TIP_SLOT - 2 + 120);
      // The signed body's TTL is checked again until the build stage ends,
      // the wall up to the drift bound past the monotonic clock.
      expect(
        step.network.eligibleFutureSlots(
          ttl,
          PROMISE_RESPONSE_PROPAGATION_MS +
            PROMISE_RESPONSE_BUILD_STAGE_MS +
            step.network.maxWallMonotonicDriftMs,
        ),
      ).toBeGreaterThanOrEqual(PROMISE_RESPONSE_MIN_FUTURE_SLOTS);
      expect(step.loop.reasons()).toEqual([]);
    },
  );

  it.each([61_000, 90_000])(
    "stays pending, never failed, with a tip %d ms behind: the anchored TTL is too short",
    async (lagMs) => {
      const step = promiseResponderAt(lagMs);
      await step.runPastRetryBurst();
      expect(step.loop.reasons()).toEqual([]);
      expect(step.lines.join("")).not.toMatch(/failed|retries_exhausted/);
      expect(step.reports).toHaveLength(
        AVAILABILITY_RESPONDER_RETRY_POLICY.retries + 2,
      );
      for (const report of step.reports)
        expect(report).toMatchObject({
          status: "pending",
          action: "publish",
          detail: expect.stringContaining(
            "has not reached the interval's lower bound",
          ),
        });
      // Deferred before anything is built.
      expect(
        SDK.buildPublishDaAvailabilityChunkTxProgram,
      ).not.toHaveBeenCalled();
      expect(step.ttls).toEqual([]);
    },
  );

  it.each([59_000, 60_000, 61_000, 62_000])(
    "never fails with a tip %d ms behind and the wall stepped the drift bound ahead during the build stage",
    async (lagMs) => {
      const step = promiseResponderAt(lagMs, 2_000);
      await step.loop.run();
      expect(step.reports).toHaveLength(1);
      expect(step.reports[0]).toMatchObject({
        status: "pending",
        action: "publish",
      });
      // Built and its TTL passed the rule at the stage's end, or deferred
      // before anything was built.
      expect(step.ttls).toHaveLength(
        vi.mocked(SDK.buildPublishDaAvailabilityChunkTxProgram).mock.calls
          .length,
      );
      expect(step.ttls.length).toBe(lagMs <= 60_000 ? 1 : 0);
      expect(step.loop.reasons()).toEqual([]);
    },
  );
});
