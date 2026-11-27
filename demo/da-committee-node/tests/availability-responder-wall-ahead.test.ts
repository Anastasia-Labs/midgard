import { mkdtempSync, rmSync } from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";

import { openAvailabilityOperationJournal } from "@al-ft/midgard-core/availability-operation-journal";
import * as SDK from "@al-ft/midgard-sdk";
import type { LucidEvolution, UTxO } from "@lucid-evolution/lucid";
import { afterEach, describe, expect, it, vi } from "vitest";

import { runPlainAvailabilityResponderOperation } from "../src/availability/plain-response-run.js";
import type { AvailabilityResponderAction } from "../src/availability/responder.js";
import {
  payload,
  withResponseDeadline,
} from "./helpers/availability-challenge.js";
import { l1ClockAt } from "./helpers/l1-clock.js";

/**
 * The plain responder's deadline is decided on the committee's L1 clock
 * only: a wall clock ahead of it cannot refuse a publication the L1 clock
 * still admits. The builder hands back a real unsigned body, and signing
 * rejects with a marker, so the run stops exactly where signing starts.
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
    buildPublishDaAvailabilityChunkTxProgram: vi.fn(() =>
      Effect.succeed({
        tx: {
          toTransaction: () => ({
            body: () =>
              CML.TransactionBody.new(
                CML.TransactionInputList.new(),
                CML.TransactionOutputList.new(),
                0n,
              ),
          }),
          sign: { withWallet: () => ({ complete }) },
        },
      }),
    ),
  };
});

const ZERO_TIME = 1_700_000_000_000;
const TIP_SLOT = 10_000;
const TIP_MS = ZERO_TIME + TIP_SLOT * 1_000;

const lucid = {
  config: () => ({
    slotConfig: { zeroTime: ZERO_TIME, zeroSlot: 0, slotLength: 1_000 },
    protocolParameters: { collateralPercentage: 150 },
  }),
  unixTimeToSlot: (ms: number) => Math.floor((ms - ZERO_TIME) / 1_000),
  slotToUnixTime: (slot: number) => ZERO_TIME + slot * 1_000,
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

const dirs: string[] = [];
afterEach(() => {
  vi.useRealTimers();
  for (const dir of dirs.splice(0)) rmSync(dir, { recursive: true });
});

describe("plain responder deadline with the wall clock ahead of L1", () => {
  it("builds the publication and reaches signing with the wall 30 s ahead and the deadline 10 s ahead on L1", async () => {
    // L1 "now" is the tip plus 5 s; the deadline is 10 s past it.
    const { clock } = l1ClockAt({
      slotConfig: lucid.config().slotConfig!,
      tipSlot: TIP_SLOT,
      elapsedMs: 5_000,
    });
    const l1NowMs = TIP_MS + 5_000;
    const challenge = withResponseDeadline(l1NowMs + 10_000);
    const action: AvailabilityResponderAction = {
      kind: "publish",
      challenge,
      tranche: challenge.tranches[0]!,
      publication: SDK.planDaAvailabilityPublications({
        commitment: challenge.record.datum.commitment,
        challengeAssetName: challenge.record.datum.challenge_asset_name,
        payload,
      })[0]!.publications[0]!,
    };
    // The wall clock is 30 s ahead of L1: 20 s past the deadline.
    vi.useFakeTimers({ toFake: ["Date"] });
    vi.setSystemTime(l1NowMs + 30_000);
    const dir = mkdtempSync(join(tmpdir(), "availability-wall-ahead-"));
    dirs.push(dir);
    const journal = openAvailabilityOperationJournal(join(dir, "ops.sqlite"));
    const context: SDK.DaAvailabilityOperationContext = {
      actor: "44".repeat(28),
      hubOraclePolicyId: "55".repeat(28),
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
          action,
          clock,
        ),
      ).rejects.toThrow("signing reached");
      expect(
        SDK.buildPublishDaAvailabilityChunkTxProgram,
      ).toHaveBeenCalledOnce();
      expect(signing).toHaveBeenCalledOnce();
      expect(context.submit).not.toHaveBeenCalled();
    } finally {
      journal.close();
    }
  });
});
