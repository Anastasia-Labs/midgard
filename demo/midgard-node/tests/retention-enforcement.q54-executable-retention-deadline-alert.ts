import { createHash } from "node:crypto";

import {
  MIDGARD_RETENTION_WINDOW,
  RETENTION_MS_PER_DAY,
} from "@al-ft/midgard-core";
import { MIDGARD_CONSENSUS_PROFILE_ID } from "@al-ft/midgard-core/consensus-profile";
import { SELECTED_DEPLOYMENT_PROFILE } from "@al-ft/midgard-core/deployment-profile";
import * as SDK from "@al-ft/midgard-sdk";
import { SqlClient } from "@effect/sql";
import { Effect } from "effect";
import { beforeAll, describe, expect, it } from "vitest";

import {
  evaluateRetentionCheck,
  parseRetentionAlertThresholdOption,
  retentionCheckExitCode,
} from "../src/commands/retention-check.js";
import {
  DaPayloadsDB,
  DaPayloadTerminalOutcomesDB,
} from "../src/database/index.js";
import { makeFinalizedDeploymentManifestFixture } from "./helpers/finalized-deployment-manifest.js";
import { deterministicFixtureBytes } from "./utils.js";

export const REQUIRED_RETENTION_MS =
  MIDGARD_RETENTION_WINDOW.requiredRetentionMs;

export const NOW = new Date("2026-08-03T00:00:00.000Z");

export let deploymentManifest: Awaited<
  ReturnType<typeof makeFinalizedDeploymentManifestFixture>
>;

// The fixture compiles and parameterizes the whole canonical contract set —
// 287 contracts as of this writing — so it runs well past Vitest's 10-second
// default hook budget. The budget is the fixture's, not a symptom: the suite's
// own assertions are pure and fast.
beforeAll(async () => {
  deploymentManifest = await makeFinalizedDeploymentManifestFixture();
}, 120_000);

describe("Q54 executable retention deadline alert", () => {
  const headerHash = "ab".repeat(28);
  const blockEndTimeMs = NOW.getTime() - REQUIRED_RETENTION_MS / 2;

  it("raises no deadline alert without an operator threshold", () => {
    // Opt-in alert (owner ruling 2026-09-26): with no threshold, not even a
    // record one millisecond from its deadline alerts. Pruning never removes
    // still-challengeable evidence, so ageing towards the deadline is no fault.
    const result = evaluateRetentionCheck({
      nowMillis: blockEndTimeMs + REQUIRED_RETENTION_MS - 1,
      records: [
        {
          headerHash,
          blockEndTimeMs,
          headerStatus: "attested",
          queueReference: "none",
        },
      ],
    });
    expect(result.stillChallengeable).toBe(1);
    expect(result.ok).toBe(true);
    expect(result.alerts).toEqual([]);
    expect(result.alertThresholdMs).toBeNull();
    expect(retentionCheckExitCode(result)).toBe(0);
  });

  it("exits 0 when every retained record has headroom", () => {
    const alertThresholdMs = REQUIRED_RETENTION_MS / 4;
    const result = evaluateRetentionCheck({
      nowMillis: NOW.getTime(),
      alertThresholdMs,
      records: [
        {
          headerHash,
          blockEndTimeMs,
          headerStatus: "attested",
          queueReference: "none",
        },
      ],
    });
    expect(result.stillChallengeable).toBe(1);
    expect(result.ok).toBe(true);
    expect(result.alerts).toEqual([]);
    expect(retentionCheckExitCode(result)).toBe(0);
    expect(result.alertThresholdMs).toBe(alertThresholdMs);
    expect(result.requiredRetentionMs).toBe(REQUIRED_RETENTION_MS);
    expect(result.deployedRetentionMs).toBe(
      MIDGARD_RETENTION_WINDOW.retentionDays * RETENTION_MS_PER_DAY,
    );
    expect(result.marginMs).toBe(
      result.deployedRetentionMs - REQUIRED_RETENTION_MS,
    );
  });

  it("alerts at zero remaining headroom but not at one millisecond", () => {
    const at = (remainingMs: number) =>
      evaluateRetentionCheck({
        nowMillis: blockEndTimeMs + REQUIRED_RETENTION_MS - remainingMs,
        alertThresholdMs: 0,
        records: [
          {
            headerHash,
            blockEndTimeMs,
            headerStatus: "attested",
            queueReference: "none",
          },
        ],
      });
    expect(at(0).ok).toBe(false);
    expect(retentionCheckExitCode(at(0))).toBe(1);
    expect(at(1).ok).toBe(true);
    expect(retentionCheckExitCode(at(1))).toBe(0);
  });

  it("retains and alerts on a deployment fingerprint mismatch", () => {
    const result = evaluateRetentionCheck({
      nowMillis: NOW.getTime(),
      expectedDeploymentFingerprint: "aa".repeat(32),
      records: [
        {
          headerHash,
          blockEndTimeMs,
          headerStatus: "attested",
          queueReference: "none",
          deploymentFingerprint: "bb".repeat(32),
        },
      ],
    });
    expect(result.ok).toBe(false);
    expect(result.stillChallengeable).toBe(1);
    expect(result.alerts[0]?.reasonCode).toBe(
      "deployment_fingerprint_mismatch",
    );
    expect(retentionCheckExitCode(result)).toBe(1);
  });

  it("counts only still-challengeable records toward the deadline alert", () => {
    const pastHorizon = NOW.getTime() - REQUIRED_RETENTION_MS - 1;
    const result = evaluateRetentionCheck({
      nowMillis: NOW.getTime(),
      alertThresholdMs: REQUIRED_RETENTION_MS,
      records: [
        {
          headerHash: "01".repeat(28),
          blockEndTimeMs: pastHorizon,
          headerStatus: "unobserved",
          queueReference: "none",
        },
        {
          headerHash: "02".repeat(28),
          blockEndTimeMs: pastHorizon,
          headerStatus: "merged",
          queueReference: "confirmed_head",
        },
        {
          headerHash: "03".repeat(28),
          blockEndTimeMs,
          headerStatus: "removed",
          queueReference: "none",
        },
        {
          headerHash: "04".repeat(28),
          blockEndTimeMs,
          headerStatus: "unobserved",
          queueReference: "none",
        },
      ],
    });
    expect(result.checked).toBe(4);
    expect(result.stillChallengeable).toBe(1);
    expect(result.alerts.map((alert) => alert.headerHash)).toEqual([
      "04".repeat(28),
    ]);
  });

  it("rejects malformed alert thresholds", () => {
    for (const bad of [Number.NaN, -1, 1.5, 2 ** 53]) {
      expect(() =>
        evaluateRetentionCheck({
          nowMillis: NOW.getTime(),
          alertThresholdMs: bad,
          records: [],
        }),
      ).toThrow(/alertThresholdMs/u);
    }
  });

  it("accepts --alert-threshold-ms only below the merged-payload window", () => {
    // A header merges no earlier than block maturity after its end time, so a
    // merged payload has at most the horizon minus maturity left.
    const mergedWindowMs =
      REQUIRED_RETENTION_MS - MIDGARD_RETENTION_WINDOW.maturityMs;
    expect(parseRetentionAlertThresholdOption(undefined)).toBeUndefined();
    expect(
      parseRetentionAlertThresholdOption((mergedWindowMs - 1).toString()),
    ).toBe(mergedWindowMs - 1);
    // At the window, or anywhere up to the horizon, every merged payload
    // alerts from the moment it merges.
    for (const refused of [mergedWindowMs, REQUIRED_RETENTION_MS - 1]) {
      expect(() =>
        parseRetentionAlertThresholdOption(refused.toString()),
      ).toThrow(
        /--alert-threshold-ms=\d+ must be below the merged-payload window/u,
      );
    }
    for (const bad of ["-1", "1.5", "soon", ""]) {
      expect(() => parseRetentionAlertThresholdOption(bad)).toThrow(
        /--alert-threshold-ms must be a non-negative integer/u,
      );
    }
  });
});

describe("Q54 authenticated release retention authority", () => {
  it("admits the deployment identity and finality depth", () => {
    expect(
      DaPayloadTerminalOutcomesDB.admitDaPayloadRetentionReleaseAuthority(
        deploymentManifest,
      ),
    ).toMatchObject({
      minimumFinalityDepth: BigInt(
        SELECTED_DEPLOYMENT_PROFILE.l1_finality.confirmation_depth,
      ),
    });
  });

  it.each(["active", "unknown"] as const)(
    "rejects caller-authored Q58 %s state",
    (state) => {
      expect(
        DaPayloadTerminalOutcomesDB.admitDaPayloadRetentionReleaseAuthority({
          ...deploymentManifest,
          availabilityChallenges: { state },
        }),
      ).toBeNull();
    },
  );
});

export const dbEnabled = process.env.MIDGARD_SKIP_DB_TESTS !== "1";

export const daPayloadFixture = (
  label: string,
  blockEndTime: Date,
): DaPayloadsDB.InsertInput => {
  const headerHash = deterministicFixtureBytes(`retention-${label}`, 28);
  const payload = deterministicFixtureBytes(`retention-payload-${label}`, 64);
  return {
    [DaPayloadsDB.Columns.HEADER_HASH]: headerHash,
    [DaPayloadsDB.Columns.CONSENSUS_PROFILE_ID]: MIDGARD_CONSENSUS_PROFILE_ID,
    [DaPayloadsDB.Columns.VERSION]: 1,
    [DaPayloadsDB.Columns.PAYLOAD_CBOR]: payload,
    [DaPayloadsDB.Columns.PAYLOAD_SHA256]: createHash("sha256")
      .update(payload)
      .digest(),
    [DaPayloadsDB.Columns.UTXOS_ROOT]: SDK.EMPTY_MERKLE_TREE_ROOT,
    [DaPayloadsDB.Columns.FORCED_TRANSACTIONS_ROOT]: SDK.EMPTY_MERKLE_TREE_ROOT,
    [DaPayloadsDB.Columns.TRANSACTIONS_ROOT]: SDK.EMPTY_MERKLE_TREE_ROOT,
    [DaPayloadsDB.Columns.DEPOSITS_ROOT]: SDK.EMPTY_MERKLE_TREE_ROOT,
    [DaPayloadsDB.Columns.WITHDRAWALS_ROOT]: SDK.EMPTY_MERKLE_TREE_ROOT,
    [DaPayloadsDB.Columns.TRANSITION_TRACE_ROOT]: SDK.EMPTY_MERKLE_TREE_ROOT,
    [DaPayloadsDB.Columns.EVENT_TO_STEP_ROOT]: SDK.EMPTY_MERKLE_TREE_ROOT,
    [DaPayloadsDB.Columns.VALIDATION_TRACES_ROOT]: SDK.EMPTY_MERKLE_TREE_ROOT,
    [DaPayloadsDB.Columns.WITHDRAWAL_COUNT]: 0n,
    [DaPayloadsDB.Columns.FORCED_TRANSACTION_COUNT]: 0n,
    [DaPayloadsDB.Columns.L2_TRANSACTION_COUNT]: 0n,
    [DaPayloadsDB.Columns.DEPOSIT_COUNT]: 0n,
    [DaPayloadsDB.Columns.TOTAL_EVENT_COUNT]: 0n,
    [DaPayloadsDB.Columns.TRANSITION_STEP_COUNT]: 0n,
    [DaPayloadsDB.Columns.VALIDATION_TRACE_COUNT]: 0n,
    [DaPayloadsDB.Columns.BLOCK_START_TIME]: new Date(
      blockEndTime.getTime() - 1_000,
    ),
    [DaPayloadsDB.Columns.BLOCK_END_TIME]: blockEndTime,
  };
};

/** Seeds one row with an explicit created_at (bypassing the DEFAULT). */
export const seedPayload = (
  label: string,
  blockEndTime: Date,
  createdAt: Date,
): Effect.Effect<Buffer, unknown, SqlClient.SqlClient> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const row = daPayloadFixture(label, blockEndTime);
    yield* DaPayloadsDB.upsertAvailable(row);
    const headerHash = row[DaPayloadsDB.Columns.HEADER_HASH];
    yield* sql`UPDATE da_payloads SET created_at = ${createdAt} WHERE header_hash = ${headerHash}`;
    return headerHash;
  });

export const h32 = (byte: string): string => byte.repeat(64);
