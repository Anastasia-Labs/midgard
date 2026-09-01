import { createHash } from "node:crypto";

import {
  MIDGARD_RETENTION_WINDOW_V1,
  RETENTION_MS_PER_DAY_V1,
} from "@al-ft/midgard-core";
import { MIDGARD_CONSENSUS_PROFILE_V1_ID } from "@al-ft/midgard-core/consensus-profile-v1";
import * as SDK from "@al-ft/midgard-sdk";
import { SqlClient } from "@effect/sql";
import { Data } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { beforeAll, describe, expect, it } from "vitest";

import { evaluateReadiness } from "@/commands/readiness.js";
import {
  evaluateRetentionCheck,
  retentionCheckExitCode,
} from "@/commands/retention-check.js";
import { DaPayloadsDB, DaPayloadTerminalOutcomesDB } from "@/database/index.js";
import {
  computeChallengeableCutoff,
  computeRetentionCutoff,
} from "@/database/retention-policy.js";

import * as MigrationRunner from "../src/database/migrations/runner.js";
import { makeFinalizedDeploymentManifestV1Fixture } from "./helpers/finalized-deployment-manifest-v1.js";
import { deterministicFixtureBytes, provideDatabaseLayers } from "./utils.js";

const REQUIRED_RETENTION_MS = MIDGARD_RETENTION_WINDOW_V1.requiredRetentionMs;
const NOW = new Date("2026-08-03T00:00:00.000Z");
let deploymentManifest: Awaited<
  ReturnType<typeof makeFinalizedDeploymentManifestV1Fixture>
>;

beforeAll(async () => {
  deploymentManifest = await makeFinalizedDeploymentManifestV1Fixture();
});

const readinessBase = {
  nowMillis: NOW.getTime(),
  maxHeartbeatAgeMs: 60_000,
  maxQueueDepth: 10,
  queueDepth: 0,
  workerHeartbeats: {
    blockCommitment: NOW.getTime(),
    blockConfirmation: NOW.getTime(),
    merge: NOW.getTime(),
    depositFetch: NOW.getTime(),
    withdrawalFetch: NOW.getTime(),
    txQueueProcessor: NOW.getTime(),
  },
  localFinalizationPending: false,
  unresolvedBlockSubmissionAgeMs: 0,
  maxUnresolvedBlockSubmissionAgeMs: 60_000,
  dbHealthy: true,
  awaitingForeignTipReconciliations: 0,
};

describe("Q54 executable retention deadline alert", () => {
  const headerHash = "ab".repeat(28);
  const blockEndTimeMs = NOW.getTime() - REQUIRED_RETENTION_MS / 2;

  it("exits 0 when every retained record has headroom", () => {
    const result = evaluateRetentionCheck({
      nowMillis: NOW.getTime(),
      records: [{ headerHash, blockEndTimeMs, headerStatus: "attested" }],
    });
    expect(result.ok).toBe(true);
    expect(result.alerts).toEqual([]);
    expect(retentionCheckExitCode(result)).toBe(0);
    expect(result.requiredRetentionMs).toBe(907_200_000);
    expect(result.deployedRetentionMs).toBe(1_296_000_000);
    expect(result.marginMs).toBe(388_800_000);
    expect(result.alertThresholdMs).toBe(388_800_000);
  });

  it("alerts at zero remaining headroom but not at one millisecond", () => {
    const at = (remainingMs: number) =>
      evaluateRetentionCheck({
        nowMillis: blockEndTimeMs + REQUIRED_RETENTION_MS - remainingMs,
        alertThresholdMs: 0,
        records: [{ headerHash, blockEndTimeMs, headerStatus: "attested" }],
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

  it("fails closed on missing block end time and unknown status", () => {
    const missing = evaluateRetentionCheck({
      nowMillis: NOW.getTime(),
      records: [{ headerHash, blockEndTimeMs: null, headerStatus: "merged" }],
    });
    expect(missing.ok).toBe(false);
    expect(missing.alerts[0]?.reasonCode).toBe("missing_block_end_time");

    const unknown = evaluateRetentionCheck({
      nowMillis: NOW.getTime(),
      records: [{ headerHash, blockEndTimeMs, headerStatus: "not-a-status" }],
    });
    expect(unknown.ok).toBe(false);
    expect(unknown.alerts[0]?.reasonCode).toBe("header_status_unknown");
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

  it("surfaces one retention reason through node readiness", () => {
    expect(evaluateReadiness({ ...readinessBase }).ready).toBe(true);
    const alerting = evaluateReadiness({
      ...readinessBase,
      retentionDeadlineAlerts: 2,
    });
    expect(alerting.ready).toBe(false);
    expect(alerting.reasons).toContain("retention_deadline_alert:2");
    expect(
      evaluateReadiness({ ...readinessBase, retentionDeadlineAlerts: 0 })
        .reasons,
    ).not.toContain("retention_deadline_alert:0");
  });
});

describe("Q54 authenticated release retention authority", () => {
  it("treats missing Q58 capability as a fail-closed retention hold", () => {
    expect(
      DaPayloadTerminalOutcomesDB.admitDaPayloadRetentionReleaseAuthorityV1(
        deploymentManifest,
      ),
    ).toMatchObject({
      availabilityChallengeCapability: "missing",
      minimumFinalityDepth: 30n,
    });
  });

  it.each(["active", "unknown"] as const)(
    "rejects caller-authored Q58 %s state",
    (state) => {
      expect(
        DaPayloadTerminalOutcomesDB.admitDaPayloadRetentionReleaseAuthorityV1({
          ...deploymentManifest,
          availabilityChallenges: { state },
        }),
      ).toBeNull();
    },
  );
});

const dbEnabled = process.env.MIDGARD_SKIP_DB_TESTS !== "1";

const daPayloadFixture = (
  label: string,
  blockEndTime: Date,
): DaPayloadsDB.InsertInput => {
  const headerHash = deterministicFixtureBytes(`retention-${label}`, 28);
  const payload = deterministicFixtureBytes(`retention-payload-${label}`, 64);
  return {
    [DaPayloadsDB.Columns.HEADER_HASH]: headerHash,
    [DaPayloadsDB.Columns.CONSENSUS_PROFILE_ID]:
      MIDGARD_CONSENSUS_PROFILE_V1_ID,
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
const seedPayload = (
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

const h32 = (byte: string): string => byte.repeat(64);
const terminalMerge = (
  headerHash: Buffer,
  sequence: number,
): SDK.StateQueueAuthenticatedTransitionV1 => {
  const policyId = deploymentManifest.contracts.stateQueueMint.scriptHash;
  const transactionHash = h32(sequence.toString(16));
  const rootOutRef = `${h32("0")}#0`;
  const headerOutRef = `${h32((sequence + 4).toString(16))}#0`;
  const redeemer = {
    MergeToConfirmedStateV1: {
      yield_to_ref_input_index: 0n,
      header_node_key: headerHash.toString("hex"),
      confirmed_state_input_outref: {
        transactionId: h32("0"),
        outputIndex: 0n,
      },
      confirmed_state_output_index: 0n,
      m_settlement_redeemer_index: null,
      merged_block_withdrawals_root: h32("1"),
      merged_block_forced_transactions_root: h32("2"),
      merged_block_transactions_root: h32("3"),
      merged_block_deposits_root: h32("4"),
      merged_block_transition_trace_root: h32("5"),
      merged_block_event_to_step_root: h32("6"),
      merged_block_validation_traces_root: h32("7"),
      merged_block_withdrawal_count: 0n,
      merged_block_forced_transaction_count: 0n,
      merged_block_l2_transaction_count: 0n,
      merged_block_deposit_count: 0n,
      merged_block_total_event_count: 0n,
      merged_block_transition_step_count: 0n,
      merged_block_validation_trace_count: 0n,
    },
  } as const;
  const transition = SDK.deriveStateQueueAuthenticatedTransitionV1({
    deploymentIdentityDigest: deploymentManifest.manifestId,
    stateQueuePolicyId: policyId,
    transactionHash,
    blockHash: h32((sequence + 8).toString(16)),
    slot: (100 + sequence).toString(),
    blockNo: (90 + sequence).toString(),
    transactionIndex: "0",
    chainPointId: h32((sequence + 12).toString(16)),
    finalityDepth: deploymentManifest.l1Finality.confirmationDepth.toString(),
    mintPolicyIds: [policyId],
    referenceInputOutRefs: [`${h32("f")}#0`],
    correctionLockWitness: {
      kind: "idle_reference",
      referenceOutRef: `${h32("f")}#0`,
      datum: "Idle",
    },
    redeemers: [
      {
        purpose: "mint",
        index: "0",
        cborHex: Data.to(redeemer, SDK.StateQueueRedeemer),
      },
    ],
    spentInputOutRefs: [rootOutRef, headerOutRef],
    previousQueue: [
      { headerHash: null, outRef: rootOutRef },
      { headerHash: headerHash.toString("hex"), outRef: headerOutRef },
    ],
    nextQueue: [{ headerHash: null, outRef: `${transactionHash}#0` }],
  });
  if (transition === null) throw new Error("invalid terminal merge fixture");
  return transition;
};

const seedTerminal = (
  headerHash: Buffer,
  sequence: number,
): Effect.Effect<void, unknown, SqlClient.SqlClient> =>
  DaPayloadTerminalOutcomesDB.recordAuthenticatedTransitionV1(
    terminalMerge(headerHash, sequence),
    deploymentManifest,
  );

const countRows = Effect.gen(function* () {
  const sql = yield* SqlClient.SqlClient;
  const rows = yield* sql<{
    readonly count: string;
  }>`SELECT COUNT(*)::text AS count FROM da_payloads`;
  return Number(rows[0]?.count ?? "0");
});

describe.skipIf(!dbEnabled)(
  "Q54 challengeability-aware DA payload pruning",
  () => {
    beforeAll(async () => {
      await Effect.runPromise(
        provideDatabaseLayers(
          Effect.gen(function* () {
            const sql = yield* SqlClient.SqlClient;
            yield* sql`
              DROP SCHEMA public CASCADE;
              CREATE SCHEMA public;`;
            yield* MigrationRunner.migrate({
              appVersion: "test",
              actor: "retention-enforcement-v1.test",
            });
          }),
        ) as Effect.Effect<void, never, never>,
      );
    }, 120_000);

    const run = <A>(effect: Effect.Effect<A, unknown, SqlClient.SqlClient>) =>
      Effect.runPromise(
        provideDatabaseLayers(
          Effect.gen(function* () {
            yield* DaPayloadTerminalOutcomesDB.clear;
            yield* DaPayloadsDB.clear;
            const result = yield* effect;
            return result;
          }),
        ) as Effect.Effect<A, never, never>,
      );

    it("retains a 16-day-old terminal record while Q58 authority is missing", async () => {
      const days16 = new Date(NOW.getTime() - 16 * RETENTION_MS_PER_DAY_V1);
      const deleted = await run(
        Effect.gen(function* () {
          const headerHash = yield* seedPayload("expired", days16, days16);
          yield* seedTerminal(headerHash, 1);
          return yield* DaPayloadsDB.pruneOlderThan(
            computeRetentionCutoff(NOW, 15),
            computeChallengeableCutoff(NOW),
            deploymentManifest,
          );
        }),
      );
      expect(deleted).toBe(0);
    });

    it("retains a block_end_time exactly at the cutoff and prunes 1ms past it", async () => {
      const cutoff = computeChallengeableCutoff(NOW);
      const createdAt = new Date(NOW.getTime() - 16 * RETENTION_MS_PER_DAY_V1);
      const atCutoff = await run(
        Effect.gen(function* () {
          const headerHash = yield* seedPayload("at-cutoff", cutoff, createdAt);
          yield* seedTerminal(headerHash, 1);
          const deleted = yield* DaPayloadsDB.pruneOlderThan(
            computeRetentionCutoff(NOW, 15),
            cutoff,
            deploymentManifest,
          );
          return { deleted, remaining: yield* countRows };
        }),
      );
      expect(atCutoff).toEqual({ deleted: 0, remaining: 1 });

      const pastCutoff = await run(
        Effect.gen(function* () {
          const headerHash = yield* seedPayload(
            "past-cutoff",
            new Date(cutoff.getTime() - 1),
            createdAt,
          );
          yield* seedTerminal(headerHash, 2);
          const deleted = yield* DaPayloadsDB.pruneOlderThan(
            computeRetentionCutoff(NOW, 15),
            cutoff,
            deploymentManifest,
          );
          return { deleted, remaining: yield* countRows };
        }),
      );
      expect(pastCutoff).toEqual({ deleted: 0, remaining: 1 });
    });

    it("never prunes on the created_at predicate alone (regression guard)", async () => {
      // A row inserted long ago but whose block is still challengeable must
      // survive. A created_at-only predicate would delete it.
      const oldCreatedAt = new Date(
        NOW.getTime() - 40 * RETENTION_MS_PER_DAY_V1,
      );
      const recentBlockEnd = new Date(NOW.getTime() - 1_000);
      const outcome = await run(
        Effect.gen(function* () {
          const headerHash = yield* seedPayload(
            "young-block",
            recentBlockEnd,
            oldCreatedAt,
          );
          yield* seedTerminal(headerHash, 1);
          const deleted = yield* DaPayloadsDB.pruneOlderThan(
            computeRetentionCutoff(NOW, 15),
            computeChallengeableCutoff(NOW),
            deploymentManifest,
          );
          return { deleted, remaining: yield* countRows };
        }),
      );
      expect(outcome).toEqual({ deleted: 0, remaining: 1 });
    });

    it("never prunes a NULL block_end_time", async () => {
      const old = new Date(NOW.getTime() - 40 * RETENTION_MS_PER_DAY_V1);
      const outcome = await run(
        Effect.gen(function* () {
          const sql = yield* SqlClient.SqlClient;
          const headerHash = yield* seedPayload("null-end", old, old);
          yield* seedTerminal(headerHash, 1);
          // The production schema declares block_end_time NOT NULL; relax it
          // only for this adversarial row so the predicate's IS NOT NULL guard
          // is exercised rather than assumed.
          yield* sql`ALTER TABLE da_payloads ALTER COLUMN block_end_time DROP NOT NULL`;
          yield* sql`ALTER TABLE da_payloads DROP CONSTRAINT IF EXISTS da_payloads_check`;
          yield* sql`UPDATE da_payloads SET block_end_time = NULL WHERE header_hash = ${headerHash}`;
          const deleted = yield* DaPayloadsDB.pruneOlderThan(
            computeRetentionCutoff(NOW, 15),
            computeChallengeableCutoff(NOW),
            deploymentManifest,
          );
          const remaining = yield* countRows;
          yield* sql`UPDATE da_payloads SET block_end_time = ${old} WHERE header_hash = ${headerHash}`;
          yield* sql`ALTER TABLE da_payloads ALTER COLUMN block_end_time SET NOT NULL`;
          yield* sql`ALTER TABLE da_payloads ADD CONSTRAINT da_payloads_check CHECK (block_end_time >= block_start_time)`;
          return { deleted, remaining };
        }),
      );
      expect(outcome).toEqual({ deleted: 0, remaining: 1 });
    });

    it("prunes nothing when the wall-clock retention cutoff has not passed", async () => {
      const days16 = new Date(NOW.getTime() - 16 * RETENTION_MS_PER_DAY_V1);
      const outcome = await run(
        Effect.gen(function* () {
          const headerHash = yield* seedPayload("fresh-insert", days16, NOW);
          yield* seedTerminal(headerHash, 1);
          const deleted = yield* DaPayloadsDB.pruneOlderThan(
            computeRetentionCutoff(NOW, 15),
            computeChallengeableCutoff(NOW),
            deploymentManifest,
          );
          return { deleted, remaining: yield* countRows };
        }),
      );
      expect(outcome).toEqual({ deleted: 0, remaining: 1 });
    });

    it("retains expired payloads without exact terminal or release authority", async () => {
      const old = new Date(NOW.getTime() - 40 * RETENTION_MS_PER_DAY_V1);
      const outcomes = await run(
        Effect.gen(function* () {
          const noTerminal = yield* seedPayload("nonterminal", old, old);
          const deletedWithoutTerminal = yield* DaPayloadsDB.pruneOlderThan(
            computeRetentionCutoff(NOW, 15),
            computeChallengeableCutoff(NOW),
            deploymentManifest,
          );
          yield* seedTerminal(noTerminal, 1);
          const deletedWithoutManifest = yield* DaPayloadsDB.pruneOlderThan(
            computeRetentionCutoff(NOW, 15),
            computeChallengeableCutoff(NOW),
          );
          const deletedWithUnknownQ58 = yield* DaPayloadsDB.pruneOlderThan(
            computeRetentionCutoff(NOW, 15),
            computeChallengeableCutoff(NOW),
            {
              ...deploymentManifest,
              availabilityChallenges: { state: "unknown" },
            },
          );
          return {
            deletedWithoutTerminal,
            deletedWithoutManifest,
            deletedWithUnknownQ58,
            remaining: yield* countRows,
          };
        }),
      );
      expect(outcomes).toEqual({
        deletedWithoutTerminal: 0,
        deletedWithoutManifest: 0,
        deletedWithUnknownQ58: 0,
        remaining: 1,
      });
    });

    it("rejects forged/foreign transitions and cannot use a foreign deployment row", async () => {
      const old = new Date(NOW.getTime() - 40 * RETENTION_MS_PER_DAY_V1);
      const outcome = await run(
        Effect.gen(function* () {
          const sql = yield* SqlClient.SqlClient;
          const headerHash = yield* seedPayload("foreign", old, old);
          const canonical = terminalMerge(headerHash, 1);
          const forged = {
            ...canonical,
            deploymentIdentityDigest: "ff".repeat(32),
          };
          const rejected = yield* Effect.either(
            DaPayloadTerminalOutcomesDB.recordAuthenticatedTransitionV1(
              forged,
              deploymentManifest,
            ),
          );
          yield* sql`
            INSERT INTO da_payload_terminal_outcomes (
              header_hash, terminal_outcome, transition_kind,
              deployment_identity_digest, state_queue_policy_id,
              transaction_hash, block_hash, slot, block_no,
              transaction_index, chain_point_id, finality_depth,
              transition_digest, transition_record
            ) VALUES (
              ${headerHash}, 'merged', 'merge', ${Buffer.from("ff".repeat(32), "hex")},
              ${Buffer.from(deploymentManifest.contracts.stateQueueMint.scriptHash, "hex")},
              ${Buffer.from(canonical.transactionHash, "hex")},
              ${Buffer.from(canonical.blockHash, "hex")}, ${canonical.slot},
              ${canonical.blockNo}, ${Number(canonical.transactionIndex)},
              ${Buffer.from(canonical.chainPointId, "hex")},
              ${canonical.finalityDepth},
              ${Buffer.from(canonical.transitionDigest, "hex")},
              ${JSON.stringify(canonical)}
            )`;
          const deleted = yield* DaPayloadsDB.pruneOlderThan(
            computeRetentionCutoff(NOW, 15),
            computeChallengeableCutoff(NOW),
            deploymentManifest,
          );
          return {
            rejected: rejected._tag,
            deleted,
            remaining: yield* countRows,
          };
        }),
      );
      expect(outcome).toEqual({ rejected: "Left", deleted: 0, remaining: 1 });
    });

    it("prunes multiple independently authenticated terminal outcomes and revokes on rollback", async () => {
      const old = new Date(NOW.getTime() - 40 * RETENTION_MS_PER_DAY_V1);
      const outcome = await run(
        Effect.gen(function* () {
          const first = yield* seedPayload("multi-first", old, old);
          const second = yield* seedPayload("multi-second", old, old);
          const firstTransition = terminalMerge(first, 1);
          const secondTransition = terminalMerge(second, 2);
          yield* DaPayloadTerminalOutcomesDB.recordAuthenticatedTransitionV1(
            firstTransition,
            deploymentManifest,
          );
          yield* DaPayloadTerminalOutcomesDB.recordAuthenticatedTransitionV1(
            secondTransition,
            deploymentManifest,
          );
          yield* DaPayloadTerminalOutcomesDB.revokeAuthenticatedTransitionV1(
            secondTransition,
            deploymentManifest,
          );
          const deleted = yield* DaPayloadsDB.pruneOlderThan(
            computeRetentionCutoff(NOW, 15),
            computeChallengeableCutoff(NOW),
            deploymentManifest,
          );
          return { deleted, remaining: yield* countRows };
        }),
      );
      expect(outcome).toEqual({ deleted: 0, remaining: 2 });
    });
  },
);
