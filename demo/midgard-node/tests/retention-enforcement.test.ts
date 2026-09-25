import { createHash } from "node:crypto";

import {
  MIDGARD_RETENTION_WINDOW,
  RETENTION_MS_PER_DAY,
} from "@al-ft/midgard-core";
import {
  MIDGARD_CONSENSUS_PROFILE,
  MIDGARD_CONSENSUS_PROFILE_ID,
} from "@al-ft/midgard-core/consensus-profile";
import * as SDK from "@al-ft/midgard-sdk";
import { SqlClient } from "@effect/sql";
import { Data } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { beforeAll, describe, expect, it } from "vitest";

import { evaluateReadiness } from "../src/commands/readiness.js";
import {
  evaluateRetentionCheck,
  retentionCheckExitCode,
  retentionCheckProgram,
} from "../src/commands/retention-check.js";
import {
  DaPayloadsDB,
  DaPayloadTerminalOutcomesDB,
} from "../src/database/index.js";
import * as MigrationRunner from "../src/database/migrations/runner.js";
import { computeChallengeableCutoff } from "../src/database/retention-policy.js";
import { retentionSweepAction } from "../src/fibers/retention-sweeper.js";
import {
  ContractDeploymentIdentity,
  NodeConfig,
} from "../src/services/index.js";
import {
  createDatabaseStateQueueCorrectionObserverStore,
  parseStateQueueCorrectionObserverState,
} from "../src/services/state-queue-correction-observer.js";
import { makeFinalizedDeploymentManifestFixture } from "./helpers/finalized-deployment-manifest.js";
import { makeRetentionL1Queue } from "./helpers/retention-l1-view.js";
import { deterministicFixtureBytes, provideDatabaseLayers } from "./utils.js";

const REQUIRED_RETENTION_MS = MIDGARD_RETENTION_WINDOW.requiredRetentionMs;
const NOW = new Date("2026-08-03T00:00:00.000Z");
let deploymentManifest: Awaited<
  ReturnType<typeof makeFinalizedDeploymentManifestFixture>
>;

// The fixture compiles and parameterizes the whole canonical contract set —
// 287 contracts as of this writing — so it runs well past Vitest's 10-second
// default hook budget. The budget is the fixture's, not a symptom: the suite's
// own assertions are pure and fast.
beforeAll(async () => {
  deploymentManifest = await makeFinalizedDeploymentManifestFixture();
}, 120_000);

const readinessBase = {
  nowMillis: NOW.getTime(),
  maxHeartbeatAgeMs: 60_000,
  maxQueueDepth: 10,
  queueDepth: 0,
  workerHeartbeats: {
    blockCommitment: NOW.getTime(),
    blockConfirmation: NOW.getTime(),
    merge: NOW.getTime(),
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
      records: [
        {
          headerHash,
          blockEndTimeMs,
          headerStatus: "attested",
          queueReference: "none",
        },
      ],
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
  it("admits the deployment identity and finality depth", () => {
    expect(
      DaPayloadTerminalOutcomesDB.admitDaPayloadRetentionReleaseAuthority(
        deploymentManifest,
      ),
    ).toMatchObject({
      minimumFinalityDepth: 30n,
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

const dbEnabled = process.env.MIDGARD_SKIP_DB_TESTS !== "1";

const daPayloadFixture = (
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
  finalityDepth = BigInt(deploymentManifest.l1Finality.confirmationDepth),
): SDK.StateQueueAuthenticatedTransition => {
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
  const transition = SDK.deriveStateQueueAuthenticatedTransition({
    deploymentIdentityDigest: deploymentManifest.manifestId,
    stateQueuePolicyId: policyId,
    transactionHash,
    blockHash: h32((sequence + 8).toString(16)),
    slot: (100 + sequence).toString(),
    blockNo: (90 + sequence).toString(),
    transactionIndex: "0",
    chainPointId: h32((sequence + 12).toString(16)),
    finalityDepth: finalityDepth.toString(),
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

const publishedFixture = (endTime: Date, sequence = 1) => {
  const header: SDK.Header = {
    ...SDK.EMPTY_HEADER_TRANSITION_COMMITMENTS,
    prevUtxosRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
    utxosRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
    transactionsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
    depositsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
    withdrawalsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
    startTime: BigInt(endTime.getTime() - 1000),
    endTime: BigInt(endTime.getTime()),
    blockSlot: BigInt(sequence),
    expectedNetworkId: 0n,
    minFeeA: 44n,
    minFeeB: 155381n,
    prevHeaderHash: "11".repeat(28),
    operatorVkey: "22".repeat(28),
    protocolVersion: 1n,
  };
  const headerHash = Buffer.from(
    Effect.runSync(SDK.hashBlockHeader(header)),
    "hex",
  );
  const transition = terminalMerge(headerHash, sequence);
  return { headerHash, transition };
};

const seedPublished = (endTime: Date, sequence = 1) =>
  Effect.gen(function* () {
    const fixture = publishedFixture(endTime, sequence);
    const row = {
      ...daPayloadFixture(`published-${sequence}`, endTime),
      [DaPayloadsDB.Columns.HEADER_HASH]: fixture.headerHash,
    };
    yield* DaPayloadsDB.upsertAvailable(row);
    const sql = yield* SqlClient.SqlClient;
    yield* sql`UPDATE da_payloads SET created_at = ${new Date(NOW.getTime() - 40 * RETENTION_MS_PER_DAY)} WHERE header_hash = ${fixture.headerHash}`;
    yield* DaPayloadTerminalOutcomesDB.recordAuthenticatedTransition(
      fixture.transition,
      deploymentManifest,
    );
    return fixture;
  });

const seedTerminal = (
  headerHash: Buffer,
  sequence: number,
): Effect.Effect<void, unknown, SqlClient.SqlClient> =>
  DaPayloadTerminalOutcomesDB.recordAuthenticatedTransition(
    terminalMerge(headerHash, sequence),
    deploymentManifest,
  );

const manifestDigest = (): Buffer =>
  Buffer.from(deploymentManifest.manifestId, "hex");

/** Records an authenticated-looking `removed` outcome under `digest`. */
const seedRemoved = (
  headerHash: Buffer,
  sequence: number,
  digest: Buffer,
): Effect.Effect<void, unknown, SqlClient.SqlClient> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const transition = terminalMerge(headerHash, sequence);
    yield* sql`
      INSERT INTO da_payload_terminal_outcomes (
        header_hash, terminal_outcome, transition_kind,
        deployment_identity_digest, state_queue_policy_id,
        transaction_hash, block_hash, slot, block_no,
        transaction_index, chain_point_id, finality_depth,
        transition_digest, transition_record
      ) VALUES (
        ${headerHash}, 'removed', 'fraud_removal', ${digest},
        ${Buffer.from(deploymentManifest.contracts.stateQueueMint.scriptHash, "hex")},
        ${Buffer.from(transition.transactionHash, "hex")},
        ${Buffer.from(transition.blockHash, "hex")}, ${transition.slot},
        ${transition.blockNo}, ${Number(transition.transactionIndex)},
        ${Buffer.from(transition.chainPointId, "hex")},
        ${transition.finalityDepth},
        ${Buffer.from(transition.transitionDigest, "hex")},
        ${JSON.stringify(transition)}
      )`;
  });

/** An L1 view that references none of the seeded payloads. */
const unrelatedView: DaPayloadsDB.RetentionL1View = {
  confirmedHeadHash: deterministicFixtureBytes("unrelated-head", 28),
  liveQueueHeaderHashes: [deterministicFixtureBytes("unrelated-live", 28)],
};

const prune = (
  options: {
    readonly view?: DaPayloadsDB.RetentionL1View;
    readonly digest?: Buffer | undefined;
  } = {},
) =>
  DaPayloadsDB.pruneBeyondRetention({
    challengeableCutoff: computeChallengeableCutoff(NOW),
    view: options.view ?? unrelatedView,
    deploymentIdentityDigest:
      "digest" in options ? options.digest : manifestDigest(),
  });

const remainingHashes = Effect.gen(function* () {
  const sql = yield* SqlClient.SqlClient;
  const rows = yield* sql<{
    readonly header_hash: Buffer;
  }>`SELECT header_hash FROM da_payloads`;
  return rows.map((row) => row.header_hash.toString("hex")).sort();
});

/** Runs a sweep with RETENTION_DAYS overridden and this deployment's identity. */
const withSweepServices = <A, E, R>(
  effect: Effect.Effect<A, E, R>,
  retentionDays: number,
) =>
  Effect.gen(function* () {
    const nodeConfig = yield* NodeConfig;
    return yield* effect.pipe(
      Effect.provideService(NodeConfig, {
        ...nodeConfig,
        RETENTION_DAYS: retentionDays,
      }),
      Effect.provideService(
        ContractDeploymentIdentity,
        ContractDeploymentIdentity.make({
          kind: "manifest",
          manifestId: deploymentManifest.manifestId,
          consensusProfile: MIDGARD_CONSENSUS_PROFILE,
        }),
      ),
    );
  });

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

    const run = <A>(
      effect: Effect.Effect<A, unknown, SqlClient.SqlClient | NodeConfig>,
    ) =>
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

    it("collects and reloads terminal authority through the durable observer store", async () => {
      const old = new Date(NOW.getTime() - 40 * RETENTION_MS_PER_DAY);
      const deleted = await run(
        Effect.gen(function* () {
          const f = yield* seedPublished(old);
          const sql = yield* SqlClient.SqlClient;
          const canonicalJson = (value: unknown): string =>
            value === null || typeof value !== "object"
              ? JSON.stringify(value)
              : Array.isArray(value)
                ? `[${value.map(canonicalJson).join(",")}]`
                : `{${Object.entries(value)
                    .sort(([a], [b]) => a.localeCompare(b))
                    .map(
                      ([key, item]) =>
                        `${JSON.stringify(key)}:${canonicalJson(item)}`,
                    )
                    .join(",")}}`;
          const base = {
            schemaVersion:
              "midgard-node-state-queue-correction-observer-v1" as const,
            deploymentIdentityDigest: deploymentManifest.manifestId,
            stateQueuePolicyId:
              deploymentManifest.contracts.stateQueueMint.scriptHash,
            cursorQueue: f.transition.nextQueue,
            pending: [],
            admitted: [f.transition],
            retractedTransactionHashes: [],
            postFinalityRollbackIncidents: [],
          };
          const state = {
            ...base,
            stateDigest: createHash("sha256")
              .update(canonicalJson(base))
              .digest("hex"),
          };
          expect(parseStateQueueCorrectionObserverState(state)).not.toBeNull();
          const store = createDatabaseStateQueueCorrectionObserverStore({
            sql,
            deploymentManifest,
          });
          yield* Effect.promise(() => store.save(state));
          expect(
            parseStateQueueCorrectionObserverState(
              yield* Effect.promise(() => store.load()),
            ),
          ).toEqual(state);
          return yield* prune();
        }),
      );
      expect(deleted).toBe(1);
    });
    it("retains a block_end_time exactly at the horizon and prunes 1ms past it", async () => {
      const cutoff = computeChallengeableCutoff(NOW);
      const outcome = await run(
        Effect.gen(function* () {
          const atCutoff = yield* seedPayload("at-cutoff", cutoff, cutoff);
          yield* seedPayload(
            "past-cutoff",
            new Date(cutoff.getTime() - 1),
            cutoff,
          );
          const deleted = yield* prune();
          return { deleted, remaining: yield* remainingHashes, atCutoff };
        }),
      );
      expect(outcome.deleted).toBe(1);
      expect(outcome.remaining).toEqual([outcome.atCutoff.toString("hex")]);
    });

    it("prunes past the horizon with no terminal outcome, whatever created_at says", async () => {
      const old = new Date(NOW.getTime() - 40 * RETENTION_MS_PER_DAY);
      const outcome = await run(
        Effect.gen(function* () {
          yield* seedPayload("old-fresh-insert", old, NOW);
          const young = yield* seedPayload(
            "young-old-insert",
            new Date(NOW.getTime() - 1_000),
            old,
          );
          const deleted = yield* prune();
          return { deleted, remaining: yield* remainingHashes, young };
        }),
      );
      expect(outcome.deleted).toBe(1);
      expect(outcome.remaining).toEqual([outcome.young.toString("hex")]);
    });

    it("prunes past the horizon regardless of a merged outcome or its deployment", async () => {
      const old = new Date(NOW.getTime() - 40 * RETENTION_MS_PER_DAY);
      const outcome = await run(
        Effect.gen(function* () {
          const merged = yield* seedPayload("merged", old, old);
          yield* seedTerminal(merged, 1);
          yield* seedPayload("unobserved", old, old);
          const deleted = yield* prune({ digest: undefined });
          return { deleted, remaining: yield* countRows };
        }),
      );
      expect(outcome).toEqual({ deleted: 2, remaining: 0 });
    });

    it("retains a merged header inside the horizon", async () => {
      const young = new Date(NOW.getTime() - 1_000);
      const outcome = await run(
        Effect.gen(function* () {
          const merged = yield* seedPayload("young-merged", young, young);
          yield* seedTerminal(merged, 1);
          const deleted = yield* prune();
          return { deleted, remaining: yield* remainingHashes, merged };
        }),
      );
      expect(outcome.deleted).toBe(0);
      expect(outcome.remaining).toEqual([outcome.merged.toString("hex")]);
    });

    it("refuses to record a terminal transition shallower than the manifest's finality depth", async () => {
      const depth = BigInt(deploymentManifest.l1Finality.confirmationDepth);
      expect(depth).toBeGreaterThan(0n);
      const outcome = await run(
        Effect.gen(function* () {
          const young = new Date(NOW.getTime() - 1_000);
          const shallow = yield* seedPayload("shallow", young, young);
          const refused = yield* Effect.either(
            DaPayloadTerminalOutcomesDB.recordAuthenticatedTransition(
              terminalMerge(shallow, 1, depth - 1n),
              deploymentManifest,
            ),
          );
          const final = yield* seedPayload("final", young, young);
          const admitted = yield* Effect.either(
            DaPayloadTerminalOutcomesDB.recordAuthenticatedTransition(
              terminalMerge(final, 2, depth),
              deploymentManifest,
            ),
          );
          const sql = yield* SqlClient.SqlClient;
          const recorded = yield* sql<{ readonly header_hash: Buffer }>`
            SELECT header_hash FROM da_payload_terminal_outcomes`;
          return { refused, admitted, recorded, final };
        }),
      );
      expect(outcome.refused._tag).toBe("Left");
      expect(outcome.admitted._tag).toBe("Right");
      expect(
        outcome.recorded.map((row) => row.header_hash.toString("hex")),
      ).toEqual([outcome.final.toString("hex")]);
    });

    it("prunes a removed header inside the horizon only under this deployment", async () => {
      const young = new Date(NOW.getTime() - 1_000);
      const outcome = await run(
        Effect.gen(function* () {
          const removed = yield* seedPayload("removed", young, young);
          yield* seedRemoved(removed, 1, manifestDigest());
          const foreign = yield* seedPayload("removed-foreign", young, young);
          yield* seedRemoved(foreign, 2, Buffer.from("ff".repeat(32), "hex"));
          const withoutDigest = yield* prune({ digest: undefined });
          const deleted = yield* prune();
          return {
            withoutDigest,
            deleted,
            remaining: yield* remainingHashes,
            foreign,
          };
        }),
      );
      expect(outcome.withoutDigest).toBe(0);
      expect(outcome.deleted).toBe(1);
      expect(outcome.remaining).toEqual([outcome.foreign.toString("hex")]);
    });

    it("retains the L1 confirmed head and live queue headers even when prunable", async () => {
      const old = new Date(NOW.getTime() - 40 * RETENTION_MS_PER_DAY);
      const outcome = await run(
        Effect.gen(function* () {
          const head = yield* seedPayload("head", old, old);
          const live = yield* seedPayload("live", old, old);
          const liveRemoved = yield* seedPayload("live-removed", NOW, NOW);
          yield* seedRemoved(liveRemoved, 1, manifestDigest());
          yield* seedPayload("unreferenced", old, old);
          const deleted = yield* prune({
            view: {
              confirmedHeadHash: head,
              liveQueueHeaderHashes: [live, liveRemoved],
            },
          });
          return {
            deleted,
            remaining: yield* remainingHashes,
            kept: [head, live, liveRemoved]
              .map((hash) => hash.toString("hex"))
              .sort(),
          };
        }),
      );
      expect(outcome.deleted).toBe(1);
      expect(outcome.remaining).toEqual(outcome.kept);
    });

    it("exempts the L1 confirmed head and live queue headers in the retention check", async () => {
      const queue = await makeRetentionL1Queue({
        confirmedHeadHash: deterministicFixtureBytes(
          "retention-check-head",
          28,
        ).toString("hex"),
        liveUtxosRoots: ["71".repeat(32), "72".repeat(32)],
      });
      // Every record is one minute from its deadline, so any that the check
      // does not exempt is still challengeable and alerts.
      const nearDeadline = new Date(
        Date.now() - REQUIRED_RETENTION_MS + 60_000,
      );
      const result = await run(
        Effect.gen(function* () {
          const hashes = [
            deterministicFixtureBytes("retention-check-head", 28),
            ...queue.liveHeaderHashes.map((hash) => Buffer.from(hash, "hex")),
          ];
          for (const [index, headerHash] of hashes.entries()) {
            yield* DaPayloadsDB.upsertAvailable({
              ...daPayloadFixture(`check-${index.toString()}`, nearDeadline),
              [DaPayloadsDB.Columns.HEADER_HASH]: headerHash,
            });
          }
          const control = yield* seedPayload(
            "check-unreferenced",
            nearDeadline,
            nearDeadline,
          );
          const check = yield* queue.provide(retentionCheckProgram()).pipe(
            Effect.provideService(
              ContractDeploymentIdentity,
              ContractDeploymentIdentity.make({
                kind: "manifest",
                manifestId: deploymentManifest.manifestId,
                consensusProfile: MIDGARD_CONSENSUS_PROFILE,
              }),
            ),
          );
          return { check, control: control.toString("hex") };
        }),
      );
      expect(result.check.checked).toBe(4);
      expect(result.check.stillChallengeable).toBe(1);
      expect(result.check.alerts.map(({ headerHash }) => headerHash)).toEqual([
        result.control,
      ]);
    });

    it("declares block_end_time NOT NULL, so no row can escape the horizon", async () => {
      const outcome = await run(
        Effect.gen(function* () {
          const sql = yield* SqlClient.SqlClient;
          const headerHash = yield* seedPayload("null-end", NOW, NOW);
          return yield* Effect.either(
            sql`UPDATE da_payloads SET block_end_time = NULL WHERE header_hash = ${headerHash}`,
          );
        }),
      );
      expect(outcome._tag).toBe("Left");
    });

    it("prunes DA payloads with RETENTION_DAYS=0 but leaves the wall-clock tables alone", async () => {
      const old = new Date(NOW.getTime() - 40 * RETENTION_MS_PER_DAY);
      const sweep = (retentionDays: number) =>
        run(
          Effect.gen(function* () {
            const sql = yield* SqlClient.SqlClient;
            yield* sql`DELETE FROM tx_rejections`;
            yield* seedPayload("split", old, old);
            yield* sql`INSERT INTO tx_rejections (tx_id, reject_code, created_at) VALUES (${deterministicFixtureBytes("split-rejection", 32)}, 'test', ${old})`;
            yield* withSweepServices(
              retentionSweepAction(
                {
                  confirmedHeadHash: deterministicFixtureBytes("head", 28),
                  liveQueueHeaderHashes: [],
                },
                NOW,
              ),
              retentionDays,
            );
            const rejections = yield* sql<{
              readonly count: string;
            }>`SELECT COUNT(*)::text AS count FROM tx_rejections`;
            return {
              daPayloads: yield* countRows,
              txRejections: Number(rejections[0]?.count ?? "0"),
            };
          }),
        );
      expect(await sweep(0)).toEqual({ daPayloads: 0, txRejections: 1 });
      expect(await sweep(15)).toEqual({ daPayloads: 0, txRejections: 0 });
    });

    it("prunes no DA payload in a sweep without an L1 view", async () => {
      const old = new Date(NOW.getTime() - 40 * RETENTION_MS_PER_DAY);
      const remaining = await run(
        Effect.gen(function* () {
          yield* seedPayload("no-view", old, old);
          yield* withSweepServices(retentionSweepAction(undefined, NOW), 0);
          return yield* countRows;
        }),
      );
      expect(remaining).toBe(1);
    });
  },
);
