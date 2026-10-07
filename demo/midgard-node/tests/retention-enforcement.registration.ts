import { createHash } from "node:crypto";

import { RETENTION_MS_PER_DAY } from "@al-ft/midgard-core";
import { MIDGARD_CONSENSUS_PROFILE } from "@al-ft/midgard-core/consensus-profile";
import { SqlClient } from "@effect/sql";
import { Effect, Schedule } from "effect";
import { beforeAll, describe, expect, it, vi } from "vitest";

import {
  retentionCheckExitCode,
  retentionCheckProgram,
} from "../src/commands/retention-check.js";
import {
  DaPayloadsDB,
  DaPayloadTerminalOutcomesDB,
} from "../src/database/index.js";
import * as MigrationRunner from "../src/database/migrations/runner.js";
import { computeChallengeableCutoff } from "../src/database/retention-policy.js";
import {
  retentionSweepAction,
  retentionSweeperFiber,
} from "../src/fibers/retention-sweeper.js";
import {
  ContractDeploymentIdentity,
  Lucid,
  MidgardContracts,
  NodeConfig,
} from "../src/services/index.js";
import {
  createDatabaseStateQueueCorrectionObserverStore,
  parseStateQueueCorrectionObserverState,
} from "../src/services/state-queue-correction-observer.js";
import { makeRetentionL1Queue } from "./helpers/retention-l1-view.js";
import {
  daPayloadFixture,
  dbEnabled,
  deploymentManifest,
  NOW,
  REQUIRED_RETENTION_MS,
  seedPayload,
} from "./retention-enforcement.q54-executable-retention-deadline-alert.js";
import {
  countRows,
  manifestDigest,
  prune,
  remainingHashes,
  seedPublished,
  seedRemoved,
  seedTerminal,
  terminalMerge,
  withSweepServices,
} from "./retention-enforcement.terminal-merge.js";
import { deterministicFixtureBytes, provideDatabaseLayers } from "./utils.js";

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
      const outcome = await run(
        Effect.gen(function* () {
          const f = yield* seedPublished(old);
          // The later final merge releases f; the newest final head stays held.
          const later = yield* seedPublished(old, 2);
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
            cursorQueue: later.transition.nextQueue,
            pending: [],
            admitted: [f.transition, later.transition],
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
          return {
            deleted: yield* prune(),
            remaining: yield* remainingHashes,
            later: later.headerHash.toString("hex"),
          };
        }),
      );
      expect(outcome.deleted).toBe(1);
      expect(outcome.remaining).toEqual([outcome.later]);
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
      // Every record is one minute from its deadline, inside the two-minute
      // threshold, so any that the check does not exempt alerts.
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
          const identity = ContractDeploymentIdentity.make({
            kind: "manifest",
            manifestId: deploymentManifest.manifestId,
            consensusProfile: MIDGARD_CONSENSUS_PROFILE,
          });
          const check = yield* queue
            .provide(retentionCheckProgram(120_000))
            .pipe(Effect.provideService(ContractDeploymentIdentity, identity));
          const unthresholded = yield* queue
            .provide(retentionCheckProgram())
            .pipe(Effect.provideService(ContractDeploymentIdentity, identity));
          return { check, unthresholded, control: control.toString("hex") };
        }),
      );
      expect(result.check.checked).toBe(4);
      expect(result.check.stillChallengeable).toBe(1);
      expect(result.check.alerts.map(({ headerHash }) => headerHash)).toEqual([
        result.control,
      ]);
      // The same records without an operator threshold raise no alert.
      expect(result.unthresholded.stillChallengeable).toBe(1);
      expect(result.unthresholded.alerts).toEqual([]);
      expect(retentionCheckExitCode(result.unthresholded)).toBe(0);
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
      const sweep = (retentionDays: number | undefined) =>
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
      // Unset: the verified manifest's 15-day window applies.
      expect(await sweep(undefined)).toEqual({
        daPayloads: 0,
        txRejections: 0,
      });
    });

    it("dates the sweep at L1 now: a wall clock 10 minutes fast prunes no still-challengeable payload", async () => {
      const tenMinutesMs = 10 * 60_000;
      const remaining = await run(
        Effect.gen(function* () {
          // Challengeable for 5 more minutes at L1 now (NOW), past the
          // horizon on the wall clock (NOW + 10 minutes).
          const kept = yield* seedPayload(
            "fast-clock-kept",
            new Date(NOW.getTime() - REQUIRED_RETENTION_MS + 5 * 60_000),
            NOW,
          );
          // Past the horizon at L1 now as well: the sweep did run.
          yield* seedPayload(
            "fast-clock-pruned",
            new Date(NOW.getTime() - REQUIRED_RETENTION_MS - 60_000),
            NOW,
          );
          vi.useFakeTimers({ toFake: ["Date"] });
          vi.setSystemTime(NOW.getTime() + tenMinutesMs);
          yield* withSweepServices(
            retentionSweeperFiber(Schedule.recurs(0), {
              fetchL1View: Effect.succeed({
                confirmedHeadHash: deterministicFixtureBytes("head", 28),
                liveQueueHeaderHashes: [],
              }),
              l1NowMs: Effect.succeed(NOW.getTime()),
            }),
            0,
          ).pipe(
            Effect.provideService(Lucid, {} as never),
            Effect.provideService(MidgardContracts, {} as never),
            Effect.ensuring(Effect.sync(() => vi.useRealTimers())),
          );
          return { hashes: yield* remainingHashes, kept: kept.toString("hex") };
        }),
      );
      expect(remaining.hashes).toEqual([remaining.kept]);
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
