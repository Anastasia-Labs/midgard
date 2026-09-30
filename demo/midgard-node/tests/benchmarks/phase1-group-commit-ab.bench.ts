import "node:crypto";
import "node:fs/promises";
import "node:path";
import "node:perf_hooks";
import "@al-ft/midgard-core/cek-proof";
import "@effect/sql";
import "effect";
import "vitest";
import "../../src/database/index.js";
import "../../src/services/admission-writer.js";
import "../../src/services/config.js";
import "../../src/services/database.js";
import "./phase1-group-commit-ab.build-fixture.js";
import "./phase1-group-commit-ab.verify-semantic-state.js";

import { createHash } from "node:crypto";
import { writeFile } from "node:fs/promises";

import { SqlClient } from "@effect/sql";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import { MigrationRunner } from "../../src/database/index.js";
import { NodeConfig } from "../../src/services/config.js";
import { AdmissionSql, Database } from "../../src/services/database.js";
import {
  buildFixture,
  laneCount,
  measureVariant,
  mergedRows,
  operatorEnabled,
  outputPath,
  repetitions,
  resetAndSeed,
  rowsPerLane,
  runToken,
  summarize,
  type Variant,
} from "./phase1-group-commit-ab.build-fixture.js";
import {
  verifyRollbackParity,
  verifySemanticState,
} from "./phase1-group-commit-ab.verify-semantic-state.js";

describe("Phase 1 global group-commit PostgreSQL A/B diagnostic", () => {
  it.skipIf(!operatorEnabled)(
    "compares two concurrent 128-row commits with one ordered 256-row commit",
    async () => {
      const database = process.env.POSTGRES_DB ?? "";
      expect(runToken).toMatch(/^[a-z0-9_]+$/u);
      expect(database).toBe(`midgard_phase1_group_commit_${runToken}`);
      expect(process.env.POSTGRES_HOST).toMatch(
        /^(127\.0\.0\.1|localhost|::1)$/u,
      );
      expect(Number(process.env.POSTGRES_PORT)).not.toBe(5_433);
      expect(Number.isSafeInteger(repetitions) && repetitions >= 9).toBe(true);
      expect(outputPath).toContain(runToken);

      const report = await Effect.runPromise(
        Effect.gen(function* () {
          const batchSql = yield* SqlClient.SqlClient;
          const observerSql = yield* AdmissionSql;
          const identity = yield* batchSql<{
            readonly database: string;
            readonly server_version: string;
          }>`SELECT current_database() AS database,
              current_setting('server_version') AS server_version`;
          expect(identity[0]?.database).toBe(database);
          expect(identity[0]?.server_version.startsWith("15.")).toBe(true);
          yield* batchSql`DROP SCHEMA public CASCADE; CREATE SCHEMA public`;
          yield* MigrationRunner.migrate({
            appVersion: "phase1-group-commit-ab",
            actor: "phase1-group-commit-ab",
          });
          const fixture = buildFixture();
          expect(fixture.mergedRequests).toHaveLength(mergedRows);
          expect(
            fixture.laneRequests.map((requests) => requests.length),
          ).toEqual([rowsPerLane, rowsPerLane]);
          const requestDigest = createHash("sha256");
          for (const request of fixture.mergedRequests) {
            requestDigest.update(request.txId);
            requestDigest.update(request.txCanonicalCbor);
          }

          yield* verifyRollbackParity("two_concurrent_128", batchSql);
          yield* verifyRollbackParity("one_ordered_256", batchSql);

          const samples: {
            readonly repetition: number;
            readonly position: number;
            readonly variant: Variant;
            readonly durationMs: number;
            readonly walBytes: number;
            readonly xactCommitDelta: number;
            readonly xactRollbackDelta: number;
            readonly tuplesInsertedDelta: number;
            readonly tuplesUpdatedDelta: number;
          }[] = [];
          for (let repetition = 0; repetition < repetitions; repetition += 1) {
            const order: readonly Variant[] =
              repetition % 2 === 0
                ? ["two_concurrent_128", "one_ordered_256"]
                : ["one_ordered_256", "two_concurrent_128"];
            for (const [position, variant] of order.entries()) {
              yield* resetAndSeed(batchSql, fixture);
              const measured = yield* measureVariant(
                variant,
                fixture,
                batchSql,
                observerSql,
              );
              expect(measured.walBytes).toBeGreaterThan(0);
              expect(measured.xactCommitDelta).toBe(
                variant === "two_concurrent_128" ? 2 : 1,
              );
              expect(measured.xactRollbackDelta).toBe(0);
              yield* verifySemanticState(variant, fixture, measured.outcomes);
              samples.push({
                repetition,
                position,
                variant,
                durationMs: measured.durationMs,
                walBytes: measured.walBytes,
                xactCommitDelta: measured.xactCommitDelta,
                xactRollbackDelta: measured.xactRollbackDelta,
                tuplesInsertedDelta: measured.tuplesInsertedDelta,
                tuplesUpdatedDelta: measured.tuplesUpdatedDelta,
              });
            }
          }
          const byVariant = (variant: Variant) =>
            samples.filter((sample) => sample.variant === variant);
          const summarizeVariant = (variant: Variant) => {
            const variantSamples = byVariant(variant);
            return {
              repetitions: variantSamples.length,
              commitDurationMs: summarize(
                variantSamples.map((sample) => sample.durationMs),
              ),
              walBytes: summarize(
                variantSamples.map((sample) => sample.walBytes),
              ),
              xactCommitDelta: variantSamples.map(
                (sample) => sample.xactCommitDelta,
              ),
              xactRollbackDelta: variantSamples.map(
                (sample) => sample.xactRollbackDelta,
              ),
              tuplesInsertedDelta: variantSamples.map(
                (sample) => sample.tuplesInsertedDelta,
              ),
              tuplesUpdatedDelta: variantSamples.map(
                (sample) => sample.tuplesUpdatedDelta,
              ),
            };
          };
          return {
            generatedAtIso: new Date().toISOString(),
            database,
            serverVersion: identity[0]!.server_version,
            nodeVersion: process.version,
            runToken,
            repetitions,
            laneCount,
            rowsPerLane,
            mergedRows,
            requestSha256: requestDigest.digest("hex"),
            expectedOutcomeCounts: {
              new: fixture.expectedKinds.filter((kind) => kind === "new")
                .length,
              duplicate: fixture.expectedKinds.filter(
                (kind) => kind === "duplicate",
              ).length,
              conflict: fixture.expectedKinds.filter(
                (kind) => kind === "conflict",
              ).length,
            },
            rollbackParity: {
              twoConcurrent128: "both lane statements failed; zero rows",
              oneOrdered256: "merged statement failed; zero rows",
            },
            runOrder: Array.from({ length: repetitions }, (_, repetition) =>
              repetition % 2 === 0
                ? ["two_concurrent_128", "one_ordered_256"]
                : ["one_ordered_256", "two_concurrent_128"],
            ),
            samples,
            summary: {
              twoConcurrent128: summarizeVariant("two_concurrent_128"),
              oneOrdered256: summarizeVariant("one_ordered_256"),
            },
          };
        }).pipe(
          Effect.provide(Database.layer),
          Effect.provide(NodeConfig.layer),
        ),
      );
      await writeFile(outputPath, `${JSON.stringify(report, null, 2)}\n`);
      expect(report.expectedOutcomeCounts).toEqual({
        new: 248,
        duplicate: 4,
        conflict: 4,
      });
      expect(report.summary.twoConcurrent128.repetitions).toBe(repetitions);
      expect(report.summary.oneOrdered256.repetitions).toBe(repetitions);
    },
    900_000,
  );
});
