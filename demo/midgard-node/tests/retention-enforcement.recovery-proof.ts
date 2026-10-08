import { RETENTION_MS_PER_DAY } from "@al-ft/midgard-core";
import { SqlClient } from "@effect/sql";
import { Effect } from "effect";
import { beforeAll, describe, expect, it, vi } from "vitest";

import {
  DaPayloadsDB,
  DaPayloadTerminalOutcomesDB,
} from "../src/database/index.js";
import * as MigrationRunner from "../src/database/migrations/runner.js";
import { computeChallengeableCutoff } from "../src/database/retention-policy.js";
import { fetchDaPayloadRetirementProofs } from "../src/fibers/retention-sweeper.da-retirement-view.js";
import {
  deploymentManifest,
  NOW,
  seedPayload,
} from "./retention-enforcement.q54-executable-retention-deadline-alert.js";
import {
  remainingHashes,
  seedPublished,
} from "./retention-enforcement.terminal-merge.js";
import { terminalRemoval } from "./retention-enforcement.terminal-removal.js";
import {
  deterministicFixtureBytes,
  provideDatabaseLayers,
  resetApplicationTables,
} from "./utils.js";

const clear = resetApplicationTables;

const run = <A>(work: Effect.Effect<A, unknown, SqlClient.SqlClient>) =>
  Effect.runPromise(
    provideDatabaseLayers(
      clear.pipe(Effect.zipRight(work), Effect.ensuring(Effect.orDie(clear))),
    ) as Effect.Effect<A, never>,
  );

const identity = () => Buffer.from(deploymentManifest.manifestId, "hex");
const view = {
  confirmedHeadHash: deterministicFixtureBytes("recovery-proof-unrelated", 28),
  liveQueueHeaderHashes: [],
};
const cutoff = () => computeChallengeableCutoff(NOW);
const old = () => new Date(NOW.getTime() - 40 * RETENTION_MS_PER_DAY);

const proofFor = (depth: bigint | null) =>
  fetchDaPayloadRetirementProofs({
    deploymentIdentityDigest: deploymentManifest.manifestId,
    stateQueuePolicyId: deploymentManifest.contracts.stateQueueMint.scriptHash,
    automaticRecoveryMaxDepth:
      deploymentManifest.l1Finality.automaticRecoveryMaxDepth,
    source: { canonicalDepth: async () => depth },
  });

describe.skipIf(process.env.MIDGARD_SKIP_DB_TESTS === "1")(
  "DA retirement requires fresh recovery-depth proof",
  () => {
    beforeAll(async () => {
      await Effect.runPromise(
        provideDatabaseLayers(
          MigrationRunner.migrate({
            appVersion: "test",
            actor: "da-recovery-proof",
          }),
        ) as Effect.Effect<unknown, never>,
      );
    }, 120_000);

    it("keeps admitted merge bytes at inclusive 2161 and retires at 2162 with original byte/identity binding", async () => {
      const result = await run(
        Effect.gen(function* () {
          const first = yield* seedPublished(old(), 1);
          const successor = yield* seedPublished(old(), 2);
          const shallow = yield* proofFor(2161n);
          const held = yield* DaPayloadsDB.pruneBeyondRetention({
            challengeableCutoff: cutoff(),
            view: { ...view, retirementProofs: shallow.proofs },
            deploymentIdentityDigest: identity(),
          });
          const deep = yield* proofFor(2162n);
          const staleBytes = deep.proofs.map((proof) => ({
            ...proof,
            payloadSha256: "00".repeat(32),
          }));
          const wrongBytes = yield* DaPayloadsDB.pruneBeyondRetention({
            challengeableCutoff: cutoff(),
            view: { ...view, retirementProofs: staleBytes },
            deploymentIdentityDigest: identity(),
          });
          const wrongPoint = deep.proofs.map((proof) => ({
            ...proof,
            blockHash: "00".repeat(32),
          }));
          const wrongIdentity = yield* DaPayloadsDB.pruneBeyondRetention({
            challengeableCutoff: cutoff(),
            view: { ...view, retirementProofs: wrongPoint },
            deploymentIdentityDigest: identity(),
          });
          const released = yield* DaPayloadsDB.pruneBeyondRetention({
            challengeableCutoff: cutoff(),
            view: { ...view, retirementProofs: deep.proofs },
            deploymentIdentityDigest: identity(),
          });
          return {
            held,
            wrongBytes,
            wrongIdentity,
            released,
            first,
            successor,
            remaining: yield* remainingHashes,
          };
        }),
      );
      expect(result).toMatchObject({
        held: 0,
        wrongBytes: 0,
        wrongIdentity: 0,
        released: 1,
      });
      expect(result.remaining).toEqual([
        result.successor.headerHash.toString("hex"),
      ]);
    });

    it("retains a confirmed removal inside the time horizon until inclusive depth 2162", async () => {
      const result = await run(
        Effect.gen(function* () {
          const hash = yield* seedPayload("removed-recovery", NOW, NOW);
          yield* DaPayloadTerminalOutcomesDB.recordAuthenticatedTransition(
            terminalRemoval(hash, 1),
            deploymentManifest,
          );
          const deleted: number[] = [];
          for (const depth of [13n, 2161n, 2162n]) {
            const retirement = yield* proofFor(depth);
            deleted.push(
              yield* DaPayloadsDB.pruneBeyondRetention({
                challengeableCutoff: cutoff(),
                view: { ...view, retirementProofs: retirement.proofs },
                deploymentIdentityDigest: identity(),
              }),
            );
          }
          return deleted;
        }),
      );
      expect(result).toEqual([0, 0, 1]);
    });

    it("retains on canonical proof loss and re-proves on the next read", async () => {
      const calls = vi
        .fn<() => Promise<bigint | null>>()
        .mockResolvedValueOnce(null)
        .mockResolvedValueOnce(2162n);
      const result = await run(
        Effect.gen(function* () {
          yield* seedPublished(old());
          const args = {
            deploymentIdentityDigest: deploymentManifest.manifestId,
            stateQueuePolicyId:
              deploymentManifest.contracts.stateQueueMint.scriptHash,
            automaticRecoveryMaxDepth:
              deploymentManifest.l1Finality.automaticRecoveryMaxDepth,
            source: { canonicalDepth: calls },
          };
          const unavailable = yield* fetchDaPayloadRetirementProofs(args);
          const retried = yield* fetchDaPayloadRetirementProofs(args);
          return { unavailable, retried };
        }),
      );
      expect(result.unavailable).toEqual({ proofs: [], unavailable: true });
      expect(result.retried.proofs).toHaveLength(1);
      expect(calls).toHaveBeenCalledTimes(2);
    });
  },
);
