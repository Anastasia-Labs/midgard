import "./utils.js";

import * as SDK from "@al-ft/midgard-sdk";
import { SqlClient } from "@effect/sql";
import { Effect, Option, Ref } from "effect";
import { beforeEach, describe, expect, it, vi } from "vitest";

import * as HistoryAuthority from "../src/database/eventHistoryAuthority.js";
import type { HistoryProducerPermit } from "../src/services/event-history-producer.js";
import {
  foreignBaseVerificationForAuthority,
  type ForeignBaseVerificationOutcome,
} from "../src/services/foreign-base-verification.js";
import { Globals, NodeConfig } from "../src/services/index.js";
import { Lucid } from "../src/services/lucid.js";
import {
  ContractDeploymentIdentity,
  MidgardContracts,
} from "../src/services/midgard-contracts.js";
import { provideDatabaseLayers } from "./utils.js";

const BASE_HEADER_HASH = SDK.GENESIS_HEADER_HASH;

const verifier = vi.hoisted(() => ({
  calls: 0,
  outcomes: [] as ("verified" | "missing" | "refused")[],
}));
const snapshotCalls = vi.hoisted(() => ({ count: 0 }));
const buildAndSubmitMock = vi.hoisted(() => vi.fn());

// The commit worker: an idle tick must never reach it.
vi.mock(
  "../src/fibers/block-commitment.build-and-submit-commitment-block-action.js",
  () => ({ buildAndSubmitCommitmentBlockAction: buildAndSubmitMock }),
);
vi.mock("../src/fibers/queue-metrics.js", async () => {
  const { Effect: EffectModule } = await import("effect");
  return { emitQueueStateMetrics: EffectModule.void };
});
// A freshly initialized queue: only the root (confirmed-state) node.
vi.mock("../src/services/landed-state-queue.js", async (importOriginal) => {
  const { Effect: EffectModule } = await import("effect");
  const { GENESIS_HEADER_HASH } = await import("@al-ft/midgard-sdk");
  return {
    ...(await importOriginal<Record<string, unknown>>()),
    landedStateQueueSnapshot: () =>
      EffectModule.sync(() => {
        snapshotCalls.count += 1;
        return {
          snapshotId: "idle",
          root: { outRef: "root#0", headerHash: GENESIS_HEADER_HASH },
          tailCommitBase: {
            outRef: "root#0",
            headerHash: GENESIS_HEADER_HASH,
            utxo: "serialized-root",
            blockEndTimeMs: 0,
          },
        };
      }),
  };
});
vi.mock(
  "../src/workers/utils/commit-block-header.js",
  async (importOriginal) => {
    const { Effect: EffectModule } = await import("effect");
    return {
      ...(await importOriginal<Record<string, unknown>>()),
      deserializeStateQueueUTxO: () =>
        EffectModule.succeed({ datum: { key: "Empty" } }),
    };
  },
);
// The commit path's base check, scripted per call: the root-only queue
// verifies as `not_required`, or the check reports missing/refused material.
vi.mock(
  "../src/workers/commit-block-header.verify-foreign-base.js",
  async (importOriginal) => {
    const { Effect: EffectModule } = await import("effect");
    const { ForeignBlockVerificationError: VerificationError } = await import(
      "../src/mpf/verified-block-import.js"
    );
    const { GENESIS_HEADER_HASH } = await import("@al-ft/midgard-sdk");
    return {
      ...(await importOriginal<Record<string, unknown>>()),
      verifyForeignCommitBase: () =>
        EffectModule.suspend(() => {
          verifier.calls += 1;
          const next = verifier.outcomes.shift() ?? "verified";
          if (next === "verified")
            return EffectModule.succeed({
              verification: {
                status: "not_required",
                baseHeaderHash: GENESIS_HEADER_HASH,
              } satisfies ForeignBaseVerificationOutcome,
            });
          return EffectModule.fail(
            new VerificationError({
              foreignHeaderHash: GENESIS_HEADER_HASH,
              reason: next === "missing" ? "missing" : "invalid",
              detail: `scripted_${next}`,
            }),
          );
        }),
    };
  },
);

import { blockCommitmentAction } from "../src/fibers/block-commitment.block-commitment-action.js";
import {
  COMMIT_HORIZON_LAG_SOURCE,
  COMMIT_HORIZON_LAG_UNAVAILABLE,
} from "../src/fibers/block-commitment.commit-horizon-lag-readiness.js";
import { activeLivenessReasons } from "../src/services/liveness-halt.js";

const nodeConfig = {
  STATE_QUEUE_MUTATION_LEASE_TTL_MS: 120_000,
  STATE_QUEUE_MUTATION_LEASE_RENEW_INTERVAL_MS: 30_000,
  HISTORY_COMMIT_HORIZON_LAG_BLOCKS: 0,
} as unknown as NodeConfig["Type"];

/** Stands in for the node's history owner: registers the producer and hands
 * the work the current authority's token, as `runHistoryProducer` expects. */
const historyOwner = {
  runProducer: <A, E, R>(
    work: (
      token: HistoryProducerPermit["token"],
      assertCurrent: Effect.Effect<void>,
      coverage: HistoryProducerPermit["coverage"],
    ) => Effect.Effect<A, E, R>,
  ) =>
    HistoryAuthority.retrieve.pipe(
      Effect.map(Option.getOrThrow),
      Effect.flatMap((row) =>
        work(HistoryAuthority.tokenFromRow(row), Effect.void, {
          bindingDigest: "44".repeat(32),
          checkpointRevision: "1",
          point: { id: "55".repeat(32), slot: 1 },
          snapshotDigest: "66".repeat(32),
          includedThroughMs: 0,
        }),
      ),
    ),
};

/** What `/readyz` feeds `evaluateReadiness` for the foreign base. */
const readinessEvidence = Effect.gen(function* () {
  const globals = yield* Globals;
  const authority = yield* HistoryAuthority.retrieve;
  return foreignBaseVerificationForAuthority(
    yield* Ref.get(globals.FOREIGN_BASE_VERIFICATION),
    Option.isSome(authority) && authority.value.state === "ready"
      ? HistoryAuthority.tokenFromRow(authority.value)
      : undefined,
  );
});

const leaseCount = Effect.gen(function* () {
  const sql = yield* SqlClient.SqlClient;
  const [row] = yield* sql<{ n: number }>`SELECT COUNT(*)::int AS n
    FROM state_queue_mutation_leases`;
  return row?.n ?? -1;
});

/** A started node with a Ready history authority and no work at all. */
const onIdleReadyNode = <A, E, R>(
  program: Effect.Effect<A, E, R>,
  config: NodeConfig["Type"] = nodeConfig,
) =>
  Effect.runPromise(
    provideDatabaseLayers(
      Effect.gen(function* () {
        const sql = yield* SqlClient.SqlClient;
        const clear = sql`TRUNCATE TABLE state_queue_mutation_leases,
          event_history_authority, mempool, deposits_utxos,
          forced_transaction_utxos, withdrawal_utxos
          RESTART IDENTITY CASCADE`;
        yield* clear;
        const token = yield* HistoryAuthority.acquire({
          deploymentIdentity: "5a".repeat(32),
          ownerToken: "5a5a5a5a-5a5a-4a5a-8a5a-5a5a5a5a5a5a",
          leaseDurationMs: 600_000,
        });
        yield* HistoryAuthority.publishReady(token, {
          point: { id: "5b".repeat(32), slot: 1 },
          snapshotDigest: "5c".repeat(32),
        });
        const globals = yield* Globals;
        yield* Ref.set(globals.EVENT_HISTORY_OWNER, historyOwner as never);
        yield* Ref.set(globals.NATIVE_MPF_OWNER, {} as never);
        return yield* program.pipe(Effect.ensuring(Effect.orDie(clear)));
      }).pipe(
        Effect.provideService(NodeConfig, config),
        Effect.provideService(Lucid, { api: {} } as unknown as Lucid),
        Effect.provideService(MidgardContracts, {
          stateQueue: {},
        } as unknown as MidgardContracts),
        Effect.provideService(ContractDeploymentIdentity, {} as never),
        Effect.provide(Globals.Default),
      ),
    ) as Effect.Effect<A, unknown, never>,
  );

describe("foreign base verification on an idle commitment tick", () => {
  beforeEach(() => {
    verifier.calls = 0;
    verifier.outcomes = [];
    snapshotCalls.count = 0;
    buildAndSubmitMock.mockReset();
  });

  it("verifies the base of a fresh idle node, so /readyz can pass without any events", async () => {
    const result = await onIdleReadyNode(
      Effect.gen(function* () {
        const globals = yield* Globals;
        const before = yield* readinessEvidence;
        yield* blockCommitmentAction;
        return {
          before: before.status,
          after: yield* readinessEvidence,
          idle: yield* Ref.get(globals.COMMIT_PIPELINE_IDLE),
          leases: yield* leaseCount,
        };
      }),
    );
    expect(result.before).toBe("unobserved");
    expect(result.idle).toBe(true);
    expect(result.after).toMatchObject({
      status: "verified",
      foreignHeaderHash: BASE_HEADER_HASH,
    });
    // Nothing to commit: no worker, no state-queue mutation lease.
    expect(buildAndSubmitMock).not.toHaveBeenCalled();
    expect(result.leases).toBe(0);
    expect(verifier.calls).toBe(1);
  });

  it("does not re-verify on later idle ticks once the current authority is verified", async () => {
    const result = await onIdleReadyNode(
      Effect.gen(function* () {
        for (let tick = 0; tick < 4; tick += 1) yield* blockCommitmentAction;
        return (yield* readinessEvidence).status;
      }),
    );
    expect(result).toBe("verified");
    expect(verifier.calls).toBe(1);
    expect(snapshotCalls.count).toBe(1);
    expect(buildAndSubmitMock).not.toHaveBeenCalled();
  });

  it.each(["missing", "refused"] as const)(
    "keeps a %s base unready until the next idle tick verifies it",
    async (failure) => {
      verifier.outcomes = [failure];
      const result = await onIdleReadyNode(
        Effect.gen(function* () {
          yield* blockCommitmentAction;
          const held = yield* readinessEvidence;
          yield* blockCommitmentAction;
          return { held, after: (yield* readinessEvidence).status };
        }),
      );
      expect(result.held).toMatchObject({
        status: failure,
        reason: `scripted_${failure}`,
      });
      // Only the same base's own later verification discharges the hold.
      expect(result.after).toBe("verified");
      expect(verifier.calls).toBe(2);
      expect(buildAndSubmitMock).not.toHaveBeenCalled();
    },
  );

  it("re-verifies on the next idle tick after the authority generation changes", async () => {
    const result = await onIdleReadyNode(
      Effect.gen(function* () {
        yield* blockCommitmentAction;
        const sql = yield* SqlClient.SqlClient;
        // A recovery republishes the authority under a new Ready generation.
        yield* sql`UPDATE event_history_authority
          SET generation = generation + 1 WHERE singleton = true`;
        const afterRecovery = (yield* readinessEvidence).status;
        yield* blockCommitmentAction;
        return {
          afterRecovery,
          afterTick: (yield* readinessEvidence).status,
        };
      }),
    );
    expect(result).toEqual({
      afterRecovery: "unobserved",
      afterTick: "verified",
    });
    expect(verifier.calls).toBe(2);
    expect(buildAndSubmitMock).not.toHaveBeenCalled();
  });

  it("names a horizon lag hold on /readyz from the commitment tick while the follower lacks the lagged block", async () => {
    const reasons = await onIdleReadyNode(
      Effect.gen(function* () {
        const sql = yield* SqlClient.SqlClient;
        // No follower cursor yet: the block 1 below the covered tip is unknown.
        yield* sql`DELETE FROM l1_follower_cursor`;
        yield* blockCommitmentAction;
        return yield* activeLivenessReasons(yield* Globals);
      }),
      { ...nodeConfig, HISTORY_COMMIT_HORIZON_LAG_BLOCKS: 1 },
    );
    expect(reasons).toContainEqual(
      expect.objectContaining({
        source: COMMIT_HORIZON_LAG_SOURCE,
        reason: COMMIT_HORIZON_LAG_UNAVAILABLE,
      }),
    );
  });
});
