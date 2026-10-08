import "./utils.js";

import { SqlClient } from "@effect/sql";
import { Effect } from "effect";
import { afterEach, describe, expect, it, vi } from "vitest";

import * as PendingBlockFinalizationsDB from "../src/database/pendingBlockFinalizations.js";
import {
  FORCED_ORDER_CARRIAGE_PENDING,
  FORCED_ORDER_INGESTION_FAILED,
} from "../src/forced-orders/index.js";
import { STATE_QUEUE_UNHEALTHY } from "../src/l1-state-queue/index.js";
import {
  L1_FOLLOWER_NOT_STARTED,
  type L1FollowerState,
} from "../src/services/l1-follower.readiness.js";
import {
  header,
  journalFixture,
} from "./local-mutation-job-abandonment.journal-fixture.js";
import {
  type L1AccessStub,
  readyz,
} from "./readiness-honest-degradation-route.readyz-fixture.js";
import {
  followingAtTip,
  runningFollower,
} from "./readiness-l1-follower.fixture.js";

const signIntent = (headerHash: Buffer) =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const txHash = Buffer.alloc(32, 9);
    yield* sql`UPDATE pending_block_finalizations
      SET prepared_tx_hash = ${txHash}, intended_tx_hash = ${txHash},
        signed_tx_cbor = ${Buffer.from("84a0a0f5f6", "hex")}
      WHERE header_hash = ${headerHash}`;
  });

const backdateJournals = (ms: number) =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    yield* sql`UPDATE pending_block_finalizations
      SET created_at = NOW() - make_interval(secs => ${ms / 1000})`;
  });

const asJournalSetup = <E, R>(effect: Effect.Effect<void, E, R>) =>
  effect as unknown as Effect.Effect<void, unknown, never>;

describe("GET /readyz under internal transients", () => {
  it.each([
    { boundMs: 200_000, ageMs: 150_000, healthy: true },
    { boundMs: 10_000, ageMs: 15_000, healthy: false },
    { boundMs: 600_000, ageMs: 350_000, healthy: true },
  ])(
    "uses the runtime $boundMs ms ledger-tip bound in the raw probe for a $ageMs ms gap",
    async ({ boundMs, ageMs, healthy }) => {
      let reads = 0;
      const response = await readyz({
        provider: [{ healthy: true, agoMs: 1_000 }],
        nodeBehindMaxMs: boundMs,
        l1Access: {
          transport: { ready: true, nodeToClientVersion: 16 },
          ledgerLagMs: ageMs,
          onRead: () => {
            reads += 1;
          },
        },
        forceProviderProbe: true,
      });
      expect(reads).toBeGreaterThan(0);
      expect(response.providerQueryHealthy).toBe(healthy);
      // Failed probes remain within the existing readiness grace after a
      // recent exact success; the tip policy is distinct from that grace.
      expect(response.status).toBe(200);
      expect(response.details).toEqual(
        healthy
          ? []
          : [expect.stringMatching(/^provider_query_degraded:l1-provider:/u)],
      );
    },
  );

  it("answers a database that does not serve with 503 db_unhealthy, not a server error", async () => {
    const down = await readyz({ databaseDown: true });
    expect(down.status).toBe(503);
    expect(down.ready).toBe(false);
    expect(down.reasons).toEqual(["db_unhealthy"]);
    expect(down.details).toEqual([]);
    expect(down.dbError).toContain("Injected statement failure");
    expect(down.settlement).toBeDefined();

    const up = await readyz();
    expect(up.status).toBe(200);
    expect(up.ready).toBe(true);
    expect(up.reasons).toEqual([]);
    expect(up.dbError).toBeUndefined();
  });

  it("keeps a provider failure shortly after a success as a detail and stays ready", async () => {
    const degraded = await readyz({
      provider: [
        { healthy: true, agoMs: 60_000 },
        { healthy: false, agoMs: 0 },
      ],
    });
    expect(degraded.status).toBe(200);
    expect(degraded.ready).toBe(true);
    expect(degraded.reasons).toEqual([]);
    expect(degraded.details).toHaveLength(1);
    expect(degraded.details[0]).toMatch(
      /^provider_query_degraded:l1-provider:\d+$/u,
    );
  });

  it("goes unready on a provider failing past the bound, or one that never succeeded", async () => {
    for (const provider of [
      [
        { healthy: true, agoMs: 6 * 60_000 },
        { healthy: false, agoMs: 0 },
      ],
      [{ healthy: false, agoMs: 0 }],
    ]) {
      const unhealthy = await readyz({ provider });
      expect(unhealthy.status).toBe(503);
      expect(unhealthy.reasons).toEqual([
        "provider_query_unhealthy:l1-provider",
      ]);
      expect(unhealthy.details).toEqual([]);
    }
  });

  it("holds a provider failure as a detail for as long as the ledger-tip bound allows", async () => {
    const provider = [
      { healthy: true, agoMs: 6 * 60_000 },
      { healthy: false, agoMs: 0 },
    ];
    const insideDerived = await readyz({
      provider,
      nodeBehindMaxMs: 10 * 60_000,
    });
    expect(insideDerived.status).toBe(200);
    expect(insideDerived.reasons).toEqual([]);
    expect(insideDerived.details[0]).toMatch(
      /^provider_query_degraded:l1-provider:\d+$/u,
    );
    const pastDerived = await readyz({
      provider,
      nodeBehindMaxMs: 5 * 60_000,
    });
    expect(pastDerived.status).toBe(503);
    expect(pastDerived.reasons).toEqual([
      "provider_query_unhealthy:l1-provider",
    ]);
  });

  it("reports how long journals have held back the next commit", async () => {
    const none = await readyz();
    expect(none.pendingFinalizationAgeMs).toBeNull();
    expect(none.signedIntentUnresolvedAgeMs).toBeNull();

    const unsigned = await readyz({
      journal: asJournalSetup(
        PendingBlockFinalizationsDB.preparePendingSubmission(
          journalFixture(header("readiness-unsigned")),
        ),
      ),
    });
    expect(unsigned.pendingFinalizationAgeMs).toEqual(expect.any(Number));
    expect(unsigned.signedIntentUnresolvedAgeMs).toBeNull();
    expect(unsigned.details).toEqual([]);
    expect(unsigned.ready).toBe(true);

    const signedHeader = header("readiness-signed");
    const signedPastBound = await readyz({
      journal: asJournalSetup(
        Effect.gen(function* () {
          yield* PendingBlockFinalizationsDB.preparePendingSubmission(
            journalFixture(signedHeader),
          );
          yield* signIntent(signedHeader);
          yield* backdateJournals(16 * 60_000);
        }),
      ),
    });
    expect(signedPastBound.pendingFinalizationAgeMs).toBeGreaterThanOrEqual(
      16 * 60_000,
    );
    expect(signedPastBound.signedIntentUnresolvedAgeMs).toBeGreaterThanOrEqual(
      16 * 60_000,
    );
    expect(signedPastBound.details).toHaveLength(1);
    expect(signedPastBound.details[0]).toMatch(
      /^pending_finalization_age:\d+:900000$/u,
    );
    // Admission does not wait on the journal: still ready.
    expect(signedPastBound.reasons).toEqual([]);
    expect(signedPastBound.status).toBe(200);
  });
});

describe("GET /readyz names the L1 follower's reasons (N1)", () => {
  const holds = async (state: L1FollowerState, reason: string) => {
    const response = await readyz({ l1Follower: state });
    expect(response.status).toBe(503);
    expect(response.ready).toBe(false);
    expect(response.reasons).toEqual([reason]);
    // The process stays up: liveness answers throughout.
    const health = await readyz({ l1Follower: state, path: "healthz" });
    expect(health.status).toBe(200);
    return response;
  };

  it("is ready with the follower at the tip and nothing held", async () => {
    const response = await readyz({ l1Follower: runningFollower() });
    expect(response.status).toBe(200);
    expect(response.reasons).toEqual([]);
    expect(response.l1Follower?.state).toBe("following");
  });

  it("names a node without a follower, not yet started or unconfigured", async () => {
    const response = await holds(
      L1_FOLLOWER_NOT_STARTED,
      "l1_follower_unconfigured",
    );
    expect(response.l1Follower?.state).toBe("unconfigured");
  });

  it("names each follow-loop reason", async () => {
    await holds(
      runningFollower(followingAtTip({ atTip: false })),
      "l1_follower_catching_up",
    );
    await holds(
      runningFollower(
        followingAtTip({
          state: "waiting",
          waiting: { cause: "stream", detail: "socket closed" },
        }),
      ),
      "l1_follower_waiting",
    );
    await holds(
      runningFollower(
        followingAtTip({
          state: "waiting",
          stuck: { at: "rollforward 5.ab", failures: 5, detail: "boom" },
        }),
      ),
      "l1_follower_apply_stuck",
    );
    // F8f: prune passes failing in a row (a prune hook that throws).
    await holds(
      runningFollower(
        followingAtTip({
          prune: {
            steps: 4,
            prunedThroughSlot: 40,
            lastError: "prune hook threw",
            failures: 3,
            floorLags: [],
          },
        }),
      ),
      "l1_follower_prune_failing",
    );
    await holds(
      runningFollower(
        followingAtTip({
          state: "intervention",
          interventions: [
            { reason: "rollback_beyond_k", detail: "rolled back 3000 blocks" },
          ],
        }),
      ),
      "rollback_beyond_k",
    );
    const lost = await holds(
      runningFollower(
        followingAtTip({
          node: { reason: "node_unreachable", detail: "dial: no such file" },
        }),
      ),
      "l1_node_unavailable",
    );
    expect(lost.l1Follower?.node).toEqual({
      reason: "node_unreachable",
      detail: "dial: no such file",
    });
    const behind = { tipSlot: 100, lagMs: 600_000, boundMs: 300_000 };
    const late = await holds(
      runningFollower(followingAtTip({ nodeBehind: behind })),
      "l1_node_behind",
    );
    expect(late.l1Follower).toMatchObject({ nodeBehind: behind });
  });

  it("names each hold of the follower-change driver", async () => {
    for (const reason of [
      "l1_events_orphan_recovery",
      "l1_events_ingestion_waiting",
      "l1_events_ingestion_failed",
      "l1_events_hook_failed",
      FORCED_ORDER_CARRIAGE_PENDING,
      FORCED_ORDER_INGESTION_FAILED,
      // N2: the landed state queue (P1) is unhealthy; reads keep serving.
      STATE_QUEUE_UNHEALTHY,
    ])
      await holds(
        runningFollower(followingAtTip(), [{ reason, detail: "fixture" }]),
        reason,
      );
  });
});

describe("GET /readyz names the local node transport's fault", () => {
  afterEach(() => {
    vi.restoreAllMocks();
  });

  it.each([
    "node_unreachable",
    "sidecar_restarting",
    "node_handshake_failed",
  ] as const)(
    "goes unready on a %s transport while liveness answers and the process stays up",
    async (reason) => {
      const exit = vi.spyOn(process, "exit").mockImplementation(() => {
        throw new Error("the readiness handler must never exit");
      });
      const l1Access: L1AccessStub = {
        transport: { ready: false, reason, detail: "fixture" },
      };
      const response = await readyz({ l1Access });
      expect(response.status).toBe(503);
      expect(response.ready).toBe(false);
      expect(response.reasons).toEqual([`l1_transport_unready:${reason}`]);
      const health = await readyz({ l1Access, path: "healthz" });
      expect(health.status).toBe(200);
      expect(exit).not.toHaveBeenCalled();
    },
  );

  it("raises no transport reason on a ready transport", async () => {
    const response = await readyz({
      l1Access: { transport: { ready: true, nodeToClientVersion: 16 } },
    });
    expect(response.status).toBe(200);
    expect(response.reasons).toEqual([]);
  });
});
