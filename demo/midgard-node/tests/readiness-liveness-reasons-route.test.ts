import "./utils.js";

import { MIDGARD_CONSENSUS_PROFILE } from "@al-ft/midgard-core/consensus-profile";
import { HttpServerRequest, HttpServerResponse } from "@effect/platform";
import { SqlClient } from "@effect/sql";
import { Effect, Ref } from "effect";
import { describe, expect, it } from "vitest";

import { buildListenRouter } from "../src/commands/listen-router.js";
import { NodeConfig } from "../src/services/config.js";
import {
  Globals,
  nextL1ProviderHealthEvidence,
} from "../src/services/globals.js";
import {
  clearLivenessReason,
  setLivenessReason,
} from "../src/services/globals.liveness-reasons.js";
import {
  type ActiveLivenessReason,
  clearLivenessIncident,
  HaltSource,
  raiseLivenessIncident,
} from "../src/services/liveness-halt.js";
import { Lucid } from "../src/services/lucid.js";
import {
  ContractDeploymentIdentity,
  MidgardContracts,
} from "../src/services/midgard-contracts.js";
import type { NativeMpfOwnerService } from "../src/services/mpf-native-owner/protocol.js";
import {
  SIGNED_INTENT_UNDECIDED,
  SIGNED_INTENT_UNDECIDED_ESCALATION_MS,
} from "../src/services/signed-intent-undecided.js";
import { ValidationPool } from "../src/services/validation-pool.js";
import { provideDatabaseLayers } from "./utils.js";

/**
 * A raised liveness reason holds block production while the node keeps
 * serving, so `/readyz` must name it: unready (503) while it is raised, with
 * its source, age and escalation, and ready again once it clears.
 */

// Only the settings /readyz reads. Provider evidence is published before
// every request, so the handler never probes a real provider.
const nodeConfig = {
  READINESS_L1_PROVIDER_EVIDENCE_MAX_AGE_MS: 60_000,
  L1_PROVIDER_PREFLIGHT_TIMEOUT_MS: 1_000,
  READINESS_MAX_HEARTBEAT_AGE_MS: 60_000,
  READINESS_MAX_DURABLE_ADMISSION_BACKLOG: 1_000,
  READINESS_MAX_DURABLE_ADMISSION_AGE_MS: 60_000,
  UNCONFIRMED_BLOCK_MAX_AGE_MS: 60_000,
  VALIDATION_WORKER_JOB_TIMEOUT_MS: 60_000,
  STATE_QUEUE_MUTATION_LEASE_STALE_GRACE_MS: 60_000,
  MIN_QUEUE_LENGTH_FOR_MERGING: 1,
} as unknown as NodeConfig["Type"];

const responsiveOwner = {
  diagnostics: () =>
    Promise.resolve({
      ownerEpoch: Buffer.alloc(16, 1),
      durableRoot: "ab".repeat(32),
      residentNodes: 0,
      residentEdges: 0,
      residentBytes: 0,
      activeGenerations: 0,
      generatedNodes: 0,
      generatedBytes: 0,
      rssBytes: 0,
      peakRssBytes: 0,
      childRestarts: 0,
    }),
} as unknown as NativeMpfOwnerService;

type Readyz = {
  readonly status: number;
  readonly ready: boolean;
  readonly reasons: readonly string[];
  readonly livenessReasons?: readonly ActiveLivenessReason[];
};

type Node = {
  readonly globals: Globals;
  readonly readyz: Effect.Effect<Readyz>;
};

/** Runs `scenario` against one node's globals, which every `readyz` of it
 * reads. The tables /readyz reads are cleared before and after, so a row
 * another file left cannot turn a ready answer unready, and none leaks on. */
const onNode = <A>(scenario: (node: Node) => Effect.Effect<A>): Promise<A> =>
  Effect.runPromise(
    provideDatabaseLayers(
      Effect.gen(function* () {
        const sql = yield* SqlClient.SqlClient;
        const clear = sql`TRUNCATE TABLE pending_block_finalizations,
          state_queue_mutation_leases, event_history_authority
          RESTART IDENTITY CASCADE`;
        return yield* Effect.gen(function* () {
          yield* clear;
          const globals = yield* Globals;
          yield* Ref.set(globals.NATIVE_MPF_OWNER, responsiveOwner);
          const readyz = Effect.gen(function* () {
            yield* Ref.update(globals.L1_PROVIDER_HEALTH, (current) =>
              nextL1ProviderHealthEvidence({
                current,
                healthy: true,
                observedAtMs: Date.now(),
                successKind: "exact",
              }),
            );
            const response = (yield* buildListenRouter().pipe(
              Effect.provideService(
                HttpServerRequest.HttpServerRequest,
                HttpServerRequest.fromWeb(
                  new Request("http://midgard.test/readyz"),
                ),
              ),
            )) as HttpServerResponse.HttpServerResponse;
            const web = HttpServerResponse.toWeb(response);
            const body = (yield* Effect.promise(() => web.json())) as Omit<
              Readyz,
              "status"
            >;
            return { status: web.status, ...body };
          }).pipe(Effect.orDie) as unknown as Effect.Effect<Readyz>;
          return yield* scenario({ globals, readyz });
        }).pipe(Effect.ensuring(Effect.orDie(clear)));
      }).pipe(
        Effect.provideService(NodeConfig, nodeConfig),
        Effect.provideService(ValidationPool, {
          poolSize: 1,
          stats: Effect.succeed({
            oldestInFlightAgeMs: 0,
            liveWorkers: 1,
            restartingWorkers: 0,
          }),
        } as unknown as ValidationPool["Type"]),
        Effect.provideService(Lucid, {} as Lucid),
        Effect.provideService(MidgardContracts, {} as MidgardContracts),
        Effect.provideService(
          ContractDeploymentIdentity,
          ContractDeploymentIdentity.make({
            kind: "derived",
            consensusProfile: MIDGARD_CONSENSUS_PROFILE,
          }),
        ),
        Effect.provide(Globals.Default),
      ) as unknown as Effect.Effect<A, unknown, never>,
    ),
  );

const SOURCE = HaltSource.blockConfirmationSignedIntent;

describe("GET /readyz liveness reasons", () => {
  it("names a raised hold with its age, and is ready again once it clears", async () => {
    const [before, raised, cleared] = await onNode(({ globals, readyz }) =>
      Effect.gen(function* () {
        const before = yield* readyz;
        yield* raiseLivenessIncident(
          globals,
          SOURCE,
          SIGNED_INTENT_UNDECIDED,
          "replaced block holds its base's slot",
          { escalateAfterMs: SIGNED_INTENT_UNDECIDED_ESCALATION_MS },
        );
        const raised = yield* readyz;
        yield* clearLivenessIncident(globals, SOURCE);
        return [before, raised, yield* readyz] as const;
      }),
    );
    expect(before.status).toBe(200);
    expect(before.reasons).toEqual([]);

    expect(raised.status).toBe(503);
    expect(raised.ready).toBe(false);
    expect(raised.reasons).toEqual([SIGNED_INTENT_UNDECIDED]);
    expect(raised.livenessReasons).toEqual([
      {
        source: SOURCE,
        reason: SIGNED_INTENT_UNDECIDED,
        ageMs: expect.any(Number),
        escalateAfterMs: SIGNED_INTENT_UNDECIDED_ESCALATION_MS,
        escalated: false,
      },
    ]);

    expect(cleared.status).toBe(200);
    expect(cleared.ready).toBe(true);
    expect(cleared.reasons).toEqual([]);
    expect(cleared.livenessReasons).toEqual([]);
  });

  it("flags a hold raised past its escalation bound, and stays serving", async () => {
    const escalated = await onNode(({ globals, readyz }) =>
      Effect.gen(function* () {
        yield* raiseLivenessIncident(
          globals,
          HaltSource.stateQueueCorrectionRewind,
          "state_queue_correction_rewind_conflict",
          "removal disagrees with L1",
          { escalateAfterMs: 0 },
        );
        return yield* readyz;
      }),
    );
    expect(escalated.status).toBe(503);
    expect(escalated.reasons).toEqual([
      "state_queue_correction_rewind_conflict",
    ]);
    expect(escalated.livenessReasons).toEqual([
      expect.objectContaining({
        source: HaltSource.stateQueueCorrectionRewind,
        escalateAfterMs: 0,
        escalated: true,
      }),
    ]);
  });

  it("names a reason a fiber sets directly, keeping its age across a changed reason", async () => {
    const source = "user_event_ingestion:deposit";
    const [first, second, cleared] = await onNode(({ globals, readyz }) =>
      Effect.gen(function* () {
        yield* setLivenessReason(globals, source, "stalled:3");
        const first = yield* readyz;
        yield* Effect.sleep("20 millis");
        yield* setLivenessReason(globals, source, "stalled:4");
        const second = yield* readyz;
        yield* clearLivenessReason(globals, source);
        return [first, second, yield* readyz] as const;
      }),
    );
    expect(first.reasons).toEqual(["stalled:3"]);
    expect(second.reasons).toEqual(["stalled:4"]);
    const [entry] = second.livenessReasons ?? [];
    expect(entry).toMatchObject({ source, escalated: false });
    expect(entry?.ageMs).toBeGreaterThanOrEqual(20);
    expect(cleared.status).toBe(200);
    expect(cleared.livenessReasons).toEqual([]);
  });

  it("lists one reason two sources raise once, with both sources", async () => {
    const both = await onNode(({ globals, readyz }) =>
      Effect.gen(function* () {
        yield* raiseLivenessIncident(
          globals,
          SOURCE,
          SIGNED_INTENT_UNDECIDED,
          "confirmation",
        );
        yield* raiseLivenessIncident(
          globals,
          "history_signed_intent_release",
          SIGNED_INTENT_UNDECIDED,
          "history",
        );
        return yield* readyz;
      }),
    );
    expect(both.reasons).toEqual([SIGNED_INTENT_UNDECIDED]);
    expect(
      (both.livenessReasons ?? []).map((entry) => entry.source).sort(),
    ).toEqual([SOURCE, "history_signed_intent_release"].sort());
  });
});
