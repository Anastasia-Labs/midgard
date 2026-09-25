import "./utils.js";

import { HttpServerRequest, HttpServerResponse } from "@effect/platform";
import { Effect, Ref } from "effect";
import { describe, expect, it } from "vitest";

import { buildListenRouter } from "../src/commands/listen-router.js";
import { NodeConfig } from "../src/services/config.js";
import {
  type EventHistoryOwner,
  HISTORY_READY_MAXIMUM_LAG_BLOCKS,
  type HistoryOwnerFrontier,
} from "../src/services/event-history-owner.js";
import {
  Globals,
  nextL1ProviderHealthEvidence,
} from "../src/services/globals.js";
import { Lucid } from "../src/services/lucid.js";
import { MidgardContracts } from "../src/services/midgard-contracts.js";
import { ValidationPool } from "../src/services/validation-pool.js";
import { provideDatabaseLayers } from "./utils.js";

// Only the settings /readyz reads. Fresh exact provider evidence keeps the
// handler off Lucid and the contracts, so those are never dereferenced.
const nodeConfig = {
  READINESS_L1_PROVIDER_EVIDENCE_MAX_AGE_MS: 60_000,
  L1_PROVIDER_PREFLIGHT_TIMEOUT_MS: 1_000,
  READINESS_MAX_HEARTBEAT_AGE_MS: 60_000,
  READINESS_MAX_DURABLE_ADMISSION_BACKLOG: 1_000,
  READINESS_MAX_DURABLE_ADMISSION_AGE_MS: 60_000,
  UNCONFIRMED_BLOCK_MAX_AGE_MS: 60_000,
  VALIDATION_WORKER_JOB_TIMEOUT_MS: 60_000,
  STATE_QUEUE_MUTATION_LEASE_STALE_GRACE_MS: 60_000,
  MPF_ENGINE: "legacy",
  MIN_QUEUE_LENGTH_FOR_MERGING: 1,
} as unknown as NodeConfig["Type"];

const readyz = (frontier: HistoryOwnerFrontier | undefined) =>
  Effect.runPromise(
    provideDatabaseLayers(
      Effect.gen(function* () {
        const globals = yield* Globals;
        yield* Ref.update(globals.L1_PROVIDER_HEALTH, (current) =>
          nextL1ProviderHealthEvidence({
            current,
            healthy: true,
            observedAtMs: Date.now(),
            successKind: "exact",
          }),
        );
        yield* Ref.set(
          globals.EVENT_HISTORY_OWNER,
          frontier === undefined
            ? undefined
            : ({
                frontier: Effect.succeed(frontier),
                retentionHold: Effect.succeed(undefined),
              } as unknown as EventHistoryOwner),
        );
        const response = (yield* buildListenRouter().pipe(
          Effect.provideService(
            HttpServerRequest.HttpServerRequest,
            HttpServerRequest.fromWeb(
              new Request("http://midgard.test/readyz"),
            ),
          ),
        )) as HttpServerResponse.HttpServerResponse;
        const body = (yield* Effect.promise(() =>
          HttpServerResponse.toWeb(response).json(),
        )) as {
          readonly reasons: readonly string[];
          readonly eventHistoryFrontier: HistoryOwnerFrontier | null;
        };
        return {
          history: body.reasons.filter((reason) =>
            reason.startsWith("history_"),
          ),
          eventHistoryFrontier: body.eventHistoryFrontier,
        };
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
        Effect.provide(Globals.Default),
      ) as Effect.Effect<
        {
          history: string[];
          eventHistoryFrontier: HistoryOwnerFrontier | null;
        },
        unknown,
        never
      >,
    ),
  );

const frontier = (ready: boolean, lagBlocks: number): HistoryOwnerFrontier => ({
  ready,
  headHeight: 100,
  tipHeight: 100 + lagBlocks,
  lagBlocks,
  maximumLagBlocks: HISTORY_READY_MAXIMUM_LAG_BLOCKS,
});

describe("GET /readyz history frontier", () => {
  it.each([
    ["an open gate at the tip", frontier(true, 0), []],
    [
      "an open gate exactly at the lag bound",
      frontier(true, HISTORY_READY_MAXIMUM_LAG_BLOCKS),
      [],
    ],
    [
      "an open gate past the lag bound",
      frontier(true, HISTORY_READY_MAXIMUM_LAG_BLOCKS + 1),
      [
        `history_follower_lagging:${HISTORY_READY_MAXIMUM_LAG_BLOCKS + 1}:${HISTORY_READY_MAXIMUM_LAG_BLOCKS}`,
      ],
    ],
    [
      "a closed gate, however far behind",
      frontier(false, HISTORY_READY_MAXIMUM_LAG_BLOCKS + 1),
      ["history_owner_not_ready"],
    ],
  ])("reports %s", async (_, observed, reasons) => {
    const result = await readyz(observed);
    expect(result.history).toEqual(reasons);
    expect(result.eventHistoryFrontier).toEqual(observed);
  });

  it("reports no frontier and no history reason without a history owner", async () => {
    expect(await readyz(undefined)).toEqual({
      history: [],
      eventHistoryFrontier: null,
    });
  });
});
