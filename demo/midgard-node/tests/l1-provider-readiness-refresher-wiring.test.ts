import "./utils.js";

import { Duration, Effect, Fiber, Ref, Schedule } from "effect";
import { describe, expect, it, vi } from "vitest";

import { runNodeFiberSet } from "../src/commands/listen.node-fibers.js";
import { NodeConfig } from "../src/services/config.js";
import { Globals } from "../src/services/globals.js";
import { Lucid } from "../src/services/lucid.js";
import { MidgardContracts } from "../src/services/midgard-contracts.js";

// Records each production probe call, so the test sees exactly what the
// refresher runNode starts reads and with which arguments.
const probes = vi.hoisted(() => ({
  calls: [] as { readonly probe: string; readonly args: unknown }[],
  hubOracleHangs: false,
}));

vi.mock("../src/transactions/initialization.js", async (importOriginal) => ({
  ...(await importOriginal<
    typeof import("../src/transactions/initialization.js")
  >()),
  fetchHubOracleWitness: (lucid: unknown, contracts: unknown) =>
    Effect.suspend(() => {
      probes.calls.push({ probe: "hub_oracle", args: { lucid, contracts } });
      return probes.hubOracleHangs ? Effect.never : Effect.succeed(null);
    }),
}));

vi.mock("../src/local-ogmios-slot.js", async (importOriginal) => ({
  ...(await importOriginal<typeof import("../src/local-ogmios-slot.js")>()),
  readLocalOgmiosSubmitSlot: (options: unknown) =>
    Effect.sync(() => {
      probes.calls.push({ probe: "local_ogmios_slot", args: options });
      return ogmiosSlot;
    }),
}));

const ogmiosSlot = {
  source: "local_ogmios_tip" as const,
  currentSlot: 77,
  observedAtMs: 1_000,
  slotLengthMs: 1_000,
};
const lucidApi = { name: "lucid-api" };
const contracts = { name: "contracts" };

// Every interval the fiber set schedules on, plus what the refresher reads.
const nodeConfigWith = (preflightTimeoutMs: number) =>
  ({
    ADMISSION_BACKLOG_REFRESH_MS: 1_000,
    MIDGARD_DA_PUBLISH_RECONCILE_INTERVAL_MS: 1_000,
    WAIT_BETWEEN_BLOCK_COMMITMENT: 1_000,
    WAIT_BETWEEN_BLOCK_CONFIRMATION: 1_000,
    SPECULATIVE_COMMIT_BUILD: false,
    USER_EVENT_BARRIER_REFRESH_MS: 1_000,
    WAIT_BETWEEN_DEPOSIT_UTXO_FETCHES: 1_000,
    WAIT_BETWEEN_RETENTION_SWEEPS: 1_000,
    WAIT_BETWEEN_MERGE_TXS: 1_000,
    TX_QUEUE_POLL_INTERVAL_MS: 1_000,
    L1_PROVIDER_PREFLIGHT_TIMEOUT_MS: preflightTimeoutMs,
    L1_OGMIOS_KEY: "http://ogmios.wiring.test",
  }) as unknown as NodeConfig["Type"];

// Stand-ins for the fibers runNode builds from its startup state.
const startupFibers = {
  historyOwnerStopped: Effect.never,
  appThread: Effect.never,
  retainedPayloadServer: Effect.never,
};

/** The record runNode hands to Effect.all. */
const runNodeFibers = (preflightTimeoutMs: number) =>
  runNodeFiberSet({
    nodeConfig: nodeConfigWith(preflightTimeoutMs),
    withMonitoring: false,
    startupFibers,
  });

/**
 * Starts the refresher from the fiber set runNode runs and returns the first
 * evidence it publishes.
 */
const firstRefreshOfNodeFiberSet = (preflightTimeoutMs: number) => {
  const nodeConfig = nodeConfigWith(preflightTimeoutMs);
  return Effect.runPromise(
    Effect.gen(function* () {
      const globals = yield* Globals;
      const refresher = yield* Effect.fork(
        runNodeFibers(preflightTimeoutMs).l1ProviderReadinessRefresher,
      );
      yield* Ref.get(globals.L1_PROVIDER_HEALTH).pipe(
        Effect.repeat({
          schedule: Schedule.spaced(Duration.millis(5)),
          until: (current) => current.evidenceRevision > 0,
        }),
        Effect.timeoutFail({
          duration: Duration.seconds(5),
          onTimeout: () =>
            new Error("The node fiber set published no L1 provider evidence"),
        }),
      );
      yield* Fiber.interrupt(refresher);
      return yield* Ref.get(globals.L1_PROVIDER_HEALTH);
    }).pipe(
      Effect.provideService(NodeConfig, nodeConfig),
      Effect.provideService(Lucid, { api: lucidApi } as unknown as Lucid),
      Effect.provideService(
        MidgardContracts,
        contracts as unknown as MidgardContracts,
      ),
      Effect.provide(Globals.Default),
    ),
  );
};

describe("L1 provider readiness refresher wiring in runNode", () => {
  it("runs the refresher alongside the startup fibers", () => {
    const fibers = runNodeFibers(50);

    expect(Object.keys(fibers)).toEqual(
      expect.arrayContaining([
        "historyOwnerStopped",
        "appThread",
        "retainedPayloadServer",
        "l1ProviderReadinessRefresher",
      ]),
    );
    expect(fibers).toMatchObject(startupFibers);
  });

  it.each([
    { preflightTimeoutMs: 50, holdMs: 50 },
    { preflightTimeoutMs: 10_000, holdMs: 2_000 },
  ])(
    "reads the HubOracle and then the local Ogmios slot, bounded at $holdMs ms for a $preflightTimeoutMs ms preflight timeout",
    async ({ preflightTimeoutMs, holdMs }) => {
      probes.calls.length = 0;
      probes.hubOracleHangs = false;
      const evidence = await firstRefreshOfNodeFiberSet(preflightTimeoutMs);

      expect(probes.calls).toEqual([
        { probe: "hub_oracle", args: { lucid: lucidApi, contracts } },
        {
          probe: "local_ogmios_slot",
          args: { ogmiosUrl: "http://ogmios.wiring.test", timeoutMs: holdMs },
        },
      ]);
      expect(evidence).toMatchObject({
        lastObservationKind: "exact_success",
        lastExactObservationKind: "exact_success",
        lastOgmiosSlot: ogmiosSlot,
      });
    },
  );

  it.each([
    { preflightTimeoutMs: 50, holdMs: 50 },
    { preflightTimeoutMs: 10_000, holdMs: 2_000 },
  ])(
    "releases the control plane after $holdMs ms of a hung HubOracle read for a $preflightTimeoutMs ms preflight timeout",
    async ({ preflightTimeoutMs, holdMs }) => {
      probes.calls.length = 0;
      probes.hubOracleHangs = true;
      const evidence = await firstRefreshOfNodeFiberSet(preflightTimeoutMs);

      expect(probes.calls.map(({ probe }) => probe)).toEqual(["hub_oracle"]);
      expect(evidence).toMatchObject({
        lastObservationKind: "exact_failure",
        lastExactFailure: `L1ControlPlaneTimeoutError: L1 control-plane scope l1_provider_readiness_refresh exceeded ${holdMs.toString()}ms`,
      });
    },
    15_000,
  );
});
