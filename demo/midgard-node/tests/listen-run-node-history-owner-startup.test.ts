import "./utils.js";

import { Cause, Effect, Exit, Ref } from "effect";
import { describe, expect, it, vi } from "vitest";

import { runNode } from "../src/commands/listen.run-node.js";
import { NodeConfig } from "../src/services/config.js";
import { Globals } from "../src/services/globals.js";

// runNode's own wiring of the history-owner startup: the predecessor-lease
// wait it provides and the previous-process lease releases its startup
// preparation runs. Every step before the owner is stubbed, and the startup
// mutation gate fails on purpose, so nothing past the releases runs.

// Factories run while runNode's module graph loads (./utils.js already pulls
// it in), before this file's own imports and declarations initialise: they
// reach only hoisted state and modules they import themselves.
const { seen, recordStep } = vi.hoisted(() => {
  const seen = {
    wait: undefined as unknown,
    leaseDurationMs: undefined as unknown,
    steps: [] as string[],
  };
  const recordStep = async (name: string) =>
    (await import("effect")).Effect.sync(() => {
      seen.steps.push(name);
    });
  return { seen, recordStep };
});

vi.mock("../src/services/index.js", async (importOriginal) => ({
  ...(await importOriginal<typeof import("../src/services/index.js")>()),
  validationPoolLayer: (await import("effect")).Layer.empty,
  mempoolLedgerCacheLayer: (await import("effect")).Layer.empty,
}));
vi.mock("../src/services/settlement.js", async (importOriginal) => ({
  ...(await importOriginal<typeof import("../src/services/settlement.js")>()),
  settlementWalletAddress: () => "addr_test",
}));
vi.mock("../src/e2e/phase1-accept-crash-checkpoint.js", async () => ({
  assertPhase1AcceptCrashCheckpointConfiguration: (await import("effect"))
    .Effect.void,
}));
vi.mock("../src/services/native-mpf-startup.js", async (importOriginal) => {
  const { Effect } = await import("effect");
  return {
    ...(await importOriginal<
      typeof import("../src/services/native-mpf-startup.js")
    >()),
    requirePinnedNativeOwnerBinary: () => Effect.void,
  };
});
vi.mock("../src/da/startup.js", async (importOriginal) => {
  const { Effect } = await import("effect");
  return {
    ...(await importOriginal<typeof import("../src/da/startup.js")>()),
    runDaIdentityGatedStartupSequence: () => Effect.void,
  };
});
vi.mock("../src/commands/listen-startup.js", async (importOriginal) => {
  const { Effect } = await import("effect");
  return {
    ...(await importOriginal<
      typeof import("../src/commands/listen-startup.js")
    >()),
    seedLatestLocalBlockBoundaryOnStartup: await recordStep("seed"),
    hydratePendingBlockFinalizationOnStartup: await recordStep("hydrate"),
    releaseStateQueueLeasesOfPreviousNodeProcess:
      await recordStep("state-queue-leases"),
    assertStartupMutationJobsRecoverable: (
      await recordStep("mutation-gate")
    ).pipe(Effect.zipRight(Effect.fail(new Error("stop after the releases")))),
  };
});
vi.mock(
  "../src/commands/listen-startup.release-ledger-store-lease-of-previous-node-process.js",
  async () => ({
    releaseLedgerStoreLeaseOfPreviousNodeProcess:
      await recordStep("ledger-mpf-lease"),
  }),
);
vi.mock("../src/services/event-history-runtime.js", async () => {
  const { Effect, Option } = await import("effect");
  const { PredecessorLeaseWait } = await import(
    "../src/database/eventHistoryAuthority.js"
  );
  return {
    makeProductionEventHistoryOwner: (input: {
      readonly leaseDurationMs: number;
      readonly prepareCompletion: (
        checkpoint: unknown,
        preparation: { readonly assertCurrent: Effect.Effect<void> },
      ) => Effect.Effect<void, unknown>;
    }) =>
      Effect.gen(function* () {
        seen.wait = Option.getOrUndefined(
          yield* Effect.serviceOption(PredecessorLeaseWait),
        );
        seen.leaseDurationMs = input.leaseDurationMs;
        return yield* input.prepareCompletion(undefined, {
          assertCurrent: Effect.void,
        });
      }),
  };
});

describe("runNode history-owner startup wiring", () => {
  it("waits out a killed predecessor's lease and retires its leases before the mutation gate", async () => {
    const program = Effect.gen(function* () {
      const nativeOwner = yield* Ref.make<unknown>(undefined);
      return yield* (
        runNode(false) as unknown as Effect.Effect<
          void,
          unknown,
          NodeConfig | Globals
        >
      ).pipe(
        Effect.provideService(NodeConfig, {
          STARTUP_PROTOCOL_STATUS_QUERY_MAX_ATTEMPTS: 1,
          STARTUP_PROTOCOL_STATUS_QUERY_RETRY_DELAY_MS: 0,
        } as unknown as NodeConfig["Type"]),
        Effect.provideService(Globals, {
          NATIVE_MPF_OWNER: nativeOwner,
        } as unknown as Globals),
      );
    });
    const exit = await Effect.runPromiseExit(program);
    expect(Exit.isFailure(exit)).toBe(true);
    if (Exit.isFailure(exit))
      expect(Cause.pretty(exit.cause)).toMatch(
        /Authenticated history owner startup failed/,
      );
    // One 60 s lease plus a 10 s margin: a renewed lease still refuses.
    expect(seen.wait).toEqual({ marginMs: 10_000, pollIntervalMs: 2_000 });
    expect(seen.leaseDurationMs).toBe(60_000);
    expect(seen.steps).toEqual([
      "seed",
      "hydrate",
      "state-queue-leases",
      "ledger-mpf-lease",
      "mutation-gate",
    ]);
  });
});
