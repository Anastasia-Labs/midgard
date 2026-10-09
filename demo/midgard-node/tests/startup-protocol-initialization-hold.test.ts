/**
 * The startup protocol check (`ensureProtocolInitializedOnStartup`) holds
 * the startup instead of failing it: a run that fails (here the
 * availability-challenge reward accounts are not registered yet) is run
 * again from the start, the startup waiting under
 * `protocol_initialization_failed`, and the check goes on once a run
 * passes. The deployment's reads are stubbed; the check's own logic runs.
 */
import { Effect } from "effect";
import { describe, expect, it, vi } from "vitest";

import { ensureProtocolInitializedOnStartup } from "../src/commands/listen-startup.js";
import { Lucid, MidgardContracts, NodeConfig } from "../src/services/index.js";
import { IntentJournalWithoutFollower } from "../src/services/intent-journal.js";
import {
  PROTOCOL_INITIALIZATION_FAILED,
  StartupWaitingReporter,
} from "../src/services/startup-waiting.js";

const stub = vi.hoisted(() => ({
  statusReads: 0,
  registrationChecks: 0,
  unregisteredRuns: 0,
}));

vi.mock("../src/commands/contract-deployment-info.js", async (original) => {
  const { Effect: E } = await import("effect");
  return {
    ...(await original<
      typeof import("../src/commands/contract-deployment-info.js")
    >()),
    verifyConfiguredDeploymentManifestIfPresentProgram: E.succeed({
      ok: true,
      manifestId: "manifest",
      path: "contract-deployment-info.json",
      mismatches: [],
      recommendation: "attach",
    }),
  };
});
vi.mock("../src/transactions/initialization.js", async (original) => {
  const { Effect: E } = await import("effect");
  return {
    ...(await original<
      typeof import("../src/transactions/initialization.js")
    >()),
    fetchProtocolDeploymentStatus: () =>
      E.sync(() => {
        stub.statusReads += 1;
        return { complete: true, empty: false, stateQueueInitialized: true };
      }),
  };
});
vi.mock(
  "../src/transactions/availability-challenge-registration.js",
  async (original) => {
    const { Effect: E } = await import("effect");
    const { StateQueueError } = await import("@al-ft/midgard-sdk");
    return {
      ...(await original<
        typeof import("../src/transactions/availability-challenge-registration.js")
      >()),
      assertAvailabilityChallengeRewardAccountsRegisteredProgram: () =>
        E.suspend(() => {
          stub.registrationChecks += 1;
          return stub.registrationChecks <= stub.unregisteredRuns
            ? E.fail(
                new StateQueueError({
                  message:
                    "Availability challenge open reward account is not registered",
                  cause: "test",
                }),
              )
            : E.void;
        }),
    };
  },
);
vi.mock("../src/transactions/reference-scripts.js", async (original) => {
  const { Effect: E } = await import("effect");
  return {
    ...(await original<
      typeof import("../src/transactions/reference-scripts.js")
    >()),
    verifyNodeRuntimeReferenceScriptsProgram: () => E.succeed([]),
  };
});

describe("the startup protocol check", () => {
  it("holds the startup under its reason while a run fails, runs again from the start, and goes on once a run passes", async () => {
    stub.unregisteredRuns = 2;
    const reported: [string, readonly string[]][] = [];
    await Effect.runPromise(
      ensureProtocolInitializedOnStartup.pipe(
        Effect.provide(IntentJournalWithoutFollower),
        Effect.provideService(NodeConfig, {
          NETWORK: "Preprod",
          RUN_GENESIS_ON_STARTUP: false,
          STARTUP_PROTOCOL_STATUS_QUERY_RETRY_DELAY_MS: 0,
        } as unknown as NodeConfig["Type"]),
        Effect.provideService(Lucid, {
          api: {},
          referenceScriptsAddress: "addr_test",
        } as unknown as Lucid),
        Effect.provideService(
          MidgardContracts,
          {} as unknown as MidgardContracts,
        ),
        Effect.locally(StartupWaitingReporter, (key, reasons) =>
          Effect.sync(() => {
            reported.push([key, reasons]);
          }),
        ),
      ),
    );
    // Two failed runs, each from the start, then the run that passes.
    expect(stub.registrationChecks).toBe(3);
    expect(stub.statusReads).toBe(3);
    expect(reported).toEqual([
      ["protocol_initialization", [PROTOCOL_INITIALIZATION_FAILED]],
      ["protocol_initialization", [PROTOCOL_INITIALIZATION_FAILED]],
      ["protocol_initialization", []],
    ]);
  }, 30_000);
});
