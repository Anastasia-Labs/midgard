/**
 * The startup protocol check (`ensureProtocolInitializedOnStartup`) never
 * runs itself again: only its provider reads wait out a transient failure,
 * each under its budget (`STARTUP_PROTOCOL_STATUS_QUERY_MAX_ATTEMPTS`). An
 * unregistered availability-challenge reward account is a verdict and fails
 * the startup at once under `availability_reward_account_unregistered`; a
 * reward-account read that stays unavailable past the budget fails it under
 * `reward_account_status_unavailable`. A Lucid slot mapping other than the
 * deployment's fails it at once under `l1_slot_config_mismatch`, before any
 * read. The deployment's reads are stubbed; the check's own logic runs.
 */
import {
  type L1SlotConfig,
  NAMED_NETWORK_L1_SLOT_CONFIGS,
} from "@al-ft/midgard-core/l1-slot-config";
import { Effect, Exit } from "effect";
import { beforeEach, describe, expect, it, vi } from "vitest";

import { ensureProtocolInitializedOnStartup } from "../src/commands/listen-startup.js";
import {
  ContractDeploymentIdentity,
  Lucid,
  MidgardContracts,
  NodeConfig,
} from "../src/services/index.js";
import { IntentJournalWithoutFollower } from "../src/services/intent-journal.js";
import {
  AVAILABILITY_REWARD_ACCOUNT_UNREGISTERED,
  findStartupStepFailure,
  L1_SLOT_CONFIG_MISMATCH,
  REWARD_ACCOUNT_STATUS_UNAVAILABLE,
  StartupWaitingReporter,
} from "../src/services/startup-waiting.js";

const stub = vi.hoisted(() => ({
  statusReads: 0,
  registrationChecks: 0,
  /** How many registration reads fail, and with what. */
  failures: 0,
  failure: "unregistered" as "unregistered" | "unreachable",
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
    const actual =
      await original<
        typeof import("../src/transactions/availability-challenge-registration.js")
      >();
    return {
      ...actual,
      assertAvailabilityChallengeRewardAccountsRegisteredProgram: () =>
        E.suspend<
          void,
          | InstanceType<typeof StateQueueError>
          | InstanceType<
              typeof actual.AvailabilityRewardAccountUnregisteredError
            >,
          never
        >(() => {
          stub.registrationChecks += 1;
          if (stub.registrationChecks > stub.failures) return E.void;
          return stub.failure === "unregistered"
            ? E.fail(
                new actual.AvailabilityRewardAccountUnregisteredError({
                  message:
                    "Availability challenge open reward account is not registered",
                  cause: "test",
                }),
              )
            : E.fail(
                new StateQueueError({
                  message:
                    "Failed to read the availability challenge reward accounts",
                  cause: Object.assign(new Error("connect ECONNREFUSED"), {
                    code: "ECONNREFUSED",
                  }),
                }),
              );
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

const MAX_ATTEMPTS = 4;

const PREPROD = NAMED_NETWORK_L1_SLOT_CONFIGS.Preprod;

/** Runs the check with every startup report recorded. */
const runCheck = async (
  slots: {
    /** The mapping the node's Lucid reads from the ledger. */
    readonly lucid?: L1SlotConfig;
    /** The manifest's mapping; absent selects a derived bundle built with
     * Preprod's. */
    readonly manifest?: L1SlotConfig;
  } = { lucid: PREPROD, manifest: PREPROD },
) => {
  const reported: [string, readonly string[]][] = [];
  const exit = await Effect.runPromiseExit(
    ensureProtocolInitializedOnStartup.pipe(
      Effect.provide(IntentJournalWithoutFollower),
      Effect.provideService(NodeConfig, {
        NETWORK: "Preprod",
        RUN_GENESIS_ON_STARTUP: false,
        STARTUP_PROTOCOL_STATUS_QUERY_MAX_ATTEMPTS: MAX_ATTEMPTS,
        STARTUP_PROTOCOL_STATUS_QUERY_RETRY_DELAY_MS: 0,
      } as unknown as NodeConfig["Type"]),
      Effect.provideService(Lucid, {
        api: { config: () => ({ slotConfig: slots.lucid ?? PREPROD }) },
        referenceScriptsAddress: "addr_test",
      } as unknown as Lucid),
      Effect.provideService(
        ContractDeploymentIdentity,
        (slots.manifest === undefined
          ? { kind: "derived", l1SlotConfig: PREPROD }
          : {
              kind: "manifest",
              manifest: { l1SlotConfig: slots.manifest },
            }) as unknown as ContractDeploymentIdentity,
      ),
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
  return {
    exit,
    reported,
    failure: Exit.isFailure(exit)
      ? findStartupStepFailure(exit.cause)
      : undefined,
  };
};

beforeEach(() => {
  stub.statusReads = 0;
  stub.registrationChecks = 0;
});

describe("the startup protocol check", () => {
  it("fails the startup at once under l1_slot_config_mismatch when Lucid's slot mapping differs from the manifest's, reading nothing", async () => {
    stub.failures = 0;
    const { exit, reported, failure } = await runCheck({
      lucid: { ...PREPROD, zeroTime: PREPROD.zeroTime + 1_000 },
      manifest: PREPROD,
    });
    expect(Exit.isFailure(exit)).toBe(true);
    expect(failure).toMatchObject({
      step: "protocol_initialization",
      reason: L1_SLOT_CONFIG_MISMATCH,
      exhausted: false,
    });
    expect(failure?.message).toMatch(/l1_slot_config_mismatch: runtime/u);
    expect(stub.statusReads).toBe(0);
    expect(stub.registrationChecks).toBe(0);
    expect(reported).toEqual([]);
  });

  it("checks the deployment when Lucid's slot mapping is the manifest's or, for a derived bundle, the one it was built with", async () => {
    stub.failures = 0;
    expect(Exit.isSuccess((await runCheck()).exit)).toBe(true);
    expect(Exit.isSuccess((await runCheck({ lucid: PREPROD })).exit)).toBe(
      true,
    );
    const derivedMismatch = await runCheck({
      lucid: NAMED_NETWORK_L1_SLOT_CONFIGS.Preview,
    });
    expect(derivedMismatch.failure).toMatchObject({
      reason: L1_SLOT_CONFIG_MISMATCH,
    });
  });

  it("fails the startup at once on an unregistered reward account, without checking again", async () => {
    stub.failure = "unregistered";
    stub.failures = 2;
    const { exit, reported, failure } = await runCheck();
    expect(Exit.isFailure(exit)).toBe(true);
    expect(failure).toMatchObject({
      step: "availability_reward_accounts",
      reason: AVAILABILITY_REWARD_ACCOUNT_UNREGISTERED,
      exhausted: false,
      attempts: 1,
    });
    expect(stub.registrationChecks).toBe(1);
    expect(stub.statusReads).toBe(1);
    expect(reported).toEqual([]);
  });

  it("waits out an unreachable reward-account read within its budget, reading the status once", async () => {
    stub.failure = "unreachable";
    stub.failures = MAX_ATTEMPTS - 1;
    const { exit, reported } = await runCheck();
    expect(Exit.isSuccess(exit)).toBe(true);
    expect(stub.registrationChecks).toBe(MAX_ATTEMPTS);
    expect(stub.statusReads).toBe(1);
    expect(reported).toEqual([
      ...Array.from({ length: MAX_ATTEMPTS - 1 }, () => [
        "availability_reward_accounts",
        [REWARD_ACCOUNT_STATUS_UNAVAILABLE],
      ]),
      ["availability_reward_accounts", []],
    ]);
  });

  it("fails the startup under reward_account_status_unavailable once the read outlives its budget", async () => {
    stub.failure = "unreachable";
    stub.failures = Number.MAX_SAFE_INTEGER;
    const { failure } = await runCheck();
    expect(failure).toMatchObject({
      step: "availability_reward_accounts",
      reason: REWARD_ACCOUNT_STATUS_UNAVAILABLE,
      exhausted: true,
      attempts: MAX_ATTEMPTS,
    });
    expect(stub.registrationChecks).toBe(MAX_ATTEMPTS);
    expect(stub.statusReads).toBe(1);
  });
});
