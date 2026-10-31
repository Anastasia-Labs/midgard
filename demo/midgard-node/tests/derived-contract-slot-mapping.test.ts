import "./utils.js";

import { NAMED_NETWORK_L1_SLOT_CONFIGS } from "@al-ft/midgard-core/l1-slot-config";
import { L1ProviderTransientError } from "@al-ft/midgard-l1-follower/provider";
import type { SlotConfig } from "@lucid-evolution/lucid";
import {
  Cause,
  Effect,
  Exit,
  Fiber,
  Layer,
  TestClock,
  TestContext,
} from "effect";
import { afterEach, describe, expect, it, vi } from "vitest";

import type { L1Access } from "../src/l1-access.js";
import { L1Adapter } from "../src/services/l1-adapter.js";
import { deploymentL1SlotConfig } from "../src/services/midgard-contracts.load-reference-script-auth-validator.js";
import {
  ContractDeploymentIdentity,
  MidgardContractServices,
} from "../src/services/midgard-contracts.make-midgard-contract-runtime.js";
import {
  findStartupStepFailure,
  L1_SLOT_CONFIG_MISMATCH,
  L1_SLOT_MAPPING_PENDING,
} from "../src/services/startup-waiting.js";

// A manifest-less (derived) contract bundle applies its validation-trace
// validators with the L1 slot mapping the process's L1 port reads off the
// ledger; there is no environment knob. A finalized manifest carries its own
// mapping and never opens L1. A named network refuses a ledger mapping other
// than its static one, and an L1 that stays unreachable fails the startup
// step as a transient that outlived its budget, never as a defect.

const DEVNET = { zeroTime: 1_789_056_000_000, zeroSlot: 0, slotLength: 1000 };

const deriveContracts = vi.hoisted(() => vi.fn());
const network = vi.hoisted(() => ({ value: "Preprod" }));
const configuredManifest = vi.hoisted(() => vi.fn(() => undefined as unknown));

// Keep the contract runtime, the configuration and the slot-mapping rules
// real. The configuration is decoded against the compiled (Preprod) profile;
// only its NETWORK is set per case. Replace deployment material loading and
// contract construction.
vi.mock("../src/services/config.js", async (importOriginal) => {
  const original =
    await importOriginal<typeof import("../src/services/config.js")>();
  const { Effect, Layer } = await import("effect");
  const decoded = original.NodeConfig.layer;
  Object.defineProperty(original.NodeConfig, "layer", {
    value: Layer.effect(
      original.NodeConfig,
      Effect.map(original.NodeConfig, (config) => ({
        ...config,
        NETWORK: network.value as typeof config.NETWORK,
      })),
    ).pipe(Layer.provide(decoded)),
  });
  return original;
});
vi.mock(
  "../src/services/midgard-contracts.load-reference-script-auth-validator.js",
  async (importOriginal) => {
    const actual =
      await importOriginal<
        typeof import("../src/services/midgard-contracts.load-reference-script-auth-validator.js")
      >();
    return {
      ...actual,
      readConfiguredDeploymentManifest: configuredManifest,
      loadReferenceScriptAuthValidator: () => Effect.succeed({}),
      eventHistoryBoundsFromExplicitEnvironment: () => ({}),
      eventHistoryProtectionDurationFromExplicitEnvironment: () => 1n,
      availabilityParametersFromExplicitEnvironment: () => ({}),
    };
  },
);
vi.mock(
  "../src/services/midgard-contracts.with-real-state-queue-and-operator-contracts.js",
  () => ({
    withRealStateQueueAndOperatorContracts: (...args: unknown[]) => {
      deriveContracts(...args);
      return Effect.succeed({});
    },
  }),
);
vi.mock(
  "../src/services/midgard-contracts.assert-deployment-manifest-matches-config.js",
  () => ({ assertDeploymentManifestMatchesConfig: () => undefined }),
);
vi.mock(
  "../src/services/midgard-contracts.midgard-contracts-from-deployment-manifest.js",
  () => ({ midgardContractsFromDeploymentManifest: () => ({}) }),
);

/** A port whose `slotConfig` answers with `read`; counts opens, reads, closes. */
const fakePort = (read: () => Promise<SlotConfig>) => {
  const counts = { opened: 0, reads: 0, closed: 0 };
  const layer = Layer.succeed(L1Adapter, {
    role: "follower",
    open: () =>
      Effect.acquireRelease(
        Effect.sync(() => {
          counts.opened += 1;
          return {
            slotConfig: () => {
              counts.reads += 1;
              return read();
            },
          } as unknown as L1Access;
        }),
        () =>
          Effect.sync(() => {
            counts.closed += 1;
          }),
      ),
  });
  return { layer, counts };
};

const identityOver = (port: Layer.Layer<L1Adapter>) =>
  ContractDeploymentIdentity.pipe(
    Effect.provide(MidgardContractServices.pipe(Layer.provide(port))),
  );

const start = (selected: string, port: Layer.Layer<L1Adapter>) => {
  network.value = selected;
  return Effect.runPromiseExit(identityOver(port));
};

afterEach(() => {
  network.value = "Preprod";
  vi.unstubAllEnvs();
  deriveContracts.mockClear();
  configuredManifest.mockReset();
  configuredManifest.mockReturnValue(undefined);
});

const successOf = <A>(exit: Exit.Exit<A, unknown>): A => {
  if (Exit.isFailure(exit)) throw new Error(Cause.pretty(exit.cause));
  return exit.value;
};

const failureOf = (exit: Exit.Exit<unknown, unknown>) => {
  expect(Exit.isFailure(exit)).toBe(true);
  if (!Exit.isFailure(exit)) throw new Error("unreachable");
  // A failure the startup can name, never a defect.
  expect([...Cause.defects(exit.cause)].map(String)).toEqual([]);
  return findStartupStepFailure(exit.cause);
};

describe("a derived contract bundle's L1 slot mapping", () => {
  it("takes a Custom network's mapping from the L1 port and applies the scripts with it", async () => {
    const port = fakePort(async () => ({ ...DEVNET }));
    const identity = successOf(await start("Custom", port.layer));
    expect(identity.kind).toBe("derived");
    // The mapping the txInfo stage is applied with is the port's, and the
    // startup check reads the same one a manifest recording it would give.
    expect(deriveContracts).toHaveBeenCalledOnce();
    expect(deriveContracts.mock.calls[0]![3]).toMatchObject({
      l1SlotConfig: DEVNET,
    });
    expect(deploymentL1SlotConfig(identity)).toEqual(DEVNET);
    expect(deploymentL1SlotConfig(identity)).toEqual(
      deploymentL1SlotConfig({
        ...identity,
        kind: "manifest",
        manifest: { l1SlotConfig: DEVNET } as never,
      }),
    );
    // The access is opened for the one read and closed.
    expect(port.counts).toEqual({ opened: 1, reads: 1, closed: 1 });
  });

  it("takes a named network's static mapping when the ledger reports it", async () => {
    const port = fakePort(async () => ({
      ...NAMED_NETWORK_L1_SLOT_CONFIGS.Preprod,
    }));
    successOf(await start("Preprod", port.layer));
    expect(deriveContracts.mock.calls[0]![3]).toMatchObject({
      l1SlotConfig: NAMED_NETWORK_L1_SLOT_CONFIGS.Preprod,
    });
  });

  it("refuses a named network whose L1 port reports another mapping, as a named non-transient step", async () => {
    const port = fakePort(async () => ({ ...DEVNET }));
    const step = failureOf(await start("Preprod", port.layer));
    expect(step).toMatchObject({
      step: "derived_l1_slot_mapping",
      reason: L1_SLOT_CONFIG_MISMATCH,
      exhausted: false,
    });
    expect(String(step?.message)).toMatch(
      /the L1 access reports zeroTime=1789056000000,zeroSlot=0,slotLength=1000, not Preprod's/u,
    );
    expect(deriveContracts).not.toHaveBeenCalled();
    expect(port.counts.closed).toBe(1);
  });

  it("waits out an unreachable L1 under its unready reason, then fails as an exhausted transient", async () => {
    network.value = "Custom";
    const port = fakePort(async () => {
      throw new L1ProviderTransientError("transport", "node_unreachable");
    });
    const exit = await Effect.runPromise(
      Effect.gen(function* () {
        const fiber = yield* Effect.fork(Effect.exit(identityOver(port.layer)));
        yield* TestClock.adjust("11 minutes");
        return yield* Fiber.join(fiber);
      }).pipe(Effect.provide(TestContext.TestContext)),
    );
    const step = failureOf(exit);
    expect(step).toMatchObject({
      step: "l1_slot_mapping",
      reason: L1_SLOT_MAPPING_PENDING,
      exhausted: true,
    });
    expect(port.counts.reads).toBeGreaterThan(1);
    expect(port.counts.closed).toBe(1);
    expect(deriveContracts).not.toHaveBeenCalled();
  });

  it("uses a finalized manifest's mapping and never opens L1", async () => {
    vi.stubEnv("MIDGARD_RUN_STATE_PATH", "/nonexistent/midgard-run-state.json");
    configuredManifest.mockReturnValue({
      path: "/nonexistent/manifest.json",
      manifest: {
        manifestId: "ab".repeat(32),
        consensusProfile: {},
        l1SlotConfig: DEVNET,
      },
    });
    const port = fakePort(async () => {
      throw new Error("a manifest deployment must not read L1");
    });
    const identity = successOf(await start("Custom", port.layer));
    expect(identity.kind).toBe("manifest");
    expect(deploymentL1SlotConfig(identity)).toEqual(DEVNET);
    expect(port.counts.opened).toBe(0);
  });
});
