import "./utils.js";

import { Effect } from "effect";
import { afterEach, describe, expect, it, vi } from "vitest";

import { MIN_DA_PAYLOAD_RETENTION_DAYS } from "../src/database/retention-policy.js";
import {
  ContractDeploymentIdentity,
  MidgardContractServices,
} from "../src/services/midgard-contracts.make-midgard-contract-runtime.js";

const deriveContracts = vi.hoisted(() => vi.fn());

// Keep the startup service and configuration real. Replace only deployment
// material loading and contract construction, which follow the retention gate.
vi.mock(
  "../src/services/midgard-contracts.load-reference-script-auth-validator.js",
  async (importOriginal) => {
    const actual =
      await importOriginal<
        typeof import("../src/services/midgard-contracts.load-reference-script-auth-validator.js")
      >();
    return {
      ...actual,
      readConfiguredDeploymentManifest: () => undefined,
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

const start = (days: number | undefined) => {
  vi.stubEnv("RETENTION_DAYS", days?.toString());
  return Effect.runPromise(
    ContractDeploymentIdentity.pipe(Effect.provide(MidgardContractServices)),
  );
};

afterEach(() => {
  vi.unstubAllEnvs();
  deriveContracts.mockClear();
});

describe("derived runtime startup retention window", () => {
  it.each([1, MIN_DA_PAYLOAD_RETENTION_DAYS - 1])(
    "refuses an explicit %s-day window before deriving contracts",
    async (days) => {
      await expect(start(days)).rejects.toThrow(
        /shorter than the derived deployment's retention window/u,
      );
      expect(deriveContracts).not.toHaveBeenCalled();
    },
  );

  it.each([
    undefined,
    0,
    MIN_DA_PAYLOAD_RETENTION_DAYS,
    MIN_DA_PAYLOAD_RETENTION_DAYS + 1,
  ])("starts with the supported window %s", async (days) => {
    await expect(start(days)).resolves.toMatchObject({ kind: "derived" });
    expect(deriveContracts).toHaveBeenCalledOnce();
  });
});
