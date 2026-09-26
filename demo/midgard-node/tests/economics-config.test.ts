import "./utils.js";

import { Effect } from "effect";
import { afterEach, describe, expect, it, vi } from "vitest";

import { NodeConfig } from "../src/services/config.js";

const loadConfig = () =>
  Effect.runPromise(
    Effect.gen(function* () {
      return yield* NodeConfig;
    }).pipe(Effect.provide(NodeConfig.layer)),
  );

afterEach(() => {
  vi.unstubAllEnvs();
});

describe("release-bound economics configuration", () => {
  it("uses the explicit bounded profile without inferring economics from Preprod", async () => {
    vi.stubEnv("NETWORK", "Preprod");
    vi.stubEnv("MIDGARD_DEPLOYMENT_PROFILE", "preprod-testing");
    vi.stubEnv("OPERATOR_REQUIRED_BOND_LOVELACE", "900000000");
    vi.stubEnv("OPERATOR_SLASHING_PENALTY_LOVELACE", "500000000");
    vi.stubEnv("WAIT_BETWEEN_RETENTION_SWEEPS", undefined);
    vi.stubEnv("L1_VIEW_FATAL_MS", undefined);

    await expect(loadConfig()).resolves.toMatchObject({
      NETWORK: "Preprod",
      DEPLOYMENT_ECONOMICS_PROFILE: "bounded-acceptance-v1",
      OPERATOR_REQUIRED_BOND_LOVELACE: 900_000_000n,
      OPERATOR_SLASHING_PENALTY_LOVELACE: 500_000_000n,
      WAIT_BETWEEN_RETENTION_SWEEPS: 60_000,
      L1_VIEW_FATAL_MS: 240_000,
    });
  });

  it("rejects selecting the public profile with testing artifacts", async () => {
    vi.stubEnv("NETWORK", "Preprod");
    vi.stubEnv("MIDGARD_DEPLOYMENT_PROFILE", "preprod-public");
    vi.stubEnv("OPERATOR_REQUIRED_BOND_LOVELACE", "100000000000");
    vi.stubEnv("OPERATOR_SLASHING_PENALTY_LOVELACE", "25000000000");

    await expect(loadConfig()).rejects.toThrow(
      /must match the compiled profile/u,
    );
  });

  it("rejects operator economics that disagree with the explicit profile", async () => {
    vi.stubEnv("MIDGARD_DEPLOYMENT_PROFILE", "preprod-testing");
    vi.stubEnv("OPERATOR_REQUIRED_BOND_LOVELACE", "100000000000");
    vi.stubEnv("OPERATOR_SLASHING_PENALTY_LOVELACE", "500000000");

    await expect(loadConfig()).rejects.toThrow(
      /OPERATOR_REQUIRED_BOND_LOVELACE must equal bounded-acceptance-v1 profile economics 900000000/u,
    );
  });

  it("rejects an unknown profile instead of falling back to the Cardano network", async () => {
    vi.stubEnv("MIDGARD_DEPLOYMENT_PROFILE", "preprod");

    await expect(loadConfig()).rejects.toThrow(
      /MIDGARD_DEPLOYMENT_PROFILE must match/u,
    );
  });
});
