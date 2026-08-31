import "./utils.js";

import { Effect } from "effect";
import { afterEach, describe, expect, it, vi } from "vitest";

import { NodeConfig } from "@/services/config.js";

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
    vi.stubEnv("MIDGARD_DEPLOYMENT_ECONOMICS_PROFILE", "bounded-acceptance-v1");
    vi.stubEnv("OPERATOR_REQUIRED_BOND_LOVELACE", "900000000");
    vi.stubEnv("OPERATOR_SLASHING_PENALTY_LOVELACE", "500000000");

    await expect(loadConfig()).resolves.toMatchObject({
      NETWORK: "Preprod",
      MIDGARD_DEPLOYMENT_ECONOMICS_PROFILE: "bounded-acceptance-v1",
      OPERATOR_REQUIRED_BOND_LOVELACE: 900_000_000n,
      OPERATOR_SLASHING_PENALTY_LOVELACE: 500_000_000n,
    });
  });

  it("selects the public launch tuple on the same Preprod network", async () => {
    vi.stubEnv("NETWORK", "Preprod");
    vi.stubEnv(
      "MIDGARD_DEPLOYMENT_ECONOMICS_PROFILE",
      "public-preprod-launch-v1",
    );
    vi.stubEnv("OPERATOR_REQUIRED_BOND_LOVELACE", "100000000000");
    vi.stubEnv("OPERATOR_SLASHING_PENALTY_LOVELACE", "25000000000");

    await expect(loadConfig()).resolves.toMatchObject({
      NETWORK: "Preprod",
      MIDGARD_DEPLOYMENT_ECONOMICS_PROFILE: "public-preprod-launch-v1",
      OPERATOR_REQUIRED_BOND_LOVELACE: 100_000_000_000n,
      OPERATOR_SLASHING_PENALTY_LOVELACE: 25_000_000_000n,
    });
  });

  it("rejects operator economics that disagree with the explicit profile", async () => {
    vi.stubEnv(
      "MIDGARD_DEPLOYMENT_ECONOMICS_PROFILE",
      "public-preprod-launch-v1",
    );
    vi.stubEnv("OPERATOR_REQUIRED_BOND_LOVELACE", "900000000");
    vi.stubEnv("OPERATOR_SLASHING_PENALTY_LOVELACE", "500000000");

    await expect(loadConfig()).rejects.toThrow(
      /OPERATOR_REQUIRED_BOND_LOVELACE must equal public-preprod-launch-v1 profile economics 100000000000/u,
    );
  });

  it("rejects an unknown profile instead of falling back to the Cardano network", async () => {
    vi.stubEnv("MIDGARD_DEPLOYMENT_ECONOMICS_PROFILE", "preprod");

    await expect(loadConfig()).rejects.toThrow(
      /MIDGARD_DEPLOYMENT_ECONOMICS_PROFILE must explicitly equal/u,
    );
  });
});
