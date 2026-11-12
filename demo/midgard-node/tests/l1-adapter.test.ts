import { MIDGARD_CONSENSUS_PROFILE } from "@al-ft/midgard-core/consensus-profile";
import { Effect, Layer } from "effect";
import { afterEach, describe, expect, it, vi } from "vitest";

import { AlwaysSucceedsContract } from "../src/services/always-succeeds.js";
import type { NodeConfigDep } from "../src/services/config.js";
import { FollowerL1AdapterLive } from "../src/services/l1-adapter.follower.js";
import {
  FollowerL1ChainParametersLive,
  L1Adapter,
  L1ChainParameters,
} from "../src/services/l1-adapter.js";
import {
  ContractDeploymentIdentity,
  MidgardContracts,
} from "../src/services/midgard-contracts.js";
import { makeTestNodeConfig } from "./helpers/test-node-config.js";

/** The role's follower adapter over the always-succeeds scaffold, which
 * has no event lists. */
const scaffoldAdapter = FollowerL1AdapterLive.pipe(
  Layer.provide(
    Layer.mergeAll(
      Layer.effect(
        MidgardContracts,
        Effect.map(AlwaysSucceedsContract, (contracts) =>
          MidgardContracts.make({
            ...contracts,
            consensusProfile: MIDGARD_CONSENSUS_PROFILE,
          }),
        ),
      ).pipe(Layer.provide(AlwaysSucceedsContract.Default)),
      Layer.succeed(
        ContractDeploymentIdentity,
        ContractDeploymentIdentity.make({
          kind: "derived",
          consensusProfile: MIDGARD_CONSENSUS_PROFILE,
        }),
      ),
    ),
  ),
);

const openChainParameters = (config: Partial<NodeConfigDep>) =>
  Effect.runPromise(
    Effect.scoped(
      Effect.flatMap(L1ChainParameters, (parameters) =>
        parameters.open(makeTestNodeConfig(config)),
      ),
    ).pipe(Effect.provide(FollowerL1ChainParametersLive), Effect.flip),
  );

const openFollower = (config: Partial<NodeConfigDep>) =>
  Effect.runPromise(
    Effect.scoped(
      Effect.flatMap(L1Adapter, (adapter) =>
        adapter.open(makeTestNodeConfig(config)),
      ),
    ).pipe(Effect.provide(scaffoldAdapter), Effect.flip),
  );

/** Both role layers, the chain parameters and the follower adapter. */
const ROLE_OPENS = [
  ["chain parameters", openChainParameters],
  ["follower adapter", openFollower],
] as const;

const FOLLOWER_ONLY_ENVIRONMENT = () => {
  for (const key of [
    "L1_PROVIDER",
    "L1_KUPO_URL",
    "L1_OGMIOS_URL",
    "L1_BLOCKFROST_URL",
    "L1_BLOCKFROST_PROJECT_ID",
  ])
    vi.stubEnv(key, "");
  vi.stubEnv("L1_ACCESS", "follower");
};

describe("the role adapters (listen and its workers)", () => {
  afterEach(() => {
    vi.unstubAllEnvs();
  });

  it.each(ROLE_OPENS)(
    "%s: refuses at start a process configured with a non-follower L1 access, naming it",
    async (_name, open) => {
      for (const key of ["L1_ACCESS", "L1_PROVIDER", "L1_BLOCKFROST_URL"])
        vi.stubEnv(key, "");
      vi.stubEnv("L1_KUPO_URL", "http://kupo:1442");
      vi.stubEnv("L1_OGMIOS_URL", "http://ogmios:1337");
      const error = await open({});
      expect(error).toMatchObject({ _tag: "ConfigError" });
      expect(error.message).toMatch(
        /midgard-node listen reads L1 only through its follower; refusing to start with a non-follower L1 configuration: L1_KUPO_URL, L1_OGMIOS_URL/,
      );
      // A named startup step, so `listen` holds unready naming it.
      expect(error.cause).toMatchObject({
        _tag: "StartupStepFailedError",
        step: "l1_access",
        reason: "role_non_follower_l1_config",
        exhausted: false,
        cause: { reason: "role_non_follower_l1_config" },
      });
    },
  );

  it.each(ROLE_OPENS)(
    "%s: passes a follower-only environment on to the follower's own settings",
    async (_name, open) => {
      FOLLOWER_ONLY_ENVIRONMENT();
      const error = await open({ L1_NATIVE_LEDGER: undefined });
      expect(error.message).toMatch(/The node's L1 provider needs/);
      expect(error.message).not.toMatch(/non-follower/);
      expect(error.cause).toMatchObject({
        _tag: "StartupStepFailedError",
        step: "l1_access",
        reason: "l1_node_unconfigured",
        exhausted: false,
      });
    },
  );

  it("refuses a deployment its index cannot be built from, before opening anything", async () => {
    FOLLOWER_ONLY_ENVIRONMENT();
    const error = await openFollower({
      L1_NATIVE_LEDGER: {
        socketPath: "/nonexistent/node.socket",
        nodeConfigPath: "/nonexistent/config.json",
        binaryPath: "/nonexistent/transport",
      },
    });
    expect(error.message).toMatch(/event lists are missing/);
    expect(error.cause).toMatchObject({
      _tag: "StartupStepFailedError",
      step: "l1_access",
      reason: "l1_index_unconfigured",
      exhausted: false,
    });
  });
});
