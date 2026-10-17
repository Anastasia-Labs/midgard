import { Effect } from "effect";
import { afterEach, describe, expect, it, vi } from "vitest";

import type { NodeConfigDep } from "../src/services/config.js";
import {
  FollowerL1AdapterLive,
  L1Adapter,
} from "../src/services/l1-adapter.js";

const openFollower = (config: Partial<NodeConfigDep>) =>
  Effect.runPromise(
    Effect.scoped(
      Effect.flatMap(L1Adapter, (adapter) =>
        adapter.open({ NETWORK: "Preprod", ...config } as NodeConfigDep),
      ),
    ).pipe(Effect.provide(FollowerL1AdapterLive), Effect.flip),
  );

describe("the role adapter (listen and its workers)", () => {
  afterEach(() => {
    vi.unstubAllEnvs();
  });

  it("refuses at start a process configured with a non-follower L1 access, naming it", async () => {
    for (const key of ["L1_ACCESS", "L1_PROVIDER", "L1_BLOCKFROST_URL"])
      vi.stubEnv(key, "");
    vi.stubEnv("L1_KUPO_URL", "http://kupo:1442");
    vi.stubEnv("L1_OGMIOS_URL", "http://ogmios:1337");
    const error = await openFollower({});
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
  });

  it("passes a follower-only environment on to the follower's own settings", async () => {
    for (const key of [
      "L1_PROVIDER",
      "L1_KUPO_URL",
      "L1_OGMIOS_URL",
      "L1_BLOCKFROST_URL",
      "L1_BLOCKFROST_PROJECT_ID",
    ])
      vi.stubEnv(key, "");
    vi.stubEnv("L1_ACCESS", "follower");
    const error = await openFollower({ L1_NATIVE_LEDGER: undefined });
    expect(error.message).toMatch(/The node's L1 provider needs/);
    expect(error.message).not.toMatch(/non-follower/);
    expect(error.cause).toMatchObject({
      _tag: "StartupStepFailedError",
      step: "l1_access",
      reason: "l1_node_unconfigured",
      exhausted: false,
    });
  });
});
