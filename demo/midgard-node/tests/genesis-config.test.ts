import { SELECTED_DEPLOYMENT_PROFILE } from "@al-ft/midgard-core/deployment-profile";
import { Effect } from "effect";
import { afterEach, describe, expect, it, vi } from "vitest";

import { NodeConfig } from "../src/services/config.make-config.js";

afterEach(() => vi.unstubAllEnvs());

const loadConfig = () =>
  Effect.runPromise(NodeConfig.pipe(Effect.provide(NodeConfig.layer)));

describe("compiled network binding", () => {
  // The compiled deployment profile fixes the network before any ledger is
  // configured, so no other network's genesis is reachable.
  it.each(
    (["Preprod", "Preview", "Mainnet", "Custom"] as const).filter(
      (network) => network !== SELECTED_DEPLOYMENT_PROFILE.network,
    ),
  )("refuses to start %s under another compiled profile", async (network) => {
    vi.stubEnv("NETWORK", network);
    await expect(loadConfig()).rejects.toThrow(
      "NETWORK must match the compiled deployment profile",
    );
  });
});
