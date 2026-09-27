import "./utils.js";

import { SELECTED_DEPLOYMENT_PROFILE } from "@al-ft/midgard-core/deployment-profile";
import { Effect } from "effect";
import { afterEach, describe, expect, it, vi } from "vitest";

import { NodeConfig } from "../src/services/config.js";

afterEach(() => vi.unstubAllEnvs());

const loadConfig = () =>
  Effect.runPromise(NodeConfig.pipe(Effect.provide(NodeConfig.layer)));

describe("production genesis configuration", () => {
  it("starts the compiled profile's network with the empty ledger committed by atomic initialization", async () => {
    vi.stubEnv("NETWORK", SELECTED_DEPLOYMENT_PROFILE.network);
    const config = await loadConfig();
    // Test defaults contain the old genesis wallets. Their presence must
    // never grant an unauthenticated L2 balance.
    expect(config.GENESIS_UTXOS).toEqual([]);
    expect(config.GENESIS_UTXOS_BY_WALLET).toEqual({ A: [], B: [], C: [] });
  });

  it("does not require synthetic genesis wallet credentials", async () => {
    for (const wallet of ["A", "B", "C"]) {
      vi.stubEnv(`TESTNET_GENESIS_WALLET_SEED_PHRASE_${wallet}`, undefined);
    }
    const config = await loadConfig();
    expect(config.GENESIS_UTXOS).toEqual([]);
  });
});

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
