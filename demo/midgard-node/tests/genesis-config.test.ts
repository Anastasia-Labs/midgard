import "./utils.js";

import { Effect } from "effect";
import { afterEach, describe, expect, it, vi } from "vitest";

import { NodeConfig } from "../src/services/config.js";

afterEach(() => vi.unstubAllEnvs());

describe("production genesis configuration", () => {
  it.each(["Preprod", "Preview", "Mainnet"])(
    "starts %s with the empty ledger committed by atomic initialization",
    async (network) => {
      vi.stubEnv("NETWORK", network);
      const config = await Effect.runPromise(
        NodeConfig.pipe(Effect.provide(NodeConfig.layer)),
      );
      // Test defaults contain the old genesis wallets. Their presence must
      // never grant an unauthenticated L2 balance.
      expect(config.GENESIS_UTXOS).toEqual([]);
      expect(config.GENESIS_UTXOS_BY_WALLET).toEqual({ A: [], B: [], C: [] });
    },
  );

  it("does not require synthetic genesis wallet credentials", async () => {
    for (const wallet of ["A", "B", "C"]) {
      vi.stubEnv(`TESTNET_GENESIS_WALLET_SEED_PHRASE_${wallet}`, undefined);
    }
    const config = await Effect.runPromise(
      NodeConfig.pipe(Effect.provide(NodeConfig.layer)),
    );
    expect(config.GENESIS_UTXOS).toEqual([]);
  });
});
