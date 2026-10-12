/**
 * The rollback authentication key and the prover and availability wallet
 * secrets are pairwise distinct, and the two wallets resolve to two
 * addresses; either refusal is permanent and made before the operations
 * server binds.
 */
import * as FaultProofs from "@al-ft/midgard-fault-proofs";
import { generatePrivateKey } from "@lucid-evolution/lucid";
import { afterEach, describe, expect, it, vi } from "vitest";

import { WatcherPermanentRefusalError } from "../../src/runtime/permanent-refusal.js";
import type { WatcherProcessConfig } from "../../src/runtime/process-config.js";
import {
  loadWatcherRollbackAuthenticationKey,
  resolveWatcherRuntimeWalletAddresses,
} from "../../src/runtime/watcher-runtime.prepare-authority.js";

vi.mock("@al-ft/midgard-fault-proofs", async (importOriginal) => {
  const original = await importOriginal<typeof FaultProofs>();
  return {
    ...original,
    resolveProverSigner: vi.fn(original.resolveProverSigner),
  };
});

const ROLLBACK = "11".repeat(32);
const PROVER = generatePrivateKey();
const AVAILABILITY = generatePrivateKey();

const env = (variable: string) => ({ kind: "environment" as const, variable });

const config = {
  watcherConfig: {
    targetNetwork: "Preprod",
    storage: { rollbackAuthorityKeySource: env("WBF_TEST_ROLLBACK") },
    proverWallet: { keySource: env("WBF_TEST_PROVER") },
  },
  availability: { keySource: env("WBF_TEST_AVAILABILITY") },
} as unknown as WatcherProcessConfig;

const secrets = (rollback: string, prover: string, availability: string) => {
  vi.stubEnv("WBF_TEST_ROLLBACK", rollback);
  vi.stubEnv("WBF_TEST_PROVER", prover);
  vi.stubEnv("WBF_TEST_AVAILABILITY", availability);
};

afterEach(() => {
  vi.unstubAllEnvs();
  vi.mocked(FaultProofs.resolveProverSigner).mockClear();
});

describe("watcher secret distinctness", () => {
  it("loads the rollback key when all three secrets differ", async () => {
    secrets(ROLLBACK, PROVER, AVAILABILITY);
    expect(
      Buffer.from(await loadWatcherRollbackAuthenticationKey(config)).toString(
        "hex",
      ),
    ).toBe(ROLLBACK);
  });

  it.each([
    ["the two wallets share a secret", ROLLBACK, PROVER, PROVER],
    ["a wallet shares the rollback key", ROLLBACK, ROLLBACK, AVAILABILITY],
    ["the other wallet shares the rollback key", ROLLBACK, PROVER, ROLLBACK],
    // "41" * 32 is the hex form of the 32 bytes "A" * 32.
    [
      "a wallet is the rollback key's bytes",
      "41".repeat(32),
      "A".repeat(32),
      AVAILABILITY,
    ],
  ])(
    "refuses permanently when %s",
    async (_label, rollback, prover, availability) => {
      secrets(rollback, prover, availability);
      const refused = loadWatcherRollbackAuthenticationKey(config);
      await expect(refused).rejects.toBeInstanceOf(
        WatcherPermanentRefusalError,
      );
      await expect(refused).rejects.toThrow("pairwise distinct");
    },
  );

  it("resolves two wallets to two addresses", async () => {
    secrets(ROLLBACK, PROVER, AVAILABILITY);
    const { prover, availability } = await resolveWatcherRuntimeWalletAddresses(
      { config },
    );
    expect(prover).not.toBe(availability);
  });

  it("refuses permanently two secrets that resolve to one wallet", async () => {
    secrets(ROLLBACK, PROVER, AVAILABILITY);
    const original = vi.mocked(FaultProofs.resolveProverSigner);
    const one = original.getMockImplementation()!(
      { network: "Preprod", walletPrivateKey: PROVER },
      Object.freeze({}),
    );
    original.mockReturnValue(one);
    const refused = resolveWatcherRuntimeWalletAddresses({ config });
    await expect(refused).rejects.toBeInstanceOf(WatcherPermanentRefusalError);
    await expect(refused).rejects.toThrow("resolve to the same wallet");
    original.mockReset();
  });
});
