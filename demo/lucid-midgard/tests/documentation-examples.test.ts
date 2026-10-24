import { describe, expect, it } from "vitest";

import {
  importComposeAndChain,
  observerTransaction,
  partialSigningAndSubmit,
  providerConvenienceAndStatusStates,
  providerSwitchingAndOverrides,
  safeAndEffectErrors,
  simpleBalancedTransfer,
} from "../examples/usage.js";

const TX_ID = /^[0-9a-f]{64}$/;

/**
 * Every shipped example builds its own wallet from a freshly generated
 * ed25519 key (`CML.PrivateKey.generate_ed25519()` in `makeExampleContext`),
 * so the transaction ids the examples return are *not* fixed vectors and
 * cannot be pinned. What is deterministic is the relationship between the
 * values an example returns, and those relationships are the claims the
 * README makes — so they are what this suite asserts. The forbidden-provider
 * scan that used to live here is now the `no-restricted-syntax` rule on
 * `lucid-midgard/examples/**` in demo/eslint.config.mjs, reported as lint.
 */
describe("documentation and examples", () => {
  it("runs the package examples against the in-memory provider", async () => {
    const transferId = await simpleBalancedTransfer();
    expect(transferId).toMatch(TX_ID);

    // The switched provider plus the instance UTxO override resolve to the
    // single example input, not to the union of both provider views.
    await expect(providerSwitchingAndOverrides()).resolves.toBe(1);

    await expect(providerConvenienceAndStatusStates()).resolves.toEqual([
      "queued",
      "accepted",
      "rejected",
      "E_EXAMPLE",
      "committed",
    ]);

    const observerId = await observerTransaction();
    expect(observerId).toMatch(TX_ID);

    // The claim the compose/import example documents: re-importing the
    // composed transaction's CBOR reproduces the very transaction whose
    // derived output the local chain handed back, so the imported hash and
    // the chained output's txHash are the same id. Asserting only
    // `toHaveLength(2)` let an importer that hashed different bytes pass.
    const composed = await importComposeAndChain();
    expect(composed).toHaveLength(2);
    expect(composed[0]).toMatch(TX_ID);
    expect(composed[1]).toBe(composed[0]);

    const partialId = await partialSigningAndSubmit();
    expect(partialId).toMatch(TX_ID);

    // Three different example transactions over three independently generated
    // wallets: an example that silently returned a shared constant (or the
    // all-zero id the memory provider reports for an unknown transaction)
    // would collapse these.
    expect(new Set([transferId, observerId, partialId, composed[0]]).size).toBe(
      4,
    );
    expect([transferId, observerId, partialId, composed[0]]).not.toContain(
      "00".repeat(32),
    );

    await expect(safeAndEffectErrors()).resolves.toEqual({
      promiseCode: "BUILDER_INVARIANT",
      safeCode: "BUILDER_INVARIANT",
      effectCode: "BUILDER_INVARIANT",
      timeoutCode: "PROVIDER_ERROR",
    });
  });
});
