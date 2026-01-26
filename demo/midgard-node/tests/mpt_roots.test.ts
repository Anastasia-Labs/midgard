import { describe, expect } from "vitest";
import { it } from "@effect/vitest";
import { Effect } from "effect";
import * as SDK from "@al-ft/midgard-sdk";
import { computeFraudProofCatalogueMptRoot, createFraudProofCatalogueMpt, uint32ToKey } from "../src/transactions/initialization.js";
import { getOrderedScriptInputs } from "../src/genesis.js";
import { AlwaysSucceedsContract } from "../src/services/always-succeeds.js";
import { provideValidatorTestLayers } from "./utils.js";

describe("Fraud Proof Catalogue Root", () => {
  
  it.effect("computes root and verifies pre-image retrieval for full validator set", () =>
    Effect.gen(function* () {
      const contracts = yield* AlwaysSucceedsContract;
      
      const trie = yield* createFraudProofCatalogueMpt(contracts);
      const rootHash = yield* trie.getRootHex();
      
      // Verify retrieval of specific indices
      const scripts = getOrderedScriptInputs(contracts);
      const indicesToCheck = [0, 1, Math.floor(scripts.length / 2), scripts.length - 1];
      
      for (const i of indicesToCheck) {
        const retrievedValue = yield* Effect.tryPromise(() => trie.trie.get(uint32ToKey(i)));
        const expectedHash = yield* SDK.hashHexWithBlake2b224(scripts[i]);
        expect(Buffer.from(retrievedValue!).toString("hex")).toBe(expectedHash);
      }

    }).pipe(provideValidatorTestLayers)
  );

  it.effect("produces deterministic root hashes for identical inputs", () =>
    Effect.gen(function* () {
      const contracts = yield* AlwaysSucceedsContract;
      
      const rootHash1 = yield* computeFraudProofCatalogueMptRoot(contracts);
      const rootHash2 = yield* computeFraudProofCatalogueMptRoot(contracts);
      
      expect(rootHash1).toBe(rootHash2);
    }).pipe(provideValidatorTestLayers)
  );

  it.effect("produces Blake2b-224 compliant hashes (28 bytes)", () =>
    Effect.gen(function* () {
      const contracts = yield* AlwaysSucceedsContract;
      const trie = yield* createFraudProofCatalogueMpt(contracts);
      
      // Verify all stored hashes are exactly 28 bytes (Blake2b-224)
      const scripts = getOrderedScriptInputs(contracts);
      for (let i = 0; i < scripts.length; i++) {
        const hash = yield* Effect.tryPromise(() => trie.trie.get(uint32ToKey(i)));
        expect(hash?.length).toBe(28);
      }
    }).pipe(provideValidatorTestLayers)
  );
});
