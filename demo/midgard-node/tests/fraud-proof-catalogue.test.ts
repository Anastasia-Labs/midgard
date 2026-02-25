import { describe, expect } from "vitest";
import { it } from "@effect/vitest";
import { Effect } from "effect";
import {
  uint32ToFraudProofID,
  createFraudProofCatalogueMpt,
  fraudProofsToIndexedValidators,
} from "@/transactions/initialization.js";
import { AlwaysSucceedsContract } from "@/services/always-succeeds.js";

describe("Fraud Proof Catalogue Root", () => {
  it.effect(
    "computes root and verifies pre-image retrieval for full validator set",
    () =>
      Effect.gen(function* () {
        const contracts = yield* AlwaysSucceedsContract;

        const fraudProofs = contracts.fraudProofs;

        const indexedFraudProofs = fraudProofsToIndexedValidators(fraudProofs);

        const fraudProofsMPT =
          yield* createFraudProofCatalogueMpt(indexedFraudProofs);

        const rootHash = yield* fraudProofsMPT.getRootHex();
        console.log(`Fraud Proofs Merkle Root: ${rootHash}`);

        const indicesToCheck = [
          0,
          1,
          Math.floor(indexedFraudProofs.length / 2),
          indexedFraudProofs.length - 1,
        ];

        for (const i of indicesToCheck) {
          const retrievedValue = yield* Effect.tryPromise(() =>
            fraudProofsMPT.trie.get(uint32ToFraudProofID(i)),
          );
          const expectedHash = indexedFraudProofs[i][1].spendingScriptHash;
          expect(Buffer.from(retrievedValue!).toString("hex")).toBe(
            expectedHash,
          );
        }
      }).pipe(Effect.provide(AlwaysSucceedsContract.Default)),
  );
});
