import { describe, expect } from "vitest";
import { it } from "@effect/vitest";
import { Effect } from "effect";
import {
  uint32ToFraudProofID,
  createFraudProofCatalogueMpt,
  fraudProofsToIndexedValidators,
} from "@/transactions/initialization.js";
import {
} from "@/transactions/utils.js";
import { AlwaysSucceedsContract } from "@/services/always-succeeds.js";

describe("Fraud Proof Catalogue Root", () => {
  it.effect(
    "computes root and verifies pre-image retrieval for full validator set",
    () =>
      Effect.gen(function* () {
        console.log("Z");
        const contracts = yield* AlwaysSucceedsContract;
        console.log("A");

        const fraudProofs = contracts.fraudProofs;
        console.log("B");

        const indexedFraudProofs = fraudProofsToIndexedValidators(fraudProofs);
        console.log("C");

        const fraudProofsMPT = yield* createFraudProofCatalogueMpt(indexedFraudProofs);
        console.log("D");

        const rootHash = yield* fraudProofsMPT.getRootHex();
        console.log("E");

        const indicesToCheck = [
          0,
          1,
          Math.floor(indexedFraudProofs.length / 2),
          indexedFraudProofs.length - 1,
        ];
        console.log("F");

        for (const i of indicesToCheck) {
          console.log("G", i);
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
