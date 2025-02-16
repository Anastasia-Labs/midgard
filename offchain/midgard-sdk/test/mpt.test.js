import { fromHeaderHashes } from "../src/utils/index";
import { Trie } from "@aiken-lang/merkle-patricia-forestry";
import { Effect } from "effect";
import { describe, expect } from "vitest";
import { it } from "@effect/vitest";
describe("fromHeaderHashes", () => {
  it.effect("should create a Trie from header hashes", () =>
    Effect.gen(function* () {
      const headerHashes = [
        "225a4599b804ba53745538c83bfa699ecf8077201b61484c91171f5910a4a8f9",
        "f8077201b61484c91171f5910a4a8f9225a4599b804ba53745538c83bfa699ec"
      ];

      const trie = yield* fromHeaderHashes(headerHashes);
      const key = yield* Effect.promise(() =>
        trie.get("225a4599b804ba53745538c83bfa699ecf8077201b61484c91171f5910a4a8f9")
      );
      console.log('trie.get("225a4599b804ba53745538c83bfa699ecf8077201b61484c91171f5910a4a8f9") :>> ', key.toString());

      expect(trie).toBeInstanceOf(Trie);
    })
  );
});
