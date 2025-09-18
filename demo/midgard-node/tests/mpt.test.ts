import { Effect } from "effect";
import { Database } from "../src/services/database.js";
import { describe, expect } from "vitest";
import { it } from "@effect/vitest";
import { InitDB } from "../src/database/index.js";
import { makeMpts, processMpts } from "../src/workers/utils/mpt.js";
import { NodeConfig, User } from "../src/config.js";
import { toHex } from "@lucid-evolution/lucid";

import dotenv from "dotenv";
dotenv.config({ path: ".env" });

describe("The makeMpts tests", () => {
  it.effect("Trivial makeMpts", (_) =>
    Effect.gen(function* () {
      yield* InitDB.initializeDb();
      const { ledgerTrie, mempoolTrie } = yield* makeMpts;
      const { utxoRoot, txRoot } = yield* processMpts(
        ledgerTrie,
        mempoolTrie,
        [],
      );

      expect(utxoRoot).toBe(toHex(ledgerTrie.EMPTY_TRIE_ROOT));
      expect(txRoot).toBe(toHex(ledgerTrie.EMPTY_TRIE_ROOT));
    }).pipe(
      Effect.provide(Database.layer),
      Effect.provide(User.layer),
      Effect.provide(NodeConfig.layer),
    ),
  );
});
