import { describe, expect, beforeAll } from "vitest";
import { Effect } from "effect";
import { Database } from "../src/services/database.js";
import { it } from "@effect/vitest";
import {
  deleteMpt,
  MidgardMpt,
  MptError,
} from "../src/workers/utils/mpt.js";
import { NodeConfig } from "@/services/config.js";
import { Lucid } from "@/services/lucid.js";
import { fromHex } from "@lucid-evolution/lucid";
import dotenv from "dotenv";
import * as ETH_UTILS from "@ethereumjs/util";
dotenv.config({ path: ".env" });

const provideLayers = <A, E, R>(eff: Effect.Effect<A, E, R>) =>
  eff.pipe(
    Effect.provide(Database.layer),
    Effect.provide(Lucid.Default),
    Effect.provide(NodeConfig.layer),
  );

beforeAll(async () => {
  await Effect.runPromise(
    provideLayers(
      Effect.gen(function* () {
        yield* deleteMpt("test-mpt-db", "test-mpt");
      }),
    ),
  );
});

describe("The makeMpts tests", () => {
  it.effect("Initialization and basic functions", (_) =>
    Effect.gen(function* () {
      const mpt = yield* createTrie;
      const mptRootBytes = yield* mpt.getRoot();
      const mptRootHex = yield* mpt.getRootHex();
      const mptRootEmpty = yield* mpt.rootIsEmpty();
      yield* closeDatabase(mpt);
      expect(mptRootBytes).toStrictEqual(
        fromHex(
          "56e81f171bcc55a6ff8345e692c0f86e5b48e01b996cadc001622fb5e363b421",
        ),
      );
      expect(mptRootHex).toBe(
        "56e81f171bcc55a6ff8345e692c0f86e5b48e01b996cadc001622fb5e363b421",
      );
      expect(mptRootEmpty).toBe(true);
    }).pipe(
      Effect.provide(Database.layer),
      Effect.provide(Lucid.Default),
      Effect.provide(NodeConfig.layer),
    ),
  );

  it.effect("Put", (_) =>
    Effect.gen(function* () {
      const mpt = yield* createTrie;
      const putOp: ETH_UTILS.BatchDBOp = {
        type: "put",
        key: txId1,
        value: fromHex(tx1),
      };
      yield* mpt.batch([putOp]);
      const mptRootBytes = yield* mpt.getRoot();
      const mptRootHex = yield* mpt.getRootHex();
      const mptRootEmpty = yield* mpt.rootIsEmpty();
      yield* closeDatabase(mpt);
      expect(mptRootBytes).toStrictEqual(
        fromHex(
          "9cf4055fd9458e7b1f96266162787abcf218598f3213bd65257e2d4d10b144f3",
        ),
      );
      expect(mptRootHex).toBe(
        "9cf4055fd9458e7b1f96266162787abcf218598f3213bd65257e2d4d10b144f3",
      );
      expect(mptRootEmpty).toBe(false);
    }).pipe(
      Effect.provide(Database.layer),
      Effect.provide(Lucid.Default),
      Effect.provide(NodeConfig.layer),
    ),
  );

  it.effect("Root persistence", (_) =>
    Effect.gen(function* () {
      const mpt = yield* createTrie;
      const mptRootBytes = yield* mpt.getRoot();
      const mptRootHex = yield* mpt.getRootHex();
      const mptRootEmpty = yield* mpt.rootIsEmpty();
      yield* closeDatabase(mpt);
      expect(mptRootBytes).toStrictEqual(
        fromHex(
          "9cf4055fd9458e7b1f96266162787abcf218598f3213bd65257e2d4d10b144f3",
        ),
      );
      expect(mptRootHex).toBe(
        "9cf4055fd9458e7b1f96266162787abcf218598f3213bd65257e2d4d10b144f3",
      );
      expect(mptRootEmpty).toBe(false);
    }).pipe(
      Effect.provide(Database.layer),
      Effect.provide(Lucid.Default),
      Effect.provide(NodeConfig.layer),
    ),
  );

  it.effect("Delete", (_) =>
    Effect.gen(function* () {
      const mpt = yield* createTrie;
      const delOp: ETH_UTILS.BatchDBOp = {
        type: "del",
        key: txId1,
      };
      yield* mpt.batch([delOp]);
      const mptRootHex = yield* mpt.getRootHex();
      const mptRootEmpty = yield* mpt.rootIsEmpty();
      yield* closeDatabase(mpt);
      expect(mptRootHex).toBe(
        "56e81f171bcc55a6ff8345e692c0f86e5b48e01b996cadc001622fb5e363b421",
      );
      expect(mptRootEmpty).toBe(true);
    }).pipe(
      Effect.provide(Database.layer),
      Effect.provide(Lucid.Default),
      Effect.provide(NodeConfig.layer),
    ),
  );

  it.effect("Reverts checkpointed changes", (_) =>
    Effect.gen(function* () {
      const mpt = yield* createTrie;
      const putOp: ETH_UTILS.BatchDBOp = {
        type: "put",
        key: txId1,
        value: fromHex(tx1),
      };
      yield* mpt.checkpoint();
      yield* mpt.batch([putOp]);
      yield* mpt.revert();
      const mptRootHex = yield* mpt.getRootHex();
      const mptRootEmpty = yield* mpt.rootIsEmpty();
      yield* closeDatabase(mpt);
      expect(mptRootHex).toBe(
        "56e81f171bcc55a6ff8345e692c0f86e5b48e01b996cadc001622fb5e363b421",
      );
      expect(mptRootEmpty).toBe(true);
    }).pipe(
      Effect.provide(Database.layer),
      Effect.provide(Lucid.Default),
      Effect.provide(NodeConfig.layer),
    ),
  );

  it.effect("Commits checkpointed changes", (_) =>
    Effect.gen(function* () {
      const mpt = yield* createTrie;
      const putOp: ETH_UTILS.BatchDBOp = {
        type: "put",
        key: txId1,
        value: fromHex(tx1),
      };
      yield* mpt.checkpoint();
      yield* mpt.batch([putOp]);
      yield* mpt.commit();
      const mptRootHex = yield* mpt.getRootHex();
      const mptRootEmpty = yield* mpt.rootIsEmpty();
      yield* closeDatabase(mpt);
      expect(mptRootHex).toBe(
        "9cf4055fd9458e7b1f96266162787abcf218598f3213bd65257e2d4d10b144f3",
      );
      expect(mptRootEmpty).toBe(false);
    }).pipe(
      Effect.provide(Database.layer),
      Effect.provide(Lucid.Default),
      Effect.provide(NodeConfig.layer),
    ),
  );
});

const txId1 = Buffer.from("11111111111", "hex");
const tx1 =
  "84a800d9010282825820071204405c4ea4ed17faf45231c77e0808aaee680ca2241d8192c398c87701cb00825820071204405c4ea4ed17faf45231c77e0808aaee680ca2241d8192c398c87701cb020183a300581d702ffbb8e0ca9c656db8ca88f003d4ddc219cd8865a02fa3aa91d4b68a01821a00256ba6a1581cf56b0b5c60c7bca716b05335dc886e87a378115b52de44e33d84c125a158204e6f64658a1f1f7826e0fcbda030fad4a074ad6c534e6f4556fe0875d578a7fe01028201d818590126d8799fd8799f581c0a9d03cb57a83b601e9032b5d26cf59d7c7b9a7d4a0d46a4b3522aabffd87a80d8799f582009aecf46eab5f38bbad7afacd80e5e86314e1bdf06936830602282522de46b3d5820e8126ccc1b8894f7f3184dff5bd4b44b752c01227439c8d14b86165c30585e6c582058e6726497a8f6643236b9aee219cfce991cf573eb5597dc5b6f4097370c670f58200000000000000000000000000000000000000000000000000000000000000000582000000000000000000000000000000000000000000000000000000000000000001b0000019981f121ec1b00000199858035b3581c0a9d03cb57a83b601e9032b5d26cf59d7c7b9a7d4a0d46a4b3522aab581cea71a1290cd3236b49484e8b0bbf7d43634f6a0cabe22b0d682ee2b000ffffa300581d702ffbb8e0ca9c656db8ca88f003d4ddc219cd8865a02fa3aa91d4b68a01821a00277590a1581cf56b0b5c60c7bca716b05335dc886e87a378115b52de44e33d84c125a158204e6f64650a9d03cb57a83b601e9032b5d26cf59d7c7b9a7d4a0d46a4b3522aab01028201d818590145d8799fd8799f581ce4af364a4f65e60ac4c594333daca69e3ab96b8ae20eee4865299667ffd8799f581c0a9d03cb57a83b601e9032b5d26cf59d7c7b9a7d4a0d46a4b3522aabffd8799f58201e1a7d7e3933d0fb5c6ff1de198e5fce4ce669f52cbf2f7490df88aebbd01b59582009aecf46eab5f38bbad7afacd80e5e86314e1bdf06936830602282522de46b3d5820773626cfd9fcfd0d5d8b1965cf4cc1c4120be9ac269442e0e941e79ea288e8f758200000000000000000000000000000000000000000000000000000000000000000582000000000000000000000000000000000000000000000000000000000000000001b0000019981ed8d111b0000019981f121ec581ce4af364a4f65e60ac4c594333daca69e3ab96b8ae20eee4865299667581cea71a1290cd3236b49484e8b0bbf7d43634f6a0cabe22b0d682ee2b000ffff82583900ea71a1290cd3236b49484e8b0bbf7d43634f6a0cabe22b0d682ee2b04c570359cf0701ffb27400a475d98af7c308d440fb1168b316cae4b61a42e0f9fd021a0003687309a1581cf56b0b5c60c7bca716b05335dc886e87a378115b52de44e33d84c125a158204e6f64658a1f1f7826e0fcbda030fad4a074ad6c534e6f4556fe0875d578a7fe010b58203f177f309f3cee86b218b75cdaced0c917157a384fc76ca413cdb29df8f989db0dd9010281825820071204405c4ea4ed17faf45231c77e0808aaee680ca2241d8192c398c87701cb021082583900ea71a1290cd3236b49484e8b0bbf7d43634f6a0cabe22b0d682ee2b04c570359cf0701ffb27400a475d98af7c308d440fb1168b316cae4b61a42bf8cc0111a004c4b40a20582840000d8798082190a491a00082c5f840100d8798082190a491a00082c5f07d9010282585258500101002332259800a998012481154d6964676172642044656d6f20e28093204d696e740014a3149a2a660049211856616c696461746f722072657475726e65642066616c736500136564004ae715cd01585358510101002332259800a998012481164d6964676172642044656d6f20e28093205370656e640014a3149a2a660049211856616c696461746f722072657475726e65642066616c736500136564004ae715cd01f5f6";

const createTrie: Effect.Effect<MidgardMpt, MptError> =
  Effect.gen(function* () {
    return yield* MidgardMpt.create("test-mpt-db", "test-mpt");
  });

const closeDatabase = (trie: MidgardMpt): Effect.Effect<void, Error> =>
  Effect.gen(function* () {
    return yield* Effect.tryPromise({
      try: () => trie.database._leveldb.close(),
      catch: (e) => new Error(`Error closing database: ${e}`),
    });
  });
