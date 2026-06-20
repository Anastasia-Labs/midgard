import * as SDK from "@al-ft/midgard-sdk";
import { it } from "@effect/vitest";
import { Effect } from "effect";
import { Level } from "level";
import { afterAll, beforeAll, describe, expect } from "vitest";

import * as Ledger from "../src/database/utils/ledger.js";
import * as Tx from "../src/database/utils/tx.js";
import {
  computeLedgerMpfRootFromLedgerEntries,
  DecodedMempoolTxForCommit,
  deleteMpfStore,
  hydrateLedgerMpfFromLedgerEntries,
  keyValuePhasNonMembershipProof,
  keyValuePhasProof,
  keyValuePhasRoot,
  keyValuePhasRootWithCount,
  MidgardMpf,
  MpfBatchOp,
  orderDecodedMempoolTxsForLedgerApplication,
  verifyKeyValuePhasMembershipProof,
  verifyKeyValuePhasNonMembershipProof,
  withMpfRootTransaction,
} from "../src/workers/utils/mpf.js";

const TEST_DB = "test-mpf-db";
const EMPTY_DELETE_DB = "test-mpf-empty-delete-db";
const CORRUPT_DB = "test-mpf-corrupt-db";
const key1 = Buffer.from("01", "hex");
const key2 = Buffer.from("02", "hex");
const key3 = Buffer.from("03", "hex");
const value1 = Buffer.from("aa", "hex");
const value2 = Buffer.from("bb", "hex");
const value3 = Buffer.from("cc", "hex");

const makeTxHash = (byte: number) => Buffer.alloc(32, byte);
const makeOutRef = (byte: number) => Buffer.from([byte, 0]);

const makeDecodedMempoolTx = ({
  txHash,
  spent,
  produced,
}: {
  readonly txHash: Buffer;
  readonly spent: readonly Buffer[];
  readonly produced: readonly Buffer[];
}): DecodedMempoolTxForCommit => ({
  entry: {
    [Tx.Columns.TX_ID]: txHash,
    [Tx.Columns.TX]: txHash,
    [Tx.Columns.TIMESTAMPTZ]: new Date(0),
  },
  txHash,
  txCbor: txHash,
  spent,
  produced: produced.map((outRef) => ({
    [Ledger.Columns.OUTREF]: outRef,
    [Ledger.Columns.OUTPUT]: Buffer.from("01", "hex"),
  })),
});

beforeAll(async () => {
  await Effect.runPromise(deleteMpfStore(TEST_DB, "test-mpf"));
  await Effect.runPromise(
    deleteMpfStore(EMPTY_DELETE_DB, "test-mpf-empty-delete"),
  );
  await Effect.runPromise(deleteMpfStore(CORRUPT_DB, "test-mpf-corrupt"));
});

afterAll(async () => {
  await Effect.runPromise(deleteMpfStore(TEST_DB, "test-mpf"));
  await Effect.runPromise(
    deleteMpfStore(EMPTY_DELETE_DB, "test-mpf-empty-delete"),
  );
  await Effect.runPromise(deleteMpfStore(CORRUPT_DB, "test-mpf-corrupt"));
});

describe("Midgard MPF wrapper", () => {
  it.effect("initializes to the Midgard MPF empty root", () =>
    Effect.gen(function* () {
      const mpf = yield* MidgardMpf.create("test-mpf", TEST_DB);
      const root = yield* mpf.root();
      const rootHex = yield* mpf.rootHex();
      const rootIsEmpty = yield* mpf.rootIsEmpty();
      yield* mpf.close();

      expect(root).toStrictEqual(
        Buffer.from(SDK.EMPTY_MERKLE_TREE_ROOT, "hex"),
      );
      expect(rootHex).toBe(SDK.EMPTY_MERKLE_TREE_ROOT);
      expect(rootIsEmpty).toBe(true);
    }),
  );

  it.effect("inserts, gets, and persists a value", () =>
    Effect.gen(function* () {
      const mpf = yield* MidgardMpf.create("test-mpf", TEST_DB);
      yield* mpf.insert(key1, value1);
      const rootHex = yield* mpf.rootHex();
      const found = yield* mpf.get(key1);
      yield* mpf.close();

      const reopened = yield* MidgardMpf.create("test-mpf", TEST_DB);
      const reopenedRootHex = yield* reopened.rootHex();
      const reopenedFound = yield* reopened.get(key1);
      yield* reopened.close();

      expect(rootHex).not.toBe(SDK.EMPTY_MERKLE_TREE_ROOT);
      expect(found._tag).toBe("Some");
      expect(reopenedRootHex).toBe(rootHex);
      expect(reopenedFound._tag).toBe("Some");
    }),
  );

  it.effect("rejects duplicate inserts and missing deletes", () =>
    Effect.gen(function* () {
      const mpf = yield* MidgardMpf.createScratch("test-mpf");
      yield* mpf.insert(key1, value1);
      const duplicate = yield* mpf.insert(key1, value1).pipe(Effect.either);
      const missingDelete = yield* mpf.delete(key2).pipe(Effect.either);

      expect(duplicate._tag).toBe("Left");
      expect(missingDelete._tag).toBe("Left");
    }),
  );

  it.effect(
    "normalizes a delete-to-empty root to the canonical Midgard empty root",
    () =>
      Effect.gen(function* () {
        const mpf = yield* MidgardMpf.create(
          "test-mpf-empty-delete",
          EMPTY_DELETE_DB,
        );
        yield* mpf.insert(key3, value3);
        yield* mpf.delete(key3);
        const rootHex = yield* mpf.rootHex();
        yield* mpf.close();

        const reopened = yield* MidgardMpf.create(
          "test-mpf-empty-delete",
          EMPTY_DELETE_DB,
        );
        const reopenedRootHex = yield* reopened.rootHex();
        yield* reopened.close();

        expect(rootHex).toBe(SDK.EMPTY_MERKLE_TREE_ROOT);
        expect(reopenedRootHex).toBe(SDK.EMPTY_MERKLE_TREE_ROOT);
      }),
  );

  it.effect("applies batches deterministically and preserves old roots", () =>
    Effect.gen(function* () {
      const mpf = yield* MidgardMpf.createScratch("test-mpf");
      const ops: MpfBatchOp[] = [
        { type: "insert", key: key1, value: value1 },
        { type: "insert", key: key2, value: value2 },
      ];
      yield* mpf.applyBatch(ops);
      const firstRoot = yield* mpf.root();
      yield* mpf.delete(key1);
      const secondRoot = yield* mpf.rootHex();
      yield* mpf.resetToRoot(firstRoot);
      const restored = yield* mpf.get(key1);

      expect(firstRoot.toString("hex")).not.toBe(secondRoot);
      expect(restored._tag).toBe("Some");
    }),
  );

  it.effect("hydrates a ledger MPF from durable ledger entries", () =>
    Effect.gen(function* () {
      const mpf = yield* MidgardMpf.createScratch("test-mpf");
      yield* mpf.applyBatch([
        { type: "insert", key: key1, value: value1 },
        { type: "insert", key: key3, value: value3 },
      ]);

      const entries: Ledger.MinimalEntry[] = [
        {
          [Ledger.Columns.OUTREF]: key1,
          [Ledger.Columns.OUTPUT]: value1,
        },
        {
          [Ledger.Columns.OUTREF]: key2,
          [Ledger.Columns.OUTPUT]: value2,
        },
      ];
      const expectedRoot =
        yield* computeLedgerMpfRootFromLedgerEntries(entries);
      const hydratedRoot = yield* hydrateLedgerMpfFromLedgerEntries(
        mpf,
        entries,
      );
      const removedStaleEntry = yield* mpf.get(key3);

      expect(hydratedRoot).toBe(expectedRoot);
      expect(removedStaleEntry._tag).toBe("None");
    }),
  );

  it.effect(
    "produces the same root for the same inserts in different orders",
    () =>
      Effect.gen(function* () {
        const left = yield* MidgardMpf.createScratch("left");
        const right = yield* MidgardMpf.createScratch("right");

        yield* left.applyBatch([
          { type: "insert", key: key1, value: value1 },
          { type: "insert", key: key2, value: value2 },
        ]);
        yield* right.applyBatch([
          { type: "insert", key: key2, value: value2 },
          { type: "insert", key: key1, value: value1 },
        ]);

        expect(yield* left.rootHex()).toBe(yield* right.rootHex());
      }),
  );

  it.effect("rolls back a failed batch to its starting root", () =>
    Effect.gen(function* () {
      const mpf = yield* MidgardMpf.createScratch("test-mpf");
      yield* mpf.insert(key1, value1);
      const before = yield* mpf.rootHex();
      const result = yield* mpf
        .applyBatch([
          { type: "insert", key: key2, value: value2 },
          { type: "insert", key: key1, value: value1 },
        ])
        .pipe(Effect.either);
      const after = yield* mpf.rootHex();
      const key2AfterFailure = yield* mpf.get(key2);

      expect(result._tag).toBe("Left");
      expect(after).toBe(before);
      expect(key2AfterFailure._tag).toBe("None");
    }),
  );

  it.effect("rolls back root marker on failed wrapped work", () =>
    Effect.gen(function* () {
      const mpf = yield* MidgardMpf.createScratch("test-mpf");
      const before = yield* mpf.rootHex();
      const result = yield* withMpfRootTransaction(
        mpf,
        Effect.gen(function* () {
          yield* mpf.insert(key1, value1);
          return yield* Effect.fail(new Error("boom"));
        }),
      ).pipe(Effect.either);
      const after = yield* mpf.rootHex();

      expect(result._tag).toBe("Left");
      expect(after).toBe(before);
    }),
  );

  it.effect("creates and verifies membership proofs", () =>
    Effect.gen(function* () {
      const mpf = yield* MidgardMpf.createScratch("test-mpf");
      yield* mpf.insert(key1, value1);
      const proof = yield* mpf.prove(key1);
      const root = yield* mpf.root();
      const verifiedRoot = yield* mpf.verify(proof, true);

      expect(proof.cbor.length).toBeGreaterThan(0);
      expect(verifiedRoot).toStrictEqual(root);
    }),
  );

  it.effect("builds PHAS roots and proofs without runtime fromList", () =>
    Effect.gen(function* () {
      const phasKey = Buffer.from("4101", "hex");
      const phasValue = Buffer.from("41aa", "hex");
      const root = yield* keyValuePhasRoot([phasKey], [phasValue]);
      const proof = yield* keyValuePhasProof([phasKey], [phasValue], phasKey);

      expect(root).not.toBe(SDK.EMPTY_MERKLE_TREE_ROOT);
      expect(proof).toBeDefined();
    }),
  );

  it.effect("canonicalizes PHAS root inputs and commits the item count", () =>
    Effect.gen(function* () {
      const left = yield* keyValuePhasRootWithCount(
        [key2, key1],
        [value2, value1],
      );
      const right = yield* keyValuePhasRootWithCount(
        [key1, key2],
        [value1, value2],
      );

      expect(left.root).toBe(right.root);
      expect(left.count).toBe(2n);
      expect(left.entries.map((entry) => entry.key.toString("hex"))).toEqual([
        "01",
        "02",
      ]);
    }),
  );

  it.effect("rejects duplicate PHAS root keys before root construction", () =>
    Effect.gen(function* () {
      const result = yield* keyValuePhasRoot(
        [key1, key1],
        [value1, value2],
      ).pipe(Effect.either);

      expect(result._tag).toBe("Left");
    }),
  );

  it.effect("builds and verifies PHAS non-membership proofs", () =>
    Effect.gen(function* () {
      const root = yield* keyValuePhasRoot([key1], [value1]);
      const proof = yield* keyValuePhasNonMembershipProof(
        [key1],
        [value1],
        key2,
      );
      const membership = yield* keyValuePhasProof([key1], [value1], key1);

      yield* verifyKeyValuePhasNonMembershipProof({
        root,
        key: key2,
        proof,
      });
      yield* verifyKeyValuePhasMembershipProof({
        root,
        key: key1,
        value: value1,
        proof: membership,
      });

      const presentKey = yield* verifyKeyValuePhasNonMembershipProof({
        root,
        key: key1,
        proof,
      }).pipe(Effect.either);

      expect(presentKey._tag).toBe("Left");
    }),
  );

  it.effect("rejects PHAS membership proofs with the wrong value", () =>
    Effect.gen(function* () {
      const root = yield* keyValuePhasRoot([key1], [value1]);
      const proof = yield* keyValuePhasProof([key1], [value1], key1);
      const result = yield* verifyKeyValuePhasMembershipProof({
        root,
        key: key1,
        value: value2,
        proof,
      }).pipe(Effect.either);

      expect(result._tag).toBe("Left");
    }),
  );

  it.effect("fails closed when the persisted root marker is corrupt", () =>
    Effect.gen(function* () {
      yield* Effect.promise(async () => {
        const db = new Level<string, string>(CORRUPT_DB, {
          valueEncoding: "json",
        });
        await db.open();
        await db.put("__root__", "not-a-root");
        await db.close();
      });

      const result = yield* MidgardMpf.create(
        "test-mpf-corrupt",
        CORRUPT_DB,
      ).pipe(Effect.either);

      expect(result._tag).toBe("Left");
    }),
  );

  it.effect(
    "orders same-block child transactions after the transactions that produce their inputs",
    () =>
      Effect.gen(function* () {
        const producer = makeDecodedMempoolTx({
          txHash: makeTxHash(1),
          spent: [makeOutRef(1)],
          produced: [makeOutRef(2)],
        });
        const consumer = makeDecodedMempoolTx({
          txHash: makeTxHash(2),
          spent: [makeOutRef(2)],
          produced: [makeOutRef(3)],
        });

        const ordered = yield* orderDecodedMempoolTxsForLedgerApplication([
          consumer,
          producer,
        ]);

        expect(ordered.map((tx) => tx.txHash.toString("hex"))).toStrictEqual([
          producer.txHash.toString("hex"),
          consumer.txHash.toString("hex"),
        ]);
      }),
  );

  it.effect("fails closed on cyclic same-block transaction dependencies", () =>
    Effect.gen(function* () {
      const left = makeDecodedMempoolTx({
        txHash: makeTxHash(1),
        spent: [makeOutRef(2)],
        produced: [makeOutRef(1)],
      });
      const right = makeDecodedMempoolTx({
        txHash: makeTxHash(2),
        spent: [makeOutRef(1)],
        produced: [makeOutRef(2)],
      });

      const result = yield* orderDecodedMempoolTxsForLedgerApplication([
        left,
        right,
      ]).pipe(Effect.either);

      expect(result._tag).toBe("Left");
    }),
  );

  it.effect("fails closed on duplicate normal L2 transaction ids", () =>
    Effect.gen(function* () {
      const txHash = makeTxHash(3);
      const result = yield* orderDecodedMempoolTxsForLedgerApplication([
        makeDecodedMempoolTx({
          txHash,
          spent: [makeOutRef(3)],
          produced: [makeOutRef(4)],
        }),
        makeDecodedMempoolTx({
          txHash,
          spent: [makeOutRef(5)],
          produced: [makeOutRef(6)],
        }),
      ]).pipe(Effect.either);

      expect(result._tag).toBe("Left");
    }),
  );
});
