import { Trie } from "@aiken-lang/merkle-patricia-forestry";
import * as SDK from "@al-ft/midgard-sdk";
import { it } from "@effect/vitest";
import { Data as LucidData } from "@lucid-evolution/lucid";
import blake2b from "blake2b";
import { Effect } from "effect";
import { Level } from "level";
import { afterAll, beforeAll, describe, expect, expectTypeOf } from "vitest";

import * as Ledger from "../src/database/utils/ledger.js";
import * as Tx from "../src/database/utils/tx.js";
import {
  applyLedgerOpsToUtxoPayloadAggregateFromFullValues,
  applyTraceLedgerOpsToMpf,
  buildTransitionTraceResult,
  computeLedgerMpfRootFromLedgerEntries,
  computeUtxoPayloadRoot,
  configureMpfArenaLimits,
  configureMpfPathHydration,
  DecodedMempoolTxForCommit,
  deleteMpfStore,
  encodeEventToStepValueCbor,
  encodeTransitionEventKeyCbor,
  encodeTransitionIntegerCbor,
  encodeTransitionPhaseCbor,
  encodeTransitionStepCbor,
  estimateMpfStoredValueBytes,
  hydrateLedgerMpfFromLedgerEntries,
  keyValuePhasNonMembershipProof,
  keyValuePhasProof,
  keyValuePhasRoot,
  keyValuePhasRootWithCount,
  ledgerOutputToInsertBatchOpV1,
  type LedgerOverlayHandle,
  MidgardMpf,
  MpfBatchOp,
  orderDecodedMempoolTxsForLedgerApplication,
  resetMpfArenaLimits,
  setMpfScratchBuild,
  type TransitionTraceSourceEvent,
  utxoPayloadAggregateFromEntries,
  validateValidationTraceEventKeySet,
  verifyKeyValuePhasMembershipProof,
  verifyKeyValuePhasNonMembershipProof,
  withMpfBlockOverlays,
  withMpfRootTransaction,
  withMpfRootTransactions,
} from "../src/mpf/index.js";
import {
  AuthenticatedPackedMpfArena,
  EventFlatMutationArena,
  ResumedEventFlatOverlayV1,
} from "../src/workers/utils/mpf-event-flat.js";
import { prepareEventFlatDigest } from "../src/workers/utils/mpf-event-flat-digest.js";
import { compileAuthenticatedFlatMpfMultiproof } from "../src/workers/utils/mpf-flat-multiproof.js";
import { makeOutRefCbor } from "./midgard-output-helpers.js";

const TEST_DB = "test-mpf-db";
const EMPTY_DELETE_DB = "test-mpf-empty-delete-db";
const BATCH_PERSIST_DB = "test-mpf-batch-persist-db";
const CORRUPT_DB = "test-mpf-corrupt-db";
const OVERLAY_DB = "test-mpf-overlay-db";
const OVERLAY_RESET_DB = "test-mpf-overlay-reset-db";
const OVERLAY_SPILL_DB = "test-mpf-overlay-spill-db";
const OVERLAY_FORK_DB = "test-mpf-overlay-fork-db";
const SPECULATIVE_LEDGER_RECOVERY_DB =
  "test-mpf-speculative-ledger-recovery-db";
const SPECULATIVE_TX_RECOVERY_DB = "test-mpf-speculative-tx-recovery-db";
const OVERLAY_FAILURE_DB = "test-mpf-overlay-failure-db";
const PATH_HYDRATION_DB = "test-mpf-path-hydration-db";
const PATH_HYDRATION_FAILURE_DB = "test-mpf-path-hydration-failure-db";
const key1 = Buffer.from("01", "hex");
const key2 = Buffer.from("02", "hex");
const key3 = Buffer.from("03", "hex");
const value1 = Buffer.from("aa", "hex");
const value2 = Buffer.from("bb", "hex");
const value3 = Buffer.from("cc", "hex");

const mpfDigest = (value: Buffer): Buffer =>
  Buffer.from(blake2b(32).update(value).digest());

const mpfMerkleRoot = (children: readonly (Buffer | undefined)[]): Buffer => {
  let nodes = children.map((child) => child ?? Buffer.alloc(32));
  while (nodes.length > 1) {
    const next: Buffer[] = [];
    for (let index = 0; index < nodes.length; index += 2) {
      next.push(mpfDigest(Buffer.concat([nodes[index]!, nodes[index + 1]!])));
    }
    nodes = next;
  }
  return nodes[0]!;
};

const mpfLeafHash = (prefix: string, value: Buffer): Buffer => {
  const odd = prefix.length % 2 > 0;
  const head = odd
    ? Buffer.from([0, Number.parseInt(prefix[0]!, 16)])
    : Buffer.from([255]);
  const tail = Buffer.from(odd ? prefix.slice(1) : prefix, "hex");
  return mpfDigest(Buffer.concat([head, tail, mpfDigest(value)]));
};

const buildDeepSharedMpfDag = (
  key: Buffer,
  value: Buffer,
  depth = 10,
  sharedPrefix = "f",
) => {
  const path = mpfDigest(key).toString("hex");
  const records = new Map<string, Record<string, unknown>>();
  const chosenLeaf = {
    __kind: "Leaf",
    prefix: path.slice(depth),
    key: key.toString("hex"),
    value: value.toString("hex"),
  };
  let currentHash = mpfLeafHash(chosenLeaf.prefix, value);
  records.set(currentHash.toString("hex"), chosenLeaf);

  const sharedValue = Buffer.from("5a", "hex");
  const sharedLeaf = {
    __kind: "Leaf",
    prefix: sharedPrefix,
    key: Buffer.alloc(32, 0x5a).toString("hex"),
    value: sharedValue.toString("hex"),
  };
  const sharedHash = mpfLeafHash(sharedPrefix, sharedValue);
  records.set(sharedHash.toString("hex"), sharedLeaf);
  const chainHashes: string[] = [];

  for (let index = depth - 1; index >= 0; index -= 1) {
    const selected = Number.parseInt(path[index]!, 16);
    const sibling = (selected + 1) % 16;
    const childHashes = Array<Buffer | undefined>(16).fill(undefined);
    childHashes[selected] = currentHash;
    childHashes[sibling] = sharedHash;
    const children = childHashes.map((child) => child?.toString("hex"));
    currentHash = mpfDigest(mpfMerkleRoot(childHashes));
    records.set(currentHash.toString("hex"), {
      __kind: "Branch",
      prefix: "",
      children,
      size: depth - index + 1,
    });
    chainHashes[index] = currentHash.toString("hex");
  }
  return {
    root: currentHash.toString("hex"),
    path,
    records,
    sharedHash: sharedHash.toString("hex"),
    chainHashes,
  };
};

const seedSerializedMpfDag = async (
  path: string,
  dag: ReturnType<typeof buildDeepSharedMpfDag>,
): Promise<void> => {
  const level = new Level<string, string | Record<string, unknown>>(path, {
    valueEncoding: "json",
  });
  await level.open();
  await level.batch([
    ...[...dag.records].map(([key, value]) => ({
      type: "put" as const,
      key,
      value,
    })),
    { type: "put" as const, key: "__root__", value: dag.root },
  ]);
  await level.close();
};

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
  await prepareEventFlatDigest();
  await Effect.runPromise(deleteMpfStore(TEST_DB, "test-mpf"));
  await Effect.runPromise(
    deleteMpfStore(EMPTY_DELETE_DB, "test-mpf-empty-delete"),
  );
  await Effect.runPromise(
    deleteMpfStore(BATCH_PERSIST_DB, "test-mpf-batch-persist"),
  );
  await Effect.runPromise(deleteMpfStore(CORRUPT_DB, "test-mpf-corrupt"));
  await Effect.runPromise(deleteMpfStore(OVERLAY_DB, "test-mpf-overlay"));
  await Effect.runPromise(
    deleteMpfStore(OVERLAY_RESET_DB, "test-mpf-overlay-reset"),
  );
  await Effect.runPromise(
    deleteMpfStore(OVERLAY_SPILL_DB, "test-mpf-overlay-spill"),
  );
  await Effect.runPromise(
    deleteMpfStore(OVERLAY_FORK_DB, "test-mpf-overlay-fork"),
  );
  await Effect.runPromise(
    deleteMpfStore(OVERLAY_FAILURE_DB, "test-mpf-overlay-failure"),
  );
  await Effect.runPromise(
    deleteMpfStore(PATH_HYDRATION_DB, "test-mpf-path-hydration"),
  );
  await Effect.runPromise(
    deleteMpfStore(
      PATH_HYDRATION_FAILURE_DB,
      "test-mpf-path-hydration-failure",
    ),
  );
  await Effect.runPromise(
    deleteMpfStore(
      SPECULATIVE_LEDGER_RECOVERY_DB,
      "test-mpf-speculative-ledger-recovery",
    ),
  );
  await Effect.runPromise(
    deleteMpfStore(
      SPECULATIVE_TX_RECOVERY_DB,
      "test-mpf-speculative-tx-recovery",
    ),
  );
});

afterAll(async () => {
  await Effect.runPromise(deleteMpfStore(TEST_DB, "test-mpf"));
  await Effect.runPromise(
    deleteMpfStore(EMPTY_DELETE_DB, "test-mpf-empty-delete"),
  );
  await Effect.runPromise(
    deleteMpfStore(BATCH_PERSIST_DB, "test-mpf-batch-persist"),
  );
  await Effect.runPromise(deleteMpfStore(CORRUPT_DB, "test-mpf-corrupt"));
  await Effect.runPromise(deleteMpfStore(OVERLAY_DB, "test-mpf-overlay"));
  await Effect.runPromise(
    deleteMpfStore(OVERLAY_RESET_DB, "test-mpf-overlay-reset"),
  );
  await Effect.runPromise(
    deleteMpfStore(OVERLAY_SPILL_DB, "test-mpf-overlay-spill"),
  );
  await Effect.runPromise(
    deleteMpfStore(OVERLAY_FORK_DB, "test-mpf-overlay-fork"),
  );
  await Effect.runPromise(
    deleteMpfStore(OVERLAY_FAILURE_DB, "test-mpf-overlay-failure"),
  );
  await Effect.runPromise(
    deleteMpfStore(PATH_HYDRATION_DB, "test-mpf-path-hydration"),
  );
  await Effect.runPromise(
    deleteMpfStore(
      PATH_HYDRATION_FAILURE_DB,
      "test-mpf-path-hydration-failure",
    ),
  );
  await Effect.runPromise(
    deleteMpfStore(
      SPECULATIVE_LEDGER_RECOVERY_DB,
      "test-mpf-speculative-ledger-recovery",
    ),
  );
  await Effect.runPromise(
    deleteMpfStore(
      SPECULATIVE_TX_RECOVERY_DB,
      "test-mpf-speculative-tx-recovery",
    ),
  );
});

describe("Midgard MPF wrapper", () => {
  it("conservatively estimates raw path-cache nodes without JSON encoding", () => {
    const values = [
      {
        __kind: "Leaf",
        prefix: "abc",
        key: "01".repeat(32),
        value: "ff".repeat(4_096),
      },
      {
        __kind: "Branch",
        prefix: "",
        children: Array.from({ length: 16 }, (_, index) =>
          index % 3 === 0 ? index.toString(16).repeat(64) : null,
        ),
        size: 1_000_000,
      },
    ];
    for (const value of values) {
      expect(estimateMpfStoredValueBytes(value)).toBeGreaterThanOrEqual(
        Buffer.byteLength(JSON.stringify(value)),
      );
    }
  });

  it("matches Lucid canonical Plutus CBOR for every transition variant and integer boundary", () => {
    const lucidEncode = (value: unknown, schema: unknown): string =>
      LucidData.to(value as never, schema as never);
    const outputReference: SDK.OutputReference = {
      transactionId: "11".repeat(32),
      outputIndex: 24n,
    };
    const eventKeys: readonly SDK.EventKey[] = [
      { WithdrawalEventKey: { withdrawal_id: outputReference } },
      { ForcedTransactionEventKey: { tx_order_id: outputReference } },
      { L2TransactionEventKey: { tx_id: "22".repeat(32) } },
      { DepositEventKey: { deposit_id: outputReference } },
    ];
    const phases: readonly SDK.TransitionPhase[] = [
      "Withdrawal",
      "ForcedTransaction",
      "L2Transaction",
      "Deposit",
    ];
    const integerBoundaries = [
      0n,
      23n,
      24n,
      255n,
      256n,
      65_535n,
      65_536n,
      4_294_967_295n,
      4_294_967_296n,
      18_446_744_073_709_551_615n,
      18_446_744_073_709_551_616n,
      -1n,
      -24n,
      -25n,
      -256n,
      -257n,
      -18_446_744_073_709_551_616n,
      -18_446_744_073_709_551_617n,
    ];
    for (const integer of integerBoundaries) {
      expect(encodeTransitionIntegerCbor(integer).toString("hex")).toBe(
        lucidEncode(integer, LucidData.Integer()),
      );
    }
    for (const [index, eventKey] of eventKeys.entries()) {
      const phase = phases[index]!;
      expect(encodeTransitionEventKeyCbor(eventKey).toString("hex")).toBe(
        lucidEncode(eventKey, SDK.EventKeySchema),
      );
      expect(encodeTransitionPhaseCbor(phase).toString("hex")).toBe(
        lucidEncode(phase, SDK.TransitionPhaseSchema),
      );
      const eventToStep: SDK.EventToStepValue = {
        step_index: integerBoundaries[index + 1]!,
        phase,
      };
      expect(encodeEventToStepValueCbor(eventToStep).toString("hex")).toBe(
        lucidEncode(eventToStep, SDK.EventToStepValueSchema),
      );
      const transitionStep: SDK.TransitionStep = {
        schema_version: 1n,
        step_index: integerBoundaries[index + 1]!,
        event_key: eventKey,
        phase,
        pre_utxos_root: "33".repeat(32),
        post_utxos_root: "44".repeat(32),
      };
      expect(encodeTransitionStepCbor(transitionStep).toString("hex")).toBe(
        lucidEncode(transitionStep, SDK.TransitionStepSchema),
      );
    }
  });

  it("rejects a validation-trace provider that substitutes a non-transaction transition event", async () => {
    const expectedEventKey: SDK.EventKey = {
      L2TransactionEventKey: { tx_id: "22".repeat(32) },
    };
    const substitutedEventKey: SDK.EventKey = {
      DepositEventKey: {
        deposit_id: {
          transactionId: "33".repeat(32),
          outputIndex: 0n,
        },
      },
    };
    const expectedKeyCbor =
      encodeTransitionEventKeyCbor(expectedEventKey).toString("hex");
    const substitutedKeyCbor =
      encodeTransitionEventKeyCbor(substitutedEventKey);
    const transitionEventKeyCbors = new Set([
      expectedKeyCbor,
      substitutedKeyCbor.toString("hex"),
    ]);

    await expect(
      Effect.runPromise(
        validateValidationTraceEventKeySet({
          expectedEventKeys: [expectedEventKey],
          transitionEventKeyCbors,
          members: [
            {
              eventKey: substitutedEventKey,
              keyCbor: substitutedKeyCbor,
            },
          ],
        }),
      ),
    ).rejects.toThrow(
      "Validation trace provider returned a duplicate, foreign, or non-canonical event key",
    );
    await expect(
      Effect.runPromise(
        validateValidationTraceEventKeySet({
          expectedEventKeys: [expectedEventKey],
          transitionEventKeyCbors,
          members: [
            {
              eventKey: expectedEventKey,
              keyCbor: Buffer.from(expectedKeyCbor, "hex"),
            },
          ],
        }),
      ),
    ).resolves.toBeUndefined();
  });

  it("exports the exact Phase 4 overlay promotion contract", () => {
    expectTypeOf<ReturnType<LedgerOverlayHandle["promote"]>>().toMatchTypeOf<
      Effect.Effect<void, unknown, never>
    >();
  });
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

  it.effect("persists the root marker after a successful batch", () =>
    Effect.gen(function* () {
      const mpf = yield* MidgardMpf.create(
        "test-mpf-batch-persist",
        BATCH_PERSIST_DB,
      );
      yield* mpf.applyBatch([
        { type: "insert", key: key1, value: value1 },
        { type: "insert", key: key2, value: value2 },
      ]);
      const rootHex = yield* mpf.rootHex();
      yield* mpf.close();

      const reopened = yield* MidgardMpf.create(
        "test-mpf-batch-persist",
        BATCH_PERSIST_DB,
      );
      const reopenedRootHex = yield* reopened.rootHex();
      const reopenedValue = yield* reopened.get(key2);
      yield* reopened.close();

      expect(rootHex).not.toBe(SDK.EMPTY_MERKLE_TREE_ROOT);
      expect(reopenedRootHex).toBe(rootHex);
      expect(reopenedValue._tag).toBe("Some");
    }),
  );

  it.effect("hydrates a ledger MPF from durable ledger entries", () =>
    Effect.gen(function* () {
      const mpf = yield* MidgardMpf.createScratch("test-mpf");
      yield* mpf.applyBatch([
        { type: "insert", key: key1, value: value1 },
        { type: "insert", key: key3, value: value3 },
      ]);

      const firstOutRef = makeOutRefCbor(0x11);
      const secondOutRef = makeOutRefCbor(0x22);
      const outputCbor = Buffer.from(
        "a200581d70aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa018200a0",
        "hex",
      );
      const entries: Ledger.MinimalEntry[] = [
        {
          [Ledger.Columns.OUTREF]: firstOutRef,
          [Ledger.Columns.OUTPUT]: outputCbor,
        },
        {
          [Ledger.Columns.OUTREF]: secondOutRef,
          [Ledger.Columns.OUTPUT]: outputCbor,
        },
      ];
      const expectedRoot =
        yield* computeLedgerMpfRootFromLedgerEntries(entries);
      const hydratedRoot = yield* hydrateLedgerMpfFromLedgerEntries(
        mpf,
        entries,
      );
      const payloadRoot = yield* computeUtxoPayloadRoot(
        entries.map((entry) => ({
          outref: entry[Ledger.Columns.OUTREF],
          output: entry[Ledger.Columns.OUTPUT],
        })),
      );
      const obsoleteRawOutputRoot = yield* keyValuePhasRoot(
        entries.map((entry) => entry[Ledger.Columns.OUTREF]),
        entries.map((entry) => entry[Ledger.Columns.OUTPUT]),
      );
      const removedStaleEntry = yield* mpf.get(key3);
      const hydratedDescriptor = yield* mpf.get(firstOutRef);
      const expectedDescriptor = ledgerOutputToInsertBatchOpV1({
        outRef: firstOutRef,
        outputCbor,
      }).value;

      expect(hydratedRoot).toBe(expectedRoot);
      expect(payloadRoot).toBe(expectedRoot);
      expect(obsoleteRawOutputRoot).not.toBe(expectedRoot);
      expect(removedStaleEntry._tag).toBe("None");
      expect(hydratedDescriptor._tag).toBe("Some");
      if (hydratedDescriptor._tag === "Some") {
        expect(hydratedDescriptor.value).toEqual(expectedDescriptor);
      }
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

  it.effect(
    "resets or preserves multiple MPF roots at the worker boundary",
    () =>
      Effect.gen(function* () {
        const assertOutputBoundary = (preserve: boolean) =>
          Effect.gen(function* () {
            const left = yield* MidgardMpf.createScratch("left");
            const right = yield* MidgardMpf.createScratch("right");
            const leftBefore = yield* left.rootHex();
            const rightBefore = yield* right.rootHex();
            const result = yield* withMpfRootTransactions(
              [left, right],
              Effect.gen(function* () {
                yield* left.applyBatch([
                  { type: "insert", key: key1, value: value1 },
                ]);
                yield* right.applyBatch([
                  { type: "insert", key: key2, value: value2 },
                ]);
                return preserve;
              }),
              (value) => value,
            );

            expect(result).toBe(preserve);
            if (preserve) {
              expect(yield* left.rootHex()).not.toBe(leftBefore);
              expect(yield* right.rootHex()).not.toBe(rightBefore);
            } else {
              expect(yield* left.rootHex()).toBe(leftBefore);
              expect(yield* right.rootHex()).toBe(rightBefore);
            }
          });

        yield* assertOutputBoundary(false);
        yield* assertOutputBoundary(true);

        const left = yield* MidgardMpf.createScratch("left");
        const right = yield* MidgardMpf.createScratch("right");
        const leftBefore = yield* left.rootHex();
        const rightBefore = yield* right.rootHex();
        const failed = yield* withMpfRootTransactions(
          [left, right],
          Effect.gen(function* () {
            yield* left.applyBatch([
              { type: "insert", key: key1, value: value1 },
            ]);
            yield* right.applyBatch([
              { type: "insert", key: key2, value: value2 },
            ]);
            return yield* Effect.fail(new Error("boom"));
          }),
          () => true,
        ).pipe(Effect.either);

        expect(failed._tag).toBe("Left");
        expect(yield* left.rootHex()).toBe(leftBefore);
        expect(yield* right.rootHex()).toBe(rightBefore);
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

  it.effect("proves insert and fromList scratch roots are byte-identical", () =>
    Effect.gen(function* () {
      const keys = [
        Buffer.from("100000", "hex"),
        Buffer.from("100001", "hex"),
        Buffer.from("10ffff", "hex"),
        Buffer.from("f00000", "hex"),
      ];
      const values = keys.map((_, index) => Buffer.alloc(8, index + 1));
      setMpfScratchBuild("insert");
      const insertRoot = yield* keyValuePhasRoot(keys, values);
      setMpfScratchBuild("fromlist");
      const fromListRoot = yield* keyValuePhasRoot(keys, values);
      const proof = yield* keyValuePhasProof(keys, values, keys[2]!);
      yield* verifyKeyValuePhasMembershipProof({
        root: fromListRoot,
        key: keys[2]!,
        value: values[2]!,
        proof,
      });
      setMpfScratchBuild("insert");

      expect(fromListRoot).toBe(insertRoot);
    }),
  );

  it.effect(
    "matches legacy roots after reopening a collapsed overlay trie",
    () =>
      Effect.gen(function* () {
        const entries = [
          { key: Buffer.from("100000", "hex"), value: value1 },
          { key: Buffer.from("100001", "hex"), value: value2 },
          { key: Buffer.from("10ffff", "hex"), value: value3 },
          { key: Buffer.from("f00000", "hex"), value: value1 },
        ];
        const ops: readonly MpfBatchOp[] = [
          { type: "delete", key: Buffer.from("100001", "hex") },
          {
            type: "insert",
            key: Buffer.from("100002", "hex"),
            value: Buffer.from("dd", "hex"),
          },
        ];
        const overlay = yield* MidgardMpf.createScratchFromList(
          "collapsed-overlay",
          entries,
          { engine: "overlay" },
        );
        yield* overlay.beginBlockOverlay();
        const initialRoot = yield* overlay.root();
        yield* overlay.flushBlockOverlay(initialRoot);
        yield* overlay.beginBlockOverlay();
        const overlayRoot = yield* overlay.applyBatch(ops);

        const legacy = yield* MidgardMpf.createScratchFromList(
          "collapsed-legacy",
          entries,
          { engine: "legacy" },
        );
        const legacyRoot = yield* legacy.applyBatch(ops);

        expect(overlayRoot).toEqual(legacyRoot);
        expect((yield* overlay.get(Buffer.from("100001", "hex")))._tag).toBe(
          "None",
        );
        expect((yield* overlay.get(Buffer.from("100002", "hex")))._tag).toBe(
          "Some",
        );
        yield* overlay.discardBlockOverlay();
        yield* overlay.close();
        yield* legacy.close();
      }),
  );

  it.effect(
    "hydrates only bounded touched paths and preserves dependent per-step roots",
    () =>
      Effect.gen(function* () {
        const fixtureKey = (index: number): Buffer => {
          const key = Buffer.alloc(32);
          key.writeUInt32BE(index, 28);
          return key;
        };
        const entries = Array.from({ length: 256 }, (_, index) => ({
          key: fixtureKey(index),
          value: Buffer.alloc(32, index % 251),
        }));
        const insertedA = fixtureKey(1_000);
        const insertedB = fixtureKey(1_001);
        const insertedC = fixtureKey(1_002);
        const sourceEvents = [
          {
            phase: "L2Transaction" as const,
            eventKey: {
              L2TransactionEventKey: {
                tx_id: Buffer.alloc(32, 1).toString("hex"),
              },
            } as SDK.EventKey,
            ledgerOps: [
              { type: "delete" as const, key: fixtureKey(0) },
              {
                type: "insert" as const,
                key: insertedA,
                value: Buffer.alloc(32, 1),
              },
            ],
          },
          {
            phase: "L2Transaction" as const,
            eventKey: {
              L2TransactionEventKey: {
                tx_id: Buffer.alloc(32, 2).toString("hex"),
              },
            } as SDK.EventKey,
            ledgerOps: [
              { type: "delete" as const, key: insertedA },
              {
                type: "insert" as const,
                key: insertedB,
                value: Buffer.alloc(32, 2),
              },
            ],
          },
          {
            phase: "L2Transaction" as const,
            eventKey: {
              L2TransactionEventKey: {
                tx_id: Buffer.alloc(32, 3).toString("hex"),
              },
            } as SDK.EventKey,
            ledgerOps: [
              { type: "delete" as const, key: fixtureKey(1) },
              {
                type: "insert" as const,
                key: insertedC,
                value: Buffer.alloc(32, 3),
              },
            ],
          },
        ];
        const overlay = yield* MidgardMpf.createLevelFromListForBenchmark(
          "test-mpf-path-hydration",
          PATH_HYDRATION_DB,
          entries,
          { engine: "overlay" },
        );
        const durableBaseRoot = yield* overlay.persistedRootHex();
        yield* overlay.beginBlockOverlay();
        const levelGetsBefore = (yield* overlay.diagnostics()).levelGets;
        const overlayResult = yield* buildTransitionTraceResult({
          ledgerMpf: overlay,
          sourceEvents,
          withdrawalCount: 0,
          forcedTransactionCount: 0,
          l2TransactionCount: sourceEvents.length,
          depositCount: 0,
        });
        const overlayDiagnostics = yield* overlay.diagnostics();

        const legacy = yield* MidgardMpf.createScratchFromList(
          "test-mpf-path-hydration-legacy",
          entries,
          { engine: "legacy" },
        );
        const legacyResult = yield* buildTransitionTraceResult({
          ledgerMpf: legacy,
          sourceEvents,
          withdrawalCount: 0,
          forcedTransactionCount: 0,
          l2TransactionCount: sourceEvents.length,
          depositCount: 0,
        });

        expect(overlayResult.finalUtxosRoot).toBe(legacyResult.finalUtxosRoot);
        expect(overlayResult.transitionTraceRoot).toBe(
          legacyResult.transitionTraceRoot,
        );
        expect(overlayResult.eventToStepRoot).toBe(
          legacyResult.eventToStepRoot,
        );
        expect(
          overlayResult.transitionTraceMembers.map((member) => member.value),
        ).toStrictEqual(
          legacyResult.transitionTraceMembers.map((member) => member.value),
        );
        expect(overlayResult.pathHydration.uniquePaths).toBe(5);
        expect(overlayResult.pathHydration.maxBatchKeys).toBeGreaterThan(0);
        expect(overlayResult.pathHydration.maxBatchKeys).toBeLessThanOrEqual(
          4_096,
        );
        expect(overlayResult.pathHydration.maxInFlight).toBe(
          overlayResult.pathHydration.maxBatchKeys,
        );
        expect(
          overlayResult.pathHydration.maxBatchKeys * 2_048,
        ).toBeLessThanOrEqual(8 * 1024 * 1024);
        expect(
          overlayResult.pathHydration.maxFrontierPaths,
        ).toBeLessThanOrEqual(overlayResult.pathHydration.uniquePaths);
        expect(overlayResult.pathHydration.loadedNodes).toBeLessThanOrEqual(
          overlayResult.pathHydration.uniquePaths * 80,
        );
        expect(
          overlayResult.pathHydration.retainedBytesEstimate,
        ).toBeLessThanOrEqual(overlayResult.pathHydration.loadedNodes * 2_048);
        expect(overlayDiagnostics.levelGets - levelGetsBefore).toBe(
          overlayResult.pathHydration.loadedNodes,
        );
        expect(overlayDiagnostics.levelGetManyCalls).toBeGreaterThan(0);
        expect(overlayDiagnostics.levelGetManyCalls).toBeLessThan(
          overlayResult.pathHydration.loadedNodes,
        );
        expect(overlayDiagnostics.levelGetManyMaxKeys).toBeGreaterThan(0);
        expect(overlayDiagnostics.levelGetManyMaxKeys).toBeLessThanOrEqual(
          4_096,
        );
        expect(
          overlayDiagnostics.levelGetManyMaxKeys * 2_048,
        ).toBeLessThanOrEqual(8 * 1024 * 1024);
        expect(overlayDiagnostics.levelBatchWrites).toBe(0);
        expect(yield* overlay.persistedRootHex()).toBe(durableBaseRoot);

        yield* overlay.discardBlockOverlay();
        yield* overlay.close();
        yield* legacy.close();
      }),
  );

  it.effect(
    "preserves every dependent transition root across chunk boundaries",
    () =>
      Effect.gen(function* () {
        const fixtureKey = (index: number): Buffer => {
          const key = Buffer.alloc(32);
          key.writeUInt32BE(index, 28);
          return key;
        };
        const entries = Array.from({ length: 256 }, (_, index) => ({
          key: fixtureKey(index),
          value: Buffer.alloc(32, index % 251),
        }));
        const insertedA = fixtureKey(1_000);
        const insertedB = fixtureKey(1_001);
        const insertedC = fixtureKey(1_002);
        const sourceEvents: readonly TransitionTraceSourceEvent[] = [
          {
            phase: "L2Transaction",
            eventKey: {
              L2TransactionEventKey: { tx_id: "11".repeat(32) },
            },
            ledgerOps: [
              { type: "delete", key: fixtureKey(0) },
              { type: "insert", key: insertedA, value: value1 },
            ],
          },
          {
            phase: "L2Transaction",
            eventKey: {
              L2TransactionEventKey: { tx_id: "22".repeat(32) },
            },
            ledgerOps: [
              { type: "delete", key: insertedA },
              { type: "insert", key: insertedB, value: value2 },
            ],
          },
          {
            phase: "L2Transaction",
            eventKey: {
              L2TransactionEventKey: { tx_id: "33".repeat(32) },
            },
            ledgerOps: [
              { type: "delete", key: fixtureKey(1) },
              { type: "insert", key: insertedC, value: value3 },
            ],
          },
          {
            phase: "L2Transaction",
            eventKey: {
              L2TransactionEventKey: { tx_id: "44".repeat(32) },
            },
            ledgerOps: [
              { type: "delete", key: insertedB },
              { type: "insert", key: fixtureKey(0), value: value3 },
            ],
          },
        ];
        configureMpfPathHydration({
          mode: "whole_block",
          chunkOps: 512,
          retainDepth: 2,
        });
        const legacy = yield* MidgardMpf.createScratchFromList(
          "test-mpf-chunked-legacy",
          entries,
          { engine: "legacy" },
        );
        const expected = yield* buildTransitionTraceResult({
          ledgerMpf: legacy,
          sourceEvents,
          withdrawalCount: 0,
          forcedTransactionCount: 0,
          l2TransactionCount: sourceEvents.length,
          depositCount: 0,
        });

        for (const mode of ["chunked", "chunked_arena"] as const) {
          for (const chunkOps of [1, 2, 3, 100] as const) {
            yield* deleteMpfStore(PATH_HYDRATION_DB, "test-mpf-path-hydration");
            configureMpfPathHydration({ mode, chunkOps, retainDepth: 2 });
            const overlay = yield* MidgardMpf.createLevelFromListForBenchmark(
              `test-mpf-${mode}-${chunkOps.toString()}`,
              PATH_HYDRATION_DB,
              entries,
              { engine: "overlay" },
            );
            const durableRoot = yield* overlay.persistedRootHex();
            yield* overlay.beginBlockOverlay();
            const actual = yield* buildTransitionTraceResult({
              ledgerMpf: overlay,
              sourceEvents,
              withdrawalCount: 0,
              forcedTransactionCount: 0,
              l2TransactionCount: sourceEvents.length,
              depositCount: 0,
            });
            const diagnostics = yield* overlay.diagnostics();
            const label = `mode=${mode},chunkOps=${chunkOps.toString()}`;

            expect(actual.finalUtxosRoot, label).toBe(expected.finalUtxosRoot);
            expect(actual.transitionTraceRoot, label).toBe(
              expected.transitionTraceRoot,
            );
            expect(actual.eventToStepRoot, label).toBe(
              expected.eventToStepRoot,
            );
            expect(
              actual.transitionTraceMembers.map((member) => member.value),
              label,
            ).toStrictEqual(
              expected.transitionTraceMembers.map((member) => member.value),
            );
            expect(actual.pathHydration.chunkCount).toBeGreaterThan(0);
            expect(actual.pathHydration.verifiedUpperNodes).toBeGreaterThan(0);
            expect(actual.pathHydration.retainedUpperNodes).toBeLessThanOrEqual(
              273,
            );
            if (mode === "chunked_arena") {
              const flatMultiproof = compileAuthenticatedFlatMpfMultiproof(
                overlay.trie,
              );
              expect(flatMultiproof.rootHash.toString("hex"), label).toBe(
                actual.finalUtxosRoot,
              );
              expect(flatMultiproof.nodeCount, label).toBeGreaterThan(0);
              expect(flatMultiproof.branchCount, label).toBeGreaterThan(0);
              expect(flatMultiproof.estimatedBytes, label).toBeGreaterThan(0);
              expect(actual.pathHydration.checkpointSerializedNodes).toBe(0);
              expect(actual.pathHydration.materializeMs).toBe(0);
              expect(diagnostics.serialiseCalls).toBe(0);
              expect(diagnostics.pathCacheEntries).toBeGreaterThan(0);
              // Architecture C keeps the authenticated whole-block touched DAG
              // decoded, so chunks never re-read even the sealed raw cache.
              expect(diagnostics.pathCacheHits).toBe(0);
              expect(diagnostics.arenaCheckpointCalls).toBe(
                actual.pathHydration.chunkCount + 1,
              );
              expect(diagnostics.transientLiveNodes).toBeGreaterThan(0);
              expect(diagnostics.liveArenaPrunedNodes).toBe(0);
              expect(diagnostics.transientDirtyNodes).toBeGreaterThan(0);
              expect(diagnostics.transientSnapshotsCaptured).toBe(0);
              expect(diagnostics.eventAtomicFinalizations).toBe(
                sourceEvents.length,
              );
              expect(diagnostics.eventAtomicDirtyNodes).toBeGreaterThan(
                diagnostics.eventAtomicFinalizations,
              );
              expect(diagnostics.eventAtomicMaxDirtyNodes).toBeGreaterThan(0);
              expect(diagnostics.retainedSnapshotAuthentications).toBeLessThan(
                diagnostics.storePuts,
              );
            } else {
              expect(
                actual.pathHydration.checkpointSerializedNodes,
              ).toBeGreaterThan(0);
              expect(actual.pathHydration.peakDecodedNodes).toBeLessThanOrEqual(
                Math.max(chunkOps, 2) * 64 + 273,
              );
              expect(diagnostics.arenaCheckpointCalls).toBe(
                actual.pathHydration.chunkCount,
              );
            }
            expect(diagnostics.levelBatchWrites).toBe(0);
            expect(yield* overlay.persistedRootHex()).toBe(durableRoot);
            yield* overlay.discardBlockOverlay();
            yield* overlay.close();
          }
        }
        yield* legacy.close();
      }).pipe(
        Effect.ensuring(
          Effect.sync(() =>
            configureMpfPathHydration({
              mode: "whole_block",
              chunkOps: 512,
              retainDepth: 2,
            }),
          ),
        ),
      ),
  );

  it.effect(
    "reads intermediate retained nodes across delete and insert in one arena mutation",
    () =>
      Effect.gen(function* () {
        yield* deleteMpfStore(PATH_HYDRATION_DB, "test-mpf-path-hydration");
        configureMpfPathHydration({
          mode: "chunked_arena",
          chunkOps: 32,
          retainDepth: 2,
        });
        const entryCount = 512;
        const eventCount = 256;
        const fixtureKey = (index: number): Buffer => {
          const key = Buffer.alloc(32);
          key.writeUInt32BE(index, 28);
          return key;
        };
        const entries = Array.from({ length: entryCount }, (_, index) => ({
          key: fixtureKey(index),
          value: Buffer.alloc(64, index % 251),
        }));
        const sourceEvents: readonly TransitionTraceSourceEvent[] = Array.from(
          { length: eventCount },
          (_, index) => ({
            phase: "L2Transaction" as const,
            eventKey: {
              L2TransactionEventKey: {
                tx_id: fixtureKey(index).toString("hex"),
              },
            } as SDK.EventKey,
            ledgerOps: [
              { type: "delete" as const, key: fixtureKey(index) },
              {
                type: "insert" as const,
                key: fixtureKey(10_000 + index),
                value: Buffer.alloc(64, (index + 1) % 251),
              },
            ],
          }),
        );
        const overlay = yield* MidgardMpf.createLevelFromListForBenchmark(
          "test-mpf-arena-read-own-write",
          PATH_HYDRATION_DB,
          entries,
          { engine: "overlay" },
        );
        yield* Effect.gen(function* () {
          const durableRoot = yield* overlay.persistedRootHex();
          yield* overlay.beginBlockOverlay();
          const result = yield* buildTransitionTraceResult({
            ledgerMpf: overlay,
            sourceEvents,
            withdrawalCount: 0,
            forcedTransactionCount: 0,
            l2TransactionCount: eventCount,
            depositCount: 0,
          });
          expect(result.transitionTraceMembers).toHaveLength(eventCount);
          expect((yield* overlay.get(fixtureKey(222)))._tag).toBe("None");
          expect((yield* overlay.get(fixtureKey(10_222)))._tag).toBe("Some");
          const diagnostics = yield* overlay.diagnostics();
          expect(diagnostics.levelBatchWrites).toBe(0);
          expect(yield* overlay.persistedRootHex()).toBe(durableRoot);
          yield* overlay.discardBlockOverlay();
        }).pipe(
          Effect.ensuring(
            overlay.close().pipe(Effect.catchAll(() => Effect.void)),
          ),
        );
      }).pipe(
        Effect.ensuring(
          Effect.sync(() =>
            configureMpfPathHydration({
              mode: "whole_block",
              chunkOps: 512,
              retainDepth: 2,
            }),
          ),
        ),
      ),
  );

  it.effect(
    "hydrates only delete targets and the neighbors needed for branch collapse",
    () =>
      Effect.gen(function* () {
        const byFirstNibble = new Map<string, Buffer>();
        for (
          let index = 0;
          index < 100_000 && byFirstNibble.size < 16;
          index += 1
        ) {
          const key = Buffer.alloc(32);
          key.writeUInt32BE(index, 28);
          const firstNibble = mpfDigest(key).toString("hex")[0]!;
          if (!byFirstNibble.has(firstNibble)) {
            byFirstNibble.set(firstNibble, key);
          }
        }
        const distinctBranchKeys = [...byFirstNibble.entries()]
          .sort(([left], [right]) => left.localeCompare(right))
          .map(([, key]) => key);
        expect(distinctBranchKeys).toHaveLength(16);

        const cases = [
          { childCount: 16, deleteIndexes: [0], expectedLoadedNodes: 1 },
          { childCount: 2, deleteIndexes: [0], expectedLoadedNodes: 2 },
          { childCount: 3, deleteIndexes: [0, 1], expectedLoadedNodes: 3 },
        ] as const;

        for (const testCase of cases) {
          yield* deleteMpfStore(PATH_HYDRATION_DB, "test-mpf-path-hydration");
          const entries = distinctBranchKeys
            .slice(0, testCase.childCount)
            .map((key, index) => ({
              key,
              value: Buffer.alloc(8, index + 1),
            }));
          const ops: readonly MpfBatchOp[] = testCase.deleteIndexes.map(
            (index) => ({ type: "delete", key: entries[index]!.key }),
          );
          const overlay = yield* MidgardMpf.createLevelFromListForBenchmark(
            `test-mpf-collapse-closure-${testCase.childCount.toString()}`,
            PATH_HYDRATION_DB,
            entries,
            { engine: "overlay" },
          );
          const legacy = yield* MidgardMpf.createScratchFromList(
            `test-mpf-collapse-closure-legacy-${testCase.childCount.toString()}`,
            entries,
            { engine: "legacy" },
          );
          yield* Effect.gen(function* () {
            const durableRoot = yield* overlay.persistedRootHex();
            yield* overlay.beginBlockOverlay();
            const primed = yield* overlay.primeBlockPathArena(ops, 2, false);
            expect(primed.hydration.loadedNodes).toBe(
              testCase.expectedLoadedNodes,
            );
            const [actualRoot, expectedRoot] = yield* Effect.all([
              overlay.applyBatch(ops),
              legacy.applyBatch(ops),
            ]);
            expect(actualRoot).toStrictEqual(expectedRoot);
            expect(yield* overlay.persistedRootHex()).toBe(durableRoot);
            const diagnostics = yield* overlay.diagnostics();
            expect(diagnostics.levelBatchWrites).toBe(0);
            yield* overlay.discardBlockOverlay();
          }).pipe(
            Effect.ensuring(
              Effect.all([overlay.close(), legacy.close()], {
                discard: true,
                concurrency: 2,
              }).pipe(Effect.catchAll(() => Effect.void)),
            ),
          );
        }
      }),
  );

  it.effect(
    "fails closed without spilling when the block path cache exceeds its cap",
    () =>
      Effect.gen(function* () {
        yield* deleteMpfStore(PATH_HYDRATION_DB, "test-mpf-path-hydration");
        configureMpfArenaLimits({
          pathCacheMaxNodes: 1,
          pathCacheMaxBytes: 1,
          liveArenaMaxNodes: 1,
          liveArenaMaxBytes: 1,
        });
        configureMpfPathHydration({
          mode: "chunked_arena",
          chunkOps: 1,
          retainDepth: 2,
        });
        const overlay = yield* MidgardMpf.createLevelFromListForBenchmark(
          "test-mpf-path-cache-cap",
          PATH_HYDRATION_DB,
          [
            { key: key1, value: value1 },
            { key: key2, value: value2 },
            { key: key3, value: value3 },
          ],
          { engine: "overlay" },
        );
        const durableRoot = yield* overlay.persistedRootHex();
        yield* overlay.beginBlockOverlay();
        const result = yield* buildTransitionTraceResult({
          ledgerMpf: overlay,
          sourceEvents: [
            {
              phase: "L2Transaction",
              eventKey: {
                L2TransactionEventKey: { tx_id: "61".repeat(32) },
              },
              ledgerOps: [{ type: "delete", key: key1 }],
            },
          ],
          withdrawalCount: 0,
          forcedTransactionCount: 0,
          l2TransactionCount: 1,
          depositCount: 0,
        }).pipe(Effect.either);
        const diagnostics = yield* overlay.diagnostics();

        expect(result._tag).toBe("Left");
        expect(diagnostics.levelBatchWrites).toBe(0);
        expect(yield* overlay.persistedRootHex()).toBe(durableRoot);
        expect((yield* overlay.root().pipe(Effect.either))._tag).toBe("Left");
        yield* overlay.close();
      }).pipe(
        Effect.ensuring(
          Effect.sync(() => {
            resetMpfArenaLimits();
            configureMpfPathHydration({
              mode: "whole_block",
              chunkOps: 512,
              retainDepth: 2,
            });
          }),
        ),
      ),
  );

  it.effect("fails closed when the event-flat raw proof exceeds its cap", () =>
    Effect.gen(function* () {
      yield* deleteMpfStore(PATH_HYDRATION_DB, "test-mpf-path-hydration");
      const eventFlat = yield* MidgardMpf.createLevelFromListForBenchmark(
        "test-mpf-event-flat-raw-cap",
        PATH_HYDRATION_DB,
        [
          { key: key1, value: value1 },
          { key: key2, value: value2 },
          { key: key3, value: value3 },
        ],
        { engine: "event_flat" },
      );
      const durableRoot = yield* eventFlat.persistedRootHex();
      configureMpfArenaLimits({
        pathCacheMaxNodes: 1,
        pathCacheMaxBytes: 1,
        liveArenaMaxNodes: 1_000_000,
        liveArenaMaxBytes: 1024 * 1024 * 1024,
      });
      yield* eventFlat.beginBlockOverlay();
      const result = yield* eventFlat
        .primeBlockPathArena([{ type: "delete", key: key1 }], 2, false)
        .pipe(Effect.either);
      expect(result._tag).toBe("Left");
      expect(yield* eventFlat.persistedRootHex()).toBe(durableRoot);
      expect((yield* eventFlat.root().pipe(Effect.either))._tag).toBe("Left");
      const diagnostics = yield* eventFlat.diagnostics();
      expect(diagnostics.levelBatchWrites).toBe(0);
      yield* eventFlat.close();
    }).pipe(Effect.ensuring(Effect.sync(() => resetMpfArenaLimits()))),
  );

  it.effect(
    "hydrates insert and delete paths against the canonical empty root",
    () =>
      Effect.gen(function* () {
        yield* deleteMpfStore(PATH_HYDRATION_DB, "test-mpf-path-hydration");
        const overlay = yield* MidgardMpf.create(
          "test-mpf-empty-path-arena",
          PATH_HYDRATION_DB,
          { engine: "overlay" },
        );
        yield* Effect.gen(function* () {
          const durableRoot = yield* overlay.persistedRootHex();
          yield* overlay.beginBlockOverlay();
          const primed = yield* overlay.primeBlockPathArena(
            [
              { type: "insert", key: key1, value: value1 },
              { type: "delete", key: key2 },
            ],
            2,
          );
          expect(primed.hydration.uniquePaths).toBe(2);
          expect(primed.hydration.nodesRequested).toBe(0);
          expect(primed.hydration.loadedNodes).toBe(0);
          const diagnostics = yield* overlay.diagnostics();
          expect(diagnostics.levelBatchWrites).toBe(0);
          expect(yield* overlay.persistedRootHex()).toBe(durableRoot);
          yield* overlay.discardBlockOverlay();
        }).pipe(
          Effect.ensuring(
            overlay.close().pipe(Effect.catchAll(() => Effect.void)),
          ),
        );
      }),
  );

  it.effect(
    "poisons without spilling when the live arena exceeds its cap",
    () =>
      Effect.gen(function* () {
        yield* deleteMpfStore(PATH_HYDRATION_DB, "test-mpf-path-hydration");
        configureMpfArenaLimits({
          pathCacheMaxNodes: 1_000_000,
          pathCacheMaxBytes: 1024 * 1024 * 1024,
          liveArenaMaxNodes: 1,
          liveArenaMaxBytes: 1,
        });
        configureMpfPathHydration({
          mode: "chunked_arena",
          chunkOps: 2,
          retainDepth: 2,
        });
        const overlay = yield* MidgardMpf.createLevelFromListForBenchmark(
          "test-mpf-live-arena-cap",
          PATH_HYDRATION_DB,
          [
            { key: key1, value: value1 },
            { key: key2, value: value2 },
            { key: key3, value: value3 },
          ],
          { engine: "overlay" },
        );
        const durableRoot = yield* overlay.persistedRootHex();
        yield* overlay.beginBlockOverlay();
        const result = yield* buildTransitionTraceResult({
          ledgerMpf: overlay,
          sourceEvents: [
            {
              phase: "L2Transaction",
              eventKey: {
                L2TransactionEventKey: { tx_id: "65".repeat(32) },
              },
              ledgerOps: [
                { type: "delete", key: key1 },
                {
                  type: "insert",
                  key: Buffer.alloc(32, 0x65),
                  value: value1,
                },
              ],
            },
          ],
          withdrawalCount: 0,
          forcedTransactionCount: 0,
          l2TransactionCount: 1,
          depositCount: 0,
        }).pipe(Effect.either);
        const diagnostics = yield* overlay.diagnostics();
        expect(result._tag).toBe("Left");
        expect(diagnostics.levelBatchWrites).toBe(0);
        expect(diagnostics.overlaySpills).toBe(0);
        expect(yield* overlay.persistedRootHex()).toBe(durableRoot);
        expect((yield* overlay.root().pipe(Effect.either))._tag).toBe("Left");
        yield* overlay.close();
      }).pipe(
        Effect.ensuring(
          Effect.sync(() => {
            resetMpfArenaLimits();
            configureMpfPathHydration({
              mode: "whole_block",
              chunkOps: 512,
              retainDepth: 2,
            });
          }),
        ),
      ),
  );

  it.effect(
    "prunes orphan arena versions and atomically reopens the promoted root with proofs",
    () =>
      Effect.gen(function* () {
        yield* deleteMpfStore(PATH_HYDRATION_DB, "test-mpf-path-hydration");
        const baseKey = Buffer.alloc(32, 1);
        const siblingA = Buffer.alloc(32, 2);
        const siblingB = Buffer.alloc(32, 3);
        const temporaryKey = Buffer.alloc(32, 4);
        const finalKey = Buffer.alloc(32, 5);
        configureMpfPathHydration({
          mode: "chunked_arena",
          chunkOps: 2,
          retainDepth: 2,
        });
        const overlay = yield* MidgardMpf.createLevelFromListForBenchmark(
          "test-mpf-arena-promote",
          PATH_HYDRATION_DB,
          [
            { key: baseKey, value: value1 },
            { key: siblingA, value: value2 },
            { key: siblingB, value: value3 },
          ],
          { engine: "overlay" },
        );
        yield* overlay.beginBlockOverlay();
        const built = yield* buildTransitionTraceResult({
          ledgerMpf: overlay,
          sourceEvents: [
            {
              phase: "L2Transaction",
              eventKey: {
                L2TransactionEventKey: { tx_id: "62".repeat(32) },
              },
              ledgerOps: [
                { type: "delete", key: baseKey },
                { type: "insert", key: temporaryKey, value: value1 },
              ],
            },
            {
              phase: "L2Transaction",
              eventKey: {
                L2TransactionEventKey: { tx_id: "63".repeat(32) },
              },
              ledgerOps: [
                { type: "delete", key: temporaryKey },
                { type: "insert", key: finalKey, value: value1 },
              ],
            },
          ],
          withdrawalCount: 0,
          forcedTransactionCount: 0,
          l2TransactionCount: 2,
          depositCount: 0,
        });
        const transientRoot =
          built.transitionTraceMembers[0]!.value.post_utxos_root;
        const beforePromote = yield* overlay.diagnostics();
        expect(beforePromote.levelBatchWrites).toBe(0);
        expect(beforePromote.serialiseCalls).toBe(0);
        yield* overlay.flushBlockOverlay(
          Buffer.from(built.finalUtxosRoot, "hex"),
        );
        yield* overlay.close();

        yield* Effect.promise(async () => {
          const level = new Level<string, string | Record<string, unknown>>(
            PATH_HYDRATION_DB,
            { valueEncoding: "json" },
          );
          await level.open();
          expect(await level.get("__root__")).toBe(built.finalUtxosRoot);
          expect(await level.get(built.finalUtxosRoot)).toBeDefined();
          expect(await level.get(transientRoot).catch(() => undefined)).toBe(
            undefined,
          );
          await level.close();
        });

        const reopened = yield* MidgardMpf.create(
          "test-mpf-arena-promote",
          PATH_HYDRATION_DB,
          { engine: "overlay" },
        );
        expect(yield* reopened.rootHex()).toBe(built.finalUtxosRoot);
        expect((yield* reopened.get(baseKey))._tag).toBe("None");
        expect((yield* reopened.get(finalKey))._tag).toBe("Some");
        const proof = yield* reopened.prove(finalKey);
        expect(yield* reopened.verify(proof, true)).toStrictEqual(
          Buffer.from(built.finalUtxosRoot, "hex"),
        );
        yield* reopened.close();
      }).pipe(
        Effect.ensuring(
          Effect.sync(() =>
            configureMpfPathHydration({
              mode: "whole_block",
              chunkOps: 512,
              retainDepth: 2,
            }),
          ),
        ),
      ),
  );

  it.effect(
    "clone-detaches shared parent arena subtrees across collapse, mutation, and child promotion",
    () =>
      Effect.gen(function* () {
        yield* deleteMpfStore(PATH_HYDRATION_DB, "test-mpf-path-hydration");
        const sharedKey = Buffer.alloc(32, 7);
        const childKey = Buffer.alloc(32, 8);
        const untouchedParentKey = Buffer.alloc(32, 10);
        const parent = yield* MidgardMpf.createLevelFromListForBenchmark(
          "test-mpf-shared-arena-parent",
          PATH_HYDRATION_DB,
          [
            { key: Buffer.alloc(32, 1), value: value1 },
            { key: Buffer.alloc(32, 2), value: value2 },
            { key: Buffer.alloc(32, 3), value: value3 },
          ],
          { engine: "overlay" },
        );
        yield* parent.beginBlockOverlay();
        yield* parent.primeBlockPathArena(
          [
            { type: "insert", key: sharedKey, value: value1 },
            { type: "insert", key: untouchedParentKey, value: value2 },
          ],
          2,
          false,
        );
        yield* parent.applyBatch([
          { type: "insert", key: sharedKey, value: value1 },
          { type: "insert", key: untouchedParentKey, value: value2 },
        ]);
        yield* parent.checkpointAndCollapseDecodedArena(2, false, false);
        const parentRoot = yield* parent.rootHex();

        const child = yield* parent.forkBlockOverlay();
        const parentAfterFork = yield* parent.diagnostics();
        expect(parentAfterFork.transientLiveNodes).toBe(0);
        expect(parentAfterFork.transientSnapshotsCaptured).toBeGreaterThan(0);
        const childOps: readonly MpfBatchOp[] = [
          { type: "delete", key: sharedKey },
          { type: "insert", key: childKey, value: value1 },
        ];
        yield* child.primeBlockPathArena(childOps, 2, false);
        const childRoot = yield* child.applyBatch(childOps);
        yield* child.checkpointAndCollapseDecodedArena(2, false, false);
        const childBeforePromotion = yield* child.diagnostics();
        expect(childBeforePromotion.transientLiveNodes).toBeGreaterThan(0);
        expect(childBeforePromotion.transientDirtyNodes).toBeGreaterThan(0);

        expect(yield* parent.rootHex()).toBe(parentRoot);
        expect((yield* parent.get(sharedKey))._tag).toBe("Some");
        expect((yield* child.get(sharedKey))._tag).toBe("None");
        expect((yield* child.get(childKey))._tag).toBe("Some");
        yield* child.flushBlockOverlay(childRoot);
        yield* parent.close();
        yield* child.close();

        const reopened = yield* MidgardMpf.create(
          "test-mpf-shared-arena-parent",
          PATH_HYDRATION_DB,
          { engine: "overlay" },
        );
        expect(yield* reopened.rootHex()).toBe(childRoot.toString("hex"));
        expect((yield* reopened.get(sharedKey))._tag).toBe("None");
        expect((yield* reopened.get(childKey))._tag).toBe("Some");
        expect((yield* reopened.get(untouchedParentKey))._tag).toBe("Some");
        expect((yield* reopened.get(Buffer.alloc(32, 2)))._tag).toBe("Some");
        const [childProof, untouchedProof] = yield* Effect.all([
          reopened.prove(childKey),
          reopened.prove(untouchedParentKey),
        ]);
        expect(yield* reopened.verify(childProof, true)).toStrictEqual(
          childRoot,
        );
        expect(yield* reopened.verify(untouchedProof, true)).toStrictEqual(
          childRoot,
        );
        yield* reopened.close();
      }),
  );

  it.effect(
    "parks an authenticated transferable delta, releases Level, and resumes without total-state hydration",
    () =>
      Effect.gen(function* () {
        yield* deleteMpfStore(PATH_HYDRATION_DB, "test-mpf-path-hydration");
        const insertedKey = Buffer.alloc(32, 0x51);
        const parkedMpf = yield* MidgardMpf.createLevelFromListForBenchmark(
          "test-mpf-parked-overlay",
          PATH_HYDRATION_DB,
          [
            { key: key1, value: value1 },
            { key: key2, value: value2 },
            { key: key3, value: value3 },
          ],
          { engine: "overlay" },
        );
        const durableRoot = yield* parkedMpf.persistedRootHex();
        yield* parkedMpf.beginBlockOverlay();
        const ops: readonly MpfBatchOp[] = [
          { type: "delete", key: key1 },
          { type: "insert", key: insertedKey, value: value1 },
        ];
        yield* parkedMpf.primeBlockPathArena(ops, 2, false);
        const candidateRoot = yield* parkedMpf.applyBatch(ops);
        const artifact = yield* parkedMpf.parkBlockOverlay();
        expect(Buffer.from(artifact.baseRoot).toString("hex")).toBe(
          durableRoot,
        );
        expect(Buffer.from(artifact.candidateRoot)).toStrictEqual(
          candidateRoot,
        );
        expect(artifact.nodeCount).toBeGreaterThan(0);
        expect(artifact.encodedBytes).toBeGreaterThan(0);

        const transferred = structuredClone(artifact, {
          transfer: [
            artifact.baseRoot,
            artifact.candidateRoot,
            artifact.closureDigest,
            artifact.nodeHashes,
            artifact.nodeValues,
            artifact.nodeValueOffsets,
          ],
        });
        expect(artifact.nodeValues.byteLength).toBe(0);

        // Parking closed the owning handle, so an independent confirmation or
        // local-finalization process can open the same Level path immediately.
        const concurrent = yield* MidgardMpf.create(
          "test-mpf-parked-overlay",
          PATH_HYDRATION_DB,
          { engine: "overlay" },
        );
        expect(yield* concurrent.persistedRootHex()).toBe(durableRoot);
        yield* concurrent.close();

        const tampered = structuredClone(transferred);
        new Uint8Array(tampered.nodeValues)[0] ^= 1;
        const rejected = yield* MidgardMpf.promoteParkedOverlay(
          "test-mpf-parked-overlay",
          PATH_HYDRATION_DB,
          tampered,
        ).pipe(Effect.either);
        expect(rejected._tag).toBe("Left");
        const afterTamper = yield* MidgardMpf.create(
          "test-mpf-parked-overlay",
          PATH_HYDRATION_DB,
          { engine: "overlay" },
        );
        expect(yield* afterTamper.persistedRootHex()).toBe(durableRoot);
        yield* afterTamper.close();

        const resumed = yield* MidgardMpf.resumeParkedOverlay(
          "test-mpf-parked-overlay",
          PATH_HYDRATION_DB,
          transferred,
        );
        expect(resumed.blockOverlayIsActive()).toBe(true);
        expect(yield* resumed.root()).toStrictEqual(candidateRoot);
        expect(yield* resumed.persistedRootHex()).toBe(durableRoot);
        expect((yield* resumed.get(insertedKey))._tag).toBe("Some");
        yield* resumed.flushBlockOverlay(candidateRoot);
        yield* resumed.close();
        const reopened = yield* MidgardMpf.create(
          "test-mpf-parked-overlay",
          PATH_HYDRATION_DB,
          { engine: "overlay" },
        );
        expect(yield* reopened.persistedRootHex()).toBe(
          candidateRoot.toString("hex"),
        );
        expect((yield* reopened.get(key1))._tag).toBe("None");
        expect((yield* reopened.get(insertedKey))._tag).toBe("Some");
        const proof = yield* reopened.prove(insertedKey);
        expect(yield* reopened.verify(proof, true)).toStrictEqual(
          candidateRoot,
        );
        yield* reopened.close();
      }),
  );

  it.effect("keeps Level ownership with the parent while parking a fork", () =>
    Effect.gen(function* () {
      yield* deleteMpfStore(PATH_HYDRATION_DB, "test-mpf-path-hydration");
      const owner = yield* MidgardMpf.createLevelFromListForBenchmark(
        "test-mpf-parked-fork-owner",
        PATH_HYDRATION_DB,
        [{ key: key1, value: value1 }],
        { engine: "overlay" },
      );
      const durableRoot = yield* owner.root();
      yield* owner.beginBlockOverlay();
      const fork = yield* owner.forkBlockOverlay();
      const candidateRoot = yield* fork.applyBatch([
        { type: "insert", key: key2, value: value2 },
      ]);
      const artifact = yield* fork.parkBlockOverlay();

      // A fork borrows its parent's Level handle. Parking it releases the
      // fork relationship but must leave lifecycle ownership with the root.
      yield* owner.discardBlockOverlay();
      yield* fork.close();
      yield* owner.close();

      const reopened = yield* MidgardMpf.create(
        "test-mpf-parked-fork-owner",
        PATH_HYDRATION_DB,
        { engine: "overlay" },
      );
      expect(yield* reopened.root()).toStrictEqual(durableRoot);
      yield* reopened.close();

      const resumed = yield* MidgardMpf.resumeParkedOverlay(
        "test-mpf-parked-fork-owner",
        PATH_HYDRATION_DB,
        artifact,
      );
      expect(yield* resumed.root()).toStrictEqual(candidateRoot);
      expect((yield* resumed.get(key2))._tag).toBe("Some");
      yield* resumed.discardBlockOverlay();
      yield* resumed.close();
    }),
  );

  it.effect("transfers Level ownership to a promoted fork exactly once", () =>
    Effect.gen(function* () {
      yield* deleteMpfStore(PATH_HYDRATION_DB, "test-mpf-path-hydration");
      const owner = yield* MidgardMpf.createLevelFromListForBenchmark(
        "test-mpf-promoted-fork-owner",
        PATH_HYDRATION_DB,
        [{ key: key1, value: value1 }],
        { engine: "overlay" },
      );
      yield* owner.beginBlockOverlay();
      const fork = yield* owner.forkBlockOverlay();
      const candidateRoot = yield* fork.applyBatch([
        { type: "insert", key: key2, value: value2 },
      ]);
      yield* fork.flushBlockOverlay(candidateRoot);

      // Promotion invalidates the parent and moves the one close obligation to
      // the child. Closing the parent must not close the child's live handle.
      yield* owner.close();
      expect(yield* fork.persistedRootHex()).toBe(
        candidateRoot.toString("hex"),
      );
      yield* fork.close();

      const reopened = yield* MidgardMpf.create(
        "test-mpf-promoted-fork-owner",
        PATH_HYDRATION_DB,
        { engine: "overlay" },
      );
      expect(yield* reopened.root()).toStrictEqual(candidateRoot);
      expect((yield* reopened.get(key2))._tag).toBe("Some");
      yield* reopened.close();
    }),
  );

  it.effect("rejects a parked overlay when the durable base advanced", () =>
    Effect.gen(function* () {
      yield* deleteMpfStore(PATH_HYDRATION_DB, "test-mpf-path-hydration");
      const parkedMpf = yield* MidgardMpf.createLevelFromListForBenchmark(
        "test-mpf-stale-parked-overlay",
        PATH_HYDRATION_DB,
        [{ key: key1, value: value1 }],
        { engine: "overlay" },
      );
      yield* parkedMpf.beginBlockOverlay();
      const parkedOps: readonly MpfBatchOp[] = [
        { type: "insert", key: key2, value: value2 },
      ];
      yield* parkedMpf.primeBlockPathArena(parkedOps, 2, false);
      yield* parkedMpf.applyBatch(parkedOps);
      const artifact = yield* parkedMpf.parkBlockOverlay();

      const advancing = yield* MidgardMpf.create(
        "test-mpf-stale-parked-overlay",
        PATH_HYDRATION_DB,
        { engine: "overlay" },
      );
      yield* advancing.beginBlockOverlay();
      const advancedRoot = yield* advancing.applyBatch([
        { type: "insert", key: key3, value: value3 },
      ]);
      yield* advancing.flushBlockOverlay(advancedRoot);
      yield* advancing.close();

      const rejected = yield* MidgardMpf.promoteParkedOverlay(
        "test-mpf-stale-parked-overlay",
        PATH_HYDRATION_DB,
        artifact,
      ).pipe(Effect.either);
      expect(rejected._tag).toBe("Left");
      const reopened = yield* MidgardMpf.create(
        "test-mpf-stale-parked-overlay",
        PATH_HYDRATION_DB,
        { engine: "overlay" },
      );
      expect(yield* reopened.persistedRootHex()).toBe(
        advancedRoot.toString("hex"),
      );
      expect((yield* reopened.get(key2))._tag).toBe("None");
      expect((yield* reopened.get(key3))._tag).toBe("Some");
      yield* reopened.close();
    }),
  );

  it.effect(
    "resumes a no-op parked scratch overlay at the canonical empty root",
    () =>
      Effect.gen(function* () {
        const scratch = yield* MidgardMpf.createScratch(
          "test-mpf-parked-empty-scratch",
          { engine: "overlay" },
        );
        yield* scratch.beginBlockOverlay();
        const artifact = yield* scratch.parkBlockOverlay();
        expect(artifact.nodeCount).toBe(0);
        expect(Buffer.from(artifact.baseRoot).toString("hex")).toBe(
          SDK.EMPTY_MERKLE_TREE_ROOT,
        );
        expect(Buffer.from(artifact.candidateRoot).toString("hex")).toBe(
          SDK.EMPTY_MERKLE_TREE_ROOT,
        );

        const resumed = yield* MidgardMpf.resumeParkedOverlay(
          "test-mpf-parked-empty-scratch",
          undefined,
          artifact,
        );
        expect(resumed.blockOverlayIsActive()).toBe(true);
        expect(yield* resumed.rootIsEmpty()).toBe(true);
        yield* resumed.discardBlockOverlay();
        yield* resumed.close();
      }),
  );

  it.effect("resumes an empty-base parked scratch overlay without a path", () =>
    Effect.gen(function* () {
      const key = Buffer.alloc(32, 0x5a);
      const scratch = yield* MidgardMpf.createScratch(
        "test-mpf-parked-scratch",
        { engine: "overlay" },
      );
      yield* scratch.beginBlockOverlay();
      yield* scratch.primeBlockPathArena(
        [{ type: "insert", key, value: value1 }],
        2,
        false,
      );
      const candidateRoot = yield* scratch.applyBatch([
        { type: "insert", key, value: value1 },
      ]);
      const artifact = yield* scratch.parkBlockOverlay();
      const resumed = yield* MidgardMpf.resumeParkedOverlay(
        "test-mpf-parked-scratch",
        undefined,
        artifact,
      );
      expect(resumed.blockOverlayIsActive()).toBe(true);
      expect(yield* resumed.root()).toStrictEqual(candidateRoot);
      expect((yield* resumed.get(key))._tag).toBe("Some");
      yield* resumed.discardBlockOverlay();
      yield* resumed.close();
    }),
  );

  it.effect(
    "authenticates a parked raw closure into packed records and verifies independent proofs",
    () =>
      Effect.gen(function* () {
        const scratch = yield* MidgardMpf.createScratch(
          "test-mpf-event-flat-packed",
          { engine: "overlay" },
        );
        yield* scratch.beginBlockOverlay();
        const candidateRoot = yield* scratch.applyBatch([
          { type: "insert", key: key1, value: value1 },
          { type: "insert", key: key2, value: value2 },
          { type: "insert", key: key3, value: value3 },
        ]);
        const artifact = yield* scratch.parkBlockOverlay();
        const arena = AuthenticatedPackedMpfArena.fromParkedArtifact(artifact);
        expect(arena.rootHash()).toStrictEqual(candidateRoot);
        expect(arena.nodeCount()).toBe(5);
        for (const [key, value] of [
          [key1, value1],
          [key2, value2],
          [key3, value3],
        ] as const) {
          expect(arena.get(key)).toStrictEqual(value);
          expect(arena.prove(key).verify(true)).toStrictEqual(candidateRoot);
        }
        const missingKey = Buffer.from("04", "hex");
        expect(arena.get(missingKey)).toBeUndefined();
        expect(arena.prove(missingKey).verify(false)).toStrictEqual(
          candidateRoot,
        );

        const corrupted = structuredClone(artifact);
        new Uint8Array(corrupted.nodeValues)[0] ^= 1;
        expect(() =>
          AuthenticatedPackedMpfArena.fromParkedArtifact(corrupted),
        ).toThrow();
      }),
  );

  it.effect(
    "applies direct copy-on-write packed event mutations with exact per-event roots",
    () =>
      Effect.gen(function* () {
        const initialEntries = [
          { key: key1, value: value1 },
          { key: key2, value: value2 },
          { key: key3, value: value3 },
        ] as const;
        const source = yield* MidgardMpf.createScratch(
          "test-mpf-event-flat-mutation-source",
          { engine: "overlay" },
        );
        yield* source.beginBlockOverlay();
        yield* source.applyBatch(
          initialEntries.map(({ key, value }) => ({
            type: "insert" as const,
            key,
            value,
          })),
        );
        const artifact = yield* source.parkBlockOverlay();
        const arena = EventFlatMutationArena.fromParkedArtifact(artifact);
        const immutableSnapshot = arena.freeze();
        const immutableRoot = immutableSnapshot.rootHash();
        const reference = yield* MidgardMpf.createScratchFromList(
          "test-mpf-event-flat-mutation-reference",
          initialEntries,
          { engine: "overlay" },
        );
        yield* reference.beginBlockOverlay();
        const key4 = Buffer.from("04", "hex");
        const key5 = Buffer.from("05", "hex");
        const key6 = Buffer.from("06", "hex");
        const key7 = Buffer.from("07", "hex");
        const events: readonly (readonly MpfBatchOp[])[] = [
          [
            { type: "delete", key: key2 },
            { type: "insert", key: key4, value: value2 },
          ],
          [{ type: "insert", key: key5, value: value1 }],
          [
            { type: "delete", key: key1 },
            { type: "insert", key: key2, value: value3 },
          ],
          [
            { type: "delete", key: key3 },
            { type: "delete", key: key4 },
            { type: "insert", key: key6, value: value1 },
            { type: "insert", key: key7, value: value2 },
          ],
        ];
        for (const event of events) {
          const expected = yield* reference.applyBatch(event);
          expect(arena.applyEvent(event)).toStrictEqual(expected);
        }
        const mutationDiagnostics = arena.diagnostics();
        expect(mutationDiagnostics.incrementalBranchUpdates).toBeGreaterThan(0);
        expect(mutationDiagnostics.incrementalBranchDigests).toBe(
          mutationDiagnostics.incrementalBranchUpdates * 5,
        );
        expect(mutationDiagnostics.generatedFullBranches).toBeGreaterThan(0);
        const frozen = arena.freeze();
        expect(frozen.rootHash()).toStrictEqual(yield* reference.root());
        for (const key of [key2, key5, key6, key7]) {
          expect(frozen.prove(key).verify(true)).toStrictEqual(
            frozen.rootHash(),
          );
        }
        expect(frozen.prove(key1).verify(false)).toStrictEqual(
          frozen.rootHash(),
        );
        expect(frozen.prove(key3).verify(false)).toStrictEqual(
          frozen.rootHash(),
        );
        expect(frozen.prove(key4).verify(false)).toStrictEqual(
          frozen.rootHash(),
        );
        expect(immutableSnapshot.rootHash()).toStrictEqual(immutableRoot);
        expect(immutableSnapshot.get(key1)).toStrictEqual(value1);
        const parkedV1 = yield* Effect.promise(() =>
          arena.freezeParallel({
            trieName: "test-mpf-event-flat-v1",
            baseRoot: Buffer.from(artifact.baseRoot),
            shardCount: 2,
          }),
        );
        const transferredV1 = structuredClone(parkedV1, {
          transfer: [
            parkedV1.baseRoot,
            parkedV1.candidateRoot,
            parkedV1.closureDigest,
            ...parkedV1.shards.flatMap((shard) => [
              shard.nodeHashes,
              shard.nodeValues,
              shard.nodeValueOffsets,
              shard.digest,
            ]),
          ],
        });
        expect(parkedV1.shards[0]!.nodeValues.byteLength).toBe(0);
        const resumedV1 = new ResumedEventFlatOverlayV1(transferredV1);
        expect(resumedV1.rootHash()).toStrictEqual(frozen.rootHash());
        expect(resumedV1.get(key2)).toStrictEqual(value3);
        expect(resumedV1.prove(key1).verify(false)).toStrictEqual(
          frozen.rootHash(),
        );
        const corruptedV1 = structuredClone(transferredV1);
        new Uint8Array(corruptedV1.shards[0]!.nodeValues)[0] ^= 1;
        expect(() => new ResumedEventFlatOverlayV1(corruptedV1)).toThrow();
        yield* reference.discardBlockOverlay();
        yield* reference.close();
      }),
  );

  it.effect(
    "matches forestry across a seeded packed split and collapse differential",
    () =>
      Effect.gen(function* () {
        const numberedKey = (index: number) => {
          const key = Buffer.alloc(32);
          key.writeUInt32BE(index, 28);
          return key;
        };
        const initialEntries = Array.from({ length: 128 }, (_, index) => ({
          key: numberedKey(index),
          value: Buffer.alloc(16, index % 251),
        }));
        const source = yield* MidgardMpf.createScratch(
          "test-mpf-event-flat-seeded-source",
          { engine: "overlay" },
        );
        yield* source.beginBlockOverlay();
        yield* source.applyBatch(
          initialEntries.map(({ key, value }) => ({
            type: "insert" as const,
            key,
            value,
          })),
        );
        const arena = EventFlatMutationArena.fromParkedArtifact(
          yield* source.parkBlockOverlay(),
        );
        const reference = yield* MidgardMpf.createScratchFromList(
          "test-mpf-event-flat-seeded-reference",
          initialEntries,
          { engine: "overlay" },
        );
        yield* reference.beginBlockOverlay();
        for (let index = 0; index < 64; index += 1) {
          const event: readonly MpfBatchOp[] = [
            { type: "delete", key: numberedKey(index) },
            {
              type: "insert",
              key: numberedKey(1_000 + index),
              value: Buffer.alloc(16, (index + 17) % 251),
            },
          ];
          expect(
            arena.applyEvent(event),
            `event=${index.toString()}`,
          ).toStrictEqual(yield* reference.applyBatch(event));
        }
        const frozen = arena.freeze();
        expect(frozen.rootHash()).toStrictEqual(yield* reference.root());
        for (const index of [0, 31, 63]) {
          expect(frozen.prove(numberedKey(index)).verify(false)).toStrictEqual(
            frozen.rootHash(),
          );
          expect(
            frozen.prove(numberedKey(1_000 + index)).verify(true),
          ).toStrictEqual(frozen.rootHash());
        }
        yield* reference.discardBlockOverlay();
        yield* reference.close();
      }),
  );

  it.effect(
    "matches Forestry when a long shared prefix collapses and splits again",
    () =>
      Effect.gen(function* () {
        const byPrefix = new Map<string, Buffer>();
        let pair: readonly [Buffer, Buffer] | undefined;
        for (let index = 0; index < 100_000 && pair === undefined; index += 1) {
          const key = Buffer.alloc(32);
          key.writeUInt32BE(index, 28);
          const prefix = mpfDigest(key).toString("hex").slice(0, 6);
          const previous = byPrefix.get(prefix);
          if (previous === undefined) byPrefix.set(prefix, key);
          else pair = [previous, key];
        }
        if (pair === undefined) {
          throw new Error("Unable to construct a long-prefix MPF fixture");
        }
        const entries = [
          { key: pair[0], value: Buffer.alloc(16, 1) },
          { key: pair[1], value: Buffer.alloc(16, 2) },
          { key: Buffer.alloc(32, 0xff), value: Buffer.alloc(16, 3) },
        ];
        const source = yield* MidgardMpf.createScratch(
          "test-mpf-event-flat-long-prefix-source",
          { engine: "overlay" },
        );
        yield* source.beginBlockOverlay();
        yield* source.applyBatch(
          entries.map(({ key, value }) => ({
            type: "insert" as const,
            key,
            value,
          })),
        );
        const arena = EventFlatMutationArena.fromParkedArtifact(
          yield* source.parkBlockOverlay(),
        );
        const reference = yield* MidgardMpf.createScratchFromList(
          "test-mpf-event-flat-long-prefix-reference",
          entries,
          { engine: "overlay" },
        );
        yield* reference.beginBlockOverlay();
        for (const event of [
          [{ type: "delete" as const, key: pair[1] }],
          [
            {
              type: "insert" as const,
              key: pair[1],
              value: Buffer.alloc(16, 4),
            },
          ],
        ]) {
          expect(arena.applyEvent(event)).toStrictEqual(
            yield* reference.applyBatch(event),
          );
        }
        yield* reference.discardBlockOverlay();
        yield* reference.close();
      }),
  );

  it.effect(
    "runs the experimental event_flat engine through Level hydration, park, and resume",
    () =>
      Effect.gen(function* () {
        yield* deleteMpfStore(PATH_HYDRATION_DB, "test-mpf-path-hydration");
        const numberedKey = (index: number) => {
          const key = Buffer.alloc(32);
          key.writeUInt32BE(index, 28);
          return key;
        };
        const initialEntries = Array.from({ length: 32 }, (_, index) => ({
          key: numberedKey(index),
          value: Buffer.alloc(16, index % 251),
        }));
        const events = Array.from(
          { length: 16 },
          (_, index) =>
            [
              { type: "delete" as const, key: numberedKey(index) },
              {
                type: "insert" as const,
                key: numberedKey(1_000 + index),
                value: Buffer.alloc(16, (index + 31) % 251),
              },
            ] as const,
        );
        const eventFlat = yield* MidgardMpf.createLevelFromListForBenchmark(
          "test-mpf-event-flat-level",
          PATH_HYDRATION_DB,
          initialEntries,
          { engine: "event_flat" },
        );
        const durableRoot = yield* eventFlat.root();
        yield* eventFlat.beginBlockOverlay();
        yield* eventFlat.primeBlockPathArena(events.flat(), 2, false);
        const reference = yield* MidgardMpf.createScratchFromList(
          "test-mpf-event-flat-level-reference",
          initialEntries,
          { engine: "overlay" },
        );
        yield* reference.beginBlockOverlay();
        let candidateRoot = durableRoot;
        for (const [index, event] of events.entries()) {
          candidateRoot = yield* reference.applyBatch(event);
          expect(
            yield* eventFlat.applyBatch(event),
            `event=${index.toString()}`,
          ).toStrictEqual(candidateRoot);
        }
        expect((yield* eventFlat.get(numberedKey(0)))._tag).toBe("None");
        expect((yield* eventFlat.get(numberedKey(1_000)))._tag).toBe("Some");
        const beforePark = eventFlat.eventFlatMutationDiagnostics();
        expect(beforePark).toBeDefined();
        const artifact = yield* eventFlat.parkEventFlatOverlayV1(2);
        expect(artifact.nodeCount).toBe(beforePark!.reachableDirtyNodeCount);
        expect(artifact.nodeCount).toBeLessThan(beforePark!.reachableNodeCount);
        expect(Buffer.from(artifact.baseRoot)).toStrictEqual(durableRoot);
        expect(Buffer.from(artifact.candidateRoot)).toStrictEqual(
          candidateRoot,
        );
        const resumed = yield* MidgardMpf.resumeParkedEventFlatOverlayV1(
          "test-mpf-event-flat-level",
          PATH_HYDRATION_DB,
          artifact,
        );
        expect(yield* resumed.root()).toStrictEqual(candidateRoot);
        expect((yield* resumed.get(numberedKey(1_000)))._tag).toBe("Some");
        expect(
          yield* resumed.verify(yield* resumed.prove(numberedKey(1_000)), true),
        ).toStrictEqual(candidateRoot);
        yield* resumed.discardBlockOverlay();
        yield* resumed.close();
        yield* reference.discardBlockOverlay();
        yield* reference.close();
      }),
  );

  it.effect(
    "builds event-flat from authenticated records instead of stale Forestry caches on 100k-sampled fanout",
    () =>
      Effect.gen(function* () {
        const byTwoNibbles = new Map<string, Buffer>();
        for (
          let index = 0;
          index < 100_000 && byTwoNibbles.size < 256;
          index += 1
        ) {
          const key = Buffer.alloc(32);
          key.writeUInt32BE(index, 28);
          const prefix = mpfDigest(key).toString("hex").slice(0, 2);
          if (!byTwoNibbles.has(prefix)) byTwoNibbles.set(prefix, key);
        }
        expect(byTwoNibbles.size).toBe(256);
        const entries = [...byTwoNibbles.values()].map((key, index) => ({
          key,
          value: Buffer.alloc(16, index % 251),
        }));
        const mpf = yield* MidgardMpf.createScratchFromList(
          "test-mpf-event-flat-cache-independence",
          entries,
          { engine: "event_flat" },
        );
        const root = (
          mpf as unknown as {
            readonly trie: Trie & {
              readonly children?: readonly unknown[];
              readonly assertHydratedNodeHashes: (depth: number) => unknown;
            };
          }
        ).trie;
        root.assertHydratedNodeHashes(64);
        const pending = [...(root.children ?? [])];
        let corruptible:
          | (Trie & {
              readonly children?: readonly unknown[];
              __midgardMerkleNodes?: Buffer[];
            })
          | undefined;
        while (pending.length > 0 && corruptible === undefined) {
          const node = pending.pop();
          if (!(node instanceof Trie)) continue;
          const inspected = node as Trie & {
            readonly children?: readonly unknown[];
            __midgardMerkleNodes?: Buffer[];
          };
          if (
            inspected.children !== undefined &&
            inspected.__midgardMerkleNodes?.[1] !== undefined
          ) {
            corruptible = inspected;
            break;
          }
          pending.push(...(inspected.children ?? []));
        }
        expect(corruptible).toBeDefined();
        corruptible!.__midgardMerkleNodes![1] = Buffer.alloc(32, 0xa5);

        // The root-only check still passes, while Forestry's recursive cached
        // check reproduces the production failure shape.
        expect(() => root.assertHydratedNodeHashes(0)).not.toThrow();
        expect(() => root.assertHydratedNodeHashes(64)).toThrow(
          /hydrated node hash mismatch/,
        );

        // Architecture E authenticates serialized records and their child
        // hashes independently, so no mutable Forestry Merkle cache is trusted.
        const arena = EventFlatMutationArena.fromHydratedTrie(root);
        expect(arena.rootHash()).toStrictEqual(yield* mpf.root());
        expect(arena.nodeCount()).toBeGreaterThanOrEqual(entries.length);
        yield* mpf.close();
      }),
  );

  it.effect(
    "preserves the Phase 4 event_flat fork park-resume-post-submit lifecycle",
    () =>
      Effect.gen(function* () {
        yield* deleteMpfStore(PATH_HYDRATION_DB, "test-mpf-path-hydration");
        const numberedKey = (index: number) => {
          const key = Buffer.alloc(32);
          key.writeUInt32BE(index, 28);
          return key;
        };
        const initialEntries = Array.from({ length: 24 }, (_, index) => ({
          key: numberedKey(index),
          value: Buffer.alloc(16, index % 251),
        }));
        const owner = yield* MidgardMpf.createLevelFromListForBenchmark(
          "test-mpf-event-flat-phase4",
          PATH_HYDRATION_DB,
          initialEntries,
          { engine: "event_flat" },
        );
        const durableRoot = yield* owner.root();
        yield* owner.beginBlockOverlay();
        const fork = yield* owner.forkBlockOverlay();
        const ops: readonly MpfBatchOp[] = [
          { type: "delete", key: numberedKey(0) },
          {
            type: "insert",
            key: numberedKey(1_000),
            value: Buffer.alloc(16, 0x5a),
          },
        ];
        yield* fork.primeBlockPathArena(ops, 2, false);
        const candidateRoot = yield* fork.applyBatch(ops);
        const artifact = yield* fork.parkEventFlatOverlayV1(2);
        yield* owner.discardBlockOverlay();
        yield* fork.close();
        yield* owner.close();

        const confirmation = yield* MidgardMpf.create(
          "test-mpf-event-flat-phase4",
          PATH_HYDRATION_DB,
          { engine: "overlay" },
        );
        expect(yield* confirmation.root()).toStrictEqual(durableRoot);
        yield* confirmation.close();

        const tampered = structuredClone(artifact);
        new Uint8Array(tampered.shards[0]!.nodeValues)[0] ^= 1;
        expect(
          (yield* MidgardMpf.resumeParkedEventFlatOverlayV1(
            "test-mpf-event-flat-phase4",
            PATH_HYDRATION_DB,
            tampered,
          ).pipe(Effect.either))._tag,
        ).toBe("Left");
        const afterTamper = yield* MidgardMpf.create(
          "test-mpf-event-flat-phase4",
          PATH_HYDRATION_DB,
          { engine: "overlay" },
        );
        expect(yield* afterTamper.root()).toStrictEqual(durableRoot);
        yield* afterTamper.close();

        const resumed = yield* MidgardMpf.resumeParkedEventFlatOverlayV1(
          "test-mpf-event-flat-phase4",
          PATH_HYDRATION_DB,
          artifact,
        );
        expect(resumed.blockOverlayIsActive()).toBe(true);
        expect(yield* resumed.root()).toStrictEqual(candidateRoot);
        expect(yield* resumed.persistedRootHex()).toBe(
          durableRoot.toString("hex"),
        );
        yield* resumed.flushBlockOverlay(candidateRoot);
        yield* resumed.close();

        const reopened = yield* MidgardMpf.create(
          "test-mpf-event-flat-phase4",
          PATH_HYDRATION_DB,
          { engine: "overlay" },
        );
        expect(yield* reopened.root()).toStrictEqual(candidateRoot);
        expect((yield* reopened.get(numberedKey(0)))._tag).toBe("None");
        expect((yield* reopened.get(numberedKey(1_000)))._tag).toBe("Some");
        yield* reopened.close();
      }),
  );

  it.effect(
    "poisons and discards an event_flat overlay after a strict mutation failure",
    () =>
      Effect.gen(function* () {
        yield* deleteMpfStore(PATH_HYDRATION_DB, "test-mpf-path-hydration");
        const eventFlat = yield* MidgardMpf.createLevelFromListForBenchmark(
          "test-mpf-event-flat-failure",
          PATH_HYDRATION_DB,
          [{ key: key1, value: value1 }],
          { engine: "event_flat" },
        );
        const durableRoot = yield* eventFlat.root();
        yield* eventFlat.beginBlockOverlay();
        const duplicate = {
          type: "insert" as const,
          key: key1,
          value: value2,
        };
        yield* eventFlat.primeBlockPathArena([duplicate], 2, false);
        expect(
          (yield* eventFlat.applyBatch([duplicate]).pipe(Effect.either))._tag,
        ).toBe("Left");
        expect(eventFlat.blockOverlayIsActive()).toBe(false);
        expect(yield* eventFlat.persistedRootHex()).toBe(
          durableRoot.toString("hex"),
        );
        yield* eventFlat.close();
        const reopened = yield* MidgardMpf.create(
          "test-mpf-event-flat-failure",
          PATH_HYDRATION_DB,
          { engine: "overlay" },
        );
        expect(yield* reopened.root()).toStrictEqual(durableRoot);
        expect((yield* reopened.get(key1))._tag).toBe("Some");
        yield* reopened.close();
      }),
  );

  it.effect(
    "excludes unreachable serialized intermediates from a parked overlay",
    () =>
      Effect.gen(function* () {
        yield* deleteMpfStore(PATH_HYDRATION_DB, "test-mpf-path-hydration");
        const mpf = yield* MidgardMpf.createLevelFromListForBenchmark(
          "test-mpf-parked-pruned",
          PATH_HYDRATION_DB,
          [{ key: key1, value: value1 }],
          { engine: "overlay" },
        );
        const baseRoot = yield* mpf.root();
        yield* mpf.beginBlockOverlay();
        const intermediateRoot = yield* mpf.applyBatch([
          { type: "insert", key: key2, value: value2 },
        ]);
        const candidateRoot = yield* mpf.applyBatch([
          { type: "delete", key: key2 },
        ]);
        expect(candidateRoot).toStrictEqual(baseRoot);
        const artifact = yield* mpf.parkBlockOverlay();
        const packedHashes = new Uint8Array(artifact.nodeHashes);
        const packedHashHex = Array.from(
          { length: artifact.nodeCount },
          (_, index) =>
            Buffer.from(
              packedHashes.subarray(index * 32, (index + 1) * 32),
            ).toString("hex"),
        );
        expect(packedHashHex).not.toContain(intermediateRoot.toString("hex"));

        const resumed = yield* MidgardMpf.resumeParkedOverlay(
          "test-mpf-parked-pruned",
          PATH_HYDRATION_DB,
          artifact,
        );
        expect(yield* resumed.root()).toStrictEqual(baseRoot);
        expect((yield* resumed.get(key2))._tag).toBe("None");
        yield* resumed.discardBlockOverlay();
        yield* resumed.close();
      }),
  );

  it.effect(
    "retains an immutable untouched live snapshot shared across root versions",
    () =>
      Effect.gen(function* () {
        yield* deleteMpfStore(PATH_HYDRATION_DB, "test-mpf-path-hydration");
        const untouchedKey = Buffer.alloc(32, 0x42);
        const untouchedNibble = mpfDigest(untouchedKey).toString("hex")[0]!;
        const mutationKeys: Buffer[] = [];
        let mutationNibble: string | undefined;
        for (let index = 0; index < 4_096; index += 1) {
          const candidate = Buffer.alloc(32);
          candidate.writeUInt32BE(index, 28);
          const nibble = mpfDigest(candidate).toString("hex")[0]!;
          if (nibble === untouchedNibble) continue;
          if (mutationNibble === undefined) mutationNibble = nibble;
          if (nibble !== mutationNibble) continue;
          mutationKeys.push(candidate);
          if (mutationKeys.length === 2) break;
        }
        const [baseMutationKey, insertedKey] = mutationKeys;
        if (baseMutationKey === undefined || insertedKey === undefined) {
          throw new Error(
            "Could not find controlled across-version mutation keys",
          );
        }

        const overlay = yield* MidgardMpf.createLevelFromListForBenchmark(
          "test-mpf-shared-version-arena",
          PATH_HYDRATION_DB,
          [
            { key: untouchedKey, value: value1 },
            { key: baseMutationKey, value: value2 },
          ],
          { engine: "overlay" },
        );
        const promotedRoot = yield* Effect.gen(function* () {
          yield* overlay.beginBlockOverlay();
          const ops: readonly MpfBatchOp[] = [
            { type: "insert", key: insertedKey, value: value3 },
          ];
          yield* overlay.primeBlockPathArena([...ops, untouchedKey], 2);

          const rootWithUntouchedChild = overlay.trie as Trie & {
            readonly children?: readonly (
              | {
                  readonly hash?: Buffer;
                  readonly serialise?: () => string | Record<string, unknown>;
                }
              | undefined
            )[];
          };
          const untouchedNode =
            rootWithUntouchedChild.children?.[
              Number.parseInt(untouchedNibble, 16)
            ];
          if (
            untouchedNode?.hash === undefined ||
            untouchedNode.serialise === undefined
          ) {
            throw new Error(
              "Controlled valid trie did not hydrate its untouched branch",
            );
          }
          const untouchedHash = Buffer.from(untouchedNode.hash);
          const arenaStore = (
            overlay as unknown as {
              readonly store: {
                readonly beginDeferredMutation: () => boolean;
                readonly putRetainedNode: (
                  key: Buffer,
                  value: typeof untouchedNode,
                ) => void;
                readonly commitDeferredMutation: () => Promise<void>;
              };
            }
          ).store;
          expect(arenaStore.beginDeferredMutation()).toBe(true);
          arenaStore.putRetainedNode(untouchedHash, untouchedNode);
          yield* Effect.promise(() => arenaStore.commitDeferredMutation());

          // The base root, raw cache, and final root all refer to the same
          // content hash. Removing its durable copy makes reopen prove that
          // promotion marked the across-version live snapshot as reachable.
          yield* Effect.promise(() =>
            (
              overlay as unknown as {
                readonly level: Level<string, string | Record<string, unknown>>;
              }
            ).level.del(untouchedHash.toString("hex")),
          );
          const root = yield* overlay.applyBatch(ops);
          yield* overlay.checkpointAndCollapseDecodedArena(2, false);
          yield* overlay.flushBlockOverlay(root);
          return root;
        }).pipe(
          Effect.ensuring(
            overlay.close().pipe(Effect.catchAll(() => Effect.void)),
          ),
        );

        const reopened = yield* MidgardMpf.create(
          "test-mpf-shared-version-arena",
          PATH_HYDRATION_DB,
          { engine: "overlay" },
        );
        yield* Effect.gen(function* () {
          expect((yield* reopened.get(untouchedKey))._tag).toBe("Some");
          expect((yield* reopened.get(insertedKey))._tag).toBe("Some");
          const [untouchedProof, insertedProof] = yield* Effect.all([
            reopened.prove(untouchedKey),
            reopened.prove(insertedKey),
          ]);
          expect(yield* reopened.verify(untouchedProof, true)).toStrictEqual(
            promotedRoot,
          );
          expect(yield* reopened.verify(insertedProof, true)).toStrictEqual(
            promotedRoot,
          );
        }).pipe(
          Effect.ensuring(
            reopened.close().pipe(Effect.catchAll(() => Effect.void)),
          ),
        );
      }),
  );

  it.effect(
    "rejects a corrupt raw path-cache node below depth eight before attachment",
    () =>
      Effect.gen(function* () {
        yield* deleteMpfStore(
          PATH_HYDRATION_FAILURE_DB,
          "test-mpf-path-hydration-failure",
        );
        const deepKey = Buffer.alloc(32, 0x41);
        const dag = buildDeepSharedMpfDag(deepKey, value1, 12);
        yield* Effect.promise(() =>
          seedSerializedMpfDag(PATH_HYDRATION_FAILURE_DB, dag),
        );
        const corruptHash = dag.chainHashes[9]!;
        yield* Effect.promise(async () => {
          const level = new Level<string, string | Record<string, unknown>>(
            PATH_HYDRATION_FAILURE_DB,
            { valueEncoding: "json" },
          );
          await level.open();
          const child = (await level.get(corruptHash)) as Record<
            string,
            unknown
          >;
          await level.put(corruptHash, { ...child, prefix: "0" });
          await level.close();
        });
        configureMpfPathHydration({
          mode: "chunked_arena",
          chunkOps: 1,
          retainDepth: 2,
        });
        for (const engine of ["overlay", "event_flat"] as const) {
          const overlay = yield* MidgardMpf.create(
            `test-mpf-corrupt-raw-cache-${engine}`,
            PATH_HYDRATION_FAILURE_DB,
            { engine },
          );
          yield* overlay.beginBlockOverlay();
          const result = yield* buildTransitionTraceResult({
            ledgerMpf: overlay,
            sourceEvents: [
              {
                phase: "L2Transaction",
                eventKey: {
                  L2TransactionEventKey: { tx_id: "64".repeat(32) },
                },
                ledgerOps: [{ type: "delete", key: deepKey }],
              },
            ],
            withdrawalCount: 0,
            forcedTransactionCount: 0,
            l2TransactionCount: 1,
            depositCount: 0,
          }).pipe(Effect.either);
          const diagnostics = yield* overlay.diagnostics();
          expect(result._tag, engine).toBe("Left");
          expect(diagnostics.levelBatchWrites, engine).toBe(0);
          expect(yield* overlay.persistedRootHex(), engine).toBe(dag.root);
          yield* overlay.close();
        }
      }).pipe(
        Effect.ensuring(
          Effect.sync(() =>
            configureMpfPathHydration({
              mode: "whole_block",
              chunkOps: 512,
              retainDepth: 2,
            }),
          ),
        ),
      ),
  );

  it.effect(
    "keeps untouched frontier opaque and rejects it when a later event touches it",
    () =>
      Effect.gen(function* () {
        yield* deleteMpfStore(
          PATH_HYDRATION_FAILURE_DB,
          "test-mpf-path-hydration-failure",
        );
        const deepKey = Buffer.alloc(32, 0x52);
        const dag = buildDeepSharedMpfDag(deepKey, value1, 12);
        yield* Effect.promise(() =>
          seedSerializedMpfDag(PATH_HYDRATION_FAILURE_DB, dag),
        );
        yield* Effect.promise(async () => {
          const level = new Level<string, string | Record<string, unknown>>(
            PATH_HYDRATION_FAILURE_DB,
            { valueEncoding: "json" },
          );
          await level.open();
          await level.del(dag.sharedHash);
          await level.close();
        });
        const eventFlat = yield* MidgardMpf.create(
          "test-mpf-missing-event-flat-frontier",
          PATH_HYDRATION_FAILURE_DB,
          { engine: "event_flat" },
        );
        yield* eventFlat.beginBlockOverlay();
        const opaque = yield* eventFlat
          .primeBlockPathArena(
            [{ type: "insert", key: deepKey, value: value2 }],
            2,
            false,
          )
          .pipe(Effect.either);
        expect(opaque._tag).toBe("Right");
        yield* eventFlat.discardBlockOverlay();

        const missingNibble = (
          (Number.parseInt(dag.path[0]!, 16) + 1) %
          16
        ).toString(16);
        let missingKey: Buffer | undefined;
        for (
          let index = 0;
          index < 10_000 && missingKey === undefined;
          index++
        ) {
          const candidate = Buffer.alloc(32);
          candidate.writeUInt32BE(index, 28);
          if (mpfDigest(candidate).toString("hex")[0] === missingNibble) {
            missingKey = candidate;
          }
        }
        if (missingKey === undefined) {
          throw new Error("Unable to construct a missing-frontier key");
        }
        yield* eventFlat.beginBlockOverlay();
        const touched = yield* eventFlat
          .primeBlockPathArena(
            [{ type: "insert", key: missingKey, value: value2 }],
            2,
            false,
          )
          .pipe(Effect.either);
        expect(touched._tag).toBe("Left");
        expect(yield* eventFlat.persistedRootHex()).toBe(dag.root);
        expect((yield* eventFlat.root().pipe(Effect.either))._tag).toBe("Left");
        yield* eventFlat.close();
      }),
  );

  it.effect(
    "poisons a transient current-root arena whose mutable node no longer matches its hash",
    () =>
      Effect.gen(function* () {
        yield* deleteMpfStore(PATH_HYDRATION_DB, "test-mpf-path-hydration");
        const overlay = yield* MidgardMpf.createLevelFromListForBenchmark(
          "test-mpf-corrupt-live-arena",
          PATH_HYDRATION_DB,
          [
            { key: key1, value: value1 },
            { key: key2, value: value2 },
            { key: key3, value: value3 },
          ],
          { engine: "overlay" },
        );
        const durableRoot = yield* overlay.persistedRootHex();
        yield* overlay.beginBlockOverlay();
        const ops: readonly MpfBatchOp[] = [
          { type: "delete", key: key1 },
          { type: "insert", key: Buffer.alloc(32, 9), value: value1 },
        ];
        yield* overlay.primeBlockPathArena(ops, 2, false);
        yield* overlay.applyBatch(ops);
        const beforeCorruption = yield* overlay.diagnostics();
        expect(beforeCorruption.transientLiveNodes).toBeGreaterThan(0);
        expect(beforeCorruption.transientDirtyNodes).toBeGreaterThan(0);
        const mutableTrie = overlay.trie as Trie & { prefix: string };
        mutableTrie.prefix = `${mutableTrie.prefix}0`;
        const checkpoint = yield* overlay
          .checkpointAndCollapseDecodedArena(2, false, false)
          .pipe(Effect.either);
        const diagnostics = yield* overlay.diagnostics();
        expect(checkpoint._tag).toBe("Left");
        expect(diagnostics.levelBatchWrites).toBe(0);
        expect(yield* overlay.persistedRootHex()).toBe(durableRoot);
        expect((yield* overlay.root().pipe(Effect.either))._tag).toBe("Left");
        yield* overlay.close();
      }),
  );

  it.effect(
    "poisons transient descendant corruption before fork or promotion can capture it",
    () =>
      Effect.gen(function* () {
        for (const boundary of ["fork", "promotion"] as const) {
          yield* deleteMpfStore(PATH_HYDRATION_DB, "test-mpf-path-hydration");
          const insertedKey = Buffer.alloc(32, 9);
          const overlay = yield* MidgardMpf.createLevelFromListForBenchmark(
            `test-mpf-corrupt-transient-descendant-${boundary}`,
            PATH_HYDRATION_DB,
            [
              { key: key1, value: value1 },
              { key: key2, value: value2 },
              { key: key3, value: value3 },
            ],
            { engine: "overlay" },
          );
          const durableRoot = yield* overlay.persistedRootHex();
          yield* overlay.beginBlockOverlay();
          const ops: readonly MpfBatchOp[] = [
            { type: "delete", key: key1 },
            { type: "insert", key: insertedKey, value: value1 },
          ];
          yield* overlay.primeBlockPathArena(ops, 2, false);
          const candidateRoot = yield* overlay.applyBatch(ops);
          yield* overlay.checkpointAndCollapseDecodedArena(2, false, false);

          const root = overlay.trie as Trie & {
            readonly children?: readonly (Trie | undefined)[];
          };
          const path = mpfDigest(insertedKey).toString("hex");
          const childIndex = Number.parseInt(path[root.prefix.length]!, 16);
          const descendant = root.children?.[childIndex] as
            | (Trie & { prefix: string })
            | undefined;
          if (descendant === undefined || !(descendant instanceof Trie)) {
            throw new Error("Transient-corruption fixture has no live child");
          }
          descendant.prefix = `${descendant.prefix}0`;

          const captured = yield* (
            boundary === "fork"
              ? overlay.forkBlockOverlay()
              : overlay.flushBlockOverlay(candidateRoot)
          ).pipe(Effect.either);
          expect(captured._tag, boundary).toBe("Left");
          expect(yield* overlay.persistedRootHex(), boundary).toBe(durableRoot);
          expect(
            (yield* overlay.root().pipe(Effect.either))._tag,
            boundary,
          ).toBe("Left");
          yield* overlay.close();
        }
      }),
  );

  it.effect(
    "reauthenticates each distinct retained snapshot before replacing a content key",
    () =>
      Effect.gen(function* () {
        yield* deleteMpfStore(PATH_HYDRATION_DB, "test-mpf-path-hydration");
        const overlay = yield* MidgardMpf.createLevelFromListForBenchmark(
          "test-mpf-retained-snapshot-authentication",
          PATH_HYDRATION_DB,
          [
            { key: key1, value: value1 },
            { key: key2, value: value2 },
          ],
          { engine: "overlay" },
        );
        const durableRoot = yield* overlay.persistedRootHex();
        yield* overlay.beginBlockOverlay();
        yield* overlay.primeBlockPathArena([key1], 2);

        const goodSnapshot = overlay.trie.cloneDetached() as Trie & {
          readonly hash?: Buffer;
          prefix: string;
        };
        if (goodSnapshot.hash === undefined) {
          throw new Error("Retained-snapshot fixture has no content hash");
        }
        const retainedHash = Buffer.from(goodSnapshot.hash);
        const arenaStore = (
          overlay as unknown as {
            readonly store: {
              readonly beginDeferredMutation: () => boolean;
              readonly putRetainedNode: (key: Buffer, value: Trie) => void;
              readonly commitDeferredMutation: () => Promise<void>;
              readonly abortDeferredMutation: () => void;
            };
          }
        ).store;

        expect(arenaStore.beginDeferredMutation()).toBe(true);
        arenaStore.putRetainedNode(retainedHash, goodSnapshot);
        yield* Effect.promise(() => arenaStore.commitDeferredMutation());

        const corruptSnapshot = goodSnapshot.cloneDetached() as Trie & {
          prefix: string;
        };
        corruptSnapshot.prefix = `${corruptSnapshot.prefix}0`;
        Object.defineProperty(corruptSnapshot, "consumeMidgardMutationProof", {
          value: () => true,
        });
        expect(arenaStore.beginDeferredMutation()).toBe(true);
        expect(() =>
          arenaStore.putRetainedNode(retainedHash, corruptSnapshot),
        ).toThrow(/hydrated node hash mismatch/);
        arenaStore.abortDeferredMutation();

        const corruptChildSnapshot = goodSnapshot.cloneDetached() as Trie & {
          children?: Array<{ hash: Buffer } | undefined>;
        };
        const corruptChildIndex = corruptChildSnapshot.children?.findIndex(
          (child) => child !== undefined,
        );
        if (
          corruptChildSnapshot.children === undefined ||
          corruptChildIndex === undefined ||
          corruptChildIndex < 0
        ) {
          throw new Error("Retained-snapshot fixture has no branch child");
        }
        corruptChildSnapshot.children[corruptChildIndex] = {
          hash: Buffer.alloc(32, 0xff),
        };
        expect(arenaStore.beginDeferredMutation()).toBe(true);
        expect(() =>
          arenaStore.putRetainedNode(retainedHash, corruptChildSnapshot),
        ).toThrow(/hydrated node merkle cache mismatch/);
        arenaStore.abortDeferredMutation();

        const diagnostics = yield* overlay.diagnostics();
        expect(diagnostics.levelBatchWrites).toBe(0);
        expect(yield* overlay.persistedRootHex()).toBe(durableRoot);
        yield* overlay.discardBlockOverlay();
        yield* overlay.close();
      }),
  );

  it.effect(
    "fails missing base keys after prefetch without advancing durability",
    () =>
      Effect.gen(function* () {
        const overlay = yield* MidgardMpf.createLevelFromListForBenchmark(
          "test-mpf-path-hydration",
          PATH_HYDRATION_DB,
          [{ key: key1, value: value1 }],
          { engine: "overlay" },
        );
        const durableBaseRoot = yield* overlay.persistedRootHex();
        yield* overlay.beginBlockOverlay();
        const result = yield* buildTransitionTraceResult({
          ledgerMpf: overlay,
          sourceEvents: [
            {
              phase: "L2Transaction",
              eventKey: {
                L2TransactionEventKey: {
                  tx_id: Buffer.alloc(32, 4).toString("hex"),
                },
              } as SDK.EventKey,
              ledgerOps: [{ type: "delete", key: key2 }],
            },
          ],
          withdrawalCount: 0,
          forcedTransactionCount: 0,
          l2TransactionCount: 1,
          depositCount: 0,
        }).pipe(Effect.either);

        expect(result._tag).toBe("Left");
        expect(yield* overlay.persistedRootHex()).toBe(durableBaseRoot);
        expect((yield* overlay.root().pipe(Effect.either))._tag).toBe("Left");
        yield* overlay.resetToRoot(Buffer.from(durableBaseRoot, "hex"));
        yield* overlay.close();
      }),
  );

  it.effect(
    "rolls back cleanly when touched-path hydration cannot load a base node",
    () =>
      Effect.gen(function* () {
        const entries = [
          { key: key1, value: value1 },
          { key: key2, value: value2 },
          { key: key3, value: value3 },
        ];
        const seeded = yield* MidgardMpf.createLevelFromListForBenchmark(
          "test-mpf-path-hydration-failure",
          PATH_HYDRATION_FAILURE_DB,
          entries,
          { engine: "overlay" },
        );
        const durableBaseRoot = yield* seeded.persistedRootHex();
        yield* seeded.close();
        yield* Effect.promise(async () => {
          const level = new Level<string, string | Record<string, unknown>>(
            PATH_HYDRATION_FAILURE_DB,
            { valueEncoding: "json" },
          );
          await level.open();
          const rootNode = (await level.get(durableBaseRoot)) as {
            readonly children?: readonly (string | undefined)[];
          };
          const childHashes = rootNode.children?.filter(
            (child): child is string => typeof child === "string",
          );
          if (childHashes === undefined || childHashes.length === 0) {
            throw new Error("Hydration-failure fixture root is not a branch");
          }
          await level.batch(
            childHashes.map((child) => ({ type: "del" as const, key: child })),
          );
          await level.close();
        });
        const overlay = yield* MidgardMpf.create(
          "test-mpf-path-hydration-failure",
          PATH_HYDRATION_FAILURE_DB,
          { engine: "overlay" },
        );
        yield* overlay.beginBlockOverlay();
        const result = yield* overlay
          .prefetchTouchedPaths([key1])
          .pipe(Effect.either);

        expect(result._tag).toBe("Left");
        expect(yield* overlay.rootHex()).toBe(durableBaseRoot);
        expect(yield* overlay.persistedRootHex()).toBe(durableBaseRoot);
        const diagnostics = yield* overlay.diagnostics();
        expect(diagnostics.levelGetManyCalls).toBeGreaterThan(0);
        expect(diagnostics.levelBatchWrites).toBe(0);
        yield* overlay.discardBlockOverlay();
        yield* overlay.close();
      }),
  );

  it.effect(
    "poisons a chunked overlay when the retained upper arena is unauthenticated",
    () =>
      Effect.gen(function* () {
        yield* deleteMpfStore(
          PATH_HYDRATION_FAILURE_DB,
          "test-mpf-path-hydration-failure",
        );
        const seeded = yield* MidgardMpf.createLevelFromListForBenchmark(
          "test-mpf-path-hydration-failure",
          PATH_HYDRATION_FAILURE_DB,
          [
            { key: key1, value: value1 },
            { key: key2, value: value2 },
            { key: key3, value: value3 },
          ],
          { engine: "overlay" },
        );
        const durableRoot = yield* seeded.persistedRootHex();
        yield* seeded.close();
        yield* Effect.promise(async () => {
          const level = new Level<string, string | Record<string, unknown>>(
            PATH_HYDRATION_FAILURE_DB,
            { valueEncoding: "json" },
          );
          await level.open();
          const root = (await level.get(durableRoot)) as Record<
            string,
            unknown
          >;
          const children = root.children;
          if (!Array.isArray(children)) {
            throw new Error("Authentication fixture root is not a branch");
          }
          const populated = children.flatMap((child, index) =>
            typeof child === "string" ? [{ child, index }] : [],
          );
          if (populated.length < 2) {
            throw new Error("Authentication fixture has fewer than two roots");
          }
          const corruptedChildren = [...children];
          corruptedChildren[populated[0]!.index] = populated[1]!.child;
          await level.put(durableRoot, {
            ...root,
            children: corruptedChildren,
          });
          await level.close();
        });
        configureMpfPathHydration({
          mode: "chunked",
          chunkOps: 1,
          retainDepth: 2,
        });
        const overlay = yield* MidgardMpf.create(
          "test-mpf-path-hydration-failure",
          PATH_HYDRATION_FAILURE_DB,
          { engine: "overlay" },
        );
        yield* overlay.beginBlockOverlay();
        const result = yield* buildTransitionTraceResult({
          ledgerMpf: overlay,
          sourceEvents: [
            {
              phase: "L2Transaction",
              eventKey: {
                L2TransactionEventKey: { tx_id: "55".repeat(32) },
              },
              ledgerOps: [],
            },
          ],
          withdrawalCount: 0,
          forcedTransactionCount: 0,
          l2TransactionCount: 1,
          depositCount: 0,
        }).pipe(Effect.either);

        expect(result._tag).toBe("Left");
        expect((yield* overlay.root().pipe(Effect.either))._tag).toBe("Left");
        expect(yield* overlay.persistedRootHex()).toBe(durableRoot);
        expect((yield* overlay.diagnostics()).levelBatchWrites).toBe(0);
        yield* overlay.close();
      }).pipe(
        Effect.ensuring(
          Effect.sync(() =>
            configureMpfPathHydration({
              mode: "whole_block",
              chunkOps: 512,
              retainDepth: 2,
            }),
          ),
        ),
      ),
  );

  it.effect(
    "does not advance the durable marker before overlay promotion",
    () =>
      Effect.gen(function* () {
        const mpf = yield* MidgardMpf.create("test-mpf-overlay", OVERLAY_DB, {
          engine: "overlay",
        });
        yield* mpf.beginBlockOverlay();
        yield* mpf.applyBatch([{ type: "insert", key: key1, value: value1 }]);
        yield* mpf.spillIfNeeded();
        const candidateRoot = yield* mpf.rootHex();

        const durableBefore = yield* mpf.persistedRootHex();
        expect(durableBefore).toBe(SDK.EMPTY_MERKLE_TREE_ROOT);

        yield* mpf.flushBlockOverlay(Buffer.from(candidateRoot, "hex"));
        yield* mpf.close();
        const reopened = yield* MidgardMpf.create(
          "test-mpf-overlay",
          OVERLAY_DB,
        );
        expect(yield* reopened.rootHex()).toBe(candidateRoot);
        expect((yield* reopened.get(key1))._tag).toBe("Some");
        yield* reopened.close();
      }),
  );

  it.effect(
    "keeps reset-to-empty inside the active overlay until promotion",
    () =>
      Effect.gen(function* () {
        const mpf = yield* MidgardMpf.create(
          "test-mpf-overlay-reset",
          OVERLAY_RESET_DB,
          { engine: "overlay" },
        );
        yield* mpf.applyBatch([{ type: "insert", key: key1, value: value1 }]);
        const durableBaseRoot = yield* mpf.persistedRootHex();

        yield* mpf.beginBlockOverlay();
        yield* mpf.resetToEmpty();
        expect(mpf.blockOverlayIsActive()).toBe(true);
        expect(yield* mpf.persistedRootHex()).toBe(durableBaseRoot);
        const candidateRoot = yield* mpf.applyBatch([
          { type: "insert", key: key2, value: value2 },
        ]);
        expect(yield* mpf.persistedRootHex()).toBe(durableBaseRoot);

        yield* mpf.discardBlockOverlay();
        expect(yield* mpf.rootHex()).toBe(durableBaseRoot);
        expect((yield* mpf.get(key1))._tag).toBe("Some");
        expect((yield* mpf.get(key2))._tag).toBe("None");

        yield* mpf.beginBlockOverlay();
        yield* mpf.resetToEmpty();
        const promotedRoot = yield* mpf.applyBatch([
          { type: "insert", key: key2, value: value2 },
        ]);
        expect(promotedRoot).toEqual(candidateRoot);
        yield* mpf.flushBlockOverlay(promotedRoot);
        expect(yield* mpf.persistedRootHex()).toBe(
          promotedRoot.toString("hex"),
        );
        expect((yield* mpf.get(key1))._tag).toBe("None");
        expect((yield* mpf.get(key2))._tag).toBe("Some");
        yield* mpf.close();
      }),
  );

  it.effect("leaves the old marker after an early spill and discard", () =>
    Effect.gen(function* () {
      const mpf = yield* MidgardMpf.create(
        "test-mpf-overlay-spill",
        OVERLAY_SPILL_DB,
        { engine: "overlay", spillThresholdBytes: 1 },
      );
      yield* mpf.beginBlockOverlay();
      yield* mpf.applyBatch([{ type: "insert", key: key1, value: value1 }]);
      yield* mpf.spillIfNeeded();
      const diagnostics = yield* mpf.diagnostics();
      expect(
        diagnostics.overlaySpills,
        JSON.stringify(diagnostics),
      ).toBeGreaterThan(0);
      yield* mpf.discardBlockOverlay();
      expect(yield* mpf.rootHex()).toBe(SDK.EMPTY_MERKLE_TREE_ROOT);
      yield* mpf.close();

      const reopened = yield* MidgardMpf.create(
        "test-mpf-overlay-spill",
        OVERLAY_SPILL_DB,
      );
      expect(yield* reopened.rootHex()).toBe(SDK.EMPTY_MERKLE_TREE_ROOT);
      expect((yield* reopened.get(key1))._tag).toBe("None");
      yield* reopened.close();
    }),
  );

  it.effect(
    "spills large deferred leaves before their configured byte threshold",
    () =>
      Effect.gen(function* () {
        const mpf = yield* MidgardMpf.create(
          "test-mpf-overlay-spill",
          OVERLAY_SPILL_DB,
          { engine: "overlay", spillThresholdBytes: 4_096 },
        );
        yield* mpf.beginBlockOverlay();
        yield* mpf.applyBatch([
          { type: "insert", key: key1, value: Buffer.alloc(8_192, 0xaa) },
        ]);
        const diagnostics = yield* mpf.diagnostics();

        expect(diagnostics.overlaySpills).toBeGreaterThan(0);
        expect(
          diagnostics.deferredMaterializedEstimatedBytes,
        ).toBeGreaterThanOrEqual(diagnostics.deferredMaterializedActualBytes);
        expect(diagnostics.deferredMaterializedActualBytes).toBeGreaterThan(
          4_096,
        );
        yield* mpf.discardBlockOverlay();
        yield* mpf.close();
      }),
  );

  it.effect(
    "poisons and discards the block overlay once when a later event mutation fails",
    () =>
      Effect.gen(function* () {
        const mpf = yield* MidgardMpf.create(
          "test-mpf-overlay-failure",
          OVERLAY_FAILURE_DB,
          { engine: "overlay" },
        );
        const result = yield* withMpfBlockOverlays(
          [mpf],
          Effect.gen(function* () {
            yield* mpf.applyBatch([
              { type: "insert", key: key1, value: value1 },
            ]);
            yield* mpf.applyBatch([
              { type: "insert", key: key2, value: value2 },
              { type: "insert", key: key1, value: value3 },
            ]);
          }),
          () => false,
        ).pipe(Effect.either);

        expect(result._tag).toBe("Left");
        if (result._tag === "Left") {
          expect(result.left.message).toContain("inserting a new entry");
        }
        expect(mpf.blockOverlayIsActive()).toBe(false);
        expect((yield* mpf.root().pipe(Effect.either))._tag).toBe("Left");
        expect(yield* mpf.persistedRootHex()).toBe(SDK.EMPTY_MERKLE_TREE_ROOT);

        yield* mpf.resetToRoot(Buffer.from(SDK.EMPTY_MERKLE_TREE_ROOT, "hex"));
        expect((yield* mpf.get(key1))._tag).toBe("None");
        yield* mpf.beginBlockOverlay();
        yield* mpf.applyBatch([{ type: "insert", key: key3, value: value3 }]);
        yield* mpf.discardBlockOverlay();
        yield* mpf.close();
      }),
  );

  it.effect(
    "keeps strict overlay mutation semantics without duplicate presence walks",
    () =>
      Effect.gen(function* () {
        const missing = yield* MidgardMpf.createScratch("missing-overlay", {
          engine: "overlay",
        });
        yield* missing.beginBlockOverlay();
        const missingResult = yield* applyTraceLedgerOpsToMpf(
          missing,
          [{ type: "delete", key: key1 }],
          "missing-event",
        ).pipe(Effect.either);
        expect(missingResult._tag).toBe("Left");
        expect(missing.blockOverlayIsActive()).toBe(false);
        yield* missing.close();

        const duplicate = yield* MidgardMpf.createScratchFromList(
          "duplicate-overlay",
          [{ key: key1, value: value1 }],
          { engine: "overlay" },
        );
        yield* duplicate.beginBlockOverlay();
        const duplicateResult = yield* applyTraceLedgerOpsToMpf(
          duplicate,
          [{ type: "insert", key: key1, value: value2 }],
          "duplicate-event",
        ).pipe(Effect.either);
        expect(duplicateResult._tag).toBe("Left");
        expect(duplicate.blockOverlayIsActive()).toBe(false);
        yield* duplicate.close();

        const sequenced = yield* MidgardMpf.createScratch("sequenced-overlay", {
          engine: "overlay",
        });
        yield* sequenced.beginBlockOverlay();
        yield* applyTraceLedgerOpsToMpf(
          sequenced,
          [
            { type: "insert", key: key1, value: value1 },
            { type: "delete", key: key1 },
            { type: "insert", key: key1, value: value2 },
          ],
          "sequenced-event",
        );
        const finalValue = yield* sequenced.get(key1);
        expect(finalValue._tag).toBe("Some");
        if (finalValue._tag === "Some") {
          expect(finalValue.value).toEqual(value2);
        }
        yield* sequenced.discardBlockOverlay();
        yield* sequenced.close();
      }),
  );

  it.effect("forks ledger overlays without mutating the parent root", () =>
    Effect.gen(function* () {
      const mpf = yield* MidgardMpf.create(
        "test-mpf-overlay-fork",
        OVERLAY_FORK_DB,
        { engine: "overlay" },
      );
      const parent = yield* mpf.ledgerOverlayHandle();
      yield* parent.applyBlockDelta([
        { type: "insert", key: key1, value: value1 },
        { type: "insert", key: key2, value: value2 },
      ]);
      const parentRoot = yield* parent.rootHex();
      const serialiseCallsBeforeFork = (yield* mpf.diagnostics())
        .serialiseCalls;
      const child = yield* parent.fork();
      yield* child.applyBlockDelta([
        { type: "insert", key: key3, value: value3 },
      ]);
      const childRoot = yield* child.rootHex();
      const parentMutationWhileForked = yield* parent
        .applyBlockDelta([
          {
            type: "insert",
            key: Buffer.from("04", "hex"),
            value: value3,
          },
        ])
        .pipe(Effect.either);
      const parentPromoteWhileForked = yield* parent
        .promote()
        .pipe(Effect.either);

      expect(childRoot).not.toBe(parentRoot);
      expect(yield* parent.rootHex()).toBe(parentRoot);
      expect((yield* mpf.get(key1))._tag).toBe("Some");
      expect((yield* mpf.get(key2))._tag).toBe("Some");
      expect((yield* mpf.get(key3))._tag).toBe("None");
      expect(parentMutationWhileForked._tag).toBe("Left");
      expect(parentPromoteWhileForked._tag).toBe("Left");
      const parentDiagnostics = yield* mpf.diagnostics();
      expect(parentDiagnostics.deferredLazyReads).toBeGreaterThan(0);
      expect(parentDiagnostics.serialiseCalls).toBeGreaterThan(
        serialiseCallsBeforeFork,
      );
      expect(parentDiagnostics.deferredLazyReads).toBeLessThan(
        parentDiagnostics.overlayEntries,
      );
      expect(parentDiagnostics.arenaCheckpointCalls).toBe(0);
      yield* child.discard();
      expect(yield* parent.rootHex()).toBe(parentRoot);
      yield* parent.discard();
      yield* mpf.close();
    }),
  );

  it.effect(
    "transfers durable-root ownership to a promoted child and invalidates its parent",
    () =>
      Effect.gen(function* () {
        const parent = yield* MidgardMpf.create(
          "test-mpf-overlay-fork",
          OVERLAY_FORK_DB,
          { engine: "overlay" },
        );
        const parentHandle = yield* parent.ledgerOverlayHandle();
        yield* parentHandle.applyBlockDelta([
          { type: "insert", key: key1, value: value1 },
        ]);
        const child = yield* parent.forkBlockOverlay();
        const childHandle = yield* child.ledgerOverlayHandle();
        yield* childHandle.applyBlockDelta([
          { type: "insert", key: key2, value: value2 },
        ]);
        const childRoot = yield* childHandle.rootHex();
        yield* childHandle.promote();

        expect((yield* parentHandle.rootHex().pipe(Effect.either))._tag).toBe(
          "Left",
        );
        expect((yield* parentHandle.promote().pipe(Effect.either))._tag).toBe(
          "Left",
        );
        yield* parent.close();
        yield* child.close();

        const reopened = yield* MidgardMpf.create(
          "test-mpf-overlay-fork",
          OVERLAY_FORK_DB,
        );
        expect(yield* reopened.rootHex()).toBe(childRoot);
        expect((yield* reopened.get(key1))._tag).toBe("Some");
        expect((yield* reopened.get(key2))._tag).toBe("Some");
        yield* reopened.close();
      }),
  );

  it.effect(
    "recursively transfers nested-fork promotion ownership through every ancestor",
    () =>
      Effect.gen(function* () {
        yield* deleteMpfStore(OVERLAY_FORK_DB, "test-mpf-overlay-fork");
        const grandparent = yield* MidgardMpf.create(
          "test-mpf-nested-fork",
          OVERLAY_FORK_DB,
          { engine: "overlay" },
        );
        yield* grandparent.beginBlockOverlay();
        yield* grandparent.applyBatch([
          { type: "insert", key: key1, value: value1 },
        ]);
        const parent = yield* grandparent.forkBlockOverlay();
        yield* parent.applyBatch([
          { type: "insert", key: key2, value: value2 },
        ]);
        const child = yield* parent.forkBlockOverlay();
        const promotedRoot = yield* child.applyBatch([
          { type: "insert", key: key3, value: value3 },
        ]);
        yield* child.flushBlockOverlay(promotedRoot);

        expect((yield* grandparent.root().pipe(Effect.either))._tag).toBe(
          "Left",
        );
        expect((yield* parent.root().pipe(Effect.either))._tag).toBe("Left");
        yield* grandparent.close();
        yield* parent.close();
        yield* child.close();

        const reopened = yield* MidgardMpf.create(
          "test-mpf-nested-fork",
          OVERLAY_FORK_DB,
          { engine: "overlay" },
        );
        expect(yield* reopened.root()).toStrictEqual(promotedRoot);
        expect((yield* reopened.get(key1))._tag).toBe("Some");
        expect((yield* reopened.get(key2))._tag).toBe("Some");
        expect((yield* reopened.get(key3))._tag).toBe("Some");
        yield* reopened.close();
      }),
  );

  it.effect(
    "refuses sibling promotion before durability and prevents stale overwrite",
    () =>
      Effect.gen(function* () {
        yield* deleteMpfStore(OVERLAY_FORK_DB, "test-mpf-overlay-fork");
        const parent = yield* MidgardMpf.create(
          "test-mpf-sibling-fork",
          OVERLAY_FORK_DB,
          { engine: "overlay" },
        );
        yield* parent.beginBlockOverlay();
        yield* parent.applyBatch([
          { type: "insert", key: key1, value: value1 },
        ]);
        const left = yield* parent.forkBlockOverlay();
        const right = yield* parent.forkBlockOverlay();
        const leftRoot = yield* left.applyBatch([
          { type: "insert", key: key2, value: value2 },
        ]);
        const rightRoot = yield* right.applyBatch([
          { type: "insert", key: key3, value: value3 },
        ]);
        const durableBefore = yield* left.persistedRootHex();
        const leftBlocked = yield* left
          .flushBlockOverlay(leftRoot)
          .pipe(Effect.either);
        const rightBlocked = yield* right
          .flushBlockOverlay(rightRoot)
          .pipe(Effect.either);
        expect(leftBlocked._tag).toBe("Left");
        expect(rightBlocked._tag).toBe("Left");
        expect(yield* left.persistedRootHex()).toBe(durableBefore);

        yield* right.discardBlockOverlay();
        yield* left.flushBlockOverlay(leftRoot);
        const staleOverwrite = yield* right
          .flushBlockOverlay(rightRoot)
          .pipe(Effect.either);
        expect(staleOverwrite._tag).toBe("Left");
        yield* parent.close();
        yield* left.close();
        yield* right.close();

        const reopened = yield* MidgardMpf.create(
          "test-mpf-sibling-fork",
          OVERLAY_FORK_DB,
          { engine: "overlay" },
        );
        expect(yield* reopened.root()).toStrictEqual(leftRoot);
        expect((yield* reopened.get(key1))._tag).toBe("Some");
        expect((yield* reopened.get(key2))._tag).toBe("Some");
        expect((yield* reopened.get(key3))._tag).toBe("None");
        yield* reopened.close();
      }),
  );

  it.effect(
    "keeps confirmed transaction recovery durable when a speculative ledger child is invalidated",
    () =>
      Effect.gen(function* () {
        const ledgerParent = yield* MidgardMpf.create(
          "test-mpf-speculative-ledger-recovery",
          SPECULATIVE_LEDGER_RECOVERY_DB,
          { engine: "overlay" },
        );
        const transactionsParent = yield* MidgardMpf.create(
          "test-mpf-speculative-tx-recovery",
          SPECULATIVE_TX_RECOVERY_DB,
          { engine: "overlay" },
        );
        yield* ledgerParent.beginBlockOverlay();
        yield* ledgerParent.applyBatch([
          { type: "insert", key: key1, value: value1 },
        ]);
        const baseLedgerRoot = yield* ledgerParent.root();
        yield* ledgerParent.flushBlockOverlay(baseLedgerRoot);
        yield* transactionsParent.beginBlockOverlay();
        yield* transactionsParent.applyBatch([
          { type: "insert", key: key1, value: value1 },
        ]);
        const submittedTransactionsRoot = yield* transactionsParent.root();
        yield* transactionsParent.flushBlockOverlay(submittedTransactionsRoot);

        yield* ledgerParent.beginBlockOverlay();
        const ledgerCandidate = yield* ledgerParent.forkBlockOverlay();
        const transactionsCandidate = yield* MidgardMpf.createScratch(
          "test-mpf-speculative-transactions-candidate",
          { engine: "overlay" },
        );
        yield* transactionsCandidate.beginBlockOverlay();
        yield* ledgerCandidate.applyBatch([
          { type: "insert", key: key2, value: value2 },
        ]);
        yield* transactionsCandidate.applyBatch([
          { type: "insert", key: key2, value: value2 },
        ]);

        // Confirmation of N finalizes its per-block transaction tree. This is
        // a separate durable transition from speculative candidate N+1.
        yield* transactionsParent.resetToEmpty();

        // Model T3/T4 after recovery: N+1 is discarded, while confirmed-N
        // recovery must remain durable.
        yield* ledgerCandidate.discardBlockOverlay();
        yield* ledgerParent.discardBlockOverlay();
        yield* transactionsCandidate.discardBlockOverlay();
        yield* ledgerParent.close();
        yield* transactionsParent.close();
        yield* transactionsCandidate.close();

        const reopenedLedger = yield* MidgardMpf.create(
          "test-mpf-speculative-ledger-recovery",
          SPECULATIVE_LEDGER_RECOVERY_DB,
        );
        const reopenedTransactions = yield* MidgardMpf.create(
          "test-mpf-speculative-tx-recovery",
          SPECULATIVE_TX_RECOVERY_DB,
        );
        expect(yield* reopenedLedger.rootHex()).toBe(
          baseLedgerRoot.toString("hex"),
        );
        expect((yield* reopenedLedger.get(key1))._tag).toBe("Some");
        expect((yield* reopenedLedger.get(key2))._tag).toBe("None");
        expect(yield* reopenedTransactions.rootIsEmpty()).toBe(true);
        yield* reopenedLedger.close();
        yield* reopenedTransactions.close();
      }),
  );

  it.effect("promotes in-memory overlays and rejects a mismatched marker", () =>
    Effect.gen(function* () {
      const overlayMpf = yield* MidgardMpf.create(
        "memory-overlay-enabled",
        undefined,
        { engine: "overlay" },
      );
      yield* overlayMpf.beginBlockOverlay();
      yield* overlayMpf.applyBatch([
        { type: "insert", key: key1, value: value1 },
      ]);
      const actualRoot = yield* overlayMpf.root();
      const mismatched = yield* overlayMpf
        .flushBlockOverlay(Buffer.alloc(32, 7))
        .pipe(Effect.either);
      expect(mismatched._tag).toBe("Left");
      yield* overlayMpf.flushBlockOverlay(actualRoot);
      expect((yield* overlayMpf.get(key1))._tag).toBe("Some");
      const proof = yield* overlayMpf.prove(key1);
      expect((yield* overlayMpf.verify(proof, true)).toString("hex")).toBe(
        actualRoot.toString("hex"),
      );
    }),
  );

  it.effect("normalizes an overlay delete-to-empty root before promotion", () =>
    Effect.gen(function* () {
      const mpf = yield* MidgardMpf.createScratchFromList(
        "overlay-delete-to-empty",
        [{ key: key1, value: value1 }],
        { engine: "overlay" },
      );
      yield* mpf.beginBlockOverlay();
      const root = yield* mpf.applyBatch([{ type: "delete", key: key1 }]);

      expect(root.toString("hex")).toBe(SDK.EMPTY_MERKLE_TREE_ROOT);
      yield* mpf.flushBlockOverlay(root);
      expect(yield* mpf.rootHex()).toBe(SDK.EMPTY_MERKLE_TREE_ROOT);
      yield* mpf.close();
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

  it.effect(
    "maintains exact DA UTxO bytes across dependent within-block mutations",
    () =>
      Effect.gen(function* () {
        const payloadEntry = (key: number, value: number) => ({
          outref: Buffer.from([key]),
          output: Buffer.from([value]),
        });
        const cases: readonly {
          readonly label: string;
          readonly base: readonly ReturnType<typeof payloadEntry>[];
          readonly ops: readonly MpfBatchOp[];
        }[] = [
          {
            label: "insert-new-delete",
            base: [],
            ops: [
              {
                type: "insert",
                key: Buffer.from([1]),
                value: Buffer.from([9]),
              },
              { type: "delete", key: Buffer.from([1]) },
            ],
          },
          {
            label: "replace-existing",
            base: [payloadEntry(1, 1)],
            ops: [
              {
                type: "insert",
                key: Buffer.from([1]),
                value: Buffer.from([2]),
              },
            ],
          },
          {
            label: "delete-reinsert",
            base: [payloadEntry(1, 1)],
            ops: [
              { type: "delete", key: Buffer.from([1]) },
              {
                type: "insert",
                key: Buffer.from([1]),
                value: Buffer.from([3]),
              },
            ],
          },
          {
            label: "dependent-three-transaction-chain",
            base: [payloadEntry(1, 1)],
            ops: [
              { type: "delete", key: Buffer.from([1]) },
              {
                type: "insert",
                key: Buffer.from([2]),
                value: Buffer.from([2]),
              },
              { type: "delete", key: Buffer.from([2]) },
              {
                type: "insert",
                key: Buffer.from([3]),
                value: Buffer.from([3]),
              },
              { type: "delete", key: Buffer.from([3]) },
              {
                type: "insert",
                key: Buffer.from([4]),
                value: Buffer.from([4]),
              },
            ],
          },
        ];

        for (const testCase of cases) {
          const actual =
            yield* applyLedgerOpsToUtxoPayloadAggregateFromFullValues(
              utxoPayloadAggregateFromEntries(testCase.base),
              testCase.ops,
              new Map(
                testCase.base.map((entry) => [
                  entry.outref.toString("hex"),
                  entry.output,
                ]),
              ),
              new Map(
                testCase.ops.flatMap((operation) =>
                  operation.type === "delete"
                    ? []
                    : [
                        [
                          operation.key.toString("hex"),
                          operation.value,
                        ] as const,
                      ],
                ),
              ),
            );
          const materialized = new Map(
            testCase.base.map((entry) => [
              entry.outref.toString("hex"),
              { outref: entry.outref, output: entry.output },
            ]),
          );
          for (const op of testCase.ops) {
            const key = op.key.toString("hex");
            if (op.type === "delete") materialized.delete(key);
            else materialized.set(key, { outref: op.key, output: op.value });
          }
          const finalEntries = [...materialized.values()];
          expect(actual, testCase.label).toEqual(
            utxoPayloadAggregateFromEntries(finalEntries),
          );

          const header: SDK.HeaderV1 = {
            prevUtxosRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
            utxosRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
            withdrawalsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
            forcedTransactionsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
            transactionsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
            depositsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
            transitionTraceRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
            eventToStepRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
            validationTracesRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
            withdrawalCount: 0n,
            forcedTransactionCount: 0n,
            l2TransactionCount: 0n,
            depositCount: 0n,
            totalEventCount: 0n,
            transitionStepCount: 0n,
            validationTraceCount: 0n,
            startTime: 0n,
            endTime: 1n,
            blockSlot: 0n,
            expectedNetworkId: 0n,
            minFeeA: 0n,
            minFeeB: 0n,
            prevHeaderHash: "00".repeat(28),
            operatorVkey: "11".repeat(28),
            protocolVersion: 1n,
          };
          const payload: SDK.DaPayloadV1 = {
            version: SDK.DA_PAYLOAD_V1_VERSION,
            block_body: {
              header_hash: "22".repeat(28),
              header,
              utxos: finalEntries.map((entry) => [
                entry.outref.toString("hex"),
                entry.output.toString("hex"),
              ]),
              withdrawals: [],
              forced_transactions: [],
              transactions: [],
              deposits: [],
              transition_trace: [],
              event_to_step: [],
              transaction_preimages: [],
              forced_transaction_preimages: [],
              cek_program_material: [],
              validation_traces: [],
              validation_trace_witnesses: [],
              counts: {
                withdrawalCount: 0n,
                forcedTransactionCount: 0n,
                l2TransactionCount: 0n,
                depositCount: 0n,
                totalEventCount: 0n,
                transitionStepCount: 0n,
                validationTraceCount: 0n,
              },
            },
          };
          expect(
            SDK.daPayloadV1EncodedSizeFromUtxoAggregate(payload, actual),
            testCase.label,
          ).toBe(SDK.encodeDaPayloadV1(payload).length);
        }
      }),
  );
});
