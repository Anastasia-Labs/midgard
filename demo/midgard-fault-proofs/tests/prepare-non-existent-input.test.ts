import {
  computeHash32,
  computeMidgardNativeTxId,
  encodeCbor,
  encodeMidgardNativeTxCanonical,
  encodeMidgardSpendInputItem,
  materializeMidgardNativeTxFromCanonical,
  MIDGARD_NATIVE_TX_VERSION,
  MIDGARD_POSIX_TIME_NONE,
  type MidgardNativeTxFull,
} from "@al-ft/midgard-core";
import { asLucidSchema } from "@al-ft/midgard-core/lucid-data";
import { EMPTY_MERKLE_TREE_ROOT, Proof } from "@al-ft/midgard-sdk/core";
import * as SDK from "@al-ft/midgard-sdk/core";
import { h28, h32 } from "@al-ft/midgard-test-support/hex";
import { buildCanonicalMidgardLedgerEntryOutputMaterial } from "@al-ft/midgard-validation/ledger-output-descriptor";
import { Data } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import { isReplayLocalFault } from "../src/block-replay.faults.js";
import {
  LedgerRootMismatchError,
  predecessorLedger,
} from "../src/ledger-at.js";
import {
  createLedgerStore,
  createMemoryLedgerStorage,
  LedgerNodeMissing,
} from "../src/ledger-store/index.js";
import type { NodeTransactionPayload } from "../src/prepare-double-spend.encode-l2-transaction-source-value.js";
import { prepareNonExistentInputFromTransactions } from "../src/prepare-non-existent-input.js";
import {
  keyValuePhasNonMembershipProof,
  type KeyValuePhasRoot,
  keyValuePhasRootWithCount,
} from "../src/transition-trace/phas.js";
import { singleLedgerAt } from "./support/predecessor-ledger.js";

const EMPTY_CBOR_LIST = encodeCbor([]);
const EMPTY_CBOR_NULL = encodeCbor(null);
const EMPTY_NULL_ROOT = computeHash32(EMPTY_CBOR_NULL);
const LEDGER_OUTPUT_CBOR =
  "a200581d70aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa018200a0";

const inputCbor = (txHash: string, outputIndex: bigint): Buffer =>
  encodeMidgardSpendInputItem({
    txId: Buffer.from(txHash, "hex"),
    outputIndex: Number(outputIndex),
  });

const makeNativeTx = ({
  spendInputs,
  fee,
}: {
  readonly spendInputs: readonly Buffer[];
  readonly fee: bigint;
}): MidgardNativeTxFull =>
  materializeMidgardNativeTxFromCanonical({
    version: MIDGARD_NATIVE_TX_VERSION,
    validity: "TxIsValid",
    body: {
      spendInputsPreimageCbor: encodeCbor(spendInputs),
      referenceInputsPreimageCbor: EMPTY_CBOR_LIST,
      outputsPreimageCbor: EMPTY_CBOR_LIST,
      fee,
      validityIntervalStart: MIDGARD_POSIX_TIME_NONE,
      validityIntervalEnd: MIDGARD_POSIX_TIME_NONE,
      requiredObserversPreimageCbor: EMPTY_CBOR_LIST,
      requiredSignersPreimageCbor: EMPTY_CBOR_LIST,
      mintPreimageCbor: EMPTY_CBOR_LIST,
      scriptIntegrityHash: EMPTY_NULL_ROOT,
      auxiliaryDataHash: EMPTY_NULL_ROOT,
      networkId: 0n,
    },
    witnessSet: {
      addrTxWitsPreimageCbor: EMPTY_CBOR_LIST,
      scriptTxWitsPreimageCbor: EMPTY_CBOR_LIST,
      redeemerTxWitsPreimageCbor: EMPTY_CBOR_LIST,
    },
  });

const payloadFromTx = (tx: MidgardNativeTxFull): NodeTransactionPayload => ({
  nodeTxId: computeMidgardNativeTxId(tx).toString("hex"),
  txCbor: encodeMidgardNativeTxCanonical(tx).toString("hex"),
});

// Phantom input the bad tx spends (absent from an empty-genesis ledger and
// never produced by any block tx).
const PHANTOM_TX_ID = h32(0xde);
const badTxPayload = payloadFromTx(
  makeNativeTx({ spendInputs: [inputCbor(PHANTOM_TX_ID, 0n)], fee: 0n }),
);
// A second well-formed tx so the transactions trie is non-trivial.
const otherTxPayload = payloadFromTx(
  makeNativeTx({ spendInputs: [inputCbor(h32(0xc1), 0n)], fee: 1n }),
);

type LedgerPairs = readonly (readonly [string, string])[];

/** The MPF trie a header commits for this ledger: out-ref to descriptor. */
const ledgerPhasTrie = async (ledger: LedgerPairs): Promise<KeyValuePhasRoot> =>
  await keyValuePhasRootWithCount(
    ledger.map(([key, value]) => ({
      key: Buffer.from(key, "hex"),
      value: buildCanonicalMidgardLedgerEntryOutputMaterial({
        outRef: Buffer.from(key, "hex"),
        outputCbor: Buffer.from(value, "hex"),
      }).descriptorCbor,
    })),
  );

/** The MPF root a header commits for this ledger. */
const ledgerRoot = async (ledger: LedgerPairs): Promise<string> =>
  (await ledgerPhasTrie(ledger)).root;

// The challenged block's `prev_header_hash`: the predecessor the ledger is after.
const PREV_HEADER_HASH = h28(0x90);

/**
 * The predecessor ledger as a replaying party gets it for a header
 * committing `root`: its source answers `ledger` opened at its own root.
 */
const storePredecessor = async (ledger: LedgerPairs, root: string) =>
  await predecessorLedger({
    prevHeaderHash: PREV_HEADER_HASH,
    prevUtxosRoot: root,
    ledgerAt: singleLedgerAt(
      PREV_HEADER_HASH,
      ledger.map(([key, value]) => ({
        outRef: Buffer.from(key, "hex"),
        output: Buffer.from(value, "hex"),
      })),
      await ledgerRoot(ledger),
    ),
    label: "test predecessor ledger",
  });

// A ledger UTxO (present in the predecessor ledger) that is NOT the phantom
// input the bad tx spends.
const LEDGER_UTXO_KEY = inputCbor(h32(0xaa), 0n).toString("hex");
const PHANTOM_LEDGER_KEY = inputCbor(PHANTOM_TX_ID, 0n).toString("hex");
// The same out-ref with a different output: one wrong ledger entry.
const WRONG_OUTPUT_CBOR =
  "a200581d70bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb018200a0";

describe("prepare-non-existent-input", () => {
  it("prepares the four submit-step artifacts for a phantom-input tx over the empty ledger", async () => {
    const output = await prepareNonExistentInputFromTransactions({
      headerHash: h28(0xaa),
      transactions: [badTxPayload, otherTxPayload],
      badTxId: badTxPayload.nodeTxId,
    });

    expect(output.headerHash).toBe(h28(0xaa));
    expect(output.txCount).toBe(2);
    expect(output.badTxId).toBe(badTxPayload.nodeTxId);
    expect(output.badInputIndex).toBe(0);
    expect(output.prevUtxosRoot).toBe(EMPTY_MERKLE_TREE_ROOT);
    expect(output.missingInput).toEqual({
      tx_id: PHANTOM_TX_ID,
      output_index: 0n,
    });
    expect(output.inputsPreimage).toEqual([{ txId: PHANTOM_TX_ID, index: 0n }]);
    expect(output.transactionsRoot).toMatch(/^[0-9a-f]{64}$/);
    expect(output.committedTransactionsRoot).toMatch(/^[0-9a-f]{64}$/);
    expect(output.committedTransactionsRoot).not.toBe(output.transactionsRoot);
    expect(output.txInclusion.nativeTxId).toBe(badTxPayload.nodeTxId);
    expect(output.txInclusion.txMembershipProofCbor.length).toBeGreaterThan(0);
    expect(output.ledgerNonMembershipProofCbor.length).toBeGreaterThan(0);
    expect(output.txsNonMembershipProofCbor.length).toBeGreaterThan(0);
  });

  it("defaults to the sole transaction when --bad-tx-id is omitted", async () => {
    const output = await prepareNonExistentInputFromTransactions({
      headerHash: h28(0xbb),
      transactions: [badTxPayload],
    });
    expect(output.badTxId).toBe(badTxPayload.nodeTxId);
    expect(output.missingInput.tx_id).toBe(PHANTOM_TX_ID);
  });

  it("requires --bad-tx-id when the block has multiple transactions", async () => {
    await expect(
      prepareNonExistentInputFromTransactions({
        headerHash: h28(0xcc),
        transactions: [badTxPayload, otherTxPayload],
      }),
    ).rejects.toThrow("specify --bad-tx-id");
  });

  it("rejects a --bad-input-index out of bounds", async () => {
    await expect(
      prepareNonExistentInputFromTransactions({
        headerHash: h28(0xdd),
        transactions: [badTxPayload],
        badInputIndex: 5,
      }),
    ).rejects.toThrow("out of bounds");
  });

  it("rejects an expected transactions root that does not match the reconstruction", async () => {
    await expect(
      prepareNonExistentInputFromTransactions({
        headerHash: h28(0xee),
        transactions: [badTxPayload],
        expectedTransactionsRoot: h32(0x00),
      }),
    ).rejects.toThrow("does not match --expected-transactions-root");
  });

  it("accepts the counted transactions root committed by the block header", async () => {
    const prepared = await prepareNonExistentInputFromTransactions({
      headerHash: h28(0xef),
      transactions: [badTxPayload],
    });
    const committedRoot = await Effect.runPromise(
      SDK.commitCountedRootProgram({
        domain: SDK.ROOT_DOMAINS.transactionsV1,
        phasRoot: prepared.transactionsRoot,
        count: 1n,
      }),
    );

    const verified = await prepareNonExistentInputFromTransactions({
      headerHash: h28(0xef),
      transactions: [badTxPayload],
      expectedTransactionsRoot: committedRoot,
    });

    expect(verified.expectedTransactionsRoot).toEqual({
      value: committedRoot,
      matches: true,
    });
    expect(verified.committedTransactionsRoot).toBe(committedRoot);
    expect(verified.transactionsRoot).not.toBe(committedRoot);
  });

  it("refuses to build a ledger non-membership proof over a non-empty prev-utxos root", async () => {
    await expect(
      prepareNonExistentInputFromTransactions({
        headerHash: h28(0xff),
        transactions: [badTxPayload],
        prevUtxosRoot: h32(0xab),
      }),
    ).rejects.toThrow("starts from a non-empty ledger");
  });

  it("runs from the predecessor ledger for a non-first block", async () => {
    const ledger: LedgerPairs = [[LEDGER_UTXO_KEY, LEDGER_OUTPUT_CBOR]];
    const utxosRoot = await ledgerRoot(ledger);
    expect(utxosRoot).not.toBe(EMPTY_MERKLE_TREE_ROOT);

    const output = await prepareNonExistentInputFromTransactions({
      headerHash: h28(0xaa),
      transactions: [badTxPayload],
      prevUtxosRoot: utxosRoot,
      prevLedger: await storePredecessor(ledger, utxosRoot),
    });

    expect(output.prevUtxosRoot).toBe(utxosRoot);
    expect(output.missingInput.tx_id).toBe(PHANTOM_TX_ID);
    // A real ledger non-membership proof against the predecessor ledger's trie.
    expect(output.ledgerNonMembershipProofCbor.length).toBeGreaterThan(0);
  });

  it("submits the non-membership proof the whole predecessor ledger gives", async () => {
    const ledger: LedgerPairs = [
      [LEDGER_UTXO_KEY, LEDGER_OUTPUT_CBOR],
      [inputCbor(h32(0xdf), 1n).toString("hex"), LEDGER_OUTPUT_CBOR],
      [inputCbor(h32(0x01), 0n).toString("hex"), LEDGER_OUTPUT_CBOR],
    ];
    const utxosRoot = await ledgerRoot(ledger);
    const proof = async (trie: KeyValuePhasRoot) =>
      Data.to(
        await keyValuePhasNonMembershipProof(
          trie,
          Buffer.from(PHANTOM_LEDGER_KEY, "hex"),
        ),
        asLucidSchema(Proof),
      );
    const nonFirst = await prepareNonExistentInputFromTransactions({
      headerHash: h28(0xaa),
      transactions: [badTxPayload],
      prevUtxosRoot: utxosRoot,
      prevLedger: await storePredecessor(ledger, utxosRoot),
    });
    expect(nonFirst.ledgerNonMembershipProofCbor).toBe(
      await proof(await ledgerPhasTrie(ledger)),
    );
    const first = await prepareNonExistentInputFromTransactions({
      headerHash: h28(0xaa),
      transactions: [badTxPayload],
    });
    expect(first.ledgerNonMembershipProofCbor).toBe(
      await proof(await keyValuePhasRootWithCount([])),
    );
  });

  it("refuses a predecessor ledger with one wrong entry at the root check", async () => {
    const utxosRoot = await ledgerRoot([[LEDGER_UTXO_KEY, LEDGER_OUTPUT_CBOR]]);
    await expect(
      storePredecessor([[LEDGER_UTXO_KEY, WRONG_OUTPUT_CBOR]], utxosRoot),
    ).rejects.toBeInstanceOf(LedgerRootMismatchError);
  });

  it("refuses a predecessor ledger for a different root than the block's prev_utxos_root", async () => {
    const ledger: LedgerPairs = [[LEDGER_UTXO_KEY, LEDGER_OUTPUT_CBOR]];
    await expect(
      prepareNonExistentInputFromTransactions({
        headerHash: h28(0xaa),
        transactions: [badTxPayload],
        prevUtxosRoot: h32(0xbb),
        prevLedger: await storePredecessor(ledger, await ledgerRoot(ledger)),
      }),
    ).rejects.toBeInstanceOf(LedgerRootMismatchError);
  });

  it("refuses to prove non-membership when the input is present in the predecessor ledger", async () => {
    const ledger: LedgerPairs = [
      [LEDGER_UTXO_KEY, LEDGER_OUTPUT_CBOR],
      [PHANTOM_LEDGER_KEY, LEDGER_OUTPUT_CBOR],
    ];
    const utxosRoot = await ledgerRoot(ledger);
    await expect(
      prepareNonExistentInputFromTransactions({
        headerHash: h28(0xaa),
        transactions: [badTxPayload],
        prevUtxosRoot: utxosRoot,
        prevLedger: await storePredecessor(ledger, utxosRoot),
      }),
    ).rejects.toThrow(/present key/i);
  });

  it("names a predecessor ledger the store no longer holds this process's local fault", async () => {
    const ledger: LedgerPairs = [[LEDGER_UTXO_KEY, LEDGER_OUTPUT_CBOR]];
    const utxosRoot = await ledgerRoot(ledger);
    const store = createLedgerStore(createMemoryLedgerStorage());
    const prevLedger = await store.admit(
      ledger.map(([key, value]) => ({
        outRef: Buffer.from(key, "hex"),
        output: Buffer.from(value, "hex"),
      })),
      utxosRoot,
    );
    await store.retain(new Set());
    const error: unknown = await prepareNonExistentInputFromTransactions({
      headerHash: h28(0xaa),
      transactions: [badTxPayload],
      prevUtxosRoot: utxosRoot,
      prevLedger,
    }).catch((cause: unknown) => cause);
    expect(isReplayLocalFault(error)).toBe(true);
    expect((error as { cause: unknown }).cause).toBeInstanceOf(
      LedgerNodeMissing,
    );
  });
});
