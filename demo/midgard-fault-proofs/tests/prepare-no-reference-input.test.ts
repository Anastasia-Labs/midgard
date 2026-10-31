import { mkdtemp, readFile, rm, writeFile } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";

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
import { EMPTY_MERKLE_TREE_ROOT } from "@al-ft/midgard-sdk";
import * as SDK from "@al-ft/midgard-sdk";
import { h28, h32 } from "@al-ft/midgard-test-support/hex";
import { buildCanonicalMidgardLedgerEntryOutputMaterial } from "@al-ft/midgard-validation";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import {
  keyValuePhasRootWithCount,
  LedgerRootMismatchError,
  type NodeTransactionPayload,
  predecessorLedger,
  prepareNoReferenceInputFromFile,
  prepareNoReferenceInputFromNode,
  prepareNoReferenceInputFromTransactions,
  singleLedgerAt,
} from "../src/index.js";

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
  referenceInputs,
  fee,
}: {
  readonly spendInputs: readonly Buffer[];
  readonly referenceInputs: readonly Buffer[];
  readonly fee: bigint;
}): MidgardNativeTxFull =>
  materializeMidgardNativeTxFromCanonical({
    version: MIDGARD_NATIVE_TX_VERSION,
    validity: "TxIsValid",
    body: {
      spendInputsPreimageCbor: encodeCbor(spendInputs),
      referenceInputsPreimageCbor: encodeCbor(referenceInputs),
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

// Phantom reference input the bad tx reads (absent from an empty-genesis ledger
// and never produced by any block tx).
const PHANTOM_TX_ID = h32(0xde);
const badTxPayload = payloadFromTx(
  makeNativeTx({
    spendInputs: [inputCbor(h32(0xa0), 0n)],
    referenceInputs: [inputCbor(PHANTOM_TX_ID, 0n)],
    fee: 0n,
  }),
);
// A second well-formed tx so the transactions trie is non-trivial.
const otherTxPayload = payloadFromTx(
  makeNativeTx({
    spendInputs: [inputCbor(h32(0xc1), 0n)],
    referenceInputs: [],
    fee: 1n,
  }),
);

const withTempDir = async <A>(run: (dir: string) => Promise<A>): Promise<A> => {
  const dir = await mkdtemp(join(tmpdir(), "midgard-no-reference-input-"));
  try {
    return await run(dir);
  } finally {
    await rm(dir, { recursive: true, force: true });
  }
};

type LedgerPairs = readonly (readonly [string, string])[];

/** The MPF root a header commits for this ledger: out-ref to descriptor. */
const ledgerRoot = async (ledger: LedgerPairs): Promise<string> =>
  (
    await keyValuePhasRootWithCount(
      ledger.map(([key, value]) => ({
        key: Buffer.from(key, "hex"),
        value: buildCanonicalMidgardLedgerEntryOutputMaterial({
          outRef: Buffer.from(key, "hex"),
          outputCbor: Buffer.from(value, "hex"),
        }).descriptorCbor,
      })),
    )
  ).root;

// The challenged block's `prev_header_hash`: the predecessor the ledger is after.
const PREV_HEADER_HASH = h28(0x90);

/** The predecessor ledger as a replaying party holds it, checked against `root`. */
const checkedPredecessor = async (ledger: LedgerPairs, root: string) =>
  await predecessorLedger({
    prevHeaderHash: PREV_HEADER_HASH,
    prevUtxosRoot: root,
    ledgerAt: singleLedgerAt(
      PREV_HEADER_HASH,
      ledger.map(([key, value]) => ({
        outRef: Buffer.from(key, "hex"),
        output: Buffer.from(value, "hex"),
      })),
    ),
    label: "test predecessor ledger",
  });

// A ledger UTxO (present in the predecessor ledger) that is NOT the phantom
// reference input the bad tx reads.
const LEDGER_UTXO_KEY = inputCbor(h32(0xaa), 0n).toString("hex");
const PHANTOM_LEDGER_KEY = inputCbor(PHANTOM_TX_ID, 0n).toString("hex");
// The same out-ref with a different output: one wrong ledger entry.
const WRONG_OUTPUT_CBOR =
  "a200581d70bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb018200a0";

describe("prepare-no-reference-input", () => {
  it("prepares the four submit-step artifacts for a phantom-reference-input tx over the empty ledger", async () => {
    const output = await prepareNoReferenceInputFromTransactions({
      headerHash: h28(0xaa),
      transactions: [badTxPayload, otherTxPayload],
      badTxId: badTxPayload.nodeTxId,
    });

    expect(output.headerHash).toBe(h28(0xaa));
    expect(output.txCount).toBe(2);
    expect(output.badTxId).toBe(badTxPayload.nodeTxId);
    expect(output.badReferenceInputIndex).toBe(0);
    expect(output.prevUtxosRoot).toBe(EMPTY_MERKLE_TREE_ROOT);
    expect(output.missingReferenceInput).toEqual({
      tx_id: PHANTOM_TX_ID,
      output_index: 0n,
    });
    expect(output.referenceInputsPreimage).toEqual([
      { txId: PHANTOM_TX_ID, index: 0n },
    ]);
    expect(output.transactionsRoot).toMatch(/^[0-9a-f]{64}$/);
    expect(output.committedTransactionsRoot).toMatch(/^[0-9a-f]{64}$/);
    expect(output.committedTransactionsRoot).not.toBe(output.transactionsRoot);
    expect(output.txInclusion.nativeTxId).toBe(badTxPayload.nodeTxId);
    expect(output.txInclusion.transactionsPhasRoot).toBe(
      output.transactionsRoot,
    );
    expect(output.txInclusion.txMembershipProofCbor.length).toBeGreaterThan(0);
    expect(output.ledgerNonMembershipProofCbor.length).toBeGreaterThan(0);
    expect(output.txsNonMembershipProofCbor.length).toBeGreaterThan(0);
  });

  it("defaults to the sole transaction when --bad-tx-id is omitted", async () => {
    const output = await prepareNoReferenceInputFromTransactions({
      headerHash: h28(0xbb),
      transactions: [badTxPayload],
    });
    expect(output.badTxId).toBe(badTxPayload.nodeTxId);
    expect(output.missingReferenceInput.tx_id).toBe(PHANTOM_TX_ID);
  });

  it("requires --bad-tx-id when the block has multiple transactions", async () => {
    await expect(
      prepareNoReferenceInputFromTransactions({
        headerHash: h28(0xcc),
        transactions: [badTxPayload, otherTxPayload],
      }),
    ).rejects.toThrow("specify --bad-tx-id");
  });

  it("rejects a --bad-reference-input-index out of bounds", async () => {
    await expect(
      prepareNoReferenceInputFromTransactions({
        headerHash: h28(0xdd),
        transactions: [badTxPayload],
        badReferenceInputIndex: 5,
      }),
    ).rejects.toThrow("out of bounds");
  });

  it("rejects a transaction that reads no reference inputs at all", async () => {
    await expect(
      prepareNoReferenceInputFromTransactions({
        headerHash: h28(0xdd),
        transactions: [otherTxPayload],
      }),
    ).rejects.toThrow("out of bounds for 0 reference inputs");
  });

  it("rejects an expected transactions root that does not match the reconstruction", async () => {
    await expect(
      prepareNoReferenceInputFromTransactions({
        headerHash: h28(0xee),
        transactions: [badTxPayload],
        expectedTransactionsRoot: h32(0x00),
      }),
    ).rejects.toThrow("does not match --expected-transactions-root");
  });

  it("accepts the counted transactions root committed by the block header", async () => {
    const prepared = await prepareNoReferenceInputFromTransactions({
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

    const verified = await prepareNoReferenceInputFromTransactions({
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
      prepareNoReferenceInputFromTransactions({
        headerHash: h28(0xff),
        transactions: [badTxPayload],
        prevUtxosRoot: h32(0xab),
      }),
    ).rejects.toThrow("starts from a non-empty ledger");
  });

  it("writes deterministic submit-step files from an explicit transactions file", async () => {
    await withTempDir(async (dir) => {
      const transactionsPath = join(dir, "block-transactions.json");
      await writeFile(
        transactionsPath,
        JSON.stringify([badTxPayload, otherTxPayload]),
      );

      const output = await prepareNoReferenceInputFromFile({
        headerHash: h28(0xaa),
        transactionsPath,
        badTxId: badTxPayload.nodeTxId,
        outputDir: dir,
      });

      expect(output.files?.txInclusionPath).toBe(
        join(dir, "nri-tx-inclusion.json"),
      );
      expect(output.files?.referenceInputsPreimagePath).toBe(
        join(dir, "nri-reference-inputs-preimage.json"),
      );
      expect(output.files?.ledgerNonMembershipPath).toBe(
        join(dir, "nri-ledger-non-membership.json"),
      );
      expect(output.files?.txsNonMembershipPath).toBe(
        join(dir, "nri-txs-non-membership.json"),
      );
      expect(output.files?.planPath).toBe(join(dir, "nri-plan.json"));

      const inclusion = JSON.parse(
        await readFile(join(dir, "nri-tx-inclusion.json"), "utf8"),
      ) as { readonly nativeTxId: string };
      expect(inclusion.nativeTxId).toBe(badTxPayload.nodeTxId);

      const referenceInputsPreimage = JSON.parse(
        await readFile(join(dir, "nri-reference-inputs-preimage.json"), "utf8"),
      ) as readonly { readonly txId: string; readonly index: string }[];
      expect(referenceInputsPreimage).toEqual([
        { txId: PHANTOM_TX_ID, index: "0" },
      ]);

      const plan = JSON.parse(
        await readFile(join(dir, "nri-plan.json"), "utf8"),
      ) as {
        readonly badTxId: string;
        readonly badReferenceInputIndex: number;
      };
      expect(plan).toMatchObject({
        badTxId: badTxPayload.nodeTxId,
        badReferenceInputIndex: 0,
      });
    });
  });

  it("fetches node block transactions before preparing no-reference-input material", async () => {
    const fetchImpl = async (input: string | URL): Promise<Response> => {
      const url = new URL(String(input));
      if (url.pathname === "/block") {
        return new Response(
          JSON.stringify({ hashes: [badTxPayload.nodeTxId] }),
          { status: 200 },
        );
      }
      if (url.pathname === "/tx") {
        return new Response(JSON.stringify({ tx: badTxPayload.txCbor }), {
          status: 200,
        });
      }
      return new Response("not found", { status: 404 });
    };

    const output = await prepareNoReferenceInputFromNode({
      midgardNodeUrl: "http://node.local/",
      headerHash: h28(0xaa),
      fetchImpl,
    });

    expect(output.badTxId).toBe(badTxPayload.nodeTxId);
    expect(output.missingReferenceInput.tx_id).toBe(PHANTOM_TX_ID);
  });

  it("runs from the checked predecessor ledger for a non-first block", async () => {
    const ledger: LedgerPairs = [[LEDGER_UTXO_KEY, LEDGER_OUTPUT_CBOR]];
    const utxosRoot = await ledgerRoot(ledger);
    expect(utxosRoot).not.toBe(EMPTY_MERKLE_TREE_ROOT);

    const output = await prepareNoReferenceInputFromTransactions({
      headerHash: h28(0xaa),
      transactions: [badTxPayload],
      prevUtxosRoot: utxosRoot,
      prevLedger: await checkedPredecessor(ledger, utxosRoot),
    });

    expect(output.prevUtxosRoot).toBe(utxosRoot);
    expect(output.missingReferenceInput.tx_id).toBe(PHANTOM_TX_ID);
    // A real ledger non-membership proof against the predecessor ledger's trie.
    expect(output.ledgerNonMembershipProofCbor.length).toBeGreaterThan(0);
  });

  it("refuses a predecessor ledger with one wrong entry at the root check", async () => {
    const utxosRoot = await ledgerRoot([[LEDGER_UTXO_KEY, LEDGER_OUTPUT_CBOR]]);
    await expect(
      checkedPredecessor([[LEDGER_UTXO_KEY, WRONG_OUTPUT_CBOR]], utxosRoot),
    ).rejects.toBeInstanceOf(LedgerRootMismatchError);
  });

  it("refuses a checked ledger for a different root than the block's prev_utxos_root", async () => {
    const ledger: LedgerPairs = [[LEDGER_UTXO_KEY, LEDGER_OUTPUT_CBOR]];
    await expect(
      prepareNoReferenceInputFromTransactions({
        headerHash: h28(0xaa),
        transactions: [badTxPayload],
        prevUtxosRoot: h32(0xbb),
        prevLedger: await checkedPredecessor(ledger, await ledgerRoot(ledger)),
      }),
    ).rejects.toBeInstanceOf(LedgerRootMismatchError);
  });

  it("refuses to prove non-membership when the reference input is present in the predecessor ledger", async () => {
    const ledger: LedgerPairs = [
      [LEDGER_UTXO_KEY, LEDGER_OUTPUT_CBOR],
      [PHANTOM_LEDGER_KEY, LEDGER_OUTPUT_CBOR],
    ];
    const utxosRoot = await ledgerRoot(ledger);
    await expect(
      prepareNoReferenceInputFromTransactions({
        headerHash: h28(0xaa),
        transactions: [badTxPayload],
        prevUtxosRoot: utxosRoot,
        prevLedger: await checkedPredecessor(ledger, utxosRoot),
      }),
    ).rejects.toThrow(/present key/i);
  });

  it("reads --prev-ledger-file and checks it against --prev-utxos-root", async () => {
    await withTempDir(async (dir) => {
      const transactionsPath = join(dir, "block-transactions.json");
      await writeFile(transactionsPath, JSON.stringify([badTxPayload]));
      const ledger: LedgerPairs = [[LEDGER_UTXO_KEY, LEDGER_OUTPUT_CBOR]];
      const utxosRoot = await ledgerRoot(ledger);
      const prevLedgerPath = join(dir, "prev-ledger.json");
      await writeFile(
        prevLedgerPath,
        JSON.stringify({ headerHash: PREV_HEADER_HASH, entries: ledger }),
      );
      const wrongLedgerPath = join(dir, "wrong-ledger.json");
      await writeFile(
        wrongLedgerPath,
        JSON.stringify({
          headerHash: PREV_HEADER_HASH,
          entries: [[LEDGER_UTXO_KEY, WRONG_OUTPUT_CBOR]],
        }),
      );
      const config = {
        headerHash: h28(0xaa),
        transactionsPath,
        prevHeaderHash: PREV_HEADER_HASH,
        prevUtxosRoot: utxosRoot,
      };

      const output = await prepareNoReferenceInputFromFile({
        ...config,
        prevLedgerPath,
      });
      expect(output.prevUtxosRoot).toBe(utxosRoot);
      expect(output.missingReferenceInput.tx_id).toBe(PHANTOM_TX_ID);

      await expect(
        prepareNoReferenceInputFromFile({
          ...config,
          prevLedgerPath: wrongLedgerPath,
        }),
      ).rejects.toBeInstanceOf(LedgerRootMismatchError);
      await expect(
        prepareNoReferenceInputFromFile({
          ...config,
          prevHeaderHash: h28(0x91),
          prevLedgerPath,
        }),
      ).rejects.toThrow("is unavailable");
    });
  });
});
