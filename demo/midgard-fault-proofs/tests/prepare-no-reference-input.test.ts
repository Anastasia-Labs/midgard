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
import { wrapDaPayload } from "@al-ft/midgard-core/da-payload-envelope";
import { EMPTY_MERKLE_TREE_ROOT } from "@al-ft/midgard-sdk";
import * as SDK from "@al-ft/midgard-sdk";
import { buildCanonicalMidgardLedgerEntryOutputMaterial } from "@al-ft/midgard-validation";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import {
  keyValuePhasRootWithCount,
  type NodeTransactionPayload,
  prepareNoReferenceInputFromFile,
  prepareNoReferenceInputFromNode,
  prepareNoReferenceInputFromTransactions,
} from "../src/index.js";

const h28 = (byte: string): string => byte.repeat(28);
const h32 = (byte: string): string => byte.repeat(32);
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
const PHANTOM_TX_ID = h32("de");
const badTxPayload = payloadFromTx(
  makeNativeTx({
    spendInputs: [inputCbor(h32("a0"), 0n)],
    referenceInputs: [inputCbor(PHANTOM_TX_ID, 0n)],
    fee: 0n,
  }),
);
// A second well-formed tx so the transactions trie is non-trivial.
const otherTxPayload = payloadFromTx(
  makeNativeTx({
    spendInputs: [inputCbor(h32("c1"), 0n)],
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

const ZERO_COUNTS = {
  withdrawalCount: 0n,
  forcedTransactionCount: 0n,
  l2TransactionCount: 0n,
  depositCount: 0n,
  totalEventCount: 0n,
  transitionStepCount: 0n,
  validationTraceCount: 0n,
};

/**
 * Builds a minimal `DaPayload` whose body carries only a `utxos` set (the
 * ledger snapshot), with every other body root empty. Returns its canonical CBOR
 * and the raw `utxos_root` — which is what the *next* block commits as its
 * `prev_utxos_root`.
 */
const buildPrevBlockPayload = async (
  utxos: readonly (readonly [string, string])[],
): Promise<{
  readonly payloadEnvelopeCbor: Buffer;
  readonly utxosRoot: string;
}> => {
  const utxoRoot = await keyValuePhasRootWithCount(
    utxos.map(([key, value]) => ({
      key: Buffer.from(key, "hex"),
      value: buildCanonicalMidgardLedgerEntryOutputMaterial({
        outRef: Buffer.from(key, "hex"),
        outputCbor: Buffer.from(value, "hex"),
      }).descriptorCbor,
    })),
  );
  const header: SDK.Header = {
    prevUtxosRoot: EMPTY_MERKLE_TREE_ROOT,
    utxosRoot: utxoRoot.root,
    withdrawalsRoot: EMPTY_MERKLE_TREE_ROOT,
    forcedTransactionsRoot: EMPTY_MERKLE_TREE_ROOT,
    transactionsRoot: EMPTY_MERKLE_TREE_ROOT,
    depositsRoot: EMPTY_MERKLE_TREE_ROOT,
    transitionTraceRoot: EMPTY_MERKLE_TREE_ROOT,
    eventToStepRoot: EMPTY_MERKLE_TREE_ROOT,
    validationTracesRoot: EMPTY_MERKLE_TREE_ROOT,
    ...ZERO_COUNTS,
    startTime: 10n,
    endTime: 20n,
    blockSlot: 0n,
    expectedNetworkId: 0n,
    minFeeA: 0n,
    minFeeB: 0n,
    prevHeaderHash: h28("90"),
    operatorVkey: h28("91"),
    protocolVersion: 1n,
  };
  const headerHash = await Effect.runPromise(SDK.hashBlockHeader(header));
  const payload: SDK.DaPayload = {
    version: SDK.DA_PAYLOAD_VERSION,
    block_body: {
      header_hash: headerHash,
      header,
      utxos: [...utxos]
        .map(([key, value]) => [key, value] as [string, string])
        .sort(([left], [right]) => (left < right ? -1 : left > right ? 1 : 0)),
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
      counts: ZERO_COUNTS,
    },
  };
  return {
    payloadEnvelopeCbor: await wrapDaPayload(SDK.encodeDaPayload(payload), {
      mode: "identity",
    }),
    utxosRoot: utxoRoot.root,
  };
};

// A ledger UTxO (present in the prev-block snapshot) that is NOT the phantom
// reference input the bad tx reads.
const LEDGER_UTXO_KEY = inputCbor(h32("aa"), 0n).toString("hex");
const PHANTOM_LEDGER_KEY = inputCbor(PHANTOM_TX_ID, 0n).toString("hex");

describe("prepare-no-reference-input", () => {
  it("prepares the four submit-step artifacts for a phantom-reference-input tx over the empty ledger", async () => {
    const output = await prepareNoReferenceInputFromTransactions({
      headerHash: h28("aa"),
      transactions: [badTxPayload, otherTxPayload],
      badTxId: badTxPayload.nodeTxId,
    });

    expect(output.headerHash).toBe(h28("aa"));
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
      headerHash: h28("bb"),
      transactions: [badTxPayload],
    });
    expect(output.badTxId).toBe(badTxPayload.nodeTxId);
    expect(output.missingReferenceInput.tx_id).toBe(PHANTOM_TX_ID);
  });

  it("requires --bad-tx-id when the block has multiple transactions", async () => {
    await expect(
      prepareNoReferenceInputFromTransactions({
        headerHash: h28("cc"),
        transactions: [badTxPayload, otherTxPayload],
      }),
    ).rejects.toThrow("specify --bad-tx-id");
  });

  it("rejects a --bad-reference-input-index out of bounds", async () => {
    await expect(
      prepareNoReferenceInputFromTransactions({
        headerHash: h28("dd"),
        transactions: [badTxPayload],
        badReferenceInputIndex: 5,
      }),
    ).rejects.toThrow("out of bounds");
  });

  it("rejects a transaction that reads no reference inputs at all", async () => {
    await expect(
      prepareNoReferenceInputFromTransactions({
        headerHash: h28("dd"),
        transactions: [otherTxPayload],
      }),
    ).rejects.toThrow("out of bounds for 0 reference inputs");
  });

  it("rejects an expected transactions root that does not match the reconstruction", async () => {
    await expect(
      prepareNoReferenceInputFromTransactions({
        headerHash: h28("ee"),
        transactions: [badTxPayload],
        expectedTransactionsRoot: h32("00"),
      }),
    ).rejects.toThrow("does not match --expected-transactions-root");
  });

  it("accepts the counted transactions root committed by the block header", async () => {
    const prepared = await prepareNoReferenceInputFromTransactions({
      headerHash: h28("ef"),
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
      headerHash: h28("ef"),
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
        headerHash: h28("ff"),
        transactions: [badTxPayload],
        prevUtxosRoot: h32("ab"),
      }),
    ).rejects.toThrow("non-empty prev-utxos ledger");
  });

  it("writes deterministic submit-step files from an explicit transactions file", async () => {
    await withTempDir(async (dir) => {
      const transactionsPath = join(dir, "block-transactions.json");
      await writeFile(
        transactionsPath,
        JSON.stringify([badTxPayload, otherTxPayload]),
      );

      const output = await prepareNoReferenceInputFromFile({
        headerHash: h28("aa"),
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
      headerHash: h28("aa"),
      fetchImpl,
    });

    expect(output.badTxId).toBe(badTxPayload.nodeTxId);
    expect(output.missingReferenceInput.tx_id).toBe(PHANTOM_TX_ID);
  });

  it("reconstructs the ledger from the previous block's DA payload for a non-first block", async () => {
    const { payloadEnvelopeCbor, utxosRoot } = await buildPrevBlockPayload([
      [LEDGER_UTXO_KEY, LEDGER_OUTPUT_CBOR],
    ]);
    expect(utxosRoot).not.toBe(EMPTY_MERKLE_TREE_ROOT);

    const output = await prepareNoReferenceInputFromTransactions({
      headerHash: h28("aa"),
      transactions: [badTxPayload],
      prevUtxosRoot: utxosRoot,
      prevBlockPayloadEnvelopeCbor: payloadEnvelopeCbor,
    });

    expect(output.prevUtxosRoot).toBe(utxosRoot);
    expect(output.missingReferenceInput.tx_id).toBe(PHANTOM_TX_ID);
    // A real ledger non-membership proof against the reconstructed trie.
    expect(output.ledgerNonMembershipProofCbor.length).toBeGreaterThan(0);
  });

  it("rejects a prev-block payload whose utxos_root does not match --prev-utxos-root", async () => {
    const { payloadEnvelopeCbor } = await buildPrevBlockPayload([
      [LEDGER_UTXO_KEY, LEDGER_OUTPUT_CBOR],
    ]);
    await expect(
      prepareNoReferenceInputFromTransactions({
        headerHash: h28("aa"),
        transactions: [badTxPayload],
        prevUtxosRoot: h32("bb"),
        prevBlockPayloadEnvelopeCbor: payloadEnvelopeCbor,
      }),
    ).rejects.toThrow("does not match --prev-utxos-root");
  });

  it("refuses to prove non-membership when the reference input is present in the reconstructed ledger", async () => {
    const { payloadEnvelopeCbor, utxosRoot } = await buildPrevBlockPayload([
      [LEDGER_UTXO_KEY, LEDGER_OUTPUT_CBOR],
      [PHANTOM_LEDGER_KEY, LEDGER_OUTPUT_CBOR],
    ]);
    await expect(
      prepareNoReferenceInputFromTransactions({
        headerHash: h28("aa"),
        transactions: [badTxPayload],
        prevUtxosRoot: utxosRoot,
        prevBlockPayloadEnvelopeCbor: payloadEnvelopeCbor,
      }),
    ).rejects.toThrow(/present key/i);
  });
});
