import { mkdtemp, readFile, rm, writeFile } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";

import {
  computeHash32,
  computeMidgardNativeTxIdV1,
  encodeCbor,
  encodeMidgardNativeTxCanonicalV1,
  encodeMidgardSpendInputItemV1,
  materializeMidgardNativeTxFromCanonicalV1,
  MIDGARD_NATIVE_TX_V1_VERSION,
  MIDGARD_POSIX_TIME_NONE,
  type MidgardNativeTxFullV1,
} from "@al-ft/midgard-core";
import { wrapDaPayloadV1 } from "@al-ft/midgard-core/da-payload-envelope";
import { EMPTY_MERKLE_TREE_ROOT } from "@al-ft/midgard-sdk";
import * as SDK from "@al-ft/midgard-sdk";
import { buildCanonicalMidgardLedgerEntryOutputMaterialV1 } from "@al-ft/midgard-validation";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import {
  keyValuePhasRootWithCount,
  type NodeTransactionPayload,
  prepareNonExistentInputFromFile,
  prepareNonExistentInputFromNode,
  prepareNonExistentInputFromTransactions,
} from "../src/index.js";

const h28 = (byte: string): string => byte.repeat(28);
const h32 = (byte: string): string => byte.repeat(32);
const EMPTY_CBOR_LIST = encodeCbor([]);
const EMPTY_CBOR_NULL = encodeCbor(null);
const EMPTY_NULL_ROOT = computeHash32(EMPTY_CBOR_NULL);
const LEDGER_OUTPUT_CBOR =
  "a200581d70aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa018200a0";

const inputCbor = (txHash: string, outputIndex: bigint): Buffer =>
  encodeMidgardSpendInputItemV1({
    txId: Buffer.from(txHash, "hex"),
    outputIndex: Number(outputIndex),
  });

const makeNativeTx = ({
  spendInputs,
  fee,
}: {
  readonly spendInputs: readonly Buffer[];
  readonly fee: bigint;
}): MidgardNativeTxFullV1 =>
  materializeMidgardNativeTxFromCanonicalV1({
    version: MIDGARD_NATIVE_TX_V1_VERSION,
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

const payloadFromTx = (tx: MidgardNativeTxFullV1): NodeTransactionPayload => ({
  nodeTxId: computeMidgardNativeTxIdV1(tx).toString("hex"),
  txCbor: encodeMidgardNativeTxCanonicalV1(tx).toString("hex"),
});

// Phantom input the bad tx spends (absent from an empty-genesis ledger and
// never produced by any block tx).
const PHANTOM_TX_ID = h32("de");
const badTxPayload = payloadFromTx(
  makeNativeTx({ spendInputs: [inputCbor(PHANTOM_TX_ID, 0n)], fee: 0n }),
);
// A second well-formed tx so the transactions trie is non-trivial.
const otherTxPayload = payloadFromTx(
  makeNativeTx({ spendInputs: [inputCbor(h32("c1"), 0n)], fee: 1n }),
);

const withTempDir = async <A>(run: (dir: string) => Promise<A>): Promise<A> => {
  const dir = await mkdtemp(join(tmpdir(), "midgard-non-existent-input-"));
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
 * Builds a minimal `DaPayloadV1` whose body carries only a `utxos` set (the
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
      value: buildCanonicalMidgardLedgerEntryOutputMaterialV1({
        outRef: Buffer.from(key, "hex"),
        outputCbor: Buffer.from(value, "hex"),
      }).descriptorCbor,
    })),
  );
  const header: SDK.HeaderV1 = {
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
  const headerHash = await Effect.runPromise(SDK.hashBlockHeaderV1(header));
  const payload: SDK.DaPayloadV1 = {
    version: SDK.DA_PAYLOAD_V1_VERSION,
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
    payloadEnvelopeCbor: await wrapDaPayloadV1(SDK.encodeDaPayloadV1(payload), {
      mode: "identity",
    }),
    utxosRoot: utxoRoot.root,
  };
};

// A ledger UTxO (present in the prev-block snapshot) that is NOT the phantom
// input the bad tx spends.
const LEDGER_UTXO_KEY = inputCbor(h32("aa"), 0n).toString("hex");
const PHANTOM_LEDGER_KEY = inputCbor(PHANTOM_TX_ID, 0n).toString("hex");

describe("prepare-non-existent-input", () => {
  it("prepares the four submit-step artifacts for a phantom-input tx over the empty ledger", async () => {
    const output = await prepareNonExistentInputFromTransactions({
      headerHash: h28("aa"),
      transactions: [badTxPayload, otherTxPayload],
      badTxId: badTxPayload.nodeTxId,
    });

    expect(output.headerHash).toBe(h28("aa"));
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
      headerHash: h28("bb"),
      transactions: [badTxPayload],
    });
    expect(output.badTxId).toBe(badTxPayload.nodeTxId);
    expect(output.missingInput.tx_id).toBe(PHANTOM_TX_ID);
  });

  it("requires --bad-tx-id when the block has multiple transactions", async () => {
    await expect(
      prepareNonExistentInputFromTransactions({
        headerHash: h28("cc"),
        transactions: [badTxPayload, otherTxPayload],
      }),
    ).rejects.toThrow("specify --bad-tx-id");
  });

  it("rejects a --bad-input-index out of bounds", async () => {
    await expect(
      prepareNonExistentInputFromTransactions({
        headerHash: h28("dd"),
        transactions: [badTxPayload],
        badInputIndex: 5,
      }),
    ).rejects.toThrow("out of bounds");
  });

  it("rejects an expected transactions root that does not match the reconstruction", async () => {
    await expect(
      prepareNonExistentInputFromTransactions({
        headerHash: h28("ee"),
        transactions: [badTxPayload],
        expectedTransactionsRoot: h32("00"),
      }),
    ).rejects.toThrow("does not match --expected-transactions-root");
  });

  it("accepts the counted transactions root committed by the block header", async () => {
    const prepared = await prepareNonExistentInputFromTransactions({
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

    const verified = await prepareNonExistentInputFromTransactions({
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
      prepareNonExistentInputFromTransactions({
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

      const output = await prepareNonExistentInputFromFile({
        headerHash: h28("aa"),
        transactionsPath,
        badTxId: badTxPayload.nodeTxId,
        outputDir: dir,
      });

      expect(output.files?.txInclusionPath).toBe(
        join(dir, "ne-tx-inclusion.json"),
      );
      expect(output.files?.inputsPreimagePath).toBe(
        join(dir, "ne-inputs-preimage.json"),
      );
      expect(output.files?.ledgerNonMembershipPath).toBe(
        join(dir, "ne-ledger-non-membership.json"),
      );
      expect(output.files?.txsNonMembershipPath).toBe(
        join(dir, "ne-txs-non-membership.json"),
      );
      expect(output.files?.planPath).toBe(join(dir, "ne-plan.json"));

      const inclusion = JSON.parse(
        await readFile(join(dir, "ne-tx-inclusion.json"), "utf8"),
      ) as { readonly nativeTxId: string };
      expect(inclusion.nativeTxId).toBe(badTxPayload.nodeTxId);

      const inputsPreimage = JSON.parse(
        await readFile(join(dir, "ne-inputs-preimage.json"), "utf8"),
      ) as readonly { readonly txId: string; readonly index: string }[];
      expect(inputsPreimage).toEqual([{ txId: PHANTOM_TX_ID, index: "0" }]);

      const plan = JSON.parse(
        await readFile(join(dir, "ne-plan.json"), "utf8"),
      ) as { readonly badTxId: string; readonly badInputIndex: number };
      expect(plan).toMatchObject({
        badTxId: badTxPayload.nodeTxId,
        badInputIndex: 0,
      });
    });
  });

  it("fetches node block transactions before preparing non-existent-input material", async () => {
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

    const output = await prepareNonExistentInputFromNode({
      midgardNodeUrl: "http://node.local/",
      headerHash: h28("aa"),
      fetchImpl,
    });

    expect(output.badTxId).toBe(badTxPayload.nodeTxId);
    expect(output.missingInput.tx_id).toBe(PHANTOM_TX_ID);
  });

  it("reconstructs the ledger from the previous block's DA payload for a non-first block", async () => {
    const { payloadEnvelopeCbor, utxosRoot } = await buildPrevBlockPayload([
      [LEDGER_UTXO_KEY, LEDGER_OUTPUT_CBOR],
    ]);
    expect(utxosRoot).not.toBe(EMPTY_MERKLE_TREE_ROOT);

    const output = await prepareNonExistentInputFromTransactions({
      headerHash: h28("aa"),
      transactions: [badTxPayload],
      prevUtxosRoot: utxosRoot,
      prevBlockPayloadEnvelopeCbor: payloadEnvelopeCbor,
    });

    expect(output.prevUtxosRoot).toBe(utxosRoot);
    expect(output.missingInput.tx_id).toBe(PHANTOM_TX_ID);
    // A real ledger non-membership proof against the reconstructed trie.
    expect(output.ledgerNonMembershipProofCbor.length).toBeGreaterThan(0);
  });

  it("rejects a prev-block payload whose utxos_root does not match --prev-utxos-root", async () => {
    const { payloadEnvelopeCbor } = await buildPrevBlockPayload([
      [LEDGER_UTXO_KEY, LEDGER_OUTPUT_CBOR],
    ]);
    await expect(
      prepareNonExistentInputFromTransactions({
        headerHash: h28("aa"),
        transactions: [badTxPayload],
        prevUtxosRoot: h32("bb"),
        prevBlockPayloadEnvelopeCbor: payloadEnvelopeCbor,
      }),
    ).rejects.toThrow("does not match --prev-utxos-root");
  });

  it("refuses to prove non-membership when the input is present in the reconstructed ledger", async () => {
    const { payloadEnvelopeCbor, utxosRoot } = await buildPrevBlockPayload([
      [LEDGER_UTXO_KEY, LEDGER_OUTPUT_CBOR],
      [PHANTOM_LEDGER_KEY, LEDGER_OUTPUT_CBOR],
    ]);
    await expect(
      prepareNonExistentInputFromTransactions({
        headerHash: h28("aa"),
        transactions: [badTxPayload],
        prevUtxosRoot: utxosRoot,
        prevBlockPayloadEnvelopeCbor: payloadEnvelopeCbor,
      }),
    ).rejects.toThrow(/present key/i);
  });
});
