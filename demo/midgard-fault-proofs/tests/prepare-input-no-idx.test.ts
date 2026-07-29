import { mkdtemp, readFile, rm, writeFile } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";

import {
  computeHash32,
  computeMidgardNativeTxId,
  encodeCbor,
  encodeMidgardNativeTxCanonical,
  materializeMidgardNativeTxFromCanonical,
  MIDGARD_NATIVE_TX_VERSION,
  MIDGARD_POSIX_TIME_NONE,
  type MidgardNativeTxFull,
} from "@al-ft/midgard-core";
import * as SDK from "@al-ft/midgard-sdk";
import { CML } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import {
  type NodeTransactionPayload,
  prepareInputNoIdxFromFile,
  prepareInputNoIdxFromNode,
  prepareInputNoIdxFromTransactions,
} from "../src/index.js";

const h28 = (byte: string): string => byte.repeat(28);
const h32 = (byte: string): string => byte.repeat(32);
const EMPTY_CBOR_LIST = encodeCbor([]);
const EMPTY_CBOR_NULL = encodeCbor(null);
const EMPTY_NULL_ROOT = computeHash32(EMPTY_CBOR_NULL);

const inputCbor = (txHash: string, outputIndex: bigint): Buffer =>
  Buffer.from(
    CML.TransactionInput.new(
      CML.TransactionHash.from_hex(txHash),
      outputIndex,
    ).to_cbor_bytes(),
  );

const makeNativeTx = ({
  spendInputs,
  outputs,
  fee,
}: {
  readonly spendInputs: readonly Buffer[];
  readonly outputs: readonly Buffer[];
  readonly fee: bigint;
}): MidgardNativeTxFull =>
  materializeMidgardNativeTxFromCanonical({
    version: MIDGARD_NATIVE_TX_VERSION,
    validity: "TxIsValid",
    body: {
      spendInputsPreimageCbor: encodeCbor(spendInputs),
      referenceInputsPreimageCbor: EMPTY_CBOR_LIST,
      outputsPreimageCbor: encodeCbor(outputs),
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

// A producing transaction with a single output (`h32("b4")`).
const PRODUCING_OUTPUT = Buffer.from(h32("b4"), "hex");
const producingTx = makeNativeTx({
  spendInputs: [inputCbor(h32("b1"), 0n)],
  outputs: [PRODUCING_OUTPUT],
  fee: 2n,
});
const producingTxPayload = payloadFromTx(producingTx);
const PRODUCING_TX_ID = producingTxPayload.nodeTxId;

// The bad transaction spends the producing tx's output at an out-of-range index
// (5 >= 1 output).
const BAD_INPUT_OUTPUT_INDEX = 5n;
const badTx = makeNativeTx({
  spendInputs: [inputCbor(PRODUCING_TX_ID, BAD_INPUT_OUTPUT_INDEX)],
  outputs: [Buffer.from(h32("e4"), "hex")],
  fee: 0n,
});
const badTxPayload = payloadFromTx(badTx);

// A tx spending a producing-tx output that IS in range (index 0 of 1 output).
const inRangeTx = makeNativeTx({
  spendInputs: [inputCbor(PRODUCING_TX_ID, 0n)],
  outputs: [Buffer.from(h32("f4"), "hex")],
  fee: 1n,
});
const inRangeTxPayload = payloadFromTx(inRangeTx);

// A tx spending an input whose producing tx is absent from the block.
const phantomTx = makeNativeTx({
  spendInputs: [inputCbor(h32("de"), 0n)],
  outputs: [Buffer.from(h32("d4"), "hex")],
  fee: 3n,
});
const phantomTxPayload = payloadFromTx(phantomTx);

const withTempDir = async <A>(run: (dir: string) => Promise<A>): Promise<A> => {
  const dir = await mkdtemp(join(tmpdir(), "midgard-input-no-idx-"));
  try {
    return await run(dir);
  } finally {
    await rm(dir, { recursive: true, force: true });
  }
};

describe("prepare-input-no-idx", () => {
  it("prepares the four submit-step artifacts for an out-of-range spend input", async () => {
    const output = await prepareInputNoIdxFromTransactions({
      headerHash: h28("aa"),
      transactions: [badTxPayload, producingTxPayload],
      badTxId: badTxPayload.nodeTxId,
    });

    expect(output.headerHash).toBe(h28("aa"));
    expect(output.txCount).toBe(2);
    expect(output.badTxId).toBe(badTxPayload.nodeTxId);
    expect(output.badInputsIndex).toBe(0);
    expect(output.badInput).toEqual({
      tx_id: PRODUCING_TX_ID,
      output_index: BAD_INPUT_OUTPUT_INDEX,
    });
    expect(output.producingTxId).toBe(PRODUCING_TX_ID);
    expect(output.producingTxOutputCount).toBe(1);
    expect(output.outputsPreimage).toEqual([h32("b4")]);
    expect(output.transactionsRoot).toMatch(/^[0-9a-f]{64}$/);
    expect(output.committedTransactionsRoot).toMatch(/^[0-9a-f]{64}$/);
    expect(output.committedTransactionsRoot).not.toBe(output.transactionsRoot);
    expect(output.badTxInclusion.nativeTxId).toBe(badTxPayload.nodeTxId);
    expect(output.badTxInclusion.txMembershipProofCbor.length).toBeGreaterThan(
      0,
    );
    expect(output.producingTxInclusion.nativeTxId).toBe(PRODUCING_TX_ID);
    expect(
      output.producingTxInclusion.txMembershipProofCbor.length,
    ).toBeGreaterThan(0);
    expect(output.inputsPreimage).toEqual([
      { txId: PRODUCING_TX_ID, index: BAD_INPUT_OUTPUT_INDEX },
    ]);
  });

  it("auto-detects the offending tx/input when no selection is given", async () => {
    const output = await prepareInputNoIdxFromTransactions({
      headerHash: h28("bb"),
      transactions: [inRangeTxPayload, badTxPayload, producingTxPayload],
    });
    expect(output.badTxId).toBe(badTxPayload.nodeTxId);
    expect(output.badInput.output_index).toBe(BAD_INPUT_OUTPUT_INDEX);
    expect(output.producingTxId).toBe(PRODUCING_TX_ID);
  });

  it("rejects an input whose index is within the producing tx's output range", async () => {
    await expect(
      prepareInputNoIdxFromTransactions({
        headerHash: h28("cc"),
        transactions: [inRangeTxPayload, producingTxPayload],
        badTxId: inRangeTxPayload.nodeTxId,
        badInputIndex: 0,
      }),
    ).rejects.toThrow("within range");
  });

  it("rejects an input whose producing tx is absent from the block", async () => {
    await expect(
      prepareInputNoIdxFromTransactions({
        headerHash: h28("dd"),
        transactions: [phantomTxPayload, producingTxPayload],
        badTxId: phantomTxPayload.nodeTxId,
        badInputIndex: 0,
      }),
    ).rejects.toThrow("non-existent-input fault");
  });

  it("throws when the block contains no out-of-range spend input", async () => {
    await expect(
      prepareInputNoIdxFromTransactions({
        headerHash: h28("ee"),
        transactions: [inRangeTxPayload, producingTxPayload],
      }),
    ).rejects.toThrow("No out-of-range spend input");
  });

  it("rejects an expected transactions root that does not match the reconstruction", async () => {
    await expect(
      prepareInputNoIdxFromTransactions({
        headerHash: h28("ff"),
        transactions: [badTxPayload, producingTxPayload],
        badTxId: badTxPayload.nodeTxId,
        expectedTransactionsRoot: h32("00"),
      }),
    ).rejects.toThrow("does not match --expected-transactions-root");
  });

  it("accepts the counted transactions root committed by the block header", async () => {
    const prepared = await prepareInputNoIdxFromTransactions({
      headerHash: h28("ef"),
      transactions: [badTxPayload, producingTxPayload],
      badTxId: badTxPayload.nodeTxId,
    });
    const committedRoot = await Effect.runPromise(
      SDK.commitCountedRootProgram({
        domain: SDK.ROOT_DOMAINS.transactions,
        phasRoot: prepared.transactionsRoot,
        count: 2n,
      }),
    );

    const verified = await prepareInputNoIdxFromTransactions({
      headerHash: h28("ef"),
      transactions: [badTxPayload, producingTxPayload],
      badTxId: badTxPayload.nodeTxId,
      expectedTransactionsRoot: committedRoot,
    });

    expect(verified.expectedTransactionsRoot).toEqual({
      value: committedRoot,
      matches: true,
    });
    expect(verified.committedTransactionsRoot).toBe(committedRoot);
  });

  it("writes deterministic submit-step files from an explicit transactions file", async () => {
    await withTempDir(async (dir) => {
      const transactionsPath = join(dir, "block-transactions.json");
      await writeFile(
        transactionsPath,
        JSON.stringify([badTxPayload, producingTxPayload]),
      );

      const output = await prepareInputNoIdxFromFile({
        headerHash: h28("aa"),
        transactionsPath,
        badTxId: badTxPayload.nodeTxId,
        outputDir: dir,
      });

      expect(output.files?.badTxInclusionPath).toBe(
        join(dir, "input-no-idx-bad-tx-inclusion.json"),
      );
      expect(output.files?.producingTxInclusionPath).toBe(
        join(dir, "input-no-idx-producing-tx-inclusion.json"),
      );
      expect(output.files?.outputsPreimagePath).toBe(
        join(dir, "input-no-idx-outputs-preimage.json"),
      );
      expect(output.files?.planPath).toBe(join(dir, "input-no-idx-plan.json"));

      const outputsPreimage = JSON.parse(
        await readFile(join(dir, "input-no-idx-outputs-preimage.json"), "utf8"),
      ) as readonly string[];
      expect(outputsPreimage).toEqual([h32("b4")]);

      const plan = JSON.parse(
        await readFile(join(dir, "input-no-idx-plan.json"), "utf8"),
      ) as {
        readonly badTxId: string;
        readonly badInputsIndex: number;
        readonly producingTxId: string;
      };
      expect(plan).toMatchObject({
        badTxId: badTxPayload.nodeTxId,
        badInputsIndex: 0,
        producingTxId: PRODUCING_TX_ID,
      });
    });
  });

  it("fetches node block transactions before preparing input-no-idx material", async () => {
    const fetchImpl = async (input: string | URL): Promise<Response> => {
      const url = new URL(String(input));
      if (url.pathname === "/block") {
        return new Response(
          JSON.stringify({
            hashes: [badTxPayload.nodeTxId, producingTxPayload.nodeTxId],
          }),
          { status: 200 },
        );
      }
      if (url.pathname === "/tx") {
        const txId = url.searchParams.get("tx_hash");
        const tx =
          txId === producingTxPayload.nodeTxId
            ? producingTxPayload.txCbor
            : badTxPayload.txCbor;
        return new Response(JSON.stringify({ tx }), { status: 200 });
      }
      return new Response("not found", { status: 404 });
    };

    const output = await prepareInputNoIdxFromNode({
      midgardNodeUrl: "http://node.local/",
      headerHash: h28("aa"),
      badTxId: badTxPayload.nodeTxId,
      fetchImpl,
    });

    expect(output.badTxId).toBe(badTxPayload.nodeTxId);
    expect(output.producingTxId).toBe(PRODUCING_TX_ID);
  });
});
