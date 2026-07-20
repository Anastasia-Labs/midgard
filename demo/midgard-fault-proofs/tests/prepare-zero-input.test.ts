import { mkdtemp, readFile, rm } from "node:fs/promises";
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
import { EMPTY_SPEND_INPUTS_HASH } from "@al-ft/midgard-sdk";
import { CML } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  type NodeTransactionPayload,
  prepareZeroInputFromTransactions,
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
  spendInputCbors,
  fee,
}: {
  readonly spendInputCbors: readonly Buffer[];
  readonly fee: bigint;
}): MidgardNativeTxFull =>
  materializeMidgardNativeTxFromCanonical({
    version: MIDGARD_NATIVE_TX_VERSION,
    validity: "TxIsValid",
    body: {
      spendInputsPreimageCbor: encodeCbor(spendInputCbors),
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

const spendingTx = (fee: bigint): NodeTransactionPayload =>
  payloadFromTx(
    makeNativeTx({ spendInputCbors: [inputCbor(h32("11"), fee)], fee }),
  );

const zeroInputTx = (fee: bigint): NodeTransactionPayload =>
  payloadFromTx(makeNativeTx({ spendInputCbors: [], fee }));

const withTempDir = async <A>(run: (dir: string) => Promise<A>): Promise<A> => {
  const dir = await mkdtemp(join(tmpdir(), "midgard-zero-input-"));
  try {
    return await run(dir);
  } finally {
    await rm(dir, { recursive: true, force: true });
  }
};

describe("prepare-zero-input", () => {
  it("selects the transaction that spends no inputs", async () => {
    const validTx = spendingTx(1n);
    const badTx = zeroInputTx(2n);

    const output = await prepareZeroInputFromTransactions({
      headerHash: h28("aa"),
      transactions: [validTx, badTx],
    });

    expect(output.headerHash).toBe(h28("aa"));
    expect(output.txCount).toBe(2);
    expect(output.tx.nodeTxId).toBe(badTx.nodeTxId);
    expect(output.tx.spendInputsHash).toBe(EMPTY_SPEND_INPUTS_HASH);
    expect(output.tx.nativeTx.body.spend_inputs_hash).toBe(
      EMPTY_SPEND_INPUTS_HASH,
    );
    expect(output.tx.txInclusion.nativeTxId).toBe(badTx.nodeTxId);
    expect(output.tx.txInclusion.txMembershipProofCbor.length).toBeGreaterThan(
      0,
    );
    expect(output.compatibility.canUseSubmitStepCommands).toBe(true);
  });

  it("throws when every transaction in the block spends at least one input", async () => {
    await expect(
      prepareZeroInputFromTransactions({
        headerHash: h28("aa"),
        transactions: [spendingTx(1n), spendingTx(2n)],
      }),
    ).rejects.toThrow("No zero-input transaction found in the selected block.");
  });

  it("rejects an explicitly requested tx that does spend inputs", async () => {
    const validTx = spendingTx(1n);
    await expect(
      prepareZeroInputFromTransactions({
        headerHash: h28("aa"),
        transactions: [validTx, zeroInputTx(2n)],
        txId: validTx.nodeTxId,
      }),
    ).rejects.toThrow("spends at least one input");
  });

  it("writes tx-inclusion and plan artifacts the submit steps consume", async () => {
    const badTx = zeroInputTx(2n);
    await withTempDir(async (dir) => {
      const output = await prepareZeroInputFromTransactions({
        headerHash: h28("aa"),
        transactions: [spendingTx(1n), badTx],
        outputDir: dir,
      });

      expect(output.files?.txInclusionPath).toBe(join(dir, "tx-inclusion.json"));
      const txInclusion = JSON.parse(
        await readFile(output.files!.txInclusionPath, "utf8"),
      ) as { readonly nativeTxId: string };
      expect(txInclusion.nativeTxId).toBe(badTx.nodeTxId);

      const plan = JSON.parse(
        await readFile(output.files!.planPath, "utf8"),
      ) as { readonly spendInputsHash: string };
      expect(plan.spendInputsHash).toBe(EMPTY_SPEND_INPUTS_HASH);
    });
  });
});
