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
import { h28, h32 } from "@al-ft/midgard-test-support/hex";
import { describe, expect, it } from "vitest";

import type { NodeTransactionPayload } from "../src/prepare-double-spend.encode-l2-transaction-source-value.js";
import { prepareInvalidRangeFromTransactions } from "../src/prepare-invalid-range.js";

const EMPTY_CBOR_LIST = encodeCbor([]);
const EMPTY_CBOR_NULL = encodeCbor(null);
const EMPTY_NULL_ROOT = computeHash32(EMPTY_CBOR_NULL);

const inputCbor = (txHash: string, outputIndex: bigint): Buffer =>
  encodeMidgardSpendInputItem({
    txId: Buffer.from(txHash, "hex"),
    outputIndex: Number(outputIndex),
  });

const makeNativeTx = ({
  validityIntervalStart,
  validityIntervalEnd,
  fee,
}: {
  readonly validityIntervalStart: bigint;
  readonly validityIntervalEnd: bigint;
  readonly fee: bigint;
}): MidgardNativeTxFull =>
  materializeMidgardNativeTxFromCanonical({
    version: MIDGARD_NATIVE_TX_VERSION,
    validity: "TxIsValid",
    body: {
      spendInputsPreimageCbor: encodeCbor([inputCbor(h32(0x11), fee)]),
      referenceInputsPreimageCbor: EMPTY_CBOR_LIST,
      outputsPreimageCbor: EMPTY_CBOR_LIST,
      fee,
      validityIntervalStart,
      validityIntervalEnd,
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

describe("prepare-invalid-range", () => {
  it("prepares submit-step material for a transaction whose lower bound is after the committed block slot", async () => {
    const validTx = payloadFromTx(
      makeNativeTx({
        validityIntervalStart: 100n,
        validityIntervalEnd: 201n,
        fee: 1n,
      }),
    );
    const badTx = payloadFromTx(
      makeNativeTx({
        validityIntervalStart: 151n,
        validityIntervalEnd: 201n,
        fee: 2n,
      }),
    );

    const output = await prepareInvalidRangeFromTransactions({
      headerHash: h28(0xaa),
      transactions: [validTx, badTx],
      blockSlot: 150n,
    });

    expect(output.headerHash).toBe(h28(0xaa));
    expect(output.txCount).toBe(2);
    expect(output.tx.nodeTxId).toBe(badTx.nodeTxId);
    expect(output.blockSlot).toBe(150n);
    expect(output.tx.violationReason).toBe("starts-after-block-slot");
    expect(output.tx.normalizedValidityRange).toEqual({
      ClosedRange: { lower: 151n, upper: 200n },
    });
    expect(output.tx.txInclusion.nativeTxId).toBe(badTx.nodeTxId);
    expect(output.tx.txInclusion.txMembershipProofCbor.length).toBeGreaterThan(
      0,
    );
    expect(output.commitmentEncodings.nativeNode.transactionsRoot).toMatch(
      /^[0-9a-f]{64}$/,
    );
  });

  it("honors explicit tx selection and rejects a selected non-violating tx", async () => {
    const validTx = payloadFromTx(
      makeNativeTx({
        validityIntervalStart: 100n,
        validityIntervalEnd: 201n,
        fee: 1n,
      }),
    );
    const expiredTx = payloadFromTx(
      makeNativeTx({
        validityIntervalStart: 100n,
        validityIntervalEnd: 150n,
        fee: 2n,
      }),
    );

    await expect(
      prepareInvalidRangeFromTransactions({
        headerHash: h28(0xbb),
        transactions: [validTx, expiredTx],
        blockSlot: 150n,
        txId: validTx.nodeTxId,
      }),
    ).rejects.toThrow("is valid at --block-slot 150");

    const output = await prepareInvalidRangeFromTransactions({
      headerHash: h28(0xbb),
      transactions: [validTx, expiredTx],
      blockSlot: 150n,
      txId: expiredTx.nodeTxId,
    });
    expect(output.tx.nodeTxId).toBe(expiredTx.nodeTxId);
    expect(output.tx.violationReason).toBe("ends-before-block-slot");
  });

  it("fails closed when the expected V1 transactions root differs", async () => {
    const badTx = payloadFromTx(
      makeNativeTx({
        validityIntervalStart: 151n,
        validityIntervalEnd: 201n,
        fee: 8n,
      }),
    );
    await expect(
      prepareInvalidRangeFromTransactions({
        headerHash: h28(0xbc),
        expectedTransactionsRoot: h32(0x00),
        transactions: [badTx],
        blockSlot: 150n,
      }),
    ).rejects.toThrow("Expected V1 transactions root");
  });

  it("rejects blocks without invalid-range transactions", async () => {
    await expect(
      prepareInvalidRangeFromTransactions({
        headerHash: h28(0xcc),
        transactions: [
          payloadFromTx(
            makeNativeTx({
              validityIntervalStart: 100n,
              validityIntervalEnd: 201n,
              fee: 1n,
            }),
          ),
          payloadFromTx(
            makeNativeTx({
              validityIntervalStart: MIDGARD_POSIX_TIME_NONE,
              validityIntervalEnd: 151n,
              fee: 2n,
            }),
          ),
        ],
        blockSlot: 150n,
      }),
    ).rejects.toThrow("No transaction with a validity interval excluding");
  });
});
