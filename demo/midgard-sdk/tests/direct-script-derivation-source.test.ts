import { computeHash32 } from "@al-ft/midgard-core";
import { Data } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import { DirectScriptSourceBound } from "../src/fraud-proof/direct-script-derivation-source.js";

const bytes = (tag: number, length: number): string =>
  tag.toString(16).padStart(length * 2, "0");

/** Every field carries a distinct value, so any field reorder changes the
 * encoding. The same value and digest are pinned on-chain in
 * onchain/aiken/lib/midgard/fraud-proofs/direct-script-derivation/types.test.ak. */
const sourceBound = (): DirectScriptSourceBound => ({
  subject: {
    version: 1n,
    direction: 2n,
    source_kind: 3n,
    transaction_id: bytes(4, 32),
    source_key: bytes(5, 32),
    rejection_reason: null,
  },
  header: {
    prevUtxosRoot: bytes(11, 32),
    utxosRoot: bytes(12, 32),
    withdrawalsRoot: bytes(13, 32),
    forcedTransactionsRoot: bytes(14, 32),
    transactionsRoot: bytes(15, 32),
    depositsRoot: bytes(16, 32),
    transitionTraceRoot: bytes(17, 32),
    eventToStepRoot: bytes(18, 32),
    validationTracesRoot: bytes(19, 32),
    withdrawalCount: 20n,
    forcedTransactionCount: 21n,
    l2TransactionCount: 22n,
    depositCount: 23n,
    totalEventCount: 24n,
    transitionStepCount: 25n,
    validationTraceCount: 26n,
    startTime: 27n,
    endTime: 28n,
    blockSlot: 29n,
    expectedNetworkId: 30n,
    minFeeA: 31n,
    minFeeB: 32n,
    prevHeaderHash: bytes(33, 28),
    operatorVkey: bytes(34, 28),
    protocolVersion: 35n,
  },
  compact_cbor: bytes(36, 3),
});

describe("direct script derivation SourceBoundV1 wire twin", () => {
  it("encodes the carried source bound exactly as on-chain", () => {
    const cbor = Data.to(sourceBound(), DirectScriptSourceBound);
    expect(computeHash32(Buffer.from(cbor, "hex")).toString("hex")).toBe(
      "476753a9cdb14f3b018240c362ee53a5cc14a42143c526b2bace36fe0e062aed",
    );
    expect(Data.from(cbor, DirectScriptSourceBound)).toStrictEqual(
      sourceBound(),
    );
  });
});
