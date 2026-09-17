import { createHash } from "node:crypto";

import {
  decodeMidgardNativeByteListPreimage,
  decodeMidgardNativeTxFullFromCanonicalCbor,
  encodeMidgardSpendInputItem,
} from "@al-ft/midgard-core/codec";
import { hexToBytes } from "@al-ft/midgard-core/hex";
import * as SDK from "@al-ft/midgard-sdk";

import {
  encodeTransactionRootValue,
  type MpfInsertBatchOp,
  type TransitionTraceSourceEvent,
} from "../mpf/index.js";
import { parseOpenLoopCorpusLine } from "../open-loop-corpus-format.js";

export const canonicalOutrefCborFromLabel = (label: string): Buffer => {
  const match = /^([0-9a-f]{64})#(0|[1-9]\d*)$/u.exec(label.toLowerCase());
  if (match === null) {
    throw new Error(`Invalid canonical corpus outref ${label}`);
  }
  // The §5.3 field-0/1 item encoding — `82 ‖ 58 20 tx_id(32) ‖ 19 index_be16`,
  // fixed 38 bytes — matching on-chain `ledger_outref_key`, not CML's
  // minimal-index `TransactionInput` CBOR.
  return encodeMidgardSpendInputItem({
    txId: hexToBytes(match[1]!, { fieldName: "corpus outref txHash" }),
    outputIndex: Number(match[2]!),
  });
};

export type CanonicalProbeRow = {
  readonly txHash: string;
  readonly cbor: Buffer;
  readonly selectedInputOutref: string;
  readonly parentTxHash: string | null;
  readonly transactionOp: MpfInsertBatchOp;
  readonly sourceEvent: TransitionTraceSourceEvent;
};

export const decodeCanonicalProbeRow = (
  value: Record<string, unknown>,
  index: number,
): CanonicalProbeRow => {
  const exact = parseOpenLoopCorpusLine(JSON.stringify(value), index + 1);
  const txHash = exact.txHash;
  const canonicalCborHex = exact.canonicalCborHex;
  const canonicalCborSha256 = exact.canonicalCborSha256;
  const cbor = Buffer.from(canonicalCborHex, "hex");
  if (
    createHash("sha256").update(cbor).digest("hex") !== canonicalCborSha256 ||
    exact.canonicalCborByteLength !== cbor.length
  ) {
    throw new Error(
      `Canonical corpus slice row ${(index + 1).toString()} failed CBOR SHA/length verification`,
    );
  }
  const selectedInputOutref = exact.selectedInputOutref;
  const selectedInput = canonicalOutrefCborFromLabel(selectedInputOutref);
  const native = decodeMidgardNativeTxFullFromCanonicalCbor(cbor);
  const spendInputs = decodeMidgardNativeByteListPreimage(
    native.body.spendInputsPreimageCbor,
    "native.spend_inputs",
  );
  if (
    spendInputs.length !== 1 ||
    !Buffer.from(spendInputs[0]!).equals(selectedInput)
  ) {
    throw new Error(
      `Canonical corpus slice row ${(index + 1).toString()} does not spend its declared selected input`,
    );
  }
  const outputs = decodeMidgardNativeByteListPreimage(
    native.body.outputsPreimageCbor,
    "native.outputs",
  ).map((output) => Buffer.from(output));
  const outputOutrefs = exact.outputOutrefs;
  const expectedOutputOutrefs = outputs.map(
    (_output, outputIndex) => `${txHash}#${outputIndex.toString()}`,
  );
  if (
    outputOutrefs.length !== expectedOutputOutrefs.length ||
    outputOutrefs.some(
      (outref, outputIndex) => outref !== expectedOutputOutrefs[outputIndex],
    )
  ) {
    throw new Error(
      `Canonical corpus slice row ${(index + 1).toString()} output outrefs do not match decoded canonical outputs`,
    );
  }
  const parentTxHash = exact.parentTxHash;
  return {
    txHash,
    cbor,
    selectedInputOutref,
    parentTxHash,
    transactionOp: {
      type: "insert",
      key: Buffer.from(txHash, "hex"),
      value: encodeTransactionRootValue(cbor),
    },
    sourceEvent: {
      phase: "L2Transaction",
      eventKey: {
        L2TransactionEventKey: { tx_id: txHash },
      } as SDK.EventKey,
      ledgerOps: [
        { type: "delete", key: selectedInput },
        ...outputs.map((output, outputIndex) => ({
          type: "insert" as const,
          key: canonicalOutrefCborFromLabel(
            expectedOutputOutrefs[outputIndex]!,
          ),
          value: output,
        })),
      ],
    },
  };
};
