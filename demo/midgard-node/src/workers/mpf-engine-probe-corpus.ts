import { createHash } from "node:crypto";

import {
  decodeMidgardNativeByteListPreimage,
  decodeMidgardNativeTxFullFromCanonicalCbor,
} from "@al-ft/midgard-core/codec";
import * as SDK from "@al-ft/midgard-sdk";
import { CML } from "@lucid-evolution/lucid";

import {
  encodeTransactionRootValue,
  type MpfInsertBatchOp,
  type TransitionTraceSourceEvent,
} from "@/workers/utils/mpf.js";

export const canonicalOutrefCborFromLabel = (label: string): Buffer => {
  const match = /^([0-9a-f]{64})#(0|[1-9]\d*)$/u.exec(label.toLowerCase());
  if (match === null) {
    throw new Error(`Invalid canonical corpus outref ${label}`);
  }
  return Buffer.from(
    CML.TransactionInput.new(
      CML.TransactionHash.from_hex(match[1]!),
      BigInt(match[2]!),
    ).to_cbor_bytes(),
  );
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
  const txHash = String(value.txHash ?? "").toLowerCase();
  const canonicalCborHex = String(value.canonicalCborHex ?? "").toLowerCase();
  const canonicalCborSha256 = String(
    value.canonicalCborSha256 ?? "",
  ).toLowerCase();
  const cbor = Buffer.from(canonicalCborHex, "hex");
  if (!/^[0-9a-f]{64}$/u.test(txHash)) {
    throw new Error(
      `Canonical corpus slice row ${(index + 1).toString()} has an invalid txHash`,
    );
  }
  if (
    canonicalCborHex.length === 0 ||
    canonicalCborHex.length % 2 !== 0 ||
    cbor.toString("hex") !== canonicalCborHex
  ) {
    throw new Error(
      `Canonical corpus slice row ${(index + 1).toString()} has invalid canonical CBOR`,
    );
  }
  if (
    !/^[0-9a-f]{64}$/u.test(canonicalCborSha256) ||
    createHash("sha256").update(cbor).digest("hex") !== canonicalCborSha256 ||
    value.canonicalCborByteLength !== cbor.length
  ) {
    throw new Error(
      `Canonical corpus slice row ${(index + 1).toString()} failed CBOR SHA/length verification`,
    );
  }
  const selectedInputOutref = String(
    value.selectedInputOutref ?? "",
  ).toLowerCase();
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
  const outputOutrefs = Array.isArray(value.outputOutrefs)
    ? value.outputOutrefs.map((outref) => String(outref).toLowerCase())
    : [];
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
  const parentTxHash =
    value.parentTxHash === null
      ? null
      : String(value.parentTxHash ?? "").toLowerCase();
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
