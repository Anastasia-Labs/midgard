import { createHash } from "node:crypto";
import { type Server } from "node:http";
import type { Duplex } from "node:stream";

import { CML } from "@lucid-evolution/lucid";

import {
  type DecodedTransaction,
  outRefsOf,
  transactionMint,
  transactionRedeemers,
} from "./local-l1-observation.websocket-guid.js";

/**
 * Ogmios's JSON view of a real transaction, derived from its bytes.
 *
 * **Reference inputs are emitted in the transaction's own CBOR order and are not
 * sorted here, deliberately.** A real Ogmios emits them in the ledger's order —
 * `reference_inputs` is a `Set TxIn`, so what a validator is handed is
 * `(txHash, outputIndex)` ascending — while a builder writes them into the CBOR
 * in whatever order it collected them, which for the Lucid builder is insertion
 * order. Serving the unsorted view is the *harder* of the two for the reader, and
 * it is what makes the reader's own canonical sort testable: a reader that took
 * the wire order as given would address the wrong inputs and fail the §4
 * commitment check.
 */
export const decodeTransaction = (cborHex: string): DecodedTransaction => {
  const transaction = CML.Transaction.from_cbor_hex(cborHex);
  const body = transaction.body();
  const txHash = CML.hash_transaction(body).to_hex();
  const outputs = body.outputs();
  const outputViews: { address: string; datum: string | null }[] = [];
  const outputsJson: unknown[] = [];
  for (let index = 0; index < outputs.len(); index += 1) {
    const output = outputs.get(index);
    const address = output.address().to_bech32(undefined);
    const datum = output.datum()?.as_datum()?.to_cbor_hex() ?? null;
    outputViews.push({ address, datum });
    outputsJson.push({
      address,
      value: { ada: { lovelace: output.amount().coin().toString() } },
      ...(datum === null ? {} : { datum }),
    });
  }
  const inputs = outRefsOf(body.inputs());
  return {
    txHash,
    outputs: outputViews,
    inputs,
    json: {
      id: txHash,
      spends: "inputs",
      inputs,
      references: outRefsOf(body.reference_inputs()),
      outputs: outputsJson,
      mint: transactionMint(body),
      redeemers: transactionRedeemers(transaction.witness_set()),
      signatories: [],
      fee: { ada: { lovelace: body.fee().toString() } },
    },
  };
};

export const headerHashOf = (
  slot: number,
  txHashes: readonly string[],
): string =>
  createHash("sha256")
    .update(`local-l1-block:${slot.toString()}:${txHashes.join(",")}`)
    .digest("hex");

export const encodeWebSocketFrame = (text: string): Buffer => {
  const payload = Buffer.from(text, "utf8");
  if (payload.length < 126) {
    return Buffer.concat([Buffer.from([0x81, payload.length]), payload]);
  }
  if (payload.length < 65_536) {
    const header = Buffer.alloc(4);
    header[0] = 0x81;
    header[1] = 126;
    header.writeUInt16BE(payload.length, 2);
    return Buffer.concat([header, payload]);
  }
  const header = Buffer.alloc(10);
  header[0] = 0x81;
  header[1] = 127;
  header.writeBigUInt64BE(BigInt(payload.length), 2);
  return Buffer.concat([header, payload]);
};

export const readWebSocketFrames = (
  socket: Duplex,
  onText: (text: string) => void,
): void => {
  let buffered = Buffer.alloc(0);
  socket.on("data", (chunk: Buffer) => {
    buffered = Buffer.concat([buffered, chunk]);
    for (;;) {
      if (buffered.length < 2) {
        return;
      }
      const opcode = buffered[0]! & 0x0f;
      const masked = (buffered[1]! & 0x80) !== 0;
      let length = buffered[1]! & 0x7f;
      let offset = 2;
      if (length === 126) {
        if (buffered.length < 4) {
          return;
        }
        length = buffered.readUInt16BE(2);
        offset = 4;
      } else if (length === 127) {
        if (buffered.length < 10) {
          return;
        }
        length = Number(buffered.readBigUInt64BE(2));
        offset = 10;
      }
      const maskOffset = offset;
      if (masked) {
        offset += 4;
      }
      if (buffered.length < offset + length) {
        return;
      }
      const payload = Buffer.from(buffered.subarray(offset, offset + length));
      if (masked) {
        const mask = buffered.subarray(maskOffset, maskOffset + 4);
        for (let index = 0; index < payload.length; index += 1) {
          payload[index] ^= mask[index % 4]!;
        }
      }
      buffered = buffered.subarray(offset + length);
      if (opcode === 0x8) {
        socket.end();
        return;
      }
      if (opcode === 0x1) {
        onText(payload.toString("utf8"));
      }
    }
  });
};

export const listen = async (server: Server): Promise<number> => {
  await new Promise<void>((resolve) => {
    server.listen(0, "127.0.0.1", resolve);
  });
  const address = server.address();
  if (address === null || typeof address === "string") {
    throw new Error("local L1 server did not bind a port");
  }
  return address.port;
};
