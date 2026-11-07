// The checks `/submit` and `/peer/txs` run on a tx before durable admission
// (`checkSubmittedTransaction`): the refusal for every path in front of the
// writer, and the facts an admitted tx carries so no later stage decodes it.
import {
  computeMidgardNativeTxFullHashFromCanonicalCbor,
  decodeMidgardNativeTxFullFromCanonicalCbor,
  encodeMidgardNativeTxCanonical,
  encodeMidgardTxOutput,
  materializeMidgardNativeTxFromCanonical,
  type MidgardNativeTxCanonical,
} from "@al-ft/midgard-core/codec";
import { encodeCbor } from "@al-ft/midgard-core/codec/cbor";
import { describe, expect, it } from "vitest";

import {
  buildLoadTxShape,
  loadOutRef,
  makeLoadKey,
} from "../scripts/submit-ingress-load.tx.mjs";
import { checkSubmittedTransaction } from "../src/commands/http/transaction-admission.js";
import { makeCardanoSignedMapOutputTxBytes } from "./helpers/cardano-native-fixtures.js";

// Re-encodes a canonical tx with some body or witness fields replaced.
const withNativeFields = (
  txCbor: Buffer,
  change: (tx: MidgardNativeTxCanonical) => MidgardNativeTxCanonical,
): Buffer => {
  const decoded = decodeMidgardNativeTxFullFromCanonicalCbor(txCbor);
  return encodeMidgardNativeTxCanonical(
    materializeMidgardNativeTxFromCanonical(
      change({
        version: decoded.version,
        validity: decoded.validity,
        body: decoded.body,
        witnessSet: decoded.witnessSet,
      }),
    ),
  );
};

describe("checkSubmittedTransaction", () => {
  const valid = buildLoadTxShape({ inputCount: 2, outputCount: 3 });
  const maxBytes = 32_768;

  // One vector per refusal path in front of durable admission. Each expected
  // result is the one the check returned before it decoded once; the
  // single-decode check must keep every status, body and log line.
  it.each([
    {
      name: "an empty body",
      bytes: () => Buffer.alloc(0),
      expected: {
        ok: false,
        status: 400,
        body: { error: "Invalid canonical transaction CBOR payload" },
        log: ["Submit rejected: Invalid canonical transaction CBOR payload"],
      },
    },
    {
      name: "a body over the size cap",
      bytes: () => Buffer.alloc(maxBytes + 1, 0x80),
      expected: {
        ok: false,
        status: 413,
        body: {
          error: `Canonical transaction CBOR exceeds max size (${(maxBytes + 1).toString()} > ${maxBytes.toString()})`,
        },
        log: [
          `Submit rejected: Canonical transaction CBOR exceeds max size (${(maxBytes + 1).toString()} > ${maxBytes.toString()})`,
        ],
      },
    },
    {
      name: "bytes that are not CBOR",
      bytes: () => Buffer.from("ffff", "hex"),
      expected: {
        ok: false,
        status: 400,
        body: { error: "Invalid canonical transaction CBOR payload" },
        log: [
          "Invalid canonical transaction CBOR payload",
          "canonical native transaction decode failed",
        ],
      },
    },
    {
      name: "a Cardano-signed tx",
      bytes: () => makeCardanoSignedMapOutputTxBytes(),
      expected: {
        ok: false,
        status: 400,
        body: { error: "Invalid canonical transaction CBOR payload" },
        log: [
          "Invalid canonical transaction CBOR payload",
          "canonical native transaction decode failed",
        ],
      },
    },
    {
      name: "a native tx whose address witness is not a vkey witness",
      bytes: () =>
        withNativeFields(valid.txCbor, (tx) => ({
          ...tx,
          witnessSet: {
            ...tx.witnessSet,
            addrTxWitsPreimageCbor: encodeCbor([Buffer.from("00", "hex")]),
          },
        })),
      expected: {
        ok: false,
        status: 400,
        body: { error: "Invalid canonical transaction CBOR payload" },
        log: [
          "Invalid canonical transaction CBOR payload",
          "canonical native transaction decode failed",
        ],
      },
    },
    {
      name: "a native tx committing auxiliary data",
      bytes: () =>
        withNativeFields(valid.txCbor, (tx) => ({
          ...tx,
          body: { ...tx.body, auxiliaryDataHash: Buffer.alloc(32, 0x0a) },
        })),
      expected: {
        ok: false,
        status: 400,
        body: {
          error: "E_AUX_DATA_FORBIDDEN",
          feature: "auxiliary_data",
          detail: "V1 has no authenticated auxiliary-data preimage",
        },
        log: [],
      },
    },
    {
      name: "a native tx with an oversized output value",
      bytes: () =>
        withNativeFields(valid.txCbor, (tx) => ({
          ...tx,
          body: {
            ...tx.body,
            outputsPreimageCbor: encodeCbor([
              encodeMidgardTxOutput({
                address: makeLoadKey("value-size", 0).addressBytes,
                value: {
                  lovelace: 2_000_000n,
                  assets: new Map([
                    [
                      "ab".repeat(28),
                      new Map(
                        Array.from({ length: 160 }, (_, index) => [
                          index.toString(16).padStart(64, "0"),
                          1n,
                        ]),
                      ),
                    ],
                  ]),
                },
              }),
            ]),
          },
        })),
      expected: {
        ok: false,
        status: 400,
        body: {
          error: "E_VALUE_SIZE",
          feature: "output_value",
          detail: "output[0] Cardano Value 5639 > 5000",
        },
        log: [],
      },
    },
  ])("refuses $name exactly as before", ({ bytes, expected }) => {
    expect(checkSubmittedTransaction(bytes(), maxBytes)).toEqual(expected);
  });

  it("admits a valid native tx with every fact admission needs", () => {
    const checked = checkSubmittedTransaction(valid.txCbor, maxBytes);
    expect(checked).toEqual({
      ok: true,
      normalized: {
        txId: valid.txId,
        txIdHex: valid.txId.toString("hex"),
        txCanonicalCbor: valid.txCbor,
        txFullHash: computeMidgardNativeTxFullHashFromCanonicalCbor(
          valid.txCbor,
        ),
        spentOutRefs: valid.inputs
          .map((input) => input.outRef)
          .sort(Buffer.compare)
          .map((outRef) => outRef.toString("hex")),
        producedOutRefs: [0, 1, 2].map((index) =>
          loadOutRef(valid.txId, index).toString("hex"),
        ),
        fee: valid.fee,
        byteLength: valid.txCbor.length,
        source: "native",
      },
    });
  });
});
