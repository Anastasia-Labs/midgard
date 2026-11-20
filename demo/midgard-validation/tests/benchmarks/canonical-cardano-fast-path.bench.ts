import { createHash } from "node:crypto";

import { CML } from "@lucid-evolution/lucid";
import { bench, describe } from "vitest";

import { decodeCanonicalTransactionInput } from "../../src/ledger-tx/canonical-cardano-fast-path.js";

const SAMPLE_COUNT = 256;

const outRefs = Array.from({ length: SAMPLE_COUNT }, (_, sample) => {
  const txId = createHash("sha256")
    .update(`decode-bench-outref-${sample.toString()}`)
    .digest();
  const hash = CML.TransactionHash.from_raw_bytes(txId);
  const input = CML.TransactionInput.new(hash, BigInt(sample * 257));
  try {
    return Buffer.from(input.to_cbor_bytes());
  } finally {
    input.free();
    hash.free();
  }
});

describe("canonical Cardano decode fast path", () => {
  bench("strict TransactionInput reader x256", () => {
    for (const bytes of outRefs) {
      if (decodeCanonicalTransactionInput(bytes) === undefined) {
        throw new Error("canonical input missed fast path");
      }
    }
  });

  bench("CML TransactionInput decoder x256", () => {
    for (const bytes of outRefs) {
      const input = CML.TransactionInput.from_cbor_bytes(bytes);
      const txId = input.transaction_id();
      try {
        txId.to_raw_bytes();
        input.index();
      } finally {
        txId.free();
        input.free();
      }
    }
  });
});
