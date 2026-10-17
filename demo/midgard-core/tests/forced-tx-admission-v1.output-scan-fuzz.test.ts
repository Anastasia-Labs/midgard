import { describe, expect, it } from "vitest";

import { encodeCbor } from "../src/codec/cbor.js";
import { EMPTY_NULL_ROOT } from "../src/codec/native-constants.js";
import { encodeMidgardNativeScript } from "../src/codec/native-script.js";
import { encodeMidgardSpendInputItem } from "../src/codec/native-tx-field-items.js";
import {
  decodeMidgardTxOutput,
  encodeMidgardTxOutput,
  type MidgardTxOutput,
} from "../src/codec/output.js";
import { screenMidgardForcedTxAdmission } from "../src/consensus-validation.screen-midgard-forced-tx-admission.js";
import { buildMidgardLedgerOutputScanTrace } from "../src/ledger-output-scan.build-midgard-ledger-output-scan-trace.js";

/**
 * The admission screen's output rule reads an output's keys by position
 * (`outputRefusal`, twin of the tx-order mint's `output_is_admissible`), not
 * through the output decoder. That is sound only because every output the
 * output scan (`buildMidgardLedgerOutputScanTrace`, twin of
 * `ledger_output_scan_v1`) accepts has its keys where the positional read
 * expects them. This differential fuzz pins that: on every seeded output,
 * mutated or not, that both the scan and `decodeMidgardTxOutput` accept, the
 * screen refuses exactly the outputs that carry a reference script.
 */

const SAMPLES = 6_000;

const random = (() => {
  let seed = 0x1234567;
  return () => {
    seed ^= seed << 13;
    seed >>>= 0;
    seed ^= seed >>> 17;
    seed ^= seed << 5;
    seed >>>= 0;
    return seed / 2 ** 32;
  };
})();
const below = (n: number) => Math.floor(random() * n);
const bytes = (n: number) =>
  Buffer.from(Array.from({ length: n }, () => below(256)));

const address = () =>
  below(2) === 1
    ? Buffer.concat([Buffer.from([0x60 | below(2)]), bytes(28)])
    : Buffer.concat([Buffer.from([(below(4) << 4) | below(2)]), bytes(56)]);

const value = (): MidgardTxOutput["value"] => {
  const policies = Array.from({ length: below(3) }, () => {
    const names = [
      ...new Set(
        Array.from({ length: 1 + below(3) }, () =>
          bytes(below(5)).toString("hex"),
        ),
      ),
    ].sort();
    return [
      bytes(28).toString("hex"),
      new Map(names.map((name) => [name, BigInt(1 + below(1000))])),
    ] as const;
  });
  return {
    lovelace: BigInt(below(2 ** 30)),
    assets: new Map(
      policies.sort(([left], [right]) => (left < right ? -1 : 1)),
    ),
  };
};

const DATUMS = ["d8799f00ff", "00", "4100", "d87980", "9f0102ff", "a10102"];

const output = (): MidgardTxOutput => ({
  address: address(),
  value: value(),
  ...(below(2) === 1
    ? {
        datum: {
          kind: "inline" as const,
          cbor: Buffer.from(DATUMS[below(DATUMS.length)]!, "hex"),
        },
      }
    : {}),
  ...(below(3) === 0
    ? {
        script_ref: {
          language: "NativeCardano" as const,
          scriptBytes: encodeMidgardNativeScript({
            type: "sig",
            keyHash: bytes(28),
          }),
          nativeScript: { type: "sig" as const, keyHash: Buffer.alloc(28) },
        },
      }
    : {}),
});

const mutate = (original: Buffer): Buffer => {
  const out = Buffer.from(original);
  const at = below(out.length);
  switch (below(5)) {
    case 0:
      out[at] = out[at]! ^ (1 << below(8));
      return out;
    case 1:
      out[at] = below(256);
      return out;
    case 2:
      return Buffer.concat([
        out.subarray(0, at),
        Buffer.from([below(256)]),
        out.subarray(at),
      ]);
    case 3:
      return Buffer.concat([out.subarray(0, at), out.subarray(at + 1)]);
    default:
      return out.subarray(0, below(out.length + 1));
  }
};

const accepts = (run: () => unknown): boolean => {
  try {
    run();
    return true;
  } catch {
    return false;
  }
};

const empty = Buffer.from([0x80]);
const screenedOutputs = (outputBytes: Buffer) =>
  screenMidgardForcedTxAdmission({
    body: {
      spendInputsPreimageCbor: encodeCbor([
        encodeMidgardSpendInputItem({
          txId: Buffer.alloc(32, 0xaa),
          outputIndex: 3,
        }),
      ]),
      referenceInputsPreimageCbor: empty,
      outputsPreimageCbor: encodeCbor([outputBytes]),
      validityIntervalEnd: -1n,
      requiredObserversPreimageCbor: empty,
      requiredSignersPreimageCbor: empty,
      mintPreimageCbor: empty,
      scriptIntegrityHash: EMPTY_NULL_ROOT,
      auxiliaryDataHash: EMPTY_NULL_ROOT,
      networkId: 0n,
    },
    witnessSet: {
      addrTxWitsPreimageCbor: empty,
      scriptTxWitsPreimageCbor: empty,
      redeemerTxWitsPreimageCbor: empty,
    },
  });

describe("forced-tx admission: positional output read against the output scan", () => {
  it("refuses exactly the scan-accepted outputs that carry a reference script", () => {
    let compared = 0;
    let withScript = 0;
    for (let sample = 0; sample < SAMPLES; sample += 1) {
      let encoded: Buffer;
      try {
        encoded = encodeMidgardTxOutput(output());
      } catch {
        continue;
      }
      const candidate = sample % 5 === 0 ? encoded : mutate(encoded);
      if (!accepts(() => buildMidgardLedgerOutputScanTrace(candidate))) {
        continue;
      }
      let decoded: MidgardTxOutput;
      try {
        decoded = decodeMidgardTxOutput(candidate);
      } catch {
        continue;
      }
      compared += 1;
      const carriesScript = decoded.script_ref !== undefined;
      if (carriesScript) withScript += 1;
      const refusal = screenedOutputs(candidate);
      expect({
        output: candidate.toString("hex"),
        refused: refusal !== null,
      }).toEqual({ output: candidate.toString("hex"), refused: carriesScript });
    }
    // Both verdicts are exercised, on a meaningful share of the samples.
    expect(compared).toBeGreaterThan(SAMPLES / 4);
    expect(withScript).toBeGreaterThan(compared / 10);
    expect(compared - withScript).toBeGreaterThan(compared / 10);
  });
});
