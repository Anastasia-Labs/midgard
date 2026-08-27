import {
  encodeMidgardNativeScript,
  encodeMidgardVersionedScript,
} from "@al-ft/midgard-core";
import { Data } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  missingNativeScriptIsAbsentV1,
  MissingNativeScriptTxStep02State,
  missingNativeScriptTxStep02StateFromBadTxV1,
  MissingNativeScriptTxStep03State,
  missingNativeScriptTxStep03StateV1,
  MissingNativeScriptTxStep04State,
  missingNativeScriptTxStep04StateV1,
  MissingNativeScriptTxStep05State,
  missingNativeScriptTxStep05StateV1,
  missingNativeScriptTxVersionedScriptHashV1,
} from "../src/fraud-proof/missing-native-script-tx-v1.js";

const h32 = (byte: string): string => byte.repeat(64);
const h28 = (byte: string): string => byte.repeat(56);
const nativeScriptBytes = encodeMidgardNativeScript({
  type: "all",
  scripts: [{ type: "sig", keyHash: Buffer.from(h28("1"), "hex") }],
});
const versionedScriptItem = encodeMidgardVersionedScript({
  language: "NativeCardano",
  scriptBytes: nativeScriptBytes,
  nativeScript: {
    type: "all",
    scripts: [{ type: "sig", keyHash: Buffer.from(h28("1"), "hex") }],
  },
});

describe("missing-native-script-tx V1 twins", () => {
  it("round-trips every forwarded state in on-chain field order", () => {
    const step02 = missingNativeScriptTxStep02StateFromBadTxV1({
      badTxId: h32("a"),
      badTxWitnessSetHash: h32("b"),
    });
    expect(
      Data.from(
        Data.to(step02, MissingNativeScriptTxStep02State),
        MissingNativeScriptTxStep02State,
      ),
    ).toStrictEqual(step02);

    const step03 = missingNativeScriptTxStep03StateV1({
      inputWithMissingScript: { tx_id: h32("c"), output_index: 2n },
      badTxId: step02.bad_tx_id,
      badTxWitnessSetHash: step02.bad_tx_witness_set_hash,
    });
    expect(
      Data.from(
        Data.to(step03, MissingNativeScriptTxStep03State),
        MissingNativeScriptTxStep03State,
      ),
    ).toStrictEqual(step03);

    const step04 = missingNativeScriptTxStep04StateV1({
      producingTxId: step03.input_with_missing_script.tx_id,
      badInputOutputIndex: step03.input_with_missing_script.output_index,
      badTxId: step03.bad_tx_id,
      badTxWitnessSetHash: step03.bad_tx_witness_set_hash,
    });
    expect(
      Data.from(
        Data.to(step04, MissingNativeScriptTxStep04State),
        MissingNativeScriptTxStep04State,
      ),
    ).toStrictEqual(step04);

    const step05 = missingNativeScriptTxStep05StateV1({
      expectedMissingScriptHash: h28("d"),
      badTxId: step04.bad_tx_id,
      badTxWitnessSetHash: step04.bad_tx_witness_set_hash,
    });
    expect(
      Data.from(
        Data.to(step05, MissingNativeScriptTxStep05State),
        MissingNativeScriptTxStep05State,
      ),
    ).toStrictEqual(step05);
  });

  it("hashes tag-0 native scripts and distinguishes absence from presence", () => {
    const expectedMissingScriptHash =
      missingNativeScriptTxVersionedScriptHashV1(nativeScriptBytes);
    expect(expectedMissingScriptHash).toHaveLength(56);
    expect(
      missingNativeScriptIsAbsentV1({
        scriptTxWitsItems: [],
        expectedMissingScriptHash,
      }),
    ).toBe(true);
    expect(
      missingNativeScriptIsAbsentV1({
        scriptTxWitsItems: [versionedScriptItem],
        expectedMissingScriptHash,
      }),
    ).toBe(false);
  });

  it("refuses a non-canonical field-6 item", () => {
    expect(() =>
      missingNativeScriptIsAbsentV1({
        scriptTxWitsItems: [
          Buffer.concat([versionedScriptItem, Buffer.from([0])]),
        ],
        expectedMissingScriptHash: h28("e"),
      }),
    ).toThrow();
  });
});
