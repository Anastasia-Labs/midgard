import fs from "node:fs";
import path from "node:path";
import { fileURLToPath } from "node:url";

import { describe, expect, it } from "vitest";

import {
  buildOrdinaryNativeTxFixture,
  ORDINARY_PARAMETERS,
  ORDINARY_PRODUCER,
  type OrdinaryNativeTxFixture,
} from "./fixtures/native-ordinary.js";
import { stableNativeTxFixtureJson } from "./fixtures/native-tx-fixture-shape.js";

const fixturePath = path.join(
  path.dirname(fileURLToPath(import.meta.url)),
  "fixtures/native-ordinary-golden.json",
);

const readFixture = (): OrdinaryNativeTxFixture =>
  JSON.parse(fs.readFileSync(fixturePath, "utf8")) as OrdinaryNativeTxFixture;

/**
 * The writer for the ordinary golden, on the same contract as its two siblings:
 * `MIDGARD_SYNC_FIXTURES=1` writes the construction's bytes, check mode — the
 * default — asserts the checked-in file is what the construction produces today.
 *
 * `golden_core_native_full_tx_cbor` in `native-tx.test.ak` was the last
 * hand-maintained seed on the generator's ordinary path (#588 item 4). It is now
 * rebound from this fixture by
 * `scripts/generate-native-compact-aiken-goldens.mjs`, so the Aiken module has no
 * literal a contributor is expected to retype.
 */
const syncing = process.env.MIDGARD_SYNC_FIXTURES === "1";

describe("ordinary native golden fixture", () => {
  it("rebuilds the checked-in fixture from its declared construction", () => {
    const rebuilt = buildOrdinaryNativeTxFixture();

    expect(rebuilt.producer).toBe(ORDINARY_PRODUCER);
    expect(rebuilt.sizes.fee).toBe(ORDINARY_PARAMETERS.fee.toString(10));
    expect(rebuilt.sizes.fullTxCborBytes).toBe(rebuilt.fullTxCborHex.length / 2);
    // The six empty fields are the `80` sentinel, and they are the same byte in
    // the body and in the witness set — the property the Aiken module leans on
    // when it points every empty-field assertion at one constant.
    for (const emptyField of [
      rebuilt.preimages.requiredObserversCborHex,
      rebuilt.preimages.requiredSignersCborHex,
      rebuilt.preimages.mintCborHex,
      rebuilt.preimages.addrTxWitsCborHex,
      rebuilt.preimages.scriptTxWitsCborHex,
      rebuilt.preimages.redeemerTxWitsCborHex,
    ]) {
      expect(emptyField).toBe("80");
    }
    // One input at index 0 and one at index 1, both in the §5.3 fixed-index
    // 38-byte form: `82 ‖ 58 20 tx_id ‖ 19 index_be16`, so the two differ only
    // in the two index bytes.
    expect(rebuilt.preimages.spendInputsCborHex).toBe(
      `815826825820${"11".repeat(32)}190000`,
    );
    expect(rebuilt.preimages.referenceInputsCborHex).toBe(
      `815826825820${"22".repeat(32)}190001`,
    );

    if (syncing) {
      fs.writeFileSync(fixturePath, stableNativeTxFixtureJson(rebuilt));
      return;
    }

    expect(stableNativeTxFixtureJson(rebuilt)).toBe(
      fs.readFileSync(fixturePath, "utf8"),
    );
    expect(readFixture()).toEqual(rebuilt);
  });
});
