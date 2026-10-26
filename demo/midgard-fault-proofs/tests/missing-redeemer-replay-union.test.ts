/** The installed missing-redeemer replay next to the item decoding and
 * canonicality families. A leaf whose output, mint item or script witness is
 * not in the shape the purpose walk or the claimed purpose's domain reads has
 * an earlier first fault: the missing-redeemer member finds nothing in it,
 * and the union still returns the earlier family's finding for the block. */
import {
  encodeMidgardMintPolicyItem,
  encodeMidgardVersionedScript,
  hashMidgardVersionedScript,
} from "@al-ft/midgard-core";
import { Data } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  createCompleteCanonicalReplayUnion,
  MINT_ITEM_NON_CANONICAL_COMPLETE_CANONICAL_REPLAY,
  MISSING_REDEEMER_COMPLETE_CANONICAL_REPLAY,
  TRANSACTION_OUTPUT_NON_CANONICAL_COMPLETE_CANONICAL_REPLAY,
  WITNESS_SCRIPT_DECODING_COMPLETE_CANONICAL_REPLAY,
} from "../src/workflow/complete-replay.js";
import type { FixtureTransactionInput } from "./helpers/canonical-block-evidence-fixture.js";
import { redeemerItem } from "./support/missing-redeemer-emulator.purposes.js";
import {
  plutusScript,
  redeemerMissingVerdict,
} from "./support/missing-redeemer-emulator.subject.js";
import {
  type UnreadableItems,
  unreadableItemViolations,
} from "./support/unreadable-item-block.js";

const MR = MISSING_REDEEMER_COMPLETE_CANONICAL_REPLAY;
const OUTPUT = TRANSACTION_OUTPUT_NON_CANONICAL_COMPLETE_CANONICAL_REPLAY;
const MINT = MINT_ITEM_NON_CANONICAL_COMPLETE_CANONICAL_REPLAY;
const WITNESS = WITNESS_SCRIPT_DECODING_COMPLETE_CANONICAL_REPLAY;

const script = plutusScript(1);
const mintItem = encodeMidgardMintPolicyItem({
  policyId: Buffer.from(hashMidgardVersionedScript(script), "hex"),
  assets: [{ assetName: Buffer.alloc(0), quantity: 1n }],
});
// A Plutus mint policy with its witness and no redeemer.
const candidate = {
  mintPolicyItems: [mintItem],
  scriptWitnesses: [encodeMidgardVersionedScript(script)],
};
const output = { ...candidate, outputs: [Buffer.alloc(64)] };
const mint = {
  ...candidate,
  mintPolicyItems: [mintItem, Buffer.from("00", "hex")],
};
const witness = {
  mintPolicyItems: [mintItem],
  scriptWitnesses: [Buffer.from("00", "hex")],
};

const forcedValid = { kind: "forced", verdict: "ForcedTxValid" } as const;
const claimed = (kind: bigint) =>
  ({ kind: "forced", verdict: redeemerMissingVerdict(kind, 0n) }) as const;

// The block builder passes every transaction field it is given through.
const redeemed: UnreadableItems &
  Pick<FixtureTransactionInput, "redeemerWitnesses"> = {
  ...output,
  redeemerWitnesses: [
    redeemerItem("Mint", 0n, Buffer.from(Data.to(0n), "hex")),
  ],
};

describe("missingRedeemer replay beside an item the purpose walk cannot read", () => {
  it.each([
    ["an accepted", { kind: "normal" }],
    ["a forced-valid", forcedValid],
  ] as const)(
    "detects %s missing redeemer when every item reads",
    async (_label, source) => {
      await expect(
        unreadableItemViolations(MR, source, candidate),
      ).resolves.toEqual(["redeemer-missing"]);
    },
  );

  // Union members in catalogue order. The output and witness families
  // convict accepted leaves only; beside a forced leaf the union completes.
  it.each([
    [
      "an accepted output",
      { kind: "normal" },
      output,
      [OUTPUT, MR],
      ["transaction-output-non-canonical"],
    ],
    [
      "an accepted mint item",
      { kind: "normal" },
      mint,
      [MR, MINT],
      ["mint-item-non-canonical"],
    ],
    [
      "an accepted script witness",
      { kind: "normal" },
      witness,
      [WITNESS, MR],
      ["witness-script-header-malformed"],
    ],
    ["a forced-valid output", forcedValid, output, [OUTPUT, MR], []],
    [
      "a forced-valid mint item",
      forcedValid,
      mint,
      [MR, MINT],
      ["mint-item-non-canonical"],
    ],
    ["a forced-valid script witness", forcedValid, witness, [WITNESS, MR], []],
    [
      "a mint item in a RedeemerMissing mint claim",
      claimed(1n),
      mint,
      [MR, MINT],
      [],
    ],
    [
      "an output in a RedeemerMissing receive claim",
      claimed(3n),
      output,
      [OUTPUT, MR],
      [],
    ],
    [
      "a script witness in a RedeemerMissing mint claim",
      claimed(1n),
      witness,
      [WITNESS, MR],
      [],
    ],
  ] as const)(
    "finds nothing beside %s it cannot read and the union completes",
    async (_label, source, items, members, union) => {
      await expect(
        unreadableItemViolations(MR, source, items),
      ).resolves.toEqual([]);
      await expect(
        unreadableItemViolations(
          createCompleteCanonicalReplayUnion([...members]),
          source,
          items,
        ),
      ).resolves.toEqual(union);
    },
  );

  it("convicts a RedeemerMissing mint claim beside an output its domain does not read", async () => {
    await expect(
      unreadableItemViolations(MR, claimed(1n), redeemed),
    ).resolves.toEqual(["redeemer-missing"]);
    await expect(
      unreadableItemViolations(
        createCompleteCanonicalReplayUnion([OUTPUT, MR]),
        claimed(1n),
        redeemed,
      ),
    ).resolves.toEqual(["redeemer-missing"]);
  });
});
