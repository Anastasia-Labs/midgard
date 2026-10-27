/** The installed unused-redeemer replay next to the item decoding and
 * canonicality families. An accepted leaf whose redeemer names a purpose
 * whose mint item or output is not in the shape the walk reads, or whose
 * script witness the source index cannot read, has an earlier first fault:
 * the unused-redeemer member finds nothing in it, and the union still
 * returns the earlier family's finding for the block. A redeemer whose walk
 * never reads the malformed item is still detected. */
import { Data } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  createCompleteCanonicalReplayUnion,
  MINT_ITEM_NON_CANONICAL_COMPLETE_CANONICAL_REPLAY,
  TRANSACTION_OUTPUT_NON_CANONICAL_COMPLETE_CANONICAL_REPLAY,
  UNUSED_REDEEMER_COMPLETE_CANONICAL_REPLAY,
  WITNESS_SCRIPT_DECODING_COMPLETE_CANONICAL_REPLAY,
} from "../src/workflow/complete-replay.js";
import type { FixtureTransactionInput } from "./helpers/canonical-block-evidence-fixture.js";
import { redeemerItem } from "./support/missing-redeemer-emulator.purposes.js";
import {
  type UnreadableItems,
  unreadableItemViolations,
} from "./support/unreadable-item-block.js";

const UR = UNUSED_REDEEMER_COMPLETE_CANONICAL_REPLAY;
const OUTPUT = TRANSACTION_OUTPUT_NON_CANONICAL_COMPLETE_CANONICAL_REPLAY;
const MINT = MINT_ITEM_NON_CANONICAL_COMPLETE_CANONICAL_REPLAY;
const WITNESS = WITNESS_SCRIPT_DECODING_COMPLETE_CANONICAL_REPLAY;

// The block builder passes every transaction field it is given through.
type Items = UnreadableItems &
  Pick<FixtureTransactionInput, "redeemerWitnesses">;

const DATA = Buffer.from(Data.to(0n), "hex");
const BAD_OUTPUT = Buffer.alloc(64);
const BAD_ITEM = Buffer.from("00", "hex");
const normal = { kind: "normal" } as const;

// A Reward redeemer no observer occupies: unused.
const reward: Items = { redeemerWitnesses: [redeemerItem("Reward", 0n, DATA)] };
const mint: Items = {
  mintPolicyItems: [BAD_ITEM],
  redeemerWitnesses: [redeemerItem("Mint", 0n, DATA)],
};
const receive: Items = {
  outputs: [BAD_OUTPUT],
  redeemerWitnesses: [redeemerItem("Receive", 0n, DATA)],
};
const witness: Items = { ...reward, scriptWitnesses: [BAD_ITEM] };

describe("unusedRedeemer replay beside an item the walk cannot read", () => {
  it("detects the unused redeemer when every item reads", async () => {
    await expect(unreadableItemViolations(UR, normal, reward)).resolves.toEqual(
      ["unused-redeemer"],
    );
  });

  // Union members in catalogue order.
  it.each([
    [
      "a Mint redeemer's mint item",
      mint,
      [UR, MINT],
      ["mint-item-non-canonical"],
    ],
    [
      "a Receive redeemer's output",
      receive,
      [OUTPUT, UR],
      ["transaction-output-non-canonical"],
    ],
    [
      "a script witness",
      witness,
      [WITNESS, UR],
      ["witness-script-header-malformed"],
    ],
  ] as const)(
    "finds nothing beside %s it cannot read and the union completes",
    async (_label, items, members, union) => {
      await expect(
        unreadableItemViolations(UR, normal, items),
      ).resolves.toEqual([]);
      await expect(
        unreadableItemViolations(
          createCompleteCanonicalReplayUnion([...members]),
          normal,
          items,
        ),
      ).resolves.toEqual(union);
    },
  );

  it.each([
    [
      "an output",
      { ...reward, outputs: [BAD_OUTPUT] },
      [OUTPUT, UR],
      ["transaction-output-non-canonical", "unused-redeemer"],
    ],
    [
      "a mint item",
      { ...reward, mintPolicyItems: [BAD_ITEM] },
      [UR, MINT],
      ["unused-redeemer", "mint-item-non-canonical"],
    ],
  ] as const)(
    "detects a Reward redeemer beside %s its walk does not read",
    async (_label, items, members, union) => {
      await expect(
        unreadableItemViolations(UR, normal, items),
      ).resolves.toEqual(["unused-redeemer"]);
      await expect(
        unreadableItemViolations(
          createCompleteCanonicalReplayUnion([...members]),
          normal,
          items,
        ),
      ).resolves.toEqual(union);
    },
  );
});
