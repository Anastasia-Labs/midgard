/** The installed missing-script-source replay next to the item decoding and
 * canonicality families. A leaf whose output, mint item or script witness is
 * not in the shape the purpose walk reads has an earlier first fault: the
 * missing-source member finds nothing in it, and the union still returns the
 * earlier family's finding for the block. */
import {
  encodeMidgardMintPolicyItem,
  hashMidgardVersionedScript,
} from "@al-ft/midgard-core";
import { describe, expect, it } from "vitest";

import {
  createCompleteCanonicalReplayUnion,
  MINT_ITEM_NON_CANONICAL_COMPLETE_CANONICAL_REPLAY,
  MISSING_SCRIPT_SOURCE_COMPLETE_CANONICAL_REPLAY,
  TRANSACTION_OUTPUT_NON_CANONICAL_COMPLETE_CANONICAL_REPLAY,
  WITNESS_SCRIPT_DECODING_COMPLETE_CANONICAL_REPLAY,
} from "../src/workflow/complete-replay.js";
import { plutusScript } from "./support/missing-redeemer-emulator.subject.js";
import { scriptSourceMissingVerdict } from "./support/missing-script-source-emulator.js";
import { unreadableItemViolations } from "./support/unreadable-item-block.js";

const MSS = MISSING_SCRIPT_SOURCE_COMPLETE_CANONICAL_REPLAY;
const OUTPUT = TRANSACTION_OUTPUT_NON_CANONICAL_COMPLETE_CANONICAL_REPLAY;
const MINT = MINT_ITEM_NON_CANONICAL_COMPLETE_CANONICAL_REPLAY;
const WITNESS = WITNESS_SCRIPT_DECODING_COMPLETE_CANONICAL_REPLAY;

const mintItem = encodeMidgardMintPolicyItem({
  policyId: Buffer.from(hashMidgardVersionedScript(plutusScript(1)), "hex"),
  assets: [{ assetName: Buffer.alloc(0), quantity: 1n }],
});
// A mint policy no witness or reference input sources.
const candidate = { mintPolicyItems: [mintItem] };
const output = { ...candidate, outputs: [Buffer.alloc(64)] };
const mint = { mintPolicyItems: [mintItem, Buffer.from("00", "hex")] };
const witness = { ...candidate, scriptWitnesses: [Buffer.from("00", "hex")] };

const forcedValid = { kind: "forced", verdict: "ForcedTxValid" } as const;
const claimed = {
  kind: "forced",
  verdict: scriptSourceMissingVerdict(1n, 0n),
} as const;

describe("missingScriptSource replay beside an item the purpose walk cannot read", () => {
  it.each([
    ["an accepted", { kind: "normal" }],
    ["a forced-valid", forcedValid],
  ] as const)(
    "detects %s missing source when every item reads",
    async (_label, source) => {
      await expect(
        unreadableItemViolations(MSS, source, candidate),
      ).resolves.toEqual(["script-source-missing"]);
    },
  );

  // Union members in catalogue order. The output and witness families
  // convict accepted leaves only; beside a forced leaf the union completes.
  it.each([
    [
      "an accepted output",
      { kind: "normal" },
      output,
      [OUTPUT, MSS],
      ["transaction-output-non-canonical"],
    ],
    [
      "an accepted mint item",
      { kind: "normal" },
      mint,
      [MSS, MINT],
      ["mint-item-non-canonical"],
    ],
    [
      "an accepted script witness",
      { kind: "normal" },
      witness,
      [WITNESS, MSS],
      ["witness-script-header-malformed"],
    ],
    ["a forced-valid output", forcedValid, output, [OUTPUT, MSS], []],
    [
      "a forced-valid mint item",
      forcedValid,
      mint,
      [MSS, MINT],
      ["mint-item-non-canonical"],
    ],
    ["a forced-valid script witness", forcedValid, witness, [WITNESS, MSS], []],
    [
      "a mint item in a ScriptSourceMissing claim",
      claimed,
      mint,
      [MSS, MINT],
      [],
    ],
    [
      "a script witness in a ScriptSourceMissing claim",
      claimed,
      witness,
      [WITNESS, MSS],
      [],
    ],
  ] as const)(
    "finds nothing beside %s it cannot read and the union completes",
    async (_label, source, items, members, union) => {
      await expect(
        unreadableItemViolations(MSS, source, items),
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
});
