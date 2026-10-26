/** The installed receive-purpose-language replay next to the item decoding
 * and canonicality families. A leaf whose output, mint item or script witness
 * is not in the shape the purpose walk or the receive route reads has an
 * earlier first fault: the receive-language member finds nothing in it, and
 * the union still returns the earlier family's finding for the block. A
 * recorded receive claim whose route never reads the malformed item is still
 * detected. */
import {
  encodeMidgardTxOutput,
  encodeMidgardVersionedScript,
} from "@al-ft/midgard-core";
import { Data } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  createCompleteCanonicalReplayUnion,
  MINT_ITEM_NON_CANONICAL_COMPLETE_CANONICAL_REPLAY,
  RECEIVE_PURPOSE_LANGUAGE_COMPLETE_CANONICAL_REPLAY,
  TRANSACTION_OUTPUT_NON_CANONICAL_COMPLETE_CANONICAL_REPLAY,
  WITNESS_SCRIPT_DECODING_COMPLETE_CANONICAL_REPLAY,
} from "../src/workflow/complete-replay.js";
import type { FixtureTransactionInput } from "./helpers/canonical-block-evidence-fixture.js";
import {
  protectedScriptAddress,
  redeemerItem,
} from "./support/missing-redeemer-emulator.purposes.js";
import { plutusScript } from "./support/missing-redeemer-emulator.subject.js";
import { receivePurposeLanguageVerdict } from "./support/receive-purpose-language-emulator.js";
import {
  type UnreadableItems,
  unreadableItemViolations,
} from "./support/unreadable-item-block.js";

const RPL = RECEIVE_PURPOSE_LANGUAGE_COMPLETE_CANONICAL_REPLAY;
const OUTPUT = TRANSACTION_OUTPUT_NON_CANONICAL_COMPLETE_CANONICAL_REPLAY;
const MINT = MINT_ITEM_NON_CANONICAL_COMPLETE_CANONICAL_REPLAY;
const WITNESS = WITNESS_SCRIPT_DECODING_COMPLETE_CANONICAL_REPLAY;

// The block builder passes every transaction field it is given through.
type Items = UnreadableItems &
  Pick<FixtureTransactionInput, "redeemerWitnesses">;

const BAD_OUTPUT = Buffer.alloc(64);
const BAD_ITEM = Buffer.from("00", "hex");
const script = plutusScript(1);
const receiveOutput = encodeMidgardTxOutput({
  address: protectedScriptAddress(script),
  value: { lovelace: 2_000_000n, assets: new Map() },
});

// A protected PlutusV3 receive with its witness; accepted leaves add the
// Receive redeemer that executes it.
const recorded: Items = {
  outputs: [receiveOutput],
  scriptWitnesses: [encodeMidgardVersionedScript(script)],
};
const executed: Items = {
  ...recorded,
  redeemerWitnesses: [
    redeemerItem("Receive", 0n, Buffer.from(Data.to(0n), "hex")),
  ],
};
const outputOf = (items: Items): Items => ({
  ...items,
  outputs: [receiveOutput, BAD_OUTPUT],
});
const mintOf = (items: Items): Items => ({
  ...items,
  mintPolicyItems: [BAD_ITEM],
});
const witnessOf = (items: Items): Items => ({
  ...items,
  scriptWitnesses: [BAD_ITEM],
});

const normal = { kind: "normal" } as const;
const claimed = {
  kind: "forced",
  verdict: receivePurposeLanguageVerdict(3n, 0n),
} as const;

describe("receivePurposeLanguage replay beside an item the walk cannot read", () => {
  it("detects the forbidden execution when every item reads", async () => {
    await expect(
      unreadableItemViolations(RPL, normal, executed),
    ).resolves.toEqual(["receive-purpose-plutus-v3-forbidden"]);
  });

  // Union members in catalogue order. The output and witness families
  // convict accepted leaves only; beside a forced leaf the union completes.
  it.each([
    [
      "an accepted output",
      normal,
      outputOf(executed),
      [OUTPUT, RPL],
      ["transaction-output-non-canonical"],
    ],
    [
      "an accepted mint item",
      normal,
      mintOf(executed),
      [RPL, MINT],
      ["mint-item-non-canonical"],
    ],
    [
      "an accepted script witness",
      normal,
      witnessOf(executed),
      [WITNESS, RPL],
      ["witness-script-header-malformed"],
    ],
    [
      "an output in a recorded receive claim",
      claimed,
      outputOf(recorded),
      [OUTPUT, RPL],
      [],
    ],
    [
      "a script witness in a recorded receive claim",
      claimed,
      witnessOf(recorded),
      [WITNESS, RPL],
      [],
    ],
  ] as const)(
    "finds nothing beside %s it cannot read and the union completes",
    async (_label, source, items, members, union) => {
      await expect(
        unreadableItemViolations(RPL, source, items),
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

  it("detects a recorded receive claim beside a mint item its route does not read", async () => {
    await expect(
      unreadableItemViolations(RPL, claimed, mintOf(recorded)),
    ).resolves.toEqual(["receive-purpose-plutus-v3-forbidden"]);
  });
});
