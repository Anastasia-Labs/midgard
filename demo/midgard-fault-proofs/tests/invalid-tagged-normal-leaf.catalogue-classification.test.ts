/** The installed catalogue over a block whose only normal leaf the operator
 * committed `TxIsInvalid`. A normal leaf is an acceptance verdict, so the leaf
 * is itself the fault, and only `l2TxMistag` opens a normal leaf whatever its
 * validity tag: every other family's step validator opens an accepted subject
 * only at validity code 0. Whatever else such a leaf breaks, the block is
 * decided as `l2TxMistag`. */
import {
  decodeMidgardNativeTxFullFromCanonicalCbor,
  encodeMidgardMintPolicyItem,
  encodeMidgardVersionedScript,
  hashMidgardVersionedScript,
} from "@al-ft/midgard-core";
import { GENESIS_HEADER_HASH } from "@al-ft/midgard-sdk/core";
import { Data } from "@lucid-evolution/lucid";
import { afterEach, describe, expect, it } from "vitest";

import type { CompleteCanonicalReplayIsolation } from "../src/workflow/complete-replay.create-complete-canonical-replay-union.js";
import { classifyHeader } from "../src/workflow/header-classifier.classify-header.js";
import {
  buildCanonicalBlockFixture,
  buildFixtureTransaction,
  outRefCbor,
} from "./helpers/canonical-block-evidence-fixture.js";
import {
  replayBudget,
  setup,
  transport,
} from "./support/catalogue-retained-classification.setup.js";
import { redeemerItem } from "./support/missing-redeemer-emulator.purposes.js";
import { plutusScript } from "./support/missing-redeemer-emulator.subject.js";
import { buildRetainedValidationBlockFixture } from "./support/retained-reason-classifier.build-retained-validation-block-fixture.js";

afterEach(() => {
  transport.raw = undefined;
});

const BLOCK_SLOT = 100n;

const script = plutusScript(1);

/** A spend input of the exact item width whose interior is not a §5.3
 * output reference: the field envelope is sound, the item does not decode. */
const undecodableSpendInput = (() => {
  const item = Buffer.from(outRefCbor(4, 0n));
  item[0] = 0x83;
  return item;
})();

const classifyInvalidTaggedLeaf = async (
  leaf: Partial<
    Omit<Parameters<typeof buildFixtureTransaction>[0], "validity" | "fee">
  >,
  isolation?: CompleteCanonicalReplayIsolation,
) => {
  const predecessor = await buildCanonicalBlockFixture({
    transactions: [],
    prevHeaderHash: GENESIS_HEADER_HASH,
  });
  const transaction = buildFixtureTransaction({
    spendInputs: [],
    ...leaf,
    validity: "TxIsInvalid",
    fee: 0n,
  });
  const block = await buildRetainedValidationBlockFixture({
    subject: {
      kind: "normal",
      nativeTx: decodeMidgardNativeTxFullFromCanonicalCbor(
        transaction.canonicalCbor,
      ),
    },
    priorLedgerRoot: predecessor.header.utxosRoot,
    prevHeaderHash: predecessor.headerHash,
    blockEndTimeMs: 1_750_000_000_000,
    blockSlot: BLOCK_SLOT,
  });
  const { classifyInput } = await setup(
    {
      block,
      predecessor,
      transaction,
      orderKey: { transactionId: "52".repeat(32), outputIndex: 0n },
    },
    "normal",
    isolation,
  );
  return {
    headerHash: block.headerHash,
    decision: await classifyHeader(classifyInput),
  };
};

describe(
  "the installed catalogue over a normal leaf committed TxIsInvalid",
  replayBudget,
  () => {
    it.each([
      ["with no spend input and no fee", {}],
      [
        "whose validity range ends before the block slot",
        { validityIntervalEnd: BLOCK_SLOT - 50n },
      ],
      [
        "carrying a Plutus mint policy, its witness and a redeemer",
        {
          mintPolicyItems: [
            encodeMidgardMintPolicyItem({
              policyId: Buffer.from(hashMidgardVersionedScript(script), "hex"),
              assets: [{ assetName: Buffer.alloc(0), quantity: 1n }],
            }),
          ],
          scriptWitnesses: [encodeMidgardVersionedScript(script)],
          redeemerWitnesses: [
            redeemerItem("Mint", 0n, Buffer.from(Data.to(0n), "hex")),
          ],
        },
      ],
    ] as const)("decides a leaf %s as l2TxMistag", async (_case, leaf) => {
      const { headerHash, decision } = await classifyInvalidTaggedLeaf(leaf);
      expect(decision).toMatchObject({
        decision: "fault_detected",
        category: "l2TxMistag",
        headerHash,
      });
    });

    // Most detectors decode every leaf's inputs, so on this block they fail
    // and the watcher's isolation withholds them. The mistag is read from
    // the committed compact alone, so it still decides the block.
    it("decides a leaf whose spend input does not decode as l2TxMistag", async () => {
      const withheld: string[] = [];
      const { headerHash, decision } = await classifyInvalidTaggedLeaf(
        { spendInputs: [undecodableSpendInput] },
        {
          failsPass: () => false,
          withheld: ({ launchScope }) => withheld.push(...launchScope),
        },
      );
      expect(decision).toMatchObject({
        decision: "fault_detected",
        category: "l2TxMistag",
        headerHash,
      });
      expect(withheld).not.toContain("l2TxMistag");
    });
  },
);
