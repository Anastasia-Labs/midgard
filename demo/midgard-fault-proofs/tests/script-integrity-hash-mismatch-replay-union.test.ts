/** The installed script-integrity-hash-mismatch replay. A transaction whose
 * spends are locked by scripts of two languages commits a hash over both:
 * it is left alone, and one committing a hash over one of them is detected.
 * Next to the item decoding and canonicality families, a leaf whose output, mint item or
 * script witness is not in the shape the purpose walk reads has an earlier
 * first fault: the mismatch member finds nothing in it, and the union still
 * returns the earlier family's finding for the block. */
import { describe, expect, it } from "vitest";

import { canonicalBlockEvidenceFromVerifiedPayload } from "../src/evidence/canonical-block-evidence.js";
import { admitCompleteCanonicalReplayPredecessor } from "../src/workflow/complete-replay.admit-complete-canonical-replay-predecessor.js";
import {
  createCompleteCanonicalReplayUnion,
  MINT_ITEM_NON_CANONICAL_COMPLETE_CANONICAL_REPLAY,
} from "../src/workflow/complete-replay.create-complete-canonical-replay-union.js";
import {
  SCRIPT_INTEGRITY_HASH_MISMATCH_COMPLETE_CANONICAL_REPLAY,
  TRANSACTION_OUTPUT_NON_CANONICAL_COMPLETE_CANONICAL_REPLAY,
  WITNESS_SCRIPT_DECODING_COMPLETE_CANONICAL_REPLAY,
} from "../src/workflow/complete-replay.resolved-output-non-canonical-complete-canonical-replay.js";
import { authenticatedHeaderObservation } from "./helpers/canonical-block-evidence-fixture.js";
import { singleLedgerAt } from "./support/predecessor-ledger.js";
import {
  buildIntegrityBlock,
  expectedHash,
  type IntegrityHash,
  plutusScript,
  scriptLockedEntries,
} from "./support/script-integrity-hash-mismatch-emulator.subject.js";
import { unreadableItemViolations } from "./support/unreadable-item-block.js";

const SIHM = SCRIPT_INTEGRITY_HASH_MISMATCH_COMPLETE_CANONICAL_REPLAY;
const OUTPUT = TRANSACTION_OUTPUT_NON_CANONICAL_COMPLETE_CANONICAL_REPLAY;
const MINT = MINT_ITEM_NON_CANONICAL_COMPLETE_CANONICAL_REPLAY;
const WITNESS = WITNESS_SCRIPT_DECODING_COMPLETE_CANONICAL_REPLAY;

/** Detections of the installed replay of an accepted transaction spending
 * one output locked by a Plutus V3 script and one locked by a Midgard V1
 * script, each carried as a witness and redeemed, committing `integrity`. */
const twoLanguageSpendDetections = async (integrity: IntegrityHash) => {
  const scripts = [plutusScript(3), plutusScript(4, "MidgardV1")];
  const spend = scriptLockedEntries(0x61, scripts);
  const built = await buildIntegrityBlock({
    shape: { spend, witnesses: scripts, spendRedeemers: [0n, 1n], integrity },
    source: { kind: "normal" },
  });
  const evidence = await canonicalBlockEvidenceFromVerifiedPayload({
    observation: authenticatedHeaderObservation({
      header: built.block.header,
      headerHash: built.headerHash,
    }),
    payloadEnvelopeCbor: built.block.reconstruction.payloadEnvelopeCbor,
    daProvenance: {
      trustClass: "public_or_permissionless_da",
      sourceId: "test/script-integrity-hash-mismatch-replay",
      grade: "security",
    },
  });
  const decision = await SIHM.replay(evidence, {
    predecessor: await admitCompleteCanonicalReplayPredecessor({
      ledgerAt: singleLedgerAt(
        evidence.header.prevHeaderHash,
        spend,
        evidence.header.prevUtxosRoot,
      ),
      currentEvidence: evidence,
    }),
  });
  return decision.detections.length;
};

describe("scriptIntegrityHashMismatch replay over spends of two languages", () => {
  it("leaves alone a hash over both spends' languages", async () => {
    await expect(
      twoLanguageSpendDetections(expectedHash("PlutusV3", "MidgardV1")),
    ).resolves.toBe(0);
  });

  it.each(["PlutusV3", "MidgardV1"] as const)(
    "detects a hash over %s alone",
    async (language) => {
      await expect(
        twoLanguageSpendDetections(expectedHash(language)),
      ).resolves.toBe(1);
    },
  );
});

const output = { outputs: [Buffer.alloc(64)] };
const mint = { mintPolicyItems: [Buffer.from("00", "hex")] };
const witness = { scriptWitnesses: [Buffer.from("00", "hex")] };

describe("scriptIntegrityHashMismatch replay beside an item the purpose walk cannot read", () => {
  it("finds nothing in a leaf whose every item reads", async () => {
    await expect(
      unreadableItemViolations(SIHM, { kind: "normal" }, {}),
    ).resolves.toEqual([]);
  });

  // Union members in catalogue order. The output family convicts accepted
  // outputs only; beside a forced-valid leaf the union still completes.
  it.each([
    [
      "an accepted output",
      { kind: "normal" },
      output,
      [OUTPUT, SIHM],
      ["transaction-output-non-canonical"],
    ],
    [
      "an accepted mint item",
      { kind: "normal" },
      mint,
      [SIHM, MINT],
      ["mint-item-non-canonical"],
    ],
    [
      "an accepted script witness",
      { kind: "normal" },
      witness,
      [WITNESS, SIHM],
      ["witness-script-header-malformed"],
    ],
    [
      "a forced-valid output",
      { kind: "forced", verdict: "ForcedTxValid" },
      output,
      [OUTPUT, SIHM],
      [],
    ],
  ] as const)(
    "finds nothing beside %s it cannot read and the union completes",
    async (_label, source, items, members, union) => {
      await expect(
        unreadableItemViolations(SIHM, source, items),
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
