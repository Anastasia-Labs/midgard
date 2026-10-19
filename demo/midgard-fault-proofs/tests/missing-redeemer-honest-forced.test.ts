/**
 * A forced transaction the admission screen accepts and whose first fault is
 * RedeemerMissing at its required observer: it spends one signed-for output
 * and sources its Plutus observer only from a reference input's script. The
 * node's classifier writes that verdict, and the installed replay finds
 * nothing in it; the same transaction committed under another verdict is
 * detected.
 */
import * as SDK from "@al-ft/midgard-sdk";
import { describe, expect, it } from "vitest";

import { canonicalBlockEvidenceFromVerifiedPayload } from "../src/evidence/canonical-block-evidence.js";
import { singleLedgerAt } from "../src/ledger-at.js";
import {
  admitCompleteCanonicalReplayPredecessor,
  MISSING_REDEEMER_COMPLETE_CANONICAL_REPLAY,
} from "../src/workflow/complete-replay.js";
import { authenticatedHeaderObservation } from "./helpers/canonical-block-evidence-fixture.js";
import {
  buildMissingRedeemerBlock,
  honestForcedObserverShape,
  machineForcedVerdict,
  redeemerMissingVerdict,
} from "./support/missing-redeemer-emulator.subject.js";

const daProvenance = {
  trustClass: "public_or_permissionless_da",
  sourceId: "test/missing-redeemer-honest-forced",
  grade: "security",
} as const;

const shape = honestForcedObserverShape(5);

/** Installed replay of the block committing the subject under `verdict`. */
const replayUnder = async (verdict: SDK.OperatorVerdict) => {
  const built = await buildMissingRedeemerBlock({
    shape,
    source: { kind: "forced", verdict },
  });
  const evidence = await canonicalBlockEvidenceFromVerifiedPayload({
    observation: authenticatedHeaderObservation({
      header: built.block.header,
      headerHash: built.headerHash,
    }),
    payloadEnvelopeCbor: built.block.reconstruction.payloadEnvelopeCbor,
    daProvenance,
  });
  const decision = await MISSING_REDEEMER_COMPLETE_CANONICAL_REPLAY.replay(
    evidence,
    {
      predecessor: await admitCompleteCanonicalReplayPredecessor({
        ledgerAt: singleLedgerAt(evidence.header.prevHeaderHash, [
          ...(shape.spend ?? []),
          ...(shape.reference ?? []),
        ]),
        currentEvidence: evidence,
      }),
    },
  );
  return decision.detections.map((detection) => detection.detectionId);
};

describe("missingRedeemer honest forced rejection", () => {
  it("is the verdict the node writes for the subject", async () => {
    await expect(machineForcedVerdict(shape)).resolves.toStrictEqual(
      redeemerMissingVerdict(2n, 0n),
    );
  });

  it("is not detected by the installed replay", async () => {
    await expect(replayUnder(redeemerMissingVerdict(2n, 0n))).resolves.toEqual(
      [],
    );
  });

  it("is detected when the same transaction is committed as admitted", async () => {
    await expect(replayUnder("ForcedTxValid")).resolves.toEqual([
      "redeemer-missing:1:0:2:0",
    ]);
  });

  it("is detected when the rejection names an observer the transaction lacks", async () => {
    await expect(replayUnder(redeemerMissingVerdict(2n, 1n))).resolves.toEqual([
      "redeemer-missing:1:0:claim",
    ]);
  });
});
