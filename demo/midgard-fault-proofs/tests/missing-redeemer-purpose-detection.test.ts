/**
 * The installed missing-redeemer replay over spend, mint and receive
 * purposes: an admitted transaction whose script purpose has a
 * redeemer-bearing source and no redeemer is detected at that purpose; a
 * forced RedeemerMissing rejection naming a spend, mint or receive coordinate
 * is detected exactly when the claim is false; transactions whose purposes
 * carry their redeemers, and honest rejections, are not detected.
 */
import { describe, expect, it } from "vitest";

import { canonicalBlockEvidenceFromVerifiedPayload } from "../src/evidence/canonical-block-evidence.js";
import { singleLedgerAt } from "../src/ledger-at.js";
import {
  admitCompleteCanonicalReplayPredecessor,
  MISSING_REDEEMER_COMPLETE_CANONICAL_REPLAY,
} from "../src/workflow/complete-replay.js";
import { authenticatedHeaderObservation } from "./helpers/canonical-block-evidence-fixture.js";
import { scriptAddress } from "./support/missing-redeemer-emulator.purposes.js";
import {
  buildMissingRedeemerBlock,
  honestForcedObserverShape,
  machineForcedVerdict,
  missingRedeemerLedgerEntries,
  type MissingRedeemerSource,
  type MissingRedeemerSubjectShape,
  plutusScript,
  redeemerMissingVerdict,
} from "./support/missing-redeemer-emulator.subject.js";

const daProvenance = {
  trustClass: "public_or_permissionless_da",
  sourceId: "test/missing-redeemer-purpose-detection",
  grade: "security",
} as const;

/** Installed replay of the block committing `shape` from `source`. */
const replay = async (
  shape: MissingRedeemerSubjectShape,
  source: MissingRedeemerSource,
) => {
  const built = await buildMissingRedeemerBlock({ shape, source });
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

const forced = (kind: bigint, index: bigint): MissingRedeemerSource => ({
  kind: "forced",
  verdict: redeemerMissingVerdict(kind, index),
});
const admitted: MissingRedeemerSource = {
  kind: "forced",
  verdict: "ForcedTxValid",
};

/** A signed spend of a key-locked output, as the honest observer subject. */
const signedSpend = (): MissingRedeemerSubjectShape => ({
  ...honestForcedObserverShape(5),
  observers: [],
  reference: [],
});

/** A signed spend plus a Plutus mint policy sourced from a reference input. */
const referenceMint = (redeemer: boolean): MissingRedeemerSubjectShape => ({
  ...signedSpend(),
  mints: [{ script: plutusScript(7), witness: false, redeemer }],
  reference: missingRedeemerLedgerEntries(0x31, [plutusScript(7)]),
});

/** A signed spend plus a protected output whose Plutus credential is
 * sourced from a reference input. */
const referenceReceive = (redeemer: boolean): MissingRedeemerSubjectShape => ({
  ...signedSpend(),
  receives: [{ script: plutusScript(8), witness: false, redeemer }],
  reference: missingRedeemerLedgerEntries(0x31, [plutusScript(8)]),
});

/** A spend of an output locked by a Plutus script sourced from a
 * reference input. */
const scriptSpend = (redeemer: boolean): MissingRedeemerSubjectShape => ({
  observers: [],
  spend: missingRedeemerLedgerEntries(
    0x41,
    [null],
    scriptAddress(plutusScript(9)),
  ),
  reference: missingRedeemerLedgerEntries(0x31, [plutusScript(9)]),
  ...(redeemer ? { spendRedeemers: [0n] } : {}),
});

const CLAIM = "redeemer-missing:1:0:claim";

const PURPOSES = [
  { name: "mint", kind: 1n, subject: referenceMint },
  { name: "receive", kind: 3n, subject: referenceReceive },
  { name: "spend", kind: 0n, subject: scriptSpend },
] as const;

describe.each(PURPOSES)(
  "missing-redeemer $name purpose",
  ({ kind, subject }) => {
    it("is where the node's classifier finds the missing redeemer", async () => {
      await expect(machineForcedVerdict(subject(false))).resolves.toStrictEqual(
        redeemerMissingVerdict(kind, 0n),
      );
    });

    it("is not detected under the node's honest rejection", async () => {
      await expect(replay(subject(false), forced(kind, 0n))).resolves.toEqual(
        [],
      );
    });

    it("is detected when the transaction is committed as admitted", async () => {
      await expect(replay(subject(false), admitted)).resolves.toEqual([
        `redeemer-missing:1:0:${kind.toString()}:0`,
      ]);
    });

    it("is not detected as admitted when it carries its redeemer", async () => {
      await expect(replay(subject(true), admitted)).resolves.toEqual([]);
    });

    it("is detected when the rejection names it while its redeemer is present", async () => {
      await expect(replay(subject(true), forced(kind, 0n))).resolves.toEqual([
        CLAIM,
      ]);
    });

    it("is detected when the rejection names an index past the transaction's own", async () => {
      await expect(replay(subject(false), forced(kind, 1n))).resolves.toEqual([
        CLAIM,
      ]);
    });
  },
);

describe("missing-redeemer rejection naming a purpose the transaction lacks", () => {
  it.each([
    ["a mint", 1n],
    ["a receive", 3n],
    ["a script spend of its key-locked input", 0n],
  ] as const)("is detected for %s", async (_, kind) => {
    await expect(
      replay(honestForcedObserverShape(5), forced(kind, 0n)),
    ).resolves.toEqual([CLAIM]);
  });
});
