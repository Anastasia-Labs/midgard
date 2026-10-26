/**
 * The installed missing-script-source replay over forced subjects. The
 * honest subject spends a signed-for output and names a Plutus observer
 * that neither a witness nor a reference input sources; the node's forced
 * classifier rejects it with ScriptSourceMissing at that observer and the
 * replay finds nothing in that rejection. The same transaction committed as
 * admitted, a ScriptSourceMissing rejection whose observer has a source, a
 * purpose-free reason the purpose phase precedes, and a RedeemerMissing
 * rejection behind an earlier missing source are each detected; reasons
 * that precede the missing source are not.
 */
import { screenMidgardForcedTxAdmission } from "@al-ft/midgard-core";
import * as SDK from "@al-ft/midgard-sdk";
import { describe, expect, it } from "vitest";

import { canonicalBlockEvidenceFromVerifiedPayload } from "../src/evidence/canonical-block-evidence.js";
import { singleLedgerAt } from "../src/ledger-at.js";
import {
  admitCompleteCanonicalReplayPredecessor,
  MISSING_SCRIPT_SOURCE_COMPLETE_CANONICAL_REPLAY,
} from "../src/workflow/complete-replay.js";
import { authenticatedHeaderObservation } from "./helpers/canonical-block-evidence-fixture.js";
import {
  buildMissingRedeemerBlock,
  buildMissingRedeemerSubject,
  honestForcedObserverShape,
  machineForcedVerdict,
  missingRedeemerLedgerEntries,
  type MissingRedeemerSubjectShape,
  plutusScript,
  redeemerMissingVerdict,
} from "./support/missing-redeemer-emulator.subject.js";
import { scriptSourceMissingVerdict } from "./support/missing-script-source-emulator.js";

const daProvenance = {
  trustClass: "public_or_permissionless_da",
  sourceId: "test/missing-script-source-replay",
  grade: "security",
} as const;

const signed = honestForcedObserverShape(5);

/** The honest subject: its only observer has no source anywhere. */
const SOURCELESS: MissingRedeemerSubjectShape = {
  ...signed,
  observers: [{ script: plutusScript(5), witness: false, redeemer: false }],
  reference: [],
};

/** Two observers, plutusScript(11) ordered before plutusScript(12); the
 * one named `sourced` resolves through a reference input and carries no
 * redeemer, the other has no source. */
const twoObservers = (sourced: 11 | 12): MissingRedeemerSubjectShape => ({
  ...signed,
  observers: [11, 12].map((seed) => ({
    script: plutusScript(seed),
    witness: false,
    redeemer: false,
  })),
  reference: missingRedeemerLedgerEntries(0x31, [plutusScript(sourced)]),
});

const rejected = (reason: SDK.RejectionReason): SDK.OperatorVerdict => ({
  ForcedTxInvalid: { reason },
});

/** Installed replay of the block committing `shape` under `verdict`. */
const replayUnder = async (
  verdict: SDK.OperatorVerdict,
  shape: MissingRedeemerSubjectShape = SOURCELESS,
) => {
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
  const decision = await MISSING_SCRIPT_SOURCE_COMPLETE_CANONICAL_REPLAY.replay(
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

describe("missingScriptSource honest forced rejection", () => {
  it("is admitted by the forced screen and is the verdict the node writes", async () => {
    expect(
      screenMidgardForcedTxAdmission(
        buildMissingRedeemerSubject(SOURCELESS).nativeTx,
      ),
    ).toBeNull();
    await expect(machineForcedVerdict(SOURCELESS)).resolves.toStrictEqual(
      scriptSourceMissingVerdict(2n, 0n),
    );
  });

  it("is not detected by the installed replay", async () => {
    await expect(
      replayUnder(scriptSourceMissingVerdict(2n, 0n)),
    ).resolves.toEqual([]);
  });

  it("is detected when the same transaction is committed as admitted", async () => {
    await expect(replayUnder("ForcedTxValid")).resolves.toEqual([
      "script-source-missing:1:0:2:0",
    ]);
  });
});

describe("missingScriptSource rejections the replay detects", () => {
  it("a ScriptSourceMissing whose observer has a source, through the claim", async () => {
    const shape = honestForcedObserverShape(5);
    await expect(machineForcedVerdict(shape)).resolves.toStrictEqual(
      redeemerMissingVerdict(2n, 0n),
    );
    await expect(
      replayUnder(scriptSourceMissingVerdict(2n, 0n), shape),
    ).resolves.toEqual(["script-source-missing:1:0:claim"]);
  });

  it.each([
    [
      "PlutusExecutionFailed",
      rejected({
        PlutusExecutionFailed: { purpose_kind: 2n, purpose_index: 0n },
      }),
    ],
    ["UnusedRedeemer", rejected({ UnusedRedeemer: { redeemer_index: 0n } })],
  ] as const)(
    "a recorded %s behind the missing source",
    async (_label, verdict) => {
      await expect(replayUnder(verdict)).resolves.toEqual([
        "script-source-missing:1:0:2:0",
      ]);
    },
  );

  it("a RedeemerMissing at a later observer than a missing source", async () => {
    const shape = twoObservers(12);
    await expect(machineForcedVerdict(shape)).resolves.toStrictEqual(
      scriptSourceMissingVerdict(2n, 0n),
    );
    await expect(
      replayUnder(redeemerMissingVerdict(2n, 1n), shape),
    ).resolves.toEqual(["script-source-missing:1:0:2:0"]);
  });

  it("a ScriptSourceMissing naming a later observer than the first missing source", async () => {
    const shape: MissingRedeemerSubjectShape = {
      ...twoObservers(11),
      reference: [],
    };
    await expect(machineForcedVerdict(shape)).resolves.toStrictEqual(
      scriptSourceMissingVerdict(2n, 0n),
    );
    // Observer 1 has no source either, so the claim is true; the replay
    // convicts through the earlier missing source.
    await expect(
      replayUnder(scriptSourceMissingVerdict(2n, 1n), shape),
    ).resolves.toEqual(["script-source-missing:1:0:2:0"]);
  });
});

describe("missingScriptSource rejections the replay leaves alone", () => {
  it("a recorded reason that precedes the purpose phase", async () => {
    await expect(
      replayUnder(rejected("ScriptIntegrityHashMissing")),
    ).resolves.toEqual([]);
  });

  it("a RedeemerMissing at an observer before the missing source", async () => {
    const shape = twoObservers(11);
    await expect(machineForcedVerdict(shape)).resolves.toStrictEqual(
      redeemerMissingVerdict(2n, 0n),
    );
    await expect(
      replayUnder(redeemerMissingVerdict(2n, 0n), shape),
    ).resolves.toEqual([]);
  });
});
