/**
 * `missingRedeemer` over an accepted forced transaction, from a genuine Init.
 *
 * The subject spends an output locked by a Plutus script that only a
 * reference input sources. It carries no script witness and no redeemer, so
 * the forced-admission screen admits it, and the node's forced classifier
 * rejects it with RedeemerMissing at that spend. Committed as accepted, the
 * thread convicts it and its block is removed. Committed with the node's own
 * rejection, the installed replay detects nothing; the prover's and the
 * terminal's refusal of that same leaf is the "F-A spend honest" row of
 * `submit-init-emulator-missing-redeemer-purpose-routes.test.ts`.
 */
import { screenMidgardForcedTxAdmission } from "@al-ft/midgard-core";
import * as SDK from "@al-ft/midgard-sdk";
import { describe, expect, it } from "vitest";

import { canonicalBlockEvidenceFromVerifiedPayload } from "../src/evidence/canonical-block-evidence.js";
import { submitRemoveFraudulentBlock } from "../src/index.js";
import { singleLedgerAt } from "../src/ledger-at.js";
import {
  admitCompleteCanonicalReplayPredecessor,
  MISSING_REDEEMER_COMPLETE_CANONICAL_REPLAY,
} from "../src/workflow/complete-replay.js";
import { authenticatedHeaderObservation } from "./helpers/canonical-block-evidence-fixture.js";
import {
  driveMissingRedeemerThread,
  makeMissingRedeemerHarness,
  MISSING_REDEEMER_STEP_BOUNDS,
  publishMissingRedeemerRoles,
  stepTable,
} from "./support/missing-redeemer-emulator.js";
import { scriptAddress } from "./support/missing-redeemer-emulator.purposes.js";
import {
  buildMissingRedeemerBlock,
  buildMissingRedeemerSubject,
  machineForcedVerdict,
  missingRedeemerLedgerEntries,
  type MissingRedeemerSubjectShape,
  plutusScript,
  redeemerMissingVerdict,
  setupMissingRedeemerScenario,
} from "./support/missing-redeemer-emulator.subject.js";
import {
  buildRemovalDeploymentInfo,
  expectSingleUtxoWithUnit,
  network,
  publishRemovalReferenceScripts,
} from "./support/submit-init-emulator-shared.js";

/** A spend of an output locked by a Plutus script sourced only from a
 * reference input, with no script witness and no redeemer. */
const SHAPE: MissingRedeemerSubjectShape = {
  observers: [],
  spend: missingRedeemerLedgerEntries(
    0x41,
    [null],
    scriptAddress(plutusScript(9)),
  ),
  reference: missingRedeemerLedgerEntries(0x31, [plutusScript(9)]),
};

/** The node's verdict for SHAPE: RedeemerMissing at spend 0. */
const HONEST_VERDICT = redeemerMissingVerdict(0n, 0n);

const daProvenance = {
  trustClass: "public_or_permissionless_da",
  sourceId: "test/missing-redeemer-accepted-forced",
  grade: "security",
} as const;

/** Installed replay of the block committing SHAPE under `verdict`. */
const replayUnder = async (verdict: SDK.OperatorVerdict) => {
  const built = await buildMissingRedeemerBlock({
    shape: SHAPE,
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
          ...(SHAPE.spend ?? []),
          ...(SHAPE.reference ?? []),
        ]),
        currentEvidence: evidence,
      }),
    },
  );
  return { built, detections: decision.detections };
};

describe("missing-redeemer accepted forced transaction", () => {
  it("is admitted as a forced transaction and rejected by the node with RedeemerMissing at its spend", async () => {
    const { nativeTx } = buildMissingRedeemerSubject(SHAPE);
    expect(screenMidgardForcedTxAdmission(nativeTx)).toBeNull();
    await expect(machineForcedVerdict(SHAPE)).resolves.toStrictEqual(
      HONEST_VERDICT,
    );
  });

  it("is detected by the installed replay when committed as accepted, and not under the node's rejection", async () => {
    const accepted = await replayUnder("ForcedTxValid");
    expect(
      accepted.detections.map((detection) => detection.detectionId),
    ).toStrictEqual(["redeemer-missing:1:0:0:0"]);
    const honest = await replayUnder(HONEST_VERDICT);
    expect(honest.detections).toStrictEqual([]);
    // Both blocks commit the same transaction.
    expect(honest.built.block.nativeTxId).toStrictEqual(
      accepted.built.block.nativeTxId,
    );
  });

  it("A-F: convicts it committed as accepted from a genuine Init and removes its block", async () => {
    const harness = await makeMissingRedeemerHarness();
    const references = await publishMissingRedeemerRoles(harness);
    const scenario = await setupMissingRedeemerScenario({
      harness,
      shape: SHAPE,
      source: { kind: "forced", verdict: "ForcedTxValid" },
    });
    expect(scenario.block.txInclusion).toBeNull();
    expect(scenario.material.bound.subject.source_kind).toBe(
      SDK.PROOF_THREAD_SOURCE_KIND_FORCED,
    );
    const run = await driveMissingRedeemerThread({
      harness,
      scenario,
      references,
    });
    expect(run.prepared.convicts).toBe(true);
    expect(run.prepared.final.claim).toBe("NotRequired");
    expect(run.prepared.final.context.observed?.redeemer_missing).toBe(true);
    const outcome = await run.terminal();
    console.log("MR A-F steps", JSON.stringify(stepTable(run.steps)));
    for (const { label, measurement } of run.steps) {
      expect(
        measurement.executionMemory,
        `${label} memory`,
      ).toBeLessThanOrEqual(MISSING_REDEEMER_STEP_BOUNDS.memory);
      expect(measurement.executionSteps, `${label} cpu`).toBeLessThanOrEqual(
        MISSING_REDEEMER_STEP_BOUNDS.steps,
      );
      expect(
        measurement.completeSignedBytes,
        `${label} bytes`,
      ).toBeLessThanOrEqual(MISSING_REDEEMER_STEP_BOUNDS.bytes);
    }
    expect(outcome.fraudulentHeaderHash).toBe(scenario.setup.headerHash);
    await expectSingleUtxoWithUnit(
      harness.proverLucid,
      harness.missingRedeemer.fraudProof.spendingScriptAddress,
      outcome.fraudProofUnit,
    );

    const removalReferenceScripts = await publishRemovalReferenceScripts({
      lucid: harness.proverLucid,
      contracts: harness.contracts,
    });
    const now = BigInt(harness.emulator.now());
    const removal = await submitRemoveFraudulentBlock({
      lucid: harness.proverLucid,
      blueprint: harness.realBlueprint,
      deploymentInfo: buildRemovalDeploymentInfo(
        harness.contracts,
        harness.catalogue,
        { removalReferenceScripts: removalReferenceScripts.published },
      ),
      network,
      signer: harness.proverSigner,
      fraudCategory: "missingRedeemer",
      fraudulentHeaderHash: scenario.setup.headerHash,
      awaitConfirmation: true,
      requireReferenceScripts: true,
      validFrom: now > 120_000n ? now - 120_000n : 0n,
      validTo: now + 300_000n,
    });
    expect(removal.fraudCategory).toBe("missingRedeemer");
    await expect(
      harness.proverLucid.utxosAtWithUnit(
        harness.contracts.stateQueue.spendingScriptAddress,
        scenario.setup.stateQueueBlockUnit,
      ),
    ).resolves.toHaveLength(0);
  }, 1_200_000);
});
