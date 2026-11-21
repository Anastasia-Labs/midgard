import "./redeemer-canonicity-lifecycle.redeemer-canonicity-accepted-lifecycle.js";

import {
  decodeMidgardForcedTxCompact,
  midgardFieldCommitment,
} from "@al-ft/midgard-core";
import { forcedVerdictSubject } from "@al-ft/midgard-sdk/fraud-proof";
import { getAddressDetails } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import { prepareRedeemerCanonicityEvidence } from "../src/redeemer-canonicity/family.js";
import { buildRedeemerCanonicityStep01Forced } from "../src/redeemer-canonicity/submit-step-01.js";
import { buildRedeemerCanonicityStep02 } from "../src/redeemer-canonicity/submit-step-02.js";
import { buildRedeemerCanonicityStep03 } from "../src/redeemer-canonicity/submit-step-03.js";
import { buildForcedTransactionLeafMembershipProof } from "../src/transition-trace/witnesses.build-source-membership-proof.js";
import {
  emitFit,
  familyContracts,
  initThread,
  publishFamilyReferences,
  readRedeemerData,
} from "./redeemer-canonicity-lifecycle.init-thread.js";
import { alignUnixTimeToEmulatorSlotBoundary } from "./support/emulator/emulator-context.js";
import { makeFaultProofEmulatorHarness } from "./support/emulator/harness.js";
import { captureEmulatorSubmission } from "./support/emulator/measurement.js";
import { submitSetupTx } from "./support/emulator/setup-tx.js";
import { buildInvalidForcedTransitionTraceFixture } from "./support/submit-init/forced-transitions.js";
import {
  buildAndSubmitUnjournaled,
  submitTurnsUnjournaled,
} from "./support/submit-unjournaled.js";

describe("redeemer-canonicity forced lifecycle", () => {
  it("authenticates a canonical redeemer against the exact malformed reason", async () => {
    const harness = await makeFaultProofEmulatorHarness({
      contractOptions: {
        realRedeemerCanonicity: true,
        alwaysFraudProofCatalogue: true,
      },
    });
    const contracts = familyContracts(harness);
    const category = harness.catalogue.categories.redeemerCanonicity;
    if (category === undefined) throw new Error("redeemer category absent");
    const credential = getAddressDetails(
      await harness.funderLucid.wallet().address(),
    ).paymentCredential;
    if (credential?.type !== "Key") throw new Error("missing funder key");
    const forced = await buildInvalidForcedTransitionTraceFixture({
      operatorVkey: credential.hash,
      now:
        alignUnixTimeToEmulatorSlotBoundary(
          harness.funderLucid,
          harness.emulator.now() + 120_000,
        ) - 1,
      redeemerMalformedIndex: 0,
    });
    const setup = await submitSetupTx({
      lucid: harness.funderLucid,
      contracts: harness.contracts,
      nonceUtxo: harness.nonceUtxo,
      catalogue: harness.catalogue,
      header: forced.header,
    });
    const references = await publishFamilyReferences(harness);
    const membership = await buildForcedTransactionLeafMembershipProof({
      reconstruction: forced.reconstruction,
      eventKey: forced.eventKey,
    });
    const rejectionReason = {
      RedeemerMalformed: { redeemer_index: 0n },
    } as const;
    const evidence = prepareRedeemerCanonicityEvidence({
      finding: {
        subject: forcedVerdictSubject({
          transactionId: forced.forcedTransaction.tx_id,
          sourceKey: membership.key,
          rejectionReason,
        }),
        redeemerIndex: 0n,
      },
      fieldPreimage:
        forced.forcedNativeTx.witnessSet.redeemerTxWitsPreimageCbor,
      committedFieldHashHex: midgardFieldCommitment(
        forced.forcedNativeTx.witnessSet.redeemerTxWitsPreimageCbor,
      ).toString("hex"),
    });
    expect(evidence.canonical).toBe(true);
    const init = await initThread({
      harness,
      contracts,
      category,
      fraudulentBlockOutRef: setup.fraudulentBlockOutRef,
    });
    const step01 = await captureEmulatorSubmission(harness.emulator, () =>
      buildAndSubmitUnjournaled(buildRedeemerCanonicityStep01Forced, {
        lucid: harness.proverLucid,
        contracts,
        categoryId: category.categoryId,
        signer: harness.proverSigner,
        threadOutRef: `${init.txHash}#${init.firstStepOutputIndex.toString()}`,
        finding: evidence,
        forcedSource: {
          header: forced.header,
          membership,
        },
        witnessSetHash: Buffer.from(
          decodeMidgardForcedTxCompact(
            Buffer.from(
              forced.forcedTransaction.submitted_source.compact_cbor,
              "hex",
            ),
          ).transactionWitnessSetHash,
        ).toString("hex"),
        referenceScriptUtxo: references[0],
      }),
    );
    emitFit("forced-step01", step01.measurement);
    const step02 = await captureEmulatorSubmission(harness.emulator, () =>
      submitTurnsUnjournaled(buildRedeemerCanonicityStep02, {
        lucid: harness.proverLucid,
        contracts,
        categoryId: category.categoryId,
        signer: harness.proverSigner,
        threadOutRef: step01.result.nextThreadOutRef,
        evidence,
        nativeTxCompactCbor:
          forced.forcedTransaction.submitted_source.compact_cbor,
        witnessSetCompactCbor:
          forced.forcedTransaction.submitted_source.witness_set_compact_cbor,
        referenceScriptUtxo: references[1],
      }),
    );
    emitFit("forced-step02", step02.measurement);
    expect(step02.result.nextStage).toBe("step02Data");
    const read = await readRedeemerData({
      harness,
      contracts,
      categoryId: category.categoryId,
      threadOutRef: step02.result.nextThreadOutRef,
      evidence,
      referenceScriptUtxo: references[2],
      onStep: (measurement) => emitFit("forced-step02-data", measurement),
    });
    const step03 = await captureEmulatorSubmission(harness.emulator, () =>
      buildAndSubmitUnjournaled(buildRedeemerCanonicityStep03, {
        lucid: harness.proverLucid,
        contracts,
        categoryId: category.categoryId,
        signer: harness.proverSigner,
        threadOutRef: read.threadOutRef,
        evidence,
        referenceScriptUtxo: references[3],
        witnessReferenceScripts: harness.witnessReferenceScripts,
      }),
    );
    emitFit("forced-step03-permanent-mint", step03.measurement);
    expect(step03.result.fraudProofUnit).toBeTruthy();
    for (const capture of [step01, step02, step03]) {
      expect(capture.measurement.l1ByteMargin).toBeGreaterThan(0);
      expect(capture.measurement.executionMemory).toBeGreaterThan(0n);
    }
  }, 300_000);
});
