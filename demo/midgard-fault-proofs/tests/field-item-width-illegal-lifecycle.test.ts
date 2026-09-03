import {
  deriveMidgardNativeTxWitnessSetCompact,
  encodeMidgardNativeTxWitnessSetCompact,
  midgardFieldCommitment,
} from "@al-ft/midgard-core";
import {
  acceptedVerdictSubject,
  forcedVerdictSubject,
} from "@al-ft/midgard-sdk";
import { getAddressDetails, type UTxO } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import { submitCommittedFieldShapeInit } from "../src/committed-field-shape/submit-committed-field-shape-init.js";
import type { FieldItemWidthIllegalContracts } from "../src/field-item-width-illegal/contracts-v1.js";
import {
  prepareFieldItemWidthEvidence,
  submitFieldItemWidthIllegalCancel,
  submitFieldItemWidthIllegalStep01Accepted,
  submitFieldItemWidthIllegalStep01Forced,
  submitFieldItemWidthIllegalStep02,
  submitFieldItemWidthIllegalStep03,
} from "../src/field-item-width-illegal/index.js";
import { submitRemoveFraudulentBlock } from "../src/remove-fraudulent-block.js";
import { buildForcedTransactionLeafMembershipProof } from "../src/transition-trace/witnesses.js";
import {
  type CommittedFieldShapeEmulatorHarness,
  setupCommittedFieldShapeScenario,
} from "./support/committed-field-shape-emulator-v1.js";
import { network } from "./support/emulator/blueprints.js";
import { alignUnixTimeToEmulatorSlotBoundary } from "./support/emulator/emulator-context.js";
import { makeFaultProofEmulatorHarness } from "./support/emulator/harness.js";
import { captureEmulatorSubmission } from "./support/emulator/measurement.js";
import { publishPlainReferenceScriptUtxo } from "./support/emulator/reference-scripts.js";
import { buildRemovalDeploymentInfo } from "./support/emulator/removal-deployment.js";
import { submitSetupTx } from "./support/emulator/setup-tx.js";
import { buildInvalidForcedTransitionTraceFixture } from "./support/submit-init-emulator-fixtures.js";
import { publishRemovalReferenceScripts } from "./support/submit-init-emulator-shared.js";

describe("field-item-width-illegal accepted lifecycle", () => {
  it("runs real Init through proof mint for a maximum-width output", async () => {
    const harness = await makeFaultProofEmulatorHarness({
      contractOptions: {
        realFieldItemWidthIllegal: true,
        alwaysFraudProofCatalogue: true,
      },
    });
    const chain = harness.contracts.fraudProofContracts.fieldItemWidthIllegal;
    const category = harness.catalogue.categories.fieldItemWidthIllegal;
    if (category === undefined) throw new Error("width category absent");
    const contracts: FieldItemWidthIllegalContracts = {
      steps: chain.steps as never,
      computationThread: harness.contracts.computationThread,
      fraudProof: harness.contracts.fraudProof,
      hubOraclePolicyId: harness.contracts.hubOracle.policyId,
      stateQueuePolicyId: harness.contracts.stateQueue.policyId,
      fieldPreimageCertificatePolicyId:
        harness.contracts.fieldPreimageCertificate.policyId,
      fieldPreimageCertificateMintingScript:
        harness.contracts.fieldPreimageCertificate.mintingScript,
    };
    const scenario = await setupCommittedFieldShapeScenario({
      harness: harness as unknown as CommittedFieldShapeEmulatorHarness,
      kind: "field-item-width-illegal",
    });
    const references: UTxO[] = [];
    for (const [index, step] of chain.steps.entries())
      references.push(
        (
          await publishPlainReferenceScriptUtxo({
            lucid: harness.funderLucid,
            script: step.spendingScript,
            label: `width-${index.toString()}`,
          })
        ).utxo,
      );
    const certificateReference = (
      await publishPlainReferenceScriptUtxo({
        lucid: harness.funderLucid,
        script: harness.contracts.fieldPreimageCertificate.mintingScript,
        label: "width-certificate",
      })
    ).utxo;
    const evidence = prepareFieldItemWidthEvidence({
      finding: {
        subject: acceptedVerdictSubject(scenario.nativeTxId),
        fieldIndex: 2,
        itemIndex: 0,
      },
      fieldPreimage: scenario.committedPreimage,
      committedFieldHashHex: midgardFieldCommitment(
        scenario.committedPreimage,
      ).toString("hex"),
    });
    const init = await captureEmulatorSubmission(harness.emulator, () =>
      submitCommittedFieldShapeInit({
        lucid: harness.proverLucid,
        blueprint: harness.realBlueprint,
        network,
        contracts: contracts as never,
        category,
        catalogue: {
          policyId: harness.contracts.fraudProofCatalogue.policyId,
          spendingScriptAddress:
            harness.contracts.fraudProofCatalogue.spendingScriptAddress,
          root: harness.catalogue.root,
        },
        signer: harness.proverSigner,
        fraudulentBlockOutRef: scenario.setup.fraudulentBlockOutRef,
        witnessReferenceScripts: harness.witnessReferenceScripts,
      }),
    );
    const [threadUtxo] = await harness.proverLucid.utxosByOutRef([
      {
        txHash: init.result.txHash,
        outputIndex: init.result.firstStepOutputIndex,
      },
    ]);
    if (threadUtxo === undefined) throw new Error("init thread absent");
    const step01 = await captureEmulatorSubmission(harness.emulator, () =>
      submitFieldItemWidthIllegalStep01Accepted({
        lucid: harness.proverLucid,
        blueprint: harness.realBlueprint,
        network,
        contracts,
        signer: harness.proverSigner,
        finding: evidence,
        threadUtxo,
        threadToken: {
          unit: init.result.computationThreadUnit,
          fraudulentHeaderHash: init.result.fraudulentHeaderHash,
        },
        stateQueueBlockOutRef: scenario.setup.fraudulentBlockOutRef,
        txInclusion: scenario.inclusion,
        referenceScriptUtxo: references[0]!,
        witnessReferenceScripts: harness.witnessReferenceScripts,
      }),
    );
    const step02 = await captureEmulatorSubmission(harness.emulator, () =>
      submitFieldItemWidthIllegalStep02({
        lucid: harness.proverLucid,
        contracts,
        categoryId: category.categoryId,
        signer: harness.proverSigner,
        threadOutRef: step01.result.nextThreadOutRef,
        evidence,
        nativeTxCompactCbor: scenario.compactCbor,
        witnessSetCompactCbor: encodeMidgardNativeTxWitnessSetCompact(
          deriveMidgardNativeTxWitnessSetCompact(scenario.fullTx!.witnessSet),
        ).toString("hex"),
        referenceScriptUtxo: references[1]!,
        certificateReferenceScriptUtxo: certificateReference,
      }),
    );
    const step03 = await captureEmulatorSubmission(harness.emulator, () =>
      submitFieldItemWidthIllegalStep03({
        lucid: harness.proverLucid,
        contracts,
        categoryId: category.categoryId,
        signer: harness.proverSigner,
        threadOutRef: step02.result.nextThreadOutRef,
        evidence,
        referenceScriptUtxo: references[2]!,
        witnessReferenceScripts: harness.witnessReferenceScripts,
      }),
    );
    for (const capture of [step01, step02, step03]) {
      expect(capture.measurement.l1ByteMargin).toBeGreaterThan(0);
      expect(capture.measurement.executionMemory).toBeGreaterThan(0n);
      expect(capture.measurement.executionSteps).toBeGreaterThan(0n);
    }
    expect(step03.result.fraudProofUnit).toBeTruthy();
    const removalReferences = await publishRemovalReferenceScripts({
      lucid: harness.proverLucid,
      contracts: harness.contracts,
    });
    const now = BigInt(harness.emulator.now());
    const removal = await captureEmulatorSubmission(harness.emulator, () =>
      submitRemoveFraudulentBlock({
        lucid: harness.proverLucid,
        blueprint: harness.realBlueprint,
        deploymentInfo: buildRemovalDeploymentInfo(
          harness.contracts,
          harness.catalogue,
          { removalReferenceScripts: removalReferences.published },
        ),
        network,
        signer: harness.proverSigner,
        fraudCategory: "fieldItemWidthIllegal",
        fraudulentHeaderHash: scenario.setup.headerHash,
        awaitConfirmation: true,
        requireReferenceScripts: true,
        validFrom: now > 120_000n ? now - 120_000n : 0n,
        validTo: now + 300_000n,
      }),
    );
    expect(removal.result.fraudCategoryId).toBe("00000021");
    expect(removal.measurement.l1ByteMargin).toBeGreaterThan(0);
    expect(removal.measurement.executionMemory).toBeGreaterThan(0n);
  }, 120_000);

  it.each([0, 1, 2] as const)(
    "cancels the real computation thread at physical step %s",
    async (targetStep) => {
      const harness = await makeFaultProofEmulatorHarness({
        contractOptions: {
          realFieldItemWidthIllegal: true,
          alwaysFraudProofCatalogue: true,
        },
      });
      const chain = harness.contracts.fraudProofContracts.fieldItemWidthIllegal;
      const category = harness.catalogue.categories.fieldItemWidthIllegal;
      if (category === undefined) throw new Error("width category absent");
      const contracts: FieldItemWidthIllegalContracts = {
        steps: chain.steps as never,
        computationThread: harness.contracts.computationThread,
        fraudProof: harness.contracts.fraudProof,
        hubOraclePolicyId: harness.contracts.hubOracle.policyId,
        stateQueuePolicyId: harness.contracts.stateQueue.policyId,
        fieldPreimageCertificatePolicyId:
          harness.contracts.fieldPreimageCertificate.policyId,
        fieldPreimageCertificateMintingScript:
          harness.contracts.fieldPreimageCertificate.mintingScript,
      };
      const scenario = await setupCommittedFieldShapeScenario({
        harness: harness as unknown as CommittedFieldShapeEmulatorHarness,
        kind: "field-item-width-illegal",
      });
      const references: UTxO[] = [];
      for (const [index, step] of chain.steps.entries()) {
        references.push(
          (
            await publishPlainReferenceScriptUtxo({
              lucid: harness.funderLucid,
              script: step.spendingScript,
              label: `width-cancel-${targetStep.toString()}-${index.toString()}`,
            })
          ).utxo,
        );
      }
      const certificateReference = (
        await publishPlainReferenceScriptUtxo({
          lucid: harness.funderLucid,
          script: harness.contracts.fieldPreimageCertificate.mintingScript,
          label: `width-cancel-certificate-${targetStep.toString()}`,
        })
      ).utxo;
      const evidence = prepareFieldItemWidthEvidence({
        finding: {
          subject: acceptedVerdictSubject(scenario.nativeTxId),
          fieldIndex: 2,
          itemIndex: 0,
        },
        fieldPreimage: scenario.committedPreimage,
        committedFieldHashHex: midgardFieldCommitment(
          scenario.committedPreimage,
        ).toString("hex"),
      });
      const init = await submitCommittedFieldShapeInit({
        lucid: harness.proverLucid,
        blueprint: harness.realBlueprint,
        network,
        contracts: contracts as never,
        category,
        catalogue: {
          policyId: harness.contracts.fraudProofCatalogue.policyId,
          spendingScriptAddress:
            harness.contracts.fraudProofCatalogue.spendingScriptAddress,
          root: harness.catalogue.root,
        },
        signer: harness.proverSigner,
        fraudulentBlockOutRef: scenario.setup.fraudulentBlockOutRef,
        witnessReferenceScripts: harness.witnessReferenceScripts,
      });
      let threadOutRef = `${init.txHash}#${init.firstStepOutputIndex.toString()}`;
      if (targetStep > 0) {
        const [threadUtxo] = await harness.proverLucid.utxosByOutRef([
          {
            txHash: init.txHash,
            outputIndex: init.firstStepOutputIndex,
          },
        ]);
        if (threadUtxo === undefined) throw new Error("init thread absent");
        const step01 = await submitFieldItemWidthIllegalStep01Accepted({
          lucid: harness.proverLucid,
          blueprint: harness.realBlueprint,
          network,
          contracts,
          signer: harness.proverSigner,
          finding: evidence,
          threadUtxo,
          threadToken: {
            unit: init.computationThreadUnit,
            fraudulentHeaderHash: init.fraudulentHeaderHash,
          },
          stateQueueBlockOutRef: scenario.setup.fraudulentBlockOutRef,
          txInclusion: scenario.inclusion,
          referenceScriptUtxo: references[0]!,
          witnessReferenceScripts: harness.witnessReferenceScripts,
        });
        threadOutRef = step01.nextThreadOutRef;
      }
      if (targetStep > 1) {
        const step02 = await submitFieldItemWidthIllegalStep02({
          lucid: harness.proverLucid,
          contracts,
          categoryId: category.categoryId,
          signer: harness.proverSigner,
          threadOutRef,
          evidence,
          nativeTxCompactCbor: scenario.compactCbor,
          witnessSetCompactCbor: encodeMidgardNativeTxWitnessSetCompact(
            deriveMidgardNativeTxWitnessSetCompact(scenario.fullTx!.witnessSet),
          ).toString("hex"),
          referenceScriptUtxo: references[1]!,
          certificateReferenceScriptUtxo: certificateReference,
        });
        threadOutRef = step02.nextThreadOutRef;
      }
      const cancel = await captureEmulatorSubmission(harness.emulator, () =>
        submitFieldItemWidthIllegalCancel({
          lucid: harness.proverLucid,
          contracts,
          categoryId: category.categoryId,
          signer: harness.proverSigner,
          threadOutRef,
          referenceScriptUtxo: references[targetStep]!,
          witnessReferenceScripts: harness.witnessReferenceScripts,
        }),
      );
      expect(cancel.measurement.l1ByteMargin).toBeGreaterThan(0);
      expect(cancel.measurement.executionMemory).toBeGreaterThan(0n);
    },
    120_000,
  );
});

describe("field-item-width-illegal forced lifecycle", () => {
  it("authenticates an exact forced rejection and removes the block", async () => {
    const harness = await makeFaultProofEmulatorHarness({
      contractOptions: {
        realFieldItemWidthIllegal: true,
        alwaysFraudProofCatalogue: true,
      },
    });
    const chain = harness.contracts.fraudProofContracts.fieldItemWidthIllegal;
    const category = harness.catalogue.categories.fieldItemWidthIllegal;
    if (category === undefined) throw new Error("width category absent");
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
      fieldItemWidthIllegalCoordinate: { fieldIndex: 2, itemIndex: 0 },
    });
    const setup = await submitSetupTx({
      lucid: harness.funderLucid,
      contracts: harness.contracts,
      nonceUtxo: harness.nonceUtxo,
      catalogue: harness.catalogue,
      header: forced.header,
    });
    const contracts: FieldItemWidthIllegalContracts = {
      steps: chain.steps as never,
      computationThread: harness.contracts.computationThread,
      fraudProof: harness.contracts.fraudProof,
      hubOraclePolicyId: harness.contracts.hubOracle.policyId,
      stateQueuePolicyId: harness.contracts.stateQueue.policyId,
      fieldPreimageCertificatePolicyId:
        harness.contracts.fieldPreimageCertificate.policyId,
      fieldPreimageCertificateMintingScript:
        harness.contracts.fieldPreimageCertificate.mintingScript,
    };
    const references: UTxO[] = [];
    for (const [index, step] of chain.steps.entries())
      references.push(
        (
          await publishPlainReferenceScriptUtxo({
            lucid: harness.funderLucid,
            script: step.spendingScript,
            label: `width-forced-${index.toString()}`,
          })
        ).utxo,
      );
    const membership = await buildForcedTransactionLeafMembershipProof({
      reconstruction: forced.reconstruction,
      eventKey: forced.eventKey,
    });
    const rejectionReason = {
      FieldItemWidthIllegal: { field_index: 2n, item_index: 0n },
    } as const;
    const evidence = prepareFieldItemWidthEvidence({
      finding: {
        subject: forcedVerdictSubject({
          transactionId: forced.forcedTransaction.tx_id,
          sourceKey: membership.key,
          rejectionReason,
        }),
        fieldIndex: 2,
        itemIndex: 0,
      },
      fieldPreimage: forced.forcedNativeTx.body.outputsPreimageCbor,
      committedFieldHashHex: midgardFieldCommitment(
        forced.forcedNativeTx.body.outputsPreimageCbor,
      ).toString("hex"),
    });
    expect(evidence.decisiveFaultHolds).toBe(false);
    const init = await submitCommittedFieldShapeInit({
      lucid: harness.proverLucid,
      blueprint: harness.realBlueprint,
      network,
      contracts: contracts as never,
      category,
      catalogue: {
        policyId: harness.contracts.fraudProofCatalogue.policyId,
        spendingScriptAddress:
          harness.contracts.fraudProofCatalogue.spendingScriptAddress,
        root: harness.catalogue.root,
      },
      signer: harness.proverSigner,
      fraudulentBlockOutRef: setup.fraudulentBlockOutRef,
      witnessReferenceScripts: harness.witnessReferenceScripts,
    });
    const step01 = await captureEmulatorSubmission(harness.emulator, () =>
      submitFieldItemWidthIllegalStep01Forced({
        lucid: harness.proverLucid,
        contracts,
        categoryId: category.categoryId,
        signer: harness.proverSigner,
        threadOutRef: `${init.txHash}#${init.firstStepOutputIndex.toString()}`,
        finding: evidence,
        forcedSource: { header: forced.header, membership, direction: 1n },
        referenceScriptUtxo: references[0]!,
      }),
    );
    const step02 = await captureEmulatorSubmission(harness.emulator, () =>
      submitFieldItemWidthIllegalStep02({
        lucid: harness.proverLucid,
        contracts,
        categoryId: category.categoryId,
        signer: harness.proverSigner,
        threadOutRef: step01.result.nextThreadOutRef,
        evidence,
        nativeTxCompactCbor: forced.forcedTransaction.source.compact_cbor,
        witnessSetCompactCbor:
          forced.forcedTransaction.source.witness_set_compact_cbor,
        referenceScriptUtxo: references[1]!,
      }),
    );
    const step03 = await captureEmulatorSubmission(harness.emulator, () =>
      submitFieldItemWidthIllegalStep03({
        lucid: harness.proverLucid,
        contracts,
        categoryId: category.categoryId,
        signer: harness.proverSigner,
        threadOutRef: step02.result.nextThreadOutRef,
        evidence,
        referenceScriptUtxo: references[2]!,
        witnessReferenceScripts: harness.witnessReferenceScripts,
      }),
    );
    expect(step03.result.fraudProofUnit).toBeTruthy();
    for (const capture of [step01, step02, step03])
      expect(capture.measurement.l1ByteMargin).toBeGreaterThan(0);
    const removalReferences = await publishRemovalReferenceScripts({
      lucid: harness.proverLucid,
      contracts: harness.contracts,
    });
    const now = BigInt(harness.emulator.now());
    const removal = await captureEmulatorSubmission(harness.emulator, () =>
      submitRemoveFraudulentBlock({
        lucid: harness.proverLucid,
        blueprint: harness.realBlueprint,
        deploymentInfo: buildRemovalDeploymentInfo(
          harness.contracts,
          harness.catalogue,
          { removalReferenceScripts: removalReferences.published },
        ),
        network,
        signer: harness.proverSigner,
        fraudCategory: "fieldItemWidthIllegal",
        fraudulentHeaderHash: setup.headerHash,
        awaitConfirmation: true,
        requireReferenceScripts: true,
        validFrom: now > 120_000n ? now - 120_000n : 0n,
        validTo: now + 300_000n,
      }),
    );
    expect(removal.result.transactions.map(({ kind }) => kind)).toEqual([
      "remove-target",
    ]);
  }, 120_000);
});
