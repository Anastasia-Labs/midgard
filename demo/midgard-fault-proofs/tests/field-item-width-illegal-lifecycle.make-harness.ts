import {
  AddressData,
  addressDataFromBech32,
  type FieldOpening,
} from "@al-ft/midgard-sdk";
import { Data, type UTxO } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { expect } from "vitest";

import { submitCommittedFieldShapeInit } from "../src/committed-field-shape/submit-committed-field-shape-init.js";
import {
  applyFieldItemWidthIllegalScripts,
  FIELD_ITEM_WIDTH_ILLEGAL_BLUEPRINT_TITLES,
  type FieldItemWidthEvidence,
  type FieldItemWidthFinding,
  type FieldItemWidthIllegalContracts,
  submitFieldItemWidthIllegalCancel,
  submitFieldItemWidthIllegalStep01Accepted,
  submitFieldItemWidthIllegalStep01Forced,
  submitFieldItemWidthIllegalStep02,
  submitFieldItemWidthIllegalStep03,
} from "../src/field-item-width-illegal/index.js";
import { submitRemoveFraudulentBlock } from "../src/remove-fraudulent-block.js";
import type { SubmitStep01TxInclusion } from "../src/step-support.js";
import {
  CANCELLABLE_STEPS,
  CATEGORY_ID,
  coverage,
  network,
  record,
} from "./field-item-width-illegal-lifecycle.record.js";
import { makeFaultProofEmulatorHarness } from "./support/emulator/harness.js";
import { captureEmulatorSubmission } from "./support/emulator/measurement.js";
import { publishPlainReferenceScriptUtxo } from "./support/emulator/reference-scripts.js";
import {
  expectRegisteredChainParity,
  familyStepsFromRegisteredChain,
} from "./support/emulator/registered-chain.js";
import { buildRemovalDeploymentInfo } from "./support/emulator/removal-deployment.js";
import {
  compactCborHex,
  submitWidthStep02Raw,
  submitWidthStep03Raw,
  type WidthShape,
  witnessSetCompactCborHex,
} from "./support/field-item-width-illegal-shapes.js";
import { publishRemovalReferenceScripts } from "./support/submit-init-emulator-shared.js";

export let publicationsRecorded = false;

/**
 * The registered chain is the deployed identity: the harness folds its first
 * step into the catalogue root. The family-side application must reproduce it
 * step for step before the suite drives it.
 */
export const makeHarness = async () => {
  const harness = await makeFaultProofEmulatorHarness({
    contractOptions: {
      realFieldItemWidthIllegal: true,
      alwaysFraudProofCatalogue: true,
    },
  });
  const addressData = await Effect.runPromise(
    addressDataFromBech32(
      harness.contracts.fraudProof.spendingScriptAddress,
    ).pipe(Effect.map((address) => Data.from(Data.to(address, AddressData)))),
  );
  const registered =
    harness.contracts.fraudProofContracts.fieldItemWidthIllegal;
  const category = harness.catalogue.categories.fieldItemWidthIllegal;
  if (category === undefined) throw new Error("width category absent");
  expect(category.categoryId).toBe(CATEGORY_ID);
  expectRegisteredChainParity({
    registered,
    applied: applyFieldItemWidthIllegalScripts({
      blueprint: harness.realBlueprint,
      network,
      computationThreadPolicyId: harness.contracts.computationThread.policyId,
      fraudProofPolicyId: harness.contracts.fraudProof.policyId,
      fraudProofTokenAddressData: addressData,
      fieldPreimageCertificatePolicyId:
        harness.contracts.fieldPreimageCertificate.policyId,
      hubOracleScriptHash: harness.contracts.hubOracle.spendingScriptHash,
    }),
    category,
  });
  const steps = familyStepsFromRegisteredChain(
    registered.steps,
    Object.values(FIELD_ITEM_WIDTH_ILLEGAL_BLUEPRINT_TITLES),
  );
  const contracts: FieldItemWidthIllegalContracts = {
    steps,
    computationThread: harness.contracts.computationThread,
    fraudProof: harness.contracts.fraudProof,
    hubOraclePolicyId: harness.contracts.hubOracle.policyId,
    stateQueuePolicyId: harness.contracts.stateQueue.policyId,
    fieldPreimageCertificatePolicyId:
      harness.contracts.fieldPreimageCertificate.policyId,
    fieldPreimageCertificateMintingScript:
      harness.contracts.fieldPreimageCertificate.mintingScript,
  };
  const catalogue = harness.catalogue;
  const references: UTxO[] = [];
  let certificateReference: UTxO | undefined;
  /**
   * Published only after the block setup: the setup mint policies are
   * parameterized on the funder's nonce UTxO, which any earlier funder
   * transaction would consume.
   */
  const publishReferences = async () => {
    if (references.length > 0) return;
    for (const [index, step] of steps.entries()) {
      const published = await captureEmulatorSubmission(harness.emulator, () =>
        publishPlainReferenceScriptUtxo({
          lucid: harness.funderLucid,
          script: step.spendingScript,
          label: `width-step-${(index + 1).toString()}`,
        }),
      );
      if (!publicationsRecorded) {
        record(
          `publish-step0${(index + 1).toString()}`,
          "fully applied testnet validator",
          published.measurement,
          "publication",
        );
      }
      references.push(published.result.utxo);
    }
    publicationsRecorded = true;
    certificateReference = (
      await publishPlainReferenceScriptUtxo({
        lucid: harness.funderLucid,
        script: harness.contracts.fieldPreimageCertificate.mintingScript,
        label: "width-certificate",
      })
    ).utxo;
  };
  const requireCertificateReference = (): UTxO => {
    if (certificateReference === undefined)
      throw new Error("references not published");
    return certificateReference;
  };

  const init = (fraudulentBlockOutRef: string) =>
    captureEmulatorSubmission(harness.emulator, () =>
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
          root: catalogue.root,
        },
        signer: harness.proverSigner,
        fraudulentBlockOutRef,
        witnessReferenceScripts: harness.witnessReferenceScripts,
      }),
    );
  type Initialized = Awaited<ReturnType<typeof init>>["result"];
  const threadOf = (initialized: Initialized) =>
    `${initialized.txHash}#${initialized.firstStepOutputIndex.toString()}`;
  const step01Accepted = async (
    initialized: Initialized,
    finding: FieldItemWidthFinding,
    txInclusion: SubmitStep01TxInclusion,
    stateQueueBlockOutRef: string,
  ) => {
    const [threadUtxo] = await harness.proverLucid.utxosByOutRef([
      {
        txHash: initialized.txHash,
        outputIndex: initialized.firstStepOutputIndex,
      },
    ]);
    if (threadUtxo === undefined) throw new Error("init thread absent");
    return captureEmulatorSubmission(harness.emulator, () =>
      submitFieldItemWidthIllegalStep01Accepted({
        lucid: harness.proverLucid,
        blueprint: harness.realBlueprint,
        network,
        contracts,
        signer: harness.proverSigner,
        finding,
        threadUtxo,
        threadToken: {
          unit: initialized.computationThreadUnit,
          fraudulentHeaderHash: initialized.fraudulentHeaderHash,
        },
        stateQueueBlockOutRef,
        txInclusion,
        referenceScriptUtxo: references[0]!,
        witnessReferenceScripts: harness.witnessReferenceScripts,
      }),
    );
  };
  const step01Forced = (
    threadOutRef: string,
    finding: FieldItemWidthFinding,
    forcedSource: Readonly<Record<string, unknown>>,
  ) =>
    captureEmulatorSubmission(harness.emulator, () =>
      submitFieldItemWidthIllegalStep01Forced({
        lucid: harness.proverLucid,
        contracts,
        categoryId: category.categoryId,
        signer: harness.proverSigner,
        threadOutRef,
        finding,
        forcedSource,
        referenceScriptUtxo: references[0]!,
      }),
    );
  const step02 = (
    threadOutRef: string,
    evidence: FieldItemWidthEvidence,
    shape: WidthShape,
  ) =>
    captureEmulatorSubmission(harness.emulator, () =>
      submitFieldItemWidthIllegalStep02({
        lucid: harness.proverLucid,
        contracts,
        categoryId: category.categoryId,
        signer: harness.proverSigner,
        threadOutRef,
        evidence,
        nativeTxCompactCbor: compactCborHex(
          shape.nativeTx,
          evidence.subject.source_kind,
        ),
        witnessSetCompactCbor: witnessSetCompactCborHex(shape.nativeTx),
        referenceScriptUtxo: references[1]!,
        certificateReferenceScriptUtxo: requireCertificateReference(),
      }),
    );
  const step02Raw = (
    threadOutRef: string,
    evidence: FieldItemWidthEvidence,
    shape: WidthShape,
    options: {
      readonly mutateOpening?: (
        opening: FieldOpening,
        referenceInputs: readonly UTxO[],
      ) => FieldOpening;
      readonly nextStepIndex?: 1 | 2;
    },
  ) =>
    submitWidthStep02Raw({
      lucid: harness.proverLucid,
      contracts,
      categoryId: category.categoryId,
      signer: harness.proverSigner,
      threadOutRef,
      evidence,
      nativeTxCompactCbor: compactCborHex(
        shape.nativeTx,
        evidence.subject.source_kind,
      ),
      witnessSetCompactCbor: witnessSetCompactCborHex(shape.nativeTx),
      referenceScriptUtxo: references[1]!,
      certificateReferenceScriptUtxo: requireCertificateReference(),
      ...options,
    });
  const step03 = (threadOutRef: string, evidence: FieldItemWidthEvidence) =>
    captureEmulatorSubmission(harness.emulator, () =>
      submitFieldItemWidthIllegalStep03({
        lucid: harness.proverLucid,
        contracts,
        categoryId: category.categoryId,
        signer: harness.proverSigner,
        threadOutRef,
        evidence,
        referenceScriptUtxo: references[2]!,
        witnessReferenceScripts: harness.witnessReferenceScripts,
      }),
    );
  const step03Raw = (threadOutRef: string) =>
    submitWidthStep03Raw({
      lucid: harness.proverLucid,
      contracts,
      categoryId: category.categoryId,
      signer: harness.proverSigner,
      threadOutRef,
      referenceScriptUtxo: references[2]!,
      witnessReferenceScripts: harness.witnessReferenceScripts,
    });
  const cancel = async (threadOutRef: string, stepIndex: 0 | 1 | 2) => {
    const cancelled = await captureEmulatorSubmission(harness.emulator, () =>
      submitFieldItemWidthIllegalCancel({
        lucid: harness.proverLucid,
        contracts,
        categoryId: category.categoryId,
        signer: harness.proverSigner,
        threadOutRef,
        referenceScriptUtxo: references[stepIndex]!,
        witnessReferenceScripts: harness.witnessReferenceScripts,
      }),
    );
    coverage.cancelled(CANCELLABLE_STEPS[stepIndex]);
    return cancelled;
  };
  const removal = async (fraudulentHeaderHash: string) => {
    const removalReferences = await publishRemovalReferenceScripts({
      lucid: harness.proverLucid,
      contracts: harness.contracts,
    });
    // A registered family resolves removal through the canonical catalogue.
    const deploymentInfo = buildRemovalDeploymentInfo(
      harness.contracts,
      catalogue,
      { removalReferenceScripts: removalReferences.published },
    );
    const now = BigInt(harness.emulator.now());
    const removed = await captureEmulatorSubmission(harness.emulator, () =>
      submitRemoveFraudulentBlock({
        lucid: harness.proverLucid,
        blueprint: harness.realBlueprint,
        deploymentInfo,
        network,
        signer: harness.proverSigner,
        fraudCategory: "fieldItemWidthIllegal",
        fraudulentHeaderHash,
        awaitConfirmation: true,
        requireReferenceScripts: true,
        validFrom: now > 120_000n ? now - 120_000n : 0n,
        validTo: now + 300_000n,
      }),
    );
    expect(removed.result.fraudCategoryId).toBe(CATEGORY_ID);
    expect(removed.result.transactions.map(({ kind }) => kind)).toEqual([
      "remove-target",
    ]);
    coverage.scenario("permanent_proof_token_and_descendant_removal");
    return removed;
  };
  return {
    harness,
    contracts,
    catalogue,
    category,
    publishReferences,
    init,
    threadOf,
    step01Accepted,
    step01Forced,
    step02,
    step02Raw,
    step03,
    step03Raw,
    cancel,
    removal,
  };
};
