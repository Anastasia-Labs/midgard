import { AddressData, addressDataFromBech32 } from "@al-ft/midgard-sdk";
import { Data, type UTxO } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { expect } from "vitest";

import { submitCommittedFieldShapeInit } from "../src/committed-field-shape/submit-committed-field-shape-init.js";
import {
  certifyFaultProofFieldCarriage,
  planFaultProofFieldOpening,
  publishFaultProofFieldCarriage,
} from "../src/field-opening.js";
import {
  applyObserversForbiddenScripts,
  OBSERVERS_FORBIDDEN_BLUEPRINT_TITLES,
  type ObserversForbiddenContracts,
} from "../src/observers-forbidden-on-untagged-network/contracts.js";
import {
  type ObserversForbiddenEvidence,
  type ObserversForbiddenFinding,
} from "../src/observers-forbidden-on-untagged-network/family.js";
import { submitObserversForbiddenCancel } from "../src/observers-forbidden-on-untagged-network/submit-cancel.js";
import {
  submitObserversForbiddenStep01Accepted,
  submitObserversForbiddenStep01Forced,
} from "../src/observers-forbidden-on-untagged-network/submit-step-01.js";
import { submitObserversForbiddenStep02 } from "../src/observers-forbidden-on-untagged-network/submit-step-02.js";
import { submitRemoveFraudulentBlock } from "../src/remove-fraudulent-block.js";
import type { SubmitStep01TxInclusion } from "../src/step-support.js";
import {
  CANCELLABLE_STEPS,
  CATEGORY_ID,
  coverage,
  MAXIMUM_CHUNKS,
  network,
  record,
} from "./observers-forbidden-on-untagged-network-lifecycle.record.js";
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
  observerHashes,
  type ObserverShape,
  submitObserversForbiddenStep01ForcedRaw,
  submitObserversForbiddenStep02Raw,
  transactionIdOf,
  witnessSetCompactCborHex,
} from "./support/observers-forbidden-on-untagged-network-raw.js";
import { publishRemovalReferenceScripts } from "./support/submit-init-emulator-shared.js";

export let publicationsRecorded = false;

// ---------------------------------------------------------------------------
// Harness
// ---------------------------------------------------------------------------

/**
 * The registered chain is the deployed identity: the harness folds its first
 * step into the catalogue root. The family-side application must reproduce it
 * step for step before the suite drives it.
 */
export const makeHarness = async () => {
  const harness = await makeFaultProofEmulatorHarness({
    contractOptions: {
      realObserversForbiddenOnUntaggedNetwork: true,
      alwaysFraudProofCatalogue: true,
      alwaysStateQueue: true,
    },
  });
  const addressData = await Effect.runPromise(
    addressDataFromBech32(
      harness.contracts.fraudProof.spendingScriptAddress,
    ).pipe(Effect.map((address) => Data.from(Data.to(address, AddressData)))),
  );
  const registered =
    harness.contracts.fraudProofContracts.observersForbiddenOnUntaggedNetwork;
  const category =
    harness.catalogue.categories.observersForbiddenOnUntaggedNetwork;
  if (category === undefined) throw new Error("observers category absent");
  expect(category.categoryId).toBe(CATEGORY_ID);
  expectRegisteredChainParity({
    registered,
    applied: applyObserversForbiddenScripts({
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
    OBSERVERS_FORBIDDEN_BLUEPRINT_TITLES,
  );
  const contracts: ObserversForbiddenContracts = {
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
          label: `observers-step-${(index + 1).toString()}`,
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
        label: "observers-certificate",
      })
    ).utxo;
  };
  const requireCertificateReference = (): UTxO => {
    if (certificateReference === undefined)
      throw new Error("references not published");
    return certificateReference;
  };
  const stepReferences = () => references as unknown as readonly [UTxO, UTxO];

  /**
   * Publish (and certify, when the tier requires it) the field-3 carriage
   * the way the production builder expects to resolve it. Returns the chunk
   * and certificate measurements of a certified publication.
   */
  const publishField = async (
    shape: ObserverShape,
    sourceKind: 0n | 1n = 0n,
  ) => {
    const planned = planFaultProofFieldOpening({
      anchorSourceKind: sourceKind,
      fieldIndex: 3,
      anchorTxId: transactionIdOf(shape),
      nativeTxCompactCbor: compactCborHex(shape.nativeTx, sourceKind),
      itemCbors: observerHashes(shape.observerCount),
      owner: harness.proverSigner.paymentKeyHash,
      publish: true,
      label: `observers field ${shape.label}`,
    });
    if (planned.plan.tier !== "Certified") {
      await publishFaultProofFieldCarriage({
        lucid: harness.proverLucid,
        signer: harness.proverSigner,
        planned,
        publisherAddress: harness.proverSigner.address,
        label: `observers field ${shape.label}`,
      });
      return { tier: planned.plan.tier, chunks: [], certificate: undefined };
    }
    const carriage = await captureEmulatorSubmission(harness.emulator, () =>
      publishFaultProofFieldCarriage({
        lucid: harness.proverLucid,
        signer: harness.proverSigner,
        planned,
        publisherAddress: harness.proverSigner.address,
        label: `observers field ${shape.label}`,
      }),
    );
    const certificate = await captureEmulatorSubmission(harness.emulator, () =>
      certifyFaultProofFieldCarriage({
        lucid: harness.proverLucid,
        network,
        signer: harness.proverSigner,
        planned,
        certificatePolicyId:
          harness.contracts.fieldPreimageCertificate.policyId,
        certificateMintingScript:
          harness.contracts.fieldPreimageCertificate.mintingScript,
        certificateReferenceScriptUtxo: requireCertificateReference(),
        chunkUtxos: carriage.result,
        compactCbor: compactCborHex(shape.nativeTx, sourceKind),
        witnessSetCompactCbor: witnessSetCompactCborHex(shape.nativeTx),
      }),
    );
    return {
      tier: planned.plan.tier,
      chunks: carriage.measurements,
      certificate: certificate.measurement,
    };
  };
  const recordCarriage = (
    prefix: string,
    shape: ObserverShape,
    published: Awaited<ReturnType<typeof publishField>>,
  ) => {
    expect(published.tier).toBe("Certified");
    expect(published.chunks).toHaveLength(MAXIMUM_CHUNKS);
    published.chunks.forEach((measurement, index) =>
      record(
        `${prefix}-carriage-chunk0${(index + 1).toString()}`,
        shape.label,
        measurement,
      ),
    );
    record(
      `${prefix}-carriage-certificate`,
      shape.label,
      published.certificate!,
    );
  };

  const init = (fraudulentBlockOutRef: string, fraudulentHeaderHash: string) =>
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
        fraudulentHeaderHash,
        witnessReferenceScripts: harness.witnessReferenceScripts,
      }),
    );
  type Initialized = Awaited<ReturnType<typeof init>>["result"];
  const threadOf = (initialized: Initialized) =>
    `${initialized.txHash}#${initialized.firstStepOutputIndex.toString()}`;
  const threadUtxoOf = async (initialized: Initialized) => {
    const [threadUtxo] = await harness.proverLucid.utxosByOutRef([
      {
        txHash: initialized.txHash,
        outputIndex: initialized.firstStepOutputIndex,
      },
    ]);
    if (threadUtxo === undefined) throw new Error("init thread absent");
    return threadUtxo;
  };
  const step01Accepted = async (
    initialized: Initialized,
    finding: ObserversForbiddenFinding,
    txInclusion: SubmitStep01TxInclusion,
    stateQueueBlockOutRef: string,
  ) =>
    captureEmulatorSubmission(harness.emulator, async () =>
      submitObserversForbiddenStep01Accepted({
        lucid: harness.proverLucid,
        blueprint: harness.realBlueprint,
        network,
        contracts,
        signer: harness.proverSigner,
        finding,
        threadUtxo: await threadUtxoOf(initialized),
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
  const step01Forced = (
    threadOutRef: string,
    finding: ObserversForbiddenFinding,
    forcedSource: Readonly<Record<string, unknown>>,
  ) =>
    captureEmulatorSubmission(harness.emulator, () =>
      submitObserversForbiddenStep01Forced({
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
  const step01ForcedRaw = (
    threadOutRef: string,
    finding: ObserversForbiddenFinding,
    forcedSource: Readonly<Record<string, unknown>>,
    nextStepIndex: 0 | 1,
  ) =>
    submitObserversForbiddenStep01ForcedRaw({
      lucid: harness.proverLucid,
      contracts,
      categoryId: category.categoryId,
      signer: harness.proverSigner,
      threadOutRef,
      finding,
      forcedSource,
      referenceScriptUtxo: references[0]!,
      nextStepIndex,
    });
  const step02 = (
    threadOutRef: string,
    evidence: ObserversForbiddenEvidence,
    shape: ObserverShape,
  ) =>
    captureEmulatorSubmission(harness.emulator, () =>
      submitObserversForbiddenStep02({
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
        referenceScriptUtxo: references[1]!,
        witnessReferenceScripts: harness.witnessReferenceScripts,
      }),
    );
  const step02Raw = (
    threadOutRef: string,
    evidence: ObserversForbiddenEvidence,
    shape: ObserverShape,
    mutateOpening?: Parameters<
      typeof submitObserversForbiddenStep02Raw
    >[0]["mutateOpening"],
  ) =>
    submitObserversForbiddenStep02Raw({
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
      referenceScriptUtxo: references[1]!,
      witnessReferenceScripts: harness.witnessReferenceScripts,
      mutateOpening,
    });
  const cancel = async (threadOutRef: string, stepIndex: 0 | 1) => {
    const cancelled = await captureEmulatorSubmission(harness.emulator, () =>
      submitObserversForbiddenCancel({
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
  const removalDeploymentInfo = async () => {
    const removalReferences = await publishRemovalReferenceScripts({
      lucid: harness.proverLucid,
      contracts: harness.contracts,
    });
    // A registered family resolves removal through the canonical catalogue:
    // the manifest's fraudProofObserversForbiddenOnUntaggedNetwork entries
    // carry the registered chain the harness built.
    return buildRemovalDeploymentInfo(harness.contracts, catalogue, {
      removalReferenceScripts: removalReferences.published,
    });
  };
  const removal = async (fraudulentHeaderHash: string) => {
    const deploymentInfo = await removalDeploymentInfo();
    const now = BigInt(harness.emulator.now());
    const removed = await captureEmulatorSubmission(harness.emulator, () =>
      submitRemoveFraudulentBlock({
        lucid: harness.proverLucid,
        blueprint: harness.realBlueprint,
        deploymentInfo,
        network,
        signer: harness.proverSigner,
        fraudCategory: "observersForbiddenOnUntaggedNetwork",
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
    requireCertificateReference,
    stepReferences,
    publishField,
    recordCarriage,
    init,
    threadOf,
    step01Accepted,
    step01Forced,
    step01ForcedRaw,
    step02,
    step02Raw,
    cancel,
    removalDeploymentInfo,
    removal,
  };
};
