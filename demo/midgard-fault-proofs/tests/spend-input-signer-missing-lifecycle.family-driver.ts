import { type UTxO } from "@lucid-evolution/lucid";

import { submitCommittedFieldShapeInit } from "../src/committed-field-shape/submit-committed-field-shape-init.js";
import { requireLinearFaultThreadUtxo } from "../src/linear-fault-family.js";
import { submitLinearFaultFinalize } from "../src/linear-fault-finalize.js";
import { submitRemoveFraudulentBlock } from "../src/remove-fraudulent-block.js";
import {
  type SpendInputSignerMissingEvidence,
  submitSpendInputSignerMissingCancel,
  submitSpendInputSignerMissingStep01Accepted,
  submitSpendInputSignerMissingStep01Forced,
  submitSpendInputSignerMissingStep02,
  submitSpendInputSignerMissingStep03,
  submitSpendInputSignerMissingStep04,
  submitSpendInputSignerMissingStep05,
} from "../src/spend-input-signer-missing/index.js";
import { SpendInputSignerStep05RedeemerSchema } from "../src/spend-input-signer-missing/schemas.js";
import { type SubmitStep01TxInclusion } from "../src/step-support.js";
import {
  type Captured,
  FAMILY,
  type Family,
  type Harness,
  network,
} from "./spend-input-signer-missing-lifecycle.registered-contracts.js";
import { captureEmulatorSubmission } from "./support/emulator/measurement.js";
import { buildRemovalDeploymentInfo } from "./support/emulator/removal-deployment.js";
import { publishRemovalReferenceScripts } from "./support/submit-init-emulator-shared.js";

/** Thin step drivers over one harness, family, and reference set. */
export const familyDriver = (
  harness: Harness,
  family: Family,
  references: readonly UTxO[],
  certificateReference: UTxO,
) => {
  const { contracts, category } = family;
  const lucid = harness.proverLucid;
  const signer = harness.proverSigner;
  const categoryId = category.categoryId;
  const init = (blockOutRef: string) =>
    submitCommittedFieldShapeInit({
      lucid,
      blueprint: harness.realBlueprint,
      network,
      contracts: contracts as never,
      category,
      catalogue: {
        policyId: harness.contracts.fraudProofCatalogue.policyId,
        spendingScriptAddress:
          harness.contracts.fraudProofCatalogue.spendingScriptAddress,
        root: family.catalogue.root,
      },
      signer,
      fraudulentBlockOutRef: blockOutRef,
      witnessReferenceScripts: harness.witnessReferenceScripts,
    });
  const initThread = async (blockOutRef: string) => {
    const result = await init(blockOutRef);
    return {
      result,
      threadOutRef: `${result.txHash}#${result.firstStepOutputIndex.toString()}`,
    };
  };
  const threadUtxoAt = async (threadOutRef: string) => {
    const [txHash, outputIndex] = threadOutRef.split("#");
    const [utxo] = await lucid.utxosByOutRef([
      { txHash: txHash!, outputIndex: Number(outputIndex) },
    ]);
    if (utxo === undefined) throw new Error(`thread ${threadOutRef} absent`);
    return utxo;
  };
  const step01Accepted = async (
    thread: Awaited<ReturnType<typeof initThread>>,
    evidence: SpendInputSignerMissingEvidence,
    blockOutRef: string,
    txInclusion: SubmitStep01TxInclusion,
  ) =>
    submitSpendInputSignerMissingStep01Accepted({
      lucid,
      blueprint: harness.realBlueprint,
      network,
      contracts,
      signer,
      evidence,
      threadUtxo: await threadUtxoAt(thread.threadOutRef),
      threadToken: {
        unit: thread.result.computationThreadUnit,
        fraudulentHeaderHash: thread.result.fraudulentHeaderHash,
      },
      stateQueueBlockOutRef: blockOutRef,
      txInclusion,
      referenceScriptUtxo: references[0]!,
      witnessReferenceScripts: harness.witnessReferenceScripts,
    });
  const step01Forced = (
    threadOutRef: string,
    evidence: SpendInputSignerMissingEvidence,
    forcedSource: Readonly<Record<string, unknown>>,
  ) =>
    submitSpendInputSignerMissingStep01Forced({
      lucid,
      contracts,
      categoryId,
      signer,
      threadOutRef,
      evidence,
      forcedSource,
      referenceScriptUtxo: references[0]!,
    });
  const step02 = (
    threadOutRef: string,
    evidence: SpendInputSignerMissingEvidence,
    nativeTxCompactCbor: string,
    witnessSetCompactCbor: string,
  ) =>
    submitSpendInputSignerMissingStep02({
      lucid,
      network,
      contracts,
      categoryId,
      signer,
      threadOutRef,
      evidence,
      nativeTxCompactCbor,
      witnessSetCompactCbor,
      referenceScriptUtxo: references[1]!,
      membershipReferenceScriptUtxo:
        harness.witnessReferenceScripts.phasMembershipWithdraw!,
    });
  const step03 = (
    threadOutRef: string,
    evidence: SpendInputSignerMissingEvidence,
    nativeTxCompactCbor: string,
    witnessSetCompactCbor: string,
  ) =>
    submitSpendInputSignerMissingStep03({
      lucid,
      network,
      contracts,
      categoryId,
      signer,
      threadOutRef,
      evidence,
      nativeTxCompactCbor,
      witnessSetCompactCbor,
      referenceScriptUtxo: references[2]!,
      certificateReferenceScriptUtxo: certificateReference,
    });
  const step04 = (
    threadOutRef: string,
    evidence: SpendInputSignerMissingEvidence,
    nativeTxCompactCbor: string,
    witnessSetCompactCbor: string,
  ) =>
    submitSpendInputSignerMissingStep04({
      lucid,
      network,
      contracts,
      categoryId,
      signer,
      threadOutRef,
      evidence,
      nativeTxCompactCbor,
      witnessSetCompactCbor,
      referenceScriptUtxo: references[3]!,
      certificateReferenceScriptUtxo: certificateReference,
    });
  const step05 = (
    threadOutRef: string,
    evidence: SpendInputSignerMissingEvidence,
  ) =>
    submitSpendInputSignerMissingStep05({
      lucid,
      contracts,
      categoryId,
      signer,
      threadOutRef,
      evidence,
      referenceScriptUtxo: references[4]!,
      witnessReferenceScripts: harness.witnessReferenceScripts,
    });
  const cancel = (threadOutRef: string, stepIndex: number) =>
    submitSpendInputSignerMissingCancel({
      lucid,
      contracts,
      categoryId,
      signer,
      threadOutRef,
      referenceScriptUtxo: references[stepIndex]!,
      witnessReferenceScripts: harness.witnessReferenceScripts,
    });
  /**
   * The generic finalizer over a step-05 thread, with no family evidence in
   * the way: this is what an honest terminal has to be refused by, on chain.
   */
  const finalizeDirect = async (threadOutRef: string) => {
    const { threadUtxo, threadToken } = await requireLinearFaultThreadUtxo({
      lucid,
      contracts,
      categoryId,
      family: FAMILY,
      stepIndex: 4,
      threadOutRef,
    });
    return submitLinearFaultFinalize({
      lucid,
      family: FAMILY,
      stepIndex: 4,
      step: contracts.steps[4],
      computationThread: contracts.computationThread,
      fraudProof: contracts.fraudProof,
      signer,
      threadUtxo,
      threadToken,
      spendRedeemerSchema: SpendInputSignerStep05RedeemerSchema,
      buildFamilyArgs: ({
        inputIndex,
        outputIndex,
        fraudProofMintRedeemerIndex,
      }) => ({
        input_index: inputIndex,
        output_index: outputIndex,
        fraud_proof_mint_redeemer_index: fraudProofMintRedeemerIndex,
      }),
      referenceScriptUtxo: references[4]!,
      witnessReferenceScripts: harness.witnessReferenceScripts,
      awaitConfirmation: true,
    });
  };
  /** Runs step 04 to its terminal, returning every capture. */
  const scanToTerminal = async (
    threadOutRef: string,
    evidence: SpendInputSignerMissingEvidence,
    nativeTxCompactCbor: string,
    witnessSetCompactCbor: string,
  ) => {
    const scans: Captured[] = [];
    let outRef = threadOutRef;
    for (;;) {
      const scan = await captureEmulatorSubmission(harness.emulator, () =>
        step04(outRef, evidence, nativeTxCompactCbor, witnessSetCompactCbor),
      );
      scans.push(scan);
      outRef = scan.result.nextThreadOutRef;
      if (scan.result.stage === "step05") break;
    }
    return { scans, threadOutRef: outRef };
  };
  const removal = async (headerHash: string) => {
    const removalReferences = await publishRemovalReferenceScripts({
      lucid,
      contracts: harness.contracts,
    });
    // A registered family resolves removal through the canonical catalogue:
    // the manifest's fraudProofSpendInputSignerMissing entries carry the
    // registered chain the harness built.
    const deploymentInfo = buildRemovalDeploymentInfo(
      harness.contracts,
      family.catalogue,
      { removalReferenceScripts: removalReferences.published },
    );
    const now = BigInt(harness.emulator.now());
    return captureEmulatorSubmission(harness.emulator, () =>
      submitRemoveFraudulentBlock({
        lucid,
        blueprint: harness.realBlueprint,
        deploymentInfo,
        network,
        signer,
        fraudCategory: "spendInputSignerMissing",
        fraudulentHeaderHash: headerHash,
        requireReferenceScripts: true,
        awaitConfirmation: true,
        validFrom: now > 120_000n ? now - 120_000n : 0n,
        validTo: now + 300_000n,
      }),
    );
  };
  return {
    initThread,
    step01Accepted,
    step01Forced,
    step02,
    step03,
    step04,
    step05,
    cancel,
    finalizeDirect,
    scanToTerminal,
    removal,
  };
};
