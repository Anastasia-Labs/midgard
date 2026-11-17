import { type UTxO } from "@lucid-evolution/lucid";

import { submitCommittedFieldShapeInit } from "../src/committed-field-shape/submit-committed-field-shape-init.js";
import { requireLinearFaultThreadUtxo } from "../src/linear-fault-family.js";
import { submitLinearFaultFinalize } from "../src/linear-fault-finalize.js";
import { submitRemoveFraudulentBlock } from "../src/remove-fraudulent-block.js";
import { SignerWitnessTerminalRedeemerSchema } from "../src/signer-witness-scan/schemas.js";
import {
  SIGNER_WITNESS_TERMINAL_STEP_INDEX,
  submitSignerWitnessScan,
} from "../src/signer-witness-scan/submit-scan.js";
import { submitSignerWitnessTerminal } from "../src/signer-witness-scan/submit-terminal.js";
import {
  type SpendInputSignerMissingEvidence,
  submitSpendInputSignerMissingCancel,
  submitSpendInputSignerMissingStep01Accepted,
  submitSpendInputSignerMissingStep01Forced,
  submitSpendInputSignerMissingStep02,
} from "../src/spend-input-signer-missing/index.js";
import { type SubmitStep01TxInclusion } from "../src/step-support.js";
import {
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
      certificateReferenceScriptUtxo: certificateReference,
    });
  /** One turn of the shared scan: the opening turn or a resume. */
  const scan = (
    threadOutRef: string,
    evidence: SpendInputSignerMissingEvidence,
    nativeTxCompactCbor: string,
    witnessSetCompactCbor: string,
  ) =>
    submitSignerWitnessScan({
      lucid,
      family: FAMILY,
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
  const terminal = (
    threadOutRef: string,
    evidence: SpendInputSignerMissingEvidence,
  ) =>
    submitSignerWitnessTerminal({
      lucid,
      family: FAMILY,
      contracts,
      categoryId,
      signer,
      threadOutRef,
      evidence,
      referenceScriptUtxo: references[3]!,
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
   * The generic finalizer over a terminal thread, with no family evidence in
   * the way: this is what an honest terminal has to be refused by, on chain.
   */
  const finalizeDirect = async (threadOutRef: string) => {
    const { threadUtxo, threadToken } = await requireLinearFaultThreadUtxo({
      lucid,
      contracts,
      categoryId,
      family: FAMILY,
      stepIndex: SIGNER_WITNESS_TERMINAL_STEP_INDEX,
      threadOutRef,
    });
    return submitLinearFaultFinalize({
      lucid,
      family: FAMILY,
      stepIndex: SIGNER_WITNESS_TERMINAL_STEP_INDEX,
      step: contracts.steps[SIGNER_WITNESS_TERMINAL_STEP_INDEX],
      computationThread: contracts.computationThread,
      fraudProof: contracts.fraudProof,
      signer,
      threadUtxo,
      threadToken,
      spendRedeemerSchema: SignerWitnessTerminalRedeemerSchema,
      buildFamilyArgs: ({
        inputIndex,
        outputIndex,
        fraudProofMintRedeemerIndex,
      }) => ({
        input_index: inputIndex,
        output_index: outputIndex,
        fraud_proof_mint_redeemer_index: fraudProofMintRedeemerIndex,
      }),
      referenceScriptUtxo: references[SIGNER_WITNESS_TERMINAL_STEP_INDEX]!,
      witnessReferenceScripts: harness.witnessReferenceScripts,
      awaitConfirmation: true,
    });
  };
  /** Runs the scan from its opening turn to the terminal, returning every
   * capture. */
  const scanToTerminal = async (
    threadOutRef: string,
    evidence: SpendInputSignerMissingEvidence,
    nativeTxCompactCbor: string,
    witnessSetCompactCbor: string,
  ) => {
    const captureTurn = (outRef: string) =>
      captureEmulatorSubmission(harness.emulator, () =>
        scan(outRef, evidence, nativeTxCompactCbor, witnessSetCompactCbor),
      );
    const scans: Awaited<ReturnType<typeof captureTurn>>[] = [];
    let outRef = threadOutRef;
    for (;;) {
      const turn = await captureTurn(outRef);
      scans.push(turn);
      outRef = turn.result.nextThreadOutRef;
      if (turn.result.stage === "terminal") break;
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
    scan,
    terminal,
    cancel,
    finalizeDirect,
    scanToTerminal,
    removal,
  };
};
