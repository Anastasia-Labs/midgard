import { assertSecurityGradeEvidence } from "@al-ft/midgard-sdk";

import { parseArgs } from "./bin.parse-args.js";
import { usage } from "./bin.parsed-args.js";
import {
  buildRemoveFraudulentBlockCliConfig,
  buildRemoveUnattestedBlockCliConfig,
  requireValidationOneStepCliArguments,
  writeJson,
} from "./bin.require-validation-one-step-cli-arguments.js";
import {
  diagnosticEvidenceBanner,
  LOCAL_FILE_DIAGNOSTIC_PROVENANCE,
  MIDGARD_NODE_URL_DIAGNOSTIC_PROVENANCE,
  SAMPLE_EVIDENCE_DIAGNOSTIC_PROVENANCE,
} from "./evidence/diagnostic-evidence.js";
import {
  resolveFabricatedDepositCliContracts,
  resolveFabricatedWithdrawalCliContracts,
} from "./fabricated-cli-contracts.js";
import {
  inspectContractsFromFiles,
  parseNetwork,
} from "./inspect-contracts.js";
import { rejectRetiredUnauthenticatedSubmissionRoute } from "./legacy-submission-boundary.js";
import { prepareTransitionTraceFromDaEnvelope } from "./prepare-transition-trace.js";
import { submitRemoveFraudulentBlockFromFiles } from "./remove-fraudulent-block.js";
import { submitUnattestedTimeoutCorrectionFromFiles } from "./remove-unattested-block.js";
import { submitDaHashPreimageStep01FromFiles } from "./submit-da-hash-preimage-step-01.js";
import { submitDaHashPreimageStep02FromFiles } from "./submit-da-hash-preimage-step-02.js";
import { submitFabricatedDepositStep01FromFiles } from "./submit-fabricated-deposit-step-01.js";
import { submitFabricatedDepositStep02FromFiles } from "./submit-fabricated-deposit-step-02.js";
import { submitFabricatedDepositStep03FromFiles } from "./submit-fabricated-deposit-step-03.js";
import { submitFabricatedDepositStep04FromFiles } from "./submit-fabricated-deposit-step-04.js";
import { submitFabricatedWithdrawalStep01FromFiles } from "./submit-fabricated-withdrawal-step-01.js";
import { submitFabricatedWithdrawalStep02FromFiles } from "./submit-fabricated-withdrawal-step-02.js";
import { submitFabricatedWithdrawalStep03FromFiles } from "./submit-fabricated-withdrawal-step-03.js";
import { submitFabricatedWithdrawalStep04FromFiles } from "./submit-fabricated-withdrawal-step-04.js";
import { submitInitFromFiles } from "./submit-init.js";
import { submitInputNoIdxStep01FromFiles } from "./submit-input-no-idx-step-01.js";
import { submitInputNoIdxStep02FromFiles } from "./submit-input-no-idx-step-02.js";
import { submitInputNoIdxStep03FromFiles } from "./submit-input-no-idx-step-03.js";
import { submitInputNoIdxStep04FromFiles } from "./submit-input-no-idx-step-04.js";
import { submitInvalidRangeStep01FromFiles } from "./submit-invalid-range-step-01.js";
import { submitInvalidRangeStep02FromFiles } from "./submit-invalid-range-step-02.js";
import { submitInvalidSignatureStep01FromFiles } from "./submit-invalid-signature-step-01.js";
import { submitInvalidSignatureStep02FromFiles } from "./submit-invalid-signature-step-02.js";
import { submitNoReferenceInputStep01FromFiles } from "./submit-no-reference-input-step-01.js";
import { submitNoReferenceInputStep02FromFiles } from "./submit-no-reference-input-step-02.js";
import { submitNoReferenceInputStep03FromFiles } from "./submit-no-reference-input-step-03.js";
import { submitNoReferenceInputStep04FromFiles } from "./submit-no-reference-input-step-04.js";
import { submitReferenceInputNoIdxStep01FromFiles } from "./submit-reference-input-no-idx-step-01.js";
import { submitReferenceInputNoIdxStep02FromFiles } from "./submit-reference-input-no-idx-step-02.js";
import { submitReferenceInputNoIdxStep03FromFiles } from "./submit-reference-input-no-idx-step-03.js";
import { submitReferenceInputNoIdxStep04FromFiles } from "./submit-reference-input-no-idx-step-04.js";
import { submitTransitionTraceProofFromCborFile } from "./submit-transition-trace-proof.js";
import { submitZeroInputStep01FromFiles } from "./submit-zero-input-step-01.js";
import { submitZeroInputStep02FromFiles } from "./submit-zero-input-step-02.js";
import {
  submitValidationDisputeAwardFromFiles,
  submitValidationDisputeEnterResolutionFromFiles,
  submitValidationDisputeEnterTimeoutFromFiles,
  submitValidationDisputeOpenFromFiles,
  submitValidationDisputePrepareResolutionFromFiles,
  submitValidationDisputePrepareSelectedFromFiles,
  submitValidationDisputeRevealFromFiles,
  submitValidationDisputeSemanticResolutionFromFiles,
  submitValidationDisputeTimeoutFromFiles,
  submitValidationDisputeVerifySourceFromFiles,
} from "./validation-dispute/from-files.js";
import {
  runFraudProofWorkflowCli,
  workflowReadinessReport,
} from "./workflow/cli.js";

export const main = async (): Promise<void> => {
  const args = parseArgs(process.argv);

  if (args.command === "workflow-readiness") {
    writeJson(
      workflowReadinessReport(
        args.fraudCategory === undefined ? undefined : [args.fraudCategory],
      ),
    );
    return;
  }

  if (args.command === "run-workflow" || args.command === "resume-workflow") {
    if (args.fraudCategory === undefined) {
      throw new Error(`Missing required --fraud-category.\n${usage}`);
    }
    if (args.deploymentFingerprint === undefined) {
      throw new Error(`Missing required --deployment-fingerprint.\n${usage}`);
    }
    if (args.workflowJournalDir === undefined) {
      throw new Error(`Missing required --workflow-journal-dir.\n${usage}`);
    }
    if (args.headerHash === undefined) {
      throw new Error(`Missing required --header-hash.\n${usage}`);
    }
    if (args.workflowRuntimeConfigPath === undefined) {
      throw new Error(`Missing required --workflow-runtime-config.\n${usage}`);
    }
    await runFraudProofWorkflowCli({
      mode: args.command === "run-workflow" ? "run" : "resume",
      category: args.fraudCategory,
      deploymentFingerprint: args.deploymentFingerprint,
      headerHash: args.headerHash,
      journalDirectory: args.workflowJournalDir,
      runtimeConfigPath: args.workflowRuntimeConfigPath,
    });
    return;
  }

  // Unlike the legacy caller-asserted prepare inputs below, the retained DA
  // envelope is authenticated byte-for-byte against the pinned committed
  // header hash before any proof artifact is written. Keep this security-grade
  // route ahead of the generic prepare rejection gate.
  if (args.command === "prepare-transition-trace") {
    if (args.daPayloadEnvelopePath === undefined) {
      throw new Error(
        `Missing required --da-payload-envelope <path>.\n${usage}`,
      );
    }
    if (args.headerHash === undefined) {
      throw new Error(`Missing required --header-hash <hex>.\n${usage}`);
    }
    if (
      args.midgardNodeUrl !== undefined ||
      args.transactionsPath !== undefined ||
      args.sampleDoubleSpend
    ) {
      throw new Error(
        "prepare-transition-trace accepts only authenticated retained-DA evidence; legacy --midgard-node-url, --transactions-file, and --sample-double-spend inputs are forbidden",
      );
    }
    writeJson(
      await prepareTransitionTraceFromDaEnvelope({
        daPayloadEnvelopePath: args.daPayloadEnvelopePath,
        headerHash: args.headerHash,
        ...(args.outputDir === undefined ? {} : { outputDir: args.outputDir }),
      }),
    );
    return;
  }

  if (
    args.command !== "prepare-double-spend" &&
    args.command !== "prepare-invalid-range" &&
    args.command !== "prepare-non-existent-input" &&
    args.command !== "prepare-zero-input" &&
    args.command !== "prepare-input-no-idx" &&
    args.command !== "inspect-contracts" &&
    args.command !== "submit-init" &&
    args.command !== "submit-step-01" &&
    args.command !== "submit-step-02" &&
    args.command !== "submit-step-03" &&
    args.command !== "submit-step-04" &&
    args.command !== "submit-invalid-range-step-01" &&
    args.command !== "submit-invalid-range-step-02" &&
    args.command !== "submit-fabricated-deposit-step-01" &&
    args.command !== "submit-fabricated-deposit-step-02" &&
    args.command !== "submit-fabricated-deposit-step-03" &&
    args.command !== "submit-fabricated-deposit-step-04" &&
    args.command !== "submit-fabricated-withdrawal-step-01" &&
    args.command !== "submit-fabricated-withdrawal-step-02" &&
    args.command !== "submit-fabricated-withdrawal-step-03" &&
    args.command !== "submit-fabricated-withdrawal-step-04" &&
    args.command !== "submit-non-existent-input-step-01" &&
    args.command !== "submit-non-existent-input-step-02" &&
    args.command !== "submit-non-existent-input-step-03" &&
    args.command !== "submit-non-existent-input-step-04" &&
    args.command !== "submit-zero-input-step-01" &&
    args.command !== "submit-zero-input-step-02" &&
    args.command !== "submit-da-hash-preimage-step-01" &&
    args.command !== "submit-da-hash-preimage-step-02" &&
    args.command !== "submit-input-no-idx-step-01" &&
    args.command !== "submit-input-no-idx-step-02" &&
    args.command !== "submit-input-no-idx-fold" &&
    args.command !== "submit-input-no-idx-step-03" &&
    args.command !== "submit-input-no-idx-step-04" &&
    args.command !== "submit-no-reference-input-step-01" &&
    args.command !== "submit-no-reference-input-step-02" &&
    args.command !== "submit-no-reference-input-step-03" &&
    args.command !== "submit-no-reference-input-step-04" &&
    args.command !== "submit-reference-input-no-idx-step-01" &&
    args.command !== "submit-reference-input-no-idx-step-02" &&
    args.command !== "submit-reference-input-no-idx-step-03" &&
    args.command !== "submit-reference-input-no-idx-step-04" &&
    args.command !== "submit-invalid-signature-step-01" &&
    args.command !== "submit-invalid-signature-step-02" &&
    args.command !== "submit-validation-dispute-open" &&
    args.command !== "submit-validation-dispute-verify-source" &&
    args.command !== "submit-validation-dispute-reveal" &&
    args.command !== "submit-validation-dispute-enter-resolution" &&
    args.command !== "submit-validation-dispute-prepare-resolution" &&
    args.command !== "submit-validation-dispute-prepare-selected" &&
    args.command !== "submit-validation-dispute-semantic-resolution" &&
    args.command !== "submit-validation-dispute-award" &&
    args.command !== "submit-validation-dispute-enter-timeout" &&
    args.command !== "submit-validation-dispute-timeout" &&
    args.command !== "submit-transition-trace-proof" &&
    args.command !== "remove-fraudulent-block" &&
    args.command !== "remove-unattested-block"
  ) {
    throw new Error(
      `Expected a supported prepare, inspect, submit, validation-dispute, or removal command.\n${usage}`,
    );
  }

  // RF-043: reject every legacy diagnostic submission route before any
  // blueprint/deployment read or provider/wallet construction.  Only
  // canonical evidence submitters may cross this boundary in the future.
  rejectRetiredUnauthenticatedSubmissionRoute({
    command: args.command,
    fraudCategory: args.fraudCategory,
  });

  // Q03: the executable CLI has no operator-private compatibility route. The
  // current REST/file/sample flags are retained solely as clearly labelled
  // diagnostic imports and are rejected by the same security-grade gate every
  // canonical builder uses. Security-grade callers route all four verbs through
  // `executeCanonicalPrepareCommandV1`, supplying verified DA/L1 evidence from
  // the watcher/public transport rather than claiming a local file is trusted.
  if (args.command.startsWith("prepare-")) {
    if (
      args.command === "prepare-zero-input" &&
      args.expectedTransactionsRoot === undefined
    ) {
      throw new Error(
        `Missing required --expected-transactions-root <hex>.\n${usage}`,
      );
    }
    const provenance =
      args.midgardNodeUrl !== undefined
        ? MIDGARD_NODE_URL_DIAGNOSTIC_PROVENANCE
        : args.transactionsPath !== undefined
          ? LOCAL_FILE_DIAGNOSTIC_PROVENANCE
          : SAMPLE_EVIDENCE_DIAGNOSTIC_PROVENANCE;
    process.stderr.write(`${diagnosticEvidenceBanner(provenance)}\n`);
    assertSecurityGradeEvidence(provenance);
    // `assertSecurityGradeEvidence` always throws for these prohibited
    // diagnostic trust classes. This return documents that no prepare command
    // can fall through to blueprint/wallet/submission paths.
    return;
  }

  if (args.command === "remove-unattested-block") {
    const output = await submitUnattestedTimeoutCorrectionFromFiles(
      buildRemoveUnattestedBlockCliConfig(args),
    );
    writeJson(output);
    return;
  }

  if (args.blueprintPath === undefined) {
    throw new Error(`Missing required --blueprint <path>.\n${usage}`);
  }
  if (args.deploymentInfoPath === undefined) {
    throw new Error(`Missing required --deployment-info <path>.\n${usage}`);
  }

  if (args.command === "submit-transition-trace-proof") {
    if (args.threadOutRef === undefined) {
      throw new Error(
        `Missing required --thread-out-ref <txHash#outputIndex>.\n${usage}`,
      );
    }
    if (args.transitionFaultProofPath === undefined) {
      throw new Error(
        `Missing required --transition-fault-proof <path>.\n${usage}`,
      );
    }
    writeJson(
      await submitTransitionTraceProofFromCborFile({
        blueprintPath: args.blueprintPath,
        deploymentInfoPath: args.deploymentInfoPath,
        network: parseNetwork(args.network),
        provider: args.provider,
        blockfrostApiUrl: args.blockfrostApiUrl,
        blockfrostKey: args.blockfrostKey,
        kupoUrl: args.kupoUrl,
        ogmiosUrl: args.ogmiosUrl,
        walletSeedPhrase: args.walletSeedPhrase,
        walletSeedPhraseEnv: args.walletSeedPhraseEnv,
        walletPrivateKey: args.walletPrivateKey,
        walletPrivateKeyEnv: args.walletPrivateKeyEnv,
        threadOutRef: args.threadOutRef,
        transitionFaultProofPath: args.transitionFaultProofPath,
        referenceInputOutRefs: args.referenceInputOutRefs,
        awaitConfirmation: args.awaitConfirmation,
      }),
    );
    return;
  }

  if (args.command === "submit-init") {
    if (args.fraudulentBlockOutRef === undefined) {
      throw new Error(
        `Missing required --fraudulent-block-out-ref <txHash#outputIndex>.\n${usage}`,
      );
    }
    const output = await submitInitFromFiles({
      blueprintPath: args.blueprintPath,
      deploymentInfoPath: args.deploymentInfoPath,
      network: parseNetwork(args.network),
      provider: args.provider,
      blockfrostApiUrl: args.blockfrostApiUrl,
      blockfrostKey: args.blockfrostKey,
      kupoUrl: args.kupoUrl,
      ogmiosUrl: args.ogmiosUrl,
      walletSeedPhrase: args.walletSeedPhrase,
      walletSeedPhraseEnv: args.walletSeedPhraseEnv,
      walletPrivateKey: args.walletPrivateKey,
      walletPrivateKeyEnv: args.walletPrivateKeyEnv,
      fraudCategory: args.fraudCategory,
      fraudulentBlockOutRef: args.fraudulentBlockOutRef,
      fraudulentHeaderHash: args.fraudulentHeaderHash,
      awaitConfirmation: args.awaitConfirmation,
    });

    writeJson(output);
    return;
  }

  if (args.command === "submit-validation-dispute-open") {
    if (args.threadOutRef === undefined) {
      throw new Error(
        `Missing required --thread-out-ref <txHash#outputIndex>.\n${usage}`,
      );
    }
    if (args.stateQueueBlockOutRef === undefined) {
      throw new Error(
        `Missing required --state-queue-block-out-ref <txHash#outputIndex>.\n${usage}`,
      );
    }
    if (args.validationClaimCborPath === undefined) {
      throw new Error(
        `Missing required --validation-claim-cbor <path>.\n${usage}`,
      );
    }
    if (args.challengerDescriptorCborPath === undefined) {
      throw new Error(
        `Missing required --challenger-descriptor-cbor <path>.\n${usage}`,
      );
    }
    const output = await submitValidationDisputeOpenFromFiles({
      blueprintPath: args.blueprintPath,
      deploymentInfoPath: args.deploymentInfoPath,
      network: parseNetwork(args.network),
      provider: args.provider,
      blockfrostApiUrl: args.blockfrostApiUrl,
      blockfrostKey: args.blockfrostKey,
      kupoUrl: args.kupoUrl,
      ogmiosUrl: args.ogmiosUrl,
      walletSeedPhrase: args.walletSeedPhrase,
      walletSeedPhraseEnv: args.walletSeedPhraseEnv,
      walletPrivateKey: args.walletPrivateKey,
      walletPrivateKeyEnv: args.walletPrivateKeyEnv,
      threadOutRef: args.threadOutRef,
      stateQueueBlockOutRef: args.stateQueueBlockOutRef,
      claimCborPath: args.validationClaimCborPath,
      challengerDescriptorCborPath: args.challengerDescriptorCborPath,
      awaitConfirmation: args.awaitConfirmation,
    });
    writeJson(output);
    return;
  }

  if (args.command === "submit-validation-dispute-verify-source") {
    if (args.threadOutRef === undefined) {
      throw new Error(
        `Missing required --thread-out-ref <txHash#outputIndex>.\n${usage}`,
      );
    }
    const output = await submitValidationDisputeVerifySourceFromFiles({
      blueprintPath: args.blueprintPath,
      deploymentInfoPath: args.deploymentInfoPath,
      network: parseNetwork(args.network),
      provider: args.provider,
      blockfrostApiUrl: args.blockfrostApiUrl,
      blockfrostKey: args.blockfrostKey,
      kupoUrl: args.kupoUrl,
      ogmiosUrl: args.ogmiosUrl,
      walletSeedPhrase: args.walletSeedPhrase,
      walletSeedPhraseEnv: args.walletSeedPhraseEnv,
      walletPrivateKey: args.walletPrivateKey,
      walletPrivateKeyEnv: args.walletPrivateKeyEnv,
      threadOutRef: args.threadOutRef,
      awaitConfirmation: args.awaitConfirmation,
    });
    writeJson(output);
    return;
  }

  if (args.command === "submit-validation-dispute-reveal") {
    if (args.threadOutRef === undefined) {
      throw new Error(
        `Missing required --thread-out-ref <txHash#outputIndex>.\n${usage}`,
      );
    }
    if (args.validationDisputeRole === undefined) {
      throw new Error(
        `Missing required --validation-dispute-role <operator|challenger>.\n${usage}`,
      );
    }
    if (args.validationTraceProofCborPath === undefined) {
      throw new Error(
        `Missing required --validation-trace-proof-cbor <path>.\n${usage}`,
      );
    }
    const output = await submitValidationDisputeRevealFromFiles({
      blueprintPath: args.blueprintPath,
      deploymentInfoPath: args.deploymentInfoPath,
      network: parseNetwork(args.network),
      provider: args.provider,
      blockfrostApiUrl: args.blockfrostApiUrl,
      blockfrostKey: args.blockfrostKey,
      kupoUrl: args.kupoUrl,
      ogmiosUrl: args.ogmiosUrl,
      walletSeedPhrase: args.walletSeedPhrase,
      walletSeedPhraseEnv: args.walletSeedPhraseEnv,
      walletPrivateKey: args.walletPrivateKey,
      walletPrivateKeyEnv: args.walletPrivateKeyEnv,
      threadOutRef: args.threadOutRef,
      role: args.validationDisputeRole,
      proofCborPath: args.validationTraceProofCborPath,
      awaitConfirmation: args.awaitConfirmation,
    });
    writeJson(output);
    return;
  }

  if (args.command === "submit-validation-dispute-enter-resolution") {
    if (args.threadOutRef === undefined) {
      throw new Error(
        `Missing required --thread-out-ref <txHash#outputIndex>.\n${usage}`,
      );
    }
    const output = await submitValidationDisputeEnterResolutionFromFiles({
      blueprintPath: args.blueprintPath,
      deploymentInfoPath: args.deploymentInfoPath,
      network: parseNetwork(args.network),
      provider: args.provider,
      blockfrostApiUrl: args.blockfrostApiUrl,
      blockfrostKey: args.blockfrostKey,
      kupoUrl: args.kupoUrl,
      ogmiosUrl: args.ogmiosUrl,
      walletSeedPhrase: args.walletSeedPhrase,
      walletSeedPhraseEnv: args.walletSeedPhraseEnv,
      walletPrivateKey: args.walletPrivateKey,
      walletPrivateKeyEnv: args.walletPrivateKeyEnv,
      threadOutRef: args.threadOutRef,
      awaitConfirmation: args.awaitConfirmation,
    });
    writeJson(output);
    return;
  }

  if (args.command === "submit-validation-dispute-prepare-resolution") {
    if (args.threadOutRef === undefined) {
      throw new Error(
        `Missing required --thread-out-ref <txHash#outputIndex>.\n${usage}`,
      );
    }
    if (args.validationBoundaryEvidenceCborPath === undefined) {
      throw new Error(
        `Missing required --validation-boundary-evidence-cbor <path>.\n${usage}`,
      );
    }
    const output = await submitValidationDisputePrepareResolutionFromFiles({
      blueprintPath: args.blueprintPath,
      deploymentInfoPath: args.deploymentInfoPath,
      network: parseNetwork(args.network),
      provider: args.provider,
      blockfrostApiUrl: args.blockfrostApiUrl,
      blockfrostKey: args.blockfrostKey,
      kupoUrl: args.kupoUrl,
      ogmiosUrl: args.ogmiosUrl,
      walletSeedPhrase: args.walletSeedPhrase,
      walletSeedPhraseEnv: args.walletSeedPhraseEnv,
      walletPrivateKey: args.walletPrivateKey,
      walletPrivateKeyEnv: args.walletPrivateKeyEnv,
      threadOutRef: args.threadOutRef,
      boundaryEvidenceCborPath: args.validationBoundaryEvidenceCborPath,
      awaitConfirmation: args.awaitConfirmation,
    });
    writeJson(output);
    return;
  }

  if (
    args.command === "submit-validation-dispute-prepare-selected" ||
    args.command === "submit-validation-dispute-semantic-resolution"
  ) {
    if (args.threadOutRef === undefined) {
      throw new Error(
        `Missing required --thread-out-ref <txHash#outputIndex>.\n${usage}`,
      );
    }
    const oneStep = requireValidationOneStepCliArguments(
      args,
      args.command === "submit-validation-dispute-semantic-resolution",
    );
    const config = {
      blueprintPath: args.blueprintPath,
      deploymentInfoPath: args.deploymentInfoPath,
      network: parseNetwork(args.network),
      provider: args.provider,
      blockfrostApiUrl: args.blockfrostApiUrl,
      blockfrostKey: args.blockfrostKey,
      kupoUrl: args.kupoUrl,
      ogmiosUrl: args.ogmiosUrl,
      walletSeedPhrase: args.walletSeedPhrase,
      walletSeedPhraseEnv: args.walletSeedPhraseEnv,
      walletPrivateKey: args.walletPrivateKey,
      walletPrivateKeyEnv: args.walletPrivateKeyEnv,
      threadOutRef: args.threadOutRef,
      awaitConfirmation: args.awaitConfirmation,
      ...oneStep,
    };
    const output =
      args.command === "submit-validation-dispute-prepare-selected"
        ? await submitValidationDisputePrepareSelectedFromFiles(config)
        : await submitValidationDisputeSemanticResolutionFromFiles(config);
    writeJson(output);
    return;
  }

  if (args.command === "submit-validation-dispute-award") {
    if (args.threadOutRef === undefined) {
      throw new Error(
        `Missing required --thread-out-ref <txHash#outputIndex>.\n${usage}`,
      );
    }
    const output = await submitValidationDisputeAwardFromFiles({
      blueprintPath: args.blueprintPath,
      deploymentInfoPath: args.deploymentInfoPath,
      network: parseNetwork(args.network),
      provider: args.provider,
      blockfrostApiUrl: args.blockfrostApiUrl,
      blockfrostKey: args.blockfrostKey,
      kupoUrl: args.kupoUrl,
      ogmiosUrl: args.ogmiosUrl,
      walletSeedPhrase: args.walletSeedPhrase,
      walletSeedPhraseEnv: args.walletSeedPhraseEnv,
      walletPrivateKey: args.walletPrivateKey,
      walletPrivateKeyEnv: args.walletPrivateKeyEnv,
      threadOutRef: args.threadOutRef,
      awaitConfirmation: args.awaitConfirmation,
    });
    writeJson(output);
    return;
  }

  if (args.command === "submit-validation-dispute-enter-timeout") {
    if (args.threadOutRef === undefined) {
      throw new Error(
        `Missing required --thread-out-ref <txHash#outputIndex>.\n${usage}`,
      );
    }
    const output = await submitValidationDisputeEnterTimeoutFromFiles({
      blueprintPath: args.blueprintPath,
      deploymentInfoPath: args.deploymentInfoPath,
      network: parseNetwork(args.network),
      provider: args.provider,
      blockfrostApiUrl: args.blockfrostApiUrl,
      blockfrostKey: args.blockfrostKey,
      kupoUrl: args.kupoUrl,
      ogmiosUrl: args.ogmiosUrl,
      walletSeedPhrase: args.walletSeedPhrase,
      walletSeedPhraseEnv: args.walletSeedPhraseEnv,
      walletPrivateKey: args.walletPrivateKey,
      walletPrivateKeyEnv: args.walletPrivateKeyEnv,
      threadOutRef: args.threadOutRef,
      awaitConfirmation: args.awaitConfirmation,
    });
    writeJson(output);
    return;
  }

  if (args.command === "submit-validation-dispute-timeout") {
    if (args.threadOutRef === undefined) {
      throw new Error(
        `Missing required --thread-out-ref <txHash#outputIndex>.\n${usage}`,
      );
    }
    const output = await submitValidationDisputeTimeoutFromFiles({
      blueprintPath: args.blueprintPath,
      deploymentInfoPath: args.deploymentInfoPath,
      network: parseNetwork(args.network),
      provider: args.provider,
      blockfrostApiUrl: args.blockfrostApiUrl,
      blockfrostKey: args.blockfrostKey,
      kupoUrl: args.kupoUrl,
      ogmiosUrl: args.ogmiosUrl,
      walletSeedPhrase: args.walletSeedPhrase,
      walletSeedPhraseEnv: args.walletSeedPhraseEnv,
      walletPrivateKey: args.walletPrivateKey,
      walletPrivateKeyEnv: args.walletPrivateKeyEnv,
      threadOutRef: args.threadOutRef,
      awaitConfirmation: args.awaitConfirmation,
    });
    writeJson(output);
    return;
  }

  if (args.command === "submit-invalid-range-step-01") {
    if (args.threadOutRef === undefined) {
      throw new Error(
        `Missing required --thread-out-ref <txHash#outputIndex>.\n${usage}`,
      );
    }
    if (args.stateQueueBlockOutRef === undefined) {
      throw new Error(
        `Missing required --state-queue-block-out-ref <txHash#outputIndex>.\n${usage}`,
      );
    }
    if (args.txInclusionPath === undefined) {
      throw new Error(`Missing required --tx-inclusion <path>.\n${usage}`);
    }
    const output = await submitInvalidRangeStep01FromFiles({
      blueprintPath: args.blueprintPath,
      deploymentInfoPath: args.deploymentInfoPath,
      network: parseNetwork(args.network),
      provider: args.provider,
      blockfrostApiUrl: args.blockfrostApiUrl,
      blockfrostKey: args.blockfrostKey,
      kupoUrl: args.kupoUrl,
      ogmiosUrl: args.ogmiosUrl,
      walletSeedPhrase: args.walletSeedPhrase,
      walletSeedPhraseEnv: args.walletSeedPhraseEnv,
      walletPrivateKey: args.walletPrivateKey,
      walletPrivateKeyEnv: args.walletPrivateKeyEnv,
      threadOutRef: args.threadOutRef,
      stateQueueBlockOutRef: args.stateQueueBlockOutRef,
      txInclusionPath: args.txInclusionPath,
      awaitConfirmation: args.awaitConfirmation,
    });

    writeJson(output);
    return;
  }

  if (args.command === "submit-invalid-range-step-02") {
    if (args.threadOutRef === undefined) {
      throw new Error(
        `Missing required --thread-out-ref <txHash#outputIndex>.\n${usage}`,
      );
    }
    const output = await submitInvalidRangeStep02FromFiles({
      blueprintPath: args.blueprintPath,
      deploymentInfoPath: args.deploymentInfoPath,
      network: parseNetwork(args.network),
      provider: args.provider,
      blockfrostApiUrl: args.blockfrostApiUrl,
      blockfrostKey: args.blockfrostKey,
      kupoUrl: args.kupoUrl,
      ogmiosUrl: args.ogmiosUrl,
      walletSeedPhrase: args.walletSeedPhrase,
      walletSeedPhraseEnv: args.walletSeedPhraseEnv,
      walletPrivateKey: args.walletPrivateKey,
      walletPrivateKeyEnv: args.walletPrivateKeyEnv,
      threadOutRef: args.threadOutRef,
      awaitConfirmation: args.awaitConfirmation,
    });

    writeJson(output);
    return;
  }

  if (args.command === "submit-zero-input-step-01") {
    if (args.threadOutRef === undefined) {
      throw new Error(
        `Missing required --thread-out-ref <txHash#outputIndex>.\n${usage}`,
      );
    }
    if (args.stateQueueBlockOutRef === undefined) {
      throw new Error(
        `Missing required --state-queue-block-out-ref <txHash#outputIndex>.\n${usage}`,
      );
    }
    if (args.txInclusionPath === undefined) {
      throw new Error(`Missing required --tx-inclusion <path>.\n${usage}`);
    }
    const output = await submitZeroInputStep01FromFiles({
      blueprintPath: args.blueprintPath,
      deploymentInfoPath: args.deploymentInfoPath,
      network: parseNetwork(args.network),
      provider: args.provider,
      blockfrostApiUrl: args.blockfrostApiUrl,
      blockfrostKey: args.blockfrostKey,
      kupoUrl: args.kupoUrl,
      ogmiosUrl: args.ogmiosUrl,
      walletSeedPhrase: args.walletSeedPhrase,
      walletSeedPhraseEnv: args.walletSeedPhraseEnv,
      walletPrivateKey: args.walletPrivateKey,
      walletPrivateKeyEnv: args.walletPrivateKeyEnv,
      threadOutRef: args.threadOutRef,
      stateQueueBlockOutRef: args.stateQueueBlockOutRef,
      txInclusionPath: args.txInclusionPath,
      awaitConfirmation: args.awaitConfirmation,
    });

    writeJson(output);
    return;
  }

  if (args.command === "submit-zero-input-step-02") {
    if (args.threadOutRef === undefined) {
      throw new Error(
        `Missing required --thread-out-ref <txHash#outputIndex>.\n${usage}`,
      );
    }
    if (args.nativeTxCompactPath === undefined) {
      throw new Error(
        `Missing required --native-tx-compact <native-tx-compact.json>.\n${usage}`,
      );
    }
    const output = await submitZeroInputStep02FromFiles({
      blueprintPath: args.blueprintPath,
      deploymentInfoPath: args.deploymentInfoPath,
      network: parseNetwork(args.network),
      provider: args.provider,
      blockfrostApiUrl: args.blockfrostApiUrl,
      blockfrostKey: args.blockfrostKey,
      kupoUrl: args.kupoUrl,
      ogmiosUrl: args.ogmiosUrl,
      walletSeedPhrase: args.walletSeedPhrase,
      walletSeedPhraseEnv: args.walletSeedPhraseEnv,
      walletPrivateKey: args.walletPrivateKey,
      walletPrivateKeyEnv: args.walletPrivateKeyEnv,
      threadOutRef: args.threadOutRef,
      nativeTxCompactPath: args.nativeTxCompactPath,
      awaitConfirmation: args.awaitConfirmation,
    });

    writeJson(output);
    return;
  }

  if (args.command === "submit-input-no-idx-step-01") {
    if (args.threadOutRef === undefined) {
      throw new Error(
        `Missing required --thread-out-ref <txHash#outputIndex>.\n${usage}`,
      );
    }
    if (args.stateQueueBlockOutRef === undefined) {
      throw new Error(
        `Missing required --state-queue-block-out-ref <txHash#outputIndex>.\n${usage}`,
      );
    }
    if (args.txInclusionPath === undefined) {
      throw new Error(`Missing required --tx-inclusion <path>.\n${usage}`);
    }
    const output = await submitInputNoIdxStep01FromFiles({
      blueprintPath: args.blueprintPath,
      deploymentInfoPath: args.deploymentInfoPath,
      network: parseNetwork(args.network),
      provider: args.provider,
      blockfrostApiUrl: args.blockfrostApiUrl,
      blockfrostKey: args.blockfrostKey,
      kupoUrl: args.kupoUrl,
      ogmiosUrl: args.ogmiosUrl,
      walletSeedPhrase: args.walletSeedPhrase,
      walletSeedPhraseEnv: args.walletSeedPhraseEnv,
      walletPrivateKey: args.walletPrivateKey,
      walletPrivateKeyEnv: args.walletPrivateKeyEnv,
      threadOutRef: args.threadOutRef,
      stateQueueBlockOutRef: args.stateQueueBlockOutRef,
      txInclusionPath: args.txInclusionPath,
      awaitConfirmation: args.awaitConfirmation,
    });

    writeJson(output);
    return;
  }

  if (args.command === "submit-input-no-idx-step-02") {
    if (args.threadOutRef === undefined) {
      throw new Error(
        `Missing required --thread-out-ref <txHash#outputIndex>.\n${usage}`,
      );
    }
    if (args.inputsPreimagePath === undefined) {
      throw new Error(`Missing required --inputs-preimage <path>.\n${usage}`);
    }
    if (args.nativeTxCompactPath === undefined) {
      throw new Error(
        `Missing required --native-tx-compact <native-tx-compact.json>.\n${usage}`,
      );
    }
    const output = await submitInputNoIdxStep02FromFiles({
      blueprintPath: args.blueprintPath,
      deploymentInfoPath: args.deploymentInfoPath,
      network: parseNetwork(args.network),
      provider: args.provider,
      blockfrostApiUrl: args.blockfrostApiUrl,
      blockfrostKey: args.blockfrostKey,
      kupoUrl: args.kupoUrl,
      ogmiosUrl: args.ogmiosUrl,
      walletSeedPhrase: args.walletSeedPhrase,
      walletSeedPhraseEnv: args.walletSeedPhraseEnv,
      walletPrivateKey: args.walletPrivateKey,
      walletPrivateKeyEnv: args.walletPrivateKeyEnv,
      threadOutRef: args.threadOutRef,
      inputsPreimagePath: args.inputsPreimagePath,
      nativeTxCompactPath: args.nativeTxCompactPath,
      awaitConfirmation: args.awaitConfirmation,
    });

    writeJson(output);
    return;
  }

  if (args.command === "submit-input-no-idx-fold") {
    // Retired by #604, not renamed. `fraud_proofs/input_no_idx/step_02.Args` is
    // a flat record on-chain: the `FoldStart`/`FoldNext` arms this command drove
    // no longer exist, so there is no redeemer it could emit. The ordered fold
    // was a way to reproduce a collection inside the step in order to re-hash it,
    // and §4's flat commitment plus the §8.8 door removed the need — the whole
    // preimage now travels under one §8 carriage tier and the step reads item
    // `n` by arithmetic. Run `submit-input-no-idx-step-02`; if the preimage does
    // not fit the step's own redeemer, §8's ladder publishes it.
    throw new Error(
      "submit-input-no-idx-fold is retired: input-no-idx step 02 has a single " +
        "route since #575 moved it onto the \u00a78.8 field-opening door, and the " +
        "FoldStart/FoldNext redeemer arms it drove no longer exist on-chain. " +
        "Use submit-input-no-idx-step-02 --native-tx-compact <native-tx-compact.json>; " +
        "\u00a78's carriage ladder publishes the preimage when it does not fit the " +
        `redeemer.\n${usage}`,
    );
  }

  if (args.command === "submit-input-no-idx-step-03") {
    if (args.threadOutRef === undefined) {
      throw new Error(
        `Missing required --thread-out-ref <txHash#outputIndex>.\n${usage}`,
      );
    }
    if (args.stateQueueBlockOutRef === undefined) {
      throw new Error(
        `Missing required --state-queue-block-out-ref <txHash#outputIndex>.\n${usage}`,
      );
    }
    if (args.txInclusionPath === undefined) {
      throw new Error(`Missing required --tx-inclusion <path>.\n${usage}`);
    }
    const output = await submitInputNoIdxStep03FromFiles({
      blueprintPath: args.blueprintPath,
      deploymentInfoPath: args.deploymentInfoPath,
      network: parseNetwork(args.network),
      provider: args.provider,
      blockfrostApiUrl: args.blockfrostApiUrl,
      blockfrostKey: args.blockfrostKey,
      kupoUrl: args.kupoUrl,
      ogmiosUrl: args.ogmiosUrl,
      walletSeedPhrase: args.walletSeedPhrase,
      walletSeedPhraseEnv: args.walletSeedPhraseEnv,
      walletPrivateKey: args.walletPrivateKey,
      walletPrivateKeyEnv: args.walletPrivateKeyEnv,
      threadOutRef: args.threadOutRef,
      stateQueueBlockOutRef: args.stateQueueBlockOutRef,
      txInclusionPath: args.txInclusionPath,
      awaitConfirmation: args.awaitConfirmation,
    });

    writeJson(output);
    return;
  }

  if (args.command === "submit-input-no-idx-step-04") {
    if (args.threadOutRef === undefined) {
      throw new Error(
        `Missing required --thread-out-ref <txHash#outputIndex>.\n${usage}`,
      );
    }
    if (args.outputsPreimagePath === undefined) {
      throw new Error(`Missing required --outputs-preimage <path>.\n${usage}`);
    }
    if (args.nativeTxCompactPath === undefined) {
      throw new Error(
        `Missing required --native-tx-compact <native-tx-compact.json>.\n${usage}`,
      );
    }
    const output = await submitInputNoIdxStep04FromFiles({
      blueprintPath: args.blueprintPath,
      deploymentInfoPath: args.deploymentInfoPath,
      network: parseNetwork(args.network),
      provider: args.provider,
      blockfrostApiUrl: args.blockfrostApiUrl,
      blockfrostKey: args.blockfrostKey,
      kupoUrl: args.kupoUrl,
      ogmiosUrl: args.ogmiosUrl,
      walletSeedPhrase: args.walletSeedPhrase,
      walletSeedPhraseEnv: args.walletSeedPhraseEnv,
      walletPrivateKey: args.walletPrivateKey,
      walletPrivateKeyEnv: args.walletPrivateKeyEnv,
      threadOutRef: args.threadOutRef,
      outputsPreimagePath: args.outputsPreimagePath,
      nativeTxCompactPath: args.nativeTxCompactPath,
      awaitConfirmation: args.awaitConfirmation,
    });

    writeJson(output);
    return;
  }

  if (args.command === "submit-no-reference-input-step-01") {
    if (args.threadOutRef === undefined) {
      throw new Error(
        `Missing required --thread-out-ref <txHash#outputIndex>.\n${usage}`,
      );
    }
    if (args.stateQueueBlockOutRef === undefined) {
      throw new Error(
        `Missing required --state-queue-block-out-ref <txHash#outputIndex>.\n${usage}`,
      );
    }
    if (args.txInclusionPath === undefined) {
      throw new Error(`Missing required --tx-inclusion <path>.\n${usage}`);
    }
    const output = await submitNoReferenceInputStep01FromFiles({
      blueprintPath: args.blueprintPath,
      deploymentInfoPath: args.deploymentInfoPath,
      network: parseNetwork(args.network),
      provider: args.provider,
      blockfrostApiUrl: args.blockfrostApiUrl,
      blockfrostKey: args.blockfrostKey,
      kupoUrl: args.kupoUrl,
      ogmiosUrl: args.ogmiosUrl,
      walletSeedPhrase: args.walletSeedPhrase,
      walletSeedPhraseEnv: args.walletSeedPhraseEnv,
      walletPrivateKey: args.walletPrivateKey,
      walletPrivateKeyEnv: args.walletPrivateKeyEnv,
      threadOutRef: args.threadOutRef,
      stateQueueBlockOutRef: args.stateQueueBlockOutRef,
      txInclusionPath: args.txInclusionPath,
      awaitConfirmation: args.awaitConfirmation,
    });

    writeJson(output);
    return;
  }

  if (args.command === "submit-no-reference-input-step-02") {
    if (args.threadOutRef === undefined) {
      throw new Error(
        `Missing required --thread-out-ref <txHash#outputIndex>.\n${usage}`,
      );
    }
    if (args.referenceInputsPreimagePath === undefined) {
      throw new Error(
        `Missing required --reference-inputs-preimage <path>.\n${usage}`,
      );
    }
    if (args.badReferenceInputIndex === undefined) {
      throw new Error(
        `Missing required --bad-reference-input-index <n>.\n${usage}`,
      );
    }
    if (args.nativeTxCompactPath === undefined) {
      throw new Error(
        `Missing required --native-tx-compact <native-tx-compact.json>.\n${usage}`,
      );
    }
    const output = await submitNoReferenceInputStep02FromFiles({
      blueprintPath: args.blueprintPath,
      deploymentInfoPath: args.deploymentInfoPath,
      network: parseNetwork(args.network),
      provider: args.provider,
      blockfrostApiUrl: args.blockfrostApiUrl,
      blockfrostKey: args.blockfrostKey,
      kupoUrl: args.kupoUrl,
      ogmiosUrl: args.ogmiosUrl,
      walletSeedPhrase: args.walletSeedPhrase,
      walletSeedPhraseEnv: args.walletSeedPhraseEnv,
      walletPrivateKey: args.walletPrivateKey,
      walletPrivateKeyEnv: args.walletPrivateKeyEnv,
      threadOutRef: args.threadOutRef,
      referenceInputsPreimagePath: args.referenceInputsPreimagePath,
      nativeTxCompactPath: args.nativeTxCompactPath,
      badReferenceInputIndex: args.badReferenceInputIndex,
      awaitConfirmation: args.awaitConfirmation,
    });

    writeJson(output);
    return;
  }

  if (args.command === "submit-no-reference-input-step-03") {
    if (args.threadOutRef === undefined) {
      throw new Error(
        `Missing required --thread-out-ref <txHash#outputIndex>.\n${usage}`,
      );
    }
    if (args.ledgerNonMembershipProofPath === undefined) {
      throw new Error(
        `Missing required --ledger-non-membership-proof <path>.\n${usage}`,
      );
    }
    const output = await submitNoReferenceInputStep03FromFiles({
      blueprintPath: args.blueprintPath,
      deploymentInfoPath: args.deploymentInfoPath,
      network: parseNetwork(args.network),
      provider: args.provider,
      blockfrostApiUrl: args.blockfrostApiUrl,
      blockfrostKey: args.blockfrostKey,
      kupoUrl: args.kupoUrl,
      ogmiosUrl: args.ogmiosUrl,
      walletSeedPhrase: args.walletSeedPhrase,
      walletSeedPhraseEnv: args.walletSeedPhraseEnv,
      walletPrivateKey: args.walletPrivateKey,
      walletPrivateKeyEnv: args.walletPrivateKeyEnv,
      threadOutRef: args.threadOutRef,
      ledgerNonMembershipProofPath: args.ledgerNonMembershipProofPath,
      awaitConfirmation: args.awaitConfirmation,
    });

    writeJson(output);
    return;
  }

  if (args.command === "submit-no-reference-input-step-04") {
    if (args.threadOutRef === undefined) {
      throw new Error(
        `Missing required --thread-out-ref <txHash#outputIndex>.\n${usage}`,
      );
    }
    if (args.txsNonMembershipProofPath === undefined) {
      throw new Error(
        `Missing required --txs-non-membership-proof <path>.\n${usage}`,
      );
    }
    const output = await submitNoReferenceInputStep04FromFiles({
      blueprintPath: args.blueprintPath,
      deploymentInfoPath: args.deploymentInfoPath,
      network: parseNetwork(args.network),
      provider: args.provider,
      blockfrostApiUrl: args.blockfrostApiUrl,
      blockfrostKey: args.blockfrostKey,
      kupoUrl: args.kupoUrl,
      ogmiosUrl: args.ogmiosUrl,
      walletSeedPhrase: args.walletSeedPhrase,
      walletSeedPhraseEnv: args.walletSeedPhraseEnv,
      walletPrivateKey: args.walletPrivateKey,
      walletPrivateKeyEnv: args.walletPrivateKeyEnv,
      threadOutRef: args.threadOutRef,
      txsNonMembershipProofPath: args.txsNonMembershipProofPath,
      awaitConfirmation: args.awaitConfirmation,
    });

    writeJson(output);
    return;
  }

  if (args.command === "submit-reference-input-no-idx-step-01") {
    if (args.threadOutRef === undefined) {
      throw new Error(
        `Missing required --thread-out-ref <txHash#outputIndex>.\n${usage}`,
      );
    }
    if (args.stateQueueBlockOutRef === undefined) {
      throw new Error(
        `Missing required --state-queue-block-out-ref <txHash#outputIndex>.\n${usage}`,
      );
    }
    if (args.txInclusionPath === undefined) {
      throw new Error(`Missing required --tx-inclusion <path>.\n${usage}`);
    }
    const output = await submitReferenceInputNoIdxStep01FromFiles({
      blueprintPath: args.blueprintPath,
      deploymentInfoPath: args.deploymentInfoPath,
      network: parseNetwork(args.network),
      provider: args.provider,
      blockfrostApiUrl: args.blockfrostApiUrl,
      blockfrostKey: args.blockfrostKey,
      kupoUrl: args.kupoUrl,
      ogmiosUrl: args.ogmiosUrl,
      walletSeedPhrase: args.walletSeedPhrase,
      walletSeedPhraseEnv: args.walletSeedPhraseEnv,
      walletPrivateKey: args.walletPrivateKey,
      walletPrivateKeyEnv: args.walletPrivateKeyEnv,
      threadOutRef: args.threadOutRef,
      stateQueueBlockOutRef: args.stateQueueBlockOutRef,
      txInclusionPath: args.txInclusionPath,
      awaitConfirmation: args.awaitConfirmation,
    });

    writeJson(output);
    return;
  }

  if (args.command === "submit-reference-input-no-idx-step-02") {
    if (args.threadOutRef === undefined) {
      throw new Error(
        `Missing required --thread-out-ref <txHash#outputIndex>.\n${usage}`,
      );
    }
    if (args.referenceInputsPreimagePath === undefined) {
      throw new Error(
        `Missing required --reference-inputs-preimage <path>.\n${usage}`,
      );
    }
    if (args.nativeTxCompactPath === undefined) {
      throw new Error(
        `Missing required --native-tx-compact <native-tx-compact.json>.\n${usage}`,
      );
    }
    const output = await submitReferenceInputNoIdxStep02FromFiles({
      blueprintPath: args.blueprintPath,
      deploymentInfoPath: args.deploymentInfoPath,
      network: parseNetwork(args.network),
      provider: args.provider,
      blockfrostApiUrl: args.blockfrostApiUrl,
      blockfrostKey: args.blockfrostKey,
      kupoUrl: args.kupoUrl,
      ogmiosUrl: args.ogmiosUrl,
      walletSeedPhrase: args.walletSeedPhrase,
      walletSeedPhraseEnv: args.walletSeedPhraseEnv,
      walletPrivateKey: args.walletPrivateKey,
      walletPrivateKeyEnv: args.walletPrivateKeyEnv,
      threadOutRef: args.threadOutRef,
      referenceInputsPreimagePath: args.referenceInputsPreimagePath,
      nativeTxCompactPath: args.nativeTxCompactPath,
      badReferenceInputIndex: args.badReferenceInputIndex,
      awaitConfirmation: args.awaitConfirmation,
    });

    writeJson(output);
    return;
  }

  if (args.command === "submit-reference-input-no-idx-step-03") {
    if (args.threadOutRef === undefined) {
      throw new Error(
        `Missing required --thread-out-ref <txHash#outputIndex>.\n${usage}`,
      );
    }
    if (args.stateQueueBlockOutRef === undefined) {
      throw new Error(
        `Missing required --state-queue-block-out-ref <txHash#outputIndex>.\n${usage}`,
      );
    }
    if (args.txInclusionPath === undefined) {
      throw new Error(`Missing required --tx-inclusion <path>.\n${usage}`);
    }
    const output = await submitReferenceInputNoIdxStep03FromFiles({
      blueprintPath: args.blueprintPath,
      deploymentInfoPath: args.deploymentInfoPath,
      network: parseNetwork(args.network),
      provider: args.provider,
      blockfrostApiUrl: args.blockfrostApiUrl,
      blockfrostKey: args.blockfrostKey,
      kupoUrl: args.kupoUrl,
      ogmiosUrl: args.ogmiosUrl,
      walletSeedPhrase: args.walletSeedPhrase,
      walletSeedPhraseEnv: args.walletSeedPhraseEnv,
      walletPrivateKey: args.walletPrivateKey,
      walletPrivateKeyEnv: args.walletPrivateKeyEnv,
      threadOutRef: args.threadOutRef,
      stateQueueBlockOutRef: args.stateQueueBlockOutRef,
      txInclusionPath: args.txInclusionPath,
      awaitConfirmation: args.awaitConfirmation,
    });

    writeJson(output);
    return;
  }

  if (args.command === "submit-reference-input-no-idx-step-04") {
    if (args.threadOutRef === undefined) {
      throw new Error(
        `Missing required --thread-out-ref <txHash#outputIndex>.\n${usage}`,
      );
    }
    if (args.outputsPreimagePath === undefined) {
      throw new Error(`Missing required --outputs-preimage <path>.\n${usage}`);
    }
    if (args.nativeTxCompactPath === undefined) {
      throw new Error(
        `Missing required --native-tx-compact <native-tx-compact.json>.\n${usage}`,
      );
    }
    const output = await submitReferenceInputNoIdxStep04FromFiles({
      blueprintPath: args.blueprintPath,
      deploymentInfoPath: args.deploymentInfoPath,
      network: parseNetwork(args.network),
      provider: args.provider,
      blockfrostApiUrl: args.blockfrostApiUrl,
      blockfrostKey: args.blockfrostKey,
      kupoUrl: args.kupoUrl,
      ogmiosUrl: args.ogmiosUrl,
      walletSeedPhrase: args.walletSeedPhrase,
      walletSeedPhraseEnv: args.walletSeedPhraseEnv,
      walletPrivateKey: args.walletPrivateKey,
      walletPrivateKeyEnv: args.walletPrivateKeyEnv,
      threadOutRef: args.threadOutRef,
      outputsPreimagePath: args.outputsPreimagePath,
      nativeTxCompactPath: args.nativeTxCompactPath,
      awaitConfirmation: args.awaitConfirmation,
    });

    writeJson(output);
    return;
  }

  if (args.command === "submit-invalid-signature-step-01") {
    if (args.threadOutRef === undefined) {
      throw new Error(
        `Missing required --thread-out-ref <txHash#outputIndex>.\n${usage}`,
      );
    }
    if (args.stateQueueBlockOutRef === undefined) {
      throw new Error(
        `Missing required --state-queue-block-out-ref <txHash#outputIndex>.\n${usage}`,
      );
    }
    if (args.txInclusionPath === undefined) {
      throw new Error(`Missing required --tx-inclusion <path>.\n${usage}`);
    }
    if (args.witnessSetCompactPath === undefined) {
      throw new Error(
        `Missing required --witness-set-compact <path>.\n${usage}`,
      );
    }
    const output = await submitInvalidSignatureStep01FromFiles({
      blueprintPath: args.blueprintPath,
      deploymentInfoPath: args.deploymentInfoPath,
      network: parseNetwork(args.network),
      provider: args.provider,
      blockfrostApiUrl: args.blockfrostApiUrl,
      blockfrostKey: args.blockfrostKey,
      kupoUrl: args.kupoUrl,
      ogmiosUrl: args.ogmiosUrl,
      walletSeedPhrase: args.walletSeedPhrase,
      walletSeedPhraseEnv: args.walletSeedPhraseEnv,
      walletPrivateKey: args.walletPrivateKey,
      walletPrivateKeyEnv: args.walletPrivateKeyEnv,
      threadOutRef: args.threadOutRef,
      stateQueueBlockOutRef: args.stateQueueBlockOutRef,
      txInclusionPath: args.txInclusionPath,
      witnessSetCompactPath: args.witnessSetCompactPath,
      awaitConfirmation: args.awaitConfirmation,
    });

    writeJson(output);
    return;
  }

  if (args.command === "submit-invalid-signature-step-02") {
    if (args.threadOutRef === undefined) {
      throw new Error(
        `Missing required --thread-out-ref <txHash#outputIndex>.\n${usage}`,
      );
    }
    if (args.addrTxWitsPreimagePath === undefined) {
      throw new Error(
        `Missing required --addr-tx-wits-preimage <path>.\n${usage}`,
      );
    }
    if (args.badAddrTxWitIndex === undefined) {
      throw new Error(
        `Missing required --bad-addr-tx-wit-index <n>.\n${usage}`,
      );
    }
    if (args.nativeTxCompactPath === undefined) {
      throw new Error(
        `Missing required --native-tx-compact <native-tx-compact.json>.\n${usage}`,
      );
    }
    if (args.witnessSetCompactPath === undefined) {
      throw new Error(
        `Missing required --witness-set-compact <witness-set-compact.json>.\n${usage}`,
      );
    }
    const output = await submitInvalidSignatureStep02FromFiles({
      blueprintPath: args.blueprintPath,
      deploymentInfoPath: args.deploymentInfoPath,
      network: parseNetwork(args.network),
      provider: args.provider,
      blockfrostApiUrl: args.blockfrostApiUrl,
      blockfrostKey: args.blockfrostKey,
      kupoUrl: args.kupoUrl,
      ogmiosUrl: args.ogmiosUrl,
      walletSeedPhrase: args.walletSeedPhrase,
      walletSeedPhraseEnv: args.walletSeedPhraseEnv,
      walletPrivateKey: args.walletPrivateKey,
      walletPrivateKeyEnv: args.walletPrivateKeyEnv,
      threadOutRef: args.threadOutRef,
      addrTxWitsPreimagePath: args.addrTxWitsPreimagePath,
      nativeTxCompactPath: args.nativeTxCompactPath,
      witnessSetCompactPath: args.witnessSetCompactPath,
      badAddrTxWitIndex: args.badAddrTxWitIndex,
      awaitConfirmation: args.awaitConfirmation,
    });

    writeJson(output);
    return;
  }

  if (args.command === "submit-da-hash-preimage-step-01") {
    if (args.threadOutRef === undefined) {
      throw new Error(
        `Missing required --thread-out-ref <txHash#outputIndex>.\n${usage}`,
      );
    }
    if (args.stateQueueBlockOutRef === undefined) {
      throw new Error(
        `Missing required --state-queue-block-out-ref <txHash#outputIndex>.\n${usage}`,
      );
    }
    if (args.txInclusionPath === undefined) {
      throw new Error(`Missing required --tx-inclusion <path>.\n${usage}`);
    }
    const output = await submitDaHashPreimageStep01FromFiles({
      blueprintPath: args.blueprintPath,
      deploymentInfoPath: args.deploymentInfoPath,
      network: parseNetwork(args.network),
      provider: args.provider,
      blockfrostApiUrl: args.blockfrostApiUrl,
      blockfrostKey: args.blockfrostKey,
      kupoUrl: args.kupoUrl,
      ogmiosUrl: args.ogmiosUrl,
      walletSeedPhrase: args.walletSeedPhrase,
      walletSeedPhraseEnv: args.walletSeedPhraseEnv,
      walletPrivateKey: args.walletPrivateKey,
      walletPrivateKeyEnv: args.walletPrivateKeyEnv,
      threadOutRef: args.threadOutRef,
      stateQueueBlockOutRef: args.stateQueueBlockOutRef,
      txInclusionPath: args.txInclusionPath,
      awaitConfirmation: args.awaitConfirmation,
    });

    writeJson(output);
    return;
  }

  if (args.command === "submit-da-hash-preimage-step-02") {
    if (args.threadOutRef === undefined) {
      throw new Error(
        `Missing required --thread-out-ref <txHash#outputIndex>.\n${usage}`,
      );
    }
    const output = await submitDaHashPreimageStep02FromFiles({
      blueprintPath: args.blueprintPath,
      deploymentInfoPath: args.deploymentInfoPath,
      network: parseNetwork(args.network),
      provider: args.provider,
      blockfrostApiUrl: args.blockfrostApiUrl,
      blockfrostKey: args.blockfrostKey,
      kupoUrl: args.kupoUrl,
      ogmiosUrl: args.ogmiosUrl,
      walletSeedPhrase: args.walletSeedPhrase,
      walletSeedPhraseEnv: args.walletSeedPhraseEnv,
      walletPrivateKey: args.walletPrivateKey,
      walletPrivateKeyEnv: args.walletPrivateKeyEnv,
      threadOutRef: args.threadOutRef,
      awaitConfirmation: args.awaitConfirmation,
    });

    writeJson(output);
    return;
  }

  if (args.command === "submit-fabricated-deposit-step-01") {
    if (args.threadOutRef === undefined) {
      throw new Error(
        `Missing required --thread-out-ref <txHash#outputIndex>.\n${usage}`,
      );
    }
    if (args.stateQueueBlockOutRef === undefined) {
      throw new Error(
        `Missing required --state-queue-block-out-ref <txHash#outputIndex>.\n${usage}`,
      );
    }
    if (args.depositInclusionPath === undefined) {
      throw new Error(`Missing required --deposit-inclusion <path>.\n${usage}`);
    }
    const { contracts, referenceScriptUtxo } =
      await resolveFabricatedDepositCliContracts({
        config: {
          blueprintPath: args.blueprintPath,
          deploymentInfoPath: args.deploymentInfoPath,
          network: parseNetwork(args.network),
          provider: args.provider,
          blockfrostApiUrl: args.blockfrostApiUrl,
          blockfrostKey: args.blockfrostKey,
          kupoUrl: args.kupoUrl,
          ogmiosUrl: args.ogmiosUrl,
        },
        stepIndex: 0,
      });
    const output = await submitFabricatedDepositStep01FromFiles({
      blueprintPath: args.blueprintPath,
      deploymentInfoPath: args.deploymentInfoPath,
      network: parseNetwork(args.network),
      provider: args.provider,
      blockfrostApiUrl: args.blockfrostApiUrl,
      blockfrostKey: args.blockfrostKey,
      kupoUrl: args.kupoUrl,
      ogmiosUrl: args.ogmiosUrl,
      walletSeedPhrase: args.walletSeedPhrase,
      walletSeedPhraseEnv: args.walletSeedPhraseEnv,
      walletPrivateKey: args.walletPrivateKey,
      walletPrivateKeyEnv: args.walletPrivateKeyEnv,
      threadOutRef: args.threadOutRef,
      stateQueueBlockOutRef: args.stateQueueBlockOutRef,
      depositInclusionPath: args.depositInclusionPath,
      awaitConfirmation: args.awaitConfirmation,
      contracts,
      referenceScriptUtxo,
    });

    writeJson(output);
    return;
  }

  if (args.command === "submit-fabricated-deposit-step-02") {
    if (args.threadOutRef === undefined) {
      throw new Error(
        `Missing required --thread-out-ref <txHash#outputIndex>.\n${usage}`,
      );
    }
    const { contracts, referenceScriptUtxo } =
      await resolveFabricatedDepositCliContracts({
        config: {
          blueprintPath: args.blueprintPath,
          deploymentInfoPath: args.deploymentInfoPath,
          network: parseNetwork(args.network),
          provider: args.provider,
          blockfrostApiUrl: args.blockfrostApiUrl,
          blockfrostKey: args.blockfrostKey,
          kupoUrl: args.kupoUrl,
          ogmiosUrl: args.ogmiosUrl,
        },
        stepIndex: 1,
      });
    const output = await submitFabricatedDepositStep02FromFiles({
      network: parseNetwork(args.network),
      provider: args.provider,
      blockfrostApiUrl: args.blockfrostApiUrl,
      blockfrostKey: args.blockfrostKey,
      kupoUrl: args.kupoUrl,
      ogmiosUrl: args.ogmiosUrl,
      walletSeedPhrase: args.walletSeedPhrase,
      walletSeedPhraseEnv: args.walletSeedPhraseEnv,
      walletPrivateKey: args.walletPrivateKey,
      walletPrivateKeyEnv: args.walletPrivateKeyEnv,
      threadOutRef: args.threadOutRef,
      eventOutRef: args.eventOutRef,
      awaitConfirmation: args.awaitConfirmation,
      contracts,
      referenceScriptUtxo,
    });

    writeJson(output);
    return;
  }

  if (args.command === "submit-fabricated-deposit-step-03") {
    if (args.threadOutRef === undefined) {
      throw new Error(
        `Missing required --thread-out-ref <txHash#outputIndex>.\n${usage}`,
      );
    }
    const { contracts, referenceScriptUtxo } =
      await resolveFabricatedDepositCliContracts({
        config: {
          blueprintPath: args.blueprintPath,
          deploymentInfoPath: args.deploymentInfoPath,
          network: parseNetwork(args.network),
          provider: args.provider,
          blockfrostApiUrl: args.blockfrostApiUrl,
          blockfrostKey: args.blockfrostKey,
          kupoUrl: args.kupoUrl,
          ogmiosUrl: args.ogmiosUrl,
        },
        stepIndex: 2,
      });
    const output = await submitFabricatedDepositStep03FromFiles({
      network: parseNetwork(args.network),
      provider: args.provider,
      blockfrostApiUrl: args.blockfrostApiUrl,
      blockfrostKey: args.blockfrostKey,
      kupoUrl: args.kupoUrl,
      ogmiosUrl: args.ogmiosUrl,
      walletSeedPhrase: args.walletSeedPhrase,
      walletSeedPhraseEnv: args.walletSeedPhraseEnv,
      walletPrivateKey: args.walletPrivateKey,
      walletPrivateKeyEnv: args.walletPrivateKeyEnv,
      threadOutRef: args.threadOutRef,
      authenticContentPath: args.authenticContentPath,
      awaitConfirmation: args.awaitConfirmation,
      contracts,
      referenceScriptUtxo,
    });

    writeJson(output);
    return;
  }

  if (args.command === "submit-fabricated-deposit-step-04") {
    if (args.threadOutRef === undefined) {
      throw new Error(
        `Missing required --thread-out-ref <txHash#outputIndex>.\n${usage}`,
      );
    }
    const { contracts, referenceScriptUtxo, witnessReferenceScripts } =
      await resolveFabricatedDepositCliContracts({
        config: {
          blueprintPath: args.blueprintPath,
          deploymentInfoPath: args.deploymentInfoPath,
          network: parseNetwork(args.network),
          provider: args.provider,
          blockfrostApiUrl: args.blockfrostApiUrl,
          blockfrostKey: args.blockfrostKey,
          kupoUrl: args.kupoUrl,
          ogmiosUrl: args.ogmiosUrl,
        },
        stepIndex: 3,
      });
    const output = await submitFabricatedDepositStep04FromFiles({
      network: parseNetwork(args.network),
      provider: args.provider,
      blockfrostApiUrl: args.blockfrostApiUrl,
      blockfrostKey: args.blockfrostKey,
      kupoUrl: args.kupoUrl,
      ogmiosUrl: args.ogmiosUrl,
      walletSeedPhrase: args.walletSeedPhrase,
      walletSeedPhraseEnv: args.walletSeedPhraseEnv,
      walletPrivateKey: args.walletPrivateKey,
      walletPrivateKeyEnv: args.walletPrivateKeyEnv,
      threadOutRef: args.threadOutRef,
      awaitConfirmation: args.awaitConfirmation,
      contracts,
      referenceScriptUtxo,
      witnessReferenceScripts,
    });

    writeJson(output);
    return;
  }

  if (args.command === "submit-fabricated-withdrawal-step-01") {
    if (args.threadOutRef === undefined) {
      throw new Error(
        `Missing required --thread-out-ref <txHash#outputIndex>.\n${usage}`,
      );
    }
    if (args.stateQueueBlockOutRef === undefined) {
      throw new Error(
        `Missing required --state-queue-block-out-ref <txHash#outputIndex>.\n${usage}`,
      );
    }
    if (args.withdrawalInclusionPath === undefined) {
      throw new Error(
        `Missing required --withdrawal-inclusion <path>.\n${usage}`,
      );
    }
    const { contracts, referenceScriptUtxo } =
      await resolveFabricatedWithdrawalCliContracts({
        config: {
          blueprintPath: args.blueprintPath,
          deploymentInfoPath: args.deploymentInfoPath,
          network: parseNetwork(args.network),
          provider: args.provider,
          blockfrostApiUrl: args.blockfrostApiUrl,
          blockfrostKey: args.blockfrostKey,
          kupoUrl: args.kupoUrl,
          ogmiosUrl: args.ogmiosUrl,
        },
        stepIndex: 0,
      });
    const output = await submitFabricatedWithdrawalStep01FromFiles({
      blueprintPath: args.blueprintPath,
      deploymentInfoPath: args.deploymentInfoPath,
      network: parseNetwork(args.network),
      provider: args.provider,
      blockfrostApiUrl: args.blockfrostApiUrl,
      blockfrostKey: args.blockfrostKey,
      kupoUrl: args.kupoUrl,
      ogmiosUrl: args.ogmiosUrl,
      walletSeedPhrase: args.walletSeedPhrase,
      walletSeedPhraseEnv: args.walletSeedPhraseEnv,
      walletPrivateKey: args.walletPrivateKey,
      walletPrivateKeyEnv: args.walletPrivateKeyEnv,
      threadOutRef: args.threadOutRef,
      stateQueueBlockOutRef: args.stateQueueBlockOutRef,
      withdrawalInclusionPath: args.withdrawalInclusionPath,
      awaitConfirmation: args.awaitConfirmation,
      contracts,
      referenceScriptUtxo,
    });

    writeJson(output);
    return;
  }

  if (args.command === "submit-fabricated-withdrawal-step-02") {
    if (args.threadOutRef === undefined) {
      throw new Error(
        `Missing required --thread-out-ref <txHash#outputIndex>.\n${usage}`,
      );
    }
    const { contracts, referenceScriptUtxo } =
      await resolveFabricatedWithdrawalCliContracts({
        config: {
          blueprintPath: args.blueprintPath,
          deploymentInfoPath: args.deploymentInfoPath,
          network: parseNetwork(args.network),
          provider: args.provider,
          blockfrostApiUrl: args.blockfrostApiUrl,
          blockfrostKey: args.blockfrostKey,
          kupoUrl: args.kupoUrl,
          ogmiosUrl: args.ogmiosUrl,
        },
        stepIndex: 1,
      });
    const output = await submitFabricatedWithdrawalStep02FromFiles({
      network: parseNetwork(args.network),
      provider: args.provider,
      blockfrostApiUrl: args.blockfrostApiUrl,
      blockfrostKey: args.blockfrostKey,
      kupoUrl: args.kupoUrl,
      ogmiosUrl: args.ogmiosUrl,
      walletSeedPhrase: args.walletSeedPhrase,
      walletSeedPhraseEnv: args.walletSeedPhraseEnv,
      walletPrivateKey: args.walletPrivateKey,
      walletPrivateKeyEnv: args.walletPrivateKeyEnv,
      threadOutRef: args.threadOutRef,
      eventOutRef: args.eventOutRef,
      awaitConfirmation: args.awaitConfirmation,
      contracts,
      referenceScriptUtxo,
    });

    writeJson(output);
    return;
  }

  if (args.command === "submit-fabricated-withdrawal-step-03") {
    if (args.threadOutRef === undefined) {
      throw new Error(
        `Missing required --thread-out-ref <txHash#outputIndex>.\n${usage}`,
      );
    }
    const { contracts, referenceScriptUtxo } =
      await resolveFabricatedWithdrawalCliContracts({
        config: {
          blueprintPath: args.blueprintPath,
          deploymentInfoPath: args.deploymentInfoPath,
          network: parseNetwork(args.network),
          provider: args.provider,
          blockfrostApiUrl: args.blockfrostApiUrl,
          blockfrostKey: args.blockfrostKey,
          kupoUrl: args.kupoUrl,
          ogmiosUrl: args.ogmiosUrl,
        },
        stepIndex: 2,
      });
    const output = await submitFabricatedWithdrawalStep03FromFiles({
      network: parseNetwork(args.network),
      provider: args.provider,
      blockfrostApiUrl: args.blockfrostApiUrl,
      blockfrostKey: args.blockfrostKey,
      kupoUrl: args.kupoUrl,
      ogmiosUrl: args.ogmiosUrl,
      walletSeedPhrase: args.walletSeedPhrase,
      walletSeedPhraseEnv: args.walletSeedPhraseEnv,
      walletPrivateKey: args.walletPrivateKey,
      walletPrivateKeyEnv: args.walletPrivateKeyEnv,
      threadOutRef: args.threadOutRef,
      authenticContentPath: args.authenticContentPath,
      awaitConfirmation: args.awaitConfirmation,
      contracts,
      referenceScriptUtxo,
    });

    writeJson(output);
    return;
  }

  if (args.command === "submit-fabricated-withdrawal-step-04") {
    if (args.threadOutRef === undefined) {
      throw new Error(
        `Missing required --thread-out-ref <txHash#outputIndex>.\n${usage}`,
      );
    }
    const { contracts, referenceScriptUtxo, witnessReferenceScripts } =
      await resolveFabricatedWithdrawalCliContracts({
        config: {
          blueprintPath: args.blueprintPath,
          deploymentInfoPath: args.deploymentInfoPath,
          network: parseNetwork(args.network),
          provider: args.provider,
          blockfrostApiUrl: args.blockfrostApiUrl,
          blockfrostKey: args.blockfrostKey,
          kupoUrl: args.kupoUrl,
          ogmiosUrl: args.ogmiosUrl,
        },
        stepIndex: 3,
      });
    const output = await submitFabricatedWithdrawalStep04FromFiles({
      network: parseNetwork(args.network),
      provider: args.provider,
      blockfrostApiUrl: args.blockfrostApiUrl,
      blockfrostKey: args.blockfrostKey,
      kupoUrl: args.kupoUrl,
      ogmiosUrl: args.ogmiosUrl,
      walletSeedPhrase: args.walletSeedPhrase,
      walletSeedPhraseEnv: args.walletSeedPhraseEnv,
      walletPrivateKey: args.walletPrivateKey,
      walletPrivateKeyEnv: args.walletPrivateKeyEnv,
      threadOutRef: args.threadOutRef,
      awaitConfirmation: args.awaitConfirmation,
      contracts,
      referenceScriptUtxo,
      witnessReferenceScripts,
    });

    writeJson(output);
    return;
  }

  if (args.command === "remove-fraudulent-block") {
    const output = await submitRemoveFraudulentBlockFromFiles(
      buildRemoveFraudulentBlockCliConfig(args),
    );

    writeJson(output);
    return;
  }

  const output = await inspectContractsFromFiles({
    blueprintPath: args.blueprintPath,
    deploymentInfoPath: args.deploymentInfoPath,
    network: parseNetwork(args.network),
  });

  writeJson(output);
};
