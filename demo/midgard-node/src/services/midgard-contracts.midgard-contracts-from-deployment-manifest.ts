import type { DeploymentManifest } from "@al-ft/midgard-core/deployment-manifest-identity";
import {
  parseDeploymentManifestEventHistoryBounds,
  parseDeploymentManifestEventHistoryRetentionAddresses,
} from "@al-ft/midgard-core/deployment-manifest-identity";
import * as SDK from "@al-ft/midgard-sdk";
import {
  credentialToAddress,
  Network,
  scriptHashToCredential,
} from "@lucid-evolution/lucid";

import { TRANSITION_TRACE_FINAL_CONTRACT_NAMES } from "../deployable-scripts.js";
import {
  authenticatedValidatorFromManifest,
  mintingValidatorFromManifest,
  spendingValidatorFromManifest,
  withdrawalValidatorFromManifest,
} from "./midgard-contracts.assert-deployment-manifest-matches-config.js";
import {
  legacyFaultProofChainFromManifest,
  linearFaultProofChainFromManifest,
} from "./midgard-contracts.linear-fault-proof-chain-from-manifest.js";
import { requireManifestString } from "./midgard-contracts.load-reference-script-auth-validator.js";
import {
  eventHistoryContractsFromManifest,
  validationTraceDisputeFromManifest,
} from "./midgard-contracts.validation-trace-dispute-from-manifest.js";

export const midgardContractsFromDeploymentManifest = (
  network: Network,
  manifest: DeploymentManifest,
  sourcePath: string,
): SDK.MidgardValidators => {
  const eventHistory = eventHistoryContractsFromManifest(
    network,
    manifest,
    sourcePath,
  );
  const referenceScriptAuth = mintingValidatorFromManifest(
    manifest,
    sourcePath,
    "referenceScriptAuthMint",
  );
  const referenceScriptAuthPolicyId = requireManifestString(
    manifest.referenceScriptAuthPolicy?.policyId,
    "referenceScriptAuthPolicy.policyId",
    sourcePath,
  ).toLowerCase();
  if (referenceScriptAuth.policyId !== referenceScriptAuthPolicyId) {
    throw new Error(
      `Deployment manifest at "${sourcePath}" reference-script auth policy mismatch: contracts.referenceScriptAuthMint=${referenceScriptAuth.policyId}, referenceScriptAuthPolicy.policyId=${referenceScriptAuthPolicyId}`,
    );
  }
  const hubOracleMint = mintingValidatorFromManifest(
    manifest,
    sourcePath,
    "hubOracleMint",
  );
  // The canonical Aiken tree ships only the one-shot hub-oracle mint policy;
  // its witness lives at that policy's script credential, so the one script
  // that can govern the address is the mint script itself.
  const hubOracle: SDK.AuthenticatedValidator = {
    spendingScriptCBOR: hubOracleMint.mintingScriptCBOR,
    spendingScript: hubOracleMint.mintingScript,
    spendingScriptHash: hubOracleMint.policyId,
    spendingScriptAddress: credentialToAddress(
      network,
      scriptHashToCredential(hubOracleMint.policyId),
    ),
    ...hubOracleMint,
  };
  const txOrder = authenticatedValidatorFromManifest(
    network,
    manifest,
    sourcePath,
    "txOrderSpend",
    "txOrderMint",
  );
  // #579: no `txOrderFieldPreimage` or `txOrderFieldReceipt` resolution here.
  // The manifest no longer registers any of the three retired tx-field names,
  // so asking for one would throw on every manifest-sourced load.
  const fieldPreimageCertificate = {
    ...spendingValidatorFromManifest(
      network,
      manifest,
      sourcePath,
      "fieldPreimageCertificateSpend",
    ),
    ...mintingValidatorFromManifest(
      manifest,
      sourcePath,
      "fieldPreimageCertificateMint",
    ),
  };
  const cekProgramMaterial = spendingValidatorFromManifest(
    network,
    manifest,
    sourcePath,
    "cekProgramMaterialSpend",
  );
  const transitionTraceRoute = spendingValidatorFromManifest(
    network,
    manifest,
    sourcePath,
    "fraudProofTransitionTrace",
  );
  const transitionTraceFinals = TRANSITION_TRACE_FINAL_CONTRACT_NAMES.map(
    (contractName) =>
      spendingValidatorFromManifest(
        network,
        manifest,
        sourcePath,
        contractName,
      ),
  ) as unknown as SDK.FaultProofContractChains["transitionTrace"]["finals"];
  const transitionHistory = manifest.contracts.fraudProofTransitionTrace;
  const transitionBounds = parseDeploymentManifestEventHistoryBounds(
    transitionHistory.eventHistoryBounds,
  );
  const transitionTrace: SDK.FaultProofContractChains["transitionTrace"] = {
    history: {
      inlineLimitBytes: BigInt(transitionBounds.inlineLimitBytes),
      maxPayloadBytes: BigInt(transitionBounds.maxPayloadBytes),
      maxPayloadNodes: BigInt(transitionBounds.maxPayloadNodes),
      retentionAddresses: parseDeploymentManifestEventHistoryRetentionAddresses(
        transitionHistory.eventHistoryRetentionAddresses,
      ),
    },
    firstStep: transitionTraceRoute,
    route: transitionTraceRoute,
    finals: transitionTraceFinals,
    steps: [transitionTraceRoute, ...transitionTraceFinals],
    yields: {
      l2Open: withdrawalValidatorFromManifest(
        manifest,
        sourcePath,
        "fraudProofTransitionTraceAcceptedTransactionL2OpenWithdraw",
      ),
      l2Summaries: withdrawalValidatorFromManifest(
        manifest,
        sourcePath,
        "fraudProofTransitionTraceAcceptedTransactionL2SummariesWithdraw",
      ),
      l2Replay: withdrawalValidatorFromManifest(
        manifest,
        sourcePath,
        "fraudProofTransitionTraceAcceptedTransactionL2ReplayWithdraw",
      ),
      claimStructure: withdrawalValidatorFromManifest(
        manifest,
        sourcePath,
        "fraudProofTransitionTraceAcceptedTransactionClaimStructureWithdraw",
      ),
      claimSource: withdrawalValidatorFromManifest(
        manifest,
        sourcePath,
        "fraudProofTransitionTraceAcceptedTransactionClaimSourceWithdraw",
      ),
      claimEndpoints: withdrawalValidatorFromManifest(
        manifest,
        sourcePath,
        "fraudProofTransitionTraceAcceptedTransactionClaimEndpointsWithdraw",
      ),
      depositProjection: withdrawalValidatorFromManifest(
        manifest,
        sourcePath,
        "fraudProofTransitionTraceDepositProjectionWithdraw",
      ),
      l1Event: withdrawalValidatorFromManifest(
        manifest,
        sourcePath,
        "fraudProofTransitionTraceL1EventWithdraw",
      ),
      forcedTiming: withdrawalValidatorFromManifest(
        manifest,
        sourcePath,
        "fraudProofTransitionTraceForcedTimingWithdraw",
      ),
      depositSummaries: withdrawalValidatorFromManifest(
        manifest,
        sourcePath,
        "fraudProofTransitionTraceDepositSummariesWithdraw",
      ),
      l2Assembly: withdrawalValidatorFromManifest(
        manifest,
        sourcePath,
        "fraudProofTransitionTraceAcceptedTransactionL2AssemblyWithdraw",
      ),
      l2Scan: withdrawalValidatorFromManifest(
        manifest,
        sourcePath,
        "fraudProofTransitionTraceAcceptedTransactionL2ScanWithdraw",
      ),
      l2Value: withdrawalValidatorFromManifest(
        manifest,
        sourcePath,
        "fraudProofTransitionTraceAcceptedTransactionL2ValueWithdraw",
      ),
      depositAssembly: withdrawalValidatorFromManifest(
        manifest,
        sourcePath,
        "fraudProofTransitionTraceDepositAssemblyWithdraw",
      ),
      depositScan: withdrawalValidatorFromManifest(
        manifest,
        sourcePath,
        "fraudProofTransitionTraceDepositScanWithdraw",
      ),
      depositValue: withdrawalValidatorFromManifest(
        manifest,
        sourcePath,
        "fraudProofTransitionTraceDepositValueWithdraw",
      ),
      depositReplay: withdrawalValidatorFromManifest(
        manifest,
        sourcePath,
        "fraudProofTransitionTraceDepositReplayWithdraw",
      ),
    },
  };
  const validationTraceDispute = validationTraceDisputeFromManifest(
    network,
    manifest,
    sourcePath,
    cekProgramMaterial,
  );
  const fraudProofContracts: SDK.FaultProofContractChains = {
    doubleSpend: legacyFaultProofChainFromManifest(
      network,
      manifest,
      sourcePath,
      "doubleSpend",
    ),
    nonExistentInput: legacyFaultProofChainFromManifest(
      network,
      manifest,
      sourcePath,
      "nonExistentInput",
    ),
    nonExistentInputNoIndex: legacyFaultProofChainFromManifest(
      network,
      manifest,
      sourcePath,
      "nonExistentInputNoIndex",
    ),
    invalidRange: legacyFaultProofChainFromManifest(
      network,
      manifest,
      sourcePath,
      "invalidRange",
    ),
    transitionTrace,
    zeroInput: legacyFaultProofChainFromManifest(
      network,
      manifest,
      sourcePath,
      "zeroInput",
    ),
    validationTraceDispute,
    daHashPreimage: legacyFaultProofChainFromManifest(
      network,
      manifest,
      sourcePath,
      "daHashPreimage",
    ),
    noReferenceInput: legacyFaultProofChainFromManifest(
      network,
      manifest,
      sourcePath,
      "noReferenceInput",
    ),
    referenceInputNoIdx: legacyFaultProofChainFromManifest(
      network,
      manifest,
      sourcePath,
      "referenceInputNoIdx",
    ),
    invalidSignature: legacyFaultProofChainFromManifest(
      network,
      manifest,
      sourcePath,
      "invalidSignature",
    ),
    fabricatedDeposit: linearFaultProofChainFromManifest(
      network,
      manifest,
      sourcePath,
      "fabricatedDeposit",
    ),
    fabricatedWithdrawal: linearFaultProofChainFromManifest(
      network,
      manifest,
      sourcePath,
      "fabricatedWithdrawal",
    ),
    nativeScriptDecoding: linearFaultProofChainFromManifest(
      network,
      manifest,
      sourcePath,
      "nativeScriptDecoding",
    ),
    missingSignature: linearFaultProofChainFromManifest(
      network,
      manifest,
      sourcePath,
      "missingSignature",
    ),
    withdrawnReferenceInput: linearFaultProofChainFromManifest(
      network,
      manifest,
      sourcePath,
      "withdrawnReferenceInput",
    ),
    canonicalDecodability: linearFaultProofChainFromManifest(
      network,
      manifest,
      sourcePath,
      "canonicalDecodability",
    ),
    committedFieldShape: linearFaultProofChainFromManifest(
      network,
      manifest,
      sourcePath,
      "committedFieldShape",
    ),
    minFee: linearFaultProofChainFromManifest(
      network,
      manifest,
      sourcePath,
      "minFee",
    ),
    withdrawalMistag: linearFaultProofChainFromManifest(
      network,
      manifest,
      sourcePath,
      "withdrawalMistag",
    ),
    doubleWithdraw: linearFaultProofChainFromManifest(
      network,
      manifest,
      sourcePath,
      "doubleWithdraw",
    ),
    crossBlockDuplicateEvent: linearFaultProofChainFromManifest(
      network,
      manifest,
      sourcePath,
      "crossBlockDuplicateEvent",
    ),
    l2TxMistag: linearFaultProofChainFromManifest(
      network,
      manifest,
      sourcePath,
      "l2TxMistag",
    ),
    withdrawnInput: linearFaultProofChainFromManifest(
      network,
      manifest,
      sourcePath,
      "withdrawnInput",
    ),
    valueNotPreserved: linearFaultProofChainFromManifest(
      network,
      manifest,
      sourcePath,
      "valueNotPreserved",
    ),
    inputSetUniqueness: linearFaultProofChainFromManifest(
      network,
      manifest,
      sourcePath,
      "inputSetUniqueness",
    ),
    mintAuthorization: linearFaultProofChainFromManifest(
      network,
      manifest,
      sourcePath,
      "mintAuthorization",
    ),
    networkId: linearFaultProofChainFromManifest(
      network,
      manifest,
      sourcePath,
      "networkId",
    ),
    nativeScriptInvalid: linearFaultProofChainFromManifest(
      network,
      manifest,
      sourcePath,
      "nativeScriptInvalid",
    ),
    minAda: linearFaultProofChainFromManifest(
      network,
      manifest,
      sourcePath,
      "minAda",
    ),
    fieldPreimageLengthMismatch: linearFaultProofChainFromManifest(
      network,
      manifest,
      sourcePath,
      "fieldPreimageLengthMismatch",
    ),
    fieldItemWidthIllegal: linearFaultProofChainFromManifest(
      network,
      manifest,
      sourcePath,
      "fieldItemWidthIllegal",
    ),
    witnessScriptDecoding: linearFaultProofChainFromManifest(
      network,
      manifest,
      sourcePath,
      "witnessScriptDecoding",
    ),
    scriptIntegrityHashMissing: linearFaultProofChainFromManifest(
      network,
      manifest,
      sourcePath,
      "scriptIntegrityHashMissing",
    ),
    transactionOutputNonCanonical: linearFaultProofChainFromManifest(
      network,
      manifest,
      sourcePath,
      "transactionOutputNonCanonical",
    ),
    mintItemNonCanonical: linearFaultProofChainFromManifest(
      network,
      manifest,
      sourcePath,
      "mintItemNonCanonical",
    ),
    resolvedOutputNonCanonical: linearFaultProofChainFromManifest(
      network,
      manifest,
      sourcePath,
      "resolvedOutputNonCanonical",
    ),
    mintDeclaredAssetLimit: linearFaultProofChainFromManifest(
      network,
      manifest,
      sourcePath,
      "mintDeclaredAssetLimit",
    ),
    spendInputSignerMissing: linearFaultProofChainFromManifest(
      network,
      manifest,
      sourcePath,
      "spendInputSignerMissing",
    ),
    protectedOutputSignerMissing: linearFaultProofChainFromManifest(
      network,
      manifest,
      sourcePath,
      "protectedOutputSignerMissing",
    ),
    observersForbiddenOnUntaggedNetwork: linearFaultProofChainFromManifest(
      network,
      manifest,
      sourcePath,
      "observersForbiddenOnUntaggedNetwork",
    ),
    outputReferenceScriptDecoding: linearFaultProofChainFromManifest(
      network,
      manifest,
      sourcePath,
      "outputReferenceScriptDecoding",
    ),
    executionSourceScriptDecoding: linearFaultProofChainFromManifest(
      network,
      manifest,
      sourcePath,
      "executionSourceScriptDecoding",
    ),
    observerOrderInvalid: linearFaultProofChainFromManifest(
      network,
      manifest,
      sourcePath,
      "observerOrderInvalid",
    ),
    redeemerCanonicity: linearFaultProofChainFromManifest(
      network,
      manifest,
      sourcePath,
      "redeemerCanonicity",
    ),
    receivePurposeLanguage: linearFaultProofChainFromManifest(
      network,
      manifest,
      sourcePath,
      "receivePurposeLanguage",
    ),
    unusedScriptWitness: linearFaultProofChainFromManifest(
      network,
      manifest,
      sourcePath,
      "unusedScriptWitness",
    ),
    missingScriptSource: linearFaultProofChainFromManifest(
      network,
      manifest,
      sourcePath,
      "missingScriptSource",
    ),
    missingRedeemer: linearFaultProofChainFromManifest(
      network,
      manifest,
      sourcePath,
      "missingRedeemer",
    ),
    unusedRedeemer: linearFaultProofChainFromManifest(
      network,
      manifest,
      sourcePath,
      "unusedRedeemer",
    ),
    executionNativeScriptInvalid: linearFaultProofChainFromManifest(
      network,
      manifest,
      sourcePath,
      "executionNativeScriptInvalid",
    ),
    scriptIntegrityHashMismatch: linearFaultProofChainFromManifest(
      network,
      manifest,
      sourcePath,
      "scriptIntegrityHashMismatch",
    ),
    distinctAssetAccumulationLimit: linearFaultProofChainFromManifest(
      network,
      manifest,
      sourcePath,
      "distinctAssetAccumulationLimit",
    ),
  };
  const fraudProofs = SDK.fraudProofContractsToFirstSteps(fraudProofContracts);

  const contracts: SDK.MidgardValidators = {
    referenceScriptAuth,
    hubOracle,
    daParamsGovernor: authenticatedValidatorFromManifest(
      network,
      manifest,
      sourcePath,
      "daParamsGovernorSpend",
      "daParamsGovernorMint",
    ),
    daBondPool: authenticatedValidatorFromManifest(
      network,
      manifest,
      sourcePath,
      "daBondPoolSpend",
      "daBondPoolMint",
    ),
    daAttestation: authenticatedValidatorFromManifest(
      network,
      manifest,
      sourcePath,
      "daAttestationSpend",
      "daAttestationMint",
    ),
    availabilityChallenge: {
      ...authenticatedValidatorFromManifest(
        network,
        manifest,
        sourcePath,
        "availabilityChallengeSpend",
        "availabilityChallengeMint",
      ),
      yields: {
        open: withdrawalValidatorFromManifest(
          manifest,
          sourcePath,
          "availabilityChallengeOpenWithdraw",
        ),
        settle: withdrawalValidatorFromManifest(
          manifest,
          sourcePath,
          "availabilityChallengeSettleWithdraw",
        ),
        close: withdrawalValidatorFromManifest(
          manifest,
          sourcePath,
          "availabilityChallengeCloseWithdraw",
        ),
        timeout: withdrawalValidatorFromManifest(
          manifest,
          sourcePath,
          "availabilityChallengeTimeoutWithdraw",
        ),
      },
    },
    correctionLock: spendingValidatorFromManifest(
      network,
      manifest,
      sourcePath,
      "correctionLockSpend",
    ),
    stateQueue: {
      ...authenticatedValidatorFromManifest(
        network,
        manifest,
        sourcePath,
        "stateQueueSpend",
        "stateQueueMint",
      ),
      yields: {
        commit: withdrawalValidatorFromManifest(
          manifest,
          sourcePath,
          "stateQueueCommitWithdraw",
        ),
        unattestedTimeout: withdrawalValidatorFromManifest(
          manifest,
          sourcePath,
          "stateQueueUnattestedTimeoutWithdraw",
        ),
        unavailableTimeout: withdrawalValidatorFromManifest(
          manifest,
          sourcePath,
          "stateQueueUnavailableTimeoutWithdraw",
        ),
        fraudRemoval: withdrawalValidatorFromManifest(
          manifest,
          sourcePath,
          "stateQueueFraudRemovalWithdraw",
        ),
        merge: withdrawalValidatorFromManifest(
          manifest,
          sourcePath,
          "stateQueueMergeWithdraw",
        ),
      },
    },
    scheduler: authenticatedValidatorFromManifest(
      network,
      manifest,
      sourcePath,
      "schedulerSpend",
      "schedulerMint",
    ),
    registeredOperators: authenticatedValidatorFromManifest(
      network,
      manifest,
      sourcePath,
      "registeredOperatorsSpend",
      "registeredOperatorsMint",
    ),
    activeOperators: authenticatedValidatorFromManifest(
      network,
      manifest,
      sourcePath,
      "activeOperatorsSpend",
      "activeOperatorsMint",
    ),
    retiredOperators: authenticatedValidatorFromManifest(
      network,
      manifest,
      sourcePath,
      "retiredOperatorsSpend",
      "retiredOperatorsMint",
    ),
    escapeHatch: authenticatedValidatorFromManifest(
      network,
      manifest,
      sourcePath,
      "escapeHatchSpend",
      "escapeHatchMint",
    ),
    fraudProofCatalogue: authenticatedValidatorFromManifest(
      network,
      manifest,
      sourcePath,
      "fraudProofCatalogueSpend",
      "fraudProofCatalogueMint",
    ),
    computationThread: mintingValidatorFromManifest(
      manifest,
      sourcePath,
      "computationThreadMint",
    ),
    fraudProof: authenticatedValidatorFromManifest(
      network,
      manifest,
      sourcePath,
      "fraudProofSpend",
      "fraudProofMint",
    ),
    chunkedVerify: withdrawalValidatorFromManifest(
      manifest,
      sourcePath,
      "chunkedVerifyWithdraw",
    ),
    pexcludes: withdrawalValidatorFromManifest(
      manifest,
      sourcePath,
      "pexcludesWithdraw",
    ),
    eventHistory,
    deposit: eventHistory.deposit.list,
    withdrawal: eventHistory.withdrawal.list,
    txOrder,
    fieldPreimageCertificate,
    cekProgramMaterial,
    settlement: authenticatedValidatorFromManifest(
      network,
      manifest,
      sourcePath,
      "settlementSpend",
      "settlementMint",
    ),
    reserve: {
      ...spendingValidatorFromManifest(
        network,
        manifest,
        sourcePath,
        "reserveSpend",
      ),
      ...withdrawalValidatorFromManifest(
        manifest,
        sourcePath,
        "reserveWithdraw",
      ),
    },
    payout: authenticatedValidatorFromManifest(
      network,
      manifest,
      sourcePath,
      "payoutSpend",
      "payoutMint",
    ),
    fraudProofContracts,
    fraudProofs,
  };
  return contracts;
};
