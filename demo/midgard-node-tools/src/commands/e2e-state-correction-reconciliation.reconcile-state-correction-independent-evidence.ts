import { readFile } from "node:fs/promises";

import { computeDeploymentManifestJsonDigest } from "@al-ft/midgard-core";
import { type FraudProofWorkflowTerminal } from "@al-ft/midgard-fault-proofs";
import { parseDeploymentManifestValue } from "midgard-node/deployment-manifest";

import type { DbEvidence, RawEvidenceRef } from "../e2e/summary.js";
import type { E2EStateCorrectionAcceptance } from "./e2e-state-correction-acceptance.js";
import { REQUIRED_STATE_CORRECTION_GATE_LABELS } from "./e2e-state-correction-acceptance.js";
import {
  deriveAuthenticatedL1Observation,
  parseAuthenticatedL1Observation,
  parseRecoveryObservation,
} from "./e2e-state-correction-reconciliation.derive-authenticated-l1-observation.js";
import {
  type ChainPoint,
  exactKeys,
  record,
  type StateCorrectionIndependentAuthority,
  type StateCorrectionIndependentEvidence,
  type StateCorrectionIndependentSourcePaths,
} from "./e2e-state-correction-reconciliation.final-snapshot.js";
import {
  loadWorkflow,
  manifestCatalogue,
  parseNodeDatabaseExport,
  requiredTransactions,
} from "./e2e-state-correction-reconciliation.load-workflow.js";
import { parseFinalSnapshot } from "./e2e-state-correction-reconciliation.parse-final-snapshot.js";
import {
  assertEqual,
  canonicalLovelace,
  jsonDigest,
  parseKupoMatches,
  parseOgmiosTip,
  readDigestCheckedJson,
  readJson,
  sha256,
} from "./e2e-state-correction-reconciliation.parse-kupo-matches.js";

export const reconcileStateCorrectionIndependentEvidence = async ({
  expectedRunId,
  claim,
  paths,
  authority,
}: {
  readonly expectedRunId: string;
  readonly claim: E2EStateCorrectionAcceptance;
  readonly paths: StateCorrectionIndependentSourcePaths;
  readonly authority: StateCorrectionIndependentAuthority;
}): Promise<StateCorrectionIndependentEvidence> => {
  assertEqual(claim.runId, expectedRunId, "state-correction acceptance run");
  const [
    manifestValue,
    blueprintBytes,
    catalogueValue,
    parametersValue,
    workflows,
    l1Values,
    recoveryValues,
    finalSnapshotValue,
  ] = await Promise.all([
    readJson(paths.deploymentManifestPath),
    readFile(paths.blueprintPath),
    readJson(paths.cataloguePath),
    readJson(paths.parametersPath),
    Promise.all(paths.workflowJournalDirectories.map(loadWorkflow)),
    Promise.all(paths.l1ObservationPaths.map(readJson)),
    Promise.all(paths.recoveryObservationPaths.map(readJson)),
    readJson(paths.finalSnapshotPath),
  ]);

  const manifest = parseDeploymentManifestValue(manifestValue);
  if (manifest.network !== "Preprod") {
    throw new Error(
      `deployment manifest network must be Preprod, found ${manifest.network}`,
    );
  }
  const catalogueCandidate = record(catalogueValue, "catalogue source");
  exactKeys(catalogueCandidate, ["root", "categories"], "catalogue source");
  const catalogue = manifestCatalogue(manifest);
  assertEqual(catalogueValue, catalogue, "independent catalogue source");
  const blueprintSha256 = sha256(blueprintBytes);
  const parametersSha256 = computeDeploymentManifestJsonDigest(parametersValue);
  assertEqual(
    manifest.manifestId,
    claim.deployment.manifestId,
    "manifest identity",
  );
  assertEqual(
    manifest.artifacts.blueprintHash,
    blueprintSha256,
    "manifest/blueprint identity",
  );
  assertEqual(
    claim.deployment.blueprintSha256,
    blueprintSha256,
    "claim blueprint identity",
  );
  assertEqual(catalogue.root, claim.deployment.catalogueRoot, "catalogue root");
  assertEqual(
    manifest.cardanoProtocolParameters.snapshot,
    parametersValue,
    "protocol parameter snapshot",
  );
  assertEqual(
    manifest.cardanoProtocolParameters.digest,
    parametersSha256,
    "manifest parameter digest",
  );
  assertEqual(
    claim.deployment.parametersSha256,
    parametersSha256,
    "claim parameter digest",
  );

  if (workflows.length !== claim.families.length) {
    throw new Error(
      `workflow journal count mismatch: expected ${claim.families.length.toString()}, found ${workflows.length.toString()}`,
    );
  }
  const workflowByCategory = new Map(
    workflows.map((workflow) => [
      workflow.entries[0]!.identity.category,
      workflow,
    ]),
  );
  if (workflowByCategory.size !== workflows.length) {
    throw new Error("workflow journals contain duplicate family categories");
  }
  const terminals = new Map<string, FraudProofWorkflowTerminal>();
  for (const family of claim.families) {
    const workflow = workflowByCategory.get(
      family.familyId as Parameters<typeof workflowByCategory.get>[0],
    );
    if (workflow === undefined) {
      throw new Error(`missing workflow journal for ${family.familyId}`);
    }
    const identity = workflow.entries[0]!.identity;
    const terminal = workflow.terminal;
    assertEqual(
      identity.deploymentFingerprint,
      manifest.manifestId,
      `${family.familyId} workflow deployment`,
    );
    assertEqual(
      identity.target,
      { kind: "state_queue_header", headerHash: family.headerHash },
      `${family.familyId} workflow target`,
    );
    assertEqual(
      terminal.category,
      family.familyId,
      `${family.familyId} terminal category`,
    );
    assertEqual(
      terminal.headerHash,
      family.headerHash,
      `${family.familyId} terminal header`,
    );
    assertEqual(
      terminal.proofToken.createdByTxHash,
      family.proofTokenTxHash,
      `${family.familyId} proof-token creation`,
    );
    assertEqual(
      terminal.proofToken.retainedAtFinalState,
      true,
      `${family.familyId} permanent proof-token retention`,
    );
    assertEqual(
      terminal.correction.removalTxHash,
      family.removalTxHash,
      `${family.familyId} removal`,
    );
    assertEqual(
      terminal.correction.referencedProofTokenOutRef,
      terminal.proofToken.outRef,
      `${family.familyId} removal proof-token reference`,
    );
    assertEqual(
      family.correctionTxHash,
      family.removalTxHash,
      `${family.familyId} correction/removal identity`,
    );
    assertEqual(
      terminal.economics.slashedLovelace,
      family.expectedSlashLovelace,
      `${family.familyId} exact slash`,
    );
    assertEqual(
      terminal.economics.proverRewardLovelace,
      family.expectedProverRewardLovelace,
      `${family.familyId} exact prover reward`,
    );
    assertEqual(
      {
        slot: terminal.observedAt.slot,
        blockHash: terminal.observedAt.blockHash,
      },
      family.chainPoint,
      `${family.familyId} final chain point`,
    );
    const familyTxHashes = new Set([
      family.initTxHash,
      ...family.proofStepTxHashes,
      family.proofTokenTxHash,
      family.removalTxHash,
      family.correctionTxHash,
    ]);
    for (const txHash of familyTxHashes) {
      if (!workflow.confirmedTxHashes.has(txHash)) {
        throw new Error(
          `${family.familyId} required transaction ${txHash} is not confirmed in its journal`,
        );
      }
    }
    if (workflow.confirmedTxHashes.size !== familyTxHashes.size) {
      throw new Error(
        `${family.familyId} workflow has confirmed transactions omitted from the aggregate claim`,
      );
    }
    terminals.set(family.familyId, terminal);
  }

  const claimedL1Observations = l1Values.map((value, index) =>
    parseAuthenticatedL1Observation(
      value,
      `L1 observation ${paths.l1ObservationPaths[index] ?? index.toString()}`,
    ),
  );
  const derivedL1Observations = await Promise.all(
    claimedL1Observations.map((observation, index) =>
      deriveAuthenticatedL1Observation({
        observation,
        observationPath: paths.l1ObservationPaths[index]!,
        authority,
      }),
    ),
  );
  const l1Observations = derivedL1Observations.map(
    (derived) => derived.observation,
  );
  const observationByTxHash = new Map(
    l1Observations.map((observation, index) => [
      observation.txHash,
      { observation, path: paths.l1ObservationPaths[index]! },
    ]),
  );
  if (observationByTxHash.size !== l1Observations.length) {
    throw new Error(
      "authenticated L1 observations contain duplicate transaction hashes",
    );
  }
  const required = requiredTransactions(claim);
  const requiredHashes = new Set(required.map((entry) => entry.txHash));
  for (const requiredTx of required) {
    const observed = observationByTxHash.get(requiredTx.txHash)?.observation;
    if (observed === undefined) {
      throw new Error(
        `required transaction ${requiredTx.label}:${requiredTx.txHash} has no authenticated L1 observation`,
      );
    }
    assertEqual(
      observed.runId,
      claim.runId,
      `${requiredTx.label} observation run`,
    );
    assertEqual(
      observed.manifestId,
      manifest.manifestId,
      `${requiredTx.label} observation deployment`,
    );
  }
  if (observationByTxHash.size !== requiredHashes.size) {
    throw new Error(
      "authenticated L1 observations contain transactions outside the required Q57 set",
    );
  }
  for (const family of claim.families) {
    const terminal = terminals.get(family.familyId)!;
    const removalObservation = observationByTxHash.get(
      family.removalTxHash,
    )!.observation;
    assertEqual(
      removalObservation.observedAtTip,
      terminal.observedAt,
      `${family.familyId} authenticated terminal chain observation`,
    );
  }
  const terminalPoint = (txHash: string): ChainPoint => {
    const observed = observationByTxHash.get(txHash)?.observation.observedAtTip;
    if (observed === undefined) {
      throw new Error(
        `missing authenticated terminal observation for ${txHash}`,
      );
    }
    return { slot: observed.slot, blockHash: observed.blockHash };
  };
  assertEqual(
    terminalPoint(claim.withdrawalReservePayout.payoutConcludeTxHash),
    claim.withdrawalReservePayout.chainPoint,
    "withdrawal payout authenticated terminal chain point",
  );
  for (const drill of claim.forcedClassifications) {
    assertEqual(
      terminalPoint(drill.correctionTxHash),
      drill.chainPoint,
      `${drill.direction} authenticated terminal chain point`,
    );
  }

  if (recoveryValues.length !== claim.recoveryDrills.length) {
    throw new Error(
      `recovery observation count mismatch: expected ${claim.recoveryDrills.length.toString()}, found ${recoveryValues.length.toString()}`,
    );
  }
  const recoveryObservations = recoveryValues.map((value, index) =>
    parseRecoveryObservation(
      value,
      `recovery observation ${paths.recoveryObservationPaths[index] ?? index.toString()}`,
    ),
  );
  for (const [index, observed] of recoveryObservations.entries()) {
    const claimed = claim.recoveryDrills[index]!;
    assertEqual(
      observed.id,
      claimed.id,
      `recovery observation ${index.toString()} id`,
    );
    assertEqual(observed.runId, claim.runId, `${observed.id} run`);
    assertEqual(
      observed.manifestId,
      manifest.manifestId,
      `${observed.id} deployment`,
    );
    for (const [field, count] of Object.entries({
      duplicateSubmissionCount: observed.duplicateSubmissionCount,
      lostEvidenceCount: observed.lostEvidenceCount,
      verifiedBeforeReconciliationCount:
        observed.verifiedBeforeReconciliationCount,
      unrecoverableWorkflowCount: observed.unrecoverableWorkflowCount,
      manualRepairCount: observed.manualRepairCount,
    })) {
      if (count !== 0) throw new Error(`${observed.id} ${field} must be zero`);
    }
    const path = paths.recoveryObservationPaths[index]!;
    assertEqual(
      claimed.evidenceSha256,
      sha256(await readFile(path)),
      `${observed.id} raw evidence digest`,
    );
  }

  const finalSnapshot = parseFinalSnapshot(finalSnapshotValue);
  assertEqual(finalSnapshot.runId, claim.runId, "final snapshot run");
  assertEqual(
    finalSnapshot.manifestId,
    manifest.manifestId,
    "final snapshot deployment",
  );
  const finalAuthentication = finalSnapshot.authentication;
  const [
    rawStateQueue,
    rawFinalOgmiosTip,
    rawNodeDatabaseExport,
    rawProofTokens,
  ] = await Promise.all([
    readDigestCheckedJson({
      parentPath: paths.finalSnapshotPath,
      childPath: finalAuthentication.kupoStateQueueResponsePath,
      expectedSha256: finalAuthentication.kupoStateQueueResponseSha256,
      field: "final raw Kupo state-queue response",
    }),
    readDigestCheckedJson({
      parentPath: paths.finalSnapshotPath,
      childPath: finalAuthentication.ogmiosTipResponsePath,
      expectedSha256: finalAuthentication.ogmiosTipResponseSha256,
      field: "final raw Ogmios tip response",
    }),
    readDigestCheckedJson({
      parentPath: paths.finalSnapshotPath,
      childPath: finalAuthentication.nodeDatabaseExportPath,
      expectedSha256: finalAuthentication.nodeDatabaseExportSha256,
      field: "final raw node database export",
    }),
    Promise.all(
      finalAuthentication.kupoProofTokenResponses.map((response, index) =>
        readDigestCheckedJson({
          parentPath: paths.finalSnapshotPath,
          childPath: response.responsePath,
          expectedSha256: response.responseSha256,
          field: `final raw Kupo proof-token response ${index.toString()}`,
        }),
      ),
    ),
  ]);
  const rawStateQueueMatches = parseKupoMatches(
    rawStateQueue.value,
    "final raw Kupo state-queue response",
  );
  if (rawStateQueueMatches.length !== 0) {
    throw new Error("final raw Kupo state-queue response is not drained");
  }
  const expectedRetainedProofTokens = claim.families.map((family) => {
    const token = terminals.get(family.familyId)!.proofToken;
    return { unit: token.unit, outRef: token.outRef };
  });
  assertEqual(
    finalAuthentication.kupoProofTokenResponses.map(({ unit, outRef }) => ({
      unit,
      outRef,
    })),
    expectedRetainedProofTokens,
    "final Kupo retained proof-token query set",
  );
  const proofTokenKeys = new Set<string>();
  for (const [index, rawProofToken] of rawProofTokens.entries()) {
    const declared = finalAuthentication.kupoProofTokenResponses[index]!;
    const key = `${declared.unit}:${declared.outRef}`;
    if (proofTokenKeys.has(key)) {
      throw new Error(`duplicate final Kupo proof-token query ${key}`);
    }
    proofTokenKeys.add(key);
    const matches = parseKupoMatches(
      rawProofToken.value,
      `final raw Kupo proof-token response ${index.toString()}`,
    );
    if (matches.length !== 1) {
      throw new Error(
        `final raw Kupo proof-token response ${index.toString()} must contain exactly one match`,
      );
    }
    const match = matches[0]!;
    const separator = declared.outRef.lastIndexOf("#");
    const expectedTxHash = declared.outRef.slice(0, separator);
    const expectedOutputIndex = Number(declared.outRef.slice(separator + 1));
    assertEqual(
      { txHash: match.transactionId, outputIndex: match.outputIndex },
      { txHash: expectedTxHash, outputIndex: expectedOutputIndex },
      `final retained proof-token ${key} output reference`,
    );
    if (match.spentAt !== null) {
      throw new Error(`final permanent proof token ${key} is spent`);
    }
    const matchingAssetEntries = Object.entries(match.assets).filter(
      ([assetKey]) => assetKey.replaceAll(".", "") === declared.unit,
    );
    if (
      matchingAssetEntries.length !== 1 ||
      canonicalLovelace(
        matchingAssetEntries[0]?.[1],
        `final retained proof-token ${key} quantity`,
      ) !== "1"
    ) {
      throw new Error(
        `final permanent proof token ${key} is missing or not retained at quantity one`,
      );
    }
  }
  const finalOgmiosTip = parseOgmiosTip(
    rawFinalOgmiosTip.value,
    "final raw Ogmios tip response",
  );
  const newestRequiredInclusionHeight = Math.max(
    ...derivedL1Observations.map((derived) => derived.inclusionHeight),
  );
  if (finalOgmiosTip.height < newestRequiredInclusionHeight) {
    throw new Error(
      "final raw Ogmios tip precedes a required Q57 transaction inclusion",
    );
  }
  const derivedFinalObservedAt = {
    slot: finalOgmiosTip.slot,
    blockHash: finalOgmiosTip.blockHash,
    confirmationDepth:
      finalOgmiosTip.height - newestRequiredInclusionHeight + 1,
  };
  assertEqual(
    finalSnapshot.observedAt,
    derivedFinalObservedAt,
    "final snapshot/raw Ogmios observation",
  );
  const nodeDatabaseExport = parseNodeDatabaseExport(
    rawNodeDatabaseExport.value,
  );
  assertEqual(
    nodeDatabaseExport.runId,
    claim.runId,
    "node database export run",
  );
  assertEqual(
    nodeDatabaseExport.manifestId,
    manifest.manifestId,
    "node database export deployment",
  );
  assertEqual(
    nodeDatabaseExport.stateQueue,
    { depth: rawStateQueueMatches.length, fraudulentHeaderHashes: [] },
    "Kupo/node database final state queue",
  );
  assertEqual(
    {
      stateQueue: finalSnapshot.stateQueue,
      jobs: finalSnapshot.jobs,
      watcher: finalSnapshot.watcher,
      economics: finalSnapshot.economics,
      withdrawalReservePayout: finalSnapshot.withdrawalReservePayout,
      forcedClassifications: finalSnapshot.forcedClassifications,
    },
    {
      stateQueue: nodeDatabaseExport.stateQueue,
      jobs: nodeDatabaseExport.jobs,
      watcher: nodeDatabaseExport.watcher,
      economics: nodeDatabaseExport.economics,
      withdrawalReservePayout: nodeDatabaseExport.withdrawalReservePayout,
      forcedClassifications: nodeDatabaseExport.forcedClassifications,
    },
    "final snapshot/raw node database state",
  );
  const finalSnapshotDigest = jsonDigest(finalSnapshotValue);
  const transactionAuthorityInput = (txHash: string) => {
    const derived = derivedL1Observations.find(
      (entry) => entry.observation.txHash === txHash,
    );
    if (derived === undefined) {
      throw new Error(`missing derived L1 authority input for ${txHash}`);
    }
    return {
      kupoOutputIndex: derived.kupoOutputIndex,
      includedAt: derived.observation.includedAt,
    };
  };
  await authority.authenticateFinalState({
    manifestId: manifest.manifestId,
    observedAt: derivedFinalObservedAt,
    stateQueueDepth: nodeDatabaseExport.stateQueue.depth,
    unfinishedMutationJobs: nodeDatabaseExport.jobs.unfinishedMutationJobs,
    pendingFinalizations: nodeDatabaseExport.jobs.pendingFinalizations,
    retainedProofTokens: expectedRetainedProofTokens,
    economics: finalSnapshot.economics.map((entry) => ({
      familyId: entry.familyId,
      removalTxHash: entry.removalTxHash,
      ...transactionAuthorityInput(entry.removalTxHash),
      referencedProofTokenOutRef: entry.removalReferencedProofTokenOutRef,
      operatorCredential: entry.operatorCredential,
      proverCredential: entry.proverCredential,
      operatorBondInputOutRef: entry.operatorBondInputOutRef,
      operatorBondInputLovelace: entry.operatorBondInputLovelace,
      proverRewardOutputOutRef: entry.proverRewardOutputOutRef,
      removalFeeLovelace: entry.removalFeeLovelace,
      slashedLovelace: entry.slashedLovelace,
      proverRewardLovelace: entry.proverRewardLovelace,
    })),
    withdrawalReservePayout: {
      payoutConcludeTxHash:
        finalSnapshot.withdrawalReservePayout.payoutConcludeTxHash,
      ...transactionAuthorityInput(
        finalSnapshot.withdrawalReservePayout.payoutConcludeTxHash,
      ),
      destination: finalSnapshot.withdrawalReservePayout.destination,
      payoutValueSha256:
        finalSnapshot.withdrawalReservePayout.payoutValueSha256,
      reserveValueSha256:
        finalSnapshot.withdrawalReservePayout.reserveValueSha256,
    },
    snapshotDigest: finalSnapshotDigest,
    rawSourceDigests: {
      kupoStateQueueResponseSha256:
        finalAuthentication.kupoStateQueueResponseSha256,
      kupoProofTokenResponseSha256s:
        finalAuthentication.kupoProofTokenResponses.map(
          (response) => response.responseSha256,
        ),
      ogmiosTipResponseSha256: finalAuthentication.ogmiosTipResponseSha256,
      nodeDatabaseExportSha256: finalAuthentication.nodeDatabaseExportSha256,
    },
  });
  if (
    finalSnapshot.stateQueue.depth !== 0 ||
    finalSnapshot.stateQueue.fraudulentHeaderHashes.length !== 0 ||
    finalSnapshot.jobs.unfinishedMutationJobs !== 0 ||
    finalSnapshot.jobs.pendingFinalizations !== 0
  ) {
    throw new Error("final chain/queue state is not drained and corrected");
  }
  assertEqual(
    finalSnapshot.economics.map((entry) => entry.familyId),
    claim.families.map((family) => family.familyId),
    "final economics family order",
  );
  for (const [index, observed] of finalSnapshot.economics.entries()) {
    const family = claim.families[index]!;
    const terminal = terminals.get(family.familyId)!;
    assertEqual(
      observed.removalTxHash,
      family.removalTxHash,
      `${family.familyId} economic removal`,
    );
    assertEqual(
      observed.proofTokenUnit,
      terminal.proofToken.unit,
      `${family.familyId} retained proof-token unit`,
    );
    assertEqual(
      observed.proofTokenOutRef,
      terminal.proofToken.outRef,
      `${family.familyId} retained proof-token outref`,
    );
    assertEqual(
      observed.removalReferencedProofTokenOutRef,
      terminal.correction.referencedProofTokenOutRef,
      `${family.familyId} snapshot removal proof-token reference`,
    );
    assertEqual(
      observed.proofTokenFinalState,
      "retained",
      `${family.familyId} snapshot proof-token final state`,
    );
    assertEqual(
      observed.operatorCredential,
      terminal.economics.operatorCredential,
      `${family.familyId} operator credential`,
    );
    assertEqual(
      observed.proverCredential,
      terminal.economics.proverCredential,
      `${family.familyId} prover credential`,
    );
    assertEqual(
      observed.operatorBondInputOutRef,
      terminal.economics.operatorBondInputOutRef,
      `${family.familyId} operator bond input outref`,
    );
    assertEqual(
      observed.operatorBondInputLovelace,
      terminal.economics.operatorBondInputLovelace,
      `${family.familyId} operator bond input lovelace`,
    );
    assertEqual(
      observed.proverRewardOutputOutRef,
      terminal.economics.proverRewardOutputOutRef,
      `${family.familyId} prover reward output outref`,
    );
    assertEqual(
      observed.removalFeeLovelace,
      terminal.economics.removalFeeLovelace,
      `${family.familyId} removal fee`,
    );
    assertEqual(
      observed.slashedLovelace,
      family.expectedSlashLovelace,
      `${family.familyId} snapshot slash`,
    );
    assertEqual(
      observed.proverRewardLovelace,
      family.expectedProverRewardLovelace,
      `${family.familyId} snapshot reward`,
    );
    if (observed.duplicateRewardCount !== 0) {
      throw new Error(
        `${family.familyId} final snapshot observed a duplicate reward`,
      );
    }
  }
  const withdrawalClaim = claim.withdrawalReservePayout;
  assertEqual(
    finalSnapshot.withdrawalReservePayout,
    {
      withdrawalOrderTxHash: withdrawalClaim.withdrawalOrderTxHash,
      reserveTxHash: withdrawalClaim.reserveTxHash,
      payoutInitTxHash: withdrawalClaim.payoutInitTxHash,
      payoutAddTxHashes: withdrawalClaim.payoutAddTxHashes,
      payoutConcludeTxHash: withdrawalClaim.payoutConcludeTxHash,
      destination: withdrawalClaim.expectedDestination,
      payoutValueSha256: withdrawalClaim.expectedPayoutValueSha256,
      reserveValueSha256: withdrawalClaim.expectedReserveValueSha256,
      status: "paid",
    },
    "final withdrawal/reserve/payout state",
  );
  assertEqual(
    finalSnapshot.forcedClassifications,
    claim.forcedClassifications.map((drill) => ({
      direction: drill.direction,
      evidenceTxHash: drill.evidenceTxHash,
      correctionTxHash: drill.correctionTxHash,
      canonicalClassification: drill.canonicalClassification,
      finalClassification: drill.finalClassification,
    })),
    "forced classification final state",
  );
  assertEqual(
    claim.finalState.finalStateSha256,
    finalSnapshotDigest,
    "final snapshot digest",
  );

  const identityDetails = {
    runId: claim.runId,
    manifestId: manifest.manifestId,
    blueprintSha256,
    catalogueRoot: catalogue.root,
    parametersSha256,
  };
  const satisfied = (
    label: string,
    details: Readonly<Record<string, string>>,
  ): DbEvidence => ({
    label,
    status: "satisfied",
    source: "independent-state-correction-reconciliation-v1",
    details: { ...identityDetails, ...details },
  });
  const transactions = required.map((entry) => ({
    label: entry.label,
    txHash: entry.txHash,
    status: "confirmed" as const,
    source: `authenticated-l1:${observationByTxHash.get(entry.txHash)!.path}`,
  }));
  const rawEvidence: RawEvidenceRef[] = [
    {
      label: "state-correction-deployment-manifest",
      path: paths.deploymentManifestPath,
    },
    { label: "state-correction-blueprint", path: paths.blueprintPath },
    { label: "state-correction-catalogue", path: paths.cataloguePath },
    { label: "state-correction-parameters", path: paths.parametersPath },
    ...workflows.flatMap((workflow) => [
      {
        label: `workflow-journal:${workflow.entries[0]!.identity.category}`,
        path: workflow.directory,
      },
      ...workflow.entryPaths.map((path, index) => ({
        label: `workflow-journal-entry:${workflow.entries[0]!.identity.category}:${index.toString()}`,
        path,
      })),
    ]),
    ...paths.l1ObservationPaths.map((path, index) => ({
      label: `authenticated-l1-observation:${index.toString()}`,
      path,
    })),
    ...derivedL1Observations.flatMap((derived, observationIndex) =>
      derived.rawPaths.map((path, rawIndex) => ({
        label: `authenticated-l1-raw:${observationIndex.toString()}:${rawIndex.toString()}`,
        path,
      })),
    ),
    ...paths.recoveryObservationPaths.map((path, index) => ({
      label: `state-correction-recovery:${claim.recoveryDrills[index]!.id}`,
      path,
    })),
    { label: "state-correction-final-snapshot", path: paths.finalSnapshotPath },
    {
      label: "state-correction-final-kupo-state-queue-raw",
      path: rawStateQueue.path,
    },
    ...rawProofTokens.map((raw, index) => ({
      label: `state-correction-final-kupo-proof-token-raw:${index.toString()}`,
      path: raw.path,
    })),
    {
      label: "state-correction-final-ogmios-tip-raw",
      path: rawFinalOgmiosTip.path,
    },
    {
      label: "state-correction-final-node-database-export-raw",
      path: rawNodeDatabaseExport.path,
    },
  ];
  return {
    db: [
      satisfied(REQUIRED_STATE_CORRECTION_GATE_LABELS[0], {
        familyCount: claim.families.length.toString(),
        workflowJournalDigests: workflows
          .map((workflow) => workflow.digest)
          .join(","),
        authenticatedL1TransactionCount: requiredHashes.size.toString(),
      }),
      satisfied(REQUIRED_STATE_CORRECTION_GATE_LABELS[1], {
        reconciledFamilies: claim.families.length.toString(),
        duplicateRewards: "0",
      }),
      satisfied(REQUIRED_STATE_CORRECTION_GATE_LABELS[2], {
        destination: finalSnapshot.withdrawalReservePayout.destination,
        payoutValueSha256:
          finalSnapshot.withdrawalReservePayout.payoutValueSha256,
        reserveValueSha256:
          finalSnapshot.withdrawalReservePayout.reserveValueSha256,
      }),
      satisfied(REQUIRED_STATE_CORRECTION_GATE_LABELS[3], {
        directions: finalSnapshot.forcedClassifications
          .map((drill) => drill.direction)
          .join(","),
      }),
      satisfied(REQUIRED_STATE_CORRECTION_GATE_LABELS[4], {
        recoveredCases: recoveryObservations.map((drill) => drill.id).join(","),
      }),
      satisfied(REQUIRED_STATE_CORRECTION_GATE_LABELS[5], {
        observedAtSlot: finalSnapshot.observedAt.slot,
        observedAtBlockHash: finalSnapshot.observedAt.blockHash,
        confirmationDepth:
          finalSnapshot.observedAt.confirmationDepth.toString(),
        finalSnapshotSha256: finalSnapshotDigest,
      }),
    ],
    transactions,
    rawEvidence,
    notes: [
      `State-correction acceptance independently reconciled ${claim.families.length.toString()} family journals, ${requiredHashes.size.toString()} authenticated L1 transactions, and ${recoveryObservations.length.toString()} recovery observations at ${finalSnapshot.observedAt.slot}:${finalSnapshot.observedAt.blockHash}.`,
    ],
  };
};
