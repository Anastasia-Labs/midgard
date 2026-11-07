import assert from "node:assert/strict";
import { join } from "node:path";

import { verifyFinalizedDeploymentManifest } from "@al-ft/midgard-core/deployment-manifest-identity";
import {
  computeFraudProofReleaseFinalityPolicyDigest,
  type FraudProofWorkflowJournalEntry,
  type FraudProofWorkflowTerminal,
  validateFraudProofWorkflowJournal,
} from "@al-ft/midgard-fault-proofs";
import * as SDK from "@al-ft/midgard-sdk";
import {
  credentialToAddress,
  Data,
  paymentCredentialOf,
  toUnit,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";

import { readJourneyArtifact } from "./artifacts.js";
import type { JourneyFinalizedEvidenceStamp } from "./correction.finalize-pending-journey-evidence.js";
import type {
  JourneyBlock,
  JourneyCategory,
  JourneySuccessor,
} from "./fixture.js";
import {
  canonicalDigest,
  inputLabels,
  type JourneyEvidenceDeployment,
  type JourneyResult,
  outputsOf,
  readDecisions,
  readJourneyCanonicalTransactions,
  readJourneyNativeEvidencePath,
  sha256,
} from "./readiness-evidence.read-journey-canonical-transactions.js";
import {
  readJourneySuccessorPredecessor,
  verifyJourneyVerifiedHeaders,
} from "./verified-headers.js";

/**
 * A result label is a claim. Live completion requires its manifest, validated
 * journal, native transactions and healthy-successor decision to agree.
 */
export const verifyJourneyResultEvidence = async (
  runDirectory: string,
  directory: string,
  category: JourneyCategory,
  deployment: JourneyEvidenceDeployment,
) => {
  verifyFinalizedDeploymentManifest(deployment.manifest);
  const result = await readJourneyArtifact<JourneyResult>(
    join(directory, "result.json"),
  );
  const fingerprint = deployment.manifest.manifestId;
  assert.equal(result.status, "passed", "Journey result is not passing");
  assert.equal(result.category, category, "Result names another family");
  assert.equal(
    result.deploymentFingerprint,
    fingerprint,
    "Result belongs to another deployment",
  );
  const staged = await readJourneyArtifact<{
    predecessor: JourneyBlock;
    current: JourneyBlock;
  }>(join(directory, "staged.json"));
  const successor = await readJourneyArtifact<JourneySuccessor>(
    join(directory, "successor.json"),
  );
  for (const block of [staged.predecessor, staged.current, successor])
    assert.equal(
      await Effect.runPromise(SDK.hashBlockHeader(block.header)),
      block.headerHash,
      "Retained header hash changed",
    );
  assert.equal(result.successor, successor.headerHash);
  const successorPredecessor = await readJourneySuccessorPredecessor(
    directory,
    staged.predecessor,
  );
  assert.equal(
    await Effect.runPromise(SDK.hashBlockHeader(successorPredecessor.header)),
    successorPredecessor.headerHash,
  );
  assert.equal(
    successor.header.prevHeaderHash,
    successorPredecessor.headerHash,
  );
  const saved = await readJourneyArtifact<FraudProofWorkflowJournalEntry[]>(
    join(directory, "completed-workflow.json"),
  );
  assert(saved.length > 0, "Workflow journal is empty");
  let entries = validateFraudProofWorkflowJournal({
    workflowId: saved[0]!.workflowId,
    entries: saved,
  });
  let nativeEvidencePath = await readJourneyNativeEvidencePath(directory);
  let anchoredTerminal: FraudProofWorkflowTerminal | undefined;
  if (result.executionPolicy === "authenticated-inclusion") {
    const provisional = [...entries]
      .reverse()
      .find(
        ({ event }) =>
          event.kind === "terminal_included" || event.kind === "completed",
      )?.event;
    assert(
      provisional?.kind === "terminal_included" ||
        provisional?.kind === "completed",
    );
    assert.equal(
      canonicalDigest(result.completion),
      canonicalDigest(provisional.terminal),
    );
    const stamp = await readJourneyArtifact<JourneyFinalizedEvidenceStamp>(
      join(directory, "finalized-evidence-stamp.json"),
    );
    assert.equal(stamp.category, category);
    assert.equal(stamp.headerHash, staged.current.headerHash);
    assert.equal(stamp.deploymentFingerprint, fingerprint);
    assert.equal(
      stamp.finalityDepth,
      deployment.manifest.l1Finality.confirmationDepth,
    );
    assert.equal(
      stamp.releaseFinalityPolicyDigest,
      computeFraudProofReleaseFinalityPolicyDigest(
        deployment.manifest.l1Finality,
      ),
    );
    entries = validateFraudProofWorkflowJournal({
      workflowId: saved[0]!.workflowId,
      entries: await readJourneyArtifact<FraudProofWorkflowJournalEntry[]>(
        join(directory, "finalized-workflow.json"),
      ),
    });
    assert.deepEqual(
      entries.slice(0, saved.length),
      saved,
      "Final journal changed the provisional history",
    );
    // An included anchor's release depth is re-checked below against the
    // native capture; older stamps predate the kind and anchor `completed`.
    const terminalKind = stamp.terminalKind ?? "completed";
    const anchor =
      terminalKind === "completed"
        ? entries.at(-1)!.event
        : [...entries]
            .reverse()
            .find(({ event }) => event.kind === "terminal_included")?.event;
    assert(
      anchor?.kind === terminalKind,
      "Finalized journal lacks the stamp's anchored terminal",
    );
    if (terminalKind === "terminal_included")
      assert(
        entries.every(({ event }) => event.kind !== "completed"),
        "Included anchor ignored a completed terminal",
      );
    assert.equal(
      canonicalDigest(stamp.terminal),
      canonicalDigest(anchor.terminal),
    );
    anchoredTerminal = anchor.terminal;
    nativeEvidencePath = stamp.nativeEvidencePath;
  }
  assert.equal(entries[0]!.identity.deploymentFingerprint, fingerprint);
  assert.equal(entries[0]!.identity.category, category);
  assert.deepEqual(entries[0]!.identity.target, {
    kind: "state_queue_header",
    headerHash: staged.current.headerHash,
  });
  const completedTerminal = () => {
    const terminal = entries.at(-1)!.event;
    assert.equal(terminal.kind, "completed", "Workflow has not completed");
    if (terminal.kind !== "completed")
      throw new Error("Workflow has not completed");
    assert.equal(
      canonicalDigest(result.completion),
      canonicalDigest(terminal.terminal),
      "Result changed the validated terminal",
    );
    return terminal.terminal;
  };
  const completion = anchoredTerminal ?? completedTerminal();
  const decisions = await readDecisions(runDirectory, fingerprint);
  const fault = decisions.find(
    (decision) => decision.headerHash === staged.current.headerHash,
  );
  assert(
    fault?.decision === "fault_detected" && fault.category === category,
    "Intended automatic fault decision is missing",
  );
  assert.equal(entries[0]!.identity.decisionDigest, fault.decisionDigest);
  // Healthy decisions are not journaled; the watcher's verified diagnostics are.
  await verifyJourneyVerifiedHeaders(directory, [
    staged.predecessor,
    successorPredecessor,
    successor,
  ]);
  assert.equal(
    fault.payloadEnvelopeSha256,
    sha256(staged.current.payloadEnvelopeCbor),
  );
  const canonical = await readJourneyCanonicalTransactions(nativeEvidencePath);
  const transaction = (hash: string) => {
    const found = canonical.transactions.get(hash);
    assert(
      found !== undefined,
      `Transaction ${hash} is absent from canonical native evidence`,
    );
    return found;
  };
  assert.equal(
    deployment.initialization.txHash,
    deployment.manifest.steps.initProtocol.txHash,
  );
  transaction(deployment.initialization.txHash);
  for (const { event } of entries)
    if (event.kind === "confirmed") transaction(event.txHash);
  // The release depth (a stamp's finalityDepth) authenticates the anchor.
  const confirmationDepth = BigInt(
    deployment.manifest.l1Finality.confirmationDepth,
  );
  for (const hash of [
    completion.proofToken.createdByTxHash,
    completion.correction.removalTxHash,
    successor.commitTxHash,
  ])
    assert(
      canonical.tip.blockNo - transaction(hash).blockNo + 1n >=
        confirmationDepth,
      "Native evidence has insufficient finality depth",
    );
  const point = canonical.blocks.get(completion.observedAt.blockHash);
  assert(
    point !== undefined && point.slot === BigInt(completion.observedAt.slot),
    "Terminal confirmation point is not canonical",
  );
  const spent = new Set(
    [...canonical.transactions.values()].flatMap(({ transaction }) =>
      inputLabels(transaction.body().inputs()),
    ),
  );
  const unspent = [...canonical.transactions.values()]
    .flatMap(({ transaction }) => outputsOf(transaction))
    .filter((utxo) => !spent.has(`${utxo.txHash}#${utxo.outputIndex}`));
  const output = (outRef: string | null) => {
    assert(outRef !== null, "Economic output reference is missing");
    const [hash, index] = outRef.split("#");
    const found = outputsOf(transaction(hash!).transaction)[Number(index)];
    assert(found !== undefined, "Native output reference does not exist");
    return found;
  };
  const { contracts, manifest } = deployment;
  for (const [name, policy] of [
    ["fraudProofMint", contracts.fraudProof.policyId],
    ["stateQueueMint", contracts.stateQueue.policyId],
    ["schedulerMint", contracts.scheduler.policyId],
  ] as const)
    assert.equal(
      manifest.contracts[name].scriptHash,
      policy,
      "Stored contract differs from verified manifest",
    );
  const unit = toUnit(
    contracts.fraudProof.policyId,
    SDK.FRAUD_PROOF_CATALOGUE_CATEGORY_IDS[category] +
      staged.current.headerHash,
  );
  assert.equal(completion.proofToken.unit, unit);
  const proofs = unspent.filter((utxo) => utxo.assets[unit] === 1n);
  assert.equal(
    proofs.length,
    1,
    "Permanent proof token is not uniquely retained",
  );
  assert.equal(
    `${proofs[0]!.txHash}#${proofs[0]!.outputIndex}`,
    completion.proofToken.outRef,
  );
  assert.equal(proofs[0]!.address, contracts.fraudProof.spendingScriptAddress);
  const prover = paymentCredentialOf(
    manifest.referenceScriptDeployAddress,
  ).hash;
  assert.deepEqual(Data.from(proofs[0]!.datum!, SDK.FraudProofTokenDatum), {
    fraud_prover: prover,
  });
  const headerUnit = (hash: string) =>
    toUnit(
      contracts.stateQueue.policyId,
      SDK.STATE_QUEUE_NODE_ASSET_NAME_PREFIX + hash,
    );
  assert.equal(
    unspent.filter(
      (utxo) => utxo.assets[headerUnit(staged.current.headerHash)] === 1n,
    ).length,
    0,
  );
  assert.equal(
    unspent.filter(
      (utxo) => utxo.assets[headerUnit(successor.headerHash)] === 1n,
    ).length,
    1,
  );
  const removal = transaction(completion.correction.removalTxHash).transaction;
  assert(
    inputLabels(removal.body().inputs()).includes(
      completion.correction.removedStateQueueOutRef,
    ),
  );
  assert.equal(
    output(completion.correction.removedStateQueueOutRef).assets[
      headerUnit(staged.current.headerHash)
    ],
    1n,
  );
  const referenceInputs = removal.body().reference_inputs();
  assert(
    referenceInputs !== undefined &&
      inputLabels(referenceInputs).includes(completion.proofToken.outRef),
  );
  const tails = outputsOf(removal).filter(
    (utxo) => utxo.assets[headerUnit(staged.predecessor.headerHash)] === 1n,
  );
  assert.equal(tails.length, 1);
  assert.equal(
    (await Effect.runPromise(SDK.getLinkedListNodeViewFromUTxO(tails[0]!)))
      .next,
    "Empty",
  );
  const bond = output(completion.economics.operatorBondInputOutRef);
  const reward = output(completion.economics.proverRewardOutputOutRef);
  assert.equal(
    bond.assets.lovelace,
    BigInt(manifest.economics.requiredBondLovelace),
  );
  assert.equal(
    reward.assets.lovelace,
    BigInt(manifest.economics.fraudProverRewardLovelace),
  );
  const slashing = transaction(reward.txHash).transaction;
  assert.equal(
    slashing.body().fee(),
    BigInt(manifest.economics.slashingPenaltyLovelace),
  );
  assert(
    inputLabels(slashing.body().inputs()).includes(
      completion.economics.operatorBondInputOutRef!,
    ),
  );
  assert.equal(
    reward.address,
    credentialToAddress("Preprod", { type: "Key", hash: prover }),
  );
  assert.equal(result.diagnostics.status.readiness, "ready");
  assert.equal(result.diagnostics.status.liveness, "live");
  assert.deepEqual(result.diagnostics.status.activeAlerts, []);
  assert.deepEqual(result.diagnostics.status.readinessReasons, []);
  assert.equal(result.diagnostics.status.launchScope.complete, true);
  return {
    workflowId: entries[0]!.workflowId,
    headerHash: staged.current.headerHash,
    successor: successor.headerHash,
  };
};
