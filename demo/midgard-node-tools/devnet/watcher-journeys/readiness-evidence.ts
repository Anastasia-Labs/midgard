import assert from "node:assert/strict";
import { createHash } from "node:crypto";
import { createReadStream, existsSync } from "node:fs";
import { readdir, readFile } from "node:fs/promises";
import { isAbsolute, join, normalize } from "node:path";
import { createInterface } from "node:readline";

import { canonicalJson } from "@al-ft/midgard-core/canonical-json";
import { verifyFinalizedDeploymentManifest } from "@al-ft/midgard-core/deployment-manifest-identity";
import {
  computeFraudProofReleaseFinalityPolicyDigest,
  type FraudProofWorkflowJournalEntry,
  type FraudProofWorkflowTerminal,
  validateFraudProofWorkflowJournal,
} from "@al-ft/midgard-fault-proofs";
import * as SDK from "@al-ft/midgard-sdk";
import {
  CML,
  coreToUtxo,
  credentialToAddress,
  Data,
  paymentCredentialOf,
  toUnit,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";
import {
  admitWatcherNativeRollForwardBlock,
  parseWatcherNativeChainSyncEvent,
  WATCHER_FAULT_DECISION_RECORD_SCHEMA_VERSION,
  WATCHER_INSTALLED_WORKFLOW_CATEGORIES,
  type WatcherPersistedFaultDecisionRecord,
} from "midgard-watcher";

import { readJourneyArtifact } from "./artifacts.js";
import type {
  JourneyBlock,
  JourneyCategory,
  JourneyContext,
  JourneySuccessor,
} from "./fixture.js";

export type JourneyEvidenceDeployment = Pick<
  JourneyContext["deployment"],
  "manifest" | "initialization" | "contracts"
>;
type JourneyResult = {
  status: string;
  executionPolicy?: "authenticated-inclusion";
  nativeEvidencePath?: string;
  category: JourneyCategory;
  deploymentFingerprint: string;
  completion: FraudProofWorkflowTerminal;
  successor: string;
  diagnostics: {
    status: {
      readiness: string;
      liveness: string;
      readinessReasons: unknown[];
      activeAlerts: unknown[];
      launchScope: { complete: boolean };
    };
  };
};

/** A finalized stamp can name a later session than the provisional result. */
export const readJourneyNativeEvidencePath = async (directory: string) => {
  for (const filename of ["finalized-evidence-stamp.json", "result.json"]) {
    const path = join(directory, filename);
    if (!existsSync(path)) continue;
    const record = await readJourneyArtifact<{ nativeEvidencePath?: unknown }>(
      path,
    );
    if (
      filename === "result.json" &&
      !Object.hasOwn(record, "nativeEvidencePath")
    )
      continue;
    const nativeEvidencePath = record.nativeEvidencePath;
    assert(
      typeof nativeEvidencePath === "string" &&
        isAbsolute(nativeEvidencePath) &&
        normalize(nativeEvidencePath) === nativeEvidencePath,
      `${filename} has an invalid native evidence path`,
    );
    return nativeEvidencePath;
  }
  const retained = join(directory, "native-chain.ndjson");
  assert(existsSync(retained), "No retained native evidence path is available");
  return retained;
};

const sha256 = (bytes: string | Uint8Array) =>
  createHash("sha256").update(bytes).digest("hex");
const canonicalDigest = (value: unknown) =>
  sha256(canonicalJson(value, "journey evidence"));
const inputLabels = (inputs: CML.TransactionInputList) =>
  Array.from({ length: inputs.len() }, (_, index) => {
    const input = inputs.get(index);
    return `${input.transaction_id().to_hex()}#${input.index()}`;
  });
const outputsOf = (transaction: CML.Transaction) => {
  const hash = CML.hash_transaction(transaction.body());
  const outputs = transaction.body().outputs();
  return Array.from({ length: outputs.len() }, (_, index) =>
    coreToUtxo(
      CML.TransactionUnspentOutput.new(
        CML.TransactionInput.new(hash, BigInt(index)),
        outputs.get(index),
      ),
    ),
  );
};

/** Re-admit native block bytes and apply recorded rollbacks without starting a node. */
export const readJourneyCanonicalTransactions = async (path: string) => {
  const transactions = new Map<
    string,
    {
      transaction: CML.Transaction;
      blockHash: string;
      blockNo: bigint;
      slot: bigint;
    }
  >();
  const blocks = new Map<string, { blockNo: bigint; slot: bigint }>();
  let tip: { hash: string; blockNo: bigint } | undefined;
  const lines = createInterface({
    input: createReadStream(path),
    crlfDelay: Infinity,
  });
  for await (const line of lines) {
    if (line.length === 0) continue;
    const event = parseWatcherNativeChainSyncEvent(JSON.parse(line));
    if (event.kind === "roll_backward") {
      if (event.point.kind !== "origin") {
        const retained = blocks.get(event.point.blockHash);
        assert(
          retained !== undefined && retained.slot === BigInt(event.point.slot),
          "Native rollback names a point outside the recorded chain",
        );
      }
      for (const [hash, point] of blocks) {
        if (
          event.point.kind === "origin" ||
          point.slot > BigInt(event.point.slot) ||
          (point.slot === BigInt(event.point.slot) &&
            hash !== event.point.blockHash)
        )
          blocks.delete(hash);
      }
      for (const [hash, transaction] of transactions)
        if (!blocks.has(transaction.blockHash)) transactions.delete(hash);
      tip =
        event.point.kind === "origin"
          ? undefined
          : {
              hash: event.point.blockHash,
              blockNo: blocks.get(event.point.blockHash)?.blockNo ?? -1n,
            };
      continue;
    }
    const block = admitWatcherNativeRollForwardBlock(event);
    if (tip !== undefined)
      assert.equal(
        event.prevHash,
        tip.hash,
        "Native capture has a discontinuous parent",
      );
    const point = { blockNo: BigInt(block.blockNo), slot: BigInt(block.slot) };
    if (tip !== undefined)
      assert.equal(
        point.blockNo,
        tip.blockNo + 1n,
        "Native block numbers are discontinuous",
      );
    blocks.set(block.blockHash, point);
    tip = { hash: block.blockHash, blockNo: point.blockNo };
    for (let index = 0; index < block.transactionIds.length; index++) {
      const transaction = CML.Transaction.from_cbor_hex(
        block.transactionCbors[index]!,
      );
      if (transaction.is_valid())
        transactions.set(block.transactionIds[index]!, {
          transaction,
          blockHash: block.blockHash,
          ...point,
        });
    }
  }
  assert(
    tip !== undefined && tip.blockNo >= 0n,
    "No canonical native tip was captured",
  );
  return { transactions, blocks, tip };
};

/** Audit persisted decision bytes; opening the production writer would create files. */
const readDecisions = async (runDirectory: string, fingerprint: string) => {
  const directory = join(
    runDirectory,
    "work/journeys/runtime/workflows/fault-decisions",
  );
  const names = (await readdir(directory)).sort();
  let prior: string | null = null;
  const decisions: WatcherPersistedFaultDecisionRecord["decision"][] = [];
  for (let index = 0; index < names.length; index++) {
    assert.equal(
      names[index],
      `${index.toString().padStart(20, "0")}.json`,
      "Decision journal has a revision gap",
    );
    const bytes = await readFile(join(directory, names[index]!));
    const record: WatcherPersistedFaultDecisionRecord = JSON.parse(
      bytes.toString("utf8"),
    );
    assert.equal(
      record.schemaVersion,
      WATCHER_FAULT_DECISION_RECORD_SCHEMA_VERSION,
    );
    assert.equal(record.revision, index.toString());
    assert.equal(record.priorRecordSha256, prior);
    assert.equal(
      bytes.toString("utf8"),
      `${canonicalJson(record, "decision record")}\n`,
    );
    const { decisionDigest, ...decision } = record.decision;
    assert.equal(
      canonicalDigest(decision),
      decisionDigest,
      "Decision digest differs from its bytes",
    );
    assert.equal(decision.deploymentFingerprint, fingerprint);
    assert.deepEqual(
      [...decision.launchScope].sort(),
      [...WATCHER_INSTALLED_WORKFLOW_CATEGORIES].sort(),
    );
    decisions.push(record.decision);
    prior = sha256(bytes);
  }
  return decisions;
};

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
  const successorPredecessor = existsSync(
    join(directory, "successor-predecessor.json"),
  )
    ? await readJourneyArtifact<JourneySuccessor>(
        join(directory, "successor-predecessor.json"),
      )
    : staged.predecessor;
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
    const stamp = await readJourneyArtifact<{
      category: JourneyCategory;
      headerHash: string;
      deploymentFingerprint: string;
      releaseFinalityPolicyDigest: string;
      finalityDepth: number;
      terminal: FraudProofWorkflowTerminal;
      nativeEvidencePath: string;
    }>(join(directory, "finalized-evidence-stamp.json"));
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
    const completed = entries.at(-1)!.event;
    assert(completed.kind === "completed");
    assert.equal(
      canonicalDigest(stamp.terminal),
      canonicalDigest(completed.terminal),
    );
    nativeEvidencePath = stamp.nativeEvidencePath;
  }
  assert.equal(entries[0]!.identity.deploymentFingerprint, fingerprint);
  assert.equal(entries[0]!.identity.category, category);
  assert.deepEqual(entries[0]!.identity.target, {
    kind: "state_queue_header",
    headerHash: staged.current.headerHash,
  });
  const terminal = entries.at(-1)!.event;
  assert.equal(terminal.kind, "completed", "Workflow has not completed");
  if (terminal.kind !== "completed")
    throw new Error("Workflow has not completed");
  if (result.executionPolicy !== "authenticated-inclusion")
    assert.equal(
      canonicalDigest(result.completion),
      canonicalDigest(terminal.terminal),
      "Result changed the validated terminal",
    );
  const completion = terminal.terminal;
  const decisions = await readDecisions(runDirectory, fingerprint);
  const fault = decisions.find(
    (decision) => decision.headerHash === staged.current.headerHash,
  );
  assert(
    fault?.decision === "fault_detected" && fault.category === category,
    "Intended automatic fault decision is missing",
  );
  assert.equal(entries[0]!.identity.decisionDigest, fault.decisionDigest);
  for (const block of [staged.predecessor, successorPredecessor, successor]) {
    const decision = decisions.find(
      (value) => value.headerHash === block.headerHash,
    );
    assert.equal(
      decision?.decision,
      "healthy",
      "Healthy predecessor/successor decision is missing",
    );
    assert.equal(
      decision?.payloadEnvelopeSha256,
      sha256(block.payloadEnvelopeCbor),
    );
  }
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
