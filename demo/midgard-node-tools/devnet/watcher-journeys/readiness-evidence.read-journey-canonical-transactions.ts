import assert from "node:assert/strict";
import { createHash } from "node:crypto";
import { createReadStream, existsSync } from "node:fs";
import { readdir, readFile } from "node:fs/promises";
import { isAbsolute, join, normalize } from "node:path";
import { createInterface } from "node:readline";

import { canonicalJson } from "@al-ft/midgard-core/canonical-json";
import { type FraudProofWorkflowTerminal } from "@al-ft/midgard-fault-proofs";
import { CML, coreToUtxo } from "@lucid-evolution/lucid";
import {
  admitWatcherNativeRollForwardBlock,
  parseWatcherNativeChainSyncEvent,
  WATCHER_FAULT_DECISION_RECORD_SCHEMA_VERSION,
  WATCHER_INSTALLED_WORKFLOW_CATEGORIES,
  type WatcherPersistedFaultDecisionRecord,
} from "midgard-watcher";

import { readJourneyArtifact } from "./artifacts.js";
import type { JourneyCategory, JourneyContext } from "./fixture.js";

export type JourneyEvidenceDeployment = Pick<
  JourneyContext["deployment"],
  "manifest" | "initialization" | "contracts"
>;

export type JourneyResult = {
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

export const sha256 = (bytes: string | Uint8Array) =>
  createHash("sha256").update(bytes).digest("hex");

export const canonicalDigest = (value: unknown) =>
  sha256(canonicalJson(value, "journey evidence"));

export const inputLabels = (inputs: CML.TransactionInputList) =>
  Array.from({ length: inputs.len() }, (_, index) => {
    const input = inputs.get(index);
    return `${input.transaction_id().to_hex()}#${input.index()}`;
  });

export const outputsOf = (transaction: CML.Transaction) => {
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
export const readDecisions = async (
  runDirectory: string,
  fingerprint: string,
) => {
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
