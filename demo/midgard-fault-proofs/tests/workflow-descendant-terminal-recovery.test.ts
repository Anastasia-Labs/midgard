import {
  castConfirmedStateToData,
  encodeLinkedListNodeView,
  makeGenesisConfirmedState,
} from "@al-ft/midgard-sdk/core";
import { STATE_QUEUE_ROOT_ASSET_NAME } from "@al-ft/midgard-sdk/state-queue";
import { CML, toUnit } from "@lucid-evolution/lucid";
import { expect, it, vi } from "vitest";

import { CURSOR_FAMILY_TRANSACTION_PORT } from "../src/workflow/cursor-family-adapter.admit-reference-scripts.js";
import { createCursorFamilyWorkflowAdapter } from "../src/workflow/cursor-family-adapter.create-cursor-family-workflow-adapter.js";
import {
  createFraudProofFamilyAuthenticatedL1TerminalVerifier,
  createFraudProofFamilyRawL1ObservationPort,
} from "../src/workflow/family-l1-observation.js";
import { createFraudProofWorkflowRegistry } from "../src/workflow/orchestrator.immutable-fraud-proof-workflow-registry.js";
import { runAdmittedFraudProofWorkflow } from "../src/workflow/orchestrator.run-admitted-fraud-proof-workflow.js";
import {
  FRAUD_PROOF_RAW_L1_SNAPSHOT_AUTHORITY,
  type FraudProofRawL1Snapshot,
} from "../src/workflow/raw-l1-snapshot.compute-fraud-proof-raw-l1-snapshot-evidence-digest.js";
import { signed, transaction } from "./cursor-family-adapter.terminal.js";
import { fixture } from "./support/raw-l1-terminal-fixture.fixture.js";
import {
  hash32,
  output,
  raw,
  releaseFinality,
} from "./support/raw-l1-terminal-fixture.output.js";
import {
  createTestWorkflowProcess,
  type TestWorkflowProcess,
} from "./support/workflow-process.js";

it("completes on another caller's correction over our in-flight descendant, and again after each restart, sending nothing more", async () => {
  // The terminal sits beyond the recovery horizon (k + 2), where the
  // workflow may complete.
  const value = await fixture({
    descendant: true,
    proofCreation: true,
    confirmationDepth: 2162,
  });
  const [peel, cleanup] = value.snapshot.transactions;
  if (peel === undefined || cleanup === undefined)
    throw new Error("missing removals");
  const target = peel.resolvedInputs[0]!;
  const bond = peel.resolvedInputs[1]!;
  const child = peel.resolvedInputs[2]!;
  const proof = peel.resolvedReferenceInputs[0]!;
  const root = raw(
    `${hash32("80")}#0`,
    output({
      address: value.definition.stateQueue.address,
      assets: {
        lovelace: 3_000_000n,
        [toUnit(
          value.definition.stateQueue.policyId,
          STATE_QUEUE_ROOT_ASSET_NAME,
        )]: 1n,
      },
      datum: encodeLinkedListNodeView({
        key: "Empty",
        next: { Key: { key: value.definition.headerHash } },
        data: castConfirmedStateToData(makeGenesisConfirmedState(0n)) as never,
      }),
    }),
  );
  const snapshotBefore: FraudProofRawL1Snapshot = {
    ...value.snapshot,
    transactions: value.snapshot.transactions.slice(2),
    history: value.snapshot.history.map((history) => ({
      ...history,
      transactionHashes: history.transactionHashes.filter(
        (txHash) => txHash !== peel.txHash && txHash !== cleanup.txHash,
      ),
    })),
    scopes: value.snapshot.scopes.map((scope) => ({
      ...scope,
      utxos:
        scope.role === "state_queue"
          ? [root, target, child]
          : scope.role === "active_operator_directory"
            ? [bond]
            : scope.utxos,
    })),
  };
  let snapshot = snapshotBefore;
  const l1 = createFraudProofFamilyRawL1ObservationPort({
    authority: {
      authorityVersion: FRAUD_PROOF_RAW_L1_SNAPSHOT_AUTHORITY,
      capture: async () => ({ snapshot: snapshot, pending: [] }),
    },
    ...value.binding,
    definition: { ...value.definition, category: "doubleSpend" },
  });
  const submit = vi.fn(async () => peel.txHash);
  const capture = vi.fn(async () => ({
    transaction: transaction({
      txHash: peel.txHash,
      referenceScripts: [
        { role: "removal", outRef: proof.outRef, scriptHash: "66".repeat(28) },
      ],
      signed: {
        ...signed({
          bodyHash: peel.txHash,
          includedReferenceOutRef: proof.outRef,
        }),
        toTransaction: () =>
          CML.Transaction.new(
            CML.TransactionBody.from_cbor_hex(peel.bodyCbor),
            CML.TransactionWitnessSet.new(),
            true,
          ),
        toCBOR: () =>
          CML.Transaction.new(
            CML.TransactionBody.from_cbor_hex(peel.bodyCbor),
            CML.TransactionWitnessSet.new(),
            true,
          ).to_cbor_hex(),
        submit,
      },
    }),
    mutationLease: lease,
  }));
  const lease = {
    token: "descendant-peel",
    source: "state-queue-observer-v1",
    renew: vi.fn(async () => undefined),
    release: vi.fn(async () => undefined),
    fail: vi.fn(async () => undefined),
  };
  const spec = {
    category: "doubleSpend",
    stepCount: 4,
    successors: {
      1: [2],
      2: [3],
      3: [4],
      4: ["proof_token"],
    },
  } as const;
  const adapter = () =>
    createCursorFamilyWorkflowAdapter({
      spec,
      l1,
      transactions: {
        portVersion: CURSOR_FAMILY_TRANSACTION_PORT,
        category: "doubleSpend",
        prepare: async () => ({}),
        capture,
      },
    });
  const process = createTestWorkflowProcess();
  const objective = {
    category: "doubleSpend" as const,
    headerHash: value.definition.headerHash,
  };
  const run = async (
    maxActions = 64,
    workflowProcess: TestWorkflowProcess = process,
  ) =>
    runAdmittedFraudProofWorkflow({
      hubOraclePolicyId: value.binding.hubOraclePolicyId,
      category: "doubleSpend",
      headerHash: value.definition.headerHash,
      prepareFamilyArtifact: async () => ({}),
      registry: createFraudProofWorkflowRegistry({
        adapters: [workflowProcess.recording(adapter())],
        launchScope: ["doubleSpend"],
      }),
      run: workflowProcess.run(objective),
      terminalVerifier:
        createFraudProofFamilyAuthenticatedL1TerminalVerifier(l1),
      releaseFinality,
      maxActions,
    });
  // Our peel is sent and in flight.
  expect((await run(1)).kind).toBe("pending");
  expect(process.sent().map(({ txHash }) => txHash)).toEqual([peel.txHash]);
  snapshot = value.snapshot;
  const removal = {
    inputOutRef: child.outRef,
    targetOutRef: target.outRef,
    proofOutRef: proof.outRef,
  };
  expect(
    await l1.transactionConfirmed({
      headerHash: value.definition.headerHash,
      txHash: peel.txHash,
      removal,
    }),
  ).toBe(true);
  for (const changed of [
    { txHash: hash32("81"), removal },
    {
      txHash: peel.txHash,
      removal: { ...removal, inputOutRef: `${hash32("82")}#0` },
    },
    {
      txHash: peel.txHash,
      removal: { ...removal, targetOutRef: `${hash32("83")}#0` },
    },
    {
      txHash: peel.txHash,
      removal: { ...removal, proofOutRef: `${hash32("84")}#0` },
    },
  ]) {
    expect(
      await l1.transactionConfirmed({
        headerHash: value.definition.headerHash,
        ...changed,
      }),
    ).toBe(false);
  }
  // Another caller's cleanup completes the correction. The run completes on
  // it, and so does every restart, none building our peel again.
  for (const workflowProcess of [
    process,
    createTestWorkflowProcess(),
    createTestWorkflowProcess(),
  ]) {
    const result = await run(64, workflowProcess);
    expect(result.kind).toBe("completed");
    if (result.kind !== "completed") throw new Error("not completed");
    expect(result.terminal.correction.removalTxHash).toBe(cleanup.txHash);
    expect(workflowProcess.sent().length).toBe(
      workflowProcess === process ? 1 : 0,
    );
  }
  expect(capture).toHaveBeenCalledOnce();
  expect(submit).toHaveBeenCalledOnce();
  expect(lease.release).toHaveBeenCalledOnce();
  expect(lease.fail).not.toHaveBeenCalled();
});
