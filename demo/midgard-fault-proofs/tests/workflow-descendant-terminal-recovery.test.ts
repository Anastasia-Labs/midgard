import {
  castConfirmedStateToData,
  encodeLinkedListNodeView,
  makeGenesisConfirmedState,
  STATE_QUEUE_ROOT_ASSET_NAME,
} from "@al-ft/midgard-sdk";
import { CML, toUnit } from "@lucid-evolution/lucid";
import { expect, it, onTestFinished, vi } from "vitest";

import {
  createCursorFamilyWorkflowAdapter,
  CURSOR_FAMILY_TRANSACTION_PORT,
} from "../src/workflow/cursor-family-adapter.js";
import {
  createFraudProofFamilyAuthenticatedL1TerminalVerifier,
  createFraudProofFamilyRawL1ObservationPort,
} from "../src/workflow/family-l1-observation.js";
import {
  DirectoryFraudProofWorkflowJournalStore,
  validateFraudProofWorkflowJournal,
} from "../src/workflow/journal.js";
import { createFraudProofWorkflowRegistry } from "../src/workflow/orchestrator.js";
import { runAdmittedFraudProofWorkflow } from "../src/workflow/orchestrator.run-admitted-fraud-proof-workflow.js";
import {
  FRAUD_PROOF_RAW_L1_SNAPSHOT_AUTHORITY,
  type FraudProofRawL1Snapshot,
} from "../src/workflow/raw-l1-snapshot.js";
import { signed, transaction } from "./cursor-family-adapter.terminal.js";
import {
  fixture,
  hash32,
  output,
  raw,
  releaseFinality,
} from "./support/raw-l1-terminal-fixture.js";
import { makeAdapter } from "./workflow.make-adapter.js";

it("resumes an included descendant intent twice after another caller completes correction", async () => {
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
      capture: async () => snapshot,
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
  const directory = await mkdtemp(
    join(tmpdir(), "workflow-descendant-terminal-"),
  );
  onTestFinished(async () => rm(directory, { recursive: true, force: true }));
  let journal = new DirectoryFraudProofWorkflowJournalStore(directory);
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
      stateQueueMutationLeaseCoordinator: {
        acquire: async () => lease,
        resume: async () => lease,
      },
    });
  const proofHash = proof.outRef.split("#")[0]!;
  const admittedProof = {
    ...makeAdapter({
      submit: async () => ({ kind: "submitted", txHash: proofHash }),
      reconcile: async () => {
        expect(
          await l1.transactionConfirmed({
            headerHash: value.definition.headerHash,
            txHash: proofHash,
          }),
        ).toBe(true);
        return { kind: "confirmed" as const, txHash: proofHash };
      },
    }),
    preflight: async () => ({
      actionId: "prove",
      txHash: proofHash,
      scriptExecution: "reference_scripts" as const,
      localUplcEvaluation: {
        status: "passed" as const,
        evaluator: "proof-admission-fixture",
      },
      referenceScripts: [
        {
          role: "proof",
          outRef: `${hash32("79")}#0`,
          scriptHash: "66".repeat(28),
        },
      ],
    }),
  };
  const run = async (maxActions = 64, seedProof = false) =>
    runAdmittedFraudProofWorkflow({
      deploymentFingerprint: value.binding.deploymentFingerprint,
      category: "doubleSpend",
      headerHash: value.definition.headerHash,
      evidenceBinding: {
        route: "canonical_block",
        headerHash: value.definition.headerHash,
        payloadEnvelopeSha256: hash32("77"),
        payloadSha256: hash32("78"),
        l1BlockHash: snapshot.cursor.point.blockHash,
        l1Slot: snapshot.cursor.point.slot,
      },
      prepareFamilyArtifact: async () => ({}),
      registry: createFraudProofWorkflowRegistry({
        adapters: [seedProof ? admittedProof : adapter()],
        launchScope: ["doubleSpend"],
      }),
      journal,
      terminalVerifier:
        createFraudProofFamilyAuthenticatedL1TerminalVerifier(l1),
      releaseFinality,
      maxActions,
    });
  // Seed the already admitted proof creation through the normal journal grammar.
  expect((await run(2, true)).kind).toBe("pending");
  const initialized = await run(1);
  if (initialized.kind !== "pending")
    throw new Error("expected pending intent");
  const entries = await journal.load(initialized.workflowId);
  expect(entries.at(-1)?.event).toMatchObject({
    kind: "submitted",
    txHash: peel.txHash,
  });
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
  for (let resume = 0; resume < 2; resume++) {
    journal = new DirectoryFraudProofWorkflowJournalStore(directory);
    const result = await run();
    expect(result.kind).toBe("completed");
    if (result.kind !== "completed") throw new Error("not completed");
    expect(result.terminal.correction.removalTxHash).toBe(cleanup.txHash);
    expect(
      result.entries.filter(
        ({ event }) =>
          event.kind === "confirmed" && event.txHash === peel.txHash,
      ),
    ).toHaveLength(1);
    expect(
      result.entries.some(
        ({ event }) =>
          event.kind === "confirmed" && event.txHash === cleanup.txHash,
      ),
    ).toBe(false);
    expect(result.entries.some(({ event }) => event.kind === "stalled")).toBe(
      false,
    );
    validateFraudProofWorkflowJournal({
      workflowId: result.workflowId,
      entries: result.entries,
    });
    // An authenticated later terminal cannot close the journal over T1's
    // unresolved signed intent, even though its proof creation is confirmed.
    const completion = result.entries.at(-1)!;
    expect(() =>
      validateFraudProofWorkflowJournal({
        workflowId: result.workflowId,
        entries: [...entries, { ...completion, sequence: entries.length }],
      }),
    ).toThrow("no unresolved submissions");
  }
  expect(capture).toHaveBeenCalledOnce();
  expect(submit).toHaveBeenCalledOnce();
  expect(lease.release).toHaveBeenCalledOnce();
  expect(lease.fail).not.toHaveBeenCalled();
});
import { mkdtemp, rm } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";
