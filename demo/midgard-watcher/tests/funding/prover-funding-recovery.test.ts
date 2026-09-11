import { createHash } from "node:crypto";
import { mkdir, mkdtemp, readdir, rm, writeFile } from "node:fs/promises";
import { join } from "node:path";

import {
  assertWorkflowJournalActuation,
  authenticatedStateQueueObservationDigest,
  bindWorkflowActuationJournal,
  bindWorkflowActuationRecoveryIdentity,
  bindWorkflowFundingReservationJournal,
  canonicalBlockEvidenceFromVerifiedPayload,
  classifyHeader,
  createDoubleSpendWorkflowRunner,
  createFraudProofWorkflowRegistry,
  createHeaderClassifier,
  createWorkflowActuationPermitController,
  DirectoryFraudProofWorkflowJournalStore,
  DOUBLE_SPEND_COMPLETE_CANONICAL_REPLAY,
  FRAUD_PROOF_WORKFLOW_ADAPTER,
  FRAUD_PROOF_WORKFLOW_JOURNAL_SCHEMA_VERSION,
  FRAUD_PROOF_WORKFLOW_SAFETY,
  FRAUD_PROOF_WORKFLOW_TERMINAL_VERIFIER,
  type FraudProofFamilyWorkflowAdapter,
  type FraudProofWorkflowJournalEvent,
  type FraudProofWorkflowJournalStore,
  type HeaderFaultDecision,
  journalJsonDigest,
  normalizeJournalJson,
  runFraudProofWorkflow,
  workflowActuationDecisionDigest,
} from "@al-ft/midgard-fault-proofs";
import {
  authenticatedHeaderObservation,
  buildCanonicalBlockFixture,
  buildFixtureTransaction,
  outRefCbor,
} from "@al-ft/midgard-fault-proofs/test-support/canonical-block-evidence-fixture";
import { CML, type UTxO } from "@lucid-evolution/lucid";
import { afterEach, describe, expect, it, vi } from "vitest";

import { openWatcherFaultDecisionJournal } from "../../src/fault-proofs/fault-decision-journal.js";
import { unsafeCreateWatcherProtocolParameterRuntimeAuthorityForTest } from "../../src/funding/prover-funding.js";
import { createWatcherProverFundingAuthorityFactory } from "../../src/funding/prover-funding-authority.js";
import { authorizeWatcherProverFundingRecovery } from "../../src/funding/prover-funding-recovery.js";
import {
  parseWatcherProverFundingReservationRecord,
  type WatcherProverFundingReservationPlan,
} from "../../src/funding/prover-funding-reservation.js";
import { openWatcherSqliteProverFundingReservationStore } from "../../src/funding/sqlite-prover-funding-reservation-store.js";
import { watcherDeploymentReleaseFinalityAuthority } from "../../src/runtime/deployment-identity.js";
import { makeWatcherDeploymentAuthorityFixture } from "../support/deployment-authority-fixture.js";

const directories: string[] = [];
const closers: (() => void)[] = [];
afterEach(async () => {
  for (const close of closers.splice(0)) close();
  await Promise.all(
    directories
      .splice(0)
      .map((path) => rm(path, { recursive: true, force: true })),
  );
});
const key = CML.PrivateKey.from_normal_bytes(Buffer.alloc(32, 0x44));
const walletAddress = CML.EnterpriseAddress.new(
  0,
  CML.Credential.new_pub_key(key.to_public().hash()),
)
  .to_address()
  .to_bech32();
const deploymentIdentity = makeWatcherDeploymentAuthorityFixture().result;
const finality = watcherDeploymentReleaseFinalityAuthority(deploymentIdentity);
const sourcesFor = (payloadEnvelopeCbor: Buffer) => [
  {
    sourceId: "libp2p-test",
    fetchPayloadByHeaderHash: async () => ({
      ok: true as const,
      provenance: {
        trustClass: "public_or_permissionless_da" as const,
        sourceId: "libp2p-test/peer-a",
        grade: "security" as const,
      },
      sourceId: "libp2p-test",
      sourcePeerId: "peer-a",
      payloadEnvelopeCbor,
      attempts: [],
    }),
  },
];

const setup = async () => {
  const journalRoot = await mkdtemp(
    join(process.cwd(), ".watcher-funding-recovery-"),
  );
  directories.push(journalRoot);
  const path = join(journalRoot, "watcher.sqlite");
  let database = await openWatcherSqliteProverFundingReservationStore({ path });
  closers.push(() => database.close());
  const sharedInput = outRefCbor(91, 0n);
  const fixture = await buildCanonicalBlockFixture({
    transactions: [
      buildFixtureTransaction({ spendInputs: [sharedInput], fee: 1n }),
      buildFixtureTransaction({ spendInputs: [sharedInput], fee: 2n }),
    ],
  });
  const observation = authenticatedHeaderObservation(fixture);
  const classifier = await createHeaderClassifier({
    deploymentFingerprint: deploymentIdentity.manifestId,
    replayer: DOUBLE_SPEND_COMPLETE_CANONICAL_REPLAY,
    releaseFinalityAuthority: finality,
  });
  const classify = async (confirmationDepth: number) => {
    const observed = { ...observation, confirmationDepth };
    const decision = await classifyHeader({
      classifier,
      observation: observed,
      authenticatedObservationDigest:
        await authenticatedStateQueueObservationDigest({
          observation: observed,
          minimumConfirmationDepth: 30,
        }),
      sources: sourcesFor(fixture.payloadEnvelopeCbor),
    });
    if (decision.decision !== "fault_detected")
      throw new Error("fixture must classify double spend");
    return decision;
  };
  const old = await classify(30);
  const fresh = await classify(31);
  expect(old.decisionDigest).not.toBe(fresh.decisionDigest);
  const decisionJournal = await openWatcherFaultDecisionJournal({
    directory: journalRoot,
    deploymentFingerprint: deploymentIdentity.manifestId,
    launchScope: old.launchScope,
  });
  expect(old.detectionId).toContain("#");
  await decisionJournal.appendLiveDecision(old);
  await decisionJournal.appendLiveDecision(fresh);
  expect(
    (await decisionJournal.readAll()).map(({ decision }) => decision),
  ).toEqual([old, fresh]);
  const evidence = await canonicalBlockEvidenceFromVerifiedPayload({
    observation,
    payloadEnvelopeCbor: fixture.payloadEnvelopeCbor,
    daProvenance: {
      trustClass: "public_or_permissionless_da",
      sourceId: "libp2p-test/peer-a",
      grade: "security",
    },
  });
  const protocolParameters =
    await unsafeCreateWatcherProtocolParameterRuntimeAuthorityForTest({
      deploymentIdentity,
      ogmiosUrl: "http://127.0.0.1:1337",
      timeoutMs: 10_000,
      fetchImpl: async (_url, init) => {
        const request = JSON.parse(String(init?.body)) as { id: string };
        return new Response(
          JSON.stringify({
            jsonrpc: "2.0",
            id: request.id,
            result: {
              minFeeCoefficient: 44,
              minFeeConstant: { ada: { lovelace: 155381 } },
              scriptExecutionPrices: {
                memory: "577/10000",
                cpu: "721/10000000",
              },
              minUtxoDepositCoefficient: 4310,
              collateralPercentage: 150,
              maxCollateralInputs: 3,
              maxTransactionSize: { bytes: 16384 },
              maxValueSize: { bytes: 5000 },
              maxExecutionUnitsPerTransaction: {
                memory: 16_500_000,
                cpu: 10_000_000_000,
              },
              minFeeReferenceScripts: {
                base: 15,
                range: 25_600,
                multiplier: 1.2,
              },
              maxReferenceScriptsSizePerTransaction: { bytes: 204_800 },
            },
          }),
          { status: 200, headers: { "content-type": "application/json" } },
        );
      },
    });
  const runner = createDoubleSpendWorkflowRunner(async () => {
    throw new Error("adapter runner must not build in reconciliation test");
  });
  const walletUtxos: UTxO[] = [0, 1, 2, 3].map((outputIndex) => ({
    txHash: "99".repeat(32),
    outputIndex,
    address: walletAddress,
    assets: { lovelace: 2_000_000_000n },
  }));
  let capturedPlan: WatcherProverFundingReservationPlan | undefined;
  const factory = () =>
    createWatcherProverFundingAuthorityFactory({
      journalRoot,
      deploymentIdentity,
      protocolParameters,
      store: {
        ...database.store,
        reserve: async (plan) => {
          capturedPlan = plan;
          return await database.store.reserve(plan);
        },
      },
    });
  const createPermit = async (
    decision: HeaderFaultDecision,
    generation: string,
    controller = createWorkflowActuationPermitController({
      decision,
      rollbackGeneration: generation,
    }),
  ) => {
    const permit = await factory().create({
      category: "doubleSpend",
      runner,
      actuationPermit: controller.permit,
      rollbackGeneration: generation,
      decisionDigest: decision.decisionDigest,
      walletAddress,
      walletUtxos,
      resolveInputs: async (refs) =>
        walletUtxos.filter((utxo) =>
          refs.includes(`${utxo.txHash}#${utxo.outputIndex}`),
        ),
      resolveProtocolInputAuthority: async () => {
        throw new Error("unexpected protocol input lookup");
      },
    });
    return { permit, controller };
  };
  const original = await createPermit(old, "1");
  if (capturedPlan === undefined)
    throw new Error("missing admitted reservation plan");
  const plan = capturedPlan;
  const journalDirectory = join(
    journalRoot,
    "fault-proofs",
    "doubleSpend",
    old.headerHash,
  );
  const bind = (
    decision: HeaderFaultDecision,
    admitted: Awaited<ReturnType<typeof createPermit>>,
  ) =>
    bindWorkflowFundingReservationJournal({
      journal: bindWorkflowActuationJournal({
        journal: new DirectoryFraudProofWorkflowJournalStore(journalDirectory),
        permit: admitted.controller.permit,
        category: "doubleSpend",
        deploymentFingerprint: deploymentIdentity.manifestId,
        headerHash: decision.headerHash,
        decisionDigest: decision.decisionDigest,
      }),
      permit: admitted.permit,
    });
  const adapter: FraudProofFamilyWorkflowAdapter = {
    adapterVersion: FRAUD_PROOF_WORKFLOW_ADAPTER,
    category: "doubleSpend",
    safety: FRAUD_PROOF_WORKFLOW_SAFETY,
    prepare: vi.fn(async () => ({ headerHash: old.headerHash })),
    observe: vi.fn<FraudProofFamilyWorkflowAdapter["observe"]>(async () => ({
      kind: "pending",
      reason: "next canonical proof step",
    })),
    preflight: vi.fn(async () => {
      throw new Error("unexpected new action");
    }),
    submit: vi.fn(async () => {
      throw new Error("unexpected new submission");
    }),
    reconcile: vi.fn<FraudProofFamilyWorkflowAdapter["reconcile"]>(
      async ({ txHash }) => ({ kind: "confirmed", txHash: txHash! }),
    ),
  };
  const run = (journal: FraudProofWorkflowJournalStore) =>
    runFraudProofWorkflow({
      deploymentFingerprint: deploymentIdentity.manifestId,
      evidence,
      detections: [
        {
          detectionId: old.detectionId,
          headerHash: old.headerHash,
          violationId: old.violationId,
          position: BigInt(old.position),
        },
      ],
      registry: createFraudProofWorkflowRegistry({
        adapters: [adapter],
        launchScope: ["doubleSpend"],
      }),
      journal,
      releaseFinalityAuthority: finality,
      terminalVerifier: {
        verifierVersion: FRAUD_PROOF_WORKFLOW_TERMINAL_VERIFIER,
        verify: async () => {
          throw new Error("unexpected terminal");
        },
      },
    });
  const journal = bind(old, original);
  const initial = await run(journal);
  if (!("workflowId" in initial))
    throw new Error("fixture did not start workflow");
  const funding = plan.inputs.find(({ role }) => role === "funding")!;
  const [fundingHash, fundingIndex] = funding.outRef.split("#");
  const inputs = CML.TransactionInputList.new();
  inputs.add(
    CML.TransactionInput.new(
      CML.TransactionHash.from_hex(fundingHash!),
      BigInt(fundingIndex!),
    ),
  );
  const outputs = CML.TransactionOutputList.new();
  const remaining = BigInt(funding.lovelace) - 1_000_000n;
  outputs.add(
    CML.TransactionOutput.new(
      CML.Address.from_bech32(walletAddress),
      CML.Value.from_coin(remaining),
    ),
  );
  const body = CML.TransactionBody.new(inputs, outputs, 1_000_000n);
  const witnesses = CML.TransactionWitnessSet.new();
  const vkeys = CML.VkeywitnessList.new();
  vkeys.add(
    CML.Vkeywitness.new(
      key.to_public(),
      key.sign(CML.hash_transaction(body).to_raw_bytes()),
    ),
  );
  witnesses.set_vkeywitnesses(vkeys);
  const signedTransactionCborHex = CML.Transaction.new(
    body,
    witnesses,
    true,
    undefined,
  ).to_cbor_hex();
  const transactionHash = CML.hash_transaction(body).to_hex();
  const pending = await database.store.prepareTransition({
    plan,
    expectedRevision: "0",
    actionKind: "proof.init",
    signedTransactionCborHex,
    transactionHash,
    transactionBodySha256: createHash("sha256")
      .update(Buffer.from(body.to_cbor_hex(), "hex"))
      .digest("hex"),
    consumedOutRefs: [funding.outRef],
    producedInputs: [
      {
        outRef: `${transactionHash}#0`,
        role: "funding",
        lovelace: remaining.toString(),
        assets: [],
      },
    ],
  });
  const append = async (event: FraudProofWorkflowJournalEvent) => {
    const entries = await journal.load(initial.workflowId);
    await journal.append(
      {
        schemaVersion: FRAUD_PROOF_WORKFLOW_JOURNAL_SCHEMA_VERSION,
        workflowId: initial.workflowId,
        identity: initial.identity,
        sequence: entries.length,
        recordedAt: new Date().toISOString(),
        event,
      },
      entries.length,
    );
  };
  await append({
    kind: "preflight_passed",
    actionId: "init",
    txHash: transactionHash,
    localEvaluator: "test-uplc",
    referenceScripts: [
      {
        role: "init",
        outRef: `${"88".repeat(32)}#0`,
        scriptHash: "77".repeat(28),
      },
    ],
  });
  await append({
    kind: "submission_intent",
    actionId: "init",
    actionInput: { kind: "proof.init" },
    attempt: 1,
    txHash: transactionHash,
  });
  await append({
    kind: "submitted",
    actionId: "init",
    attempt: 1,
    txHash: transactionHash,
  });
  await append({
    kind: "reconciled",
    actionId: "init",
    outcome: "pending",
    txHash: transactionHash,
  });
  database.close();
  database = await openWatcherSqliteProverFundingReservationStore({ path });
  vi.mocked(adapter.observe).mockClear();
  vi.mocked(adapter.prepare).mockClear();
  const originalEntries = await journal.load(initial.workflowId);
  walletUtxos.splice(
    0,
    walletUtxos.length,
    ...walletUtxos.filter(
      (utxo) => `${utxo.txHash}#${utxo.outputIndex}` !== funding.outRef,
    ),
    {
      txHash: transactionHash,
      outputIndex: 0,
      address: walletAddress,
      assets: { lovelace: remaining },
    },
  );
  const records = async () =>
    (await database.store.readAll()).map(
      parseWatcherProverFundingReservationRecord,
    );
  const recover = async () => bind(fresh, await createPermit(fresh, "2"));
  return {
    old,
    fresh,
    journalRoot,
    journalDirectory,
    journal,
    initial,
    pending,
    plan,
    transactionHash,
    signedTransactionCborHex,
    originalEntries,
    createPermit,
    bind,
    recover,
    adapter,
    run,
    records,
    restartStore: async () => {
      database.close();
      database = await openWatcherSqliteProverFundingReservationStore({ path });
    },
    store: database.store,
  };
};

// The setup uses the production classifier, opaque permits, actual signed bytes,
// SQLite leases and directory journals. Only canonical transaction observations
// are controlled; all recovery and lease rotation run through the orchestrator.
describe("funding recovery across authenticated observation refresh", () => {
  it("reconciles the original signed init and rotates its durable leases exactly once", async () => {
    const test = await setup();
    const journal = await test.recover();
    expect(workflowActuationDecisionDigest(journal)).toBe(
      test.old.decisionDigest,
    );
    expect(await test.records()).toEqual([test.pending]);
    const result = await test.run(journal);
    expect(result).toMatchObject({
      kind: "pending",
      workflowId: test.initial.workflowId,
    });
    const [record] = await test.records();
    expect(record).toMatchObject({
      reservationId: test.plan.reservationId,
      decisionDigest: test.old.decisionDigest,
      state: "active",
      pendingTransition: null,
    });
    expect(record!.activeInputs).toEqual(
      expect.arrayContaining(
        test.plan.inputs.filter(({ role }) => role === "collateral"),
      ),
    );
    expect(
      record!.activeInputs.some(
        ({ outRef }) => outRef === `${test.transactionHash}#0`,
      ),
    ).toBe(true);
    expect(vi.mocked(test.adapter.reconcile)).toHaveBeenCalledTimes(1);
    expect(vi.mocked(test.adapter.observe)).toHaveBeenCalledTimes(1);
    await test.run(await test.recover());
    expect(await test.records()).toEqual([record]);
    expect(vi.mocked(test.adapter.reconcile)).toHaveBeenCalledTimes(1);
    expect(vi.mocked(test.adapter.submit)).not.toHaveBeenCalled();
    expect(vi.mocked(test.adapter.prepare)).not.toHaveBeenCalled();
    expect(await readdir(test.journalDirectory)).toEqual([
      test.initial.workflowId,
    ]);
    const entries = await test.journal.load(test.initial.workflowId);
    expect(
      entries.filter(({ event }) => event.kind === "confirmed"),
    ).toHaveLength(1);
    expect(
      entries.filter(({ event }) => event.kind === "submission_intent"),
    ).toHaveLength(1);
  });

  it("survives a crash after SQLite confirmation commits but before the workflow records it", async () => {
    const test = await setup();
    const journal = await test.recover();
    const crash = new Error(
      "process interrupted before reconciliation journal append",
    );
    vi.spyOn(journal, "append").mockImplementationOnce(async (entry) => {
      expect(entry.event).toEqual({
        kind: "reconciled",
        actionId: "init",
        outcome: "confirmed",
        txHash: test.transactionHash,
      });
      expect((await test.records())[0]).toMatchObject({
        revision: "2",
        pendingTransition: null,
      });
      throw crash;
    });
    await expect(test.run(journal)).rejects.toBe(crash);
    const [committed] = await test.records();
    expect(await test.journal.load(test.initial.workflowId)).toEqual(
      test.originalEntries,
    );
    expect(test.adapter.observe).not.toHaveBeenCalled();
    await test.restartStore();
    expect(await test.run(await test.recover())).toMatchObject({
      kind: "pending",
      workflowId: test.initial.workflowId,
    });
    expect(await test.records()).toEqual([committed]);
    expect(test.adapter.reconcile).toHaveBeenCalledTimes(2);
    expect(test.adapter.observe).toHaveBeenCalledTimes(1);
    expect(test.adapter.submit).not.toHaveBeenCalled();
    const entries = await test.journal.load(test.initial.workflowId);
    expect(entries.slice(0, test.originalEntries.length)).toEqual(
      test.originalEntries,
    );
    expect(
      entries.filter(({ event }) => event.kind === "confirmed"),
    ).toHaveLength(1);
    expect(
      entries.filter(({ event }) => event.kind === "submission_intent"),
    ).toHaveLength(1);
    expect(await readdir(test.journalDirectory)).toEqual([
      test.initial.workflowId,
    ]);
  });

  it("retains unknown submissions and their original signed bytes without another action", async () => {
    const test = await setup();
    vi.mocked(test.adapter.reconcile).mockResolvedValue({
      kind: "pending",
      txHash: test.transactionHash,
    });
    expect(await test.run(await test.recover())).toMatchObject({
      kind: "pending",
      workflowId: test.initial.workflowId,
    });
    expect(await test.records()).toEqual([test.pending]);
    expect(
      (await test.records())[0]!.pendingTransition!.signedTransactionCborHex,
    ).toBe(test.signedTransactionCborHex);
    expect(test.adapter.observe).not.toHaveBeenCalled();
    expect(test.adapter.submit).not.toHaveBeenCalled();
  });

  it("quarantines rejected/conflicting lineage without releasing collateral or submitting", async () => {
    const test = await setup();
    vi.mocked(test.adapter.reconcile).mockResolvedValue({
      kind: "conflict",
      reason: "canonical transaction consumes different inputs",
    });
    expect(await test.run(await test.recover())).toMatchObject({
      kind: "stalled",
    });
    expect((await test.records())[0]).toMatchObject({
      reservationId: test.plan.reservationId,
      state: "conflict",
      activeInputs: test.pending.activeInputs,
    });
    expect(test.adapter.observe).not.toHaveBeenCalled();
    expect(test.adapter.submit).not.toHaveBeenCalled();
    await expect(test.recover()).rejects.toThrow(
      "unique active original funding reservation",
    );
  });

  it("honors fresh authority revocation and seals the journal execution identity", async () => {
    const test = await setup();
    const admitted = await test.createPermit(test.fresh, "2");
    const journal = test.bind(test.fresh, admitted);
    expect(() =>
      bindWorkflowActuationRecoveryIdentity({
        permit: admitted.controller.permit,
        category: "doubleSpend",
        rollbackGeneration: "2",
        originalDecision: test.fresh,
      }),
    ).toThrow("replace its execution identity");
    admitted.controller.revoke("canonical rollback");
    expect(() =>
      assertWorkflowJournalActuation({
        journal,
        deploymentFingerprint: deploymentIdentity.manifestId,
        category: "doubleSpend",
        headerHash: test.old.headerHash,
        checkpoint: "before_reconcile",
      }),
    ).toThrow("revoked");
    await expect(test.run(journal)).rejects.toThrow("revoked");
    expect(await test.records()).toEqual([test.pending]);
    const firstBinding = createWorkflowActuationPermitController({
      decision: test.fresh,
      rollbackGeneration: "2",
    });
    bindWorkflowActuationJournal({
      journal: new DirectoryFraudProofWorkflowJournalStore(
        test.journalDirectory,
      ),
      permit: firstBinding.permit,
      category: "doubleSpend",
      deploymentFingerprint: deploymentIdentity.manifestId,
      headerHash: test.fresh.headerHash,
      decisionDigest: test.fresh.decisionDigest,
    });
    expect(() =>
      bindWorkflowActuationRecoveryIdentity({
        permit: firstBinding.permit,
        category: "doubleSpend",
        rollbackGeneration: "2",
        originalDecision: test.old,
      }),
    ).toThrow("after journal binding");
  });

  it("abandons a canonically absent expired intent without inventing confirmation or submission", async () => {
    const test = await setup();
    vi.mocked(test.adapter.reconcile).mockResolvedValue({ kind: "not_found" });
    expect(await test.run(await test.recover())).toMatchObject({
      kind: "pending",
      workflowId: test.initial.workflowId,
    });
    expect((await test.records())[0]).toMatchObject({
      reservationId: test.plan.reservationId,
      state: "active",
      pendingTransition: null,
      activeInputs: test.pending.activeInputs,
    });
    expect(test.adapter.observe).toHaveBeenCalledTimes(1);
    expect(test.adapter.submit).not.toHaveBeenCalled();
    expect(
      (await test.journal.load(test.initial.workflowId)).some(
        ({ event }) => event.kind === "confirmed",
      ),
    ).toBe(false);
  });

  it("refuses a self-consistent prepared artifact bound to another payload", async () => {
    const test = await setup();
    const entry = test.originalEntries.find(
      ({ event }) => event.kind === "prepared",
    )!;
    if (entry.event.kind !== "prepared")
      throw new Error("missing fixture artifact");
    const artifact = {
      ...entry.event.artifact,
      evidenceBinding: {
        headerHash: test.old.headerHash,
        payloadEnvelopeSha256: "fe".repeat(32),
        payloadSha256: test.old.payloadSha256,
      },
    };
    await writeFile(
      join(
        test.journalDirectory,
        test.initial.workflowId,
        `${entry.sequence.toString().padStart(8, "0")}.json`,
      ),
      JSON.stringify({
        ...entry,
        event: {
          ...entry.event,
          artifact,
          artifactDigest: journalJsonDigest(normalizeJournalJson(artifact)),
        },
      }),
    );
    await expect(test.recover()).rejects.toThrow("prepared artifact differs");
    expect(await test.records()).toEqual([test.pending]);
  });

  it("refuses a journal intent inconsistent with the reservation's exact signed transaction", async () => {
    const test = await setup();
    for (const entry of test.originalEntries) {
      if (!("txHash" in entry.event)) continue;
      await writeFile(
        join(
          test.journalDirectory,
          test.initial.workflowId,
          `${entry.sequence.toString().padStart(8, "0")}.json`,
        ),
        JSON.stringify({
          ...entry,
          event: { ...entry.event, txHash: "fe".repeat(32) },
        }),
      );
    }
    await expect(test.recover()).rejects.toThrow(
      "pending funding lineage differs",
    );
    expect(await test.records()).toEqual([test.pending]);
  });

  it("refuses ambiguous same-target workflow directories before reserving", async () => {
    const test = await setup();
    await mkdir(join(test.journalDirectory, "ab".repeat(32)));
    await expect(test.recover()).rejects.toThrow(
      "multiple candidate executions",
    );
    expect(await test.records()).toEqual([test.pending]);
  });

  it("rejects changed fault evidence even when its digest is well formed", async () => {
    const test = await setup();
    const controller = createWorkflowActuationPermitController({
      decision: test.fresh,
      rollbackGeneration: "2",
    });
    await authorizeWatcherProverFundingRecovery({
      journalRoot: test.journalRoot,
      deploymentIdentity,
      actuationPermit: controller.permit,
      category: "doubleSpend",
      rollbackGeneration: "2",
      store: test.store,
    });
    expect(() =>
      bindWorkflowActuationRecoveryIdentity({
        permit: controller.permit,
        category: "doubleSpend",
        rollbackGeneration: "2",
        originalDecision: (() => {
          const { decisionDigest: _digest, ...changed } = {
            ...test.old,
            replayDigest: "ab".repeat(32),
          };
          return {
            ...changed,
            decisionDigest: journalJsonDigest(normalizeJournalJson(changed)),
          };
        })(),
      }),
    ).toThrow("changed the classified fault evidence");
  });
});
