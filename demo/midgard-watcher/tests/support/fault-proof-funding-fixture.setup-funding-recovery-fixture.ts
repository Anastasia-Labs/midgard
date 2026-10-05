import { createHash } from "node:crypto";
import { join } from "node:path";
import { DatabaseSync } from "node:sqlite";

import {
  acceptedTransactionSubject,
  authenticatedStateQueueObservationDigest,
  bindWorkflowActuationJournal,
  bindWorkflowFundingReservationJournal,
  canonicalBlockEvidenceFromVerifiedPayload,
  classifyHeader,
  createFraudProofWorkflowRegistry,
  createHeaderClassifier,
  createWorkflowActuationPermitController,
  createWorkflowRuntimeFundingPolicy,
  DirectoryFraudProofWorkflowJournalStore,
  DOUBLE_SPEND_COMPLETE_CANONICAL_REPLAY,
  FRAUD_PROOF_WORKFLOW_ADAPTER,
  FRAUD_PROOF_WORKFLOW_JOURNAL_SCHEMA_VERSION,
  FRAUD_PROOF_WORKFLOW_SAFETY,
  FRAUD_PROOF_WORKFLOW_TERMINAL_VERIFIER,
  type FraudProofFamilyWorkflowAdapter,
  type FraudProofWorkflowJournalEvent,
  type FraudProofWorkflowJournalStore,
  type FraudProofWorkflowTerminalVerifier,
  type HeaderFaultDecision,
  runFraudProofWorkflow,
  WORKFLOW_RUNNER_FACTORIES,
  type WorkflowFundingSubmissionHandoff,
} from "@al-ft/midgard-fault-proofs";
import {
  authenticatedHeaderObservation,
  buildCanonicalBlockFixture,
  buildFixtureTransaction,
  outRefCbor,
} from "@al-ft/midgard-fault-proofs/test-support/canonical-block-evidence-fixture";
import {
  CML,
  credentialToAddress,
  scriptHashToCredential,
  type UTxO,
} from "@lucid-evolution/lucid";
import { expect, vi } from "vitest";

import { openWatcherFaultDecisionJournal } from "../../src/fault-proofs/fault-decision-journal.js";
import {
  createWatcherProverFundingAuthority,
  createWatcherProverFundingAuthorityFactory,
} from "../../src/funding/prover-funding-authority.js";
import { calculateWatcherRuntimeProverFunding } from "../../src/funding/prover-funding-calculation.js";
import {
  parseWatcherProverFundingReservationRecord,
  type WatcherProverFundingReservationPlan,
} from "../../src/funding/prover-funding-reservation.js";
import { openWatcherSqliteProverFundingReservationStore } from "../../src/funding/sqlite-prover-funding-reservation-store.js";
import {
  watcherDeploymentAppliedScriptHashes,
  watcherDeploymentProtocolScriptAuthority,
  watcherDeploymentReleaseEconomicsAuthority,
} from "../../src/runtime/deployment-identity.js";
import { runtimeAuthority } from "../funding/prover-funding-calculation.runtime-authority.js";
import { signFundingRecoveryFixtureBody } from "./fault-proof-funding-fixture.sign-body.js";
import {
  closers,
  deploymentIdentity,
  directories,
  finality,
  key,
  sourcesFor,
  walletAddress,
} from "./fault-proof-funding-fixture.sources-for.js";
import { createFundingRecoveryDirectory } from "./test-storage-root.js";

export const setupFundingRecoveryFixture = async (
  interruptAfterPreparation: boolean | "after_preflight" = false,
  legacyPending = false,
  unused = false,
  withCollateral = false,
  priorRoster: boolean | "changed-role" = false,
  headerEndTime = 20n,
  protocolMinFeeCoefficient = 44,
  protocolMaxCollateralInputs = 3,
) => {
  const journalRoot = await createFundingRecoveryDirectory();
  directories.push(journalRoot);
  const path = join(journalRoot, "watcher.sqlite");
  let database = await openWatcherSqliteProverFundingReservationStore({
    path,
    protocolParameterHistory: {
      deploymentIdentity,
      authenticationKey: Buffer.alloc(32, 0x91),
    },
  });
  closers.push(() => database.close());
  const sharedInput = outRefCbor(91, 0n);
  const fixture = await buildCanonicalBlockFixture({
    endTime: headerEndTime,
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
  const protocolParameters = await runtimeAuthority(
    deploymentIdentity,
    protocolMinFeeCoefficient,
    protocolMaxCollateralInputs,
  );
  const runner = WORKFLOW_RUNNER_FACTORIES.doubleSpend(async () => {
    throw new Error("adapter runner must not build in reconciliation test");
  });
  const walletUtxos: UTxO[] = [0, 1, 2, 3].map((outputIndex) => ({
    txHash: "99".repeat(32),
    outputIndex,
    address: walletAddress,
    assets: { lovelace: 2_000_000_000n },
  }));
  const readWalletUtxos = vi.fn(async () => walletUtxos);
  const resolveInputs = vi.fn(async (refs: readonly string[]) =>
    walletUtxos.filter((utxo) =>
      refs.includes(`${utxo.txHash}#${utxo.outputIndex}`),
    ),
  );
  let capturedPlan: WatcherProverFundingReservationPlan | undefined;
  const factory = () =>
    createWatcherProverFundingAuthorityFactory({
      launchScope: old.launchScope,
      journalRoot,
      deploymentIdentity,
      protocolParameters,
      protocolParameterHistory: database.protocolParameterHistory,
      store: {
        ...database.store,
        releaseUnused: (record) => database.store.releaseUnused!(record),
        reserve: async (plan, expectedIdleRevision) => {
          capturedPlan = plan;
          return await database.store.reserve(plan, expectedIdleRevision);
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
    const fundingFactory = factory();
    const request = {
      category: "doubleSpend" as const,
      runner,
      actuationPermit: controller.permit,
      rollbackGeneration: generation,
      decisionDigest: decision.decisionDigest,
      walletAddress,
      walletUtxos,
      readWalletUtxos,
      resolveInputs,
      resolveProtocolInputAuthority: async () => {
        throw new Error("unexpected protocol input lookup");
      },
    };
    let permit;
    if (priorRoster && capturedPlan === undefined) {
      // Admit the prior roster, retaining immutable proof-item custody.
      const contracts = Object.entries(
        watcherDeploymentAppliedScriptHashes(deploymentIdentity),
      )
        .filter(
          ([name]) => !name.endsWith("Mint") && !name.endsWith("Withdraw"),
        )
        .flatMap(([name, scriptHash]) => {
          const role =
            name === "correctionLockSpend"
              ? ("correction_lock" as const)
              : [
                    "fraudProofSpend",
                    "fraudProofCatalogueSpend",
                    "fieldPreimageCertificateSpend",
                    "cekProgramMaterialSpend",
                    "validationTraceDisputeProofItem",
                  ].includes(name)
                ? ("field_carrier" as const)
                : (name.startsWith("fraudProof") &&
                      !name.startsWith("fraudProofCatalogue")) ||
                    name.startsWith("validationTraceDispute")
                  ? ("proof_thread" as const)
                  : name.endsWith("Spend")
                    ? ("protocol_state" as const)
                    : undefined;
          return role === undefined
            ? []
            : [
                {
                  address: credentialToAddress(
                    deploymentIdentity.network,
                    scriptHashToCredential(scriptHash),
                  ),
                  scriptHash,
                  role,
                },
              ];
        });
      const references = Object.values(
        watcherDeploymentProtocolScriptAuthority(deploymentIdentity)
          .referenceScripts,
      );
      const policy = createWorkflowRuntimeFundingPolicy({
        category: "doubleSpend",
        runner,
        deploymentFingerprint: deploymentIdentity.manifestId,
        fundingPaymentKeyHash: key.to_public().hash().to_hex(),
        protocolParameters: protocolParameters.snapshot,
        economics: await watcherDeploymentReleaseEconomicsAuthority(
          deploymentIdentity,
        ).verifyForWorkflow({
          deploymentFingerprint: deploymentIdentity.manifestId,
        }),
        contracts: [
          ...new Map(
            contracts.map((entry, index) => [
              entry.address,
              priorRoster === "changed-role" && index === 0
                ? {
                    ...entry,
                    role:
                      entry.role === "proof_thread"
                        ? ("field_carrier" as const)
                        : ("proof_thread" as const),
                  }
                : entry,
            ]),
          ).values(),
        ],
        referenceScripts: [
          ...new Map(
            references.map(({ outRef, scriptHash }) => [
              outRef,
              { outRef, scriptHash },
            ]),
          ).values(),
        ],
      });
      const authority = await createWatcherProverFundingAuthority({
        ...request,
        deploymentIdentity,
        calculation: await calculateWatcherRuntimeProverFunding({
          deploymentIdentity,
          protocolParameters,
          policy,
        }),
        policy,
        store: database.store,
      });
      capturedPlan = authority.plan;
      permit = authority.permit;
    } else permit = await fundingFactory.create(request);
    return {
      permit,
      controller,
      releaseUnused: () =>
        fundingFactory.releaseUnused({ actuationPermit: controller.permit }),
    };
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
  const terminalVerify = vi.fn<FraudProofWorkflowTerminalVerifier["verify"]>(
    async ({ candidate }) => candidate,
  );
  const run = (
    journal: FraudProofWorkflowJournalStore,
    now = () => new Date(),
  ) =>
    runFraudProofWorkflow({
      now,
      deploymentFingerprint: deploymentIdentity.manifestId,
      evidence,
      detections: [
        {
          // Both spenders of the shared input.
          ...acceptedTransactionSubject(
            evidence.transactions[0]!.nodeTxId,
            evidence.transactions[1]!.nodeTxId,
          ),
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
        verify: terminalVerify,
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
  body.set_ttl(100n);
  if (withCollateral) {
    const collateral = CML.TransactionInputList.new();
    for (const reserved of plan.inputs.filter(
      ({ role }) => role === "collateral",
    )) {
      const [txHash, index] = reserved.outRef.split("#");
      collateral.add(
        CML.TransactionInput.new(
          CML.TransactionHash.from_hex(txHash!),
          BigInt(index!),
        ),
      );
    }
    body.set_collateral_inputs(collateral);
  }
  const { signedTransactionCborHex, transactionHash } =
    signFundingRecoveryFixtureBody(body);
  const prepared = (await journal.load(initial.workflowId)).find(
    ({ event }) => event.kind === "prepared",
  )!;
  if (prepared.event.kind !== "prepared")
    throw new Error("missing prepared fixture");
  const handoff: WorkflowFundingSubmissionHandoff = {
    workflowId: initial.workflowId,
    identity: initial.identity,
    preparedArtifactDigest: prepared.event.artifactDigest,
    expectedJournalSequence: (await journal.load(initial.workflowId)).length,
    preflight: {
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
    },
    submissionIntent: {
      kind: "submission_intent",
      actionId: "init",
      actionInput: { actionKind: "proof.init" },
      attempt: 1,
      txHash: transactionHash,
    },
  };
  const pending = unused
    ? parseWatcherProverFundingReservationRecord(
        (await database.store.readAll())[0],
      )
    : await database.store.prepareTransition({
        handoff,
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
  const finishSubmissionJournal = async () => {
    await append(handoff.preflight);
    await append(handoff.submissionIntent);
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
  };
  if (interruptAfterPreparation) {
    if (interruptAfterPreparation === "after_preflight")
      await append(handoff.preflight);
    // Inject the actual crash after the store's transaction committed, before
    // the first action journal append. Reopening below discards process state.
    await expect(
      (async () => {
        expect(
          await database.store.readPendingHandoff({
            reservationId: plan.reservationId,
          }),
        ).toEqual({
          transition: Object.fromEntries(
            Object.entries(pending.pendingTransition!).filter(
              ([key]) => key !== "transitionDigest",
            ),
          ),
          handoff,
        });
        throw new Error("interrupted after funding preparation commit");
      })(),
    ).rejects.toThrow("interrupted after funding preparation commit");
  } else if (!unused) await finishSubmissionJournal();
  database.close();
  if (legacyPending) {
    // Retained workflows written before action handoffs still have their exact
    // signed pending transition and complete intent in the directory journal.
    const legacy = new DatabaseSync(path);
    try {
      legacy
        .prepare(
          "DELETE FROM watcher_prover_funding_handoff_v1 WHERE reservation_id = ?",
        )
        .run(plan.reservationId);
    } finally {
      legacy.close();
    }
  }
  database = await openWatcherSqliteProverFundingReservationStore({
    path,
    protocolParameterHistory: {
      deploymentIdentity,
      authenticationKey: Buffer.alloc(32, 0x91),
    },
  });
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
    useUnspentPendingInputs: () => {
      walletUtxos.splice(
        0,
        walletUtxos.length,
        ...plan.inputs.map((input) => {
          const [txHash, outputIndex] = input.outRef.split("#");
          return {
            txHash: txHash!,
            outputIndex: Number(outputIndex),
            address: walletAddress,
            assets: {
              lovelace: BigInt(input.lovelace),
              ...Object.fromEntries(
                input.assets.map(({ unit, quantity }) => [
                  unit,
                  BigInt(quantity),
                ]),
              ),
            },
          };
        }),
      );
    },
    old,
    fresh,
    observation,
    fixture,
    original,
    fundingFactory: factory,
    runner,
    protocolParameters,
    walletUtxos,
    readWalletUtxos,
    resolveInputs,
    releaseUnusedAtStartup: () => factory().releaseUnused(),
    journalRoot,
    journalDirectory,
    journal,
    initial,
    pending,
    plan,
    transactionHash,
    signedTransactionCborHex,
    originalEntries,
    handoff,
    createPermit,
    bind,
    recover,
    adapter,
    run,
    append,
    terminalVerify,
    records,
    restartStore: async () => {
      database.close();
      database = await openWatcherSqliteProverFundingReservationStore({
        path,
        protocolParameterHistory: {
          deploymentIdentity,
          authenticationKey: Buffer.alloc(32, 0x91),
        },
      });
    },
    get protocolParameterHistory() {
      return database.protocolParameterHistory!;
    },
    get store() {
      return database.store;
    },
  };
};
