import { createHash } from "node:crypto";
import { join } from "node:path";

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
  DOUBLE_SPEND_COMPLETE_CANONICAL_REPLAY,
  FRAUD_PROOF_WORKFLOW_ADAPTER,
  FRAUD_PROOF_WORKFLOW_JOURNAL_SCHEMA_VERSION,
  FRAUD_PROOF_WORKFLOW_SAFETY,
  type FraudProofFamilyWorkflowAdapter,
  type FraudProofWorkflowJournalEntry,
  type FraudProofWorkflowJournalEvent,
  type FraudProofWorkflowJournalStore,
  type HeaderFaultDecision,
  runFraudProofWorkflow,
  WORKFLOW_RUNNER_FACTORIES,
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
import { installedLaunchScope } from "../../src/fault-proofs/fault-proof-application.js";
import {
  createWatcherProverFundingAuthority,
  createWatcherProverFundingAuthorityFactory,
} from "../../src/funding/prover-funding-authority.js";
import { calculateWatcherRuntimeProverFunding } from "../../src/funding/prover-funding-calculation.js";
import {
  parseWatcherProverFundingReservationRecord,
  type WatcherProverFundingReservationPlan,
} from "../../src/funding/prover-funding-reservation.js";
import {
  watcherDeploymentAppliedScriptHashes,
  watcherDeploymentProtocolScriptAuthority,
  watcherDeploymentReleaseEconomicsAuthority,
} from "../../src/runtime/deployment-identity.js";
import { runtimeAuthority } from "../funding/prover-funding-calculation.runtime-authority.js";
import { fixtureAttemptJournal } from "./fault-proof-funding-fixture.attempt-journal.js";
import { createFixtureProcessStops } from "./fault-proof-funding-fixture.process-stop.js";
import { signFundingRecoveryFixtureBody } from "./fault-proof-funding-fixture.sign-body.js";
import { createFixtureChain } from "./fault-proof-funding-fixture.signed-read.js";
import {
  closers,
  deploymentIdentity,
  directories,
  finality,
  key,
  sourcesFor,
  walletAddress,
} from "./fault-proof-funding-fixture.sources-for.js";
import {
  currentWorkflowJournal,
  editFixtureStore,
  fixtureFundingStore,
  objectiveWorkflowJournal,
  openFixtureFundingStore,
  rewriteWorkflowRows,
} from "./fault-proof-funding-fixture.workflow-store.js";
import { createFundingRecoveryDirectory } from "./test-storage-root.js";
import {
  recordObjectives,
  TEST_JOURNAL_KEY,
} from "./watcher-journal-fixture.js";

export const setupFundingRecoveryFixture = async (
  unused = false,
  withCollateral = false,
  priorRoster: boolean | "changed-role" = false,
  headerEndTime = 20n,
  protocolMinFeeCoefficient = 44,
  protocolMaxCollateralInputs = 3,
  attemptTtl: bigint | null = 100n,
) => {
  const journalRoot = await createFundingRecoveryDirectory();
  directories.push(journalRoot);
  const path = join(journalRoot, "watcher.sqlite");
  let database = await openFixtureFundingStore(path);
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
    launchScope: installedLaunchScope(old.launchScope),
    authenticationKey: TEST_JOURNAL_KEY,
  });
  expect(old.detectionId).toContain("#");
  await decisionJournal.appendLiveDecision(old);
  await decisionJournal.appendLiveDecision(fresh);
  // The queue records the objective before its workflow journal exists.
  recordObjectives(journalRoot, [
    { category: "doubleSpend", headerHash: old.headerHash },
  ]);
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
  const stops = createFixtureProcessStops();
  const factory = () =>
    createWatcherProverFundingAuthorityFactory({
      launchScope: installedLaunchScope(old.launchScope),
      journalRoot,
      journalAuthenticationKey: TEST_JOURNAL_KEY,
      deploymentIdentity,
      protocolParameters,
      protocolParameterHistory: database.protocolParameterHistory,
      workflowJournal: currentWorkflowJournal(() => database.journal),
      store: {
        ...stops.wrap(fixtureFundingStore(database.store)),
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
                    "scriptDecodeSpend",
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
        store: stops.wrap(fixtureFundingStore(database.store)),
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
  const objective = {
    category: "doubleSpend",
    headerHash: old.headerHash,
  } as const;
  const objectiveJournal = objectiveWorkflowJournal(
    () => database.journal,
    objective,
  );
  const bind = (
    decision: HeaderFaultDecision,
    admitted: Awaited<ReturnType<typeof createPermit>>,
  ) =>
    bindWorkflowFundingReservationJournal({
      journal: bindWorkflowActuationJournal({
        journal: objectiveJournal(),
        permit: admitted.controller.permit,
        category: "doubleSpend",
        deploymentFingerprint: deploymentIdentity.manifestId,
        headerHash: decision.headerHash,
        decisionDigest: decision.decisionDigest,
      }),
      permit: admitted.permit,
    });
  const chain = createFixtureChain();
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
      async ({ txHash }) => await chain.chain(txHash!),
    ),
  };
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
      terminalVerifier: chain.terminalVerifier,
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
  if (attemptTtl !== null) body.set_ttl(attemptTtl);
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
  const events = Object.freeze({
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
  } as const satisfies Readonly<
    Record<string, FraudProofWorkflowJournalEvent>
  >);
  const { entriesFor, prepareAttempt } = fixtureAttemptJournal({
    journal,
    workflowId: initial.workflowId,
    identity: initial.identity,
  });
  const pending = unused
    ? parseWatcherProverFundingReservationRecord(
        (await database.store.readAll())[0],
      )
    : await prepareAttempt(
        database.store,
        {
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
        },
        events,
      );
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
  if (!unused) {
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
  }
  database.close();
  database = await openFixtureFundingStore(path);
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
    objective,
    objectiveJournal,
    journal,
    initial,
    pending,
    plan,
    transactionHash,
    signedTransactionCborHex,
    originalEntries,
    events,
    entriesFor,
    /** The init attempt as the store records it, for a prepare the store
     * refuses before any journal entry matters. */
    attempt: {
      workflowId: initial.workflowId,
      intentSequence: 3,
      category: "doubleSpend",
      actionId: "init",
      actionInput: { actionKind: "proof.init" },
    },
    /** Prepares an attempt on the store open now. */
    prepareAttempt: (
      input: Parameters<typeof prepareAttempt>[1],
      attemptEvents: Parameters<typeof prepareAttempt>[2],
    ) => prepareAttempt(database.store, input, attemptEvents),
    createPermit,
    bind,
    recover,
    adapter,
    run,
    append,
    ...chain,
    records,
    /** Overwrites committed workflow rows in place, then reopens the store
     * so no cached fold hides them. */
    rewriteJournalEntries: async (
      entries: readonly FraudProofWorkflowJournalEntry[],
    ) => {
      database.close();
      editFixtureStore(path, (raw) => rewriteWorkflowRows(raw, entries));
      database = await openFixtureFundingStore(path);
    },
    stopAt: stops.stopAt,
    restartStore: async () => {
      database.close();
      database = await openFixtureFundingStore(path);
    },
    get protocolParameterHistory() {
      return database.protocolParameterHistory!;
    },
    get store() {
      return fixtureFundingStore(database.store);
    },
    /** The per-proof workflow journal of the store open now. */
    get workflowJournal() {
      return database.journal;
    },
  };
};
