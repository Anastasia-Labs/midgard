import { createHash } from "node:crypto";
import { mkdir, mkdtemp, readFile, rm, writeFile } from "node:fs/promises";
import { homedir } from "node:os";
import { join } from "node:path";
import { fileURLToPath } from "node:url";

import * as SDK from "@al-ft/midgard-sdk";
import {
  CML,
  credentialToAddress,
  Data,
  Emulator,
  getAddressDetails,
  scriptFromNative,
  scriptHashToCredential,
  toUnit,
  type UTxO,
  validatorToScriptHash,
} from "@lucid-evolution/lucid";
import { afterAll, describe, expect, it, vi } from "vitest";

import { TRANSITION_TRACE_COMPLETE_CANONICAL_REPLAY } from "../src/workflow/complete-replay.js";
import {
  authenticatedStateQueueObservationDigest,
  classifyHeader,
  createHeaderClassifier,
} from "../src/workflow/header-classifier.js";

const hooks = vi.hoisted(() => ({
  binding: undefined as unknown,
  authority: undefined as unknown,
}));
vi.mock("../src/workflow/deployment-manifest-binding.js", async (load) => ({
  ...(await load<
    typeof import("../src/workflow/deployment-manifest-binding.js")
  >()),
  bindFraudProofWorkflowDeployment: async () => hooks.binding,
}));
vi.mock("../src/workflow/family-l1-observation.js", async (load) => {
  const actual =
    await load<typeof import("../src/workflow/family-l1-observation.js")>();
  return {
    ...actual,
    createFraudProofFamilyLocalKupmiosL1ObservationPort: (
      input: Parameters<
        typeof actual.createFraudProofFamilyLocalKupmiosL1ObservationPort
      >[0],
    ) =>
      actual.createFraudProofFamilyRawL1ObservationPort({
        ...input,
        authority: hooks.authority as Parameters<
          typeof actual.createFraudProofFamilyRawL1ObservationPort
        >[0]["authority"],
      }),
  };
});

import {
  buildVanRossemFitLedger,
  type VanRossemFitMeasurement,
  writeVanRossemFitLedger,
} from "../src/proof-fit/van-rossem-fit-ledger.js";
import {
  captureTransitionTraceL1Events,
  requireTransitionTraceL1Events,
  unsafeCreateTransitionTraceEventAuthorityFromRawForTest,
} from "../src/transition-trace/l1-events.js";
import { TRANSITION_TRACE_FINAL_REFERENCE_SCRIPT_ENTRIES } from "../src/transition-trace/submit.js";
import {
  createManifestBoundTransitionTraceWorkflow,
  runOrResumeManifestBoundTransitionTraceWorkflow,
} from "../src/transition-trace/workflow.js";
import {
  createHistoricalNativeScriptHistorySource,
  createHistoricalNativeScriptProviderRoster,
  createSqliteHistoricalNativeScriptCheckpointStore,
  HISTORICAL_NATIVE_SCRIPT_HISTORY_RECORD,
} from "../src/workflow/historical-native-script-corpus.js";
import { DirectoryFraudProofWorkflowJournalStore } from "../src/workflow/journal.js";
import { computeFraudProofRawL1PointId } from "../src/workflow/raw-l1-snapshot.js";
import {
  computeFraudProofReleaseEconomicsPolicyDigest,
  FRAUD_PROOF_RELEASE_ECONOMICS_POLICY_SCHEMA_VERSION,
} from "../src/workflow/release-economics-policy.js";
import {
  computeFraudProofReleaseFinalityPolicyDigest,
  FRAUD_PROOF_RELEASE_FINALITY_POLICY_SCHEMA_VERSION,
} from "../src/workflow/release-finality-policy.js";
import { recordCrossBlockRawEmulator } from "./support/cross-block-raw-emulator.js";
import { realBlueprintPath } from "./support/emulator/blueprints.js";
import { alignUnixTimeToEmulatorSlotBoundary } from "./support/emulator/emulator-context.js";
import { prepareFamilyHistory } from "./support/emulator/family-history.js";
import { makeFaultProofEmulatorHarness } from "./support/emulator/harness.js";
import { insertHistoryFillerAfter } from "./support/emulator/history-pair.js";
import { measureCompleteSignedTransaction } from "./support/emulator/measurement.js";
import {
  publishFraudProofChainReferenceScripts,
  publishRemovalReferenceScripts,
} from "./support/emulator/reference-scripts.js";
import { buildRemovalDeploymentInfo } from "./support/emulator/removal-deployment.js";
import {
  submitSecondHeaderTx,
  submitSetupTx,
} from "./support/emulator/setup-tx.js";
import {
  transitionTraceAcceptedRetainedFixture,
  transitionTraceDepositRetainedFixture,
  transitionTraceTimingRetainedFixture,
} from "./support/transition-trace-retained.js";
import { publishTransitionTraceYields } from "./support/transition-trace-yields.js";

// Explicit experimental payload bound; Cardano production limits are unchanged.
const HISTORY_BOUNDS = {
  inlineLimitBytes: 512n,
  maxPayloadBytes: 14000n,
  maxPayloadNodes: 512n,
};
const historyRecords: unknown[] = [];

const DEPLOYMENT = "11".repeat(32);
const finalityPolicy = {
  confirmationDepth: 30,
  automaticRecoveryMaxDepth: 2160,
  deepRollbackPolicy: "automated_rewind_replay_incident-v1",
} as const;
const economicsPolicy = {
  profile: "bounded-acceptance-v1",
  requiredBondLovelace: "900000000",
  slashingPenaltyLovelace: "500000000",
  fraudProverRewardLovelace: "400000000",
  inactivitySlashingPenaltyLovelace: "100000000",
  proverCollateralFloorLovelace: "5000000",
} as const;

const installedCases = [
  {
    name: "deposit",
    datumBytes: 0,
    honest: false,
    kind: "Deposit",
    timing: null,
  },
  {
    name: "honest-deposit",
    datumBytes: 0,
    honest: true,
    kind: "Deposit",
    timing: null,
  },
  {
    name: "maximum-deposit-datum",
    datumBytes: 12000,
    honest: false,
    kind: "Deposit",
    timing: null,
  },
  {
    name: "accepted",
    datumBytes: 0,
    honest: false,
    kind: "Deposit",
    timing: null,
  },
  {
    name: "honest-accepted",
    datumBytes: 0,
    honest: true,
    kind: "Deposit",
    timing: null,
  },
  ...(["Deposit", "Withdrawal"] as const).flatMap((kind) => [
    {
      name: `omitted-${kind}`,
      datumBytes: 0,
      honest: false,
      kind,
      timing: "omitted" as const,
    },
    {
      name: `honest-late-${kind}`,
      datumBytes: 0,
      honest: true,
      kind,
      timing: "omitted" as const,
    },
    {
      name: `outside-${kind}`,
      datumBytes: 0,
      honest: false,
      kind,
      timing: "outside" as const,
    },
    {
      name: `large-outside-${kind}`,
      datumBytes: 12000,
      honest: false,
      kind,
      timing: "outside" as const,
    },
  ]),
] as const;

const measurements: VanRossemFitMeasurement[] = [];
const completedCases = new Set<string>();
afterAll(async () => {
  const directory = process.env.MIDGARD_EVENT_HISTORY_EVIDENCE_DIR;
  if (directory === undefined) return;
  await mkdir(directory, { recursive: true });
  await writeFile(
    join(directory, "installed-transition-history.json"),
    JSON.stringify(
      {
        blueprintSha256: createHash("sha256")
          .update(await readFile(realBlueprintPath))
          .digest("hex"),
        completedCases: [...completedCases],
        bounds: HISTORY_BOUNDS,
        records: historyRecords,
      },
      (_key, value: unknown) =>
        typeof value === "bigint" ? value.toString() : value,
      2,
    ) + "\n",
  );
});
afterAll(async () => {
  expect([...completedCases].sort()).toEqual(
    installedCases.map(({ name }) => name).sort(),
  );
  await writeVanRossemFitLedger(
    fileURLToPath(
      new URL(
        "../../../docs/fault-proofs/size-plans/transition-trace-workflow-fit-ledger.json",
        import.meta.url,
      ),
    ),
    buildVanRossemFitLedger({
      category: "transitionTrace",
      blueprintSha256: createHash("sha256")
        .update(await readFile(realBlueprintPath))
        .digest("hex"),
      compilerVersion: "aiken v1.1.23+5adf783",
      measurements,
    }),
  );
});

describe("transition trace installed retained-history workflow", () => {
  it.each(installedCases)(
    "runs $name against real registered chain and raw L1 observations",
    async ({ name, datumBytes, honest, kind, timing }) => {
      const recorder = recordCrossBlockRawEmulator();
      let clock: ReturnType<typeof vi.spyOn> | undefined;
      let directory: string | undefined;
      let submissionIndex = 0;
      const originalSubmit = Emulator.prototype.submitTx;
      const measure = vi
        .spyOn(Emulator.prototype, "submitTx")
        .mockImplementation(async function (this: Emulator, transaction) {
          const result = await originalSubmit.call(this, transaction);
          const m = measureCompleteSignedTransaction(transaction);
          historyRecords.push({
            label: "installed-transaction",
            scenario: name,
            txHash: result,
            transactionCbor: transaction,
            measurement: m,
            fee: CML.Transaction.from_cbor_hex(transaction).body().fee(),
          });
          const outputs = CML.Transaction.from_cbor_hex(transaction)
            .body()
            .outputs();
          const isPublication =
            Array.from({ length: outputs.len() }, (_, i) =>
              outputs.get(i),
            ).some((output) => output.script_ref() !== undefined) ||
            (m.redeemerCount === 0 &&
              Array.from({ length: outputs.len() }, (_, i) =>
                outputs.get(i),
              ).some((output) => output.datum() !== undefined));
          measurements.push({
            name: `${name}/${submissionIndex++}`,
            kind: isPublication ? "publication" : "lifecycle",
            maximumShape: name,
            signedBytes: m.completeSignedBytes,
            memoryUnits: m.executionMemory,
            cpuUnits: m.executionSteps,
          });
          return result;
        });
      try {
        hooks.authority = recorder.authority;
        const h = await makeFaultProofEmulatorHarness({
          contractOptions: {
            realTransitionTrace: true,
            eventHistoryBounds: HISTORY_BOUNDS,
            alwaysFraudProofCatalogue: true,
          },
        });
        const { proverLucid: lucid } = h;
        // Optimized scaffold scripts alias each other. Give the event domains
        // distinct identities so authenticated address discovery is meaningful.
        const emptyEventDomain = (tag: number): SDK.AuthenticatedValidator => {
          const script = scriptFromNative({
            type: "all",
            scripts: [
              { type: "sig", keyHash: h.proverSigner.paymentKeyHash },
              { type: "before", slot: 10000000 + tag },
            ],
          });
          const hash = validatorToScriptHash(script);
          return {
            mintingScript: script,
            mintingScriptCBOR: script.script,
            policyId: hash,
            spendingScript: script,
            spendingScriptCBOR: script.script,
            spendingScriptHash: hash,
            spendingScriptAddress: credentialToAddress(
              "Custom",
              scriptHashToCredential(hash),
            ),
          };
        };
        const history = await prepareFamilyHistory(
          h,
          historyRecords,
          HISTORY_BOUNDS,
        );
        const contracts = {
          ...history.contracts,
          txOrder: emptyEventDomain(2),
        };
        const chain = await publishFraudProofChainReferenceScripts({
          lucid,
          steps: contracts.fraudProofContracts.transitionTrace.steps,
          entryNames: [
            "fraudProofTransitionTrace",
            ...TRANSITION_TRACE_FINAL_REFERENCE_SCRIPT_ENTRIES,
          ],
          familyLabel: "transition-trace",
        });
        const yields = await publishTransitionTraceYields(lucid, contracts);
        const removal = await publishRemovalReferenceScripts({
          lucid,
          contracts,
        });
        const now =
          alignUnixTimeToEmulatorSlotBoundary(
            h.funderLucid,
            h.emulator.now() + 120000,
          ) - 1;
        const nonce = history.nonce(kind);
        const id = {
          transactionId: nonce.txHash,
          outputIndex: BigInt(nonce.outputIndex),
        };
        const info: SDK.DepositInfo = {
          l2_address: {
            paymentCredential: { PublicKeyCredential: ["aa".repeat(28)] },
            stakeCredential: null,
          },
          l2_network_id: 0n,
          l2_datum: datumBytes === 0 ? null : "ab".repeat(datumBytes),
        };
        const event = { id, info };
        const payload: SDK.EventHistoryPayload =
          kind === "Deposit"
            ? { DepositPayload: { event } }
            : {
                WithdrawalPayload: {
                  event: {
                    id,
                    info: {
                      body: {
                        l2_outref: {
                          transactionId: "aa".repeat(32),
                          outputIndex: 0n,
                        },
                        l2_owner: "bb".repeat(28),
                        l2_value: new Map([
                          ["", new Map([["", 100_000_000n]])],
                        ]),
                        l1_address: info.l2_address,
                        l1_datum:
                          datumBytes === 0
                            ? "NoDatum"
                            : {
                                InlineDatum: { data: "ab".repeat(datumBytes) },
                              },
                      },
                      signature: ["aa".repeat(32), "bb".repeat(64)],
                      validity: "WithdrawalIsValid",
                    },
                  },
                  refund_address: info.l2_address,
                  refund_datum: "NoDatum",
                },
              };
        const originalAssets = { lovelace: 90_000_000n };
        const operator = getAddressDetails(
          await h.funderLucid.wallet().address(),
        ).paymentCredential!;
        const fixture =
          timing !== null
            ? await transitionTraceTimingRetainedFixture({
                operatorVkey: operator.hash,
                now,
                payload,
                originalAssets,
                omitted: timing === "omitted",
              })
            : name.includes("accepted")
              ? await transitionTraceAcceptedRetainedFixture({
                  operatorVkey: operator.hash,
                  now,
                  honest,
                })
              : await transitionTraceDepositRetainedFixture({
                  operatorVkey: operator.hash,
                  now,
                  event,
                  originalAssets,
                  honest,
                });
        let admitAfterPredecessor: (() => Promise<void>) | undefined;
        await submitSetupTx({
          lucid: h.funderLucid,
          contracts,
          nonceUtxo: h.nonceUtxo,
          catalogue: h.catalogue,
          header: fixture.predecessor.header,
          beforeHeaderCommit: async (hub) => {
            const admitEvent = async () => {
              const admitted = await history.admit(
                hub,
                payload,
                timing !== null
                  ? {
                      ...fixture.current.header,
                      endTime:
                        timing === "outside"
                          ? fixture.current.header.startTime
                          : honest
                            ? fixture.current.header.endTime + 1000n
                            : fixture.current.header.startTime + 1000n,
                    }
                  : name.includes("accepted")
                    ? fixture.predecessor.header
                    : {
                        ...fixture.current.header,
                        endTime: fixture.current.header.startTime + 1000n,
                      },
                { lovelace: kind === "Deposit" ? 95_000_000n : 100_000_000n },
              );
              if (kind === "Deposit")
                expect(admitted.captured.originalAssets.get("")?.get("")).toBe(
                  originalAssets.lovelace,
                );
              expect(admitted.witness.retainedDataUtxo !== undefined).toBe(
                datumBytes > 0,
              );
            };
            if (timing !== null && honest) admitAfterPredecessor = admitEvent;
            else await admitEvent();
          },
        });
        await admitAfterPredecessor?.();
        await submitSecondHeaderTx({
          lucid: h.funderLucid,
          contracts,
          header: fixture.current.header,
        });
        if (timing !== null) {
          const eligibleAt = Number(fixture.current.header.endTime) + 2000;
          if (h.emulator.now() < eligibleAt)
            h.emulator.awaitSlot(
              Math.ceil((eligibleAt - h.emulator.now()) / 1000),
            );
        }
        clock = vi
          .spyOn(Date, "now")
          .mockImplementation(() => h.emulator.now());
        const deploymentInfo = buildRemovalDeploymentInfo(
          contracts,
          h.catalogue,
          {
            removalReferenceScripts: removal.published,
            fraudProofReferenceScripts: { ...chain, ...yields },
          },
        );
        historyRecords.push({
          label: "installed-deployment",
          scenario: name,
          deploymentInfo,
          appliedTransition: contracts.fraudProofContracts.transitionTrace,
          prover: h.proverSigner.paymentKeyHash,
        });
        const referenceScripts: Record<string, UTxO> = Object.fromEntries(
          Object.entries({ ...chain, ...yields }).map(([name, publication]) => [
            name,
            publication.utxo,
          ]),
        );
        referenceScripts.stateQueueSpend = removal.published.stateQueueSpend;
        referenceScripts.computationThreadMint =
          h.witnessReferenceScripts.computationThreadMint!;
        referenceScripts.fraudProofMint =
          h.witnessReferenceScripts.fraudProofMint!;
        referenceScripts.phasMembershipWithdraw =
          h.witnessReferenceScripts.phasMembershipWithdraw!;
        const schemas = [
          SDK.FraudProofComputationThreadStepDatum,
          SDK.TransitionTraceStepDatum,
          SDK.TransitionTraceStepDatum,
          SDK.TransitionTraceStepDatum,
          SDK.TransitionTraceStepDatum,
          SDK.TransitionTraceProofCommitmentDatum,
          SDK.TransitionTraceProofCommitmentDatum,
          SDK.TransitionTraceStepDatum,
          SDK.TransitionTraceStepDatum,
        ];
        const definition = {
          category: "transitionTrace" as const,
          categoryId: "00000004",
          headerHash: fixture.current.headerHash,
          proverCredential: h.proverSigner.paymentKeyHash,
          stateQueue: {
            policyId: contracts.stateQueue.policyId,
            address: contracts.stateQueue.spendingScriptAddress,
          },
          computationThread: {
            policyId: contracts.computationThread.policyId,
            steps: contracts.fraudProofContracts.transitionTrace.steps.map(
              (step, index) => ({
                role: `computation_thread_step_${String(index + 1).padStart(2, "0")}`,
                address: step.spendingScriptAddress,
                datumSchema: schemas[index]!,
              }),
            ),
          },
          proofToken: {
            policyId: contracts.fraudProof.policyId,
            address: contracts.fraudProof.spendingScriptAddress,
          },
          operatorDirectory: {
            activePolicyId: contracts.activeOperators.policyId,
            activeAddress: contracts.activeOperators.spendingScriptAddress,
            retiredPolicyId: contracts.retiredOperators.policyId,
            retiredAddress: contracts.retiredOperators.spendingScriptAddress,
          },
          schedulerAddress: contracts.scheduler.spendingScriptAddress,
        };
        const binding = {
          deploymentFingerprint: DEPLOYMENT,
          definition,
          blueprint: h.realBlueprint,
          blueprintJson: JSON.stringify(h.realBlueprint),
          deploymentInfo,
          network: "Custom",
          resolvedContracts: {
            hubOraclePolicyId: contracts.hubOracle.policyId,
            contracts: contracts.fraudProofContracts,
          },
          referenceScriptsByContract: Object.fromEntries(
            Object.entries(referenceScripts).map(([name, utxo]) => [
              name,
              {
                outRef: `${utxo.txHash}#${utxo.outputIndex}`,
                scriptHash: validatorToScriptHash(utxo.scriptRef!),
              },
            ]),
          ),
          releaseFinality: {
            schemaVersion: FRAUD_PROOF_RELEASE_FINALITY_POLICY_SCHEMA_VERSION,
            deploymentIdentityDigest: DEPLOYMENT,
            blueprintHash: "bb".repeat(32),
            policyDigest:
              computeFraudProofReleaseFinalityPolicyDigest(finalityPolicy),
            policy: finalityPolicy,
          },
          releaseEconomics: {
            schemaVersion: FRAUD_PROOF_RELEASE_ECONOMICS_POLICY_SCHEMA_VERSION,
            deploymentIdentityDigest: DEPLOYMENT,
            blueprintHash: "bb".repeat(32),
            policyDigest:
              computeFraudProofReleaseEconomicsPolicyDigest(economicsPolicy),
            policy: economicsPolicy,
          },
        };
        hooks.binding = binding;
        const archive = new Map(
          [fixture.predecessor, fixture.current].map((item) => [
            item.headerHash,
            item,
          ]),
        );
        vi.stubGlobal("fetch", async (input: string | URL | Request) => {
          const url = new URL(
            typeof input === "string"
              ? input
              : input instanceof URL
                ? input.toString()
                : input.url,
          );
          const archived = archive.get(url.pathname.split("/").at(-1)!);
          if (archived === undefined)
            return new Response("not found", { status: 404 });
          const point = {
            slot: "1",
            blockNo: "1",
            blockHash: "01".padStart(64, "0"),
          };
          return new Response(
            JSON.stringify({
              schemaVersion: HISTORICAL_NATIVE_SCRIPT_HISTORY_RECORD,
              deploymentFingerprint: DEPLOYMENT,
              headerHash: archived.headerHash,
              payloadEnvelopeCborHex:
                archived.payloadEnvelopeCbor.toString("hex"),
              inclusionPoint: {
                ...point,
                pointId: computeFraudProofRawL1PointId(point),
              },
            }),
            { status: 200, headers: { "content-type": "application/json" } },
          );
        });
        directory = await mkdtemp(
          join(homedir(), "transition-trace-installed-"),
        );
        const historicalNativeScriptHistorySource =
          createHistoricalNativeScriptHistorySource({
            providerRoster: createHistoricalNativeScriptProviderRoster({
              deploymentFingerprint: DEPLOYMENT,
              providers: [
                {
                  sourceId: "archive-a",
                  authorityEndpoint: "https://archive-a.example.test",
                  operatorIdentitySha256: "aa".repeat(32),
                },
                {
                  sourceId: "archive-b",
                  authorityEndpoint: "https://archive-b.example.test",
                  operatorIdentitySha256: "bb".repeat(32),
                },
              ],
            }),
          });
        // Model the separate mutation-lease service across application restarts.
        const makeLease = (token: string) => ({
          token,
          source: "emulator",
          renew: vi.fn(async () => undefined),
          release: vi.fn(async () => undefined),
          fail: vi.fn(async (_reason: string) => undefined),
        });
        const leases = new Map<string, ReturnType<typeof makeLease>>();
        const stateQueueMutationLeaseCoordinator = {
          acquire: async () => {
            const lease = makeLease(`transition-lease-${leases.size}`);
            leases.set(lease.token, lease);
            return lease;
          },
          resume: async ({
            token,
            source,
          }: {
            token: string;
            source: string;
          }) => {
            const lease = leases.get(token);
            if (lease === undefined || lease.source !== source)
              throw new Error("Missing durable fixture lease");
            return lease;
          },
        };
        const config = {
          manifest: {},
          blueprintJson: JSON.stringify(h.realBlueprint),
          deploymentInfo,
          headerHash: fixture.current.headerHash,
          lucid,
          signer: h.proverSigner,
          referenceScripts,
          source: {} as never,
          historicalNativeScriptCheckpointStore:
            createSqliteHistoricalNativeScriptCheckpointStore({
              path: join(directory, "history.sqlite"),
              rollbackAuthenticationKey: Buffer.alloc(32, 0x90),
            }),
          historicalNativeScriptHistorySource,
          stateQueueMutationLeaseCoordinator,
        };
        const sources = [
          {
            sourceId: "retained",
            fetchPayloadByHeaderHash: async (headerHash: string) => {
              const item = archive.get(headerHash);
              if (item === undefined)
                throw new Error("missing retained payload");
              return {
                ok: true as const,
                sourceId: "retained",
                sourcePeerId: "peer-1",
                attempts: [],
                payloadEnvelopeCbor: item.payloadEnvelopeCbor,
                provenance: {
                  trustClass: "public_or_permissionless_da" as const,
                  sourceId: "retained/peer-1",
                  grade: "security" as const,
                },
              };
            },
          },
        ];
        let workflow = await createManifestBoundTransitionTraceWorkflow(config);
        const rawHandle = await captureTransitionTraceL1Events({
          binding: workflow.binding,
          authority: recorder.authority,
        });
        expect(requireTransitionTraceL1Events(rawHandle).events).toHaveLength(
          1,
        );
        expect(() => requireTransitionTraceL1Events({ ...rawHandle })).toThrow(
          "freshly admitted",
        );
        const classifier = await createHeaderClassifier({
          deploymentFingerprint: DEPLOYMENT,
          replayer: TRANSITION_TRACE_COMPLETE_CANONICAL_REPLAY,
          releaseFinalityAuthority: workflow.releaseFinalityAuthority,
          historicalReplayAuthority: {
            checkpointStore: config.historicalNativeScriptCheckpointStore,
            historySource: historicalNativeScriptHistorySource,
          },
          // The recorder's raw authority is injected directly rather than
          // through the module mock above: whether a module that imports the
          // mocked module during the mock factory's own load receives the
          // mock depends on evaluation order, which this test must not rely on.
          transitionTraceEventAuthority:
            unsafeCreateTransitionTraceEventAuthorityFromRawForTest({
              binding: workflow.binding,
              authority: recorder.authority,
            }),
        });
        const observation = await workflow.l1.observeHeader({
          headerHash: fixture.current.headerHash,
        });
        const decision = await classifyHeader({
          classifier,
          observation,
          authenticatedObservationDigest:
            await authenticatedStateQueueObservationDigest({
              observation,
              minimumConfirmationDepth: 30,
            }),
          sources,
        });
        expect(decision.decision).toBe(honest ? "healthy" : "fault_detected");
        if (!honest)
          expect(decision).toMatchObject({
            category: "transitionTrace",
            violationId: "transition-trace",
          });
        const journal = new DirectoryFraudProofWorkflowJournalStore(
          join(directory, "journal"),
        );
        let restarts = 0;
        let pointerChurned = false;
        let result = await runOrResumeManifestBoundTransitionTraceWorkflow({
          workflow,
          sources,
          journal,
        });
        for (
          let hop = 0;
          hop < 100 &&
          result.kind !== "completed" &&
          result.kind !== "no_fault_detected";
          hop++
        ) {
          // Recreate the installed constructor and reopen the durable journal at
          // every boundary; no in-memory proof or replay handle survives restart.
          restarts++;
          if (!honest && !name.includes("accepted") && !pointerChurned) {
            const observed = await workflow.l1.observe({
              headerHash: fixture.current.headerHash,
            });
            if (
              observed.stage.kind === "step" &&
              observed.stage.step === (timing === null ? 7 : 8)
            ) {
              const [hash, index] = observed.stage.threadOutRef.split("#");
              const thread = (
                await lucid.utxosByOutRef([
                  { txHash: hash!, outputIndex: Number(index) },
                ])
              )[0]!;
              if (timing !== null) {
                const routeCbor = recorder.signedCbors.get(hash!);
                expect(routeCbor).toBeDefined();
                const routeMeasurement = measureCompleteSignedTransaction(
                  routeCbor!,
                );
                if (datumBytes > 0)
                  expect(
                    routeMeasurement.referenceInputCount,
                  ).toBeGreaterThanOrEqual(4);
                historyRecords.push({
                  label: "installed-timing-route",
                  scenario: name,
                  routeTxHash: hash,
                  measurement: routeMeasurement,
                });
              }
              const state =
                timing === null
                  ? Data.from(
                      thread.datum!,
                      SDK.TransitionTraceProofCommitmentDatum,
                    ).data
                  : undefined;
              if (
                timing !== null ||
                (state !== null &&
                  state !== undefined &&
                  state.deposit_source_cbor !== "")
              ) {
                const witness = await SDK.fetchEventHistoryWitness(
                  { utxosAt: (address) => lucid.utxosAt(address) },
                  {
                    policyId:
                      history.applied[kind === "Deposit" ? 0 : 1]!.policyId,
                    address:
                      history.applied[kind === "Deposit" ? 0 : 1]!.address,
                    retentionAddress:
                      history.applied[kind === "Deposit" ? 0 : 1]!.retention
                        .address,
                    inlineLimitBytes: HISTORY_BOUNDS.inlineLimitBytes,
                  },
                  id,
                );
                expect(witness.kind).toBe("Present");
                const protectedUntil = Number(
                  witness.anchor.node.protected_until,
                );
                if (h.emulator.now() < protectedUntil + 61000)
                  h.emulator.awaitSlot(
                    Math.ceil(
                      (protectedUntil + 61000 - h.emulator.now()) / 1000,
                    ),
                  );
                const hub = (
                  await lucid.utxosAtWithUnit(
                    contracts.hubOracle.spendingScriptAddress,
                    toUnit(
                      contracts.hubOracle.policyId,
                      SDK.HUB_ORACLE_ASSET_NAME,
                    ),
                  )
                )[0]!;
                const churn = await insertHistoryFillerAfter(
                  {
                    applied: history.applied,
                    scripts: history.scripts,
                    lucid,
                    hub,
                    owner: h.proverSigner.paymentKeyHash,
                    funding: async () =>
                      (await lucid.wallet().getUtxos()).filter(
                        (utxo) => utxo.datum == null && utxo.scriptRef == null,
                      ),
                    bounds: () => ({
                      lower: h.emulator.now() - 60000,
                      validTo: h.emulator.now() + 10000,
                      protectedUntil:
                        BigInt(h.emulator.now() + 9999) +
                        history.recipes[0]!.protectionDurationMs,
                    }),
                  },
                  kind,
                  witness,
                  "ff".repeat(32),
                  [],
                );
                await history.submit("installed-pointer-churn", churn);
                expect(
                  await lucid.utxosByOutRef([witness.anchor.utxo]),
                ).toHaveLength(0);
                pointerChurned = true;
              }
            }
          }
          h.emulator.awaitBlock();
          workflow = await createManifestBoundTransitionTraceWorkflow(config);
          result = await runOrResumeManifestBoundTransitionTraceWorkflow({
            workflow,
            sources,
            journal: new DirectoryFraudProofWorkflowJournalStore(
              join(directory, "journal"),
            ),
          });
        }
        expect(
          result.kind,
          "reason" in result ? result.reason : undefined,
        ).toBe(honest ? "no_fault_detected" : "completed");
        if (!honest) {
          expect(restarts).toBeGreaterThan(2);
          expect(
            (
              await workflow.l1.observe({
                headerHash: fixture.current.headerHash,
              })
            ).stage.kind,
          ).toBe("removed");
          await expect(
            workflow.l1.observeHeader({
              headerHash: fixture.current.headerHash,
            }),
          ).rejects.toThrow("live target");
        }
        const leaseState = [...leases.values()].map((lease) => ({
          token: lease.token,
          renewals: lease.renew.mock.calls.length,
          releases: lease.release.mock.calls.length,
          failures: lease.fail.mock.calls,
        }));
        const journalRows =
          "workflowId" in result ? await journal.load(result.workflowId) : [];
        const intents = journalRows.flatMap(({ event }) =>
          event.kind === "submission_intent" ? [event] : [],
        );
        if (timing !== null && !honest) {
          expect(
            intents.some(
              (event) =>
                event.actionInput.stage === "step_08" &&
                event.actionInput.requiresMutationLease === true,
            ),
          ).toBe(true);
        }
        historyRecords.push({
          label: "installed-workflow-outcome",
          scenario: name,
          outcome: result.kind,
          workflowId: "workflowId" in result ? result.workflowId : null,
          restarts,
          pointerChurned,
          leaseState,
          submissionIntents: intents,
          confirmations: journalRows.flatMap(({ event }) =>
            event.kind === "confirmed" ? [event] : [],
          ),
        });
        if (!honest && !name.includes("accepted")) {
          expect(pointerChurned).toBe(true);
          expect(leases.size).toBeGreaterThan(0);
          for (const lease of leases.values()) {
            expect(
              lease.release,
              JSON.stringify(leaseState),
            ).toHaveBeenCalled();
            expect(lease.fail).not.toHaveBeenCalled();
          }
        }
        completedCases.add(name);
        if (!honest)
          expect(
            await lucid.utxosAtWithUnit(
              contracts.fraudProof.spendingScriptAddress,
              toUnit(
                contracts.fraudProof.policyId,
                "00000004" + fixture.current.headerHash,
              ),
            ),
          ).toHaveLength(1);
      } finally {
        clock?.mockRestore();
        measure.mockRestore();
        recorder.restore();
        vi.unstubAllGlobals();
        if (directory !== undefined)
          await rm(directory, { recursive: true, force: true });
      }
    },
    600000,
  );
});
