import { createFraudProofFamilyRawL1ObservationPort } from "../src/workflow/family-l1-observation.js";
import {
  authenticatedStateQueueObservationDigest,
  classifyHeader,
  createHeaderClassifier,
} from "../src/workflow/header-classifier.js";
import {
  computeFraudProofReleaseEconomicsPolicyDigest,
  FRAUD_PROOF_RELEASE_ECONOMICS_POLICY_SCHEMA_VERSION,
} from "../src/workflow/release-economics-policy.js";
import { FRAUD_PROOF_RELEASE_FINALITY_AUTHORITY } from "../src/workflow/release-finality-policy.js";
const economicsPolicy = {
  profile: "bounded-acceptance-v1",
  requiredBondLovelace: "900000000",
  slashingPenaltyLovelace: "500000000",
  fraudProverRewardLovelace: "400000000",
  inactivitySlashingPenaltyLovelace: "100000000",
  proverCollateralFloorLovelace: "5000000",
} as const;
import * as SDK from "@al-ft/midgard-sdk";

import { admitCrossBlockDuplicateArtifact } from "../src/cross-block-duplicate-event/artifact.js";
import {
  submitCrossBlockDuplicateEventCancel,
  submitCrossBlockDuplicateEventInit,
  submitCrossBlockDuplicateEventStep01,
  submitCrossBlockDuplicateEventStep02,
} from "../src/cross-block-duplicate-event/index.js";
import {
  crossBlockDuplicateDetectionId,
  detectCrossBlockDuplicateEvents,
} from "../src/cross-block-duplicate-event/replay.js";
import { crossBlockSettlementRecords } from "../src/cross-block-duplicate-event/settlement-authority.js";
import { unsafeCreateCrossBlockSettlementAuthorityFromRawForTest } from "../src/cross-block-duplicate-event/settlement-authority.js";
import type { FraudProofRawL1Snapshot } from "../src/workflow/raw-l1-snapshot.js";
import {
  computeFraudProofReleaseFinalityPolicyDigest,
  FRAUD_PROOF_RELEASE_FINALITY_POLICY_SCHEMA_VERSION,
} from "../src/workflow/release-finality-policy.js";
import { makeMaximumCrossBlockProof } from "./support/cross-block-maximum-proof.js";
import { recordCrossBlockRawEmulator } from "./support/cross-block-raw-emulator.js";
import { crossBlockRetainedFixture } from "./support/cross-block-retained.js";
import { settleOldestCrossBlockHeader } from "./support/cross-block-settlement-emulator.js";
import { makeHeader } from "./support/submit-init-emulator-shared.js";
const finalityPolicy = {
  confirmationDepth: 30,
  automaticRecoveryMaxDepth: 2160,
  deepRollbackPolicy: "automated_rewind_replay_incident-v1",
} as const;
import { createHash } from "node:crypto";
import { mkdtemp, readFile } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";
import { fileURLToPath } from "node:url";

import { getAddressDetails, toUnit } from "@lucid-evolution/lucid";
import { afterAll, describe, expect, it, vi } from "vitest";

import { createCrossBlockDuplicateEventTransactionPort } from "../src/cross-block-duplicate-event/workflow.js";
import {
  buildVanRossemFitLedger,
  type VanRossemFitMeasurement,
  writeVanRossemFitLedger,
} from "../src/proof-fit/van-rossem-fit-ledger.js";
import { submitRemoveFraudulentBlock } from "../src/remove-fraudulent-block.js";
import { classifyCanonicalBlockViolations } from "../src/workflow/classification.js";
import { CROSS_BLOCK_DUPLICATE_EVENT_COMPLETE_CANONICAL_REPLAY } from "../src/workflow/complete-replay.js";
import { createCursorFamilyWorkflowAdapter } from "../src/workflow/cursor-family-adapter.js";
import { CROSS_BLOCK_DUPLICATE_EVENT_CURSOR_SPEC } from "../src/workflow/cursor-family-spec.js";
import {
  computeFraudProofWorkflowId,
  DirectoryFraudProofWorkflowJournalStore,
  type FraudProofWorkflowIdentity,
  type FraudProofWorkflowJournalEvent,
  journalJsonDigest,
} from "../src/workflow/journal.js";
import { realBlueprintPath } from "./support/emulator/blueprints.js";
import { alignUnixTimeToEmulatorSlotBoundary } from "./support/emulator/emulator-context.js";
import {
  captureEmulatorSubmission,
  measureCompleteSignedTransaction,
} from "./support/emulator/measurement.js";
import { buildRemovalDeploymentInfo } from "./support/emulator/removal-deployment.js";
import {
  submitSecondHeaderTx,
  submitSetupTx,
} from "./support/emulator/setup-tx.js";
import {
  makeFaultProofEmulatorHarness,
  publishCrossBlockDuplicateEventReferenceScripts,
  publishRemovalReferenceScripts,
} from "./support/submit-init-emulator-shared.js";

const measurements: VanRossemFitMeasurement[] = [];
afterAll(async () => {
  if (process.env.MIDGARD_WRITE_FIT_LEDGER !== "1") return;
  expect(
    measurements.some((row) =>
      row.name.startsWith("forced-transaction/step_02"),
    ),
  ).toBe(true);
  await writeVanRossemFitLedger(
    fileURLToPath(
      new URL(
        "../../../docs/fault-proofs/size-plans/cross-block-duplicate-event-workflow-fit-ledger.json",
        import.meta.url,
      ),
    ),
    buildVanRossemFitLedger({
      category: "crossBlockDuplicateEvent",
      blueprintSha256: createHash("sha256")
        .update(await readFile(realBlueprintPath))
        .digest("hex"),
      compilerVersion: "aiken v1.1.23+5adf783",
      measurements,
    }),
  );
});
describe("crossBlockDuplicateEvent installed cursor actuator", () => {
  it.each(["deposit", "withdrawal", "forced-transaction"] as const)(
    "captures and restarts %s with real settlement authority",
    async (kind) => {
      const recorder = recordCrossBlockRawEmulator();
      let clock: ReturnType<typeof vi.spyOn> | undefined;
      try {
        const baseHarness = await makeFaultProofEmulatorHarness({
          contractOptions: {
            realCrossBlockDuplicateEvent: true,
            realSettlement: true,
          },
        });
        const h = {
          ...baseHarness,
          family: baseHarness.contracts.crossBlockDuplicateEvent!,
          category: baseHarness.catalogue.categories.crossBlockDuplicateEvent!,
        };
        for (const role of [
          "computationThreadMint",
          "phasMembershipWithdraw",
          "fraudProofMint",
        ] as const) {
          const reference = h.witnessReferenceScripts[role]!;
          const cbor = recorder.signedCbors.get(reference.txHash);
          if (cbor === undefined)
            throw new Error("required witness publication was not recorded");
          const m = measureCompleteSignedTransaction(cbor);
          measurements.push({
            name: `${kind}/publish-witness/${role}`,
            kind: "publication",
            maximumShape: "registered required witness reference script",
            signedBytes: m.completeSignedBytes,
            memoryUnits: m.executionMemory,
            cpuUnits: m.executionSteps,
          });
        }
        const operator = getAddressDetails(
          await h.funderLucid.wallet().address(),
        ).paymentCredential!;
        const start =
          alignUnixTimeToEmulatorSlotBoundary(
            h.funderLucid,
            h.emulator.now() + 120_000,
          ) - 1;
        const settled = await crossBlockRetainedFixture({
          operatorVkey: operator.hash,
          now: start,
          largeDatum: true,
          kind,
        });
        const fixture = await crossBlockRetainedFixture({
          operatorVkey: operator.hash,
          now: Number(settled.header.endTime),
          largeDatum: true,
          kind,
          prevHeaderHash: settled.headerHash,
          prevUtxosRoot: settled.header.utxosRoot,
        });
        await submitSetupTx({
          lucid: h.funderLucid,
          contracts: h.contracts,
          nonceUtxo: h.nonceUtxo,
          catalogue: h.catalogue,
          header: settled.header,
        });
        const committed = await submitSecondHeaderTx({
          lucid: h.funderLucid,
          contracts: h.contracts,
          header: fixture.header,
        });
        const removal = await publishRemovalReferenceScripts({
          lucid: h.proverLucid,
          contracts: h.contracts,
        });
        await settleOldestCrossBlockHeader(h);
        clock = vi
          .spyOn(Date, "now")
          .mockImplementation(() => h.emulator.now());
        const setup = {
          headerHash: committed.headerHash,
          fraudulentBlockOutRef: committed.blockOutRef,
        };
        const scenario = { name: kind };
        const publications = await captureEmulatorSubmission(h.emulator, () =>
          publishCrossBlockDuplicateEventReferenceScripts({
            lucid: h.proverLucid,
            contracts: h.family,
          }),
        );
        const steps = publications.result;
        publications.measurements.forEach((m, index) =>
          measurements.push({
            name: `${kind}/publish/${index}`,
            kind: "publication",
            maximumShape: "two physical validators",
            signedBytes: m.completeSignedBytes,
            memoryUnits: m.executionMemory,
            cpuUnits: m.executionSteps,
          }),
        );
        const leaseCoordinator = {
          acquire: async () => ({
            token: "decoding-lease",
            source: "emulator",
            renew: async () => {},
            release: async () => {},
            fail: async () => {},
          }),
        };
        const binding = {
          definition: { headerHash: setup.headerHash },
          blueprint: h.realBlueprint,
          deploymentInfo: buildRemovalDeploymentInfo(h.contracts, h.catalogue, {
            removalReferenceScripts: removal.published,
          }),
          network: "Custom",
          resolvedContracts: {
            category: h.category,
            hubOraclePolicyId: h.contracts.hubOracle.policyId,
          },
          deploymentFingerprint: "aa".repeat(32),
          releaseIdentityDigest: "bb".repeat(32),
          releaseFinality: {
            schemaVersion: FRAUD_PROOF_RELEASE_FINALITY_POLICY_SCHEMA_VERSION,
            deploymentIdentityDigest: "aa".repeat(32),
            releaseIdentityDigest: "bb".repeat(32),
            policyDigest:
              computeFraudProofReleaseFinalityPolicyDigest(finalityPolicy),
            policy: finalityPolicy,
          },
          releaseEconomics: {
            schemaVersion: FRAUD_PROOF_RELEASE_ECONOMICS_POLICY_SCHEMA_VERSION,
            deploymentIdentityDigest: "aa".repeat(32),
            releaseIdentityDigest: "bb".repeat(32),
            policyDigest:
              computeFraudProofReleaseEconomicsPolicyDigest(economicsPolicy),
            policy: economicsPolicy,
          },
        };
        let hideSettlement = false;
        const raw = {
          ...recorder.authority,
          capture: async (
            request: Parameters<typeof recorder.authority.capture>[0],
          ) => {
            const snapshot = (await recorder.authority.capture(
              request,
            )) as FraudProofRawL1Snapshot;
            return hideSettlement
              ? {
                  ...snapshot,
                  scopes: snapshot.scopes.map((scope) =>
                    scope.role === "settlement"
                      ? { ...scope, utxos: [] }
                      : scope,
                  ),
                }
              : snapshot;
          },
        };
        const authority =
          unsafeCreateCrossBlockSettlementAuthorityFromRawForTest({
            binding: binding as never,
            raw,
            historySource: {
              fetchPayloadByHeaderHash: async ({
                headerHash,
              }: {
                headerHash: string;
              }) => {
                if (headerHash !== settled.headerHash)
                  throw new Error("unexpected historical header");
                return { payloadEnvelopeCbor: settled.payloadEnvelopeCbor };
              },
            } as never,
          });
        const classifier = await createHeaderClassifier({
          deploymentFingerprint: binding.deploymentFingerprint,
          replayer: CROSS_BLOCK_DUPLICATE_EVENT_COMPLETE_CANONICAL_REPLAY,
          releaseFinalityAuthority: {
            authorityVersion: FRAUD_PROOF_RELEASE_FINALITY_AUTHORITY,
            verifyForWorkflow: async () => binding.releaseFinality,
          },
          settlementAuthority: authority,
        });
        const observed = fixture.evidence.observation;
        const decision = await classifyHeader({
          classifier,
          observation: observed,
          authenticatedObservationDigest:
            await authenticatedStateQueueObservationDigest({
              observation: observed,
              minimumConfirmationDepth: 30,
            }),
          sources: [
            {
              sourceId: "public-retained",
              fetchPayloadByHeaderHash: async () => ({
                ok: true,
                sourceId: "public-retained",
                sourcePeerId: "retained-peer",
                payloadEnvelopeCbor: fixture.payloadEnvelopeCbor,
                attempts: [],
                provenance: {
                  trustClass: "public_or_permissionless_da",
                  sourceId: "public-retained/retained-peer",
                  grade: "security",
                },
              }),
            },
          ],
        });
        expect(decision).toMatchObject({
          decision: "fault_detected",
          category: "crossBlockDuplicateEvent",
          headerHash: fixture.headerHash,
        });
        const replayContext = {
          settlements: await authority.capture(fixture.evidence),
        };
        const port = createCrossBlockDuplicateEventTransactionPort({
          binding: binding as never,
          lucid: h.proverLucid,
          signer: h.proverSigner,
          contracts: h.family,
          references: {
            steps,
            witnesses: h.witnessReferenceScripts as Required<
              typeof h.witnessReferenceScripts
            >,
          },
          settlementAuthority: authority,
          stateQueueMutationLeaseCoordinator: leaseCoordinator,
        });
        const replayed =
          await CROSS_BLOCK_DUPLICATE_EVENT_COMPLETE_CANONICAL_REPLAY.replay(
            fixture.evidence,
            replayContext,
          );
        const classification = await classifyCanonicalBlockViolations({
          evidence: fixture.evidence,
          detections: replayed.detections,
          minimumConfirmationDepth: 1,
        });
        if (
          classification.decision !== "fault_detected" ||
          classification.category !== "crossBlockDuplicateEvent"
        )
          throw new Error("real fixture did not detect");
        const artifact = await port.prepare({
          evidence: fixture.evidence,
          classification: {
            ...classification,
            category: "crossBlockDuplicateEvent",
          },
        });
        expect(() =>
          crossBlockSettlementRecords(fixture.evidence, {
            ...replayContext.settlements,
          }),
        ).toThrow(/not admitted/u);
        const alteredHistory =
          unsafeCreateCrossBlockSettlementAuthorityFromRawForTest({
            binding: binding as never,
            raw: recorder.authority,
            historySource: {
              fetchPayloadByHeaderHash: async () => ({
                payloadEnvelopeCbor: fixture.payloadEnvelopeCbor,
              }),
            } as never,
          });
        await expect(
          alteredHistory.capture(fixture.evidence),
        ).rejects.toThrow();
        const honest = await crossBlockRetainedFixture({
          operatorVkey: operator.hash,
          now: start,
          kind: kind === "deposit" ? "withdrawal" : "deposit",
        });
        expect(
          detectCrossBlockDuplicateEvents({
            evidence: honest.evidence,
            context: await authority.capture(honest.evidence),
          }),
        ).toEqual([]);
        const wrongKey = JSON.parse(JSON.stringify(artifact));
        wrongKey.coordinate.transactionId = "17".repeat(32);
        wrongKey.detectionId = crossBlockDuplicateDetectionId(
          wrongKey.coordinate,
        );
        await expect(
          admitCrossBlockDuplicateArtifact(wrongKey),
        ).rejects.toThrow(/absent/u);
        const sameHeader = JSON.parse(JSON.stringify(artifact));
        sameHeader.coordinate.settledHeaderHash = fixture.headerHash;
        sameHeader.settledPayloadEnvelopeCbor =
          fixture.payloadEnvelopeCbor.toString("hex");
        sameHeader.detectionId = crossBlockDuplicateDetectionId(
          sameHeader.coordinate,
        );
        await expect(
          admitCrossBlockDuplicateArtifact(sameHeader),
        ).rejects.toThrow(/distinct headers/u);
        const wrongBytes = {
          ...artifact,
          settledPayloadEnvelopeCbor:
            fixture.payloadEnvelopeCbor.toString("hex"),
        };
        await expect(
          admitCrossBlockDuplicateArtifact(wrongBytes),
        ).rejects.toThrow();
        const identity: FraudProofWorkflowIdentity = {
          schemaVersion: "midgard-fraud-proof-workflow-identity-v1",
          deploymentFingerprint: "aa".repeat(32),
          category: "crossBlockDuplicateEvent",
          target: { kind: "state_queue_header", headerHash: setup.headerHash },
        };
        const workflowId = computeFraudProofWorkflowId(identity);
        const directory = await mkdtemp(join(tmpdir(), "cross-block-journal-"));
        const append = async (event: FraudProofWorkflowJournalEvent) => {
          const store = new DirectoryFraudProofWorkflowJournalStore(directory);
          const entries = await store.load(workflowId);
          await store.append(
            {
              schemaVersion: "midgard-fraud-proof-workflow-journal-entry-v1",
              workflowId,
              identity,
              sequence: entries.length,
              recordedAt: new Date().toISOString(),
              event,
            },
            entries.length,
          );
        };
        await append({ kind: "started" });
        await append({
          kind: "prepared",
          artifact,
          artifactDigest: journalJsonDigest(artifact),
        });
        const proofUnit = toUnit(
          h.family.fraudProof.policyId,
          h.category.categoryId + setup.headerHash,
        );
        const l1 = createFraudProofFamilyRawL1ObservationPort({
          authority: recorder.authority,
          releaseFinality: binding.releaseFinality,
          releaseEconomics: binding.releaseEconomics,
          definition: {
            category: "crossBlockDuplicateEvent",
            categoryId: h.category.categoryId,
            headerHash: setup.headerHash,
            proverCredential: h.proverSigner.paymentKeyHash,
            stateQueue: {
              policyId: h.contracts.stateQueue.policyId,
              address: h.contracts.stateQueue.spendingScriptAddress,
            },
            computationThread: {
              policyId: h.family.computationThread.policyId,
              steps: [
                {
                  role: "computation_thread_step_01",
                  address: h.family.steps[0].spendingScriptAddress,
                  datumSchema: SDK.FraudProofComputationThreadStepDatum,
                },
                {
                  role: "computation_thread_step_02",
                  address: h.family.steps[1].spendingScriptAddress,
                  datumSchema: SDK.CrossBlockDuplicateEventStep02DatumSchema,
                },
              ],
            },
            proofToken: {
              policyId: h.family.fraudProof.policyId,
              address: h.family.fraudProof.spendingScriptAddress,
            },
            operatorDirectory: {
              activePolicyId: h.contracts.activeOperators.policyId,
              activeAddress: h.contracts.activeOperators.spendingScriptAddress,
              retiredPolicyId: h.contracts.retiredOperators.policyId,
              retiredAddress:
                h.contracts.retiredOperators.spendingScriptAddress,
            },
            schedulerAddress: h.contracts.scheduler.spendingScriptAddress,
          },
        });
        const adapter = () =>
          createCursorFamilyWorkflowAdapter({
            spec: CROSS_BLOCK_DUPLICATE_EVENT_CURSOR_SPEC,
            l1,
            transactions: port,
            stateQueueMutationLeaseCoordinator: leaseCoordinator,
          });
        for (let n = 0; n < 30; n++) {
          const store = new DirectoryFraudProofWorkflowJournalStore(directory);
          const entries = await store.load(workflowId);
          const restored = JSON.parse(
            JSON.stringify(
              entries.find((row) => row.event.kind === "prepared")!.event,
            ),
          );
          const context = {
            identity,
            workflowId,
            artifact: restored.artifact,
            entries,
          };
          const current = adapter();
          const observation = await current.observe(context);
          if (observation.kind !== "action_required")
            throw new Error("missing action");
          const action = observation.action;

          if (action.input.stage === "init") {
            hideSettlement = true;
            await expect(
              current.preflight({ ...context, action }),
            ).rejects.toThrow(/no longer live/u);
            hideSettlement = false;
            for (const field of [
              "settlementPolicyId",
              "settlementOutRef",
            ] as const) {
              const substituted = {
                ...context,
                artifact: {
                  ...context.artifact,
                  [field]:
                    field === "settlementPolicyId"
                      ? "18".repeat(28)
                      : `${"19".repeat(32)}#0`,
                },
              };
              await expect(
                current.preflight({ ...substituted, action }),
              ).rejects.toThrow(/no longer live/u);
            }
          }
          const preflight = await current.preflight({ ...context, action });
          await append({
            kind: "preflight_passed",
            actionId: action.actionId,
            txHash: preflight.txHash,
            localEvaluator: preflight.localUplcEvaluation.evaluator,
            referenceScripts: preflight.referenceScripts,
          });
          await append({
            kind: "submission_intent",
            actionId: action.actionId,
            actionInput: action.input,
            attempt: 1,
            txHash: preflight.txHash,
          });
          const result = await captureEmulatorSubmission(h.emulator, () =>
            current.submit({ ...context, action, preflight }),
          );
          result.measurements.forEach((m, index) =>
            measurements.push({
              name: `${scenario.name}/${String(action.input.stage)}/${n}/${index}`,
              kind: "lifecycle",
              maximumShape: `${1} event per root`,
              signedBytes: m.completeSignedBytes,
              memoryUnits: m.executionMemory,
              cpuUnits: m.executionSteps,
            }),
          );
          h.emulator.awaitBlock();

          await append({
            kind: "submission_ambiguous",
            actionId: action.actionId,
            attempt: 1,
            txHash: preflight.txHash,
            detail: "emulated restart after broadcast before receipt",
          });
          const recovered = await adapter().reconcile({
            ...context,
            action,
            txHash: preflight.txHash,
          });
          expect(recovered).toEqual({
            kind: "confirmed",
            txHash: preflight.txHash,
          });
          await append({
            kind: "reconciled",
            actionId: action.actionId,
            outcome: "confirmed",
            txHash: preflight.txHash,
          });
          await append({
            kind: "confirmed",
            actionId: action.actionId,
            txHash: preflight.txHash,
          });
          if (action.input.stage === "remove") break;
        }
        expect(
          await h.proverLucid.utxosAtWithUnit(
            h.family.fraudProof.spendingScriptAddress,
            proofUnit,
          ),
        ).toHaveLength(1);

        expect(
          (await l1.observe({ headerHash: setup.headerHash })).stage.kind,
        ).toBe("removed");
      } finally {
        clock?.mockRestore();
        recorder.restore();
      }
    },
    180000,
  );
});

it.each(["deposit", "withdrawal", "forced-transaction"] as const)(
  "fits maximum 64-branch openings with a real %s settlement",
  async (kind) => {
    const h = await makeFaultProofEmulatorHarness({
      contractOptions: {
        realCrossBlockDuplicateEvent: true,
        realSettlement: true,
      },
    });
    const family = h.contracts.crossBlockDuplicateEvent!;
    const category = h.catalogue.categories.crossBlockDuplicateEvent!;
    const maximum = await makeMaximumCrossBlockProof(kind);
    const start =
      alignUnixTimeToEmulatorSlotBoundary(
        h.funderLucid,
        h.emulator.now() + 120_000,
      ) - 1;
    const operator = getAddressDetails(await h.funderLucid.wallet().address())
      .paymentCredential!.hash;
    const commitments =
      kind === "deposit"
        ? { depositsRoot: maximum.counted.root, depositCount: 1n }
        : kind === "withdrawal"
          ? { withdrawalsRoot: maximum.counted.root, withdrawalCount: 1n }
          : {
              forcedTransactionsRoot: maximum.counted.root,
              forcedTransactionCount: 1n,
              validationTracesRoot: maximum.counted.root,
              validationTraceCount: 1n,
            };
    const settled = {
      ...makeHeader(operator, start),
      ...commitments,
      endTime: BigInt(start) + 61_000n,
      totalEventCount: 1n,
      transitionStepCount: 1n,
      transitionTraceRoot: maximum.counted.root,
      eventToStepRoot: maximum.counted.root,
    };
    const old = await submitSetupTx({
      lucid: h.funderLucid,
      contracts: h.contracts,
      nonceUtxo: h.nonceUtxo,
      catalogue: h.catalogue,
      header: settled,
    });
    const current = {
      ...settled,
      startTime: settled.endTime,
      endTime: settled.endTime + 61_000n,
      prevHeaderHash: old.headerHash,
      prevUtxosRoot: settled.utxosRoot,
    };
    const block = await submitSecondHeaderTx({
      lucid: h.funderLucid,
      contracts: h.contracts,
      header: current,
    });
    const measure = async <A>(
      stage: string,
      action: () => Promise<A>,
      type: "publication" | "lifecycle" = "lifecycle",
    ) => {
      const result = await captureEmulatorSubmission(h.emulator, action);
      result.measurements.forEach((m, index) =>
        measurements.push({
          name: `maximum64/${kind}/${stage}/${index}`,
          kind: type,
          maximumShape: `64 widest MPF branches; authenticated claimed count 1; ${maximum.fullValueBytes} source bytes authenticated by value digest`,
          signedBytes: m.completeSignedBytes,
          memoryUnits: m.executionMemory,
          cpuUnits: m.executionSteps,
        }),
      );
      return result.result;
    };
    const removal = await measure(
      "publish-removal",
      () =>
        publishRemovalReferenceScripts({
          lucid: h.proverLucid,
          contracts: h.contracts,
        }),
      "publication",
    );
    const references = await measure(
      "publish-steps",
      () =>
        publishCrossBlockDuplicateEventReferenceScripts({
          lucid: h.proverLucid,
          contracts: family,
        }),
      "publication",
    );
    await settleOldestCrossBlockHeader(h);
    const settlement = (
      await h.proverLucid.utxosAtWithUnit(
        h.contracts.settlement.spendingScriptAddress,
        toUnit(h.contracts.settlement.policyId, old.headerHash),
      )
    )[0]!;
    const init = () =>
      submitCrossBlockDuplicateEventInit({
        lucid: h.proverLucid,
        blueprint: h.realBlueprint,
        network: "Custom",
        contracts: family,
        category: {
          ...category,
          categoryId: SDK.CROSS_BLOCK_DUPLICATE_EVENT_FRAUD_CATEGORY_ID,
        },
        catalogue: {
          policyId: h.contracts.fraudProofCatalogue.policyId,
          spendingScriptAddress:
            h.contracts.fraudProofCatalogue.spendingScriptAddress,
          root: h.catalogue.root,
        },
        signer: h.proverSigner,
        fraudulentBlockOutRef: block.blockOutRef,
        witnessReferenceScripts: h.witnessReferenceScripts,
      });
    let thread = await measure("init", init);
    const step01 = (threadOutRef: string) =>
      submitCrossBlockDuplicateEventStep01({
        lucid: h.proverLucid,
        network: "Custom",
        contracts: family,
        signer: h.proverSigner,
        threadOutRef,
        stateQueueBlockOutRef: block.blockOutRef,
        committedEvent: maximum.committedEvent,
        referenceScriptUtxo: references[0],
      });
    if (kind === "deposit") {
      await measure("cancel-01", () =>
        submitCrossBlockDuplicateEventCancel({
          lucid: h.proverLucid,
          contracts: family,
          signer: h.proverSigner,
          threadOutRef: thread.nextThreadOutRef,
          referenceScriptUtxo: references[0],
          witnessReferenceScripts: h.witnessReferenceScripts,
        }),
      );
      thread = await measure("reinit-01", init);
      const advanced = await measure("step-01-before-cancel", () =>
        step01(thread.nextThreadOutRef),
      );
      await measure("cancel-02", () =>
        submitCrossBlockDuplicateEventCancel({
          lucid: h.proverLucid,
          contracts: family,
          signer: h.proverSigner,
          threadOutRef: advanced.nextThreadOutRef,
          referenceScriptUtxo: references[1],
          witnessReferenceScripts: h.witnessReferenceScripts,
        }),
      );
      thread = await measure("reinit-02", init);
    }
    const membershipOf = (event: SDK.CommittedDuplicateEventProof) =>
      "CommittedDuplicateEventDigestV1" in event
        ? event.CommittedDuplicateEventDigestV1.membership
        : "CommittedDuplicateDepositV1" in event
          ? event.CommittedDuplicateDepositV1.membership
          : "CommittedDuplicateWithdrawalV1" in event
            ? event.CommittedDuplicateWithdrawalV1.membership
            : event.CommittedDuplicateForcedTransactionV1.membership;
    const forged = structuredClone(maximum.committedEvent);
    const branch = membershipOf(forged).proof[0]!;
    if (!("Branch" in branch))
      throw new Error("maximum proof does not begin with branch");
    branch.Branch.neighbors =
      (branch.Branch.neighbors.startsWith("00") ? "01" : "00") +
      branch.Branch.neighbors.slice(2);
    const absent = structuredClone(maximum.committedEvent);
    membershipOf(absent).key.transactionId = "16".repeat(32);
    if (!("CommittedDuplicateEventDigestV1" in maximum.committedEvent))
      throw new Error("expected digest opening");
    const wrongDigest: SDK.CommittedDuplicateEventProof = {
      CommittedDuplicateEventDigestV1: {
        ...maximum.committedEvent.CommittedDuplicateEventDigestV1,
        membership: {
          ...maximum.committedEvent.CommittedDuplicateEventDigestV1.membership,
          value: "11".repeat(32),
        },
      },
    };
    for (const committedEvent of [forged, absent, wrongDigest])
      await expect(
        submitCrossBlockDuplicateEventStep01({
          lucid: h.proverLucid,
          network: "Custom",
          contracts: family,
          signer: h.proverSigner,
          threadOutRef: thread.nextThreadOutRef,
          stateQueueBlockOutRef: block.blockOutRef,
          committedEvent,
          referenceScriptUtxo: references[0],
        }),
      ).rejects.toThrow();
    const first = await measure("step-01", () =>
      step01(thread.nextThreadOutRef),
    );
    await expect(
      submitCrossBlockDuplicateEventStep02({
        lucid: h.proverLucid,
        contracts: family,
        signer: h.proverSigner,
        threadOutRef: first.nextThreadOutRef,
        settlementOutRef: `${settlement.txHash}#${settlement.outputIndex}`,
        settledHeaderHash: old.headerHash,
        settledEvent: forged,
        referenceScriptUtxo: references[1],
        witnessReferenceScripts: h.witnessReferenceScripts,
      }),
    ).rejects.toThrow();
    const final = await measure("step-02", () =>
      submitCrossBlockDuplicateEventStep02({
        lucid: h.proverLucid,
        contracts: family,
        signer: h.proverSigner,
        threadOutRef: first.nextThreadOutRef,
        settlementOutRef: `${settlement.txHash}#${settlement.outputIndex}`,
        settledHeaderHash: old.headerHash,
        settledEvent: maximum.committedEvent,
        referenceScriptUtxo: references[1],
        witnessReferenceScripts: h.witnessReferenceScripts,
      }),
    );
    expect(
      await h.proverLucid.utxosAtWithUnit(
        family.fraudProof.spendingScriptAddress,
        final.fraudProofUnit,
      ),
    ).toHaveLength(1);
    const now = BigInt(h.emulator.now());
    await measure("remove", () =>
      submitRemoveFraudulentBlock({
        lucid: h.proverLucid,
        blueprint: h.realBlueprint,
        deploymentInfo: buildRemovalDeploymentInfo(h.contracts, h.catalogue, {
          removalReferenceScripts: removal.published,
        }),
        network: "Custom",
        signer: h.proverSigner,
        fraudCategory: "crossBlockDuplicateEvent",
        fraudulentHeaderHash: block.headerHash,
        requireReferenceScripts: true,
        validFrom: now - 120_000n,
        validTo: now + 300_000n,
      }),
    );
  },
  180000,
);
