import { createHash } from "node:crypto";
import { mkdtemp, readFile } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";
import { fileURLToPath } from "node:url";
import { inspect } from "node:util";

import * as SDK from "@al-ft/midgard-sdk";
import { getAddressDetails, toUnit } from "@lucid-evolution/lucid";
import { afterAll, describe, expect, it, vi } from "vitest";

import {
  buildVanRossemFitLedger,
  type VanRossemFitMeasurement,
  writeVanRossemFitLedger,
} from "../src/proof-fit/van-rossem-fit-ledger.js";
import {
  createWithdrawalMistagTransactionPort,
  withdrawalMistagEvidenceRequirement,
} from "../src/withdrawal-mistag/workflow.js";
import { classifyCanonicalBlockViolations } from "../src/workflow/classification.js";
import { WITHDRAWAL_MISTAG_COMPLETE_CANONICAL_REPLAY } from "../src/workflow/complete-replay.js";
import { createCursorFamilyWorkflowAdapter } from "../src/workflow/cursor-family-adapter.js";
import { WITHDRAWAL_MISTAG_CURSOR_SPEC } from "../src/workflow/cursor-family-spec.js";
import { createFraudProofFamilyRawL1ObservationPort } from "../src/workflow/family-l1-observation.js";
import {
  computeFraudProofWorkflowId,
  DirectoryFraudProofWorkflowJournalStore,
  type FraudProofWorkflowIdentity,
  type FraudProofWorkflowJournalEvent,
  journalJsonDigest,
} from "../src/workflow/journal.js";
import {
  createAuthenticatedRawDatumPreimagePrerequisitePort,
  withRawDatumPreimagePrerequisite,
} from "../src/workflow/raw-datum-preimage-prerequisite.js";
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
import { captureEmulatorSubmission } from "./support/emulator/measurement.js";
import { buildRemovalDeploymentInfo } from "./support/emulator/removal-deployment.js";
import {
  submitSecondHeaderTx,
  submitSetupTx,
} from "./support/emulator/setup-tx.js";
import { publishRemovalReferenceScripts } from "./support/submit-init-emulator-shared.js";
import {
  makeWithdrawalMistagEmulatorHarness,
  publishWithdrawalMistagScripts,
} from "./support/withdrawal-mistag-emulator.js";
import { runWithdrawalMistagMaximumProof } from "./support/withdrawal-mistag-maximum-proof.js";
import { withdrawalMistagRetainedFixture } from "./support/withdrawal-mistag-retained.js";

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
const measurements: VanRossemFitMeasurement[] = [];
const completedCases = new Set<string>();
afterAll(async () => {
  if (process.env.MIDGARD_WRITE_FIT_LEDGER !== "1") return;
  expect([...completedCases].sort()).toEqual(
    [
      "valid-marked-invalid",
      "invalid-marked-valid",
      "maximum-payout",
      "maximum-value",
      "asset-boundary",
      "maximum-output",
      "maximum-proof",
      "maximum-asset-names",
    ].sort(),
  );
  await writeVanRossemFitLedger(
    fileURLToPath(
      new URL(
        "../../../docs/fault-proofs/size-plans/withdrawal-mistag-workflow-fit-ledger.json",
        import.meta.url,
      ),
    ),
    buildVanRossemFitLedger({
      category: "withdrawalMistag",
      blueprintSha256: createHash("sha256")
        .update(await readFile(realBlueprintPath))
        .digest("hex"),
      compilerVersion: "aiken v1.1.23+5adf783",
      measurements,
    }),
  );
});

describe("withdrawalMistag installed cursor actuator", () => {
  it.each([
    {
      name: "valid-marked-invalid",
      direction: "valid-marked-invalid" as const,
    },
    {
      name: "invalid-marked-valid",
      direction: "invalid-marked-valid" as const,
    },
    {
      name: "maximum-payout",
      direction: "valid-marked-invalid" as const,
      assetCount: 100,
      outputBytes: 16384,
      payoutDatumBytes: 12000,
    },
    {
      name: "maximum-value",
      direction: "invalid-marked-valid" as const,
      assetCount: 1304,
    },
    {
      name: "asset-boundary",
      direction: "valid-marked-invalid" as const,
      assetCount: 100,
    },
    {
      name: "maximum-output",
      direction: "valid-marked-invalid" as const,
      outputBytes: 16384,
    },
  ])(
    "captures, journals and restarts $name through proof mint and removal",
    async (scenario) => {
      const recorder = recordCrossBlockRawEmulator();
      let clock: ReturnType<typeof vi.spyOn> | undefined;
      try {
        const h = await makeWithdrawalMistagEmulatorHarness();
        const operator = getAddressDetails(
          await h.funderLucid.wallet().address(),
        ).paymentCredential!;
        const fixture = await withdrawalMistagRetainedFixture({
          ...scenario,
          operatorVkey: operator.hash,
          now:
            alignUnixTimeToEmulatorSlotBoundary(
              h.funderLucid,
              h.emulator.now() + 120_000,
            ) - 1,
        });
        await submitSetupTx({
          lucid: h.funderLucid,
          contracts: h.contracts,
          nonceUtxo: h.nonceUtxo,
          catalogue: h.catalogue,
          header: fixture.predecessor.header,
        });
        const committed = await submitSecondHeaderTx({
          lucid: h.funderLucid,
          contracts: h.contracts,
          header: fixture.evidence.header,
        });
        const setup = {
          headerHash: committed.headerHash,
          fraudulentBlockOutRef: committed.blockOutRef,
        };
        const publications = await captureEmulatorSubmission(
          h.emulator,
          async () => await publishWithdrawalMistagScripts({ harness: h }),
        );
        const steps = publications.result.refs;
        publications.measurements.forEach((m, index) =>
          measurements.push({
            name: `${scenario.name}/publish/${index}`,
            kind: "publication",
            maximumShape: "five physical validators",
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
        const removalCapture = await captureEmulatorSubmission(h.emulator, () =>
          publishRemovalReferenceScripts({
            lucid: h.proverLucid,
            contracts: h.contracts,
          }),
        );
        removalCapture.measurements.forEach((m, index) =>
          measurements.push({
            name: `${scenario.name}/publish-removal/${index}`,
            kind: "publication",
            maximumShape: scenario.name,
            signedBytes: m.completeSignedBytes,
            memoryUnits: m.executionMemory,
            cpuUnits: m.executionSteps,
          }),
        );
        const removal = removalCapture.result;
        clock = vi
          .spyOn(Date, "now")
          .mockImplementation(() => h.emulator.now());
        const binding = {
          definition: { headerHash: setup.headerHash },
          blueprint: h.realBlueprint,
          deploymentInfo: buildRemovalDeploymentInfo(h.contracts, h.catalogue, {
            removalReferenceScripts: removal.published,
          }),
          network: "Custom",
          resolvedContracts: { category: h.category },
          catalogue: {
            policyId: h.contracts.fraudProofCatalogue.policyId,
            spendingScriptAddress:
              h.contracts.fraudProofCatalogue.spendingScriptAddress,
            root: h.catalogue.root,
          },
          releaseFinality: {
            schemaVersion: FRAUD_PROOF_RELEASE_FINALITY_POLICY_SCHEMA_VERSION,
            deploymentIdentityDigest: "aa".repeat(32),
            blueprintHash: "bb".repeat(32),
            policyDigest:
              computeFraudProofReleaseFinalityPolicyDigest(finalityPolicy),
            policy: finalityPolicy,
          },
          releaseEconomics: {
            schemaVersion: FRAUD_PROOF_RELEASE_ECONOMICS_POLICY_SCHEMA_VERSION,
            deploymentIdentityDigest: "aa".repeat(32),
            blueprintHash: "bb".repeat(32),
            policyDigest:
              computeFraudProofReleaseEconomicsPolicyDigest(economicsPolicy),
            policy: economicsPolicy,
          },
        };
        const roles = [
          "computation_thread_step_01",
          "computation_thread_step_02",
          "computation_thread_step_03",
          "computation_thread_step_04",
          "computation_thread_step_05",
        ] as const;
        const datumSchemas = [
          SDK.FraudProofComputationThreadStepDatum,
          SDK.WithdrawalMistagStep02Datum,
          SDK.WithdrawalMistagStep03Datum,
          SDK.WithdrawalMistagStep04Datum,
          SDK.WithdrawalMistagStep05Datum,
        ];
        const l1 = createFraudProofFamilyRawL1ObservationPort({
          authority: recorder.authority,
          releaseFinality: binding.releaseFinality,
          releaseEconomics: binding.releaseEconomics,
          definition: {
            category: "withdrawalMistag",
            categoryId: h.category.categoryId,
            headerHash: setup.headerHash,
            proverCredential: h.proverSigner.paymentKeyHash,
            stateQueue: {
              policyId: h.contracts.stateQueue.policyId,
              address: h.contracts.stateQueue.spendingScriptAddress,
            },
            computationThread: {
              policyId: h.withdrawalMistag.computationThread.policyId,
              steps: h.withdrawalMistag.steps.map((step, index) => ({
                role: roles[index]!,
                address: step.spendingScriptAddress,
                datumSchema: datumSchemas[index]!,
              })),
            },
            proofToken: {
              policyId: h.withdrawalMistag.fraudProof.policyId,
              address: h.withdrawalMistag.fraudProof.spendingScriptAddress,
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
        const prerequisite = () =>
          createAuthenticatedRawDatumPreimagePrerequisitePort({
            category: "withdrawalMistag",
            lucid: h.proverLucid,
            network: "Custom",
            signer: h.proverSigner,
            publications: l1.publications,
            requirementForAction: withdrawalMistagEvidenceRequirement,
            transactionConfirmed: (args) => l1.transactionConfirmed(args),
          });
        const port = createWithdrawalMistagTransactionPort({
          evidencePrerequisite: prerequisite(),
          binding: binding as never,
          lucid: h.proverLucid,
          signer: h.proverSigner,
          contracts: h.withdrawalMistag,
          references: {
            steps,
            witnesses: h.witnessReferenceScripts as Required<
              typeof h.witnessReferenceScripts
            >,
          },
          replayContext: fixture.context,
          stateQueueMutationLeaseCoordinator: leaseCoordinator,
        });
        const decision =
          await WITHDRAWAL_MISTAG_COMPLETE_CANONICAL_REPLAY.replay(
            fixture.evidence,
            fixture.context,
          );
        const classification = await classifyCanonicalBlockViolations({
          evidence: fixture.evidence,
          detections: decision.detections,
          minimumConfirmationDepth: 1,
        });
        if (
          classification.decision !== "fault_detected" ||
          classification.category !== "withdrawalMistag"
        )
          throw new Error("real fixture did not detect");
        const artifact = await port.prepare({
          evidence: fixture.evidence,
          classification: { ...classification, category: "withdrawalMistag" },
        });
        const identity: FraudProofWorkflowIdentity = {
          schemaVersion: "midgard-fraud-proof-workflow-identity-v1",
          deploymentFingerprint: "aa".repeat(32),
          category: "withdrawalMistag",
          target: { kind: "state_queue_header", headerHash: setup.headerHash },
        };
        const workflowId = computeFraudProofWorkflowId(identity);
        const directory = await mkdtemp(
          join(tmpdir(), "withdrawal-mistag-journal-"),
        );
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
          h.withdrawalMistag.fraudProof.policyId,
          h.category.categoryId + setup.headerHash,
        );
        const adapter = () =>
          withRawDatumPreimagePrerequisite({
            category: "withdrawalMistag",
            prerequisite: prerequisite(),
            base: createCursorFamilyWorkflowAdapter({
              spec: WITHDRAWAL_MISTAG_CURSOR_SPEC,
              l1,
              transactions: port,
              stateQueueMutationLeaseCoordinator: leaseCoordinator,
            }),
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
          const preflight = await current
            .preflight({ ...context, action })
            .catch((error) => {
              throw new Error(
                `${String(action.input.stage)}: ${inspect(error, { depth: 8 })}`,
              );
            });
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
            durableRecovery: preflight.durableRecovery,
            attempt: 1,
            txHash: preflight.txHash,
          });
          const result = await captureEmulatorSubmission(h.emulator, () =>
            current.submit({ ...context, action, preflight }),
          );
          result.measurements.forEach((m, index) =>
            measurements.push({
              name: `${scenario.name}/${String(action.input.stage)}/${n}/${index}`,
              kind:
                action.input.stage === "publish_field_carriage"
                  ? "publication"
                  : "lifecycle",
              maximumShape: scenario.name,
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
          const persisted = (await store.load(workflowId))
            .map((row) => row.event)
            .reverse()
            .find(
              (event) =>
                event.kind === "submission_intent" &&
                event.actionId === action.actionId,
            );
          if (persisted?.kind !== "submission_intent")
            throw new Error("missing persisted intent");
          const recovered = await adapter().reconcile({
            durableRecovery: persisted.durableRecovery,
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
            h.withdrawalMistag.fraudProof.spendingScriptAddress,
            proofUnit,
          ),
        ).toHaveLength(1);
        expect(
          (await l1.observe({ headerHash: setup.headerHash })).stage.kind,
        ).toBe("removed");
        completedCases.add(scenario.name);
      } finally {
        clock?.mockRestore();
        recorder.restore();
      }
    },
    180000,
  );
});

it.each([
  { name: "maximum-proof", maximumAssetNames: false },
  { name: "maximum-asset-names", maximumAssetNames: true },
])(
  "fits $name with 64-branch openings, maximum output and large payout datum through removal",
  async ({ name, maximumAssetNames }) => {
    await runWithdrawalMistagMaximumProof(
      (label, measurement) =>
        measurements.push({
          name: `${name}/${label}`,
          kind:
            label.startsWith("script-") ||
            label.startsWith("evidence-") ||
            label.startsWith("removal-script-")
              ? "publication"
              : "lifecycle",
          maximumShape: name,
          signedBytes: measurement.completeSignedBytes,
          memoryUnits: measurement.executionMemory,
          cpuUnits: measurement.executionSteps,
        }),
      maximumAssetNames,
    );
    completedCases.add(name);
  },
  600_000,
);
