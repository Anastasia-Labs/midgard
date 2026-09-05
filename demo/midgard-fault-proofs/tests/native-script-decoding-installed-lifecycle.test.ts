import { createHash } from "node:crypto";
import { mkdtemp, readFile } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";
import { fileURLToPath } from "node:url";

import { encodeCbor } from "@al-ft/midgard-core/codec/cbor";
import { getAddressDetails, toUnit } from "@lucid-evolution/lucid";
import { afterAll,describe, expect, it } from "vitest";

import { createNativeScriptDecodingTransactionPort } from "../src/native-script-decoding/workflow.js";
import {
  buildVanRossemFitLedger,
  type VanRossemFitMeasurement,
  writeVanRossemFitLedger,
} from "../src/proof-fit/van-rossem-fit-ledger.js";
import { submitRemoveFraudulentBlock } from "../src/remove-fraudulent-block.js";
import { classifyCanonicalBlockViolations } from "../src/workflow/classification.js";
import { NATIVE_SCRIPT_DECODING_COMPLETE_CANONICAL_REPLAY } from "../src/workflow/complete-replay.js";
import { createCursorFamilyWorkflowAdapter } from "../src/workflow/cursor-family-adapter.js";
import { NATIVE_SCRIPT_DECODING_CURSOR_SPEC } from "../src/workflow/cursor-family-spec.js";
import {
  FRAUD_PROOF_FAMILY_L1_OBSERVATION_PORT,
  type FraudProofFamilyL1ObservationPort,
} from "../src/workflow/family-l1-observation.js";
import {
  computeFraudProofWorkflowId,
  DirectoryFraudProofWorkflowJournalStore,
  type FraudProofWorkflowIdentity,
  type FraudProofWorkflowJournalEvent,
  journalJsonDigest,
} from "../src/workflow/journal.js";
import { realBlueprintPath } from "./support/emulator/blueprints.js";
import { alignUnixTimeToEmulatorSlotBoundary } from "./support/emulator/emulator-context.js";
import { captureEmulatorSubmission } from "./support/emulator/measurement.js";
import { buildRemovalDeploymentInfo } from "./support/emulator/removal-deployment.js";
import {
  submitSecondHeaderTx,
  submitSetupTx,
} from "./support/emulator/setup-tx.js";
import {
  makeDecodingEmulatorHarness,
  publishDecodingReferenceScripts,
} from "./support/native-script-decoding-emulator.js";
import { nativeDecodingFixture } from "./support/native-script-decoding-retained.js";
import {
  publishPlainReferenceScriptUtxo,
  publishRemovalReferenceScripts,
} from "./support/submit-init-emulator-shared.js";

const measurements: VanRossemFitMeasurement[] = [];
afterAll(async () => {
  if (process.env.MIDGARD_WRITE_FIT_LEDGER !== "1") return;
  expect(
    measurements.some((row) => row.name.startsWith("maximum/step_05")),
  ).toBe(true);
  await writeVanRossemFitLedger(
    fileURLToPath(
      new URL(
        "../../../docs/fault-proofs/size-plans/native-script-decoding-workflow-fit-ledger.json",
        import.meta.url,
      ),
    ),
    buildVanRossemFitLedger({
      category: "nativeScriptDecoding",
      blueprintSha256: createHash("sha256")
        .update(await readFile(realBlueprintPath))
        .digest("hex"),
      compilerVersion: "aiken v1.1.23+5adf783",
      measurements,
    }),
  );
});
const maximumItem = encodeCbor([
  0n,
  Buffer.concat([Buffer.from("8109", "hex"), Buffer.alloc(16_377)]),
]);

describe("nativeScriptDecoding installed cursor actuator", () => {
  it.each([
    {
      name: "wrongful-rejection",
      direction: 1 as const,
      item: Buffer.from("820043820400", "hex"),
    },
    {
      name: "wrongful-acceptance",
      direction: 0 as const,
      item: Buffer.from("8200428109", "hex"),
    },
    { name: "maximum", direction: 0 as const, item: maximumItem },
  ])(
    "captures, journals and restarts $name through proof mint and removal",
    async (scenario) => {
      const h = await makeDecodingEmulatorHarness();
      const operator = getAddressDetails(
        await h.funderLucid.wallet().address(),
      ).paymentCredential!;
      const fixture = await nativeDecodingFixture({
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
        async () => ({
          steps: await publishDecodingReferenceScripts({
            lucid: h.proverLucid,
            contracts: h.decoding,
          }),
          certificate: (
            await publishPlainReferenceScriptUtxo({
              lucid: h.proverLucid,
              script: h.contracts.fieldPreimageCertificate.mintingScript,
              label: "decoding field certificate",
            })
          ).utxo,
        }),
      );
      const { steps, certificate } = publications.result;
      publications.measurements.forEach((m, index) =>
        measurements.push({
          name: `${scenario.name}/publish/${index}`,
          kind: "publication",
          maximumShape: "six physical validators and field certificate",
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
        deploymentInfo: buildRemovalDeploymentInfo(h.contracts, h.catalogue),
        network: "Custom",
        resolvedContracts: { category: h.category },
        releaseEconomics: { policy: { fraudProverRewardLovelace: "0" } },
      };
      const port = createNativeScriptDecodingTransactionPort({
        binding: binding as never,
        lucid: h.proverLucid,
        signer: h.proverSigner,
        contracts: h.decoding,
        references: {
          steps,
          witnesses: h.witnessReferenceScripts as Required<
            typeof h.witnessReferenceScripts
          >,
          fieldPreimageCertificateMint: certificate,
        },
        replayContext: fixture.context,
        stateQueueMutationLeaseCoordinator: leaseCoordinator,
      });
      const decision =
        await NATIVE_SCRIPT_DECODING_COMPLETE_CANONICAL_REPLAY.replay(
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
        classification.category !== "nativeScriptDecoding"
      )
        throw new Error("real fixture did not detect");
      const artifact = await port.prepare({
        evidence: fixture.evidence,
        classification: { ...classification, category: "nativeScriptDecoding" },
      });
      const identity: FraudProofWorkflowIdentity = {
        schemaVersion: "midgard-fraud-proof-workflow-identity-v1",
        deploymentFingerprint: "aa".repeat(32),
        category: "nativeScriptDecoding",
        target: { kind: "state_queue_header", headerHash: setup.headerHash },
      };
      const workflowId = computeFraudProofWorkflowId(identity);
      const directory = await mkdtemp(
        join(tmpdir(), "native-decoding-journal-"),
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
      const threadUnit = toUnit(
        h.decoding.computationThread.policyId,
        h.category.categoryId + setup.headerHash,
      );
      const proofUnit = toUnit(
        h.decoding.fraudProof.policyId,
        h.category.categoryId + setup.headerHash,
      );
      const confirmed = new Set<string>();
      const l1: FraudProofFamilyL1ObservationPort<"nativeScriptDecoding"> = {
        portVersion: FRAUD_PROOF_FAMILY_L1_OBSERVATION_PORT,
        category: "nativeScriptDecoding",
        publications: {} as never,
        observeHeader: async () => fixture.evidence.observation,
        transactionConfirmed: async ({ txHash }) => confirmed.has(txHash),
        observe: async () => {
          const provenance = {
            trustClass: "authenticated_cardano_l1",
            sourceId: "lucid-emulator",
            grade: "security",
          } as const;
          for (const [index, step] of h.decoding.steps.entries()) {
            const outputs = await h.proverLucid.utxosAtWithUnit(
              step.spendingScriptAddress,
              threadUnit,
            );
            if (outputs.length === 1)
              return {
                provenance,
                stage: {
                  kind: "step",
                  step: (index + 1) as 1 | 2 | 3 | 4 | 5 | 6,
                  threadOutRef: `${outputs[0]!.txHash}#${outputs[0]!.outputIndex}`,
                  stateQueueBlockOutRef: setup.fraudulentBlockOutRef,
                },
              };
          }
          const proofs = await h.proverLucid.utxosAtWithUnit(
            h.decoding.fraudProof.spendingScriptAddress,
            proofUnit,
          );
          if (proofs.length === 1)
            return {
              provenance,
              stage: {
                kind: "proof_token",
                fraudProofOutRef: `${proofs[0]!.txHash}#${proofs[0]!.outputIndex}`,
                stateQueueBlockOutRef: setup.fraudulentBlockOutRef,
                nextRemovalOutRef: setup.fraudulentBlockOutRef,
              },
            };
          return {
            provenance,
            stage: {
              kind: "not_started",
              stateQueueBlockOutRef: setup.fraudulentBlockOutRef,
            },
          };
        },
      };
      const adapter = () =>
        createCursorFamilyWorkflowAdapter({
          spec: NATIVE_SCRIPT_DECODING_CURSOR_SPEC,
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
        if (action.input.stage === "remove") break;
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
            maximumShape: `${scenario.item.length} raw script bytes`,
            signedBytes: m.completeSignedBytes,
            memoryUnits: m.executionMemory,
            cpuUnits: m.executionSteps,
          }),
        );
        h.emulator.awaitBlock();
        confirmed.add(preflight.txHash);
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
      }
      expect(
        await h.proverLucid.utxosAtWithUnit(
          h.decoding.fraudProof.spendingScriptAddress,
          proofUnit,
        ),
      ).toHaveLength(1);
      const removal = await publishRemovalReferenceScripts({
        lucid: h.proverLucid,
        contracts: h.contracts,
      });
      const now = BigInt(h.emulator.now());
      const removed = await captureEmulatorSubmission(h.emulator, () =>
        submitRemoveFraudulentBlock({
          lucid: h.proverLucid,
          blueprint: h.realBlueprint,
          deploymentInfo: buildRemovalDeploymentInfo(h.contracts, h.catalogue, {
            removalReferenceScripts: removal.published,
          }),
          network: "Custom",
          signer: h.proverSigner,
          fraudCategory: "nativeScriptDecoding",
          fraudulentHeaderHash: setup.headerHash,
          requireReferenceScripts: true,
          stateQueueMutationLeaseCoordinator: leaseCoordinator,
          validFrom: now > 120000n ? now - 120000n : 0n,
          validTo: now + 300000n,
        }),
      );
      removed.measurements.forEach((m, index) =>
        measurements.push({
          name: `${scenario.name}/remove/${index}`,
          kind: "lifecycle",
          maximumShape: `${scenario.item.length} raw script bytes`,
          signedBytes: m.completeSignedBytes,
          memoryUnits: m.executionMemory,
          cpuUnits: m.executionSteps,
        }),
      );
    },
    180000,
  );
});
