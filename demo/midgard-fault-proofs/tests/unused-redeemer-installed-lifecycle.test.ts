/**
 * The installed `unusedRedeemer` cursor actuator: complete replay
 * detects the fault, the port prepares a journaled artifact, and the cursor
 * adapter drives every one of the 25 roles from a genuine Init to the proof
 * token across an emulated restart after each broadcast; the block is then
 * removed. The node's own forced rejection of an admitted forced transaction,
 * which carries no redeemer, committed after the anchor that carries its
 * predecessor ledger, yields no detection.
 */
import { mkdtemp } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";

import * as SDK from "@al-ft/midgard-sdk";
import { Data, toUnit, type UTxO } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import { canonicalBlockEvidenceFromVerifiedPayload } from "../src/evidence/canonical-block-evidence.js";
import { singleLedgerAt } from "../src/ledger-at.js";
import { submitRemoveFraudulentBlock } from "../src/remove-fraudulent-block.js";
import {
  createUnusedRedeemerTransactionPort,
  type UnusedRedeemerWorkflowReferenceScripts,
} from "../src/unused-redeemer/index.js";
import { classifyCanonicalBlockViolations } from "../src/workflow/classification.js";
import {
  admitCompleteCanonicalReplayPredecessor,
  UNUSED_REDEEMER_COMPLETE_CANONICAL_REPLAY,
} from "../src/workflow/complete-replay.js";
import { createCursorFamilyWorkflowAdapter } from "../src/workflow/cursor-family-adapter.js";
import { UNUSED_REDEEMER_CURSOR_SPEC } from "../src/workflow/cursor-family-spec.js";
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
import { authenticatedHeaderObservation } from "./helpers/canonical-block-evidence-fixture.js";
import { buildRemovalDeploymentInfo } from "./support/emulator/removal-deployment.js";
import {
  honestForcedObserverShape,
  machineForcedVerdict,
  plutusScript,
  setupMissingRedeemerScenario,
} from "./support/missing-redeemer-emulator.subject.js";
import { scriptSourceMissingVerdict } from "./support/missing-script-source-emulator.js";
import {
  publishPlainReferenceScriptUtxo,
  publishRemovalReferenceScripts,
} from "./support/submit-init-emulator-shared.js";
import {
  makeUnusedRedeemerHarness,
  publishUnusedRedeemerRoles,
  type UnusedRedeemerHarness,
  unusedRedeemerVerdict,
} from "./support/unused-redeemer-emulator.js";

const daProvenance = {
  trustClass: "public_or_permissionless_da",
  sourceId: "test/unused-redeemer-retained-da",
  grade: "security",
} as const;

const leaseCoordinator = {
  acquire: async () => ({
    token: "unused-redeemer-lease",
    source: "emulator",
    renew: async () => {},
    release: async () => {},
    fail: async () => {},
  }),
};

const blockEvidence = async (
  scenario: Awaited<ReturnType<typeof setupMissingRedeemerScenario>>,
) =>
  await canonicalBlockEvidenceFromVerifiedPayload({
    observation: authenticatedHeaderObservation({
      header: scenario.block.header,
      headerHash: scenario.setup.headerHash,
    }),
    payloadEnvelopeCbor: scenario.block.reconstruction.payloadEnvelopeCbor,
    daProvenance,
  });

const workflowReferences = async (
  h: UnusedRedeemerHarness,
): Promise<UnusedRedeemerWorkflowReferenceScripts> => {
  const published = await publishUnusedRedeemerRoles(h);
  const steps = SDK.UNUSED_REDEEMER_ROLE_ORDER.map(
    (role) => published.get(role)!.utxo,
  ) as unknown as UnusedRedeemerWorkflowReferenceScripts["steps"];
  const certificate = await publishPlainReferenceScriptUtxo({
    lucid: h.proverLucid,
    script: h.contracts.fieldPreimageCertificate.mintingScript,
    label: "unused-redeemer field certificate",
  });
  return {
    steps,
    witnesses:
      h.witnessReferenceScripts as UnusedRedeemerWorkflowReferenceScripts["witnesses"],
    fieldPreimageCertificateMint: certificate.utxo,
  };
};

describe("unusedRedeemer installed cursor actuator", () => {
  it.each([
    {
      name: "accepted redeemer with no purpose",
      shape: {
        observers: [{ script: plutusScript(1), redeemer: true }],
        paddingRedeemers: [Buffer.from(Data.to(0n), "hex")],
      },
      source: { kind: "normal" as const },
    },
    {
      name: "forced UnusedRedeemer past an empty redeemer field",
      shape: { observers: [{ script: plutusScript(3), redeemer: false }] },
      source: {
        kind: "forced" as const,
        verdict: unusedRedeemerVerdict(0n),
      },
    },
  ])(
    "captures, journals and restarts the $name through proof mint and removal",
    async (scenarioCase) => {
      const h = await makeUnusedRedeemerHarness();
      const scenario = await setupMissingRedeemerScenario({
        harness: h,
        shape: scenarioCase.shape,
        source: scenarioCase.source,
      });
      const { setup } = scenario;
      const evidence = await blockEvidence(scenario);
      const references = await workflowReferences(h);
      const binding = {
        definition: { headerHash: setup.headerHash },
        blueprint: h.realBlueprint,
        deploymentInfo: buildRemovalDeploymentInfo(h.contracts, h.catalogue),
        network: "Custom",
        resolvedContracts: { category: h.category },
        releaseEconomics: { policy: { fraudProverRewardLovelace: "0" } },
      };
      const port = createUnusedRedeemerTransactionPort({
        binding: binding as never,
        lucid: h.proverLucid,
        signer: h.proverSigner,
        contracts: h.unusedRedeemer,
        references,
        stateQueueMutationLeaseCoordinator: leaseCoordinator,
      });
      const decision =
        await UNUSED_REDEEMER_COMPLETE_CANONICAL_REPLAY.replay(evidence);
      const classification = await classifyCanonicalBlockViolations({
        evidence,
        detections: decision.detections,
        minimumConfirmationDepth: 1,
      });
      if (
        classification.decision !== "fault_detected" ||
        classification.category !== "unusedRedeemer"
      )
        throw new Error("unused-redeemer block did not detect");
      const artifact = await port.prepare({
        evidence,
        classification: { ...classification, category: "unusedRedeemer" },
      });
      const identity: FraudProofWorkflowIdentity = {
        schemaVersion: "midgard-fraud-proof-workflow-identity-v1",
        deploymentFingerprint: "aa".repeat(32),
        category: "unusedRedeemer",
        target: { kind: "state_queue_header", headerHash: setup.headerHash },
      };
      const workflowId = computeFraudProofWorkflowId(identity);
      const directory = await mkdtemp(
        join(tmpdir(), "unused-redeemer-journal-"),
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
        h.unusedRedeemer.computationThread.policyId,
        h.category.categoryId + setup.headerHash,
      );
      const proofUnit = toUnit(
        h.unusedRedeemer.fraudProof.policyId,
        h.category.categoryId + setup.headerHash,
      );
      const outRef = (utxo: UTxO) =>
        `${utxo.txHash}#${utxo.outputIndex.toString()}`;
      const confirmed = new Set<string>();
      const l1: FraudProofFamilyL1ObservationPort<"unusedRedeemer"> = {
        portVersion: FRAUD_PROOF_FAMILY_L1_OBSERVATION_PORT,
        category: "unusedRedeemer",
        publications: {} as never,
        observeHeader: async () => evidence.observation,
        transactionConfirmed: async ({ txHash }) => confirmed.has(txHash),
        observe: async () => {
          const provenance = {
            trustClass: "authenticated_cardano_l1",
            sourceId: "lucid-emulator",
            grade: "security",
          } as const;
          for (const [index, step] of h.unusedRedeemer.steps.entries()) {
            const outputs = await h.proverLucid.utxosAtWithUnit(
              step.spendingScriptAddress,
              threadUnit,
            );
            if (outputs.length === 1)
              return {
                provenance,
                stage: {
                  kind: "step",
                  step: index + 1,
                  threadOutRef: outRef(outputs[0]!),
                  stateQueueBlockOutRef: setup.fraudulentBlockOutRef,
                },
              } as never;
          }
          const proofs = await h.proverLucid.utxosAtWithUnit(
            h.unusedRedeemer.fraudProof.spendingScriptAddress,
            proofUnit,
          );
          if (proofs.length === 1)
            return {
              provenance,
              stage: {
                kind: "proof_token",
                fraudProofOutRef: outRef(proofs[0]!),
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
          spec: UNUSED_REDEEMER_CURSOR_SPEC,
          l1,
          transactions: port,
          stateQueueMutationLeaseCoordinator: leaseCoordinator,
        });
      const stages: string[] = [];
      for (let n = 0; n < 120; n++) {
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
        stages.push(String(action.input.stage));
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
        await current.submit({ ...context, action, preflight });
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
      console.log(`UR installed ${scenarioCase.name} stages`, stages.join(","));
      expect(stages[0]).toBe("init");
      expect(stages.at(-2)).toBe("step_25");
      expect(stages.at(-1)).toBe("remove");
      expect(
        await h.proverLucid.utxosAtWithUnit(
          h.unusedRedeemer.fraudProof.spendingScriptAddress,
          proofUnit,
        ),
      ).toHaveLength(1);
      const removal = await publishRemovalReferenceScripts({
        lucid: h.proverLucid,
        contracts: h.contracts,
      });
      const now = BigInt(h.emulator.now());
      const removed = await submitRemoveFraudulentBlock({
        lucid: h.proverLucid,
        blueprint: h.realBlueprint,
        deploymentInfo: buildRemovalDeploymentInfo(h.contracts, h.catalogue, {
          removalReferenceScripts: removal.published,
        }),
        network: "Custom",
        signer: h.proverSigner,
        fraudCategory: "unusedRedeemer",
        fraudulentHeaderHash: setup.headerHash,
        requireReferenceScripts: true,
        stateQueueMutationLeaseCoordinator: leaseCoordinator,
        validFrom: now > 120000n ? now - 120000n : 0n,
        validTo: now + 300000n,
      });
      expect(removed.fraudCategory).toBe("unusedRedeemer");
    },
    1_800_000,
  );

  it("detects nothing in the node's own rejection of an admitted forced transaction", async () => {
    const h = await makeUnusedRedeemerHarness();
    const shape = {
      ...honestForcedObserverShape(5),
      observers: [{ script: plutusScript(5), witness: false, redeemer: false }],
      reference: [],
    };
    const verdict = scriptSourceMissingVerdict(2n, 0n);
    await expect(machineForcedVerdict(shape)).resolves.toStrictEqual(verdict);
    const scenario = await setupMissingRedeemerScenario({
      harness: h,
      shape,
      source: { kind: "forced", verdict },
    });
    const evidence = await blockEvidence(scenario);
    const decision = await UNUSED_REDEEMER_COMPLETE_CANONICAL_REPLAY.replay(
      evidence,
      {
        predecessor: await admitCompleteCanonicalReplayPredecessor({
          ledgerAt: singleLedgerAt(evidence.header.prevHeaderHash, [
            ...(shape.spend ?? []),
            ...(shape.reference ?? []),
          ]),
          currentEvidence: evidence,
        }),
      },
    );
    expect(decision.detections).toStrictEqual([]);
  }, 600_000);
});
