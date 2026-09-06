import { createHash } from "node:crypto";
import { mkdtemp, readFile, writeFile } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";
import { setImmediate } from "node:timers/promises";
import { fileURLToPath } from "node:url";
import { inspect } from "node:util";

import {
  computeHash28,
  decodeMidgardNativeScript,
  encodeMidgardFieldPreimage,
  encodeMidgardVersionedScript,
} from "@al-ft/midgard-core";
import { MintAuthorizationStep02ThreadDatum } from "@al-ft/midgard-sdk";
import { Data, getAddressDetails, toUnit } from "@lucid-evolution/lucid";
import { afterAll, describe, expect, it } from "vitest";

import { canonicalBlockEvidenceFromVerifiedPayload } from "../src/evidence/canonical-block-evidence.js";
import { admitMintAuthorizationWorkflowArtifact } from "../src/mint-authorization/artifact.js";
import { submitMintAuthorizationCancel } from "../src/mint-authorization/submit-mint-authorization-cancel.js";
import {
  createMintAuthorizationTransactionPort,
  mintAuthorizationWorkflowFieldRequirement,
  mintAuthorizationWorkflowRawRequirement,
} from "../src/mint-authorization/workflow.js";
import {
  buildVanRossemFitLedger,
  type VanRossemFitLedger,
  type VanRossemFitMeasurement,
  writeVanRossemFitLedger,
} from "../src/proof-fit/van-rossem-fit-ledger.js";
import { submitRemoveFraudulentBlock } from "../src/remove-fraudulent-block.js";
import { classifyCanonicalBlockViolations } from "../src/workflow/classification.js";
import { MINT_AUTHORIZATION_COMPLETE_CANONICAL_REPLAY } from "../src/workflow/complete-replay.js";
import { createCursorFamilyWorkflowAdapter } from "../src/workflow/cursor-family-adapter.js";
import { MINT_AUTHORIZATION_CURSOR_SPEC } from "../src/workflow/cursor-family-spec.js";
import {
  FRAUD_PROOF_FAMILY_L1_OBSERVATION_PORT,
  type FraudProofFamilyL1ObservationPort,
} from "../src/workflow/family-l1-observation.js";
import {
  createAuthenticatedFieldCarriagePrerequisitePort,
  withFieldCarriagePrerequisite,
} from "../src/workflow/field-carriage-prerequisite.js";
import {
  computeFraudProofWorkflowId,
  DirectoryFraudProofWorkflowJournalStore,
  type FraudProofWorkflowIdentity,
  type FraudProofWorkflowJournalEvent,
  journalJsonDigest,
  MemoryFraudProofWorkflowJournalStore,
} from "../src/workflow/journal.js";
import {
  createAuthenticatedRawDatumPreimagePrerequisitePort,
  withRawDatumPreimagePrerequisite,
} from "../src/workflow/raw-datum-preimage-prerequisite.js";
import type { FraudProofAuthenticatedPublicationObserver } from "../src/workflow/raw-l1-publication-observation.js";
import { FRAUD_PROOF_AUTHENTICATED_PUBLICATION_OBSERVER } from "../src/workflow/raw-l1-publication-observation.js";
import { realBlueprintPath } from "./support/emulator/blueprints.js";
import { alignUnixTimeToEmulatorSlotBoundary } from "./support/emulator/emulator-context.js";
import { captureEmulatorSubmission } from "./support/emulator/measurement.js";
import { publishFaultProofWitnessReferenceScripts } from "./support/emulator/reference-scripts.js";
import { buildRemovalDeploymentInfo } from "./support/emulator/removal-deployment.js";
import {
  submitSecondHeaderTx,
  submitSetupTx,
} from "./support/emulator/setup-tx.js";
import {
  makeMintAuthorizationEmulatorHarness,
  publishMintAuthorizationReferenceScripts,
} from "./support/mint-authorization-emulator.js";
import {
  addressWitnessItemCbors,
  buildMintAuthorizationSubject,
  directionBNativeScript,
  mintItemCborV1,
  setupMintAuthorizationScenario,
} from "./support/mint-authorization-emulator.js";
import {
  mintAuthorizationMaximumMintField,
  mintAuthorizationMaximumSelectedAssets,
  mintAuthorizationMaximumWitnessField,
} from "./support/mint-authorization-maxima.js";
import { runMintAuthorizationProofMaximum } from "./support/mint-authorization-proof-maximum.js";
import { mintAuthorizationRetainedReferenceFixture } from "./support/mint-authorization-retained-reference.js";
import {
  publishPlainReferenceScriptUtxo,
  publishRemovalReferenceScripts,
} from "./support/submit-init-emulator-shared.js";

const measurements: VanRossemFitMeasurement[] = [];
afterAll(async () => {
  if (process.env.MIDGARD_WRITE_FIT_LEDGER !== "1") return;
  expect(measurements.length).toBeGreaterThan(0);
  const path = fileURLToPath(
    new URL(
      "../../../docs/fault-proofs/size-plans/mint-authorization-workflow-fit-ledger.json",
      import.meta.url,
    ),
  );
  const blueprintSha256 = createHash("sha256")
    .update(await readFile(realBlueprintPath))
    .digest("hex");
  const checkpointPath =
    process.env.MIDGARD_FIT_MEASUREMENT_CHECKPOINT ??
    join(tmpdir(), `mint-authorization-measurements-${process.pid}.json`);
  await writeFile(
    checkpointPath,
    JSON.stringify(
      {
        diagnosticOnly: true,
        category: "mintAuthorization",
        blueprintSha256,
        compilerVersion: "aiken v1.1.23+5adf783",
        measurements,
      },
      (_key, value) => (typeof value === "bigint" ? value.toString() : value),
      2,
    ) + "\n",
  );
  process.stderr.write(`Diagnostic measured transactions: ${checkpointPath}\n`);
  // A filtered rerun replaces its whole named scenario. Other scenarios may
  // be retained only from the exact current blueprint; the verifier below
  // independently requires the complete named lifecycle surface.
  let previous: VanRossemFitLedger | undefined;
  try {
    previous = JSON.parse(await readFile(path, "utf8")) as VanRossemFitLedger;
  } catch (cause) {
    if (!(cause instanceof Error && "code" in cause && cause.code === "ENOENT"))
      throw cause;
  }
  const replaced = new Set(measurements.map((row) => row.name.split("/")[0]));
  const retained =
    previous?.blueprintSha256 === blueprintSha256
      ? previous.entries
          .filter((row) => !replaced.has(row.name.split("/")[0]))
          .map((row) => ({
            name: row.name,
            kind: row.kind,
            maximumShape: row.maximumShape,
            signedBytes: row.signedBytes,
            memoryUnits: BigInt(row.memoryUnits),
            cpuUnits: BigInt(row.cpuUnits),
          }))
      : [];
  await writeVanRossemFitLedger(
    path,
    buildVanRossemFitLedger({
      category: "mintAuthorization",
      blueprintSha256,
      compilerVersion: "aiken v1.1.23+5adf783",
      measurements: [...retained, ...measurements],
    }),
  );
});
describe("mintAuthorization installed cursor actuator", () => {
  it.each([
    "absent-mint",
    "absent-burn",
    "unsatisfied-mint",
    "unsatisfied-burn",
    "maximum-signers",
    "maximum-mint-tail",
    "maximum-witnesses",
    "maximum-native-wide",
    "maximum-native-deep",
    "maximum-native-signers",
    "reference-absent",
    "reference-unsatisfied",
    "maximum-reference-tail",
    "cancel-witness-scan",
    "cancel-mint-scan",
    "maximum-proofs",
    "maximum-selected-assets",
  ])(
    "captures and restarts %s through mint and removal",
    async (name) => {
      if (name === "maximum-selected-assets") {
        measurements.push(
          ...(await runMintAuthorizationProofMaximum(
            mintAuthorizationMaximumSelectedAssets(),
            name,
          )),
        );
        return;
      }
      if (name === "maximum-proofs") {
        measurements.push(...(await runMintAuthorizationProofMaximum()));
        return;
      }
      const present =
        !name.startsWith("absent") && name !== "cancel-witness-scan";
      const scenario = { name, item: Buffer.alloc(0) };
      const h = await makeMintAuthorizationEmulatorHarness();
      const nativePayload =
        name === "maximum-native-signers"
          ? Buffer.from(
              "8202990405" +
                ("8200581c" + "cc".repeat(28)).repeat(1023) +
                "820280".repeat(6),
              "hex",
            )
          : name === "maximum-native-wide"
            ? Buffer.from("8202992aa6" + "820280".repeat(10918), "hex")
            : name === "maximum-native-deep"
              ? Buffer.from("820281".repeat(10918) + "8204190100", "hex")
              : Buffer.from(directionBNativeScript().scriptBytesHex, "hex");
      const policyId = computeHash28(
        Buffer.concat([Buffer.from([0]), nativePayload]),
      );
      const policy = {
        script: decodeMidgardNativeScript(nativePayload).script,
        scriptBytesHex: nativePayload.toString("hex"),
        mintItemCbor: mintItemCborV1({
          policyId,
          assetName: Buffer.from("beef", "hex"),
          quantity: name.endsWith("burn") ? -1n : 1n,
        }),
      };
      const script = encodeMidgardVersionedScript({
        language: "NativeCardano",
        nativeScript: policy.script,
        scriptBytes: Buffer.from(policy.scriptBytesHex, "hex"),
      });
      const maximumMint =
        name === "maximum-mint-tail" || name === "cancel-mint-scan"
          ? mintAuthorizationMaximumMintField()
          : undefined;
      const maximumWitnesses =
        name === "maximum-witnesses"
          ? mintAuthorizationMaximumWitnessField()
          : undefined;
      const subject = buildMintAuthorizationSubject({
        mintItemCbors: [policy.mintItemCbor],
        scriptWitnessItemCbors: present ? [script.toString("hex")] : [],
        addrWitnessItemCbors: addressWitnessItemCbors(
          name === "maximum-signers" || name === "maximum-native-signers"
            ? 318
            : 0,
        ),
        ...maximumMint,
        ...maximumWitnesses,
      });
      if (maximumWitnesses !== undefined)
        expect(
          encodeMidgardFieldPreimage(
            maximumWitnesses.scriptWitnessItemCbors.map((item) =>
              Buffer.from(item, "hex"),
            ),
          ).length,
        ).toBe(32768);
      if (name.startsWith("maximum-native"))
        expect(encodeMidgardFieldPreimage([script]).length).toBe(32768);
      scenario.item = nativePayload;
      const { setup, fixture } = await (async () => {
        if (
          name.startsWith("reference-") ||
          name === "maximum-reference-tail"
        ) {
          const operatorVkey = getAddressDetails(
            await h.funderLucid.wallet().address(),
          ).paymentCredential!.hash;
          const fixture = await mintAuthorizationRetainedReferenceFixture({
            now:
              alignUnixTimeToEmulatorSlotBoundary(
                h.funderLucid,
                h.emulator.now() + 120_000,
              ) - 1,
            operatorVkey,
            present: name === "reference-unsatisfied",
            count: name === "maximum-reference-tail" ? 819 : 1,
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
          return {
            fixture,
            setup: {
              headerHash: committed.headerHash,
              fraudulentBlockOutRef: committed.blockOutRef,
            },
          };
        }
        const prepared = await setupMintAuthorizationScenario({
          harness: h,
          subject,
        });
        const { setup, block } = prepared;
        const evidence = await canonicalBlockEvidenceFromVerifiedPayload({
          observation: {
            schemaVersion: "midgard-canonical-evidence-source-v1",
            sourceMode: "local_node",
            provenance: {
              trustClass: "authenticated_cardano_l1",
              sourceId: "lucid-emulator",
              grade: "security",
            },
            chainPoint: { slot: 4242n, blockHash: "11".repeat(32) },
            confirmationDepth: 30,
            headerHash: block.headerHash,
            header: block.header,
          },
          payloadEnvelopeCbor: block.payloadEnvelopeCbor,
          daProvenance: {
            trustClass: "public_or_permissionless_da",
            sourceId: "retained-mint-test",
            grade: "security",
          },
        });
        return { setup, fixture: { evidence, context: undefined } };
      })();
      const publications = await captureEmulatorSubmission(
        h.emulator,
        async () => ({
          witnesses: await publishFaultProofWitnessReferenceScripts({
            lucid: h.proverLucid,
            realBlueprint: h.realBlueprint,
            includeChunkedVerify: true,
            includePexcludes: true,
            computationThreadMintingScript:
              h.family.computationThread.mintingScript,
            fraudProofMintingScript: h.family.fraudProof.mintingScript,
          }),
          steps: await publishMintAuthorizationReferenceScripts({
            lucid: h.proverLucid,
            contracts: h.family,
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
      const { steps, certificate, witnesses } = publications.result;
      publications.measurements.forEach((m, index) =>
        measurements.push({
          name: `${scenario.name}/publish/${index}`,
          kind: "publication",
          maximumShape:
            "seven physical validators, five witnesses and field certificate",
          signedBytes: m.completeSignedBytes,
          memoryUnits: m.executionMemory,
          cpuUnits: m.executionSteps,
        }),
      );
      // Publish the complete deployment before its native auth timelock expires.
      const removalPublication = await captureEmulatorSubmission(
        h.emulator,
        () =>
          publishRemovalReferenceScripts({
            lucid: h.proverLucid,
            contracts: h.contracts,
          }),
      );
      removalPublication.measurements.forEach((m, index) =>
        measurements.push({
          name: `${scenario.name}/publish-removal/${index}`,
          kind: "publication",
          maximumShape: "state queue removal reference scripts",
          signedBytes: m.completeSignedBytes,
          memoryUnits: m.executionMemory,
          cpuUnits: m.executionSteps,
        }),
      );
      const removal = removalPublication.result;
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
      const publicationObserver: FraudProofAuthenticatedPublicationObserver = {
        observerVersion: FRAUD_PROOF_AUTHENTICATED_PUBLICATION_OBSERVER,
        observeExact: async (input) => {
          const found = (await h.proverLucid.utxosAt(input.address)).find(
            (utxo) =>
              `${utxo.txHash}#${utxo.outputIndex}` === input.expectedOutRef &&
              utxo.datum === input.expectedDatumCbor &&
              utxo.scriptRef == null &&
              (input.expectedUnit === undefined ||
                utxo.assets[input.expectedUnit] === 1n) &&
              Object.keys(utxo.assets).every(
                (unit) => unit === "lovelace" || unit === input.expectedUnit,
              ),
          );
          return found === undefined
            ? { kind: "not_found" as const }
            : { kind: "confirmed" as const, outRef: input.expectedOutRef };
        },
      };
      const rawPrerequisite =
        createAuthenticatedRawDatumPreimagePrerequisitePort({
          category: "mintAuthorization",
          lucid: h.proverLucid,
          network: "Custom",
          signer: h.proverSigner,
          publications: publicationObserver,
          requirementForAction: async ({ action, artifact }) => {
            if (
              !["step_02", "step_03", "step_06", "step_07"].includes(
                String(action.input.stage),
              )
            )
              return null;
            return mintAuthorizationWorkflowRawRequirement(
              await admitMintAuthorizationWorkflowArtifact(artifact),
              action.input.stage,
            );
          },
          transactionConfirmed: async () => false,
        });
      const fieldPrerequisite =
        createAuthenticatedFieldCarriagePrerequisitePort({
          category: "mintAuthorization",
          lucid: h.proverLucid,
          network: "Custom",
          signer: h.proverSigner,
          publications: publicationObserver,
          requirementForAction: async ({ action, artifact }) => {
            if (
              !["step_02", "step_03", "step_04"].includes(
                String(action.input.stage),
              )
            )
              return null;
            return mintAuthorizationWorkflowFieldRequirement(
              await admitMintAuthorizationWorkflowArtifact(artifact),
              h.proverSigner.paymentKeyHash,
              action.input.stage,
              {
                policyId: h.contracts.fieldPreimageCertificate.policyId,
                mintingScript:
                  h.contracts.fieldPreimageCertificate.mintingScript,
                referenceScriptUtxo: certificate,
              },
            );
          },
          transactionConfirmed: async () => false,
        });
      const port = createMintAuthorizationTransactionPort(
        {
          binding: binding as never,
          lucid: h.proverLucid,
          signer: h.proverSigner,
          contracts: h.family,
          references: {
            steps,
            witnesses: witnesses as Required<typeof h.witnessReferenceScripts>,
            fieldPreimageCertificateMint: certificate,
          },
          replayContext: fixture.context,
          stateQueueMutationLeaseCoordinator: leaseCoordinator,
        },
        rawPrerequisite,
      );
      const decision =
        await MINT_AUTHORIZATION_COMPLETE_CANONICAL_REPLAY.replay(
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
        classification.category !== "mintAuthorization"
      )
        throw new Error("real fixture did not detect");
      if (maximumMint !== undefined)
        expect(classification.selected.detectionId).toContain(
          `:${maximumMint.targetPolicyIndex}`,
        );
      const artifact = await port.prepare({
        evidence: fixture.evidence,
        classification: { ...classification, category: "mintAuthorization" },
      });
      const identity: FraudProofWorkflowIdentity = {
        schemaVersion: "midgard-fraud-proof-workflow-identity-v1",
        deploymentFingerprint: "aa".repeat(32),
        category: "mintAuthorization",
        target: { kind: "state_queue_header", headerHash: setup.headerHash },
      };
      const workflowId = computeFraudProofWorkflowId(identity);
      const directory = await mkdtemp(
        join(tmpdir(), "mint-authorization-journal-"),
      );
      // Native maxima execute thousands of real cursor actions. The bounded
      // polarity/signer cases above reopen the fsynced directory journal at
      // every boundary; the same adapter protocol uses the validated memory
      // journal here to keep filesystem O(N²) history reads out of fit tests.
      const memoryJournal =
        name.startsWith("maximum-native") ||
        name === "maximum-witnesses" ||
        name === "maximum-reference-tail"
          ? new MemoryFraudProofWorkflowJournalStore()
          : undefined;
      const journal = () =>
        memoryJournal ?? new DirectoryFraudProofWorkflowJournalStore(directory);
      const append = async (event: FraudProofWorkflowJournalEvent) => {
        const store = journal();
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
        h.family.computationThread.policyId,
        h.category.categoryId + setup.headerHash,
      );
      const proofUnit = toUnit(
        h.family.fraudProof.policyId,
        h.category.categoryId + setup.headerHash,
      );
      const confirmed = new Set<string>();
      const l1: FraudProofFamilyL1ObservationPort<"mintAuthorization"> = {
        portVersion: FRAUD_PROOF_FAMILY_L1_OBSERVATION_PORT,
        category: "mintAuthorization",
        publications: {} as never,
        observeHeader: async () => fixture.evidence.observation,
        transactionConfirmed: async ({ txHash }) => confirmed.has(txHash),
        observe: async () => {
          const provenance = {
            trustClass: "authenticated_cardano_l1",
            sourceId: "lucid-emulator",
            grade: "security",
          } as const;
          for (const [index, step] of h.family.steps.entries()) {
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
            h.family.fraudProof.spendingScriptAddress,
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
        withRawDatumPreimagePrerequisite({
          category: "mintAuthorization",
          prerequisite: rawPrerequisite,
          base: withFieldCarriagePrerequisite({
            category: "mintAuthorization",
            prerequisite: fieldPrerequisite,
            base: createCursorFamilyWorkflowAdapter({
              spec: MINT_AUTHORIZATION_CURSOR_SPEC,
              l1,
              transactions: port,
              stateQueueMutationLeaseCoordinator: leaseCoordinator,
            }),
          }),
        });
      for (let n = 0; n < 3000; n++) {
        await setImmediate();
        const store = journal();
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
        if (n % 200 === 0 && name.startsWith("maximum"))
          process.stderr.write(`${name}: cursor action ${n}\n`);
        const current = adapter();
        const observation = await current.observe(context);
        if (observation.kind !== "action_required")
          throw new Error("missing action");
        const action = observation.action;
        if (name === "absent-mint" && n === 0) {
          await expect(
            current.preflight({
              ...context,
              action,
              artifact: { ...restored.artifact, headerHash: "00".repeat(28) },
            }),
          ).rejects.toThrow();
          await expect(
            current.preflight({
              ...context,
              action,
              artifact: {
                ...restored.artifact,
                coordinate: { sourceIndex: 0, policyIndex: "1" },
              },
            }),
          ).rejects.toThrow();
        }
        if (action.input.stage === "remove") break;
        let cancelMintScan = false;
        if (name === "cancel-mint-scan" && action.input.stage === "step_02") {
          const [current] = await h.proverLucid.utxosAtWithUnit(
            h.family.steps[1].spendingScriptAddress,
            threadUnit,
          );
          const datum = Data.from(
            current!.datum!,
            MintAuthorizationStep02ThreadDatum,
          );
          cancelMintScan = datum.data !== null && "Scan" in datum.data;
        }
        if (
          (name === "cancel-witness-scan" &&
            action.input.stage === "step_07") ||
          cancelMintScan
        ) {
          const cancelStep = cancelMintScan ? 1 : 6;
          const [thread] = await h.proverLucid.utxosAtWithUnit(
            h.family.steps[cancelStep].spendingScriptAddress,
            threadUnit,
          );
          expect(thread).toBeDefined();
          const cancelled = await captureEmulatorSubmission(h.emulator, () =>
            submitMintAuthorizationCancel({
              lucid: h.proverLucid,
              contracts: h.family,
              categoryId: h.category.categoryId,
              signer: h.proverSigner,
              threadOutRef: `${thread!.txHash}#${thread!.outputIndex}`,
              referenceScriptUtxo: steps[cancelStep],
              witnessReferenceScripts: witnesses,
            }),
          );
          expect(cancelled.result.cancelledStepIndex).toBe(cancelStep);
          expect(
            await h.proverLucid.utxosAtWithUnit(
              h.family.steps[cancelStep].spendingScriptAddress,
              threadUnit,
            ),
          ).toHaveLength(0);
          expect(
            await h.proverLucid.utxosAtWithUnit(
              h.family.fraudProof.spendingScriptAddress,
              proofUnit,
            ),
          ).toHaveLength(0);
          cancelled.measurements.forEach((m, index) =>
            measurements.push({
              name: `${name}/cancel/${index}`,
              kind: "lifecycle",
              maximumShape: cancelMintScan
                ? "authenticated bounded mint scan cancellation"
                : "authenticated staged witness scan cancellation",
              signedBytes: m.completeSignedBytes,
              memoryUnits: m.executionMemory,
              cpuUnits: m.executionSteps,
            }),
          );
          return;
        }
        const preflight = await current
          .preflight({ ...context, action })
          .catch((cause: unknown) => {
            throw new Error(
              `${String(action.input.stage)}: ${inspect(cause, { depth: 15 })}`,
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
          attempt: 1,
          txHash: preflight.txHash,
        });
        const result = await captureEmulatorSubmission(h.emulator, () =>
          current.submit({ ...context, action, preflight }),
        );
        result.measurements.forEach((m, index) =>
          measurements.push({
            name: `${scenario.name}/${String(action.input.stage)}/${n}/${index}`,
            kind: String(action.input.stage).startsWith("publish_")
              ? "publication"
              : "lifecycle",
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
          durableRecovery: preflight.durableRecovery,
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
          h.family.fraudProof.spendingScriptAddress,
          proofUnit,
        ),
      ).toHaveLength(1);
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
          fraudCategory: "mintAuthorization",
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
    1_800_000,
  );
});
