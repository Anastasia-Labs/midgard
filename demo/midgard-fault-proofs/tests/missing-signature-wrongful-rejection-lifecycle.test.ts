import { createHash } from "node:crypto";
import { readFile } from "node:fs/promises";
import { mkdtemp, rm } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";
import { fileURLToPath } from "node:url";

import { midgardFieldCarriageBounds } from "@al-ft/midgard-core";
import { forcedVerdictSubject } from "@al-ft/midgard-sdk";
import { afterAll, describe, expect, it } from "vitest";

import {
  admitMissingSignatureForcedArtifact,
  missingSignatureForcedArtifact,
} from "../src/missing-signature/forced-artifact.js";
import * as family from "../src/missing-signature/index.js";
import { submitMissingSignatureForcedCancel } from "../src/missing-signature/submit-forced.js";
import { missingSignatureWrongfulRejectionCloses } from "../src/missing-signature/wrongful-rejection.js";
import {
  buildVanRossemFitLedger,
  type VanRossemFitMeasurement,
  writeVanRossemFitLedger,
} from "../src/proof-fit/van-rossem-fit-ledger.js";
import { submitRemoveFraudulentBlock } from "../src/remove-fraudulent-block.js";
import {
  computeFraudProofWorkflowId,
  DirectoryFraudProofWorkflowJournalStore,
  type FraudProofWorkflowIdentity,
  journalJsonDigest,
} from "../src/workflow/journal.js";
import {
  createMissingSignatureForcedFieldPrerequisite,
  unsafeCreateMissingSignatureTransactionPortForTest,
} from "../src/workflow/missing-signature.js";
import { missingSignatureObservation } from "../src/workflow/missing-signature-state.js";
import { FRAUD_PROOF_AUTHENTICATED_PUBLICATION_OBSERVER } from "../src/workflow/raw-l1-publication-observation.js";
import { publishPlainReferenceScriptUtxo } from "./support/emulator/reference-scripts.js";
import {
  network,
  setupMissingSignatureForcedScenario,
} from "./support/missing-signature-forced-scenario.js";
import { buildMissingSignatureForcedTransaction as transactionFor } from "./support/missing-signature-forced-shapes.js";
import { realBlueprintPath } from "./support/submit-init-emulator-shared.js";
const fitMeasurements: VanRossemFitMeasurement[] = [];
afterAll(async () => {
  if (process.env.MIDGARD_WRITE_FIT_LEDGER !== "1") return;
  const ledger = buildVanRossemFitLedger({
    category: "missingSignature:0000000e:wrongful-rejection:testnet",
    blueprintSha256: createHash("sha256")
      .update(await readFile(realBlueprintPath))
      .digest("hex"),
    compilerVersion: "aiken v1.1.23+5adf783",
    measurements: fitMeasurements,
  });
  await writeVanRossemFitLedger(
    fileURLToPath(
      new URL(
        "../../../docs/fault-proofs/size-plans/missing-signature-wrongful-rejection-v1-fit-ledger.json",
        import.meta.url,
      ),
    ),
    ledger,
  );
});
const setupScenario = (
  shape?: Parameters<typeof transactionFor>[0],
  signerIndex?: bigint,
) => setupMissingSignatureForcedScenario(fitMeasurements, shape, signerIndex);

describe("missingSignature wrongful rejection real lifecycle", () => {
  it("reloads the forced journal and captures each installed runner stage, refusing mutated source and witness evidence", async () => {
    const s = await setupScenario();
    const artifact = missingSignatureForcedArtifact(
      s.prepared,
      s.transactionCbor,
    );
    const directory = await mkdtemp(
      join(tmpdir(), "missing-signature-journal-"),
    );
    const identity: FraudProofWorkflowIdentity = {
      schemaVersion: "midgard-fraud-proof-workflow-identity-v1",
      deploymentFingerprint: "aa".repeat(32),
      category: "missingSignature",
      target: { kind: "state_queue_header", headerHash: s.prepared.headerHash },
    };
    const workflowId = computeFraudProofWorkflowId(identity);
    try {
      const store = new DirectoryFraudProofWorkflowJournalStore(directory);
      const common = {
        schemaVersion: "midgard-fraud-proof-workflow-journal-entry-v1" as const,
        workflowId,
        identity,
        recordedAt: "2026-09-05T00:00:00.000Z",
      };
      await store.append(
        { ...common, sequence: 0, event: { kind: "started" } },
        0,
      );
      await store.append(
        {
          ...common,
          sequence: 1,
          event: {
            kind: "prepared",
            artifact,
            artifactDigest: journalJsonDigest(artifact),
          },
        },
        1,
      );
      await expect(
        admitMissingSignatureForcedArtifact({
          ...artifact,
          headerHash: "ff".repeat(28),
        }),
      ).rejects.toThrow();
      await expect(
        admitMissingSignatureForcedArtifact({
          ...artifact,
          transactionCbor: "80",
        }),
      ).rejects.toThrow();
      await expect(
        admitMissingSignatureForcedArtifact({
          ...artifact,
          forcedSourceCbor: "80",
        }),
      ).rejects.toThrow();
      let thread = await s.init();
      for (const [position, ordinal] of ([1, 5, 6, 7] as const).entries()) {
        const recovered = await new DirectoryFraudProofWorkflowJournalStore(
          directory,
        ).load(workflowId);
        const prepared = recovered[1]!.event;
        if (prepared.kind !== "prepared")
          throw new Error("missing prepared journal entry");
        expect(
          (await admitMissingSignatureForcedArtifact(prepared.artifact))
            .transactionId,
        ).toBe(s.prepared.transactionId);
        const port = unsafeCreateMissingSignatureTransactionPortForTest({
          config: {
            lucid: s.harness.proverLucid,
            blueprint: s.harness.realBlueprint,
            network,
            signer: s.harness.proverSigner,
            headerHash: s.prepared.headerHash,
            contracts: s.harness.missingSignature,
            category: s.harness.category,
            catalogue: {
              policyId: s.harness.contracts.fraudProofCatalogue.policyId,
              spendingScriptAddress:
                s.harness.contracts.fraudProofCatalogue.spendingScriptAddress,
              root: s.harness.catalogue.root,
            },
            referenceScripts: {
              steps: [s.refs[0]!, s.refs[0]!, s.refs[0]!, s.refs[0]!],
              forced: {
                bind: s.refs[1]!,
                signer: s.refs[2]!,
                witness: s.refs[3]!,
              },
              witnesses: {
                ...s.harness.witnessReferenceScripts,
                computationThreadMint:
                  s.harness.witnessReferenceScripts.computationThreadMint!,
                fraudProofMint:
                  s.harness.witnessReferenceScripts.fraudProofMint!,
                phasMembershipWithdraw:
                  s.harness.witnessReferenceScripts.phasMembershipWithdraw!,
              },
            },
            stateQueueMutationLeaseCoordinator: {
              acquire: async () => {
                throw new Error("unexpected lease");
              },
            },
            fraudProverRewardLovelace: 0n,
            deploymentInfo: {},
          },
          builders: {
            init: family.submitMissingSignatureInit,
            step01: family.submitMissingSignatureStep01,
            step02: family.submitMissingSignatureStep02,
            step03: family.submitMissingSignatureStep03,
            step04: family.submitMissingSignatureStep04,
            remove: submitRemoveFraudulentBlock,
          },
        });
        const observation = missingSignatureObservation({
          headerHash: s.prepared.headerHash,
          provenance: {
            trustClass: "authenticated_cardano_l1",
            sourceId: "emulator",
            grade: "security",
          },
          stage: {
            kind: "step",
            step: ordinal,
            threadOutRef: thread,
            stateQueueBlockOutRef: s.block.setup.fraudulentBlockOutRef,
          },
        });
        if (observation.kind !== "action_required")
          throw new Error("missing durable action");
        const captured = await port.capture({
          action: observation.action,
          artifact: prepared.artifact,
        });
        await s.record(`journal-step-${ordinal}`, () =>
          captured.transaction.signed.submit(),
        );
        s.harness.emulator.awaitBlock(1);
        if (position < 3) {
          const target = [
            s.harness.missingSignature.forcedStep,
            s.harness.missingSignature.forcedSigner,
            s.harness.missingSignature.forcedWitness,
          ][position]!;
          const [next] = await s.harness.proverLucid.utxosAt(
            target.spendingScriptAddress,
          );
          if (next === undefined)
            throw new Error("missing recovered successor");
          thread = `${next.txHash}#${next.outputIndex}`;
        }
      }
      await s.remove();
    } finally {
      await rm(directory, { recursive: true, force: true });
    }
  }, 600_000);
  it("runs registered Init, cancels every position, restarts by out-ref, proves a real signature and removes", async () => {
    const s = await setupScenario();
    for (let position = 0; position < 4; position++) {
      let thread = await s.init();
      for (let i = 0; i < position; i++) thread = await s.advance(thread, i);
      await s.record(`cancel-${position}`, () =>
        submitMissingSignatureForcedCancel({
          lucid: s.harness.proverLucid,
          contracts: s.harness.missingSignature,
          categoryId: s.harness.category.categoryId,
          signer: s.harness.proverSigner,
          threadOutRef: thread,
          referenceScriptUtxo: s.refs[position]!,
          witnessReferenceScripts: s.harness.witnessReferenceScripts,
        }),
      );
    }
    let thread = await s.init();
    for (let i = 0; i < 3; i++) thread = await s.advance(thread, i);
    expect((await s.action(thread, 3)).kind).toBe("proven");
    await s.remove();
  }, 600_000);
  it.each([
    { witnessCount: 317, signerCount: 1, field: 3, tier: "Certified" },
    { witnessCount: 1, signerCount: 1088, field: 2, tier: "Certified" },
    { witnessCount: 146, signerCount: 1, field: 3, tier: "RawUtxo" },
    { witnessCount: 1, signerCount: 504, field: 2, tier: "RawUtxo" },
  ])(
    "proves maximum $tier field $field and removes",
    async (shape) => {
      const s = await setupScenario(shape, BigInt(shape.signerCount - 1));
      const planned = await s.certify(shape.field as 2 | 3);
      expect(planned.plan.tier).toBe(shape.tier);
      expect(planned.preimage.length).toBeLessThanOrEqual(
        midgardFieldCarriageBounds.maxTransactionAggregateFieldBytes,
      );
      let thread = await s.init();
      for (let i = 0; i < 3; i++) thread = await s.advance(thread, i);
      expect((await s.action(thread, 3)).kind).toBe("proven");
      await s.remove();
    },
    600_000,
  );
  it.each([
    { witnessCount: 317, signerCount: 1 },
    { witnessCount: 1, signerCount: 1088 },
  ])(
    "automatically publishes and certifies installed maximum $witnessCount witnesses/$signerCount signers with recovery",
    async (shape) => {
      const s = await setupScenario(shape, BigInt(shape.signerCount - 1));
      const artifact = missingSignatureForcedArtifact(
        s.prepared,
        s.transactionCbor,
      );
      const reference = await s.record("prerequisite-reference", () =>
        publishPlainReferenceScriptUtxo({
          lucid: s.harness.proverLucid,
          script: s.harness.contracts.fieldPreimageCertificate.mintingScript,
          label: "missing-signature prerequisite certificate",
        }),
      );
      const makePort = () =>
        createMissingSignatureForcedFieldPrerequisite({
          lucid: s.harness.proverLucid,
          network,
          signer: s.harness.proverSigner,
          certificate: s.harness.contracts.fieldPreimageCertificate,
          certificateReference: reference.utxo,
          transactionConfirmed: async () => true,
          publications: {
            observerVersion: FRAUD_PROOF_AUTHENTICATED_PUBLICATION_OBSERVER,
            observeExact: async (input) => {
              const found = (
                await s.harness.proverLucid.utxosAt(input.address)
              ).find(
                (u) =>
                  `${u.txHash}#${u.outputIndex}` === input.expectedOutRef &&
                  u.datum === input.expectedDatumCbor &&
                  (input.expectedUnit === undefined ||
                    u.assets[input.expectedUnit] === 1n),
              );
              return found === undefined
                ? { kind: "not_found" }
                : { kind: "confirmed", outRef: input.expectedOutRef };
            },
          },
        });
      let thread = await s.init();
      thread = await s.advance(thread, 0);
      thread = await s.advance(thread, 1);
      for (const ordinal of [6, 7] as const) {
        const observation = missingSignatureObservation({
          headerHash: s.prepared.headerHash,
          provenance: {
            trustClass: "authenticated_cardano_l1",
            sourceId: "emulator",
            grade: "security",
          },
          stage: {
            kind: "step",
            step: ordinal,
            threadOutRef: thread,
            stateQueueBlockOutRef: s.block.setup.fraudulentBlockOutRef,
          },
        });
        if (observation.kind !== "action_required")
          throw new Error("missing action");
        for (let attempt = 0; attempt < 16; attempt++) {
          const next = await makePort().inspect({
            headerHash: s.prepared.headerHash,
            baseAction: observation.action,
            artifact,
            entries: [],
          });
          if (next.kind === "satisfied" || next.kind === "not_required") break;
          if (next.kind !== "required")
            throw new Error("unexpected pending prerequisite");
          const captured = await makePort().capture({
            headerHash: s.prepared.headerHash,
            action: next.action,
            artifact,
          });
          const txHash = await s.record("installed-field-prerequisite", () =>
            captured.transaction.signed.submit(),
          );
          s.harness.emulator.awaitBlock(1);
          expect(
            (
              await makePort().reconcile({
                headerHash: s.prepared.headerHash,
                action: next.action,
                artifact,
                txHash,
                durableRecovery: JSON.parse(
                  JSON.stringify(captured.durableRecovery),
                ),
              })
            ).kind,
          ).toBe("confirmed");
        }
        const resolved = await makePort().resolveAuthenticated({
          headerHash: s.prepared.headerHash,
          action: observation.action,
          artifact,
        });
        if (resolved.requirement?.planned.plan.tier === "Certified")
          expect(resolved.certificate).toBeDefined();
        if (ordinal === 6) thread = await s.advance(thread, 2);
        else expect((await s.action(thread, 3)).kind).toBe("proven");
      }
      await s.remove();
    },
    600_000,
  );
  it("refuses an honest RequiredSignerUnsigned rejection on chain even when the prover claims a real signature", async () => {
    const s = await setupScenario({ forged: true });
    const valid = transactionFor();
    const lying = {
      ...s.prepared,
      evidence: { ...s.prepared.evidence, addrTxWits: valid.addrTxWits },
    };
    let thread = await s.init();
    for (let i = 0; i < 3; i++) thread = await s.advance(thread, i, lying);
    await expect(s.finalizeRaw(thread)).rejects.toThrow();
  }, 600_000);
  it("refuses a matching key with a forged signature", () => {
    const tx = transactionFor({ forged: true });
    const subject = forcedVerdictSubject({
      transactionId: tx.transactionId,
      sourceKey: { transactionId: "11".repeat(32), outputIndex: 0n },
      rejectionReason: { RequiredSignerUnsigned: { signer_index: 0n } },
    });
    expect(
      missingSignatureWrongfulRejectionCloses({
        subject,
        signerIndex: 0n,
        requiredSignerHashes: tx.requiredSignerHashes,
        addrTxWits: tx.addrTxWits,
      }),
    ).toBe(false);
  });
  it.each([1n, -1n])(
    "proves impossible signer coordinate %s",
    async (index) => {
      const s = await setupScenario({}, index);
      let thread = await s.init();
      for (let i = 0; i < 3; i++) thread = await s.advance(thread, i);
      expect((await s.action(thread, 3)).kind).toBe("proven");
      await s.remove();
    },
    600_000,
  );
});
