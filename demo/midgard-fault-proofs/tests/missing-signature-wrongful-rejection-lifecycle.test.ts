import {
  ledgerEd25519SignatureReady,
  midgardFieldCarriageBounds,
} from "@al-ft/midgard-core";
import { beforeAll, beforeEach, describe, expect, it, vi } from "vitest";

import {
  admitMissingSignatureForcedArtifact,
  missingSignatureForcedArtifact,
} from "../src/missing-signature/forced-artifact.js";
import * as family from "../src/missing-signature/index.js";
import { buildMissingSignatureForcedCancel } from "../src/missing-signature/submit-forced.js";
import { buildRemoveFraudulentBlock } from "../src/remove-fraudulent-block.build-remove-fraudulent-block.js";
import { unsafeCreateMissingSignatureTransactionPortForTest } from "../src/workflow/missing-signature.create-manifest-bound-missing-signature-workflow.js";
import { createMissingSignatureFieldPrerequisite } from "../src/workflow/missing-signature.create-missing-signature-field-prerequisite.js";
import { missingSignatureObservation } from "../src/workflow/missing-signature-state.js";
import { FRAUD_PROOF_AUTHENTICATED_PUBLICATION_OBSERVER } from "../src/workflow/raw-l1-publication-observation.js";
import {
  carriageNeedsByField,
  carriagePlanRecord,
  publishedCoverage,
  resetCarriagePlanRecord,
} from "./support/carriage-plan-parity.js";
import { publishPlainReferenceScriptUtxo } from "./support/emulator/reference-scripts.js";
import {
  network,
  setupMissingSignatureForcedScenario,
} from "./support/missing-signature-forced-scenario.js";
import { buildMissingSignatureForcedTransaction as transactionFor } from "./support/missing-signature-forced-shapes.js";
import { TEST_PROOF_MATERIALS_HOLDER } from "./support/proof-materials-holder.js";
import { buildAndSubmit } from "./support/submit-steps.js";

// The ledger's Ed25519 check is libsodium's, which initialises
// asynchronously; production entry points await it, and so does this file.
beforeAll(() => ledgerEd25519SignatureReady());
// Records each field requirement the prerequisite returns and each carriage
// plan a builder asks to publish; every call passes through unchanged.
vi.mock("../src/field-opening.js", async (importOriginal) =>
  (await import("./support/carriage-plan-parity.js")).recordingFieldOpening(
    await importOriginal(),
  ),
);
vi.mock(
  "../src/workflow/field-carriage-prerequisite.js",
  async (importOriginal) =>
    (
      await import("./support/carriage-plan-parity.js")
    ).recordingFieldCarriagePrerequisite(await importOriginal()),
);
beforeEach(resetCarriagePlanRecord);
const setupScenario = (
  shape?: Parameters<typeof transactionFor>[0],
  signerIndex?: bigint,
) => setupMissingSignatureForcedScenario(shape, signerIndex);

describe("missingSignature wrongful rejection real lifecycle", () => {
  it("restores the forced artifact and captures each installed runner stage across restarts, refusing mutated source and witness evidence", async () => {
    const s = await setupScenario();
    const artifact = missingSignatureForcedArtifact(
      s.prepared,
      s.transactionCbor,
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
      // Each stage starts from a fresh port and the artifact as the
      // evidence prepares it again.
      const prepared = { artifact: JSON.parse(JSON.stringify(artifact)) };
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
              fraudProofMint: s.harness.witnessReferenceScripts.fraudProofMint!,
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
          init: family.buildMissingSignatureInit,
          step01: family.buildMissingSignatureStep01,
          step02: family.buildMissingSignatureStep02,
          step03: family.buildMissingSignatureStep03,
          step04: family.buildMissingSignatureStep04,
          remove: buildRemoveFraudulentBlock,
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
        holder: TEST_PROOF_MATERIALS_HOLDER,
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
        if (next === undefined) throw new Error("missing recovered successor");
        thread = `${next.txHash}#${next.outputIndex}`;
      }
    }
    await s.remove();
  }, 600_000);
  it("runs registered Init, cancels every position, restarts by out-ref, proves a real signature and removes", async () => {
    const s = await setupScenario();
    for (let position = 0; position < 4; position++) {
      let thread = await s.init();
      for (let i = 0; i < position; i++) thread = await s.advance(thread, i);
      await s.record(`cancel-${position}`, () =>
        buildAndSubmit(buildMissingSignatureForcedCancel, {
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
    {
      witnessCount: 317,
      signerCount: 1,
      coverage: ["4:Inline", "7:Certified"],
    },
    {
      witnessCount: 1,
      signerCount: 1088,
      coverage: ["4:Certified", "7:Inline"],
    },
  ])(
    "automatically publishes and certifies installed maximum $witnessCount witnesses/$signerCount signers across restarts",
    async ({ coverage, ...shape }) => {
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
        createMissingSignatureFieldPrerequisite({
          lucid: s.harness.proverLucid,
          network,
          signer: s.harness.proverSigner,
          certificate: s.harness.contracts.fieldPreimageCertificate,
          certificateReference: reference.utxo,
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
            holder: TEST_PROOF_MATERIALS_HOLDER,
            headerHash: s.prepared.headerHash,
            baseAction: observation.action,
            artifact,
          });
          if (next.kind === "satisfied" || next.kind === "not_required") break;
          if (next.kind !== "required")
            throw new Error("unexpected pending prerequisite");
          const captured = await makePort().capture({
            holder: TEST_PROOF_MATERIALS_HOLDER,
            headerHash: s.prepared.headerHash,
            action: next.action,
            artifact,
          });
          await s.record("installed-field-prerequisite", () =>
            captured.transaction.signed.submit(),
          );
          s.harness.emulator.awaitBlock(1);
        }
        const resolved = await makePort().resolveAuthenticated({
          holder: TEST_PROOF_MATERIALS_HOLDER,
          headerHash: s.prepared.headerHash,
          action: observation.action,
          artifact,
        });
        if (resolved.requirement?.planned.plan.tier === "Certified")
          expect(resolved.certificate).toBeDefined();
        // The step reads the carriage the prerequisite published.
        if (ordinal === 6) thread = await s.advance(thread, 2);
        else expect((await s.action(thread, 3)).kind).toBe("proven");
      }
      // The forced steps' builders asked for exactly the carriage slots the
      // prerequisite required, from the prover's proof-materials address,
      // and each found them there by content.
      expect(publishedCoverage()).toEqual(coverage);
      expect(carriageNeedsByField(carriagePlanRecord.published)).toEqual(
        carriageNeedsByField(carriagePlanRecord.required),
      );
      expect(
        carriagePlanRecord.published.map(({ publisherAddress, turn }) => ({
          publisherAddress,
          turn,
        })),
      ).toEqual(
        carriagePlanRecord.published.map(() => ({
          publisherAddress: s.harness.proverSigner.proofMaterialsAddress,
          turn: false,
        })),
      );
      expect(
        new Set(
          carriagePlanRecord.required.map(
            ({ publisherAddress }) => publisherAddress,
          ),
        ),
      ).toEqual(new Set([s.harness.proverSigner.proofMaterialsAddress]));
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
