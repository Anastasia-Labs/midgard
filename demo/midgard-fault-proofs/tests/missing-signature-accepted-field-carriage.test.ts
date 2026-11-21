/**
 * The accepted missing-signature path's field carriage, driven through the
 * workflow's transaction port and field-carriage prerequisite on the
 * emulator. A process is rebuilt for every action: only the chain stays.
 *
 * The step-04 witness field is too wide to ride inline, so it is published
 * before the step by its prerequisite, and a step captured again by a new
 * process reads that publication instead of publishing it again. Every
 * carriage plan a step builder asks `publishFaultProofFieldCarriage` for
 * needs exactly the slots (keys and count) of the plan the prerequisite
 * required, and each finds them at the proof-materials address by content
 * instead of building a turn.
 */
import { ledgerEd25519SignatureReady } from "@al-ft/midgard-core";
import { type UTxO } from "@lucid-evolution/lucid";
import { beforeAll, beforeEach, describe, expect, it, vi } from "vitest";

import * as family from "../src/missing-signature/index.js";
import { buildRemoveFraudulentBlock } from "../src/remove-fraudulent-block.build-remove-fraudulent-block.js";
import { normalizeJournalJson } from "../src/workflow/journal.fraud-proof-workflow-terminal.js";
import { unsafeCreateMissingSignatureTransactionPortForTest } from "../src/workflow/missing-signature.create-manifest-bound-missing-signature-workflow.js";
import { createMissingSignatureFieldPrerequisite } from "../src/workflow/missing-signature.create-missing-signature-field-prerequisite.js";
import { MISSING_SIGNATURE_ARTIFACT } from "../src/workflow/missing-signature.parse-artifact.js";
import { missingSignatureObservation } from "../src/workflow/missing-signature-state.js";
import type { FraudProofWorkflowAction } from "../src/workflow/orchestrator.js";
import { FRAUD_PROOF_AUTHENTICATED_PUBLICATION_OBSERVER } from "../src/workflow/raw-l1-publication-observation.js";
import {
  carriageNeedsByField,
  carriagePlanRecord,
  publishedCoverage,
  resetCarriagePlanRecord,
} from "./support/carriage-plan-parity.js";
import { publishPlainReferenceScriptUtxo } from "./support/emulator/reference-scripts.js";
import {
  buildMissingSignatureSubject,
  makeMissingSignatureEmulatorHarness,
  MISSING_SIGNATURE_TARGET_HASH,
  MISSING_SIGNATURE_TARGET_VKEY,
  type MissingSignatureScenario,
  publishMissingSignatureReferenceScripts,
  setupMissingSignatureScenario,
} from "./support/missing-signature-emulator.build-missing-signature-subject.js";
import { network } from "./support/missing-signature-forced-scenario.js";
import { TEST_PROOF_MATERIALS_HOLDER } from "./support/proof-materials-holder.js";

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

beforeAll(() => ledgerEd25519SignatureReady());
beforeEach(resetCarriagePlanRecord);

/**
 * 142 address witnesses put field 7's preimage past the inline bound and
 * inside one publication, so step 04 needs published carriage.
 */
const WIDE_WITNESS_COUNT = 142;

const outRefOf = ({ txHash, outputIndex }: UTxO) =>
  `${txHash}#${outputIndex.toString()}`;

/** The watcher's accepted-path artifact for the scenario's block. */
const acceptedArtifact = (scenario: MissingSignatureScenario) => {
  const body = scenario.block.reconstruction.payload.block_body;
  const sources = new Map(body.transactions);
  const transactions = body.transaction_preimages.map(([nodeTxId, txCbor]) => ({
    nodeTxId,
    txCbor,
    l2TransactionSourceCbor: sources.get(nodeTxId)!,
  }));
  return normalizeJournalJson({
    schemaVersion: MISSING_SIGNATURE_ARTIFACT,
    headerHash: scenario.setup.headerHash,
    committedTransactionsRoot: scenario.block.header.transactionsRoot,
    selectedTransactionIndex: transactions.findIndex(
      ({ nodeTxId }) => nodeTxId === scenario.block.nativeTxId,
    ),
    accusedRequiredSignerIndex: 0,
    accusedRequiredSignerHash: MISSING_SIGNATURE_TARGET_HASH,
    resolvedVkey: MISSING_SIGNATURE_TARGET_VKEY,
    transactions,
  }) as never;
};

describe("missing-signature accepted-path field carriage", () => {
  it("publishes the step-04 field once through its prerequisite, and a new process captures the step from it", async () => {
    const harness = await makeMissingSignatureEmulatorHarness();
    // Another transaction of the block carries the accused key's witness,
    // so its verification key is public evidence.
    const scenario = await setupMissingSignatureScenario({
      harness,
      decoyWitnessCount: WIDE_WITNESS_COUNT,
      additionalTransactions: [
        buildMissingSignatureSubject({ honest: true, fee: 1n }).nativeTx,
      ],
    });
    const headerHash = scenario.setup.headerHash;
    const artifact = acceptedArtifact(scenario);
    const steps = await publishMissingSignatureReferenceScripts({
      lucid: harness.funderLucid,
      contracts: harness.missingSignature,
    });
    const { utxo: certificateReference } =
      await publishPlainReferenceScriptUtxo({
        lucid: harness.proverLucid,
        script: harness.contracts.fieldPreimageCertificate.mintingScript,
        label: "missing-signature certificate mint",
      });
    harness.emulator.awaitBlock(1);
    /** A new watcher process: its port and prerequisite, nothing else. */
    const process = () => ({
      port: unsafeCreateMissingSignatureTransactionPortForTest({
        config: {
          lucid: harness.proverLucid,
          blueprint: harness.realBlueprint,
          network,
          signer: harness.proverSigner,
          headerHash,
          contracts: harness.missingSignature,
          category: harness.category,
          catalogue: {
            policyId: harness.contracts.fraudProofCatalogue.policyId,
            spendingScriptAddress:
              harness.contracts.fraudProofCatalogue.spendingScriptAddress,
            root: harness.catalogue.root,
          },
          referenceScripts: {
            steps,
            witnesses: {
              ...harness.witnessReferenceScripts,
              computationThreadMint:
                harness.witnessReferenceScripts.computationThreadMint!,
              fraudProofMint: harness.witnessReferenceScripts.fraudProofMint!,
              phasMembershipWithdraw:
                harness.witnessReferenceScripts.phasMembershipWithdraw!,
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
      }),
      prerequisite: createMissingSignatureFieldPrerequisite({
        lucid: harness.proverLucid,
        network,
        signer: harness.proverSigner,
        certificate: harness.contracts.fieldPreimageCertificate,
        certificateReference,
        publications: {
          observerVersion: FRAUD_PROOF_AUTHENTICATED_PUBLICATION_OBSERVER,
          observeExact: async (input) => {
            const [found] = await harness.proverLucid.utxosByOutRef([
              {
                txHash: input.expectedOutRef.split("#")[0]!,
                outputIndex: Number(input.expectedOutRef.split("#")[1]),
              },
            ]);
            return found !== undefined &&
              found.address === input.address &&
              found.datum === input.expectedDatumCbor &&
              (input.expectedUnit === undefined ||
                found.assets[input.expectedUnit] === 1n)
              ? { kind: "confirmed", outRef: input.expectedOutRef }
              : { kind: "not_found" };
          },
        },
      }),
    });
    const provenance = {
      trustClass: "authenticated_cardano_l1",
      sourceId: "emulator",
      grade: "security",
    } as const;
    const stateQueueBlockOutRef = scenario.setup.fraudulentBlockOutRef;
    const actionAt = (
      stage: Parameters<typeof missingSignatureObservation>[0]["stage"],
    ) => {
      const observation = missingSignatureObservation({
        headerHash,
        provenance,
        stage,
      });
      if (observation.kind !== "action_required")
        throw new Error("missing action");
      return observation.action;
    };
    /** Each prerequisite action, by a new process, landed before the next. */
    const settlePrerequisite = async (baseAction: FraudProofWorkflowAction) => {
      for (let attempt = 0; attempt < 8; attempt += 1) {
        const next = await process().prerequisite.inspect({
          holder: TEST_PROOF_MATERIALS_HOLDER,
          headerHash,
          baseAction,
          artifact,
        });
        if (next.kind !== "required") return next.kind;
        const captured = await process().prerequisite.capture({
          holder: TEST_PROOF_MATERIALS_HOLDER,
          headerHash,
          action: next.action,
          artifact,
        });
        await captured.transaction.signed.submit();
        harness.emulator.awaitBlock(1);
      }
      throw new Error("prerequisite did not settle");
    };
    const capture = async (action: FraudProofWorkflowAction) =>
      await process().port.capture({
        holder: TEST_PROOF_MATERIALS_HOLDER,
        action,
        artifact,
      });
    const submit = async (
      captured: Awaited<ReturnType<typeof capture>>,
      address: string,
    ) => {
      await captured.transaction.signed.submit();
      harness.emulator.awaitBlock(1);
      const [thread] = await harness.proverLucid.utxosAt(address);
      if (thread === undefined) throw new Error("missing thread");
      return outRefOf(thread);
    };
    const datumOutputsAtProver = async () =>
      (
        await harness.proverLucid.utxosAt(
          harness.proverSigner.proofMaterialsAddress,
        )
      ).filter(({ datum, scriptRef }) => datum != null && scriptRef == null);
    const stepAddress = (index: number) =>
      harness.missingSignature.steps[index]!.spendingScriptAddress;

    let thread = await submit(
      await capture(actionAt({ kind: "not_started", stateQueueBlockOutRef })),
      stepAddress(0),
    );
    for (const step of [1, 2, 3] as const) {
      const action = actionAt({
        kind: "step",
        step,
        threadOutRef: thread,
        stateQueueBlockOutRef,
      });
      await settlePrerequisite(action);
      thread = await submit(await capture(action), stepAddress(step));
    }
    expect(await datumOutputsAtProver()).toEqual([]);

    const step04 = actionAt({
      kind: "step",
      step: 4,
      threadOutRef: thread,
      stateQueueBlockOutRef,
    });
    expect(await settlePrerequisite(step04)).toBe("satisfied");
    const published = await datumOutputsAtProver();
    expect(published).toHaveLength(1);
    // A process that captured the step stops before sending it; the next
    // one captures it again.
    await capture(step04);
    const next = await submit(await capture(step04), stepAddress(3));
    expect(next).not.toBe(thread);
    expect((await datumOutputsAtProver()).map(outRefOf)).toEqual(
      published.map(outRefOf),
    );

    // Step 02's required signers ride inline; step 04's witnesses need the
    // publication the prerequisite landed, and both builders asked for
    // exactly what it required, from the prover's proof-materials address.
    expect(publishedCoverage()).toEqual(["4:Inline", "7:RawUtxo"]);
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
        publisherAddress: harness.proverSigner.proofMaterialsAddress,
        turn: false,
      })),
    );
    expect(
      new Set(
        carriagePlanRecord.required.map(
          ({ publisherAddress }) => publisherAddress,
        ),
      ),
    ).toEqual(new Set([harness.proverSigner.proofMaterialsAddress]));
  }, 600_000);
});
