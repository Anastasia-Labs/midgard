/**
 * The accepted missing-signature path's field carriage, driven through the
 * workflow's transaction port and field-carriage prerequisite on the
 * emulator. A process is rebuilt for every action: only the journal stays.
 *
 * The step-04 witness field is too wide to ride inline, so it is published
 * before the step by its journaled prerequisite, and a step captured again
 * by a new process reads that publication instead of publishing it again.
 */
import { ledgerEd25519SignatureReady } from "@al-ft/midgard-core";
import { type UTxO } from "@lucid-evolution/lucid";
import { beforeAll, describe, expect, it } from "vitest";

import * as family from "../src/missing-signature/index.js";
import { submitRemoveFraudulentBlock } from "../src/remove-fraudulent-block.js";
import { normalizeJournalJson } from "../src/workflow/journal.js";
import {
  createMissingSignatureFieldPrerequisite,
  MISSING_SIGNATURE_ARTIFACT,
  unsafeCreateMissingSignatureTransactionPortForTest,
} from "../src/workflow/missing-signature.js";
import { missingSignatureObservation } from "../src/workflow/missing-signature-state.js";
import type { FraudProofWorkflowAction } from "../src/workflow/orchestrator.js";
import { FRAUD_PROOF_AUTHENTICATED_PUBLICATION_OBSERVER } from "../src/workflow/raw-l1-publication-observation.js";
import { createConfirmedActionJournal } from "./support/confirmed-action-journal.js";
import { publishPlainReferenceScriptUtxo } from "./support/emulator/reference-scripts.js";
import {
  buildMissingSignatureSubject,
  makeMissingSignatureEmulatorHarness,
  MISSING_SIGNATURE_TARGET_HASH,
  MISSING_SIGNATURE_TARGET_VKEY,
  type MissingSignatureScenario,
  publishMissingSignatureReferenceScripts,
  setupMissingSignatureScenario,
} from "./support/missing-signature-emulator.js";
import { network } from "./support/missing-signature-forced-scenario.js";

beforeAll(() => ledgerEd25519SignatureReady());

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
  it("publishes the step-04 field once through its journaled prerequisite, and a new process captures the step from it", async () => {
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
    const journal = createConfirmedActionJournal({
      category: "missingSignature",
      headerHash,
    });
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
          init: family.submitMissingSignatureInit,
          step01: family.submitMissingSignatureStep01,
          step02: family.submitMissingSignatureStep02,
          step03: family.submitMissingSignatureStep03,
          step04: family.submitMissingSignatureStep04,
          remove: submitRemoveFraudulentBlock,
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
    /** Each prerequisite action, by a new process, journaled once confirmed. */
    const settlePrerequisite = async (baseAction: FraudProofWorkflowAction) => {
      for (let attempt = 0; attempt < 8; attempt += 1) {
        const next = await process().prerequisite.inspect({
          headerHash,
          baseAction,
          artifact,
          entries: journal.entries(),
        });
        if (next.kind !== "required") return next.kind;
        const captured = await process().prerequisite.capture({
          headerHash,
          action: next.action,
          artifact,
          entries: journal.entries(),
        });
        const txHash = await captured.transaction.signed.submit();
        harness.emulator.awaitBlock(1);
        const durableRecovery = JSON.parse(
          JSON.stringify(captured.durableRecovery),
        );
        expect(
          (
            await process().prerequisite.reconcile({
              headerHash,
              action: next.action,
              artifact,
              txHash,
              durableRecovery,
            })
          ).kind,
        ).toBe("confirmed");
        journal.confirm({ action: next.action, txHash, durableRecovery });
      }
      throw new Error("prerequisite did not settle");
    };
    const capture = async (action: FraudProofWorkflowAction) =>
      await process().port.capture({
        action,
        artifact,
        entries: journal.entries(),
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
      (await harness.proverLucid.utxosAt(harness.proverSigner.address)).filter(
        ({ datum, scriptRef }) => datum != null && scriptRef == null,
      );
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
    // A process that captured the step stops before journaling it; the next
    // one captures it again.
    await capture(step04);
    const next = await submit(await capture(step04), stepAddress(3));
    expect(next).not.toBe(thread);
    expect((await datumOutputsAtProver()).map(outRefOf)).toEqual(
      published.map(outRefOf),
    );
  }, 600_000);
});
