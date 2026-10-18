import {
  computeHash28,
  computeMidgardNativeTxId,
  encodeMidgardSpendInputItem,
} from "@al-ft/midgard-core";
import {
  deriveMidgardForcedTxProofSource,
  encodeMidgardForcedTxCanonical,
  materializeMidgardForcedTxFromCanonical,
} from "@al-ft/midgard-core/codec/forced";
import * as SDK from "@al-ft/midgard-sdk";
import { buildCanonicalMidgardLedgerEntryOutputMaterial } from "@al-ft/midgard-validation";
import { Data, getAddressDetails, type UTxO } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  certifyFaultProofFieldCarriage,
  publishFaultProofFieldCarriage,
} from "../src/field-opening.js";
import {
  admitNonExistentInputForcedArtifact,
  nonExistentInputForcedArtifact,
} from "../src/non-existent-input/artifact.js";
import {
  nonExistentInputForcedFieldPlan,
  submitNonExistentInputForcedStep,
} from "../src/non-existent-input/submit.js";
import {
  nonExistentInputForcedSourceMaterial,
  type PreparedNonExistentInputWrongfulRejection,
} from "../src/non-existent-input/wrongful-rejection.js";
import { submitRemoveFraudulentBlock } from "../src/remove-fraudulent-block.js";
import { submitInit } from "../src/submit-init.js";
import { commitCountedRoot } from "../src/transition-trace/phas.js";
import { buildCatalogueDeploymentInfo } from "./support/emulator/catalogue.js";
import { alignUnixTimeToEmulatorSlotBoundary } from "./support/emulator/emulator-context.js";
import { makeFaultProofEmulatorHarness } from "./support/emulator/harness.js";
import { withCommitSlot } from "./support/emulator/header-fixtures.js";
import {
  captureEmulatorSubmission,
  type CompleteSignedTransactionMeasurement,
} from "./support/emulator/measurement.js";
import { makeNativeTx } from "./support/emulator/native-tx.js";
import { buildRemovalDeploymentInfo } from "./support/emulator/removal-deployment.js";
import { submitSetupTx } from "./support/emulator/setup-tx.js";
import { deepProofChunks } from "./support/input-existence-deep-chunks.js";
import { buildInvalidForcedTransitionTraceFixture } from "./support/submit-init-emulator-fixtures.js";
import {
  publishPlainReferenceScriptUtxo,
  publishRemovalReferenceScripts,
} from "./support/submit-init-emulator-shared.js";
import { syntheticDeepMembershipProof } from "./support/synthetic-deep-proof.js";

/** Per-transaction fit limits for every input-existence step. */
const MAX_MEMORY = 13_200_000n;
const MAX_CPU = 8_000_000_000n;
const MAX_SIGNED_BYTES = 16_384;

/** The Present maximum: the largest spend-input field (819 inputs, a
 * Certified field carriage), the last input selected, and 64-level ladders on
 * every membership witness, including the ledger membership of the selected
 * input that the terminal step checks. */
const INPUT_COUNT = 819;
const SELECTED_INDEX = 818n;
const BRANCH_LEVELS = 64;

type Row = {
  readonly label: string;
  readonly measurement: CompleteSignedTransactionMeasurement;
};

const marginLine = ({ label, measurement }: Row) =>
  [
    label.padEnd(30),
    `bytes ${measurement.completeSignedBytes.toString().padStart(6)} (margin ${(MAX_SIGNED_BYTES - measurement.completeSignedBytes).toString()})`,
    `mem ${measurement.executionMemory.toString().padStart(9)} (margin ${(MAX_MEMORY - measurement.executionMemory).toString()})`,
    `cpu ${measurement.executionSteps.toString().padStart(13)} (margin ${(MAX_CPU - measurement.executionSteps).toString()})`,
  ].join("  ");

describe("input-existence Present maximum fit", () => {
  it("fits every transaction of the deep 819-input wrongful-rejection proof", async () => {
    const harness = await makeFaultProofEmulatorHarness({
      contractOptions: {
        realNonExistentInput: true,
        alwaysFraudProofCatalogue: true,
      },
    });
    const contracts = {
      hubOraclePolicyId: harness.contracts.hubOracle.policyId,
      steps: harness.contracts.fraudProofContracts.nonExistentInput.steps,
      computationThread: harness.contracts.computationThread,
      fraudProof: harness.contracts.fraudProof,
    };
    const catalogue = await buildCatalogueDeploymentInfo(
      harness.contracts.fraudProofs,
    );
    const categoryId = catalogue.categories.nonExistentInput.categoryId;
    const rows: Row[] = [];
    const captures = async <T>(label: string, action: () => Promise<T>) => {
      const captured = await captureEmulatorSubmission(
        harness.emulator,
        action,
      ).catch((error) => {
        throw new Error(`stage ${label}: ${String(error)}`);
      });
      captured.measurements.forEach((measurement, position) =>
        rows.push({
          label:
            captured.measurements.length === 1
              ? label
              : `${label}[${position.toString()}]`,
          measurement,
        }),
      );
      return captured.result;
    };
    const references: UTxO[] = [];
    for (const [stepIndex, step] of contracts.steps.entries())
      references.push(
        (
          await captures(`publish-step-${stepIndex.toString()}`, () =>
            publishPlainReferenceScriptUtxo({
              lucid: harness.proverLucid,
              script: step.spendingScript,
              label: `no-input step ${stepIndex.toString()}`,
            }),
          )
        ).utxo,
      );
    const referenceScripts = {
      steps: references,
      computationThreadMint:
        harness.witnessReferenceScripts.computationThreadMint!,
      fraudProofMint: harness.witnessReferenceScripts.fraudProofMint!,
    };
    const credential = getAddressDetails(
      await harness.funderLucid.wallet().address(),
    ).paymentCredential;
    if (credential?.type !== "Key") throw new Error("operator absent");
    const base = await buildInvalidForcedTransitionTraceFixture({
      operatorVkey: credential.hash,
      now:
        alignUnixTimeToEmulatorSlotBoundary(
          harness.funderLucid,
          harness.emulator.now() + 120_000,
        ) - 1,
    });
    const inputs = Array.from({ length: INPUT_COUNT }, (_, i) =>
      encodeMidgardSpendInputItem({
        txId: Buffer.alloc(32, 0x55),
        outputIndex: i,
      }),
    );
    const native = materializeMidgardForcedTxFromCanonical(
      makeNativeTx({ spendInputCbors: inputs, fee: 0n }),
    );
    const source = deriveMidgardForcedTxProofSource(native);
    const leaf = {
      tx_id: computeMidgardNativeTxId(native).toString("hex"),
      submitted_source: {
        compact_cbor: source.compactCbor.toString("hex"),
        witness_set_compact_cbor: source.witnessSetCompactCbor.toString("hex"),
        field_preimage_lengths_cbor:
          source.fieldPreimageLengthsCbor.toString("hex"),
      },
      verdict: {
        ForcedTxInvalid: {
          reason: {
            InputNotFound: { source_kind: 0n, input_index: SELECTED_INDEX },
          },
        },
      },
    } as const;
    const eventKey = base.eventKey;
    const forcedKey = eventKey.ForcedTransactionEventKey.tx_order_id;
    const target = inputs[Number(SELECTED_INDEX)]!;
    const descriptor = buildCanonicalMidgardLedgerEntryOutputMaterial({
      outRef: target,
      outputCbor: Buffer.from(
        "a200581d70aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa018200a0",
        "hex",
      ),
    }).descriptorCbor;
    const ledger = syntheticDeepMembershipProof({
      key: target,
      value: descriptor,
      branchLevels: BRANCH_LEVELS,
    });
    const ledgerRoot = ledger.transactionsPhasRoot;
    const membership = async <K, V>(
      domain: SDK.RootDomain,
      key: K,
      value: V,
      keyCbor: string,
      valueCbor: string,
    ): Promise<SDK.RootMembershipProof<K, V>> => {
      const ladder = syntheticDeepMembershipProof({
        key: Buffer.from(keyCbor, "hex"),
        value: Buffer.from(valueCbor, "hex"),
        branchLevels: BRANCH_LEVELS,
      });
      return {
        domain,
        root: await commitCountedRoot({
          domain,
          phasRoot: ladder.transactionsPhasRoot,
          count: 1n,
        }),
        phas_root: ladder.transactionsPhasRoot,
        count: 1n,
        key,
        value,
        proof: Data.from(ladder.proofCbor, SDK.Proof),
      };
    };
    const forcedMembership = await membership(
      SDK.ROOT_DOMAINS.forcedTransactionsV1,
      forcedKey,
      leaf,
      Data.to(forcedKey, SDK.OutputReference),
      Data.to(leaf, SDK.ForcedInclusionTxV1),
    );
    const eventValue = {
      step_index: 0n,
      phase: "ForcedTransaction" as const,
    };
    const eventMembership = await membership(
      SDK.ROOT_DOMAINS.eventToStep,
      eventKey,
      eventValue,
      Data.to(eventKey, SDK.EventKey),
      Data.to(eventValue, SDK.EventToStepValue),
    );
    const transition: SDK.TransitionStep = {
      schema_version: 1n,
      step_index: 0n,
      event_key: eventKey,
      phase: "ForcedTransaction",
      pre_utxos_root: ledgerRoot,
      post_utxos_root: ledgerRoot,
    };
    const transitionMembership = await membership(
      SDK.ROOT_DOMAINS.transitionTrace,
      0n,
      transition,
      Data.to(0n),
      Data.to(transition, SDK.TransitionStep),
    );
    const header = withCommitSlot(harness.contracts.l1SlotConfig, {
      ...base.header,
      forcedTransactionsRoot: forcedMembership.root,
      eventToStepRoot: eventMembership.root,
      transitionTraceRoot: transitionMembership.root,
      utxosRoot: ledgerRoot,
    });
    const forcedSource = { header, membership: forcedMembership };
    const fullTransactionCbor =
      encodeMidgardForcedTxCanonical(native).toString("hex");
    const material = nonExistentInputForcedSourceMaterial(
      forcedSource,
      fullTransactionCbor,
    );
    expect(material.selectedInput).not.toBeNull();
    const prepared: PreparedNonExistentInputWrongfulRejection = {
      headerHash: computeHash28(SDK.encodeHeaderCbor(header)).toString("hex"),
      forcedSource,
      fullTransactionCbor,
      ...material,
      eventMembership,
      transitionMembership,
      ledgerMembership: {
        value: descriptor.toString("hex"),
        proof: Data.from(ledger.proofCbor, SDK.Proof),
      },
    };
    const artifact = nonExistentInputForcedArtifact(prepared);
    const setup = await captures("setup", () =>
      submitSetupTx({
        lucid: harness.funderLucid,
        contracts: harness.contracts,
        nonceUtxo: harness.nonceUtxo,
        catalogue,
        header,
      }),
    );
    const plan = nonExistentInputForcedFieldPlan(
      prepared,
      harness.proverSigner.paymentKeyHash,
    );
    // The case is the largest field there is, carried by certificate.
    expect(plan.plan.tier).toBe("Certified");
    expect(native.body.spendInputsPreimageCbor.length).toBe(32763);
    const chunks = await captures("field-publication", () =>
      publishFaultProofFieldCarriage({
        lucid: harness.proverLucid,
        signer: harness.proverSigner,
        planned: plan,
        publisherAddress: harness.proverSigner.address,
        label: "no-input field",
      }),
    );
    const certificateReference = await captures(
      "publish-certificate-script",
      () =>
        publishPlainReferenceScriptUtxo({
          lucid: harness.proverLucid,
          script: harness.contracts.fieldPreimageCertificate.mintingScript,
          label: "no-input certificate",
        }),
    );
    const certified = await captures("field-certification", () =>
      certifyFaultProofFieldCarriage({
        lucid: harness.proverLucid,
        network: "Custom",
        signer: harness.proverSigner,
        planned: plan,
        certificatePolicyId:
          harness.contracts.fieldPreimageCertificate.policyId,
        certificateMintingScript:
          harness.contracts.fieldPreimageCertificate.mintingScript,
        certificateReferenceScriptUtxo: certificateReference.utxo,
        chunkUtxos: chunks,
        compactCbor: leaf.submitted_source.compact_cbor,
        witnessSetCompactCbor: leaf.submitted_source.witness_set_compact_cbor,
      }),
    );
    const initResult = await captures("init", () =>
      submitInit({
        lucid: harness.proverLucid,
        witnessReferenceScripts: harness.witnessReferenceScripts,
        blueprint: harness.realBlueprint,
        deploymentInfo: buildRemovalDeploymentInfo(
          harness.contracts,
          catalogue,
        ),
        network: "Custom",
        signer: harness.proverSigner,
        fraudCategory: "nonExistentInput",
        fraudulentBlockOutRef: setup.fraudulentBlockOutRef,
        awaitConfirmation: true,
      }),
    );
    const chunked = await deepProofChunks(harness, prepared);
    let thread = `${initResult.txHash}#${initResult.firstStepOutputIndex.toString()}`;
    let fraudProofUnit: unknown;
    for (const stepIndex of [0, 1, 2, 3] as const) {
      const advanced = await captures(
        `step-${(stepIndex + 1).toString()}`,
        async () =>
          submitNonExistentInputForcedStep({
            blueprint: harness.realBlueprint,
            network: "Custom",
            witnessReferenceScripts: harness.witnessReferenceScripts,
            lucid: harness.proverLucid,
            contracts,
            categoryId,
            signer: harness.proverSigner,
            threadOutRef: thread,
            stepIndex,
            prepared: await admitNonExistentInputForcedArtifact(
              JSON.parse(JSON.stringify(artifact)),
            ),
            referenceScripts,
            fieldCarriageUtxos:
              stepIndex === 1 ? [...chunks, certified.certificateUtxo] : [],
            certificatePolicyId:
              harness.contracts.fieldPreimageCertificate.policyId,
            publishedProofChunks: chunked[stepIndex] ?? [],
          }),
      );
      if (stepIndex === 3) fraudProofUnit = advanced.fraudProofUnit;
      else thread = advanced.nextThreadOutRef!;
    }
    expect(fraudProofUnit).toBeTruthy();
    const removal = await captures("publish-removal-scripts", () =>
      publishRemovalReferenceScripts({
        lucid: harness.proverLucid,
        contracts: harness.contracts,
      }),
    );
    const now = BigInt(harness.emulator.now());
    await captures("remove", () =>
      submitRemoveFraudulentBlock({
        lucid: harness.proverLucid,
        blueprint: harness.realBlueprint,
        deploymentInfo: buildRemovalDeploymentInfo(
          harness.contracts,
          catalogue,
          { removalReferenceScripts: removal.published },
        ),
        network: "Custom",
        signer: harness.proverSigner,
        fraudCategory: "nonExistentInput",
        fraudulentHeaderHash: setup.headerHash,
        requireReferenceScripts: true,
        stateQueueMutationLeaseCoordinator: {
          acquire: async () => ({
            token: "no-input-lease",
            source: "emulator",
            renew: async () => {},
            release: async () => {},
            fail: async () => {},
          }),
        },
        validFrom: now > 120000n ? now - 120000n : 0n,
        validTo: now + 300_000n,
      }),
    );

    console.info(
      [
        `Present maximum (${INPUT_COUNT.toString()} inputs, index ${SELECTED_INDEX.toString()}, ${BRANCH_LEVELS.toString()} levels): limits ${MAX_SIGNED_BYTES.toString()} B / ${MAX_MEMORY.toString()} mem / ${MAX_CPU.toString()} cpu`,
        ...rows.map(marginLine),
      ].join("\n"),
    );
    // Every proof step and the removal ran scripts and were measured.
    for (const label of ["step-1", "step-2", "step-3", "step-4", "remove"])
      expect(
        rows.some(
          (row) => row.label === label && row.measurement.executionMemory > 0n,
        ),
        label,
      ).toBe(true);
    for (const { label, measurement } of rows) {
      expect(measurement.completeSignedBytes, label).toBeLessThanOrEqual(
        MAX_SIGNED_BYTES,
      );
      expect(measurement.executionMemory, label).toBeLessThanOrEqual(
        MAX_MEMORY,
      );
      expect(measurement.executionSteps, label).toBeLessThanOrEqual(MAX_CPU);
    }
  }, 180_000);
});
