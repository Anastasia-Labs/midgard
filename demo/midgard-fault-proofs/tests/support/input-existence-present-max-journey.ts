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
import { expect } from "vitest";

import {
  certifyFaultProofFieldCarriage,
  publishFaultProofFieldCarriage,
} from "../../src/field-opening.js";
import {
  admitNoReferenceInputForcedArtifact,
  noReferenceInputForcedArtifact,
} from "../../src/no-reference-input/artifact.js";
import {
  noReferenceInputForcedFieldPlan,
  submitNoReferenceInputForcedStep,
} from "../../src/no-reference-input/submit.js";
import { noReferenceInputForcedSourceMaterial } from "../../src/no-reference-input/wrongful-rejection.js";
import {
  admitNonExistentInputForcedArtifact,
  nonExistentInputForcedArtifact,
} from "../../src/non-existent-input/artifact.js";
import {
  nonExistentInputForcedFieldPlan,
  submitNonExistentInputForcedStep,
} from "../../src/non-existent-input/submit.js";
import {
  nonExistentInputForcedSourceMaterial,
  type PreparedNonExistentInputWrongfulRejection,
} from "../../src/non-existent-input/wrongful-rejection.js";
import { submitRemoveFraudulentBlock } from "../../src/remove-fraudulent-block.js";
import { submitInit } from "../../src/submit-init.js";
import { commitCountedRoot } from "../../src/transition-trace/phas.js";
import { verifiedMaximumForkPrefixes } from "./canonical-decodability-emulator.long-fork-proof.js";
import { buildCatalogueDeploymentInfo } from "./emulator/catalogue.js";
import { alignUnixTimeToEmulatorSlotBoundary } from "./emulator/emulator-context.js";
import { makeFaultProofEmulatorHarness } from "./emulator/harness.js";
import { withCommitSlot } from "./emulator/header-fixtures.js";
import {
  captureEmulatorSubmission,
  type CompleteSignedTransactionMeasurement,
} from "./emulator/measurement.js";
import { makeNativeTx } from "./emulator/native-tx.js";
import { buildRemovalDeploymentInfo } from "./emulator/removal-deployment.js";
import { submitSetupTx } from "./emulator/setup-tx.js";
import { deepProofChunks } from "./input-existence-deep-chunks.js";
import { makeReferenceNativeTx } from "./no-reference-input-native.js";
import { buildInvalidForcedTransitionTraceFixture } from "./submit-init-emulator-fixtures.js";
import {
  publishPlainReferenceScriptUtxo,
  publishRemovalReferenceScripts,
} from "./submit-init-emulator-shared.js";
import { syntheticDeepMembershipProof } from "./synthetic-deep-proof.js";

/** The Present maximum: the largest input field (819 inputs, a Certified
 * field carriage), the last input selected, and a maximum-depth membership
 * proof on every witness, including the ledger membership of the selected
 * input that the terminal step checks. */
export const PRESENT_MAX_INPUT_COUNT = 819;
export const PRESENT_MAX_SELECTED_INDEX = 818n;

/** `branches` is a 64-level branch ladder; the fork shapes are 63 forks
 * closed by a branch or a leaf, the largest openings the verifier admits. */
export type PresentMaxShape = "branches" | "forks_leaf" | "forks_branch";
export type PresentMaxField = 0 | 1;

export type PresentMaxRow = {
  readonly label: string;
  readonly measurement: CompleteSignedTransactionMeasurement;
};

const shapeProof = (shape: PresentMaxShape, key: Buffer, value: Buffer) =>
  shape === "branches"
    ? syntheticDeepMembershipProof({ key, value, branchLevels: 64 })
    : verifiedMaximumForkPrefixes(
        key,
        value,
        shape === "forks_leaf" ? "leaf" : "branch",
      );

const families = {
  0: {
    category: "nonExistentInput",
    label: "no-input",
    material: nonExistentInputForcedSourceMaterial,
    artifact: nonExistentInputForcedArtifact,
    admit: admitNonExistentInputForcedArtifact,
    plan: nonExistentInputForcedFieldPlan,
    step: submitNonExistentInputForcedStep,
    native: (inputs: readonly Buffer[]) =>
      makeNativeTx({ spendInputCbors: [...inputs], fee: 0n }),
  },
  1: {
    category: "noReferenceInput",
    label: "reference-input",
    material: noReferenceInputForcedSourceMaterial,
    artifact: noReferenceInputForcedArtifact,
    admit: admitNoReferenceInputForcedArtifact,
    plan: noReferenceInputForcedFieldPlan,
    step: submitNoReferenceInputForcedStep,
    native: (inputs: readonly Buffer[]) =>
      makeReferenceNativeTx({
        spendInputCbors: [],
        referenceInputCbors: inputs,
        fee: 0n,
      }),
  },
} as const;

/** Drives the forced wrongful-rejection proof of `field` at the Present
 * maximum through setup, carriage, init, the four steps and the removal,
 * and returns the measurement of every submitted transaction. */
export const runPresentMaxJourney = async ({
  field,
  shape,
}: {
  readonly field: PresentMaxField;
  readonly shape: PresentMaxShape;
}): Promise<readonly PresentMaxRow[]> => {
  const family = families[field];
  const harness = await makeFaultProofEmulatorHarness({
    contractOptions: {
      realNonExistentInput: field === 0,
      realNoReferenceInput: field === 1,
      alwaysFraudProofCatalogue: true,
    },
  });
  const contracts = {
    hubOraclePolicyId: harness.contracts.hubOracle.policyId,
    steps: harness.contracts.fraudProofContracts[family.category].steps,
    computationThread: harness.contracts.computationThread,
    fraudProof: harness.contracts.fraudProof,
  };
  const catalogue = await buildCatalogueDeploymentInfo(
    harness.contracts.fraudProofs,
  );
  const categoryId = catalogue.categories[family.category].categoryId;
  const rows: PresentMaxRow[] = [];
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
            label: `${family.label} step ${stepIndex.toString()}`,
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
  const inputs = Array.from({ length: PRESENT_MAX_INPUT_COUNT }, (_, i) =>
    encodeMidgardSpendInputItem({
      txId: Buffer.alloc(32, 0x55),
      outputIndex: i,
    }),
  );
  const native = materializeMidgardForcedTxFromCanonical(family.native(inputs));
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
          InputNotFound: {
            source_kind: BigInt(field),
            input_index: PRESENT_MAX_SELECTED_INDEX,
          },
        },
      },
    },
  } as const;
  const eventKey = base.eventKey;
  const forcedKey = eventKey.ForcedTransactionEventKey.tx_order_id;
  const target = inputs[Number(PRESENT_MAX_SELECTED_INDEX)]!;
  const descriptor = buildCanonicalMidgardLedgerEntryOutputMaterial({
    outRef: target,
    outputCbor: Buffer.from(
      "a200581d70aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa018200a0",
      "hex",
    ),
  }).descriptorCbor;
  const ledger = shapeProof(shape, target, descriptor);
  const ledgerRoot = ledger.transactionsPhasRoot;
  const membership = async <K, V>(
    domain: SDK.RootDomain,
    key: K,
    value: V,
    keyCbor: string,
    valueCbor: string,
  ): Promise<SDK.RootMembershipProof<K, V>> => {
    const ladder = shapeProof(
      shape,
      Buffer.from(keyCbor, "hex"),
      Buffer.from(valueCbor, "hex"),
    );
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
  const material = family.material(forcedSource, fullTransactionCbor);
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
  const artifact = family.artifact(prepared);
  const setup = await captures("setup", () =>
    submitSetupTx({
      lucid: harness.funderLucid,
      contracts: harness.contracts,
      nonceUtxo: harness.nonceUtxo,
      catalogue,
      header,
    }),
  );
  const plan = family.plan(prepared, harness.proverSigner.paymentKeyHash);
  // The case is the largest field there is, carried by certificate.
  expect(plan.plan.tier).toBe("Certified");
  expect(
    (field === 0
      ? native.body.spendInputsPreimageCbor
      : native.body.referenceInputsPreimageCbor
    ).length,
  ).toBe(32763);
  const chunks = await captures("field-publication", () =>
    publishFaultProofFieldCarriage({
      lucid: harness.proverLucid,
      signer: harness.proverSigner,
      planned: plan,
      publisherAddress: harness.proverSigner.address,
      label: `${family.label} field`,
    }),
  );
  const certificateReference = await captures(
    "publish-certificate-script",
    () =>
      publishPlainReferenceScriptUtxo({
        lucid: harness.proverLucid,
        script: harness.contracts.fieldPreimageCertificate.mintingScript,
        label: `${family.label} certificate`,
      }),
  );
  const certified = await captures("field-certification", () =>
    certifyFaultProofFieldCarriage({
      lucid: harness.proverLucid,
      network: "Custom",
      signer: harness.proverSigner,
      planned: plan,
      certificatePolicyId: harness.contracts.fieldPreimageCertificate.policyId,
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
      deploymentInfo: buildRemovalDeploymentInfo(harness.contracts, catalogue),
      network: "Custom",
      signer: harness.proverSigner,
      fraudCategory: family.category,
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
        family.step({
          blueprint: harness.realBlueprint,
          network: "Custom",
          witnessReferenceScripts: harness.witnessReferenceScripts,
          lucid: harness.proverLucid,
          contracts,
          categoryId,
          signer: harness.proverSigner,
          threadOutRef: thread,
          stepIndex,
          prepared: await family.admit(JSON.parse(JSON.stringify(artifact))),
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
      deploymentInfo: buildRemovalDeploymentInfo(harness.contracts, catalogue, {
        removalReferenceScripts: removal.published,
      }),
      network: "Custom",
      signer: harness.proverSigner,
      fraudCategory: family.category,
      fraudulentHeaderHash: setup.headerHash,
      requireReferenceScripts: true,
      stateQueueMutationLeaseCoordinator: {
        acquire: async () => ({
          token: `${family.label}-lease`,
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
  return rows;
};
