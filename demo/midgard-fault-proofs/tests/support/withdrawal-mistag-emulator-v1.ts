import { computeHash32 } from "@al-ft/midgard-core";
import {
  encodeMidgardSpendInputItemV1,
  encodeMidgardTxOutput,
} from "@al-ft/midgard-core/codec";
import * as SDK from "@al-ft/midgard-sdk";
import { buildCanonicalMidgardLedgerOutputMaterialV1 } from "@al-ft/midgard-validation";
import { CML, Data, type Script, type UTxO } from "@lucid-evolution/lucid";

import { fetchUtxoByOutRef, parseOutRef } from "../../src/runtime.js";
import {
  buildCountedRoot,
  keyValuePhasProof,
  keyValuePhasRootWithCount,
} from "../../src/transition-trace/phas.js";
import {
  prepareWithdrawalMistagV1,
  submitRemoveWithdrawalMistagFraudulentBlock,
  submitWithdrawalMistagInit,
  submitWithdrawalMistagStep01,
  submitWithdrawalMistagStep02,
  submitWithdrawalMistagStep03,
  submitWithdrawalMistagStep04,
  submitWithdrawalMistagStep05,
  type WithdrawalMistagCatalogueCategoryV1,
} from "../../src/withdrawal-mistag/index.js";
import {
  alignUnixTimeToEmulatorSlotBoundary,
  buildRemovalDeploymentInfo,
  funderPaymentKeyHash,
  makeFaultProofEmulatorHarnessV1,
  makeHeader,
  network,
  publishPlainReferenceScriptUtxo,
  publishRemovalReferenceScripts,
  submitSetupTx,
  WITHDRAWAL_MISTAG_REMOVAL_DEPLOYMENT_ENTRY_V1,
} from "./submit-init-emulator-shared.js";

export type WithdrawalMistagDirectionFixtureV1 =
  | "valid-marked-invalid"
  | "invalid-marked-valid";

const buildEvidenceMaterialV1 = async (
  direction: WithdrawalMistagDirectionFixtureV1,
) => {
  const privateKey = CML.PrivateKey.generate_ed25519();
  const publicKey = privateKey.to_public();
  const owner = publicKey.hash().to_hex();
  const withdrawalId: SDK.OutputReference = {
    transactionId:
      direction === "valid-marked-invalid" ? "41".repeat(32) : "42".repeat(32),
    outputIndex: 0n,
  };
  const lovelace = direction === "valid-marked-invalid" ? 1_000_000n : 1n;
  const body: SDK.WithdrawalBody = {
    l2_outref: withdrawalId,
    l2_owner: owner,
    l2_value: new Map([["", new Map([["", lovelace]])]]),
    l1_address: {
      paymentCredential: { PublicKeyCredential: [owner] },
      stakeCredential: null,
    },
    l1_datum: "NoDatum",
  };
  const message = computeHash32(
    Buffer.concat([
      Buffer.from("MidgardWithdrawalV1", "utf8"),
      Buffer.from(SDK.withdrawalBodyBytesV1(body), "hex"),
    ]),
  );
  const info: SDK.WithdrawalInfo = {
    body,
    signature: [
      Buffer.from(publicKey.to_raw_bytes()).toString("hex"),
      privateKey.sign(message).to_hex(),
    ],
    validity:
      direction === "valid-marked-invalid"
        ? "UnpayableWithdrawalValue"
        : "WithdrawalIsValid",
  };

  const outputCbor = encodeMidgardTxOutput({
    address: Buffer.concat([Buffer.from([0x60]), Buffer.from(owner, "hex")]),
    value: { lovelace, assets: new Map() },
  });
  const material = buildCanonicalMidgardLedgerOutputMaterialV1({
    outputIndex: 0,
    outputCbor,
  });
  const ledgerKey = encodeMidgardSpendInputItemV1({
    txId: Buffer.from(withdrawalId.transactionId, "hex"),
    outputIndex: 0,
  });
  const ledger = await keyValuePhasRootWithCount([
    { key: ledgerKey, value: material.descriptorCbor },
  ]);
  const ledgerProof = await keyValuePhasProof(
    ledger,
    ledgerKey,
    material.descriptorCbor,
  );

  const sourceKey = Buffer.from(
    SDK.committedWithdrawalKeyBytesV1(withdrawalId),
    "hex",
  );
  const sourceValue = Buffer.from(
    SDK.committedWithdrawalValueBytesV1(info),
    "hex",
  );
  const source = await buildCountedRoot(SDK.ROOT_DOMAINS.withdrawals, [
    { key: sourceKey, value: sourceValue },
  ]);
  const sourceProof = await keyValuePhasProof(
    { ...source, root: source.phasRoot },
    sourceKey,
    sourceValue,
  );

  const eventKey: SDK.EventKey = {
    WithdrawalEventKey: { withdrawal_id: withdrawalId },
  };
  const eventValue: SDK.EventToStepValue = {
    step_index: 0n,
    phase: "Withdrawal",
  };
  const eventKeyBytes = Buffer.from(Data.to(eventKey, SDK.EventKey), "hex");
  const eventValueBytes = Buffer.from(
    Data.to(eventValue, SDK.EventToStepValue),
    "hex",
  );
  const event = await buildCountedRoot(SDK.ROOT_DOMAINS.eventToStep, [
    { key: eventKeyBytes, value: eventValueBytes },
  ]);
  const eventProof = await keyValuePhasProof(
    { ...event, root: event.phasRoot },
    eventKeyBytes,
    eventValueBytes,
  );

  const transitionValue: SDK.TransitionStep = {
    schema_version: SDK.TRANSITION_STEP_V1_SCHEMA_VERSION,
    step_index: 0n,
    event_key: eventKey,
    phase: "Withdrawal",
    pre_utxos_root: ledger.root,
    post_utxos_root: SDK.EMPTY_MERKLE_TREE_ROOT,
  };
  const transitionKeyBytes = Buffer.from(Data.to(0n), "hex");
  const transitionValueBytes = Buffer.from(
    Data.to(transitionValue, SDK.TransitionStep),
    "hex",
  );
  const trace = await buildCountedRoot(SDK.ROOT_DOMAINS.transitionTrace, [
    { key: transitionKeyBytes, value: transitionValueBytes },
  ]);
  const traceProof = await keyValuePhasProof(
    { ...trace, root: trace.phasRoot },
    transitionKeyBytes,
    transitionValueBytes,
  );

  return {
    source,
    event,
    trace,
    ledger,
    args: {
      committedWithdrawal: {
        domain: SDK.ROOT_DOMAINS.withdrawals,
        root: source.root,
        phas_root: source.phasRoot,
        count: source.count,
        key: withdrawalId,
        value: info,
        proof: sourceProof,
      },
      eventToStep: {
        domain: SDK.ROOT_DOMAINS.eventToStep,
        root: event.root,
        phas_root: event.phasRoot,
        count: event.count,
        key: eventKey,
        value: eventValue,
        proof: eventProof,
      },
      transitionStep: {
        domain: SDK.ROOT_DOMAINS.transitionTrace,
        root: trace.root,
        phas_root: trace.phasRoot,
        count: trace.count,
        key: 0n,
        value: transitionValue,
        proof: traceProof,
      },
      ledgerEvidence: {
        PresentLedgerOutput: {
          output_cbor: outputCbor.toString("hex"),
          membership_proof: ledgerProof,
        },
      } as SDK.WithdrawalMistagLedgerEvidenceV1,
    },
  };
};

export const makeWithdrawalMistagEmulatorHarnessV1 = async () => {
  const harness = await makeFaultProofEmulatorHarnessV1({
    contractOptions: {
      realWithdrawalMistag: true,
      alwaysFraudProofCatalogue: true,
    },
  });
  const withdrawalMistag = harness.contracts.withdrawalMistag;
  const rawCategory = harness.catalogue.extraCategories.withdrawalMistag;
  if (withdrawalMistag === undefined || rawCategory === undefined) {
    throw new Error(
      "Harness did not build withdrawal-mistag contracts/category",
    );
  }
  if (rawCategory.categoryId !== SDK.WITHDRAWAL_MISTAG_TEST_CATEGORY_ID_V1) {
    throw new Error("Unexpected withdrawal-mistag test category id");
  }
  const category: WithdrawalMistagCatalogueCategoryV1 = {
    ...rawCategory,
    categoryId: SDK.WITHDRAWAL_MISTAG_TEST_CATEGORY_ID_V1,
  };
  return { ...harness, withdrawalMistag, category };
};

export const setupWithdrawalMistagScenarioV1 = async ({
  harness,
  direction,
}: {
  readonly harness: Awaited<
    ReturnType<typeof makeWithdrawalMistagEmulatorHarnessV1>
  >;
  readonly direction: WithdrawalMistagDirectionFixtureV1;
}) => {
  const material = await buildEvidenceMaterialV1(direction);
  const operatorVkey = await funderPaymentKeyHash(harness.funderLucid);
  const startTime =
    alignUnixTimeToEmulatorSlotBoundary(
      harness.funderLucid,
      harness.emulator.now() + 120_000,
    ) - 1;
  const header: SDK.HeaderV1 = {
    ...makeHeader(operatorVkey, startTime),
    withdrawalsRoot: material.source.root,
    withdrawalCount: material.source.count,
    totalEventCount: material.source.count,
    transitionStepCount: material.trace.count,
    eventToStepRoot: material.event.root,
    transitionTraceRoot: material.trace.root,
  };
  const setup = await submitSetupTx({
    lucid: harness.funderLucid,
    contracts: harness.contracts,
    nonceUtxo: harness.nonceUtxo,
    catalogue: harness.catalogue,
    header,
  });
  const prepared = await prepareWithdrawalMistagV1({
    challengedHeaderHash: setup.headerHash,
    ...material.args,
  });
  return { header, setup, prepared };
};

export const publishWithdrawalMistagScriptsV1 = async ({
  harness,
}: {
  readonly harness: Awaited<
    ReturnType<typeof makeWithdrawalMistagEmulatorHarnessV1>
  >;
}): Promise<readonly [UTxO, UTxO, UTxO, UTxO, UTxO]> => {
  const refs: UTxO[] = [];
  for (const [index, step] of harness.withdrawalMistag.steps.entries()) {
    const { utxo } = await publishPlainReferenceScriptUtxo({
      lucid: harness.funderLucid,
      script: step.spendingScript as Script,
      label: `withdrawal-mistag step-0${(index + 1).toString()}`,
      oversized: true,
    });
    refs.push(utxo);
  }
  return refs as unknown as readonly [UTxO, UTxO, UTxO, UTxO, UTxO];
};

export const driveWithdrawalMistagToFraudV1 = async ({
  harness,
  scenario,
  refs,
}: {
  readonly harness: Awaited<
    ReturnType<typeof makeWithdrawalMistagEmulatorHarnessV1>
  >;
  readonly scenario: Awaited<
    ReturnType<typeof setupWithdrawalMistagScenarioV1>
  >;
  readonly refs: readonly [UTxO, UTxO, UTxO, UTxO, UTxO];
}) => {
  const stage = async <T>(label: string, run: () => Promise<T>): Promise<T> => {
    try {
      return await run();
    } catch (error) {
      throw new Error(
        `withdrawal-mistag ${label} failed: ${JSON.stringify(error)}`,
      );
    }
  };
  const init = await stage("init", () =>
    initWithdrawalMistagThreadV1({ harness, scenario }),
  );
  const blockUtxo = await withdrawalMistagBlockUtxoV1({ harness, scenario });
  const step01 = await stage("step-01", () =>
    submitWithdrawalMistagStep01({
      lucid: harness.proverLucid,
      contracts: harness.withdrawalMistag,
      signer: harness.proverSigner,
      prepared: scenario.prepared,
      threadOutRef: init.nextThreadOutRef,
      hubOracleUtxo: scenario.setup.hubOracle,
      stateQueueBlockUtxo: blockUtxo,
      referenceScriptUtxo: refs[0],
    }),
  );
  const step02 = await stage("step-02", () =>
    submitWithdrawalMistagStep02({
      lucid: harness.proverLucid,
      contracts: harness.withdrawalMistag,
      signer: harness.proverSigner,
      prepared: scenario.prepared,
      threadOutRef: step01.nextThreadOutRef,
      referenceScriptUtxo: refs[1],
    }),
  );
  const step03 = await stage("step-03", () =>
    submitWithdrawalMistagStep03({
      lucid: harness.proverLucid,
      contracts: harness.withdrawalMistag,
      signer: harness.proverSigner,
      prepared: scenario.prepared,
      threadOutRef: step02.nextThreadOutRef,
      referenceScriptUtxo: refs[2],
    }),
  );
  const step04 = await stage("step-04", () =>
    submitWithdrawalMistagStep04({
      lucid: harness.proverLucid,
      contracts: harness.withdrawalMistag,
      signer: harness.proverSigner,
      prepared: scenario.prepared,
      threadOutRef: step03.nextThreadOutRef,
      referenceScriptUtxo: refs[3],
    }),
  );
  const fraud = await stage("step-05", () =>
    submitWithdrawalMistagStep05({
      lucid: harness.proverLucid,
      contracts: harness.withdrawalMistag,
      signer: harness.proverSigner,
      prepared: scenario.prepared,
      threadOutRef: step04.nextThreadOutRef,
      referenceScriptUtxo: refs[4],
    }),
  );
  return { init, step01, step02, step03, step04, fraud };
};

export const initWithdrawalMistagThreadV1 = async ({
  harness,
  scenario,
}: {
  readonly harness: Awaited<
    ReturnType<typeof makeWithdrawalMistagEmulatorHarnessV1>
  >;
  readonly scenario: Awaited<
    ReturnType<typeof setupWithdrawalMistagScenarioV1>
  >;
}) =>
  await submitWithdrawalMistagInit({
    lucid: harness.proverLucid,
    blueprint: harness.realBlueprint,
    network,
    contracts: harness.withdrawalMistag,
    category: harness.category,
    catalogue: {
      policyId: harness.contracts.fraudProofCatalogue.policyId,
      spendingScriptAddress:
        harness.contracts.fraudProofCatalogue.spendingScriptAddress,
      root: harness.catalogue.root,
    },
    signer: harness.proverSigner,
    fraudulentBlockOutRef: scenario.setup.fraudulentBlockOutRef,
  });

export const withdrawalMistagBlockUtxoV1 = async ({
  harness,
  scenario,
}: {
  readonly harness: Awaited<
    ReturnType<typeof makeWithdrawalMistagEmulatorHarnessV1>
  >;
  readonly scenario: Awaited<
    ReturnType<typeof setupWithdrawalMistagScenarioV1>
  >;
}) =>
  await fetchUtxoByOutRef({
    lucid: harness.proverLucid,
    outRef: parseOutRef(
      scenario.setup.fraudulentBlockOutRef,
      "fraudulent block",
    ),
    label: "withdrawal-mistag fraudulent block",
  });

export const removeWithdrawalMistagBlockV1 = async ({
  harness,
  scenario,
}: {
  readonly harness: Awaited<
    ReturnType<typeof makeWithdrawalMistagEmulatorHarnessV1>
  >;
  readonly scenario: Awaited<
    ReturnType<typeof setupWithdrawalMistagScenarioV1>
  >;
}) => {
  const removalReferenceScripts = await publishRemovalReferenceScripts({
    lucid: harness.proverLucid,
    contracts: harness.contracts,
  });
  const deploymentInfo = buildRemovalDeploymentInfo(
    harness.contracts,
    harness.catalogue,
    { removalReferenceScripts: removalReferenceScripts.published },
  );
  const now = BigInt(harness.emulator.now());
  return await submitRemoveWithdrawalMistagFraudulentBlock({
    lucid: harness.proverLucid,
    blueprint: harness.realBlueprint,
    deploymentInfo,
    network,
    signer: harness.proverSigner,
    contracts: harness.withdrawalMistag,
    firstStepDeploymentEntry: WITHDRAWAL_MISTAG_REMOVAL_DEPLOYMENT_ENTRY_V1,
    fraudulentHeaderHash: scenario.setup.headerHash,
    awaitConfirmation: true,
    requireReferenceScripts: true,
    validFrom: now > 120_000n ? now - 120_000n : 0n,
    validTo: now + 300_000n,
  });
};
