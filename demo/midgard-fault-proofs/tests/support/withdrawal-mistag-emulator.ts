import { computeHash32 } from "@al-ft/midgard-core";
import {
  encodeMidgardSpendInputItem,
  encodeMidgardTxOutput,
} from "@al-ft/midgard-core/codec";
import * as SDK from "@al-ft/midgard-sdk";
import { buildCanonicalMidgardLedgerOutputMaterial } from "@al-ft/midgard-validation";
import { CML, Data, type Script, type UTxO } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import { fetchUtxoByOutRef, parseOutRef } from "../../src/runtime.js";
import {
  buildCountedRoot,
  keyValuePhasProof,
  keyValuePhasRootWithCount,
} from "../../src/transition-trace/phas.js";
import {
  prepareWithdrawalMistag,
  submitRemoveWithdrawalMistagFraudulentBlock,
  submitWithdrawalMistagInit,
  submitWithdrawalMistagStep01,
  submitWithdrawalMistagStep02,
  submitWithdrawalMistagStep03,
  submitWithdrawalMistagStep04,
  submitWithdrawalMistagStep05,
  type WithdrawalMistagCatalogueCategory,
} from "../../src/withdrawal-mistag/index.js";
import {
  captureEmulatorSubmission,
  type CompleteSignedTransactionMeasurement,
} from "./emulator/measurement.js";
import {
  alignUnixTimeToEmulatorSlotBoundary,
  buildRemovalDeploymentInfo,
  funderPaymentKeyHash,
  makeFaultProofEmulatorHarness,
  makeHeader,
  network,
  publishPlainReferenceScriptUtxo,
  publishRemovalReferenceScripts,
  submitSetupTx,
  WITHDRAWAL_MISTAG_REMOVAL_DEPLOYMENT_ENTRY,
} from "./submit-init-emulator-shared.js";
import { syntheticDeepMembershipProof } from "./synthetic-deep-proof.js";

export type WithdrawalMistagDirectionFixture =
  | "valid-marked-invalid"
  | "invalid-marked-valid";

export const buildWithdrawalMistagEvidenceMaterial = async (
  direction: WithdrawalMistagDirectionFixture,
  honest = false,
  outputBytes = 0,
  assetCount = 0,
  payoutDatumBytes = 0,
  proofLevels = 0,
  maximumAssetNames = false,
) => {
  const privateKey = CML.PrivateKey.generate_ed25519();
  const publicKey = privateKey.to_public();
  const owner = publicKey.hash().to_hex();
  const withdrawalId: SDK.OutputReference = {
    transactionId:
      direction === "valid-marked-invalid" ? "41".repeat(32) : "42".repeat(32),
    outputIndex: 0n,
  };
  const lovelace =
    assetCount > 0
      ? 100_000_000n
      : direction === "valid-marked-invalid"
        ? 1_000_000n
        : 1n;
  const tokenEntries = Array.from({ length: assetCount }, (_, i) => {
    const name =
      assetCount === 1304
        ? i === 0
          ? ""
          : i <= 256
            ? (i - 1).toString(16).padStart(2, "0")
            : (i - 257).toString(16).padStart(4, "0")
        : maximumAssetNames
          ? i.toString(16).padStart(64, "0")
          : proofLevels > 0
            ? (255 - Math.floor(i / 32)).toString(16) + "00".repeat(i % 32)
            : i.toString(16).padStart(4, "0");
    return [name, assetCount === 1304 && i === 1303 ? 256n : 1n] as const;
  });
  const tokenAssets =
    assetCount === 0
      ? new Map<string, Map<string, bigint>>()
      : new Map([["aa".repeat(28), new Map(tokenEntries)]]);
  const body: SDK.WithdrawalBody = {
    l2_outref: withdrawalId,
    l2_owner: owner,
    l2_value: new Map([["", new Map([["", lovelace]])], ...tokenAssets]),
    l1_address: {
      paymentCredential: { PublicKeyCredential: [owner] },
      stakeCredential: null,
    },
    l1_datum:
      payoutDatumBytes === 0
        ? "NoDatum"
        : {
            InlineDatum: {
              data: Array.from(
                { length: Math.ceil(payoutDatumBytes / 64) },
                (_, i) => "ab".repeat(Math.min(64, payoutDatumBytes - i * 64)),
              ),
            },
          },
  };
  const message = computeHash32(
    Buffer.concat([
      Buffer.from("MidgardWithdrawalV1", "utf8"),
      Buffer.from(SDK.withdrawalBodyBytes(body), "hex"),
    ]),
  );
  const info: SDK.WithdrawalInfo = {
    body,
    signature: [
      Buffer.from(publicKey.to_raw_bytes()).toString("hex"),
      privateKey.sign(message).to_hex(),
    ],
    validity:
      (direction === "valid-marked-invalid") !== honest
        ? "UnpayableWithdrawalValue"
        : "WithdrawalIsValid",
  };

  let padding = 0;
  const output = () =>
    encodeMidgardTxOutput({
      address: Buffer.concat([Buffer.from([0x60]), Buffer.from(owner, "hex")]),
      value: { lovelace, assets: tokenAssets },
      ...(outputBytes === 0
        ? {}
        : {
            script_ref: {
              language: "PlutusV3" as const,
              scriptBytes: Buffer.alloc(padding, 1),
            },
          }),
    });
  let outputCbor = output();
  if (outputBytes !== 0) {
    for (let i = 0; i < 4 && outputCbor.length !== outputBytes; i++) {
      padding += outputBytes - outputCbor.length;
      outputCbor = output();
    }
    if (outputCbor.length !== outputBytes)
      throw new Error("withdrawal maximum output fixture length mismatch");
  }

  const material = buildCanonicalMidgardLedgerOutputMaterial({
    outputIndex: 0,
    outputCbor,
  });
  if (assetCount === 1304 && material.descriptor.cardanoValueSize !== 5000)
    throw new Error("withdrawal maximum Value fixture mismatch");
  const ledgerKey = encodeMidgardSpendInputItem({
    txId: Buffer.from(withdrawalId.transactionId, "hex"),
    outputIndex: 0,
  });
  let ledger = await keyValuePhasRootWithCount([
    { key: ledgerKey, value: material.descriptorCbor },
  ]);
  let ledgerProof = await keyValuePhasProof(
    ledger,
    ledgerKey,
    material.descriptorCbor,
  );

  if (proofLevels > 0) {
    const deep = syntheticDeepMembershipProof({
      key: ledgerKey,
      value: material.descriptorCbor,
      branchLevels: proofLevels,
    });
    ledger = { ...ledger, root: deep.transactionsPhasRoot };
    ledgerProof = Data.from(deep.proofCbor, SDK.Proof);
  }
  const countedMembership = async (
    domain: SDK.RootDomain,
    key: Buffer,
    value: Buffer,
  ) => {
    const counted = await buildCountedRoot(domain, [{ key, value }]);
    if (proofLevels === 0)
      return {
        counted,
        proof: await keyValuePhasProof(
          { ...counted, root: counted.phasRoot },
          key,
          value,
        ),
      };
    const deep = syntheticDeepMembershipProof({
      key,
      value,
      branchLevels: proofLevels,
    });
    const phasRoot = deep.transactionsPhasRoot;
    return {
      counted: {
        ...counted,
        phasRoot,
        root: await Effect.runPromise(
          SDK.commitCountedRootProgram({
            domain,
            phasRoot,
            count: counted.count,
          }),
        ),
      },
      proof: Data.from(deep.proofCbor, SDK.Proof),
    };
  };

  const sourceKey = Buffer.from(
    SDK.committedWithdrawalKeyBytes(withdrawalId),
    "hex",
  );
  const sourceValue = Buffer.from(
    SDK.committedWithdrawalValueBytes(info),
    "hex",
  );
  const { counted: source, proof: sourceProof } = await countedMembership(
    SDK.ROOT_DOMAINS.withdrawals,
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
  const { counted: event, proof: eventProof } = await countedMembership(
    SDK.ROOT_DOMAINS.eventToStep,
    eventKeyBytes,
    eventValueBytes,
  );

  const transitionValue: SDK.TransitionStep = {
    schema_version: SDK.TRANSITION_STEP_SCHEMA_VERSION,
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
  const { counted: trace, proof: traceProof } = await countedMembership(
    SDK.ROOT_DOMAINS.transitionTrace,
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
      },
    },
  };
};

export const makeWithdrawalMistagEmulatorHarness = async () => {
  const harness = await makeFaultProofEmulatorHarness({
    contractOptions: {
      realWithdrawalMistag: true,
      alwaysFraudProofCatalogue: true,
    },
  });
  const withdrawalMistag = harness.contracts.withdrawalMistag;
  const rawCategory = harness.catalogue.categories.withdrawalMistag;
  if (withdrawalMistag === undefined || rawCategory === undefined) {
    throw new Error(
      "Harness did not build withdrawal-mistag contracts/category",
    );
  }
  if (rawCategory.categoryId !== SDK.WITHDRAWAL_MISTAG_FRAUD_CATEGORY_ID) {
    throw new Error("Unexpected withdrawal-mistag category id");
  }
  const category: WithdrawalMistagCatalogueCategory = {
    ...rawCategory,
    categoryId: SDK.WITHDRAWAL_MISTAG_FRAUD_CATEGORY_ID,
  };
  return { ...harness, withdrawalMistag, category };
};

export const setupWithdrawalMistagScenario = async ({
  harness,
  direction,
  outputBytes = 0,
  assetCount = 0,
  payoutDatumBytes = 0,
  proofLevels = 0,
  maximumAssetNames = false,
}: {
  readonly harness: Awaited<
    ReturnType<typeof makeWithdrawalMistagEmulatorHarness>
  >;
  readonly direction: WithdrawalMistagDirectionFixture;
  readonly outputBytes?: number;
  readonly assetCount?: number;
  readonly payoutDatumBytes?: number;
  readonly proofLevels?: number;
  readonly maximumAssetNames?: boolean;
}) => {
  const material = await buildWithdrawalMistagEvidenceMaterial(
    direction,
    false,
    outputBytes,
    assetCount,
    payoutDatumBytes,
    proofLevels,
    maximumAssetNames,
  );
  const operatorVkey = await funderPaymentKeyHash(harness.funderLucid);
  const startTime =
    alignUnixTimeToEmulatorSlotBoundary(
      harness.funderLucid,
      harness.emulator.now() + 120_000,
    ) - 1;
  const header: SDK.Header = {
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
  const prepared = await prepareWithdrawalMistag({
    challengedHeaderHash: setup.headerHash,
    ...material.args,
  });
  return { header, setup, prepared };
};

export const publishWithdrawalMistagScripts = async ({
  harness,
}: {
  readonly harness: Awaited<
    ReturnType<typeof makeWithdrawalMistagEmulatorHarness>
  >;
}) => {
  const refs: UTxO[] = [];
  const publicationMeasurements = [];
  for (const [index, step] of harness.withdrawalMistag.steps.entries()) {
    const { utxo, publicationMeasurement } =
      await publishPlainReferenceScriptUtxo({
        lucid: harness.funderLucid,
        script: step.spendingScript as Script,
        label: `withdrawal-mistag step-0${(index + 1).toString()}`,
      });
    if (publicationMeasurement.l1ByteMargin < 1_024) {
      throw new Error(
        `withdrawal-mistag step-0${(index + 1).toString()} publication has only ${publicationMeasurement.l1ByteMargin.toString()} bytes of L1 headroom`,
      );
    }
    refs.push(utxo);
    publicationMeasurements.push(publicationMeasurement);
  }
  return {
    refs: refs as unknown as readonly [UTxO, UTxO, UTxO, UTxO, UTxO],
    publicationMeasurements,
  };
};

export const driveWithdrawalMistagToFraud = async ({
  harness,
  scenario,
  refs,
  evidenceReferences,
}: {
  readonly harness: Awaited<
    ReturnType<typeof makeWithdrawalMistagEmulatorHarness>
  >;
  readonly scenario: Awaited<ReturnType<typeof setupWithdrawalMistagScenario>>;
  readonly evidenceReferences?: readonly (readonly UTxO[])[];
  readonly refs: readonly [UTxO, UTxO, UTxO, UTxO, UTxO];
}) => {
  const transactionMeasurements: Record<
    string,
    CompleteSignedTransactionMeasurement
  > = {};
  const stage = async <T>(label: string, run: () => Promise<T>): Promise<T> => {
    try {
      const captured = await captureEmulatorSubmission(harness.emulator, run);
      const measurement = captured.measurement;
      const { maxTxExMem, maxTxExSteps } = harness.emulator.protocolParameters;
      if (
        measurement.l1ByteMargin <= 0 ||
        measurement.executionMemory > maxTxExMem ||
        measurement.executionSteps > maxTxExSteps
      ) {
        throw new Error(
          `${label} exceeds the real L1 transaction envelope: ${JSON.stringify({
            completeSignedBytes: measurement.completeSignedBytes,
            l1ByteMargin: measurement.l1ByteMargin,
            executionMemory: measurement.executionMemory.toString(),
            maxTxExMem: maxTxExMem.toString(),
            executionSteps: measurement.executionSteps.toString(),
            maxTxExSteps: maxTxExSteps.toString(),
          })}`,
        );
      }
      transactionMeasurements[label] = measurement;
      return captured.result;
    } catch (error) {
      throw new Error(
        `withdrawal-mistag ${label} failed: ${JSON.stringify(error)}`,
      );
    }
  };
  const init = await stage("init", () =>
    initWithdrawalMistagThread({ harness, scenario }),
  );
  const blockUtxo = await withdrawalMistagBlockUtxo({ harness, scenario });
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
      evidenceReferences: evidenceReferences?.[0],
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
      evidenceReferences: evidenceReferences?.[1],
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
      evidenceReferences: evidenceReferences?.[2],
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
      evidenceReferences: evidenceReferences?.[3],
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
      witnessReferenceScripts: harness.witnessReferenceScripts,
    }),
  );
  return {
    init,
    step01,
    step02,
    step03,
    step04,
    fraud,
    transactionMeasurements,
  };
};

export const initWithdrawalMistagThread = async ({
  harness,
  scenario,
}: {
  readonly harness: Awaited<
    ReturnType<typeof makeWithdrawalMistagEmulatorHarness>
  >;
  readonly scenario: Awaited<ReturnType<typeof setupWithdrawalMistagScenario>>;
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
    witnessReferenceScripts: harness.witnessReferenceScripts,
  });

export const withdrawalMistagBlockUtxo = async ({
  harness,
  scenario,
}: {
  readonly harness: Awaited<
    ReturnType<typeof makeWithdrawalMistagEmulatorHarness>
  >;
  readonly scenario: Awaited<ReturnType<typeof setupWithdrawalMistagScenario>>;
}) =>
  await fetchUtxoByOutRef({
    lucid: harness.proverLucid,
    outRef: parseOutRef(
      scenario.setup.fraudulentBlockOutRef,
      "fraudulent block",
    ),
    label: "withdrawal-mistag fraudulent block",
  });

export const removeWithdrawalMistagBlock = async ({
  harness,
  scenario,
}: {
  readonly harness: Awaited<
    ReturnType<typeof makeWithdrawalMistagEmulatorHarness>
  >;
  readonly scenario: Awaited<ReturnType<typeof setupWithdrawalMistagScenario>>;
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
    firstStepDeploymentEntry: WITHDRAWAL_MISTAG_REMOVAL_DEPLOYMENT_ENTRY,
    fraudulentHeaderHash: scenario.setup.headerHash,
    awaitConfirmation: true,
    requireReferenceScripts: true,
    validFrom: now > 120_000n ? now - 120_000n : 0n,
    validTo: now + 300_000n,
  });
};
