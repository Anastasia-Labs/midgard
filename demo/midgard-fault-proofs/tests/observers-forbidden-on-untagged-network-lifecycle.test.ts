import { Store, Trie } from "@aiken-lang/merkle-patricia-forestry";
import {
  adjudicateMidgardNativeTxFullV1Validity,
  computeMidgardNativeTxIdV1,
  deriveMidgardNativeTxProofSourceV1,
  deriveMidgardNativeTxWitnessSetCompactV1,
  encodeMidgardFieldPreimageV1,
  encodeMidgardNativeTxCompactV1,
  encodeMidgardNativeTxWitnessSetCompactV1,
  materializeMidgardNativeTxFromCanonicalV1,
  midgardFieldCommitmentV1,
} from "@al-ft/midgard-core";
import {
  acceptedVerdictSubjectV1,
  AddressData,
  addressDataFromBech32,
  ForcedInclusionTxV1Schema,
  forcedVerdictSubjectV1,
  OutputReference,
  Proof,
  ROOT_DOMAINS,
} from "@al-ft/midgard-sdk";
import { Data, getAddressDetails, type UTxO } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { describe, expect, it, vi } from "vitest";

import { submitCommittedFieldShapeInit } from "../src/committed-field-shape/submit-committed-field-shape-init.js";
import {
  certifyFaultProofFieldCarriageV1,
  planFaultProofFieldOpeningV1,
  publishFaultProofFieldCarriageV1,
} from "../src/field-opening-v1.js";
import {
  applyObserversForbiddenScriptsV1,
  type ObserversForbiddenContractsV1,
} from "../src/observers-forbidden-on-untagged-network/contracts-v1.js";
import {
  OBSERVERS_FORBIDDEN_ON_UNTAGGED_NETWORK_CATEGORY_ID_V1,
  prepareObserversForbiddenEvidenceV1,
} from "../src/observers-forbidden-on-untagged-network/family-v1.js";
import { createObserversForbiddenActuatorV1 } from "../src/observers-forbidden-on-untagged-network/production-actuator-v1.js";
import { buildProductionObserversForbiddenArtifactV1 } from "../src/observers-forbidden-on-untagged-network/production-artifact-v1.js";
import { submitObserversForbiddenCancelV1 } from "../src/observers-forbidden-on-untagged-network/submit-cancel-v1.js";
import {
  submitObserversForbiddenStep01AcceptedV1,
  submitObserversForbiddenStep01ForcedV1,
} from "../src/observers-forbidden-on-untagged-network/submit-step-01-v1.js";
import { submitObserversForbiddenStep02V1 } from "../src/observers-forbidden-on-untagged-network/submit-step-02-v1.js";
import { nativeTxFromCoreCompact } from "../src/submit-step-01.js";
import { buildCountedRoot } from "../src/transition-trace/phas.js";
import { submitCapturedTransactionV1 } from "../src/workflow/transaction-boundary-v1.js";
import { buildCatalogueDeploymentInfo } from "./support/emulator/catalogue.js";
import { alignUnixTimeToEmulatorSlotBoundary } from "./support/emulator/emulator-context.js";
import { makeFaultProofEmulatorHarnessV1 } from "./support/emulator/harness.js";
import { captureEmulatorSubmission } from "./support/emulator/measurement.js";
import {
  l2TransactionSourceCborV1,
  makeNativeTx,
} from "./support/emulator/native-tx.js";
import { publishPlainReferenceScriptUtxo } from "./support/emulator/reference-scripts.js";
import { buildRemovalDeploymentInfo } from "./support/emulator/removal-deployment.js";
import { submitSetupTx } from "./support/emulator/setup-tx.js";
import {
  buildInvalidForcedTransitionTraceFixture,
  setupFraudulentBlockV1,
} from "./support/submit-init-emulator-fixtures.js";
import { publishRemovalReferenceScripts } from "./support/submit-init-emulator-shared.js";

const network = "Custom" as const;

const runForcedContradiction = async ({
  networkId,
  observerCount,
}: {
  readonly networkId: 0 | 1 | 255;
  readonly observerCount: number;
}) => {
  const harness = await makeFaultProofEmulatorHarnessV1({
    contractOptions: {
      alwaysFraudProofCatalogue: true,
      alwaysStateQueue: true,
    },
  });
  const addressData = await Effect.runPromise(
    addressDataFromBech32(
      harness.contracts.fraudProof.spendingScriptAddress,
    ).pipe(Effect.map((address) => Data.from(Data.to(address, AddressData)))),
  );
  const applied = applyObserversForbiddenScriptsV1({
    blueprint: harness.realBlueprint,
    network,
    computationThreadPolicyId: harness.contracts.computationThread.policyId,
    fraudProofPolicyId: harness.contracts.fraudProof.policyId,
    fraudProofTokenAddressData: addressData,
    fieldPreimageCertificatePolicyId:
      harness.contracts.fieldPreimageCertificate.policyId,
    hubOracleScriptHash: harness.contracts.hubOracle.spendingScriptHash,
  });
  const contracts: ObserversForbiddenContractsV1 = {
    steps: applied,
    computationThread: harness.contracts.computationThread,
    fraudProof: harness.contracts.fraudProof,
    hubOraclePolicyId: harness.contracts.hubOracle.policyId,
    stateQueuePolicyId: harness.contracts.stateQueue.policyId,
    fieldPreimageCertificatePolicyId:
      harness.contracts.fieldPreimageCertificate.policyId,
    fieldPreimageCertificateMintingScript:
      harness.contracts.fieldPreimageCertificate.mintingScript,
  };
  const catalogue = await buildCatalogueDeploymentInfo(
    harness.contracts.fraudProofs,
    {
      observersForbiddenOnUntaggedNetwork: {
        categoryId: OBSERVERS_FORBIDDEN_ON_UNTAGGED_NETWORK_CATEGORY_ID_V1,
        scriptHash: applied[0].spendingScriptHash,
      },
    },
  );
  const category =
    catalogue.extraCategories.observersForbiddenOnUntaggedNetwork!;
  const credential = getAddressDetails(
    await harness.funderLucid.wallet().address(),
  ).paymentCredential;
  if (credential?.type !== "Key") throw new Error("forced funder key absent");
  const baseFixture = await buildInvalidForcedTransitionTraceFixture({
    operatorVkey: credential.hash,
    now:
      alignUnixTimeToEmulatorSlotBoundary(
        harness.funderLucid,
        harness.emulator.now() + 120_000,
      ) - 1,
  });
  const observerField = encodeMidgardFieldPreimageV1(
    Array.from({ length: observerCount }, (_, index) =>
      Buffer.alloc(28, index + 1),
    ),
  );
  const base = makeNativeTx({ spendInputCbors: [], fee: 0n });
  const nativeTx = materializeMidgardNativeTxFromCanonicalV1({
    version: base.version,
    validity: base.validity,
    body: {
      ...base.body,
      requiredObserversPreimageCbor: observerField,
      networkId: BigInt(networkId),
    },
    witnessSet: base.witnessSet,
  });
  const invalid = adjudicateMidgardNativeTxFullV1Validity(
    nativeTx,
    "TxIsInvalid",
  );
  const transactionId = computeMidgardNativeTxIdV1(invalid).toString("hex");
  const proofSource = deriveMidgardNativeTxProofSourceV1(invalid);
  const forcedTransaction = {
    tx_id: transactionId,
    source: {
      compact_cbor: proofSource.compactCbor.toString("hex"),
      witness_set_compact_cbor:
        proofSource.witnessSetCompactCbor.toString("hex"),
      field_preimage_lengths_cbor:
        proofSource.fieldPreimageLengthsCbor.toString("hex"),
    },
    verdict: {
      ForcedTxInvalid: { reason: "ObserversForbiddenOnUntaggedNetwork" },
    },
  } as const;
  const sourceKey = baseFixture.eventKey.ForcedTransactionEventKey.tx_order_id;
  const keyBytes = Buffer.from(Data.to(sourceKey, OutputReference), "hex");
  const valueBytes = Buffer.from(
    Data.to(forcedTransaction as never, ForcedInclusionTxV1Schema as never),
    "hex",
  );
  const root = await buildCountedRoot(ROOT_DOMAINS.forcedTransactionsV1, [
    { key: keyBytes, value: valueBytes },
  ]);
  const proofStore = new Store(undefined);
  await proofStore.ready();
  const proofTrie = new Trie(proofStore);
  await proofTrie.insert(keyBytes, valueBytes);
  const membershipProof = await proofTrie.prove(keyBytes);
  const membership = {
    domain: root.domain,
    root: root.root,
    phas_root: root.phasRoot,
    count: root.count,
    key: sourceKey,
    value: forcedTransaction,
    proof: Data.from(membershipProof.toCBOR().toString("hex"), Proof),
  };
  const header = { ...baseFixture.header, forcedTransactionsRoot: root.root };
  const setup = await submitSetupTx({
    lucid: harness.funderLucid,
    contracts: harness.contracts,
    nonceUtxo: harness.nonceUtxo,
    catalogue,
    header,
  });
  const evidence = prepareObserversForbiddenEvidenceV1({
    finding: {
      subject: forcedVerdictSubjectV1({
        transactionId,
        sourceKey,
        rejectionReason: "ObserversForbiddenOnUntaggedNetwork",
      }),
      networkId,
    },
    observerFieldPreimage: observerField,
    committedFieldHashHex:
      midgardFieldCommitmentV1(observerField).toString("hex"),
  });
  const references: UTxO[] = [];
  for (const [index, step] of applied.entries())
    references.push(
      (
        await publishPlainReferenceScriptUtxo({
          lucid: harness.funderLucid,
          script: step.spendingScript,
          label: `observers-forbidden-forced-${networkId.toString()}-${index.toString()}`,
        })
      ).utxo,
    );
  const forcedPlan = planFaultProofFieldOpeningV1({
    fieldIndex: 3,
    anchorTxId: transactionId,
    nativeTxCompactCbor: proofSource.compactCbor.toString("hex"),
    itemCbors: Array.from({ length: observerCount }, (_, index) =>
      Buffer.alloc(28, index + 1),
    ),
    owner: harness.proverSigner.paymentKeyHash,
    publish: true,
    label: "observers forbidden forced field",
  });
  await publishFaultProofFieldCarriageV1({
    lucid: harness.proverLucid,
    signer: harness.proverSigner,
    planned: forcedPlan,
    publisherAddress: harness.proverSigner.address,
    label: "observers forbidden forced field",
  });
  const initialized = await captureEmulatorSubmission(harness.emulator, () =>
    submitCommittedFieldShapeInit({
      lucid: harness.proverLucid,
      blueprint: harness.realBlueprint,
      network,
      contracts: contracts as never,
      category,
      catalogue: {
        policyId: harness.contracts.fraudProofCatalogue.policyId,
        spendingScriptAddress:
          harness.contracts.fraudProofCatalogue.spendingScriptAddress,
        root: catalogue.root,
      },
      signer: harness.proverSigner,
      fraudulentBlockOutRef: setup.fraudulentBlockOutRef,
      fraudulentHeaderHash: setup.headerHash,
      witnessReferenceScripts: harness.witnessReferenceScripts,
    }),
  );
  const threadOutRef = `${initialized.result.txHash}#${initialized.result.firstStepOutputIndex.toString()}`;
  const bound = await captureEmulatorSubmission(harness.emulator, () =>
    submitObserversForbiddenStep01ForcedV1({
      lucid: harness.proverLucid,
      contracts,
      categoryId: category.categoryId,
      signer: harness.proverSigner,
      threadOutRef,
      finding: evidence,
      forcedSource: { header, membership, direction: 1n },
      referenceScriptUtxo: references[0]!,
    }),
  );
  const final = await captureEmulatorSubmission(harness.emulator, () =>
    submitObserversForbiddenStep02V1({
      lucid: harness.proverLucid,
      contracts,
      categoryId: category.categoryId,
      signer: harness.proverSigner,
      threadOutRef: bound.result.nextThreadOutRef,
      evidence,
      nativeTxCompactCbor: proofSource.compactCbor.toString("hex"),
      referenceScriptUtxo: references[1]!,
      witnessReferenceScripts: harness.witnessReferenceScripts,
    }),
  );
  expect(final.result.fraudProofUnit).toBeTruthy();
  for (const captured of [initialized, bound, final]) {
    expect(captured.measurement.l1ByteMargin).toBeGreaterThan(0);
    expect(captured.measurement.executionMemory).toBeGreaterThan(0n);
    expect(captured.measurement.executionSteps).toBeGreaterThan(0n);
  }
};

describe("observersForbiddenOnUntaggedNetwork real lifecycle", () => {
  it("runs maximum certified accepted Init, cancel/restart, and permanent mint", async () => {
    const harness = await makeFaultProofEmulatorHarnessV1({
      contractOptions: { alwaysFraudProofCatalogue: true },
    });
    const addressData = await Effect.runPromise(
      addressDataFromBech32(
        harness.contracts.fraudProof.spendingScriptAddress,
      ).pipe(Effect.map((address) => Data.from(Data.to(address, AddressData)))),
    );
    const applied = applyObserversForbiddenScriptsV1({
      blueprint: harness.realBlueprint,
      network,
      computationThreadPolicyId: harness.contracts.computationThread.policyId,
      fraudProofPolicyId: harness.contracts.fraudProof.policyId,
      fraudProofTokenAddressData: addressData,
      fieldPreimageCertificatePolicyId:
        harness.contracts.fieldPreimageCertificate.policyId,
      hubOracleScriptHash: harness.contracts.hubOracle.spendingScriptHash,
    });
    const contracts: ObserversForbiddenContractsV1 = {
      steps: applied,
      computationThread: harness.contracts.computationThread,
      fraudProof: harness.contracts.fraudProof,
      hubOraclePolicyId: harness.contracts.hubOracle.policyId,
      stateQueuePolicyId: harness.contracts.stateQueue.policyId,
      fieldPreimageCertificatePolicyId:
        harness.contracts.fieldPreimageCertificate.policyId,
      fieldPreimageCertificateMintingScript:
        harness.contracts.fieldPreimageCertificate.mintingScript,
    };
    const catalogue = await buildCatalogueDeploymentInfo(
      harness.contracts.fraudProofs,
      {
        observersForbiddenOnUntaggedNetwork: {
          categoryId: OBSERVERS_FORBIDDEN_ON_UNTAGGED_NETWORK_CATEGORY_ID_V1,
          scriptHash: applied[0].spendingScriptHash,
        },
      },
    );
    const category =
      catalogue.extraCategories.observersForbiddenOnUntaggedNetwork!;

    const observerField = encodeMidgardFieldPreimageV1(
      Array.from({ length: 505 }, (_, index) => Buffer.alloc(28, index + 1)),
    );
    expect(observerField).toHaveLength(15_153);
    const base = makeNativeTx({ spendInputCbors: [], fee: 7n });
    const nativeTx = materializeMidgardNativeTxFromCanonicalV1({
      version: base.version,
      validity: base.validity,
      body: {
        ...base.body,
        requiredObserversPreimageCbor: observerField,
        networkId: 255n,
      },
      witnessSet: base.witnessSet,
    });
    const transactionId = computeMidgardNativeTxIdV1(nativeTx).toString("hex");
    const compact = encodeMidgardNativeTxCompactV1(nativeTx.compact);
    const sourceCbor = l2TransactionSourceCborV1(nativeTx);
    const store = new Store(undefined);
    await store.ready();
    const trie = new Trie(store);
    await trie.insert(
      Buffer.from(transactionId, "hex"),
      Buffer.from(sourceCbor, "hex"),
    );
    const membership = await trie.prove(Buffer.from(transactionId, "hex"));
    const transactionsRoot = Buffer.from(trie.hash).toString("hex");
    const txInclusion = {
      nativeTxId: transactionId,
      nativeTx: nativeTxFromCoreCompact(nativeTx.compact),
      nativeTxCompactCbor: compact.toString("hex"),
      l2TransactionSourceCbor: sourceCbor,
      transactionsPhasRoot: transactionsRoot,
      txMembershipProof: Data.from(membership.toCBOR().toString("hex"), Proof),
      txMembershipProofCbor: membership.toCBOR().toString("hex"),
    };
    const setup = await setupFraudulentBlockV1({
      funderLucid: harness.funderLucid,
      emulator: harness.emulator,
      contracts: harness.contracts,
      catalogue,
      fixture: { transactionsRoot, l2TransactionCount: 1n },
    });
    const evidence = prepareObserversForbiddenEvidenceV1({
      finding: {
        subject: acceptedVerdictSubjectV1(transactionId),
        networkId: 255,
      },
      observerFieldPreimage: observerField,
      committedFieldHashHex:
        midgardFieldCommitmentV1(observerField).toString("hex"),
    });
    expect(evidence.carriage).toBe("Certified");

    const references: UTxO[] = [];
    for (const [index, step] of applied.entries())
      references.push(
        (
          await publishPlainReferenceScriptUtxo({
            lucid: harness.funderLucid,
            script: step.spendingScript,
            label: `observers-forbidden-lifecycle-${index.toString()}`,
          })
        ).utxo,
      );
    const certificateReference = (
      await publishPlainReferenceScriptUtxo({
        lucid: harness.funderLucid,
        script: harness.contracts.fieldPreimageCertificate.mintingScript,
        label: "observers-forbidden-certificate",
      })
    ).utxo;
    const planned = planFaultProofFieldOpeningV1({
      fieldIndex: 3,
      anchorTxId: transactionId,
      nativeTxCompactCbor: compact.toString("hex"),
      itemCbors: Array.from({ length: 505 }, (_, index) =>
        Buffer.alloc(28, index + 1),
      ),
      owner: harness.proverSigner.paymentKeyHash,
      publish: true,
      label: "observers forbidden maximum field",
    });
    const carriage = await captureEmulatorSubmission(harness.emulator, () =>
      publishFaultProofFieldCarriageV1({
        lucid: harness.proverLucid,
        signer: harness.proverSigner,
        planned,
        publisherAddress: harness.proverSigner.address,
        label: "observers forbidden maximum field",
      }),
    );
    const certificate = await captureEmulatorSubmission(harness.emulator, () =>
      certifyFaultProofFieldCarriageV1({
        lucid: harness.proverLucid,
        network,
        signer: harness.proverSigner,
        planned,
        certificatePolicyId:
          harness.contracts.fieldPreimageCertificate.policyId,
        certificateMintingScript:
          harness.contracts.fieldPreimageCertificate.mintingScript,
        certificateReferenceScriptUtxo: certificateReference,
        chunkUtxos: carriage.result,
        compactCbor: compact.toString("hex"),
        witnessSetCompactCbor: encodeMidgardNativeTxWitnessSetCompactV1(
          deriveMidgardNativeTxWitnessSetCompactV1(nativeTx.witnessSet),
        ).toString("hex"),
      }),
    );
    const artifact = buildProductionObserversForbiddenArtifactV1({
      headerHash: setup.headerHash,
      detectionId: `${transactionId}:accepted`,
      position: 0n,
      evidence,
      nativeTxCompactCbor: compact.toString("hex"),
      witnessSetCompactCbor: encodeMidgardNativeTxWitnessSetCompactV1(
        deriveMidgardNativeTxWitnessSetCompactV1(nativeTx.witnessSet),
      ).toString("hex"),
      l2TransactionSourceCbor: sourceCbor,
      transactionsPhasRoot: transactionsRoot,
      transactionMembershipCbor: membership.toCBOR().toString("hex"),
    });
    const proofActuator = createObserversForbiddenActuatorV1({
      binding: {
        definition: { headerHash: setup.headerHash },
        resolvedContracts: {
          category: { categoryId: category.categoryId },
          contracts: {
            fraudProof: {
              spendingScriptHash:
                harness.contracts.fraudProof.spendingScriptHash,
            },
          },
        },
        network,
        blueprint: harness.realBlueprint,
        deploymentInfo: {},
        releaseEconomics: {
          policy: { fraudProverRewardLovelace: "400000000" },
        },
      } as never,
      lucid: harness.proverLucid,
      signer: harness.proverSigner,
      contracts,
      references: {
        steps: references as unknown as readonly [UTxO, UTxO],
        witnesses: harness.witnessReferenceScripts as never,
        fieldPreimageCertificateMint: certificateReference,
      },
      stateQueueMutationLeaseCoordinator: {} as never,
    });

    const captures: Array<
      readonly [string, Awaited<ReturnType<typeof captureEmulatorSubmission>>]
    > = [];
    const initialize = async () => {
      console.info("[observers-forbidden-progress] init");
      const capture = await captureEmulatorSubmission(harness.emulator, () =>
        submitCommittedFieldShapeInit({
          lucid: harness.proverLucid,
          blueprint: harness.realBlueprint,
          network,
          contracts: contracts as never,
          category,
          catalogue: {
            policyId: harness.contracts.fraudProofCatalogue.policyId,
            spendingScriptAddress:
              harness.contracts.fraudProofCatalogue.spendingScriptAddress,
            root: catalogue.root,
          },
          signer: harness.proverSigner,
          fraudulentBlockOutRef: setup.fraudulentBlockOutRef,
          fraudulentHeaderHash: setup.headerHash,
          witnessReferenceScripts: harness.witnessReferenceScripts,
        }),
      );
      captures.push(["init", capture]);
      return {
        threadOutRef: `${capture.result.txHash}#${capture.result.firstStepOutputIndex.toString()}`,
        token: {
          unit: capture.result.computationThreadUnit,
          fraudulentHeaderHash: capture.result.fraudulentHeaderHash,
        },
      };
    };
    const bind = async (initial: Awaited<ReturnType<typeof initialize>>) => {
      console.info("[observers-forbidden-progress] step-01");
      const [threadUtxo] = await harness.proverLucid.utxosByOutRef([
        {
          txHash: initial.threadOutRef.slice(0, 64),
          outputIndex: Number(initial.threadOutRef.slice(65)),
        },
      ]);
      if (threadUtxo === undefined)
        throw new Error("accepted Init thread absent");
      return await captureEmulatorSubmission(harness.emulator, () =>
        submitObserversForbiddenStep01AcceptedV1({
          lucid: harness.proverLucid,
          blueprint: harness.realBlueprint,
          network,
          contracts,
          signer: harness.proverSigner,
          finding: evidence,
          threadUtxo,
          threadToken: initial.token,
          stateQueueBlockOutRef: setup.fraudulentBlockOutRef,
          txInclusion,
          referenceScriptUtxo: references[0]!,
          witnessReferenceScripts: harness.witnessReferenceScripts,
        }),
      );
    };

    const cancelledAtInit = await initialize();
    console.info("[observers-forbidden-progress] cancel-step-01");
    const cancelInit = await captureEmulatorSubmission(harness.emulator, () =>
      submitObserversForbiddenCancelV1({
        lucid: harness.proverLucid,
        contracts,
        categoryId: category.categoryId,
        signer: harness.proverSigner,
        threadOutRef: cancelledAtInit.threadOutRef,
        referenceScriptUtxo: references[0]!,
        witnessReferenceScripts: harness.witnessReferenceScripts,
      }),
    );
    captures.push(["cancel-step-01", cancelInit]);

    const cancelledAtStep2 = await bind(await initialize());
    captures.push(["step-01-before-cancel", cancelledAtStep2]);
    console.info("[observers-forbidden-progress] cancel-step-02");
    const cancelStep2 = await captureEmulatorSubmission(harness.emulator, () =>
      submitObserversForbiddenCancelV1({
        lucid: harness.proverLucid,
        contracts,
        categoryId: category.categoryId,
        signer: harness.proverSigner,
        threadOutRef: cancelledAtStep2.result.nextThreadOutRef,
        referenceScriptUtxo: references[1]!,
        witnessReferenceScripts: harness.witnessReferenceScripts,
      }),
    );
    captures.push(["cancel-step-02", cancelStep2]);

    const restarted = await initialize();
    const step01 = await captureEmulatorSubmission(
      harness.emulator,
      async () => {
        const captured = await proofActuator.capture({
          action: {
            stage: "step_01",
            threadOutRef: restarted.threadOutRef,
            stateQueueBlockOutRef: setup.fraudulentBlockOutRef,
          },
          artifact,
        });
        const txHash = await submitCapturedTransactionV1(captured.transaction);
        expect(txHash).toBe(captured.transaction.txHash);
        await harness.proverLucid.awaitTx(txHash);
        const next = (
          await harness.proverLucid.utxosAt(applied[1].spendingScriptAddress)
        ).find((utxo) => utxo.txHash === txHash);
        if (next === undefined)
          throw new Error("actuator step-01 output absent");
        return {
          txHash,
          nextThreadOutRef: `${next.txHash}#${next.outputIndex}`,
        };
      },
    );
    captures.push(["step-01-restarted", step01]);
    console.info("[observers-forbidden-progress] final-step-02");
    const final = await captureEmulatorSubmission(
      harness.emulator,
      async () => {
        const captured = await proofActuator.capture({
          action: {
            stage: "step_02",
            threadOutRef: step01.result.nextThreadOutRef,
          },
          artifact,
        });
        const txHash = await submitCapturedTransactionV1(captured.transaction);
        expect(txHash).toBe(captured.transaction.txHash);
        await harness.proverLucid.awaitTx(txHash);
        const proof = (
          await harness.proverLucid.utxosAt(
            harness.contracts.fraudProof.spendingScriptAddress,
          )
        ).find((utxo) => utxo.txHash === txHash);
        if (proof === undefined)
          throw new Error("actuator proof output absent");
        return {
          txHash,
          fraudProofOutRef: `${proof.txHash}#${proof.outputIndex}`,
          fraudProofUnit: Object.keys(proof.assets).find(
            (unit) => unit !== "lovelace" && proof.assets[unit] === 1n,
          ),
        };
      },
    );
    captures.push(["permanent-proof-mint", final]);
    expect(final.result.fraudProofUnit).toBeTruthy();
    const removalReferences = await publishRemovalReferenceScripts({
      lucid: harness.proverLucid,
      contracts: harness.contracts,
    });
    const baseDeployment = buildRemovalDeploymentInfo(
      harness.contracts,
      catalogue,
      { removalReferenceScripts: removalReferences.published },
    );
    const deploymentInfo = {
      ...baseDeployment,
      contracts: {
        ...baseDeployment.contracts,
        fraudProofObserversForbiddenOnUntaggedNetwork: {
          scriptHash: applied[0].spendingScriptHash,
          contract: {
            type: applied[0].spendingScript.type,
            cborHex: applied[0].spendingScript.script,
          },
        },
      },
    };
    const removalActuator = createObserversForbiddenActuatorV1({
      binding: {
        definition: { headerHash: setup.headerHash },
        resolvedContracts: {
          category: { categoryId: category.categoryId },
          contracts: {
            fraudProof: {
              spendingScriptHash:
                harness.contracts.fraudProof.spendingScriptHash,
            },
          },
        },
        network,
        blueprint: harness.realBlueprint,
        deploymentInfo,
        releaseEconomics: {
          policy: { fraudProverRewardLovelace: "400000000" },
        },
      } as never,
      lucid: harness.proverLucid,
      signer: harness.proverSigner,
      contracts,
      references: {
        steps: references as unknown as readonly [UTxO, UTxO],
        witnesses: harness.witnessReferenceScripts as never,
        fieldPreimageCertificateMint: certificateReference,
      },
      stateQueueMutationLeaseCoordinator: {
        acquire: async () => ({
          token: "observers-forbidden-emulator",
          source: "emulator",
          renew: async () => {},
          release: async () => {},
          fail: async () => {},
        }),
      },
    });
    vi.setSystemTime(harness.emulator.now());
    const removal = await captureEmulatorSubmission(
      harness.emulator,
      async () => {
        const captured = await removalActuator.capture({
          action: {
            stage: "remove",
            nextRemovalOutRef: setup.fraudulentBlockOutRef,
            fraudProofOutRef: final.result.fraudProofOutRef,
          },
          artifact,
        });
        const txHash = await submitCapturedTransactionV1(captured.transaction);
        expect(txHash).toBe(captured.transaction.txHash);
        await harness.proverLucid.awaitTx(txHash);
        return { txHash };
      },
    );
    captures.push(["fraudulent-block-removal", removal]);
    expect(carriage.measurements).toHaveLength(2);
    expect(certificate.measurement.l1ByteMargin).toBeGreaterThan(0);
    for (const [label, captured] of captures) {
      expect(captured.measurement.l1ByteMargin, label).toBeGreaterThan(0);
      expect(captured.measurement.executionMemory, label).toBeGreaterThan(0n);
      expect(captured.measurement.executionSteps, label).toBeGreaterThan(0n);
    }
    if (process.env.MIDGARD_PRINT_FIT === "1")
      console.info(
        `[observers-forbidden-ledger] ${JSON.stringify(
          captures.map(([label, captured]) => [label, captured.measurement]),
          (_key, value: unknown) =>
            typeof value === "bigint" ? value.toString() : value,
        )}`,
      );
    if (process.env.MIDGARD_PRINT_FIT === "1")
      console.info(
        `[observers-forbidden-carriage-ledger] ${JSON.stringify(
          [...carriage.measurements, certificate.measurement],
          (_key, value: unknown) =>
            typeof value === "bigint" ? value.toString() : value,
        )}`,
      );
  }, 600_000);

  it.each([
    { networkId: 255 as const, observerCount: 0 },
    { networkId: 1 as const, observerCount: 1 },
  ])(
    "runs forced contradiction for network $networkId with $observerCount observers",
    async (scenario) => await runForcedContradiction(scenario),
    300_000,
  );
});
