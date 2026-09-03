import { Store, Trie } from "@aiken-lang/merkle-patricia-forestry";
import {
  adjudicateMidgardNativeTxFullValidity,
  computeMidgardNativeTxId,
  deriveMidgardNativeTxProofSource,
  deriveMidgardNativeTxWitnessSetCompact,
  encodeMidgardFieldPreimage,
  encodeMidgardNativeTxCompact,
  encodeMidgardNativeTxWitnessSetCompact,
  materializeMidgardNativeTxFromCanonical,
  midgardFieldCommitment,
} from "@al-ft/midgard-core";
import {
  acceptedVerdictSubject,
  AddressData,
  addressDataFromBech32,
  ForcedInclusionTxSchema,
  forcedVerdictSubject,
  hashBlockHeader,
  OutputReference,
  Proof,
  ROOT_DOMAINS,
} from "@al-ft/midgard-sdk";
import { Data, getAddressDetails, type UTxO } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { describe, expect, it, vi } from "vitest";

import { submitCommittedFieldShapeInit } from "../src/committed-field-shape/submit-committed-field-shape-init.js";
import {
  certifyFaultProofFieldCarriage,
  planFaultProofFieldOpening,
  publishFaultProofFieldCarriage,
} from "../src/field-opening-v1.js";
import {
  applyObserverOrderInvalidScripts,
  type ObserverOrderInvalidContracts,
} from "../src/observer-order-invalid/contracts-v1.js";
import { prepareObserverOrderInvalidEvidence } from "../src/observer-order-invalid/family-v1.js";
import { createObserverOrderInvalidActuator } from "../src/observer-order-invalid/production-actuator-v1.js";
import { buildObserverOrderInvalidArtifact } from "../src/observer-order-invalid/production-artifact-v1.js";
import { planObserverOrderInvalidStagedWalk } from "../src/observer-order-invalid/staged-plan-v1.js";
import { submitObserverOrderInvalidCancel } from "../src/observer-order-invalid/submit-cancel-v1.js";
import {
  submitObserverOrderInvalidStep01Accepted,
  submitObserverOrderInvalidStep01Forced,
} from "../src/observer-order-invalid/submit-step-01-v1.js";
import { submitObserverOrderInvalidStep02 } from "../src/observer-order-invalid/submit-step-02-v1.js";
import { submitObserverOrderInvalidStep03 } from "../src/observer-order-invalid/submit-step-03-v1.js";
import { submitObserverOrderInvalidStep04 } from "../src/observer-order-invalid/submit-step-04-v1.js";
import { nativeTxFromCoreCompact } from "../src/submit-step-01.js";
import { buildCountedRoot } from "../src/transition-trace/phas.js";
import { submitCapturedTransaction } from "../src/workflow/transaction-boundary-v1.js";
import { buildCatalogueDeploymentInfo } from "./support/emulator/catalogue.js";
import { alignUnixTimeToEmulatorSlotBoundary } from "./support/emulator/emulator-context.js";
import { makeFaultProofEmulatorHarness } from "./support/emulator/harness.js";
import { captureEmulatorSubmission } from "./support/emulator/measurement.js";
import {
  l2TransactionSourceCbor as l2TransactionSourceCborV1,
  makeNativeTx,
} from "./support/emulator/native-tx.js";
import { publishPlainReferenceScriptUtxo } from "./support/emulator/reference-scripts.js";
import { buildRemovalDeploymentInfo } from "./support/emulator/removal-deployment.js";
import { submitSetupTx } from "./support/emulator/setup-tx.js";
import { setupFraudulentBlock } from "./support/submit-init-emulator-fixtures.js";
import { buildInvalidForcedTransitionTraceFixture } from "./support/submit-init-emulator-fixtures.js";
import { publishRemovalReferenceScripts } from "./support/submit-init-emulator-shared.js";

const network = "Custom" as const;
const firstStepDeploymentEntry = "fraudProofObserverOrderInvalid";

const observer = (ordinal: number): Buffer => {
  const value = Buffer.alloc(28);
  value.writeUInt32BE(ordinal, 24);
  return value;
};

describe("observerOrderInvalid local-catalogue maximum lifecycle", () => {
  it("runs Init, authentication and observer-scan resumes, permanent mint, and removal", async () => {
    const harness = await makeFaultProofEmulatorHarness({
      contractOptions: { alwaysFraudProofCatalogue: true },
    });
    const addressData = await Effect.runPromise(
      addressDataFromBech32(
        harness.contracts.fraudProof.spendingScriptAddress,
      ).pipe(Effect.map((address) => Data.from(Data.to(address, AddressData)))),
    );
    const applied = applyObserverOrderInvalidScripts({
      blueprint: harness.realBlueprint,
      network,
      computationThreadPolicyId: harness.contracts.computationThread.policyId,
      fraudProofPolicyId: harness.contracts.fraudProof.policyId,
      fraudProofTokenAddressData: addressData,
      fieldPreimageCertificatePolicyId:
        harness.contracts.fieldPreimageCertificate.policyId,
      hubOracleScriptHash: harness.contracts.hubOracle.spendingScriptHash,
    });
    const steps = applied.map((step) => ({
      spendingScript: step.spendingScript,
      spendingScriptHash: step.spendingScriptHash,
      spendingScriptAddress: step.spendingScriptAddress,
    })) as unknown as readonly [
      (typeof harness.contracts.fraudProofContracts.doubleSpend.steps)[number],
      (typeof harness.contracts.fraudProofContracts.doubleSpend.steps)[number],
      (typeof harness.contracts.fraudProofContracts.doubleSpend.steps)[number],
      (typeof harness.contracts.fraudProofContracts.doubleSpend.steps)[number],
    ];
    const contracts: ObserverOrderInvalidContracts = {
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
    const catalogue = await buildCatalogueDeploymentInfo({
      ...harness.contracts.fraudProofs,
      observerOrderInvalid: {
        ...harness.contracts.fraudProofs.observerOrderInvalid,
        spendingScriptHash: applied[0].spendingScriptHash,
      },
    });
    const category = catalogue.categories.observerOrderInvalid;

    const observers = Array.from({ length: 505 }, (_, index) =>
      observer(index),
    );
    observers[48] = observer(47);
    const observerField = encodeMidgardFieldPreimage(observers);
    expect(observerField).toHaveLength(15_153);
    const base = makeNativeTx({ spendInputCbors: [], fee: 7n });
    const nativeTx = materializeMidgardNativeTxFromCanonical({
      version: base.version,
      validity: base.validity,
      body: { ...base.body, requiredObserversPreimageCbor: observerField },
      witnessSet: base.witnessSet,
    });
    const nativeTxId = computeMidgardNativeTxId(nativeTx).toString("hex");
    const compactCbor = encodeMidgardNativeTxCompact(nativeTx.compact);
    const sourceCbor = l2TransactionSourceCborV1(nativeTx);
    const store = new Store(undefined);
    await store.ready();
    const trie = new Trie(store);
    await trie.insert(
      Buffer.from(nativeTxId, "hex"),
      Buffer.from(sourceCbor, "hex"),
    );
    const proof = await trie.prove(Buffer.from(nativeTxId, "hex"));
    const transactionsRoot = Buffer.from(trie.hash).toString("hex");
    const txInclusion = {
      nativeTxId,
      nativeTx: nativeTxFromCoreCompact(nativeTx.compact),
      nativeTxCompactCbor: compactCbor.toString("hex"),
      l2TransactionSourceCbor: sourceCbor,
      transactionsPhasRoot: transactionsRoot,
      txMembershipProof: Data.from(proof.toCBOR().toString("hex"), Proof),
      txMembershipProofCbor: proof.toCBOR().toString("hex"),
    };
    const setup = await setupFraudulentBlock({
      funderLucid: harness.funderLucid,
      emulator: harness.emulator,
      contracts: harness.contracts,
      catalogue,
      fixture: { transactionsRoot, l2TransactionCount: 1n },
    });
    const evidence = prepareObserverOrderInvalidEvidence({
      finding: {
        subject: acceptedVerdictSubject(nativeTxId),
        observerIndex: 48,
      },
      fieldPreimage: observerField,
      committedFieldHashHex:
        midgardFieldCommitment(observerField).toString("hex"),
    });
    expect(evidence.violation).toBe(true);
    expect(evidence.carriage).toBe("Certified");
    const staged = planObserverOrderInvalidStagedWalk({
      transactionId: nativeTxId,
      fieldPreimageCbor: observerField.toString("hex"),
      observerIndex: 48,
    });
    expect(staged.walk).toHaveLength(3);

    const references: UTxO[] = [];
    for (const [index, step] of applied.entries())
      references.push(
        (
          await publishPlainReferenceScriptUtxo({
            lucid: harness.funderLucid,
            script: step.spendingScript,
            label: `observer-order-lifecycle-${index.toString()}`,
          })
        ).utxo,
      );
    const certificateReference = (
      await publishPlainReferenceScriptUtxo({
        lucid: harness.funderLucid,
        script: harness.contracts.fieldPreimageCertificate.mintingScript,
        label: "observer-order-lifecycle-certificate",
      })
    ).utxo;
    const planned = planFaultProofFieldOpening({
      fieldIndex: 3,
      anchorTxId: nativeTxId,
      nativeTxCompactCbor: compactCbor.toString("hex"),
      itemCbors: staged.items,
      owner: harness.proverSigner.paymentKeyHash,
      publish: true,
      label: "observer order invalid lifecycle",
    });
    const carriageCapture = await captureEmulatorSubmission(
      harness.emulator,
      () =>
        publishFaultProofFieldCarriage({
          lucid: harness.proverLucid,
          signer: harness.proverSigner,
          planned,
          publisherAddress: harness.proverSigner.address,
          label: "observer order invalid lifecycle",
        }),
    );
    const carriageUtxos = carriageCapture.result;
    const certificateCapture = await captureEmulatorSubmission(
      harness.emulator,
      () =>
        certifyFaultProofFieldCarriage({
          lucid: harness.proverLucid,
          network,
          signer: harness.proverSigner,
          planned,
          certificatePolicyId:
            harness.contracts.fieldPreimageCertificate.policyId,
          certificateMintingScript:
            harness.contracts.fieldPreimageCertificate.mintingScript,
          certificateReferenceScriptUtxo: certificateReference,
          chunkUtxos: carriageUtxos,
          compactCbor: compactCbor.toString("hex"),
          witnessSetCompactCbor: encodeMidgardNativeTxWitnessSetCompact(
            deriveMidgardNativeTxWitnessSetCompact(nativeTx.witnessSet),
          ).toString("hex"),
        }),
    );
    expect(carriageCapture.measurements).toHaveLength(2);
    expect(certificateCapture.measurements).toHaveLength(1);

    const captures: Array<
      readonly [string, Awaited<ReturnType<typeof captureEmulatorSubmission>>]
    > = [];
    const init = await captureEmulatorSubmission(harness.emulator, () =>
      submitCommittedFieldShapeInit({
        lucid: harness.proverLucid,
        blueprint: harness.realBlueprint,
        network,
        contracts: { ...contracts, steps } as never,
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
    captures.push(["init", init]);
    const [threadUtxo] = await harness.proverLucid.utxosByOutRef([
      {
        txHash: init.result.txHash,
        outputIndex: init.result.firstStepOutputIndex,
      },
    ]);
    if (threadUtxo === undefined)
      throw new Error("observer order invalid init thread absent");
    const step01 = await captureEmulatorSubmission(harness.emulator, () =>
      submitObserverOrderInvalidStep01Accepted({
        lucid: harness.proverLucid,
        blueprint: harness.realBlueprint,
        network,
        contracts,
        signer: harness.proverSigner,
        finding: evidence,
        threadUtxo,
        threadToken: {
          unit: init.result.computationThreadUnit,
          fraudulentHeaderHash: init.result.fraudulentHeaderHash,
        },
        stateQueueBlockOutRef: setup.fraudulentBlockOutRef,
        txInclusion,
        referenceScriptUtxo: references[0]!,
        witnessReferenceScripts: harness.witnessReferenceScripts,
      }),
    );
    captures.push(["step01", step01]);
    let threadOutRef = step01.result.nextThreadOutRef;
    const artifact = buildObserverOrderInvalidArtifact({
      headerHash: setup.headerHash,
      detectionId: `${nativeTxId}:accepted:48`,
      position: 0n,
      evidence,
      nativeTxCompactCbor: compactCbor.toString("hex"),
      witnessSetCompactCbor: encodeMidgardNativeTxWitnessSetCompact(
        deriveMidgardNativeTxWitnessSetCompact(nativeTx.witnessSet),
      ).toString("hex"),
      l2TransactionSourceCbor: sourceCbor,
      transactionsPhasRoot: transactionsRoot,
      transactionMembershipCbor: proof.toCBOR().toString("hex"),
    });
    const actuator = createObserverOrderInvalidActuator({
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
      } as never,
      lucid: harness.proverLucid,
      signer: harness.proverSigner,
      contracts,
      references: {
        steps: references as unknown as readonly [UTxO, UTxO, UTxO, UTxO],
        witnesses: harness.witnessReferenceScripts as never,
        fieldPreimageCertificateMint: certificateReference,
      },
      stateQueueMutationLeaseCoordinator: {} as never,
    });
    const submitActuatorAction = async (
      action: Parameters<typeof actuator.capture>[0]["action"],
      nextAddress: string,
    ) => {
      const captured = await actuator.capture({ action, artifact });
      const txHash = await submitCapturedTransaction(captured.transaction);
      expect(txHash).toBe(captured.transaction.txHash);
      await harness.proverLucid.awaitTx(txHash);
      const next = (await harness.proverLucid.utxosAt(nextAddress)).find(
        (utxo) => utxo.txHash === txHash,
      );
      if (next === undefined)
        throw new Error(
          "observer order invalid actuator omitted its next thread output",
        );
      return {
        txHash,
        nextThreadOutRef: `${next.txHash}#${next.outputIndex.toString()}`,
      };
    };
    const authenticated = await captureEmulatorSubmission(
      harness.emulator,
      () =>
        submitActuatorAction(
          { stage: "step_02", threadOutRef, action: { kind: "authenticate" } },
          applied[2].spendingScriptAddress,
        ),
    );
    captures.push(["authenticate-field", authenticated]);
    threadOutRef = authenticated.result.nextThreadOutRef;
    for (let ordinal = 0; ordinal < staged.walk.length; ordinal += 1) {
      const result = await captureEmulatorSubmission(harness.emulator, () =>
        submitActuatorAction(
          { stage: "step_03", threadOutRef, walkOrdinal: ordinal },
          applied[ordinal === staged.walk.length - 1 ? 3 : 2]
            .spendingScriptAddress,
        ),
      );
      captures.push([`scan${ordinal.toString()}`, result]);
      threadOutRef = result.result.nextThreadOutRef;
    }
    const final = await captureEmulatorSubmission(
      harness.emulator,
      async () => {
        const captured = await actuator.capture({
          action: { stage: "step_04", threadOutRef },
          artifact,
        });
        const txHash = await submitCapturedTransaction(captured.transaction);
        expect(txHash).toBe(captured.transaction.txHash);
        await harness.proverLucid.awaitTx(txHash);
        const proof = (
          await harness.proverLucid.utxosAt(
            harness.contracts.fraudProof.spendingScriptAddress,
          )
        ).find((utxo) => utxo.txHash === txHash);
        if (proof === undefined)
          throw new Error(
            "observer order invalid actuator omitted permanent proof token",
          );
        return {
          txHash,
          fraudProofOutRef: `${proof.txHash}#${proof.outputIndex.toString()}`,
        };
      },
    );
    captures.push(["final", final]);
    expect(final.result.fraudProofOutRef).toBeTruthy();

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
        [firstStepDeploymentEntry]: {
          scriptHash: applied[0].spendingScriptHash,
          contract: {
            type: applied[0].spendingScript.type,
            cborHex: applied[0].spendingScript.script,
          },
        },
        fraudProofObserverOrderInvalidStep02: {
          scriptHash: applied[1].spendingScriptHash,
          contract: {
            type: applied[1].spendingScript.type,
            cborHex: applied[1].spendingScript.script,
          },
        },
        fraudProofObserverOrderInvalidStep03: {
          scriptHash: applied[2].spendingScriptHash,
          contract: {
            type: applied[2].spendingScript.type,
            cborHex: applied[2].spendingScript.script,
          },
        },
        fraudProofObserverOrderInvalidStep04: {
          scriptHash: applied[3].spendingScriptHash,
          contract: {
            type: applied[3].spendingScript.type,
            cborHex: applied[3].spendingScript.script,
          },
        },
      },
    };
    const removalActuator = createObserverOrderInvalidActuator({
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
        steps: references as unknown as readonly [UTxO, UTxO, UTxO, UTxO],
        witnesses: harness.witnessReferenceScripts as never,
        fieldPreimageCertificateMint: certificateReference,
      },
      stateQueueMutationLeaseCoordinator: {
        acquire: async () => ({
          token: "observer-order-emulator",
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
        const txHash = await submitCapturedTransaction(captured.transaction);
        expect(txHash).toBe(captured.transaction.txHash);
        await harness.proverLucid.awaitTx(txHash);
        return { txHash };
      },
    );
    captures.push(["removal", removal]);
    for (const [label, value] of captures) {
      expect(value.measurement.l1ByteMargin, label).toBeGreaterThan(0);
      expect(value.measurement.executionMemory, label).toBeGreaterThan(0n);
      expect(value.measurement.executionSteps, label).toBeGreaterThan(0n);
    }
    for (const [index, measurement] of carriageCapture.measurements.entries())
      expect(
        measurement.l1ByteMargin,
        `certified-chunk-${index.toString()}`,
      ).toBeGreaterThan(0);
    expect(certificateCapture.measurement.l1ByteMargin).toBeGreaterThan(0);
    if (process.env.MIDGARD_PRINT_FIT === "1") {
      console.info(
        `[observer-order-invalid-ledger] ${JSON.stringify(
          captures.map(([label, value]) => [label, value.measurement]),
          (_key, value: unknown) =>
            typeof value === "bigint" ? value.toString() : value,
        )}`,
      );
      console.info(
        `[observer-order-invalid-carriage-ledger] ${JSON.stringify(
          [...carriageCapture.measurements, certificateCapture.measurement],
          (_key, value: unknown) =>
            typeof value === "bigint" ? value.toString() : value,
        )}`,
      );
    }
  }, 600_000);

  it("runs the exact forced wrongful-rejection arm through permanent mint", async () => {
    const harness = await makeFaultProofEmulatorHarness({
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
    const applied = applyObserverOrderInvalidScripts({
      blueprint: harness.realBlueprint,
      network,
      computationThreadPolicyId: harness.contracts.computationThread.policyId,
      fraudProofPolicyId: harness.contracts.fraudProof.policyId,
      fraudProofTokenAddressData: addressData,
      fieldPreimageCertificatePolicyId:
        harness.contracts.fieldPreimageCertificate.policyId,
      hubOracleScriptHash: harness.contracts.hubOracle.spendingScriptHash,
    });
    const contracts: ObserverOrderInvalidContracts = {
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
    const catalogue = await buildCatalogueDeploymentInfo({
      ...harness.contracts.fraudProofs,
      observerOrderInvalid: {
        ...harness.contracts.fraudProofs.observerOrderInvalid,
        spendingScriptHash: applied[0].spendingScriptHash,
      },
    });
    const category = catalogue.categories.observerOrderInvalid;
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
    const valid = makeNativeTx({ spendInputCbors: [], fee: 0n });
    const observerField = encodeMidgardFieldPreimage([
      observer(1),
      observer(2),
    ]);
    const forcedNativeTx = materializeMidgardNativeTxFromCanonical({
      version: valid.version,
      validity: valid.validity,
      body: { ...valid.body, requiredObserversPreimageCbor: observerField },
      witnessSet: valid.witnessSet,
    });
    const forcedId = computeMidgardNativeTxId(forcedNativeTx).toString("hex");
    const forcedSource = deriveMidgardNativeTxProofSource(
      adjudicateMidgardNativeTxFullValidity(forcedNativeTx, "TxIsInvalid"),
    );
    const forcedSourceId = computeMidgardNativeTxId(
      adjudicateMidgardNativeTxFullValidity(forcedNativeTx, "TxIsInvalid"),
    ).toString("hex");
    expect(forcedSourceId).toBe(forcedId);
    const reason = {
      ObserverOrderInvalid: { observer_index: 1n },
    } as const;
    const forcedTransaction = {
      tx_id: forcedId,
      source: {
        compact_cbor: forcedSource.compactCbor.toString("hex"),
        witness_set_compact_cbor:
          forcedSource.witnessSetCompactCbor.toString("hex"),
        field_preimage_lengths_cbor:
          forcedSource.fieldPreimageLengthsCbor.toString("hex"),
      },
      verdict: { ForcedTxInvalid: { reason } },
    } as const;
    const sourceKey =
      baseFixture.eventKey.ForcedTransactionEventKey.tx_order_id;
    const keyBytes = Buffer.from(Data.to(sourceKey, OutputReference), "hex");
    const valueBytes = Buffer.from(
      Data.to(forcedTransaction as never, ForcedInclusionTxSchema as never),
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
    const header = {
      ...baseFixture.header,
      forcedTransactionsRoot: root.root,
    };
    const setup = await submitSetupTx({
      lucid: harness.funderLucid,
      contracts: harness.contracts,
      nonceUtxo: harness.nonceUtxo,
      catalogue,
      header,
    });
    expect(setup.headerHash).toBe(
      await Effect.runPromise(hashBlockHeader(header)),
    );
    const evidence = prepareObserverOrderInvalidEvidence({
      finding: {
        subject: forcedVerdictSubject({
          transactionId: forcedId,
          sourceKey,
          rejectionReason: reason,
        }),
        observerIndex: 1,
      },
      fieldPreimage: observerField,
      committedFieldHashHex:
        midgardFieldCommitment(observerField).toString("hex"),
    });
    expect(evidence.violation).toBe(false);
    const staged = planObserverOrderInvalidStagedWalk({
      transactionId: forcedId,
      fieldPreimageCbor: observerField.toString("hex"),
      observerIndex: 1,
    });
    const references: UTxO[] = [];
    for (const [index, step] of applied.entries())
      references.push(
        (
          await publishPlainReferenceScriptUtxo({
            lucid: harness.funderLucid,
            script: step.spendingScript,
            label: `observer-order-forced-${index.toString()}`,
          })
        ).utxo,
      );
    const captures: Array<
      readonly [string, Awaited<ReturnType<typeof captureEmulatorSubmission>>]
    > = [];
    const measured = async <Result>(
      label: string,
      action: () => Promise<Result>,
    ): Promise<Result> => {
      const captured = await captureEmulatorSubmission(
        harness.emulator,
        action,
      );
      captures.push([label, captured]);
      return captured.result;
    };
    const planned = planFaultProofFieldOpening({
      fieldIndex: 3,
      anchorTxId: forcedId,
      nativeTxCompactCbor: forcedSource.compactCbor.toString("hex"),
      itemCbors: staged.items,
      owner: harness.proverSigner.paymentKeyHash,
      publish: true,
      label: "observer order invalid forced lifecycle",
    });
    await measured("raw-carriage-publication", () =>
      publishFaultProofFieldCarriage({
        lucid: harness.proverLucid,
        signer: harness.proverSigner,
        planned,
        publisherAddress: harness.proverSigner.address,
        label: "observer order invalid forced lifecycle",
      }),
    );
    const initialize = async () => {
      const initialized = await submitCommittedFieldShapeInit({
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
      });
      return `${initialized.txHash}#${initialized.firstStepOutputIndex.toString()}`;
    };
    const bind = async (threadOutRef: string) =>
      await submitObserverOrderInvalidStep01Forced({
        lucid: harness.proverLucid,
        contracts,
        categoryId: category.categoryId,
        signer: harness.proverSigner,
        threadOutRef,
        finding: evidence,
        forcedSource: { header, membership, direction: 1n },
        referenceScriptUtxo: references[0]!,
      });
    const decode = async (threadOutRef: string) =>
      await submitObserverOrderInvalidStep02({
        lucid: harness.proverLucid,
        contracts,
        categoryId: category.categoryId,
        signer: harness.proverSigner,
        threadOutRef,
        evidence,
        nativeTxCompactCbor: forcedSource.compactCbor.toString("hex"),
        staged,
        action: { kind: "authenticate" },
        referenceScriptUtxo: references[1]!,
      });
    const scan = async (threadOutRef: string) =>
      await submitObserverOrderInvalidStep03({
        lucid: harness.proverLucid,
        contracts,
        categoryId: category.categoryId,
        signer: harness.proverSigner,
        threadOutRef,
        evidence,
        nativeTxCompactCbor: forcedSource.compactCbor.toString("hex"),
        staged,
        walkOrdinal: 0,
        referenceScriptUtxo: references[2]!,
      });
    const cancel = async (threadOutRef: string, referenceScriptUtxo: UTxO) =>
      await submitObserverOrderInvalidCancel({
        lucid: harness.proverLucid,
        contracts,
        categoryId: category.categoryId,
        signer: harness.proverSigner,
        threadOutRef,
        referenceScriptUtxo,
        witnessReferenceScripts: harness.witnessReferenceScripts,
      });
    await measured("cancel-step-01", async () =>
      cancel(await initialize(), references[0]!),
    );
    const cancel02 = await bind(await initialize());
    await measured("cancel-step-02", () =>
      cancel(cancel02.nextThreadOutRef, references[1]!),
    );
    const cancel03 = await decode(
      (await bind(await initialize())).nextThreadOutRef,
    );
    await measured("cancel-step-03", () =>
      cancel(cancel03.nextThreadOutRef, references[2]!),
    );
    const cancel04 = await scan(
      (await decode((await bind(await initialize())).nextThreadOutRef))
        .nextThreadOutRef,
    );
    await measured("cancel-step-04", () =>
      cancel(cancel04.nextThreadOutRef, references[3]!),
    );

    const step01 = await measured(
      "step-01-forced",
      async () => await bind(await initialize()),
    );
    const step02 = await measured("forced-direct-field", () =>
      decode(step01.nextThreadOutRef),
    );
    const step03 = await measured("forced-complete-scan", () =>
      scan(step02.nextThreadOutRef),
    );
    const final = await measured("forced-permanent-proof-mint", () =>
      submitObserverOrderInvalidStep04({
        lucid: harness.proverLucid,
        contracts,
        categoryId: category.categoryId,
        signer: harness.proverSigner,
        threadOutRef: step03.nextThreadOutRef,
        evidence,
        referenceScriptUtxo: references[3]!,
        witnessReferenceScripts: harness.witnessReferenceScripts,
      }),
    );
    expect(final.fraudProofUnit).toBeTruthy();
    for (const [label, captured] of captures) {
      expect(captured.measurement.l1ByteMargin, label).toBeGreaterThan(0);
      if (label !== "raw-carriage-publication") {
        expect(captured.measurement.executionMemory, label).toBeGreaterThan(0n);
        expect(captured.measurement.executionSteps, label).toBeGreaterThan(0n);
      }
    }
    if (process.env.MIDGARD_PRINT_FIT === "1")
      console.info(
        `[observer-order-invalid-forced-ledger] ${JSON.stringify(
          captures.map(([label, captured]) => [label, captured.measurement]),
          (_key, value: unknown) =>
            typeof value === "bigint" ? value.toString() : value,
        )}`,
      );
  }, 300_000);
});
