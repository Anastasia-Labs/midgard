import {
  deriveMidgardNativeTxWitnessSetCompact,
  EMPTY_CBOR_LIST,
  EMPTY_NULL_ROOT,
  encodeMidgardNativeTxCanonical,
  encodeMidgardNativeTxCompact,
  encodeMidgardNativeTxWitnessSetCompact,
  materializeMidgardNativeTxFromCanonical,
  MIDGARD_NATIVE_NETWORK_ID_NONE,
  MIDGARD_NATIVE_TX_VERSION,
  MIDGARD_POSIX_TIME_NONE,
} from "@al-ft/midgard-core/codec";
import { encodeCbor } from "@al-ft/midgard-core/codec/cbor";
import * as SDK from "@al-ft/midgard-sdk";
import { toUnit, type UTxO } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import { submitRemoveFraudulentBlock } from "../src/index.js";
import { buildForcedTransactionLeafMembershipProof } from "../src/transition-trace/witnesses.js";
import {
  deriveWitnessScriptDecodingEvidenceFromCanonicalBlock,
  detectWitnessScriptDecodingCompleteReplay,
  prepareWitnessScriptDecodingEvidence,
  submitWitnessScriptDecodingCancel,
  submitWitnessScriptDecodingInit,
  submitWitnessScriptDecodingStep01Accepted,
  submitWitnessScriptDecodingStep01Forced,
  submitWitnessScriptDecodingStep02,
  submitWitnessScriptDecodingStep03,
  submitWitnessScriptDecodingStep04,
} from "../src/witness-script-decoding/index.js";
import { captureEmulatorSubmission } from "./support/emulator/measurement.js";
import {
  buildDecodingBlockFixture,
  decodingCanonicalItem,
  decodingMalformedMaximumItem,
} from "./support/native-script-decoding-emulator-v1.js";
import {
  alignUnixTimeToEmulatorSlotBoundary,
  buildRemovalDeploymentInfo,
  funderPaymentKeyHash,
  makeFaultProofEmulatorHarness,
  network,
  publishPlainReferenceScriptUtxo,
  publishRemovalReferenceScripts,
  submitSetupTx,
} from "./support/submit-init-emulator-shared.js";

const stage = async <T>(
  label: string,
  operation: () => Promise<T>,
): Promise<T> => {
  try {
    return await operation();
  } catch (cause) {
    throw new Error(`witness lifecycle failed at ${label}: ${String(cause)}`);
  }
};

describe("witness-script-decoding real lifecycle", () => {
  it("binds field 6, resumes the native scan, and mints the proof token", async () => {
    const harness = await makeFaultProofEmulatorHarness({
      contractOptions: {
        realWitnessScriptDecoding: true,
        alwaysFraudProofCatalogue: true,
      },
    });
    const chain = harness.contracts.fraudProofContracts.witnessScriptDecoding;
    const family = {
      steps: chain.steps,
      computationThread: harness.contracts.computationThread,
      fraudProof: harness.contracts.fraudProof,
      hubOraclePolicyId: harness.contracts.hubOracle.policyId,
      stateQueuePolicyId: harness.contracts.stateQueue.policyId,
      fieldPreimageCertificatePolicyId:
        harness.contracts.fieldPreimageCertificate.policyId,
      fieldPreimageCertificateMintingScript:
        harness.contracts.fieldPreimageCertificate.mintingScript,
    };
    const category = harness.catalogue.categories.witnessScriptDecoding;
    if (category === undefined) {
      throw new Error("harness omitted witness-script-decoding");
    }
    const item = decodingMalformedMaximumItem();
    const fieldPreimage = encodeCbor([item]);
    expect(fieldPreimage).toHaveLength(32_768);
    const nativeTx = materializeMidgardNativeTxFromCanonical({
      version: MIDGARD_NATIVE_TX_VERSION,
      validity: "TxIsValid",
      body: {
        spendInputsPreimageCbor: EMPTY_CBOR_LIST,
        referenceInputsPreimageCbor: EMPTY_CBOR_LIST,
        outputsPreimageCbor: EMPTY_CBOR_LIST,
        requiredObserversPreimageCbor: EMPTY_CBOR_LIST,
        requiredSignersPreimageCbor: EMPTY_CBOR_LIST,
        mintPreimageCbor: EMPTY_CBOR_LIST,
        scriptIntegrityHash: EMPTY_NULL_ROOT,
        auxiliaryDataHash: EMPTY_NULL_ROOT,
        fee: 1_000n,
        validityIntervalStart: MIDGARD_POSIX_TIME_NONE,
        validityIntervalEnd: MIDGARD_POSIX_TIME_NONE,
        networkId: MIDGARD_NATIVE_NETWORK_ID_NONE,
      },
      witnessSet: {
        addrTxWitsPreimageCbor: EMPTY_CBOR_LIST,
        scriptTxWitsPreimageCbor: fieldPreimage,
        redeemerTxWitsPreimageCbor: EMPTY_CBOR_LIST,
      },
    });
    const startTime = BigInt(
      alignUnixTimeToEmulatorSlotBoundary(
        harness.funderLucid,
        harness.emulator.now() + 120_000,
      ) - 1,
    );
    const block = await buildDecodingBlockFixture({
      operatorVkey: await funderPaymentKeyHash(harness.funderLucid),
      startTime,
      priorLedgerRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
      subject: { kind: "normal", nativeTx },
    });
    const setup = await stage("setup", () =>
      submitSetupTx({
        lucid: harness.funderLucid,
        contracts: harness.contracts,
        nonceUtxo: harness.nonceUtxo,
        catalogue: harness.catalogue,
        header: block.header,
      }),
    );
    const compact = deriveMidgardNativeTxWitnessSetCompact(nativeTx.witnessSet);
    const witnessSet: SDK.NativeTxWitnessSetCompact = {
      addr_tx_wits_hash: Buffer.from(compact.addrTxWitsHash).toString("hex"),
      script_tx_wits_hash: Buffer.from(compact.scriptTxWitsHash).toString(
        "hex",
      ),
      redeemer_tx_wits_hash: Buffer.from(compact.redeemerTxWitsHash).toString(
        "hex",
      ),
    };
    const evidence = prepareWitnessScriptDecodingEvidence({
      finding: {
        subject: SDK.acceptedVerdictSubject(block.nativeTxId),
        witnessSetHash:
          nativeTx.compact.transactionWitnessSetHash.toString("hex"),
        scriptIndex: 0,
      },
      fieldPreimage,
      committedFieldHashHex: witnessSet.script_tx_wits_hash,
    });
    expect(
      detectWitnessScriptDecodingCompleteReplay({
        headerHash: block.headerHash,
        transactions: [
          {
            nodeTxId: block.nativeTxId,
            txCbor: encodeMidgardNativeTxCanonical(nativeTx).toString("hex"),
          },
        ],
        reconstruction: block.reconstruction,
      } as never),
    ).toEqual([
      expect.objectContaining({
        violationId: "witness-native-script-malformed",
      }),
    ]);
    expect(
      deriveWitnessScriptDecodingEvidenceFromCanonicalBlock({
        headerHash: block.headerHash,
        transactions: [
          {
            nodeTxId: block.nativeTxId,
            txCbor: encodeMidgardNativeTxCanonical(nativeTx).toString("hex"),
          },
        ],
        reconstruction: block.reconstruction,
      } as never).itemCommitmentHex,
    ).toBe(evidence.itemCommitmentHex);
    expect(
      detectWitnessScriptDecodingCompleteReplay({
        headerHash: block.headerHash,
        transactions: [],
        reconstruction: {
          ...block.reconstruction,
          forcedTransactions: block.reconstruction.forcedTransactions.map(
            (transaction) => ({
              ...transaction,
              value: { ...transaction.value, verdict: "ForcedTxValid" },
            }),
          ),
        },
      } as never),
    ).toEqual([]);
    const stepRefs: UTxO[] = [];
    const publicationSizes: number[] = [];
    const ledger: {
      label: string;
      bytes: number;
      memory: string;
      cpu: string;
      margin: number;
    }[] = [];
    const measured = async <T>(label: string, operation: () => Promise<T>) => {
      const captured = await stage(label, () =>
        captureEmulatorSubmission(harness.emulator, operation),
      );
      for (const [index, measurement] of captured.measurements.entries()) {
        ledger.push({
          label:
            captured.measurements.length === 1
              ? label
              : `${label}-transaction-${index.toString().padStart(2, "0")}`,
          bytes: measurement.completeSignedBytes,
          memory: measurement.executionMemory.toString(),
          cpu: measurement.executionSteps.toString(),
          margin: measurement.l1ByteMargin,
        });
      }
      return captured.result;
    };
    for (const [index, step] of family.steps.entries()) {
      const captured = await stage(`publish-${index}`, () =>
        captureEmulatorSubmission(harness.emulator, () =>
          publishPlainReferenceScriptUtxo({
            lucid: harness.funderLucid,
            script: step.spendingScript,
            label: `witness-script-decoding step ${index + 1}`,
          }),
        ),
      );
      stepRefs.push(captured.result.utxo);
      publicationSizes.push(captured.measurement.completeSignedBytes);
    }
    expect(publicationSizes).toEqual([14_922, 10_542, 11_693, 2_932]);
    const certificateReference = (
      await publishPlainReferenceScriptUtxo({
        lucid: harness.funderLucid,
        script: family.fieldPreimageCertificateMintingScript,
        label: "witness-script-decoding field certificate",
      })
    ).utxo;
    if (block.txInclusion === null) throw new Error("missing normal inclusion");
    const txInclusion = block.txInclusion;
    const initialize = () =>
      submitWitnessScriptDecodingInit({
        lucid: harness.proverLucid,
        blueprint: harness.realBlueprint,
        network,
        contracts: family,
        category,
        catalogue: {
          policyId: harness.contracts.fraudProofCatalogue.policyId,
          spendingScriptAddress:
            harness.contracts.fraudProofCatalogue.spendingScriptAddress,
          root: harness.catalogue.root,
        },
        signer: harness.proverSigner,
        fraudulentBlockOutRef: setup.fraudulentBlockOutRef,
        witnessReferenceScripts: harness.witnessReferenceScripts,
      });
    const cancel = (threadOutRef: string, referenceScriptUtxo: UTxO) =>
      submitWitnessScriptDecodingCancel({
        lucid: harness.proverLucid,
        contracts: family,
        categoryId: category.categoryId,
        signer: harness.proverSigner,
        threadOutRef,
        referenceScriptUtxo,
        witnessReferenceScripts: harness.witnessReferenceScripts,
      });
    const cancelledAtStep01 = await initialize();
    await measured("cancel-step01", () =>
      cancel(cancelledAtStep01.nextThreadOutRef, stepRefs[0]!),
    );
    const cancelledAtStep02Init = await initialize();
    const cancelledAtStep02 = await submitWitnessScriptDecodingStep01Accepted({
      lucid: harness.proverLucid,
      blueprint: harness.realBlueprint,
      network,
      contracts: family,
      categoryId: category.categoryId,
      signer: harness.proverSigner,
      threadOutRef: cancelledAtStep02Init.nextThreadOutRef,
      stateQueueBlockOutRef: setup.fraudulentBlockOutRef,
      txInclusion,
      scriptIndex: 0n,
      referenceScriptUtxo: stepRefs[0]!,
      witnessReferenceScripts: harness.witnessReferenceScripts,
    });
    await measured("cancel-step02", () =>
      cancel(cancelledAtStep02.nextThreadOutRef, stepRefs[1]!),
    );
    const cancelledAtScanInit = await initialize();
    const cancelledAtScanBound =
      await submitWitnessScriptDecodingStep01Accepted({
        lucid: harness.proverLucid,
        blueprint: harness.realBlueprint,
        network,
        contracts: family,
        categoryId: category.categoryId,
        signer: harness.proverSigner,
        threadOutRef: cancelledAtScanInit.nextThreadOutRef,
        stateQueueBlockOutRef: setup.fraudulentBlockOutRef,
        txInclusion,
        scriptIndex: 0n,
        referenceScriptUtxo: stepRefs[0]!,
        witnessReferenceScripts: harness.witnessReferenceScripts,
      });
    const cancelledAtScan = await submitWitnessScriptDecodingStep02({
      lucid: harness.proverLucid,
      network,
      contracts: family,
      categoryId: category.categoryId,
      signer: harness.proverSigner,
      threadOutRef: cancelledAtScanBound.nextThreadOutRef,
      evidence,
      nativeTxCompactCbor: encodeMidgardNativeTxCompact(
        nativeTx.compact,
      ).toString("hex"),
      witnessSet,
      witnessSetCompactCbor:
        encodeMidgardNativeTxWitnessSetCompact(compact).toString("hex"),
      publishCarriage: true,
      certificateReferenceScriptUtxo: certificateReference,
      scriptWitnessItems: [item],
      referenceScriptUtxo: stepRefs[1]!,
    });
    await measured("cancel-step03", () =>
      cancel(cancelledAtScan.nextThreadOutRef, stepRefs[2]!),
    );
    const cancelledAtStep04Init = await initialize();
    const cancelledAtStep04Bound =
      await submitWitnessScriptDecodingStep01Accepted({
        lucid: harness.proverLucid,
        blueprint: harness.realBlueprint,
        network,
        contracts: family,
        categoryId: category.categoryId,
        signer: harness.proverSigner,
        threadOutRef: cancelledAtStep04Init.nextThreadOutRef,
        stateQueueBlockOutRef: setup.fraudulentBlockOutRef,
        txInclusion,
        scriptIndex: 0n,
        referenceScriptUtxo: stepRefs[0]!,
        witnessReferenceScripts: harness.witnessReferenceScripts,
      });
    const cancelledAtStep04Scan = await submitWitnessScriptDecodingStep02({
      lucid: harness.proverLucid,
      network,
      contracts: family,
      categoryId: category.categoryId,
      signer: harness.proverSigner,
      threadOutRef: cancelledAtStep04Bound.nextThreadOutRef,
      evidence,
      nativeTxCompactCbor: encodeMidgardNativeTxCompact(
        nativeTx.compact,
      ).toString("hex"),
      witnessSet,
      witnessSetCompactCbor:
        encodeMidgardNativeTxWitnessSetCompact(compact).toString("hex"),
      publishCarriage: true,
      certificateReferenceScriptUtxo: certificateReference,
      scriptWitnessItems: [item],
      referenceScriptUtxo: stepRefs[1]!,
    });
    let cancelledAtStep04OutRef = cancelledAtStep04Scan.nextThreadOutRef;
    for (;;) {
      const scan = await submitWitnessScriptDecodingStep03({
        lucid: harness.proverLucid,
        contracts: family,
        categoryId: category.categoryId,
        signer: harness.proverSigner,
        threadOutRef: cancelledAtStep04OutRef,
        evidence,
        referenceScriptUtxo: stepRefs[2]!,
      });
      cancelledAtStep04OutRef = scan.nextThreadOutRef;
      if (scan.closed) break;
    }
    await measured("cancel-step04", () =>
      cancel(cancelledAtStep04OutRef, stepRefs[3]!),
    );
    const init = await measured("init", () => initialize());
    const step01 = await measured("step01", () =>
      submitWitnessScriptDecodingStep01Accepted({
        lucid: harness.proverLucid,
        blueprint: harness.realBlueprint,
        network,
        contracts: family,
        categoryId: category.categoryId,
        signer: harness.proverSigner,
        threadOutRef: init.nextThreadOutRef,
        stateQueueBlockOutRef: setup.fraudulentBlockOutRef,
        txInclusion,
        scriptIndex: 0n,
        referenceScriptUtxo: stepRefs[0]!,
        witnessReferenceScripts: harness.witnessReferenceScripts,
      }),
    );
    const step02 = await measured("step02", () =>
      submitWitnessScriptDecodingStep02({
        lucid: harness.proverLucid,
        network,
        contracts: family,
        categoryId: category.categoryId,
        signer: harness.proverSigner,
        threadOutRef: step01.nextThreadOutRef,
        evidence,
        nativeTxCompactCbor: encodeMidgardNativeTxCompact(
          nativeTx.compact,
        ).toString("hex"),
        witnessSet,
        witnessSetCompactCbor:
          encodeMidgardNativeTxWitnessSetCompact(compact).toString("hex"),
        publishCarriage: true,
        certificateReferenceScriptUtxo: certificateReference,
        scriptWitnessItems: [item],
        referenceScriptUtxo: stepRefs[1]!,
      }),
    );
    let threadOutRef = step02.nextThreadOutRef;
    let scanTransactions = 0;
    for (;;) {
      const step03 = await measured(`step03-${scanTransactions}`, () =>
        submitWitnessScriptDecodingStep03({
          lucid: harness.proverLucid,
          contracts: family,
          categoryId: category.categoryId,
          signer: harness.proverSigner,
          threadOutRef,
          evidence,
          referenceScriptUtxo: stepRefs[2]!,
        }),
      );
      threadOutRef = step03.nextThreadOutRef;
      scanTransactions += 1;
      if (step03.closed) break;
    }
    expect(scanTransactions).toBeGreaterThan(1);
    const finalized = await measured("step04", () =>
      submitWitnessScriptDecodingStep04({
        lucid: harness.proverLucid,
        contracts: family,
        categoryId: category.categoryId,
        signer: harness.proverSigner,
        threadOutRef,
        evidence,
        referenceScriptUtxo: stepRefs[3]!,
        witnessReferenceScripts: harness.witnessReferenceScripts,
      }),
    );
    await expect(
      harness.proverLucid.utxosAtWithUnit(
        family.fraudProof.spendingScriptAddress,
        finalized.fraudProofUnit,
      ),
    ).resolves.toHaveLength(1);
    const threadUnit = toUnit(
      family.computationThread.policyId,
      `${category.categoryId}${setup.headerHash}`,
    );
    for (const step of family.steps) {
      await expect(
        harness.proverLucid.utxosAtWithUnit(
          step.spendingScriptAddress,
          threadUnit,
        ),
      ).resolves.toHaveLength(0);
    }
    const removalReferenceScripts = await publishRemovalReferenceScripts({
      lucid: harness.proverLucid,
      contracts: harness.contracts,
    });
    const removeNow = BigInt(harness.emulator.now());
    const removal = await measured("removal", () =>
      submitRemoveFraudulentBlock({
        lucid: harness.proverLucid,
        blueprint: harness.realBlueprint,
        deploymentInfo: buildRemovalDeploymentInfo(
          harness.contracts,
          harness.catalogue,
          { removalReferenceScripts: removalReferenceScripts.published },
        ),
        network,
        signer: harness.proverSigner,
        fraudCategory: "witnessScriptDecoding",
        fraudulentHeaderHash: setup.headerHash,
        awaitConfirmation: true,
        requireReferenceScripts: true,
        validFrom: removeNow > 120_000n ? removeNow - 120_000n : 0n,
        validTo: removeNow + 300_000n,
      }),
    );
    expect(removal.transactions[0]?.kind).toBe("remove-target");
    expect(ledger.map(({ label }) => label)).toContain("cancel-step04");
    expect(ledger.map(({ label }) => label)).toContain("step04");
    expect(ledger.map(({ label }) => label)).toContain("removal");
    for (const row of ledger) {
      expect(row.margin, row.label).toBeGreaterThan(0);
      expect(BigInt(row.memory), row.label).toBeLessThanOrEqual(16_500_000n);
      expect(BigInt(row.cpu), row.label).toBeLessThanOrEqual(10_000_000_000n);
    }
    console.info(
      `[witness-script-decoding-fit-ledger] ${JSON.stringify(ledger)}`,
    );
  }, 600_000);

  it("proves a forced wrongful rejection through the exact-reason contradiction", async () => {
    const harness = await makeFaultProofEmulatorHarness({
      contractOptions: {
        realWitnessScriptDecoding: true,
        alwaysFraudProofCatalogue: true,
      },
    });
    const chain = harness.contracts.fraudProofContracts.witnessScriptDecoding;
    const family = {
      steps: chain.steps,
      computationThread: harness.contracts.computationThread,
      fraudProof: harness.contracts.fraudProof,
      hubOraclePolicyId: harness.contracts.hubOracle.policyId,
      stateQueuePolicyId: harness.contracts.stateQueue.policyId,
      fieldPreimageCertificatePolicyId:
        harness.contracts.fieldPreimageCertificate.policyId,
      fieldPreimageCertificateMintingScript:
        harness.contracts.fieldPreimageCertificate.mintingScript,
    };
    const category = harness.catalogue.categories.witnessScriptDecoding!;
    const measurements: {
      label: string;
      bytes: number;
      memory: string;
      cpu: string;
    }[] = [];
    const measured = async <T>(label: string, operation: () => Promise<T>) => {
      const captured = await stage(label, () =>
        captureEmulatorSubmission(harness.emulator, operation),
      );
      measurements.push({
        label,
        bytes: captured.measurement.completeSignedBytes,
        memory: captured.measurement.executionMemory.toString(),
        cpu: captured.measurement.executionSteps.toString(),
      });
      return captured.result;
    };
    const item = decodingCanonicalItem();
    const fieldPreimage = encodeCbor([item]);
    const nativeTx = materializeMidgardNativeTxFromCanonical({
      version: MIDGARD_NATIVE_TX_VERSION,
      validity: "TxIsValid",
      body: {
        spendInputsPreimageCbor: EMPTY_CBOR_LIST,
        referenceInputsPreimageCbor: EMPTY_CBOR_LIST,
        outputsPreimageCbor: EMPTY_CBOR_LIST,
        requiredObserversPreimageCbor: EMPTY_CBOR_LIST,
        requiredSignersPreimageCbor: EMPTY_CBOR_LIST,
        mintPreimageCbor: EMPTY_CBOR_LIST,
        scriptIntegrityHash: EMPTY_NULL_ROOT,
        auxiliaryDataHash: EMPTY_NULL_ROOT,
        fee: 2_000n,
        validityIntervalStart: MIDGARD_POSIX_TIME_NONE,
        validityIntervalEnd: MIDGARD_POSIX_TIME_NONE,
        networkId: MIDGARD_NATIVE_NETWORK_ID_NONE,
      },
      witnessSet: {
        addrTxWitsPreimageCbor: EMPTY_CBOR_LIST,
        scriptTxWitsPreimageCbor: fieldPreimage,
        redeemerTxWitsPreimageCbor: EMPTY_CBOR_LIST,
      },
    });
    const orderKey = { transactionId: "cd".repeat(32), outputIndex: 0n };
    const reason: SDK.RejectionReason = {
      WitnessScriptHeaderMalformed: { script_index: 0n },
    };
    const block = await buildDecodingBlockFixture({
      operatorVkey: await funderPaymentKeyHash(harness.funderLucid),
      startTime: BigInt(
        alignUnixTimeToEmulatorSlotBoundary(
          harness.funderLucid,
          harness.emulator.now() + 120_000,
        ) - 1,
      ),
      priorLedgerRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
      subject: {
        kind: "forced",
        nativeTx,
        orderKey,
        verdict: { ForcedTxInvalid: { reason } },
      },
    });
    const setup = await stage("forced-setup", () =>
      submitSetupTx({
        lucid: harness.funderLucid,
        contracts: harness.contracts,
        nonceUtxo: harness.nonceUtxo,
        catalogue: harness.catalogue,
        header: block.header,
      }),
    );
    const compact = deriveMidgardNativeTxWitnessSetCompact(nativeTx.witnessSet);
    const witnessSet: SDK.NativeTxWitnessSetCompact = {
      addr_tx_wits_hash: Buffer.from(compact.addrTxWitsHash).toString("hex"),
      script_tx_wits_hash: Buffer.from(compact.scriptTxWitsHash).toString(
        "hex",
      ),
      redeemer_tx_wits_hash: Buffer.from(compact.redeemerTxWitsHash).toString(
        "hex",
      ),
    };
    const evidence = prepareWitnessScriptDecodingEvidence({
      finding: {
        subject: SDK.forcedVerdictSubject({
          transactionId: block.nativeTxId,
          sourceKey: orderKey,
          rejectionReason: reason,
        }),
        witnessSetHash:
          nativeTx.compact.transactionWitnessSetHash.toString("hex"),
        scriptIndex: 0,
      },
      fieldPreimage,
      committedFieldHashHex: witnessSet.script_tx_wits_hash,
    });
    expect(
      detectWitnessScriptDecodingCompleteReplay({
        headerHash: block.headerHash,
        transactions: [],
        reconstruction: block.reconstruction,
      } as never),
    ).toEqual([
      expect.objectContaining({
        violationId: "witness-script-header-malformed",
      }),
    ]);
    expect(
      deriveWitnessScriptDecodingEvidenceFromCanonicalBlock({
        headerHash: block.headerHash,
        transactions: [],
        reconstruction: block.reconstruction,
      } as never).itemCommitmentHex,
    ).toBe(evidence.itemCommitmentHex);
    expect(
      detectWitnessScriptDecodingCompleteReplay({
        headerHash: block.headerHash,
        transactions: [],
        reconstruction: {
          ...block.reconstruction,
          forcedTransactions: block.reconstruction.forcedTransactions.map(
            (transaction) => ({
              ...transaction,
              value: { ...transaction.value, verdict: "ForcedTxValid" },
            }),
          ),
        },
      } as never),
    ).toEqual([]);
    const stepRefs: UTxO[] = [];
    for (const [index, step] of family.steps.entries()) {
      stepRefs.push(
        (
          await stage(`forced-publish-${index}`, () =>
            publishPlainReferenceScriptUtxo({
              lucid: harness.funderLucid,
              script: step.spendingScript,
              label: `witness forced step ${index + 1}`,
            }),
          )
        ).utxo,
      );
    }
    const init = await measured("forced-init", () =>
      submitWitnessScriptDecodingInit({
        lucid: harness.proverLucid,
        blueprint: harness.realBlueprint,
        network,
        contracts: family,
        category,
        catalogue: {
          policyId: harness.contracts.fraudProofCatalogue.policyId,
          spendingScriptAddress:
            harness.contracts.fraudProofCatalogue.spendingScriptAddress,
          root: harness.catalogue.root,
        },
        signer: harness.proverSigner,
        fraudulentBlockOutRef: setup.fraudulentBlockOutRef,
        witnessReferenceScripts: harness.witnessReferenceScripts,
      }),
    );
    const membership = await buildForcedTransactionLeafMembershipProof({
      reconstruction: block.reconstruction,
      eventKey: { ForcedTransactionEventKey: { tx_order_id: orderKey } },
    });
    const step01 = await measured("forced-step01", () =>
      submitWitnessScriptDecodingStep01Forced({
        lucid: harness.proverLucid,
        contracts: family,
        categoryId: category.categoryId,
        signer: harness.proverSigner,
        threadOutRef: init.nextThreadOutRef,
        header: block.header,
        membership,
        direction: 1n,
        witnessSetHash: evidence.finding.witnessSetHash,
        scriptIndex: 0n,
        referenceScriptUtxo: stepRefs[0]!,
      }),
    );
    const step02 = await measured("forced-step02", () =>
      submitWitnessScriptDecodingStep02({
        lucid: harness.proverLucid,
        network,
        contracts: family,
        categoryId: category.categoryId,
        signer: harness.proverSigner,
        threadOutRef: step01.nextThreadOutRef,
        evidence,
        nativeTxCompactCbor: encodeMidgardNativeTxCompact(
          nativeTx.compact,
        ).toString("hex"),
        witnessSet,
        witnessSetCompactCbor:
          encodeMidgardNativeTxWitnessSetCompact(compact).toString("hex"),
        scriptWitnessItems: [item],
        referenceScriptUtxo: stepRefs[1]!,
      }),
    );
    let outRef = step02.nextThreadOutRef;
    for (;;) {
      const scan = await measured("forced-step03", () =>
        submitWitnessScriptDecodingStep03({
          lucid: harness.proverLucid,
          contracts: family,
          categoryId: category.categoryId,
          signer: harness.proverSigner,
          threadOutRef: outRef,
          evidence,
          referenceScriptUtxo: stepRefs[2]!,
        }),
      );
      outRef = scan.nextThreadOutRef;
      if (scan.closed) break;
    }
    const final = await measured("forced-step04", () =>
      submitWitnessScriptDecodingStep04({
        lucid: harness.proverLucid,
        contracts: family,
        categoryId: category.categoryId,
        signer: harness.proverSigner,
        threadOutRef: outRef,
        evidence,
        referenceScriptUtxo: stepRefs[3]!,
        witnessReferenceScripts: harness.witnessReferenceScripts,
      }),
    );
    expect(final.fraudProofUnit).toContain(category.categoryId);
    const removalReferenceScripts = await publishRemovalReferenceScripts({
      lucid: harness.proverLucid,
      contracts: harness.contracts,
    });
    const removeNow = BigInt(harness.emulator.now());
    const removal = await measured("forced-removal", () =>
      submitRemoveFraudulentBlock({
        lucid: harness.proverLucid,
        blueprint: harness.realBlueprint,
        deploymentInfo: buildRemovalDeploymentInfo(
          harness.contracts,
          harness.catalogue,
          { removalReferenceScripts: removalReferenceScripts.published },
        ),
        network,
        signer: harness.proverSigner,
        fraudCategory: "witnessScriptDecoding",
        fraudulentHeaderHash: setup.headerHash,
        awaitConfirmation: true,
        requireReferenceScripts: true,
        validFrom: removeNow > 120_000n ? removeNow - 120_000n : 0n,
        validTo: removeNow + 300_000n,
      }),
    );
    expect(removal.transactions[0]?.kind).toBe("remove-target");
    for (const row of measurements) {
      expect(row.bytes, row.label).toBeLessThanOrEqual(16_384);
      expect(BigInt(row.memory), row.label).toBeLessThanOrEqual(16_500_000n);
      expect(BigInt(row.cpu), row.label).toBeLessThanOrEqual(10_000_000_000n);
    }
    console.info(
      `[witness-script-decoding-forced-fit] ${JSON.stringify(measurements)}`,
    );
  }, 600_000);
});
