import { Store, Trie } from "@aiken-lang/merkle-patricia-forestry";
import {
  adjudicateMidgardNativeTxFullValidity,
  computeMidgardNativeTxId,
  deriveMidgardNativeTxProofSource,
  encodeMidgardFieldPreimage,
  encodeMidgardNativeTxCompact,
  midgardFieldCommitment,
} from "@al-ft/midgard-core";
import { encodeMidgardSpendInputItem } from "@al-ft/midgard-core/codec";
import {
  acceptedVerdictSubject,
  AddressData,
  addressDataFromBech32,
  ForcedInclusionTxSchema,
  forcedVerdictSubject,
  OutputReference,
  Proof,
  ROOT_DOMAINS,
} from "@al-ft/midgard-sdk";
import { Data, getAddressDetails, type UTxO } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import { submitCommittedFieldShapeInit } from "../src/committed-field-shape/submit-committed-field-shape-init.js";
import {
  planFaultProofFieldOpening,
  publishFaultProofFieldCarriage,
} from "../src/field-opening-v1.js";
import { submitRemoveFraudulentBlock } from "../src/remove-fraudulent-block.js";
import { nativeTxFromCoreCompact } from "../src/submit-step-01.js";
import { buildCountedRoot } from "../src/transition-trace/phas.js";
import {
  applyZeroInputScripts,
  type ZeroInputContracts,
} from "../src/zero-input/contracts-v1.js";
import {
  prepareZeroInputEvidence,
  ZERO_INPUT_CATEGORY_ID,
} from "../src/zero-input/family-v1.js";
import { submitZeroInputCancel } from "../src/zero-input/submit-cancel-v1.js";
import {
  submitZeroInputStep01Accepted,
  submitZeroInputStep01Forced,
} from "../src/zero-input/submit-step-01-v1.js";
import { submitZeroInputStep02V1 } from "../src/zero-input/submit-step-02-v1.js";
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
import { buildInvalidForcedTransitionTraceFixture } from "./support/submit-init-emulator-fixtures.js";
import { setupFraudulentBlock } from "./support/submit-init-emulator-fixtures.js";
import { publishRemovalReferenceScripts } from "./support/submit-init-emulator-shared.js";

const network = "Custom" as const;

describe("zeroInput wrongful-rejection real lifecycle", () => {
  it("preserves the accepted-invalid Init to permanent-mint lifecycle", async () => {
    const harness = await makeFaultProofEmulatorHarness({
      contractOptions: { realZeroInput: true, alwaysFraudProofCatalogue: true },
    });
    const addressData = await Effect.runPromise(
      addressDataFromBech32(
        harness.contracts.fraudProof.spendingScriptAddress,
      ).pipe(Effect.map((address) => Data.from(Data.to(address, AddressData)))),
    );
    const applied = applyZeroInputScripts({
      blueprint: harness.realBlueprint,
      network,
      computationThreadPolicyId: harness.contracts.computationThread.policyId,
      fraudProofPolicyId: harness.contracts.fraudProof.policyId,
      fraudProofTokenAddressData: addressData,
      fieldPreimageCertificatePolicyId:
        harness.contracts.fieldPreimageCertificate.policyId,
      hubOracleScriptHash: harness.contracts.hubOracle.spendingScriptHash,
    });
    const contracts: ZeroInputContracts = {
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
    expect(harness.contracts.fraudProofs.zeroInput.spendingScriptHash).toBe(
      applied[0].spendingScriptHash,
    );
    const catalogue = await buildCatalogueDeploymentInfo(
      harness.contracts.fraudProofs,
    );
    const category = catalogue.categories.zeroInput;
    const nativeTx = makeNativeTx({ spendInputCbors: [], fee: 0n });
    const transactionId = computeMidgardNativeTxId(nativeTx).toString("hex");
    const compact = encodeMidgardNativeTxCompact(nativeTx.compact);
    const sourceCbor = l2TransactionSourceCborV1(nativeTx);
    const store = new Store(undefined);
    await store.ready();
    const trie = new Trie(store);
    await trie.insert(
      Buffer.from(transactionId, "hex"),
      Buffer.from(sourceCbor, "hex"),
    );
    const proof = await trie.prove(Buffer.from(transactionId, "hex"));
    const transactionsRoot = Buffer.from(trie.hash).toString("hex");
    const txInclusion = {
      nativeTxId: transactionId,
      nativeTx: nativeTxFromCoreCompact(nativeTx.compact),
      nativeTxCompactCbor: compact.toString("hex"),
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
    const inputField = encodeMidgardFieldPreimage([]);
    const evidence = prepareZeroInputEvidence({
      finding: { subject: acceptedVerdictSubject(transactionId) },
      inputFieldPreimage: inputField,
      committedFieldHashHex: midgardFieldCommitment(inputField).toString("hex"),
    });
    const planned = planFaultProofFieldOpening({
      fieldIndex: 0,
      anchorTxId: transactionId,
      nativeTxCompactCbor: compact.toString("hex"),
      itemCbors: [],
      owner: harness.proverSigner.paymentKeyHash,
      publish: true,
      label: "zero input accepted field",
    });
    await publishFaultProofFieldCarriage({
      lucid: harness.proverLucid,
      signer: harness.proverSigner,
      planned,
      publisherAddress: harness.proverSigner.address,
      label: "zero input accepted field",
    });
    const references: UTxO[] = [];
    for (const [index, step] of applied.entries())
      references.push(
        (
          await publishPlainReferenceScriptUtxo({
            lucid: harness.funderLucid,
            script: step.spendingScript,
            label: `zero-input-accepted-${index.toString()}`,
          })
        ).utxo,
      );
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
    const [threadUtxo] = await harness.proverLucid.utxosByOutRef([
      {
        txHash: initialized.result.txHash,
        outputIndex: Number(initialized.result.firstStepOutputIndex),
      },
    ]);
    if (threadUtxo === undefined)
      throw new Error("accepted Init thread absent");
    const bound = await captureEmulatorSubmission(harness.emulator, () =>
      submitZeroInputStep01Accepted({
        lucid: harness.proverLucid,
        blueprint: harness.realBlueprint,
        network,
        contracts,
        signer: harness.proverSigner,
        finding: evidence,
        threadUtxo,
        threadToken: {
          unit: initialized.result.computationThreadUnit,
          fraudulentHeaderHash: initialized.result.fraudulentHeaderHash,
        },
        stateQueueBlockOutRef: setup.fraudulentBlockOutRef,
        txInclusion,
        referenceScriptUtxo: references[0]!,
        witnessReferenceScripts: harness.witnessReferenceScripts,
      }),
    );
    expect(threadOutRef).toContain("#");
    const final = await captureEmulatorSubmission(harness.emulator, () =>
      submitZeroInputStep02V1({
        lucid: harness.proverLucid,
        contracts,
        categoryId: category.categoryId,
        signer: harness.proverSigner,
        threadOutRef: bound.result.nextThreadOutRef,
        evidence,
        nativeTxCompactCbor: compact.toString("hex"),
        referenceScriptUtxo: references[1]!,
        witnessReferenceScripts: harness.witnessReferenceScripts,
      }),
    );
    expect(final.result.fraudProofUnit).toBeTruthy();
    for (const captured of [initialized, bound, final]) {
      expect(captured.measurement.l1ByteMargin).toBeGreaterThan(0);
      expect(captured.measurement.executionMemory).toBeLessThanOrEqual(
        16_500_000n,
      );
      expect(captured.measurement.executionSteps).toBeLessThanOrEqual(
        10_000_000_000n,
      );
    }
    console.info(
      `[zero-input-accepted-lifecycle] ${JSON.stringify([initialized, bound, final].map(({ measurement }) => ({ bytes: measurement.completeSignedBytes, memory: measurement.executionMemory.toString(), cpu: measurement.executionSteps.toString() })))}`,
    );
  }, 600_000);

  it("runs Init, both cancel boundaries, out-ref restart, forced contradiction, and permanent mint", async () => {
    const harness = await makeFaultProofEmulatorHarness({
      contractOptions: {
        realZeroInput: true,
        realInputSetUniqueness: true,
        alwaysFraudProofCatalogue: true,
        alwaysStateQueue: true,
      },
    });
    const addressData = await Effect.runPromise(
      addressDataFromBech32(
        harness.contracts.fraudProof.spendingScriptAddress,
      ).pipe(Effect.map((address) => Data.from(Data.to(address, AddressData)))),
    );
    const applied = applyZeroInputScripts({
      blueprint: harness.realBlueprint,
      network,
      computationThreadPolicyId: harness.contracts.computationThread.policyId,
      fraudProofPolicyId: harness.contracts.fraudProof.policyId,
      fraudProofTokenAddressData: addressData,
      fieldPreimageCertificatePolicyId:
        harness.contracts.fieldPreimageCertificate.policyId,
      hubOracleScriptHash: harness.contracts.hubOracle.spendingScriptHash,
    });
    const contracts: ZeroInputContracts = {
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
    expect(harness.contracts.fraudProofs.zeroInput.spendingScriptHash).toBe(
      applied[0].spendingScriptHash,
    );
    const catalogue = await buildCatalogueDeploymentInfo(
      harness.contracts.fraudProofs,
    );
    const category = catalogue.categories.zeroInput;
    expect(category.categoryId).toBe(ZERO_INPUT_CATEGORY_ID);
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
    const inputItem = encodeMidgardSpendInputItem({
      txId: Buffer.from("77".repeat(32), "hex"),
      outputIndex: 0,
    });
    const inputField = encodeMidgardFieldPreimage([inputItem]);
    const submitted = makeNativeTx({ spendInputCbors: [inputItem], fee: 0n });
    const invalid = adjudicateMidgardNativeTxFullValidity(
      submitted,
      "TxIsInvalid",
    );
    const transactionId = computeMidgardNativeTxId(invalid).toString("hex");
    const proofSource = deriveMidgardNativeTxProofSource(invalid);
    const forcedTransaction = {
      tx_id: transactionId,
      source: {
        compact_cbor: proofSource.compactCbor.toString("hex"),
        witness_set_compact_cbor:
          proofSource.witnessSetCompactCbor.toString("hex"),
        field_preimage_lengths_cbor:
          proofSource.fieldPreimageLengthsCbor.toString("hex"),
      },
      verdict: { ForcedTxInvalid: { reason: "EmptyInputs" } },
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
    const store = new Store(undefined);
    await store.ready();
    const trie = new Trie(store);
    await trie.insert(keyBytes, valueBytes);
    const membershipProof = await trie.prove(keyBytes);
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
    const evidence = prepareZeroInputEvidence({
      finding: {
        subject: forcedVerdictSubject({
          transactionId,
          sourceKey,
          rejectionReason: "EmptyInputs",
        }),
      },
      inputFieldPreimage: inputField,
      committedFieldHashHex: midgardFieldCommitment(inputField).toString("hex"),
    });
    const planned = planFaultProofFieldOpening({
      fieldIndex: 0,
      anchorTxId: transactionId,
      nativeTxCompactCbor: proofSource.compactCbor.toString("hex"),
      itemCbors: [inputItem],
      owner: harness.proverSigner.paymentKeyHash,
      publish: true,
      label: "zero input forced field",
    });
    await publishFaultProofFieldCarriage({
      lucid: harness.proverLucid,
      signer: harness.proverSigner,
      planned,
      publisherAddress: harness.proverSigner.address,
      label: "zero input forced field",
    });
    const references: UTxO[] = [];
    for (const [index, step] of applied.entries())
      references.push(
        (
          await publishPlainReferenceScriptUtxo({
            lucid: harness.funderLucid,
            script: step.spendingScript,
            label: `zero-input-forced-${index.toString()}`,
          })
        ).utxo,
      );
    const captures: Awaited<ReturnType<typeof captureEmulatorSubmission>>[] =
      [];
    const initialize = async () => {
      const captured = await captureEmulatorSubmission(harness.emulator, () =>
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
      captures.push(captured);
      return `${captured.result.txHash}#${captured.result.firstStepOutputIndex.toString()}`;
    };
    const bind = async (threadOutRef: string) => {
      const captured = await captureEmulatorSubmission(harness.emulator, () =>
        submitZeroInputStep01Forced({
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
      captures.push(captured);
      return captured.result.nextThreadOutRef;
    };

    const cancelAtStep01 = await initialize();
    captures.push(
      await captureEmulatorSubmission(harness.emulator, () =>
        submitZeroInputCancel({
          lucid: harness.proverLucid,
          contracts,
          categoryId: category.categoryId,
          signer: harness.proverSigner,
          threadOutRef: cancelAtStep01,
          referenceScriptUtxo: references[0]!,
          witnessReferenceScripts: harness.witnessReferenceScripts,
        }),
      ),
    );

    const cancelAtStep02 = await bind(await initialize());
    captures.push(
      await captureEmulatorSubmission(harness.emulator, () =>
        submitZeroInputCancel({
          lucid: harness.proverLucid,
          contracts,
          categoryId: category.categoryId,
          signer: harness.proverSigner,
          threadOutRef: cancelAtStep02,
          referenceScriptUtxo: references[1]!,
          witnessReferenceScripts: harness.witnessReferenceScripts,
        }),
      ),
    );

    const resumedStep02OutRef = await bind(await initialize());
    const final = await captureEmulatorSubmission(harness.emulator, () =>
      submitZeroInputStep02V1({
        lucid: harness.proverLucid,
        contracts,
        categoryId: category.categoryId,
        signer: harness.proverSigner,
        threadOutRef: resumedStep02OutRef,
        evidence,
        nativeTxCompactCbor: proofSource.compactCbor.toString("hex"),
        referenceScriptUtxo: references[1]!,
        witnessReferenceScripts: harness.witnessReferenceScripts,
      }),
    );
    captures.push(final);
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
        ...Object.fromEntries(
          applied.map((step, index) => [
            index === 0 ? "fraudProofZeroInput" : "fraudProofZeroInputStep02",
            {
              scriptHash: step.spendingScriptHash,
              contract: {
                type: step.spendingScript.type,
                cborHex: step.spendingScript.script,
              },
            },
          ]),
        ),
      },
    };
    const now = BigInt(harness.emulator.now());
    captures.push(
      await captureEmulatorSubmission(harness.emulator, () =>
        submitRemoveFraudulentBlock({
          lucid: harness.proverLucid,
          blueprint: harness.realBlueprint,
          deploymentInfo,
          network,
          signer: harness.proverSigner,
          fraudCategory: "zeroInput",
          fraudulentHeaderHash: setup.headerHash,
          awaitConfirmation: true,
          requireReferenceScripts: true,
          stateQueueMutationLeaseCoordinator: {
            acquire: async () => ({
              token: "zero-input-emulator-lease",
              source: "emulator",
              renew: async () => {},
              release: async () => {},
              fail: async () => {},
            }),
          },
          validFrom: now > 120_000n ? now - 120_000n : 0n,
          validTo: now + 300_000n,
        }),
      ),
    );
    for (const captured of captures) {
      expect(captured.measurement.l1ByteMargin).toBeGreaterThan(0);
      expect(captured.measurement.executionMemory).toBeLessThanOrEqual(
        16_500_000n,
      );
      expect(captured.measurement.executionSteps).toBeLessThanOrEqual(
        10_000_000_000n,
      );
    }
    console.info(
      `[zero-input-lifecycle] ${JSON.stringify(captures.map(({ measurement }) => ({ bytes: measurement.completeSignedBytes, memory: measurement.executionMemory.toString(), cpu: measurement.executionSteps.toString() })))}`,
    );
  }, 600_000);
});
