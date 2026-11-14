import { Store, Trie } from "@aiken-lang/merkle-patricia-forestry";
import {
  computeMidgardNativeTxId,
  deriveMidgardNativeTxWitnessSetCompact,
  encodeMidgardNativeTxCompact,
  encodeMidgardNativeTxWitnessSetCompact,
  midgardFieldCommitment,
} from "@al-ft/midgard-core";
import {
  AddressData,
  addressDataFromBech32,
  Proof,
  type VerdictSubject,
} from "@al-ft/midgard-sdk";
import { Data, type UTxO } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { expect } from "vitest";

import { submitCommittedFieldShapeInit } from "../src/committed-field-shape/submit-committed-field-shape-init.js";
import { submitRemoveFraudulentBlock } from "../src/remove-fraudulent-block.js";
import { nativeTxFromCoreCompact } from "../src/step-support.js";
import {
  applyTransactionOutputNonCanonicalScripts,
  prepareTransactionOutputEvidence,
  submitTransactionOutputNonCanonicalCancel,
  TRANSACTION_OUTPUT_NON_CANONICAL_BLUEPRINT_TITLES,
  type TransactionOutputNonCanonicalContracts,
  TransactionOutputScanControlSchema,
} from "../src/transaction-output-non-canonical/index.js";
import { makeFaultProofEmulatorHarness } from "./support/emulator/harness.js";
import {
  captureEmulatorSubmission,
  type CompleteSignedTransactionMeasurement,
} from "./support/emulator/measurement.js";
import {
  l2TransactionSourceCbor,
  makeNativeTx,
} from "./support/emulator/native-tx.js";
import { publishPlainReferenceScriptUtxo } from "./support/emulator/reference-scripts.js";
import {
  expectRegisteredChainParity,
  familyStepsFromRegisteredChain,
} from "./support/emulator/registered-chain.js";
import { buildRemovalDeploymentInfo } from "./support/emulator/removal-deployment.js";
import { createLifecycleCoverageRecorder } from "./support/lifecycle-coverage.js";
import { publishRemovalReferenceScripts } from "./support/submit-init-emulator-shared.js";
import { type Common } from "./support/transaction-output-non-canonical-emulator.js";

export const network = "Custom" as const;

const CATEGORY_ID = "00000029";

export const MAXIMUM_OUTPUT_BYTES = 16_384;

export const coverage = createLifecycleCoverageRecorder();

/** Physical step positions: step 01, 02, 03, the datum step, step 04. */
export type StepIndex = 0 | 1 | 2 | 3 | 4;

/** A physical step's name in labels and coverage. */
export const stepLabel = (stepIndex: StepIndex): string =>
  ["01", "02", "03", "03-datum", "04"][stepIndex]!;

type Harness = Awaited<ReturnType<typeof makeFaultProofEmulatorHarness>>;

export type NativeTx = ReturnType<typeof makeNativeTx>;

export type Evidence = ReturnType<typeof prepareTransactionOutputEvidence>;

/**
 * The registered chain is the deployed identity: the harness folds its first
 * step into the catalogue root. The family-side application must reproduce it
 * step for step before the suite drives it.
 */
export const registeredContracts = async (harness: Harness) => {
  const addressData = await Effect.runPromise(
    addressDataFromBech32(
      harness.contracts.fraudProof.spendingScriptAddress,
    ).pipe(Effect.map((address) => Data.from(Data.to(address, AddressData)))),
  );
  const registered =
    harness.contracts.fraudProofContracts.transactionOutputNonCanonical;
  const category = harness.catalogue.categories.transactionOutputNonCanonical;
  expectRegisteredChainParity({
    registered,
    applied: applyTransactionOutputNonCanonicalScripts({
      blueprint: harness.realBlueprint,
      network,
      computationThreadPolicyId: harness.contracts.computationThread.policyId,
      fraudProofPolicyId: harness.contracts.fraudProof.policyId,
      fraudProofTokenAddressData: addressData,
      fieldPreimageCertificatePolicyId:
        harness.contracts.fieldPreimageCertificate.policyId,
      hubOracleScriptHash: harness.contracts.hubOracle.spendingScriptHash,
    }),
    category,
  });
  const steps = familyStepsFromRegisteredChain(
    registered.steps,
    Object.values(TRANSACTION_OUTPUT_NON_CANONICAL_BLUEPRINT_TITLES),
  );
  const contracts: TransactionOutputNonCanonicalContracts = {
    steps,
    computationThread: harness.contracts.computationThread,
    fraudProof: harness.contracts.fraudProof,
    hubOraclePolicyId: harness.contracts.hubOracle.policyId,
    stateQueuePolicyId: harness.contracts.stateQueue.policyId,
    fieldPreimageCertificatePolicyId:
      harness.contracts.fieldPreimageCertificate.policyId,
    fieldPreimageCertificateMintingScript:
      harness.contracts.fieldPreimageCertificate.mintingScript,
  };
  // Published only after the fraudulent block is set up: setup consumes the
  // harness nonce, which reference publication must not spend first.
  const references: UTxO[] = [];
  const publishReferences = async (
    onPublication?: (
      stepIndex: StepIndex,
      measurement: CompleteSignedTransactionMeasurement,
    ) => void,
  ): Promise<UTxO> => {
    for (const [index, step] of steps.entries()) {
      const published = await captureEmulatorSubmission(harness.emulator, () =>
        publishPlainReferenceScriptUtxo({
          lucid: harness.funderLucid,
          script: step.spendingScript,
          label: `transaction-output-non-canonical-${index.toString()}`,
        }),
      );
      references.push(published.result.utxo);
      onPublication?.(index as StepIndex, published.measurement);
    }
    return (
      await publishPlainReferenceScriptUtxo({
        lucid: harness.funderLucid,
        script: harness.contracts.fieldPreimageCertificate.mintingScript,
        label: "transaction-output-non-canonical-certificate",
      })
    ).utxo;
  };
  const common = (threadOutRef: string, stepIndex: StepIndex): Common => ({
    lucid: harness.proverLucid,
    contracts,
    categoryId: category.categoryId,
    signer: harness.proverSigner,
    threadOutRef,
    referenceScriptUtxo: references[stepIndex]!,
  });
  const init = async (fraudulentBlockOutRef: string) =>
    await captureEmulatorSubmission(harness.emulator, () =>
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
          root: harness.catalogue.root,
        },
        signer: harness.proverSigner,
        fraudulentBlockOutRef,
        witnessReferenceScripts: harness.witnessReferenceScripts,
      }),
    );
  const threadUtxoOf = async (
    txHash: string,
    outputIndex: number,
  ): Promise<UTxO> => {
    const [utxo] = await harness.proverLucid.utxosByOutRef([
      { txHash, outputIndex },
    ]);
    if (utxo === undefined) throw new Error(`thread ${txHash} absent`);
    return utxo;
  };
  const cancel = async (threadOutRef: string, stepIndex: StepIndex) =>
    await captureEmulatorSubmission(harness.emulator, () =>
      submitTransactionOutputNonCanonicalCancel({
        lucid: harness.proverLucid,
        contracts,
        categoryId: category.categoryId,
        signer: harness.proverSigner,
        threadOutRef,
        referenceScriptUtxo: references[stepIndex]!,
        witnessReferenceScripts: harness.witnessReferenceScripts,
      }),
    );
  const removal = async (fraudulentHeaderHash: string) => {
    const removalReferences = await publishRemovalReferenceScripts({
      lucid: harness.proverLucid,
      contracts: harness.contracts,
    });
    // A registered family resolves removal through the canonical catalogue:
    // the manifest's fraudProofTransactionOutputNonCanonical entries carry
    // the registered chain the harness built.
    const deploymentInfo = buildRemovalDeploymentInfo(
      harness.contracts,
      harness.catalogue,
      { removalReferenceScripts: removalReferences.published },
    );
    const now = BigInt(harness.emulator.now());
    const removed = await captureEmulatorSubmission(harness.emulator, () =>
      submitRemoveFraudulentBlock({
        lucid: harness.proverLucid,
        blueprint: harness.realBlueprint,
        deploymentInfo,
        network,
        signer: harness.proverSigner,
        fraudCategory: "transactionOutputNonCanonical",
        fraudulentHeaderHash,
        awaitConfirmation: true,
        requireReferenceScripts: true,
        validFrom: now > 120_000n ? now - 120_000n : 0n,
        validTo: now + 300_000n,
      }),
    );
    expect(removed.result.fraudCategoryId).toBe(CATEGORY_ID);
    return removed;
  };
  return {
    harness,
    contracts,
    catalogue: harness.catalogue,
    category,
    references,
    publishReferences,
    common,
    init,
    threadUtxoOf,
    cancel,
    removal,
  };
};

export type Registered = Awaited<ReturnType<typeof registeredContracts>>;

export const evidenceOf = (
  subject: VerdictSubject,
  nativeTx: NativeTx,
  itemIndex: number,
): Evidence =>
  prepareTransactionOutputEvidence({
    finding: { subject, fieldIndex: 2, itemIndex },
    fieldPreimage: nativeTx.body.outputsPreimageCbor,
    committedFieldHashHex: midgardFieldCommitment(
      nativeTx.body.outputsPreimageCbor,
    ).toString("hex"),
  });

export const witnessSetCborOf = (nativeTx: NativeTx): string =>
  encodeMidgardNativeTxWitnessSetCompact(
    deriveMidgardNativeTxWitnessSetCompact(nativeTx.witnessSet),
  ).toString("hex");

/** Commits every transaction in one transactions trie and proves each of them. */
export const committedInclusions = async (nativeTxs: readonly NativeTx[]) => {
  const store = new Store(undefined);
  await store.ready();
  const trie = new Trie(store);
  const ids = nativeTxs.map((nativeTx) =>
    computeMidgardNativeTxId(nativeTx).toString("hex"),
  );
  for (const [index, nativeTx] of nativeTxs.entries()) {
    await trie.insert(
      Buffer.from(ids[index]!, "hex"),
      Buffer.from(l2TransactionSourceCbor(nativeTx), "hex"),
    );
  }
  const transactionsRoot = Buffer.from(trie.hash).toString("hex");
  const inclusions = [];
  for (const [index, nativeTx] of nativeTxs.entries()) {
    const proof = await trie.prove(Buffer.from(ids[index]!, "hex"));
    inclusions.push({
      nativeTxId: ids[index]!,
      nativeTx: nativeTxFromCoreCompact(nativeTx.compact),
      nativeTxCompactCbor: encodeMidgardNativeTxCompact(
        nativeTx.compact,
      ).toString("hex"),
      l2TransactionSourceCbor: l2TransactionSourceCbor(nativeTx),
      transactionsPhasRoot: transactionsRoot,
      txMembershipProof: Data.from(proof.toCBOR().toString("hex"), Proof),
      txMembershipProofCbor: proof.toCBOR().toString("hex"),
    });
  }
  return { transactionsRoot, inclusions };
};

export type Inclusion = Awaited<
  ReturnType<typeof committedInclusions>
>["inclusions"][number];

export const controlCbor = (control: unknown): string =>
  Data.to(control as never, TransactionOutputScanControlSchema as never);
