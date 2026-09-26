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

import { submitCommittedFieldShapeInit } from "../../src/committed-field-shape/submit-committed-field-shape-init.js";
import {
  applyMintItemNonCanonicalScripts,
  createManifestBoundMintItemNonCanonicalSubmission,
  createMintItemNonCanonicalCentralJournalAdapter,
  MINT_ITEM_NON_CANONICAL_BLUEPRINT_TITLES,
  mintItemEvidenceIdentity,
  type MintItemNonCanonicalContracts,
  prepareMintItemEvidence,
  submitMintItemNonCanonicalCancel,
} from "../../src/mint-item-non-canonical/index.js";
import { submitRemoveFraudulentBlock } from "../../src/remove-fraudulent-block.js";
import { nativeTxFromCoreCompact } from "../../src/step-support.js";
import type { FraudProofWorkflowJournalStore } from "../../src/workflow/journal.js";
import { makeFaultProofEmulatorHarness } from "./emulator/harness.js";
import { captureEmulatorSubmission } from "./emulator/measurement.js";
import { l2TransactionSourceCbor, makeNativeTx } from "./emulator/native-tx.js";
import { publishPlainReferenceScriptUtxo } from "./emulator/reference-scripts.js";
import {
  expectRegisteredChainParity,
  familyStepsFromRegisteredChain,
} from "./emulator/registered-chain.js";
import { buildRemovalDeploymentInfo } from "./emulator/removal-deployment.js";
import type { Common } from "./mint-item-non-canonical-emulator.js";
import { publishRemovalReferenceScripts } from "./submit-init-emulator-shared.js";

const network = "Custom" as const;
const CATEGORY_ID = "00000036";
type Harness = Awaited<ReturnType<typeof makeFaultProofEmulatorHarness>>;
type NativeTx = ReturnType<typeof makeNativeTx>;
type Evidence = ReturnType<typeof prepareMintItemEvidence>;

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
  const registered = harness.contracts.fraudProofContracts.mintItemNonCanonical;
  const category = harness.catalogue.categories.mintItemNonCanonical;
  expectRegisteredChainParity({
    registered,
    applied: applyMintItemNonCanonicalScripts({
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
    Object.values(MINT_ITEM_NON_CANONICAL_BLUEPRINT_TITLES),
  );
  const contracts: MintItemNonCanonicalContracts = {
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
  const publishReferences = async (): Promise<UTxO> => {
    for (const [index, step] of steps.entries()) {
      references.push(
        (
          await publishPlainReferenceScriptUtxo({
            lucid: harness.funderLucid,
            script: step.spendingScript,
            label: `mint-item-non-canonical-${index.toString()}`,
          })
        ).utxo,
      );
    }
    return (
      await publishPlainReferenceScriptUtxo({
        lucid: harness.funderLucid,
        script: harness.contracts.fieldPreimageCertificate.mintingScript,
        label: "mint-item-non-canonical-certificate",
      })
    ).utxo;
  };
  const common = (threadOutRef: string, stepIndex: 0 | 1 | 2 | 3): Common => ({
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
  const cancel = async (threadOutRef: string, stepIndex: 0 | 1 | 2 | 3) =>
    await captureEmulatorSubmission(harness.emulator, () =>
      submitMintItemNonCanonicalCancel({
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
    // the manifest's fraudProofMintItemNonCanonical entries carry
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
        fraudCategory: "mintItemNonCanonical",
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

export const evidenceOf = (
  subject: VerdictSubject,
  nativeTx: NativeTx,
  itemIndex: number,
): Evidence =>
  prepareMintItemEvidence({
    finding: { subject, fieldIndex: 5, itemIndex },
    fieldPreimage: nativeTx.body.mintPreimageCbor,
    committedFieldHashHex: midgardFieldCommitment(
      nativeTx.body.mintPreimageCbor,
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

/** Exercise the production submission adapter and durable bridge on real Lucid
 * continuations. Recreate both after every submission to cover crash recovery. */
export const journaledMintContinuation = async ({
  common,
  evidence,
  nativeTxCompactCbor,
  witnessSetCompactCbor,
  certificateReference,
  carriage,
  store,
  action,
  fraudulentBlockOutRef,
}: {
  common: Common;
  fraudulentBlockOutRef: string;
  evidence: Evidence;
  nativeTxCompactCbor: string;
  witnessSetCompactCbor: string;
  certificateReference: UTxO;
  carriage?: {
    publishedCarriageUtxos: readonly UTxO[];
    certificateUtxo?: UTxO;
  };
  store: FraudProofWorkflowJournalStore;
  action: "submitStep02" | "submitStep03";
}) => {
  const bridge = () =>
    createMintItemNonCanonicalCentralJournalAdapter({
      store,
      deploymentFingerprint: "11".repeat(32),
      headerHash: "22".repeat(28),
      decisionDigest: "33".repeat(32),
      transactionConfirmed: async () => true,
    });
  const centralJournal = bridge();
  const submission = createManifestBoundMintItemNonCanonicalSubmission({
    config: {
      lucid: common.lucid,
      contracts: common.contracts,
      signer: common.signer,
      binding: {
        resolvedContracts: { category: { categoryId: common.categoryId } },
      },
      referenceScripts: {
        step02: common.referenceScriptUtxo,
        step03: common.referenceScriptUtxo,
        fieldPreimageCertificateMint: certificateReference,
      },
    } as never,
    observe: async () => (action === "submitStep02" ? "step02" : "step03"),
    resolveStage: async () => ({
      fraudulentBlockOutRef,
      threadOutRef: common.threadOutRef,
      nativeTxCompactCbor,
      witnessSetCompactCbor,
      ...carriage,
    }),
    centralJournal,
  });
  const result = await submission.submit(action, evidence);
  // A process can stop immediately after the submitted transaction confirms.
  // The new adapter must reconcile its exact intent even for a self-loop.
  const restarted = bridge();
  await restarted.reconcile(result.stage);
  const identity = mintItemEvidenceIdentity(evidence);
  const records = await restarted.familyJournal.load(identity);
  expect(records.at(-1)).toMatchObject({
    stage: result.stage,
    txHash: result.txHash,
  });
  expect(result.outputReference).not.toBeNull();
  return {
    nextThreadOutRef: result.outputReference!,
    terminal:
      result.stage === (action === "submitStep02" ? "step03" : "step04"),
  };
};
