import { Store, Trie } from "@aiken-lang/merkle-patricia-forestry";
import {
  computeMidgardNativeTxId,
  encodeMidgardFieldPreimage,
  encodeMidgardNativeTxCompact,
  midgardFieldCommitment,
} from "@al-ft/midgard-core";
import { encodeMidgardSpendInputItem } from "@al-ft/midgard-core/codec";
import {
  deriveMidgardForcedTxProofSource,
  materializeMidgardForcedTxFromCanonical,
} from "@al-ft/midgard-core/codec/forced";
import {
  acceptedVerdictSubject,
  AddressData,
  addressDataFromBech32,
  ForcedInclusionTxV1Schema,
  forcedVerdictSubject,
  OutputReference,
  Proof,
  ROOT_DOMAINS,
} from "@al-ft/midgard-sdk";
import { Data, getAddressDetails, type UTxO } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import { submitCommittedFieldShapeInit } from "../../src/committed-field-shape/submit-committed-field-shape-init.js";
import { nativeTxFromCoreCompact } from "../../src/step-support.js";
import { buildCountedRoot } from "../../src/transition-trace/phas.js";
import {
  applyZeroInputScripts,
  type ZeroInputContracts,
} from "../../src/zero-input/contracts.js";
import {
  prepareZeroInputEvidence,
  type ZeroInputEvidence,
} from "../../src/zero-input/family.js";
import {
  submitZeroInputStep01Accepted,
  submitZeroInputStep01Forced,
} from "../../src/zero-input/submit-step-01.js";
import { buildCatalogueDeploymentInfo } from "./emulator/catalogue.js";
import { alignUnixTimeToEmulatorSlotBoundary } from "./emulator/emulator-context.js";
import {
  type FaultProofEmulatorHarness,
  makeFaultProofEmulatorHarness,
} from "./emulator/harness.js";
import { l2TransactionSourceCbor, makeNativeTx } from "./emulator/native-tx.js";
import { publishPlainReferenceScriptUtxo } from "./emulator/reference-scripts.js";
import { submitSetupTx } from "./emulator/setup-tx.js";
import {
  buildInvalidForcedTransitionTraceFixture,
  setupFraudulentBlock,
} from "./submit-init-emulator-fixtures.js";

const network = "Custom" as const;

/** The one spend input the item-bearing transactions carry. */
export const zeroInputHonestSpendInput = encodeMidgardSpendInputItem({
  txId: Buffer.from("77".repeat(32), "hex"),
  outputIndex: 0,
});

/** A zero-input thread parked at its terminal step. */
export type ZeroInputStagedThread = Readonly<{
  harness: FaultProofEmulatorHarness;
  contracts: ZeroInputContracts;
  categoryId: string;
  /** The thread UTxO locked at step 02. */
  terminalThreadOutRef: string;
  /** The finding the committed transaction yields. */
  evidence: ZeroInputEvidence;
  /** The disputed transaction's compact bytes, as the thread anchored them. */
  nativeTxCompactCbor: string;
  /** The items the disputed transaction really commits at field 0. */
  committedSpendInputs: readonly Buffer[];
  stepReferences: readonly [UTxO, UTxO];
}>;

const zeroInputContracts = async (harness: FaultProofEmulatorHarness) => {
  const addressData = await Effect.runPromise(
    addressDataFromBech32(
      harness.contracts.fraudProof.spendingScriptAddress,
    ).pipe(Effect.map((address) => Data.from(Data.to(address, AddressData)))),
  );
  const steps = applyZeroInputScripts({
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
  const catalogue = await buildCatalogueDeploymentInfo(
    harness.contracts.fraudProofs,
  );
  const references: UTxO[] = [];
  for (const [index, step] of steps.entries())
    references.push(
      (
        await publishPlainReferenceScriptUtxo({
          lucid: harness.funderLucid,
          script: step.spendingScript,
          label: `zero-input-honest-${index.toString()}`,
        })
      ).utxo,
    );
  return {
    contracts,
    catalogue,
    category: catalogue.categories.zeroInput,
    stepReferences: [references[0]!, references[1]!] as const,
  };
};

const initializeThread = async ({
  harness,
  contracts,
  catalogue,
  category,
  fraudulentBlockOutRef,
  fraudulentHeaderHash,
}: {
  readonly harness: FaultProofEmulatorHarness;
  readonly contracts: ZeroInputContracts;
  readonly catalogue: Awaited<ReturnType<typeof buildCatalogueDeploymentInfo>>;
  readonly category: Awaited<
    ReturnType<typeof buildCatalogueDeploymentInfo>
  >["categories"]["zeroInput"];
  readonly fraudulentBlockOutRef: string;
  readonly fraudulentHeaderHash: string;
}) =>
  await submitCommittedFieldShapeInit({
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
    fraudulentBlockOutRef,
    fraudulentHeaderHash,
    witnessReferenceScripts: harness.witnessReferenceScripts,
  });

/**
 * A committed block whose one transaction, accepted, spends `spendInputs`; the
 * prover opens a wrongful-acceptance thread on it and binds it at step 01. The
 * acceptance is honest when the transaction spends at least one input.
 */
export const stageZeroInputAcceptance = async ({
  spendInputs,
}: {
  readonly spendInputs: readonly Buffer[];
}): Promise<ZeroInputStagedThread> => {
  const harness = await makeFaultProofEmulatorHarness({
    contractOptions: { realZeroInput: true, alwaysFraudProofCatalogue: true },
  });
  const { contracts, catalogue, category, stepReferences } =
    await zeroInputContracts(harness);
  const nativeTx = makeNativeTx({
    spendInputCbors: [...spendInputs],
    fee: 0n,
  });
  const transactionId = computeMidgardNativeTxId(nativeTx).toString("hex");
  const compact = encodeMidgardNativeTxCompact(nativeTx.compact);
  const sourceCbor = l2TransactionSourceCbor(nativeTx);
  const store = new Store(undefined);
  await store.ready();
  const trie = new Trie(store);
  await trie.insert(
    Buffer.from(transactionId, "hex"),
    Buffer.from(sourceCbor, "hex"),
  );
  const proof = await trie.prove(Buffer.from(transactionId, "hex"));
  const transactionsRoot = Buffer.from(trie.hash).toString("hex");
  const setup = await setupFraudulentBlock({
    funderLucid: harness.funderLucid,
    emulator: harness.emulator,
    contracts: harness.contracts,
    catalogue,
    fixture: { transactionsRoot, l2TransactionCount: 1n },
  });
  const inputField = encodeMidgardFieldPreimage([...spendInputs]);
  const evidence = prepareZeroInputEvidence({
    finding: { subject: acceptedVerdictSubject(transactionId) },
    inputFieldPreimage: inputField,
    committedFieldHashHex: midgardFieldCommitment(inputField).toString("hex"),
  });
  const initialized = await initializeThread({
    harness,
    contracts,
    catalogue,
    category,
    fraudulentBlockOutRef: setup.fraudulentBlockOutRef,
    fraudulentHeaderHash: setup.headerHash,
  });
  const [threadUtxo] = await harness.proverLucid.utxosByOutRef([
    {
      txHash: initialized.txHash,
      outputIndex: Number(initialized.firstStepOutputIndex),
    },
  ]);
  if (threadUtxo === undefined) throw new Error("zero-input thread absent");
  const bound = await submitZeroInputStep01Accepted({
    lucid: harness.proverLucid,
    blueprint: harness.realBlueprint,
    network,
    contracts,
    signer: harness.proverSigner,
    finding: evidence,
    threadUtxo,
    threadToken: {
      unit: initialized.computationThreadUnit,
      fraudulentHeaderHash: initialized.fraudulentHeaderHash,
    },
    stateQueueBlockOutRef: setup.fraudulentBlockOutRef,
    txInclusion: {
      nativeTxId: transactionId,
      nativeTx: nativeTxFromCoreCompact(nativeTx.compact),
      nativeTxCompactCbor: compact.toString("hex"),
      l2TransactionSourceCbor: sourceCbor,
      transactionsPhasRoot: transactionsRoot,
      txMembershipProof: Data.from(proof.toCBOR().toString("hex"), Proof),
      txMembershipProofCbor: proof.toCBOR().toString("hex"),
    },
    referenceScriptUtxo: stepReferences[0],
    witnessReferenceScripts: harness.witnessReferenceScripts,
  });
  return {
    harness,
    contracts,
    categoryId: category.categoryId,
    terminalThreadOutRef: bound.nextThreadOutRef,
    evidence,
    nativeTxCompactCbor: compact.toString("hex"),
    committedSpendInputs: spendInputs,
    stepReferences,
  };
};

/**
 * A committed block whose forced leaf records a forced transaction with no
 * spend inputs as `EmptyInputs`, which is the honest verdict; the prover opens
 * a wrongful-rejection thread on it and binds it at step 01.
 */
export const stageZeroInputHonestRejection =
  async (): Promise<ZeroInputStagedThread> => {
    const harness = await makeFaultProofEmulatorHarness({
      contractOptions: {
        realZeroInput: true,
        alwaysFraudProofCatalogue: true,
        alwaysStateQueue: true,
      },
    });
    const { contracts, catalogue, category, stepReferences } =
      await zeroInputContracts(harness);
    const credential = getAddressDetails(
      await harness.funderLucid.wallet().address(),
    ).paymentCredential;
    if (credential?.type !== "Key") throw new Error("funder key absent");
    const baseFixture = await buildInvalidForcedTransitionTraceFixture({
      operatorVkey: credential.hash,
      now:
        alignUnixTimeToEmulatorSlotBoundary(
          harness.funderLucid,
          harness.emulator.now() + 120_000,
        ) - 1,
    });
    const forced = materializeMidgardForcedTxFromCanonical(
      makeNativeTx({ spendInputCbors: [], fee: 0n }),
    );
    const transactionId = computeMidgardNativeTxId(forced).toString("hex");
    const proofSource = deriveMidgardForcedTxProofSource(forced);
    const forcedTransaction = {
      tx_id: transactionId,
      submitted_source: {
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
      Data.to(forcedTransaction as never, ForcedInclusionTxV1Schema as never),
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
    const setup = await submitSetupTx({
      lucid: harness.funderLucid,
      contracts: harness.contracts,
      nonceUtxo: harness.nonceUtxo,
      catalogue,
      header: { ...baseFixture.header, forcedTransactionsRoot: root.root },
    });
    const inputField = encodeMidgardFieldPreimage([]);
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
    const initialized = await initializeThread({
      harness,
      contracts,
      catalogue,
      category,
      fraudulentBlockOutRef: setup.fraudulentBlockOutRef,
      fraudulentHeaderHash: setup.headerHash,
    });
    const bound = await submitZeroInputStep01Forced({
      lucid: harness.proverLucid,
      contracts,
      categoryId: category.categoryId,
      signer: harness.proverSigner,
      threadOutRef: `${initialized.txHash}#${initialized.firstStepOutputIndex.toString()}`,
      finding: evidence,
      forcedSource: {
        header: setup.header,
        membership: {
          domain: root.domain,
          root: root.root,
          phas_root: root.phasRoot,
          count: root.count,
          key: sourceKey,
          value: forcedTransaction,
          proof: Data.from(membershipProof.toCBOR().toString("hex"), Proof),
        },
      },
      referenceScriptUtxo: stepReferences[0],
    });
    return {
      harness,
      contracts,
      categoryId: category.categoryId,
      terminalThreadOutRef: bound.nextThreadOutRef,
      evidence,
      nativeTxCompactCbor: proofSource.compactCbor.toString("hex"),
      committedSpendInputs: [],
      stepReferences,
    };
  };
