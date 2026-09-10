import {
  createHash,
  createPrivateKey,
  createPublicKey,
  sign,
} from "node:crypto";
import { readFile, writeFile } from "node:fs/promises";

import { Store, Trie } from "@aiken-lang/merkle-patricia-forestry";
import {
  adjudicateMidgardNativeTxFullValidity,
  computeMidgardNativeTxId,
  deriveMidgardNativeTxProofSource,
  deriveMidgardNativeTxWitnessSetCompact,
  encodeCbor,
  encodeMidgardAddressWitnessItem,
  encodeMidgardNativeTxCanonical,
  encodeMidgardNativeTxCompact,
  encodeMidgardNativeTxWitnessSetCompact,
  encodeMidgardSpendInputItem,
  encodeMidgardTxOutput,
  type MidgardNativeTxFull,
} from "@al-ft/midgard-core";
import {
  acceptedVerdictSubject,
  AddressData,
  addressDataFromBech32,
  EMPTY_MERKLE_TREE_ROOT,
  type FieldOpening,
  forcedVerdictSubject,
  hashBlockHeader,
  missingSignatureFieldWalkCheckpoint,
  missingSignatureVkeyHash,
  Proof,
  requireInputIndex,
  requireOwnSpendPurpose,
  requireUniqueOutputIndex,
  type VerdictSubject,
} from "@al-ft/midgard-sdk";
import { buildCanonicalMidgardLedgerOutputMaterial } from "@al-ft/midgard-validation";
import {
  type BuildTxWithRedeemer,
  Data,
  type UTxO,
} from "@lucid-evolution/lucid";
import { createScalusEvaluator } from "@lucid-evolution/scalus-uplc";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import { submitCommittedFieldShapeInit } from "../src/committed-field-shape/submit-committed-field-shape-init.js";
import {
  certifyFaultProofFieldCarriage,
  faultProofFieldOpening,
  publishFaultProofFieldCarriage,
} from "../src/field-opening.js";
import {
  requireLinearFaultReferenceScript,
  requireLinearFaultThreadUtxo,
} from "../src/linear-fault-family.js";
import { submitLinearFaultFinalize } from "../src/linear-fault-finalize.js";
import { submitLinearFaultContinue } from "../src/linear-fault-submit.js";
import {
  buildVanRossemFitLedger,
  type VanRossemFitMeasurement,
} from "../src/proof-fit/van-rossem-fit-ledger.js";
import { submitRemoveFraudulentBlock } from "../src/remove-fraudulent-block.js";
import { planSpendInputSignerWitnessOpening } from "../src/spend-input-signer-missing/field-plans.js";
import {
  applySpendInputSignerMissingScripts,
  classifySpendInputSignerMissingFinding,
  prepareSpendInputSignerMissingEvidence,
  SPEND_INPUT_SIGNER_MISSING_BLUEPRINT_TITLES,
  SPEND_INPUT_SIGNER_MISSING_ID,
  type SpendInputSignerMissingContracts,
  type SpendInputSignerMissingEvidence,
  submitSpendInputSignerMissingCancel,
  submitSpendInputSignerMissingStep01Accepted,
  submitSpendInputSignerMissingStep01Forced,
  submitSpendInputSignerMissingStep02,
  submitSpendInputSignerMissingStep03,
  submitSpendInputSignerMissingStep04,
  submitSpendInputSignerMissingStep05,
} from "../src/spend-input-signer-missing/index.js";
import {
  SpendInputSignerStep03RedeemerSchema,
  SpendInputSignerStep04DatumSchema,
  SpendInputSignerStep05RedeemerSchema,
} from "../src/spend-input-signer-missing/schemas.js";
import {
  nativeTxFromCoreCompact,
  type SubmitStep01TxInclusion,
} from "../src/submit-step-01.js";
import { assertCompleteLifecycleCoverage } from "../src/testing/complete-lifecycle.js";
import { buildForcedTransactionLeafMembershipProof } from "../src/transition-trace/witnesses.js";
import { computationThreadOutputPredicate } from "../src/tx-layout.js";
import { realBlueprintPath } from "./support/emulator/blueprints.js";
import { makeFaultProofEmulatorHarness } from "./support/emulator/harness.js";
import { captureEmulatorSubmission } from "./support/emulator/measurement.js";
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
import { buildDecodingBlockFixture } from "./support/native-script-decoding-emulator.js";
import {
  countedTransactionsRoot,
  EMULATOR_HEADER_CLOCK_HEADROOM_MS,
  emulatorSuccessorHeaderStart,
  setupFraudulentBlock,
  submitSuccessorBlockTx,
} from "./support/submit-init-emulator-fixtures.js";
import {
  makeHeader,
  publishRemovalReferenceScripts,
  transitionTraceOutRef,
} from "./support/submit-init-emulator-shared.js";

const network = "Custom" as const;
const FAMILY = "spend-input-signer-missing" as const;
const REASON = "SpendInputSignerMissing" as const;
const MAXIMUM_WITNESSES = 318;
const MAXIMUM_SHAPE =
  "318 address witnesses; 32,757-byte Certified field; 16-witness scan batches";
/**
 * The smallest witness field the honest refusal run needs so that field 7
 * rides tier-3 certified carriage: 160 witnesses encode to 16,483 bytes, past
 * the raw-carriage bound, and the valid signature sits in the last batch so
 * the scan resumes nine times before it terminates.
 */
const HONEST_ACCEPTED_WITNESSES = 160;
const AUTHENTICATION_SEAMS = [
  "tx_membership",
  "prior_output_membership",
  "field_certificate",
  "forced_leaf",
  "credential",
] as const;
const PHYSICAL_STEPS = [
  "step-01",
  "step-02",
  "step-03",
  "step-04",
  "step-05",
] as const;

/**
 * Asserts that a submission was refused by a validator during local UPLC
 * evaluation, not by a builder precondition. Lucid's default evaluator reports
 * `failed script execution`; the scalus evaluator this suite runs under
 * reports the evaluation error together with the budget spent before the
 * abort. Anything else is a non-validator failure and fails the test.
 */
const expectRefusedOnChain = async (
  build: () => Promise<unknown>,
): Promise<string> => {
  let failure: unknown;
  try {
    await build();
  } catch (error) {
    failure = error;
  }
  if (failure === undefined)
    throw new Error(
      "expected the validator to refuse this transaction, but it succeeded",
    );
  const text = failure instanceof Error ? failure.message : String(failure);
  if (!/failed script execution|Error evaluated at/u.test(text))
    throw new Error(
      `expected an on-chain validator refusal, got a non-validator failure: ${text}`,
    );
  return text;
};

const coverage = createLifecycleCoverageRecorder();
/** Every submitted transaction of the maximum and adjacent runs, by name. */
const measurements: VanRossemFitMeasurement[] = [];

type Harness = Awaited<ReturnType<typeof makeFaultProofEmulatorHarness>>;
type Captured = Awaited<ReturnType<typeof captureEmulatorSubmission>>;
type Family = Awaited<ReturnType<typeof registeredContracts>>;

const recordMeasurements = (
  name: string,
  kind: VanRossemFitMeasurement["kind"],
  maximumShape: string,
  captured: Captured,
): void => {
  captured.measurements.forEach((measurement, index) => {
    const measurementName =
      captured.measurements.length === 1
        ? name
        : `${name}-${index.toString().padStart(2, "0")}`;
    expect(measurement.l1ByteMargin, measurementName).toBeGreaterThan(0);
    measurements.push({
      name: measurementName,
      kind,
      maximumShape,
      signedBytes: measurement.completeSignedBytes,
      memoryUnits: measurement.executionMemory,
      cpuUnits: measurement.executionSteps,
    });
  });
};

const ed25519Keypair = (seedByte: number) => {
  const privateKey = createPrivateKey({
    key: Buffer.concat([
      Buffer.from("302e020100300506032b657004220420", "hex"),
      Buffer.alloc(32, seedByte),
    ]),
    format: "der",
    type: "pkcs8",
  });
  const verificationKey = createPublicKey(privateKey)
    .export({ format: "der", type: "spki" })
    .subarray(-32);
  return {
    privateKey,
    verificationKey,
    keyHash: missingSignatureVkeyHash(verificationKey.toString("hex")),
  };
};

/** A witness whose signature cannot verify over any transaction id. */
const garbageWitness = (index: number): Buffer => {
  const verificationKey = Buffer.alloc(32);
  verificationKey.writeUInt32BE(index + 1, 28);
  return encodeMidgardAddressWitnessItem({
    verificationKey,
    signature: Buffer.alloc(64, 0xff),
  });
};

const priorLedgerFor = async (
  paymentCredentialHex: string,
  priorTxId: string,
  /** Address header: `0x60` pub-key enterprise, `0x70` script enterprise. */
  addressHeader = 0x60,
) => {
  const priorOutput = encodeMidgardTxOutput({
    address: Buffer.concat([
      Buffer.from([addressHeader]),
      Buffer.from(paymentCredentialHex, "hex"),
    ]),
    value: { lovelace: 2_000_000n, assets: new Map() },
  });
  const outRefBytes = encodeMidgardSpendInputItem({
    txId: Buffer.from(priorTxId, "hex"),
    outputIndex: 0,
  });
  const outputMaterial = buildCanonicalMidgardLedgerOutputMaterial({
    outputIndex: 0,
    outputCbor: priorOutput,
  });
  const store = new Store(undefined);
  await store.ready();
  const trie = new Trie(store);
  await trie.insert(outRefBytes, outputMaterial.descriptorCbor);
  const proof = await trie.prove(outRefBytes);
  const priorRoot = Buffer.from(trie.hash).toString("hex");
  return {
    outRefBytes,
    priorRoot,
    resolved: {
      priorRoot,
      transactionId: priorTxId,
      outputIndex: 0,
      descriptorCborHex: outputMaterial.descriptorCbor.toString("hex"),
      outputCborHex: priorOutput.toString("hex"),
      membershipProofCborHex: proof.toCBOR().toString("hex"),
      membershipProof: Data.from(proof.toCBOR().toString("hex"), Proof),
    },
  };
};

/** Signs the body-only transaction id with `keypair` and rebuilds the
 * transaction with the given witness items in field 7. */
const signedNativeTx = ({
  outRefBytes,
  fee,
  witnesses,
}: {
  readonly outRefBytes: Buffer;
  readonly fee: bigint;
  readonly witnesses: (txId: Buffer) => readonly Buffer[];
}): MidgardNativeTxFull => {
  const unsigned = makeNativeTx({ spendInputCbors: [outRefBytes], fee });
  const txId = computeMidgardNativeTxId(unsigned);
  const nativeTx = makeNativeTx({
    spendInputCbors: [outRefBytes],
    fee,
    addrTxWitsPreimageCbor: encodeCbor([...witnesses(txId)]),
  });
  expect(computeMidgardNativeTxId(nativeTx)).toEqual(txId);
  return nativeTx;
};

const witnessSetCompactHex = (nativeTx: MidgardNativeTxFull): string =>
  encodeMidgardNativeTxWitnessSetCompact(
    deriveMidgardNativeTxWitnessSetCompact(nativeTx.witnessSet),
  ).toString("hex");

/**
 * The registered chain is the deployed identity: the harness folds its first
 * step into the catalogue root. The family-side application must reproduce it
 * step for step before the suite drives it.
 */
const registeredContracts = async (harness: Harness) => {
  const addressData = await Effect.runPromise(
    addressDataFromBech32(
      harness.contracts.fraudProof.spendingScriptAddress,
    ).pipe(Effect.map((address) => Data.from(Data.to(address, AddressData)))),
  );
  const registered =
    harness.contracts.fraudProofContracts.spendInputSignerMissing;
  const category = harness.catalogue.categories.spendInputSignerMissing;
  expectRegisteredChainParity({
    registered,
    applied: applySpendInputSignerMissingScripts({
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
    SPEND_INPUT_SIGNER_MISSING_BLUEPRINT_TITLES,
  );
  const contracts: SpendInputSignerMissingContracts = {
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
  return { steps, contracts, catalogue: harness.catalogue, category };
};

const newHarness = async () =>
  makeFaultProofEmulatorHarness({
    contractOptions: {
      realSpendInputSignerMissing: true,
      alwaysFraudProofCatalogue: true,
    },
    lucidOptions: { evaluator: createScalusEvaluator() },
  });

/** Publishes the five applied steps and the certificate mint as plain
 * reference scripts; measured only for the maximum run's ledger. */
const publishReferences = async (
  harness: Harness,
  family: Family,
  label: string,
  measure: boolean,
) => {
  const references: UTxO[] = [];
  for (const [index, step] of family.steps.entries()) {
    const captured = await captureEmulatorSubmission(harness.emulator, () =>
      publishPlainReferenceScriptUtxo({
        lucid: harness.funderLucid,
        script: step.spendingScript,
        label: `${label}-${index.toString()}`,
      }),
    );
    if (measure)
      recordMeasurements(
        `step0${(index + 1).toString()}-reference-publication`,
        "publication",
        "fully applied testnet validator",
        captured,
      );
    references.push(captured.result.utxo);
  }
  const certificateCaptured = await captureEmulatorSubmission(
    harness.emulator,
    () =>
      publishPlainReferenceScriptUtxo({
        lucid: harness.funderLucid,
        script: harness.contracts.fieldPreimageCertificate.mintingScript,
        label: `${label}-certificate`,
      }),
  );
  if (measure)
    recordMeasurements(
      "certificate-reference-publication",
      "publication",
      "field-preimage certificate mint",
      certificateCaptured,
    );
  return {
    references,
    certificateReference: certificateCaptured.result.utxo,
  };
};

/** Commits `nativeTx` as the single transaction of an accepted successor block
 * whose header names `priorRoot` as its previous UTxO root. */
const commitAcceptedBlock = async (
  harness: Harness,
  family: Family,
  nativeTx: MidgardNativeTxFull,
  priorRoot: string,
) => {
  const nativeTxId = computeMidgardNativeTxId(nativeTx).toString("hex");
  const compactCbor = encodeMidgardNativeTxCompact(nativeTx.compact).toString(
    "hex",
  );
  const sourceCbor = l2TransactionSourceCbor(nativeTx);
  const store = new Store(undefined);
  await store.ready();
  const trie = new Trie(store);
  await trie.insert(
    Buffer.from(nativeTxId, "hex"),
    Buffer.from(sourceCbor, "hex"),
  );
  const txProof = await trie.prove(Buffer.from(nativeTxId, "hex"));
  const transactionsRoot = Buffer.from(trie.hash).toString("hex");
  const txInclusion: SubmitStep01TxInclusion = {
    nativeTxId,
    nativeTx: nativeTxFromCoreCompact(nativeTx.compact),
    nativeTxCompactCbor: compactCbor,
    l2TransactionSourceCbor: sourceCbor,
    transactionsPhasRoot: transactionsRoot,
    txMembershipProof: Data.from(txProof.toCBOR().toString("hex"), Proof),
    txMembershipProofCbor: txProof.toCBOR().toString("hex"),
  };
  const predecessor = await setupFraudulentBlock({
    funderLucid: harness.funderLucid,
    emulator: harness.emulator,
    contracts: harness.contracts,
    catalogue: family.catalogue,
    fixture: {
      transactionsRoot,
      l2TransactionCount: 1n,
      utxosRoot: priorRoot,
      headerDurationMs: EMULATOR_HEADER_CLOCK_HEADROOM_MS,
    },
  });
  const header = {
    ...makeHeader(
      predecessor.header.operatorVkey,
      emulatorSuccessorHeaderStart({
        predecessorEndTime: predecessor.header.endTime,
        emulator: harness.emulator,
      }),
      await countedTransactionsRoot(transactionsRoot, 1n),
      1n,
    ),
    prevHeaderHash: predecessor.headerHash,
    prevUtxosRoot: priorRoot,
  };
  const target = await submitSuccessorBlockTx({
    lucid: harness.funderLucid,
    emulator: harness.emulator,
    contracts: harness.contracts,
    anchorBlockUnit: predecessor.stateQueueBlockUnit,
    header,
    hubOracle: predecessor.hubOracle,
    scheduler: predecessor.scheduler,
    activeOperatorNode: predecessor.activeOperatorNode,
    activeOperatorNodeUnit: predecessor.activeOperatorNodeUnit,
  });
  return {
    nativeTxId,
    compactCbor,
    witnessSetCompactCbor: witnessSetCompactHex(nativeTx),
    txInclusion,
    blockOutRef: target.successorOutRef,
    headerHash: target.successorHeaderHash,
  };
};

/** Commits `nativeTx` as a forced transaction the operator rejected with
 * `reason`, in a successor block whose header names `priorRoot`. */
const commitForcedBlock = async (
  harness: Harness,
  family: Family,
  nativeTx: MidgardNativeTxFull,
  priorRoot: string,
  reason: {
    readonly SpendInputSignerMissing: { readonly input_index: bigint };
  },
  sourceKeyByte: string,
) => {
  const nativeTxId = computeMidgardNativeTxId(nativeTx).toString("hex");
  const proofSource = deriveMidgardNativeTxProofSource(
    adjudicateMidgardNativeTxFullValidity(nativeTx, "TxIsInvalid"),
  );
  const sourceKey = transitionTraceOutRef(sourceKeyByte);
  const predecessor = await setupFraudulentBlock({
    funderLucid: harness.funderLucid,
    emulator: harness.emulator,
    contracts: harness.contracts,
    catalogue: family.catalogue,
    fixture: {
      transactionsRoot: EMPTY_MERKLE_TREE_ROOT,
      l2TransactionCount: 0n,
      utxosRoot: priorRoot,
      headerDurationMs: EMULATOR_HEADER_CLOCK_HEADROOM_MS,
    },
  });
  const forcedBlock = await buildDecodingBlockFixture({
    operatorVkey: predecessor.header.operatorVkey,
    startTime: BigInt(
      emulatorSuccessorHeaderStart({
        predecessorEndTime: predecessor.header.endTime,
        emulator: harness.emulator,
      }),
    ),
    priorLedgerRoot: priorRoot,
    subject: {
      kind: "forced",
      nativeTx,
      orderKey: sourceKey,
      verdict: { ForcedTxInvalid: { reason } },
    },
  });
  const membership = await buildForcedTransactionLeafMembershipProof({
    reconstruction: forcedBlock.reconstruction,
    eventKey: { ForcedTransactionEventKey: { tx_order_id: sourceKey } },
  });
  const header = {
    ...forcedBlock.header,
    prevUtxosRoot: priorRoot,
    utxosRoot: priorRoot,
    prevHeaderHash: predecessor.headerHash,
  };
  const setup = await submitSuccessorBlockTx({
    lucid: harness.funderLucid,
    emulator: harness.emulator,
    contracts: harness.contracts,
    anchorBlockUnit: predecessor.stateQueueBlockUnit,
    header,
    hubOracle: predecessor.hubOracle,
    scheduler: predecessor.scheduler,
    activeOperatorNode: predecessor.activeOperatorNode,
    activeOperatorNodeUnit: predecessor.activeOperatorNodeUnit,
  });
  expect(setup.successorHeaderHash).toBe(
    await Effect.runPromise(hashBlockHeader(header)),
  );
  return {
    nativeTxId,
    sourceKey,
    subject: forcedVerdictSubject({
      transactionId: nativeTxId,
      sourceKey,
      rejectionReason: reason,
    }),
    compactCbor: proofSource.compactCbor.toString("hex"),
    witnessSetCompactCbor: proofSource.witnessSetCompactCbor.toString("hex"),
    forcedSource: { header, membership, direction: 1n },
    membership,
    blockOutRef: setup.successorOutRef,
    headerHash: setup.successorHeaderHash,
  };
};

/** Thin step drivers over one harness, family, and reference set. */
const familyDriver = (
  harness: Harness,
  family: Family,
  references: readonly UTxO[],
  certificateReference: UTxO,
) => {
  const { contracts, category } = family;
  const lucid = harness.proverLucid;
  const signer = harness.proverSigner;
  const categoryId = category.categoryId;
  const init = (blockOutRef: string) =>
    submitCommittedFieldShapeInit({
      lucid,
      blueprint: harness.realBlueprint,
      network,
      contracts: contracts as never,
      category,
      catalogue: {
        policyId: harness.contracts.fraudProofCatalogue.policyId,
        spendingScriptAddress:
          harness.contracts.fraudProofCatalogue.spendingScriptAddress,
        root: family.catalogue.root,
      },
      signer,
      fraudulentBlockOutRef: blockOutRef,
      witnessReferenceScripts: harness.witnessReferenceScripts,
    });
  const initThread = async (blockOutRef: string) => {
    const result = await init(blockOutRef);
    return {
      result,
      threadOutRef: `${result.txHash}#${result.firstStepOutputIndex.toString()}`,
    };
  };
  const threadUtxoAt = async (threadOutRef: string) => {
    const [txHash, outputIndex] = threadOutRef.split("#");
    const [utxo] = await lucid.utxosByOutRef([
      { txHash: txHash!, outputIndex: Number(outputIndex) },
    ]);
    if (utxo === undefined) throw new Error(`thread ${threadOutRef} absent`);
    return utxo;
  };
  const step01Accepted = async (
    thread: Awaited<ReturnType<typeof initThread>>,
    evidence: SpendInputSignerMissingEvidence,
    blockOutRef: string,
    txInclusion: SubmitStep01TxInclusion,
  ) =>
    submitSpendInputSignerMissingStep01Accepted({
      lucid,
      blueprint: harness.realBlueprint,
      network,
      contracts,
      signer,
      evidence,
      threadUtxo: await threadUtxoAt(thread.threadOutRef),
      threadToken: {
        unit: thread.result.computationThreadUnit,
        fraudulentHeaderHash: thread.result.fraudulentHeaderHash,
      },
      stateQueueBlockOutRef: blockOutRef,
      txInclusion,
      referenceScriptUtxo: references[0]!,
      witnessReferenceScripts: harness.witnessReferenceScripts,
    });
  const step01Forced = (
    threadOutRef: string,
    evidence: SpendInputSignerMissingEvidence,
    forcedSource: Readonly<Record<string, unknown>>,
  ) =>
    submitSpendInputSignerMissingStep01Forced({
      lucid,
      contracts,
      categoryId,
      signer,
      threadOutRef,
      evidence,
      forcedSource,
      referenceScriptUtxo: references[0]!,
    });
  const step02 = (
    threadOutRef: string,
    evidence: SpendInputSignerMissingEvidence,
    nativeTxCompactCbor: string,
    witnessSetCompactCbor: string,
  ) =>
    submitSpendInputSignerMissingStep02({
      lucid,
      network,
      contracts,
      categoryId,
      signer,
      threadOutRef,
      evidence,
      nativeTxCompactCbor,
      witnessSetCompactCbor,
      referenceScriptUtxo: references[1]!,
      membershipReferenceScriptUtxo:
        harness.witnessReferenceScripts.phasMembershipWithdraw!,
    });
  const step03 = (
    threadOutRef: string,
    evidence: SpendInputSignerMissingEvidence,
    nativeTxCompactCbor: string,
    witnessSetCompactCbor: string,
  ) =>
    submitSpendInputSignerMissingStep03({
      lucid,
      network,
      contracts,
      categoryId,
      signer,
      threadOutRef,
      evidence,
      nativeTxCompactCbor,
      witnessSetCompactCbor,
      referenceScriptUtxo: references[2]!,
      certificateReferenceScriptUtxo: certificateReference,
    });
  const step04 = (
    threadOutRef: string,
    evidence: SpendInputSignerMissingEvidence,
    nativeTxCompactCbor: string,
    witnessSetCompactCbor: string,
  ) =>
    submitSpendInputSignerMissingStep04({
      lucid,
      network,
      contracts,
      categoryId,
      signer,
      threadOutRef,
      evidence,
      nativeTxCompactCbor,
      witnessSetCompactCbor,
      referenceScriptUtxo: references[3]!,
      certificateReferenceScriptUtxo: certificateReference,
    });
  const step05 = (
    threadOutRef: string,
    evidence: SpendInputSignerMissingEvidence,
  ) =>
    submitSpendInputSignerMissingStep05({
      lucid,
      contracts,
      categoryId,
      signer,
      threadOutRef,
      evidence,
      referenceScriptUtxo: references[4]!,
      witnessReferenceScripts: harness.witnessReferenceScripts,
    });
  const cancel = (threadOutRef: string, stepIndex: number) =>
    submitSpendInputSignerMissingCancel({
      lucid,
      contracts,
      categoryId,
      signer,
      threadOutRef,
      referenceScriptUtxo: references[stepIndex]!,
      witnessReferenceScripts: harness.witnessReferenceScripts,
    });
  /**
   * The generic finalizer over a step-05 thread, with no family evidence in
   * the way: this is what an honest terminal has to be refused by, on chain.
   */
  const finalizeDirect = async (threadOutRef: string) => {
    const { threadUtxo, threadToken } = await requireLinearFaultThreadUtxo({
      lucid,
      contracts,
      categoryId,
      family: FAMILY,
      stepIndex: 4,
      threadOutRef,
    });
    return submitLinearFaultFinalize({
      lucid,
      family: FAMILY,
      stepIndex: 4,
      step: contracts.steps[4],
      computationThread: contracts.computationThread,
      fraudProof: contracts.fraudProof,
      signer,
      threadUtxo,
      threadToken,
      spendRedeemerSchema: SpendInputSignerStep05RedeemerSchema,
      buildFamilyArgs: ({
        inputIndex,
        outputIndex,
        fraudProofMintRedeemerIndex,
      }) => ({
        input_index: inputIndex,
        output_index: outputIndex,
        fraud_proof_mint_redeemer_index: fraudProofMintRedeemerIndex,
      }),
      referenceScriptUtxo: references[4]!,
      witnessReferenceScripts: harness.witnessReferenceScripts,
      awaitConfirmation: true,
    });
  };
  /** Runs step 04 to its terminal, returning every capture. */
  const scanToTerminal = async (
    threadOutRef: string,
    evidence: SpendInputSignerMissingEvidence,
    nativeTxCompactCbor: string,
    witnessSetCompactCbor: string,
  ) => {
    const scans: Captured[] = [];
    let outRef = threadOutRef;
    for (;;) {
      const scan = await captureEmulatorSubmission(harness.emulator, () =>
        step04(outRef, evidence, nativeTxCompactCbor, witnessSetCompactCbor),
      );
      scans.push(scan);
      outRef = scan.result.nextThreadOutRef;
      if (scan.result.stage === "step05") break;
    }
    return { scans, threadOutRef: outRef };
  };
  const removal = async (headerHash: string) => {
    const removalReferences = await publishRemovalReferenceScripts({
      lucid,
      contracts: harness.contracts,
    });
    // A registered family resolves removal through the canonical catalogue:
    // the manifest's fraudProofSpendInputSignerMissing entries carry the
    // registered chain the harness built.
    const deploymentInfo = buildRemovalDeploymentInfo(
      harness.contracts,
      family.catalogue,
      { removalReferenceScripts: removalReferences.published },
    );
    const now = BigInt(harness.emulator.now());
    return captureEmulatorSubmission(harness.emulator, () =>
      submitRemoveFraudulentBlock({
        lucid,
        blueprint: harness.realBlueprint,
        deploymentInfo,
        network,
        signer,
        fraudCategory: "spendInputSignerMissing",
        fraudulentHeaderHash: headerHash,
        requireReferenceScripts: true,
        awaitConfirmation: true,
        validFrom: now > 120_000n ? now - 120_000n : 0n,
        validTo: now + 300_000n,
      }),
    );
  };
  return {
    initThread,
    step01Accepted,
    step01Forced,
    step02,
    step03,
    step04,
    step05,
    cancel,
    finalizeDirect,
    scanToTerminal,
    removal,
  };
};

describe("spendInputSignerMissing registered-chain lifecycle", () => {
  it("runs the accepted 318-witness maximum from Init through proof mint, cancelling every physical step", async () => {
    const harness = await newHarness();
    const family = await registeredContracts(harness);
    const paymentCredential = Buffer.alloc(28, 0x51).toString("hex");
    const prior = await priorLedgerFor(paymentCredential, "ab".repeat(32));
    const nativeTx = makeNativeTx({
      spendInputCbors: [prior.outRefBytes],
      fee: 7n,
      addrTxWitsPreimageCbor: encodeCbor(
        Array.from({ length: MAXIMUM_WITNESSES }, (_unused, index) =>
          garbageWitness(index),
        ),
      ),
    });
    const block = await commitAcceptedBlock(
      harness,
      family,
      nativeTx,
      prior.priorRoot,
    );
    const evidence = prepareSpendInputSignerMissingEvidence({
      subject: acceptedVerdictSubject(block.nativeTxId),
      inputIndex: 0,
      canonicalTransactionCbor: encodeMidgardNativeTxCanonical(nativeTx),
      resolved: prior.resolved,
    });
    expect(evidence.witnessCarriage).toBe("Certified");
    expect(evidence.checkpoints).toHaveLength(20);
    coverage.scenario("maximum_supported_evidence");

    const { references, certificateReference } = await publishReferences(
      harness,
      family,
      FAMILY,
      true,
    );
    const run = familyDriver(harness, family, references, certificateReference);
    const measured = async <T>(
      name: string,
      operation: () => Promise<T>,
    ): Promise<T> => {
      const captured = await captureEmulatorSubmission(
        harness.emulator,
        operation,
      );
      recordMeasurements(name, "lifecycle", MAXIMUM_SHAPE, captured);
      return captured.result;
    };

    const thread = await measured("accepted-init", () =>
      run.initThread(block.blockOutRef),
    );
    const step01 = await measured("accepted-step01", () =>
      run.step01Accepted(
        thread,
        evidence,
        block.blockOutRef,
        block.txInclusion,
      ),
    );
    const step02 = await measured("accepted-step02", () =>
      run.step02(
        step01.nextThreadOutRef,
        evidence,
        block.compactCbor,
        block.witnessSetCompactCbor,
      ),
    );
    const step03 = await measured("accepted-step03", () =>
      run.step03(
        step02.nextThreadOutRef,
        evidence,
        block.compactCbor,
        block.witnessSetCompactCbor,
      ),
    );
    const { scans, threadOutRef } = await run.scanToTerminal(
      step03.nextThreadOutRef,
      evidence,
      block.compactCbor,
      block.witnessSetCompactCbor,
    );
    expect(scans).toHaveLength(20);
    scans.forEach((scan, index) =>
      recordMeasurements(
        `accepted-step04-${index.toString().padStart(2, "0")}`,
        "lifecycle",
        MAXIMUM_SHAPE,
        scan,
      ),
    );
    // Nineteen resumptions from nothing but the checkpoint digest the previous
    // transaction committed and the redeemer's re-supplied checkpoint bytes.
    coverage.resumed();
    const step05 = await measured("accepted-step05-proof-mint", () =>
      run.step05(threadOutRef, evidence),
    );
    expect(step05.fraudProofUnit).toBeTruthy();
    coverage.reason(REASON, "accepted_invalid");
    coverage.scenario("wrongful_acceptance_success");

    // Cancel from every physical step, including the resumed scan position,
    // on fresh threads over the same block. The chunks the maximum run
    // published above are reused, so only the cancels themselves are measured.
    for (const cancelTarget of [
      "step01",
      "step02",
      "step03",
      "step04-initial",
      "step04-resumed",
      "step05",
    ] as const) {
      const thread = await run.initThread(block.blockOutRef);
      let outRef = thread.threadOutRef;
      if (cancelTarget !== "step01") {
        outRef = (
          await run.step01Accepted(
            thread,
            evidence,
            block.blockOutRef,
            block.txInclusion,
          )
        ).nextThreadOutRef;
      }
      if (cancelTarget !== "step01" && cancelTarget !== "step02") {
        outRef = (
          await run.step02(
            outRef,
            evidence,
            block.compactCbor,
            block.witnessSetCompactCbor,
          )
        ).nextThreadOutRef;
      }
      if (
        cancelTarget === "step04-initial" ||
        cancelTarget === "step04-resumed" ||
        cancelTarget === "step05"
      ) {
        outRef = (
          await run.step03(
            outRef,
            evidence,
            block.compactCbor,
            block.witnessSetCompactCbor,
          )
        ).nextThreadOutRef;
      }
      if (cancelTarget === "step04-resumed" || cancelTarget === "step05") {
        for (;;) {
          const scan = await run.step04(
            outRef,
            evidence,
            block.compactCbor,
            block.witnessSetCompactCbor,
          );
          outRef = scan.nextThreadOutRef;
          if (cancelTarget === "step04-resumed" || scan.stage === "step05")
            break;
        }
      }
      const referenceIndex =
        cancelTarget === "step01"
          ? 0
          : cancelTarget === "step02"
            ? 1
            : cancelTarget === "step03"
              ? 2
              : cancelTarget === "step05"
                ? 4
                : 3;
      await measured(`cancel-${cancelTarget}`, () =>
        run.cancel(outRef, referenceIndex),
      );
      coverage.cancelled(`step-0${(referenceIndex + 1).toString()}`);
    }
    const removal = await run.removal(block.headerHash);
    expect(removal.result.fraudCategoryId).toBe(SPEND_INPUT_SIGNER_MISSING_ID);
    recordMeasurements("accepted-remove", "lifecycle", MAXIMUM_SHAPE, removal);
    coverage.scenario("permanent_proof_token_and_descendant_removal");
  }, 900_000);

  it("runs a forced wrongful rejection with a valid matching signature through removal", async () => {
    const harness = await newHarness();
    const family = await registeredContracts(harness);
    const signerKey = ed25519Keypair(7);
    const prior = await priorLedgerFor(signerKey.keyHash, "cd".repeat(32));
    const nativeTx = signedNativeTx({
      outRefBytes: prior.outRefBytes,
      fee: 7n,
      witnesses: (txId) => [
        encodeMidgardAddressWitnessItem({
          verificationKey: signerKey.verificationKey,
          signature: sign(null, txId, signerKey.privateKey),
        }),
      ],
    });
    const reason = { SpendInputSignerMissing: { input_index: 0n } } as const;
    const block = await commitForcedBlock(
      harness,
      family,
      nativeTx,
      prior.priorRoot,
      reason,
      "f7",
    );
    const evidence = prepareSpendInputSignerMissingEvidence({
      subject: block.subject,
      inputIndex: 0,
      canonicalTransactionCbor: encodeMidgardNativeTxCanonical(nativeTx),
      resolved: prior.resolved,
    });
    expect(evidence.signerMissing).toBe(false);
    expect(evidence.validSignerHashes).toEqual([signerKey.keyHash]);
    const shape =
      "1 valid matching address witness; Inline field; one scan batch";
    const { references, certificateReference } = await publishReferences(
      harness,
      family,
      `${FAMILY}-forced`,
      false,
    );
    const run = familyDriver(harness, family, references, certificateReference);
    const measured = async <T>(
      name: string,
      operation: () => Promise<T>,
    ): Promise<T> => {
      const captured = await captureEmulatorSubmission(
        harness.emulator,
        operation,
      );
      recordMeasurements(name, "lifecycle", shape, captured);
      return captured.result;
    };
    const thread = await measured("forced-init", () =>
      run.initThread(block.blockOutRef),
    );
    const step01 = await measured("forced-step01", () =>
      run.step01Forced(thread.threadOutRef, evidence, block.forcedSource),
    );
    const step02 = await measured("forced-step02", () =>
      run.step02(
        step01.nextThreadOutRef,
        evidence,
        block.compactCbor,
        block.witnessSetCompactCbor,
      ),
    );
    const step03 = await measured("forced-step03", () =>
      run.step03(
        step02.nextThreadOutRef,
        evidence,
        block.compactCbor,
        block.witnessSetCompactCbor,
      ),
    );
    const step04 = await measured("forced-step04", () =>
      run.step04(
        step03.nextThreadOutRef,
        evidence,
        block.compactCbor,
        block.witnessSetCompactCbor,
      ),
    );
    expect(step04.stage).toBe("step05");
    const step05 = await measured("forced-step05-proof-mint", () =>
      run.step05(step04.nextThreadOutRef, evidence),
    );
    expect(step05.fraudProofUnit).toBeTruthy();
    coverage.reason(REASON, "forced_rejection_wrong");
    coverage.scenario("wrongful_forced_rejection_success");
    const removal = await run.removal(block.headerHash);
    expect(removal.result.fraudCategoryId).toBe(SPEND_INPUT_SIGNER_MISSING_ID);
    recordMeasurements("forced-remove", "lifecycle", shape, removal);
  }, 600_000);

  it("refuses an honest accepted block, every substituted accepted seam, and a mutated spend coordinate on chain", async () => {
    const harness = await newHarness();
    const family = await registeredContracts(harness);
    // The credential's key signs the transaction from the last position of a
    // certified 160-witness field: the block is honest, so no acceptance
    // thread may close, however far the scan has to walk to learn it.
    const signerKey = ed25519Keypair(9);
    const prior = await priorLedgerFor(signerKey.keyHash, "ef".repeat(32));
    const nativeTx = signedNativeTx({
      outRefBytes: prior.outRefBytes,
      fee: 9n,
      witnesses: (txId) => [
        ...Array.from({ length: HONEST_ACCEPTED_WITNESSES - 1 }, (_u, index) =>
          garbageWitness(index),
        ),
        encodeMidgardAddressWitnessItem({
          verificationKey: signerKey.verificationKey,
          signature: sign(null, txId, signerKey.privateKey),
        }),
      ],
    });
    const block = await commitAcceptedBlock(
      harness,
      family,
      nativeTx,
      prior.priorRoot,
    );
    const accepted = acceptedVerdictSubject(block.nativeTxId);
    // Off chain the package refuses to prepare evidence that does not
    // contradict the verdict; the honest run below carries the authenticated
    // material under the accepted subject anyway, so the refusal is the
    // chain's.
    expect(() =>
      prepareSpendInputSignerMissingEvidence({
        subject: accepted,
        inputIndex: 0,
        canonicalTransactionCbor: encodeMidgardNativeTxCanonical(nativeTx),
        resolved: prior.resolved,
      }),
    ).toThrow(/agrees with the operator verdict/u);
    const contradicting = prepareSpendInputSignerMissingEvidence({
      subject: forcedVerdictSubject({
        transactionId: block.nativeTxId,
        sourceKey: transitionTraceOutRef("f9"),
        rejectionReason: { SpendInputSignerMissing: { input_index: 0n } },
      }),
      inputIndex: 0,
      canonicalTransactionCbor: encodeMidgardNativeTxCanonical(nativeTx),
      resolved: prior.resolved,
    });
    expect(contradicting.signerMissing).toBe(false);
    expect(contradicting.witnessCarriage).toBe("Certified");
    expect(contradicting.checkpoints).toHaveLength(10);
    const honest: SpendInputSignerMissingEvidence = {
      ...contradicting,
      subject: accepted,
    };
    const { references, certificateReference } = await publishReferences(
      harness,
      family,
      `${FAMILY}-honest`,
      false,
    );
    const run = familyDriver(harness, family, references, certificateReference);

    const thread = await run.initThread(block.blockOutRef);
    // Transaction membership: a foreign transactions root cannot bind the
    // header's counted root, whatever proof rides with it.
    await expectRefusedOnChain(() =>
      run.step01Accepted(thread, honest, block.blockOutRef, {
        ...block.txInclusion,
        transactionsPhasRoot: "11".repeat(32),
      }),
    );
    coverage.seamMutated("tx_membership");
    const bound = await run.step01Accepted(
      thread,
      honest,
      block.blockOutRef,
      block.txInclusion,
    );
    // Prior-output membership: a descriptor for another output under the same
    // out-ref is not a member of the bound prior root.
    const foreign = await priorLedgerFor(
      Buffer.alloc(28, 0x77).toString("hex"),
      "ef".repeat(32),
    );
    await expectRefusedOnChain(() =>
      run.step02(
        bound.nextThreadOutRef,
        {
          ...honest,
          resolved: {
            ...honest.resolved!,
            descriptorCborHex: foreign.resolved.descriptorCborHex,
            outputCborHex: foreign.resolved.outputCborHex,
          },
        },
        block.compactCbor,
        block.witnessSetCompactCbor,
      ),
    );
    coverage.seamMutated("prior_output_membership");
    const authenticated = await run.step02(
      bound.nextThreadOutRef,
      honest,
      block.compactCbor,
      block.witnessSetCompactCbor,
    );
    // Field certificate: a certificate honestly minted over another
    // transaction's witness field, presented under this transaction's compact
    // structure and witness set, wears the wrong field hash and is refused at
    // the door.
    const foreignTx = makeNativeTx({
      spendInputCbors: [prior.outRefBytes],
      fee: 11n,
      addrTxWitsPreimageCbor: encodeCbor(
        Array.from({ length: HONEST_ACCEPTED_WITNESSES }, (_u, index) =>
          garbageWitness(index + 1_000),
        ),
      ),
    });
    const foreignEvidence = prepareSpendInputSignerMissingEvidence({
      subject: acceptedVerdictSubject(
        computeMidgardNativeTxId(foreignTx).toString("hex"),
      ),
      inputIndex: 0,
      canonicalTransactionCbor: encodeMidgardNativeTxCanonical(foreignTx),
      resolved: prior.resolved,
    });
    expect(foreignEvidence.witnessCarriage).toBe("Certified");
    await expectRefusedOnChain(() =>
      submitStep03WithForeignCertificate({
        harness,
        family,
        threadOutRef: authenticated.nextThreadOutRef,
        evidence: honest,
        nativeTxCompactCbor: block.compactCbor,
        witnessSetCompactCbor: block.witnessSetCompactCbor,
        foreign: {
          evidence: foreignEvidence,
          nativeTxCompactCbor: encodeMidgardNativeTxCompact(
            foreignTx.compact,
          ).toString("hex"),
          witnessSetCompactCbor: witnessSetCompactHex(foreignTx),
        },
        referenceScriptUtxo: references[2]!,
        certificateReference,
      }),
    );
    coverage.seamMutated("field_certificate");
    const scanning = await run.step03(
      authenticated.nextThreadOutRef,
      honest,
      block.compactCbor,
      block.witnessSetCompactCbor,
    );
    const { scans, threadOutRef } = await run.scanToTerminal(
      scanning.nextThreadOutRef,
      honest,
      block.compactCbor,
      block.witnessSetCompactCbor,
    );
    // The valid signature sits in the tenth batch: nine real resumptions
    // before the frontier admits it and the scan terminates.
    expect(scans).toHaveLength(10);
    // The family builder refuses to finalize a terminal that agrees with the
    // block, and the generic finalizer is refused by the validator itself.
    await expect(run.step05(threadOutRef, honest)).rejects.toThrow(
      /does not contradict verdict/u,
    );
    await expectRefusedOnChain(() => run.finalizeDirect(threadOutRef));
    coverage.scenario("honest_accepted_block_refusal");
    coverage.reason(REASON);

    // Spend coordinate: step 01 binds whatever coordinate the redeemer names;
    // step 02 selects it from the authenticated field and a one-input
    // transaction has no item 1.
    const mutated = await run.initThread(block.blockOutRef);
    const boundOutOfRange = await run.step01Accepted(
      mutated,
      { ...honest, inputIndex: 1 },
      block.blockOutRef,
      block.txInclusion,
    );
    await expectRefusedOnChain(() =>
      run.step02(
        boundOutOfRange.nextThreadOutRef,
        { ...honest, inputIndex: 1 },
        block.compactCbor,
        block.witnessSetCompactCbor,
      ),
    );
    coverage.scenario("reason_or_subject_coordinate_mutation");
  }, 900_000);

  it("refuses an honest forced rejection, a mutated forced reason coordinate, and a substituted forced leaf on chain", async () => {
    const harness = await newHarness();
    const family = await registeredContracts(harness);
    // Both Wave 3 mutations in one witness field: a valid signature from the
    // wrong key, and the right key with an invalid signature. Neither enters
    // the frontier, so the operator's rejection is exactly right.
    const signerKey = ed25519Keypair(11);
    const strangerKey = ed25519Keypair(12);
    const prior = await priorLedgerFor(signerKey.keyHash, "1a".repeat(32));
    const nativeTx = signedNativeTx({
      outRefBytes: prior.outRefBytes,
      fee: 13n,
      witnesses: (txId) => [
        encodeMidgardAddressWitnessItem({
          verificationKey: strangerKey.verificationKey,
          signature: sign(null, txId, strangerKey.privateKey),
        }),
        encodeMidgardAddressWitnessItem({
          verificationKey: signerKey.verificationKey,
          signature: Buffer.alloc(64, 0xff),
        }),
      ],
    });
    const reason = { SpendInputSignerMissing: { input_index: 0n } } as const;
    const block = await commitForcedBlock(
      harness,
      family,
      nativeTx,
      prior.priorRoot,
      reason,
      "fb",
    );
    expect(() =>
      prepareSpendInputSignerMissingEvidence({
        subject: block.subject,
        inputIndex: 0,
        canonicalTransactionCbor: encodeMidgardNativeTxCanonical(nativeTx),
        resolved: prior.resolved,
      }),
    ).toThrow(/agrees with the operator verdict/u);
    const contradicting = prepareSpendInputSignerMissingEvidence({
      subject: acceptedVerdictSubject(block.nativeTxId),
      inputIndex: 0,
      canonicalTransactionCbor: encodeMidgardNativeTxCanonical(nativeTx),
      resolved: prior.resolved,
    });
    expect(contradicting.signerMissing).toBe(true);
    // The stranger's signature verifies, so it is a signer — of the wrong
    // credential; the credential's own witness never verifies.
    expect(contradicting.validSignerHashes).toEqual([strangerKey.keyHash]);
    const honest: SpendInputSignerMissingEvidence = {
      ...contradicting,
      subject: block.subject,
    };
    // Classification refuses another family's typed reason outright.
    expect(() =>
      classifySpendInputSignerMissingFinding({
        subject: forcedVerdictSubject({
          transactionId: block.nativeTxId,
          sourceKey: block.sourceKey,
          rejectionReason: "ObserversForbiddenOnUntaggedNetwork",
        }),
        inputIndex: 0,
      }),
    ).toThrow(/wrong typed rejection reason/u);
    const { references, certificateReference } = await publishReferences(
      harness,
      family,
      `${FAMILY}-honest-forced`,
      false,
    );
    const run = familyDriver(harness, family, references, certificateReference);
    const thread = await run.initThread(block.blockOutRef);
    // Forced leaf: a leaf carrying another verdict is not in the forced root.
    await expectRefusedOnChain(() =>
      run.step01Forced(thread.threadOutRef, honest, {
        ...block.forcedSource,
        membership: {
          ...block.membership,
          value: { ...block.membership.value, verdict: "ForcedTxValid" },
        },
      }),
    );
    coverage.seamMutated("forced_leaf");
    // Reason coordinate: the leaf rejects input 0; a thread claiming the same
    // reason at input 1 is refused by the exact-reason bind.
    const shifted: VerdictSubject = forcedVerdictSubject({
      transactionId: block.nativeTxId,
      sourceKey: block.sourceKey,
      rejectionReason: { SpendInputSignerMissing: { input_index: 1n } },
    });
    await expectRefusedOnChain(() =>
      run.step01Forced(
        thread.threadOutRef,
        { ...honest, subject: shifted, inputIndex: 1 },
        block.forcedSource,
      ),
    );
    coverage.scenario("reason_or_subject_coordinate_mutation");
    const bound = await run.step01Forced(
      thread.threadOutRef,
      honest,
      block.forcedSource,
    );
    // Credential seam: a pub-key coordinate whose signer really is missing
    // cannot skip the witness scan through the direct exit; the validator
    // classifies the resolved credential itself.
    await expectRefusedOnChain(() =>
      run.step02(
        bound.nextThreadOutRef,
        {
          ...honest,
          route: "script_credential",
          signerRequired: false,
          signerMissing: false,
        },
        block.compactCbor,
        block.witnessSetCompactCbor,
      ),
    );
    coverage.seamMutated("credential");
    const authenticated = await run.step02(
      bound.nextThreadOutRef,
      honest,
      block.compactCbor,
      block.witnessSetCompactCbor,
    );
    const scanning = await run.step03(
      authenticated.nextThreadOutRef,
      honest,
      block.compactCbor,
      block.witnessSetCompactCbor,
    );
    const scan = await run.step04(
      scanning.nextThreadOutRef,
      honest,
      block.compactCbor,
      block.witnessSetCompactCbor,
    );
    expect(scan.stage).toBe("step05");
    await expect(run.step05(scan.nextThreadOutRef, honest)).rejects.toThrow(
      /does not contradict verdict/u,
    );
    await expectRefusedOnChain(() => run.finalizeDirect(scan.nextThreadOutRef));
    coverage.scenario("honest_forced_rejection_refusal");
  }, 600_000);

  it("proves a forced rejection over a script-locked spend input through the direct terminal route and refuses the scan door for it", async () => {
    const harness = await newHarness();
    const family = await registeredContracts(harness);
    // The operator rejected a transaction whose spend input is script-locked:
    // canonical validation authorizes a script credential with no signer, so
    // the rejection is wrong without any witness and step 02 closes at the
    // terminal directly.
    const scriptHash = Buffer.alloc(28, 0x5c).toString("hex");
    const prior = await priorLedgerFor(scriptHash, "e1".repeat(32), 0x70);
    const nativeTx = signedNativeTx({
      outRefBytes: prior.outRefBytes,
      fee: 17n,
      witnesses: () => [],
    });
    const reason = { SpendInputSignerMissing: { input_index: 0n } } as const;
    const block = await commitForcedBlock(
      harness,
      family,
      nativeTx,
      prior.priorRoot,
      reason,
      "f9",
    );
    const evidence = prepareSpendInputSignerMissingEvidence({
      subject: block.subject,
      inputIndex: 0,
      canonicalTransactionCbor: encodeMidgardNativeTxCanonical(nativeTx),
      resolved: prior.resolved,
    });
    expect(evidence.route).toBe("script_credential");
    expect(evidence.signerRequired).toBe(false);
    const shape = "script-locked spend input; direct terminal route; no scan";
    const { references, certificateReference } = await publishReferences(
      harness,
      family,
      `${FAMILY}-forced-direct`,
      false,
    );
    const run = familyDriver(harness, family, references, certificateReference);
    const measured = async <T>(
      name: string,
      operation: () => Promise<T>,
    ): Promise<T> => {
      const captured = await captureEmulatorSubmission(
        harness.emulator,
        operation,
      );
      recordMeasurements(name, "lifecycle", shape, captured);
      return captured.result;
    };
    const thread = await measured("forced-direct-init", () =>
      run.initThread(block.blockOutRef),
    );
    const step01 = await measured("forced-direct-step01", () =>
      run.step01Forced(thread.threadOutRef, evidence, block.forcedSource),
    );
    // Credential seam: the same forced claim on this coordinate cannot take
    // the witness-scan door; the validator classifies the credential itself.
    await expectRefusedOnChain(() =>
      run.step02(
        step01.nextThreadOutRef,
        {
          ...evidence,
          route: "witness_scan",
          signerRequired: true,
          signerMissing: false,
          paymentCredentialHex: scriptHash,
        },
        block.compactCbor,
        block.witnessSetCompactCbor,
      ),
    );
    const step02 = await measured("forced-direct-step02-terminal-verdict", () =>
      run.step02(
        step01.nextThreadOutRef,
        evidence,
        block.compactCbor,
        block.witnessSetCompactCbor,
      ),
    );
    expect(step02.stage).toBe("step05");
    expect(step02.route).toBe("script_credential");
    const step05 = await measured("forced-direct-step05-proof-mint", () =>
      run.step05(step02.nextThreadOutRef, evidence),
    );
    expect(step05.fraudProofUnit).toBeTruthy();
    coverage.reason(REASON, "forced_rejection_wrong");
    coverage.scenario("wrongful_forced_rejection_success");
    const removal = await run.removal(block.headerHash);
    expect(removal.result.fraudCategoryId).toBe(SPEND_INPUT_SIGNER_MISSING_ID);
    recordMeasurements("forced-direct-remove", "lifecycle", shape, removal);
  }, 600_000);

  it("declares the complete lifecycle coverage it exercised and writes the fit ledger it measured", async () => {
    // Recorded while the suites above ran, never pre-filled. The aggregate
    // field bound (32,768 bytes, 318 witnesses) is this family's consensus
    // bound, but no lifecycle transaction can present the adjacent 319-witness
    // field: the L2 codec refuses to lay it out and the door refuses its
    // certified view (`step_04_refuses_an_adjacent_over_bound_witness_field`),
    // so the suite claims no adjacent bound of its own.
    assertCompleteLifecycleCoverage({
      coverage: coverage.snapshot(),
      expectedReasonArms: [REASON],
      authenticationSeams: [...AUTHENTICATION_SEAMS],
      cancellablePhysicalSteps: [...PHYSICAL_STEPS],
      resumable: true,
      hasAdjacentConsensusBound: false,
    });
    const blueprintBytes = await readFile(realBlueprintPath);
    const blueprint = JSON.parse(blueprintBytes.toString("utf8")) as {
      readonly preamble?: { readonly compiler?: { readonly version?: string } };
    };
    const ledger = buildVanRossemFitLedger({
      category: "spendInputSignerMissing:00000027:testnet",
      blueprintSha256: createHash("sha256")
        .update(blueprintBytes)
        .digest("hex"),
      compilerVersion:
        blueprint.preamble?.compiler?.version ?? "unknown-aiken-compiler",
      measurements,
    });
    expect(ledger.entries).toHaveLength(measurements.length);
    expect(
      ledger.entries.every(
        (entry) =>
          entry.signedByteMargin > 0 &&
          BigInt(entry.memoryUnitMargin) > 0n &&
          BigInt(entry.cpuUnitMargin) > 0n,
      ),
    ).toBe(true);
    expect(
      ledger.entries
        .filter((entry) => entry.kind === "publication")
        .every((entry) => (entry.publicationReserveMargin ?? -1) >= 0),
    ).toBe(true);
    console.info(
      `[spend-input-signer-missing-fit-ledger] ${JSON.stringify(ledger)}`,
    );
    if (process.env.MIDGARD_WRITE_FIT_LEDGER === "1")
      await writeFile(
        new URL(
          "../../../docs/fault-proofs/size-plans/spend-input-signer-missing-v1-fit-ledger.json",
          import.meta.url,
        ),
        `${JSON.stringify(ledger, null, 2)}\n`,
        "utf8",
      );
  });
});

/**
 * Step 03 over `threadOutRef` with a certificate and chunks honestly minted
 * for `foreign`'s witness field, carried under the thread's own compact
 * structure and witness set. Everything the family builder does is done here
 * with the same primitives; only the carriage is another transaction's.
 */
const submitStep03WithForeignCertificate = async ({
  harness,
  family,
  threadOutRef,
  evidence,
  nativeTxCompactCbor,
  witnessSetCompactCbor,
  foreign,
  referenceScriptUtxo,
  certificateReference,
}: {
  readonly harness: Harness;
  readonly family: Family;
  readonly threadOutRef: string;
  readonly evidence: SpendInputSignerMissingEvidence;
  readonly nativeTxCompactCbor: string;
  readonly witnessSetCompactCbor: string;
  readonly foreign: {
    readonly evidence: SpendInputSignerMissingEvidence;
    readonly nativeTxCompactCbor: string;
    readonly witnessSetCompactCbor: string;
  };
  readonly referenceScriptUtxo: UTxO;
  readonly certificateReference: UTxO;
}) => {
  const lucid = harness.proverLucid;
  const signer = harness.proverSigner;
  const { contracts, category } = family;
  const { threadUtxo, threadToken } = await requireLinearFaultThreadUtxo({
    lucid,
    contracts,
    categoryId: category.categoryId,
    family: FAMILY,
    stepIndex: 2,
    threadOutRef,
  });
  const own = planSpendInputSignerWitnessOpening({
    evidence,
    nativeTxCompactCbor,
    witnessSetCompactCbor,
    owner: signer.paymentKeyHash,
  });
  const planned = planSpendInputSignerWitnessOpening({
    evidence: foreign.evidence,
    nativeTxCompactCbor: foreign.nativeTxCompactCbor,
    witnessSetCompactCbor: foreign.witnessSetCompactCbor,
    owner: signer.paymentKeyHash,
  });
  expect(planned.plan.tier).toBe("Certified");
  signer.selectWallet(lucid);
  const carriageUtxos = await publishFaultProofFieldCarriage({
    lucid,
    signer,
    planned,
    publisherAddress: signer.address,
    label: "foreign address witnesses",
  });
  const { certificateUtxo } = await certifyFaultProofFieldCarriage({
    lucid,
    network,
    signer,
    planned,
    certificatePolicyId: contracts.fieldPreimageCertificatePolicyId,
    certificateMintingScript: contracts.fieldPreimageCertificateMintingScript,
    certificateReferenceScriptUtxo: certificateReference,
    chunkUtxos: carriageUtxos,
    compactCbor: foreign.nativeTxCompactCbor,
    witnessSetCompactCbor: foreign.witnessSetCompactCbor,
  });
  const stepReference = requireLinearFaultReferenceScript({
    utxo: referenceScriptUtxo,
    expectedScriptHash: contracts.steps[2].spendingScriptHash,
    family: FAMILY,
    stepIndex: 2,
  });
  const referenceInputs = [...carriageUtxos, certificateUtxo, stepReference];
  const foreignOpening = faultProofFieldOpening({
    planned,
    referenceInputs,
    certificatePolicyId: contracts.fieldPreimageCertificatePolicyId,
    label: "foreign address witnesses",
  });
  if (
    !("WitnessFieldOpening" in foreignOpening) ||
    own.witnessSet === undefined
  )
    throw new Error("witness field openings expected");
  const opening: FieldOpening = {
    WitnessFieldOpening: {
      ...foreignOpening.WitnessFieldOpening,
      native_tx_compact_cbor: own.nativeTxCompactCbor,
      witness_set: own.witnessSet,
    },
  };
  const initial = missingSignatureFieldWalkCheckpoint({
    txId: evidence.subject.transaction_id,
    itemCount: own.itemCount,
    totalLength: own.preimage.length,
    nextItemIndex: 0,
  });
  const nextDatum = Data.to(
    {
      fraud_prover: signer.paymentKeyHash,
      data: {
        authenticated: {
          subject: evidence.subject,
          transaction_id: evidence.subject.transaction_id,
          witness_set_hash: evidence.witnessSetHashHex,
          payment_credential: evidence.paymentCredentialHex,
        },
        checkpoint_hash: initial.checkpointHash,
      },
    } as never,
    SpendInputSignerStep04DatumSchema as never,
  );
  const outputMatches = computationThreadOutputPredicate({
    address: contracts.steps[3].spendingScriptAddress,
    datum: nextDatum,
    unit: threadToken.unit,
  });
  const redeemer = ((ctx) => {
    requireOwnSpendPurpose(ctx, threadUtxo, "foreign-certificate step-03");
    return Data.to(
      {
        Continue: [
          {
            input_index: requireInputIndex(
              ctx,
              threadUtxo,
              "foreign-certificate step-03",
            ),
            output_index: requireUniqueOutputIndex(
              ctx.outputs,
              outputMatches,
              "foreign-certificate step-03 output",
            ),
            witnesses_opening: opening,
          },
        ],
      } as never,
      SpendInputSignerStep03RedeemerSchema as never,
    );
  }) satisfies BuildTxWithRedeemer;
  return submitLinearFaultContinue({
    lucid,
    signerPaymentKeyHash: signer.paymentKeyHash,
    threadUtxo,
    threadUnit: threadToken.unit,
    stepReference,
    stepScript: contracts.steps[2].spendingScript,
    stepRole: "foreign-certificate step-03",
    nextAddress: contracts.steps[3].spendingScriptAddress,
    nextDatum,
    redeemer,
    carriageUtxos,
    extraReferenceInputs: [certificateUtxo],
    awaitConfirmation: true,
  });
};
