import { Store, Trie } from "@aiken-lang/merkle-patricia-forestry";
import {
  computeMidgardNativeTxId,
  encodeMidgardNativeTxCompact,
  type MidgardNativeTxFull,
} from "@al-ft/midgard-core";
import { deriveMidgardForcedTxProofSource } from "@al-ft/midgard-core/codec/forced";
import { materializeMidgardForcedTxFromCanonical } from "@al-ft/midgard-core/codec/forced";
import {
  EMPTY_MERKLE_TREE_ROOT,
  forcedVerdictSubject,
  hashBlockHeader,
  Proof,
} from "@al-ft/midgard-sdk";
import { Data, type UTxO } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { expect } from "vitest";

import {
  nativeTxFromCoreCompact,
  type SubmitStep01TxInclusion,
} from "../src/step-support.js";
import { buildForcedTransactionLeafMembershipProof } from "../src/transition-trace/witnesses.js";
import {
  type Family,
  type Harness,
  recordMeasurements,
  witnessSetCompactHex,
} from "./spend-input-signer-missing-lifecycle.registered-contracts.js";
import { captureEmulatorSubmission } from "./support/emulator/measurement.js";
import { l2TransactionSourceCbor } from "./support/emulator/native-tx.js";
import { publishPlainReferenceScriptUtxo } from "./support/emulator/reference-scripts.js";
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
  transitionTraceOutRef,
} from "./support/submit-init-emulator-shared.js";

/** Publishes the five applied steps and the certificate mint as plain
 * reference scripts; measured only for the maximum run's ledger. */
export const publishReferences = async (
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
export const commitAcceptedBlock = async (
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
export const commitForcedBlock = async (
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
  const proofSource = deriveMidgardForcedTxProofSource(
    materializeMidgardForcedTxFromCanonical(nativeTx),
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
