import {
  computeMidgardNativeTxId,
  screenMidgardForcedTxAdmission,
} from "@al-ft/midgard-core";
import {
  encodeMidgardForcedTxCanonical,
  materializeMidgardForcedTxFromCanonical,
} from "@al-ft/midgard-core/codec/forced";
import {
  acceptedVerdictSubject,
  forcedVerdictSubject,
} from "@al-ft/midgard-sdk";
import { getAddressDetails } from "@lucid-evolution/lucid";
import { expect } from "vitest";

import { submitRemoveFraudulentBlock } from "../src/remove-fraudulent-block.js";
import {
  submitTransactionOutputNonCanonicalStep01Accepted,
  submitTransactionOutputNonCanonicalStep01Forced,
  submitTransactionOutputNonCanonicalStep02,
  submitTransactionOutputNonCanonicalStep03,
  submitTransactionOutputNonCanonicalStep04,
} from "../src/transaction-output-non-canonical/index.js";
import { buildForcedTransactionLeafMembershipProof } from "../src/transition-trace/witnesses.js";
import { expectOnchainRefusal } from "./support/emulator/expect-onchain-refusal.js";
import { makeFaultProofEmulatorHarness } from "./support/emulator/harness.js";
import {
  captureEmulatorSubmission,
  type CompleteSignedTransactionMeasurement,
} from "./support/emulator/measurement.js";
import { makeNativeTx } from "./support/emulator/native-tx.js";
import { nodeForcedVerdict } from "./support/emulator/node-forced-verdict.js";
import { buildRemovalDeploymentInfo } from "./support/emulator/removal-deployment.js";
import { setupFraudulentBlock } from "./support/submit-init-emulator-fixtures.js";
import {
  alignUnixTimeToEmulatorSlotBoundary,
  publishRemovalReferenceScripts,
  submitSetupTx,
} from "./support/submit-init-emulator-shared.js";
import {
  buildForcedOutputFixture,
  submitOutputStep04Raw,
} from "./support/transaction-output-non-canonical-emulator.js";
import { nodeTransactionSpendingHeldInput } from "./transaction-output-non-canonical-datum-lifecycle.scenario.js";
import {
  committedInclusions,
  type Evidence,
  evidenceOf,
  network,
  type Registered,
  registeredContracts,
  witnessSetCborOf,
} from "./transaction-output-non-canonical-lifecycle.registered-contracts.js";

const harnessOptions = {
  contractOptions: {
    realTransactionOutputNonCanonical: true,
    alwaysFraudProofCatalogue: true,
  },
} as const;

/**
 * Every submitted transaction of one run: how many there were, how many were
 * step-03 scans, and the largest memory, CPU and signed size any of them used.
 */
export const measurementLog = () => {
  const log = {
    transactions: 0,
    scans: 0,
    peakMemory: 0n,
    peakSteps: 0n,
    peakBytes: 0,
  };
  const measured = async <T>(
    registered: Registered,
    operation: () => Promise<T>,
    scan = false,
  ): Promise<T> => {
    const captured = await captureEmulatorSubmission(
      registered.harness.emulator,
      operation,
    );
    for (const measurement of captured.measurements) note(measurement, scan);
    return captured.result;
  };
  const note = (
    measurement: CompleteSignedTransactionMeasurement,
    scan: boolean,
  ) => {
    expect(measurement.l1ByteMargin).toBeGreaterThan(0);
    log.transactions += 1;
    if (scan) log.scans += 1;
    if (measurement.executionMemory > log.peakMemory)
      log.peakMemory = measurement.executionMemory;
    if (measurement.executionSteps > log.peakSteps)
      log.peakSteps = measurement.executionSteps;
    log.peakBytes = Math.max(log.peakBytes, measurement.completeSignedBytes);
  };
  return { log, measured };
};

type Measured = ReturnType<typeof measurementLog>["measured"];

/** Step 04 against a terminal the subject does not contradict: refused. */
const expectStep04Refused = async (
  registered: Registered,
  threadOutRef: string,
) =>
  await expectOnchainRefusal(
    async () =>
      await submitOutputStep04Raw({
        ...registered.common(threadOutRef, 4),
        witnessReferenceScripts: registered.harness.witnessReferenceScripts,
      }),
    {
      refusedBy: "fraud_proofs/transaction_output_non_canonical/step_04",
      check: /^Validator returned false$/u,
    },
  );

/**
 * Publishes the removal reference scripts now, inside the harness's
 * reference-script auth window, which a long scan outlasts; the returned
 * removal uses them once the block is convicted, and checks it is gone.
 */
const prepareRemoval = async (registered: Registered, measured: Measured) => {
  const { harness } = registered;
  const removalReferences = await measured(registered, () =>
    publishRemovalReferenceScripts({
      lucid: harness.proverLucid,
      contracts: harness.contracts,
    }),
  );
  return async (fraudulentBlockOutRef: string, headerHash: string) => {
    const now = BigInt(harness.emulator.now());
    const removed = await measured(registered, () =>
      submitRemoveFraudulentBlock({
        lucid: harness.proverLucid,
        blueprint: harness.realBlueprint,
        deploymentInfo: buildRemovalDeploymentInfo(
          harness.contracts,
          harness.catalogue,
          { removalReferenceScripts: removalReferences.published },
        ),
        network,
        signer: harness.proverSigner,
        fraudCategory: "transactionOutputNonCanonical",
        fraudulentHeaderHash: headerHash,
        awaitConfirmation: true,
        requireReferenceScripts: true,
        validFrom: now > 120_000n ? now - 120_000n : 0n,
        validTo: now + 300_000n,
      }),
    );
    expect(removed.fraudCategoryId).toBe(registered.category.categoryId);
    const [txHash, outputIndex] = fraudulentBlockOutRef.split("#");
    expect(
      await harness.proverLucid.utxosByOutRef([
        { txHash: txHash!, outputIndex: Number(outputIndex) },
      ]),
    ).toHaveLength(0);
  };
};

/** Step 02 through every step-03 scan to the terminal checkpoint. */
const openAndScan = async ({
  registered,
  threadOutRef,
  evidence,
  nativeTxCompactCbor,
  witnessSetCompactCbor,
  certificateReference,
  measured,
}: {
  readonly registered: Registered;
  readonly threadOutRef: string;
  readonly evidence: Evidence;
  readonly nativeTxCompactCbor: string;
  readonly witnessSetCompactCbor: string;
  readonly certificateReference: Awaited<
    ReturnType<Registered["publishReferences"]>
  >;
  readonly measured: Measured;
}) => {
  const { harness, contracts, category, references } = registered;
  const step = {
    lucid: harness.proverLucid,
    contracts,
    categoryId: category.categoryId,
    signer: harness.proverSigner,
  };
  const authenticated = await measured(registered, () =>
    submitTransactionOutputNonCanonicalStep02({
      ...step,
      threadOutRef,
      evidence,
      nativeTxCompactCbor,
      witnessSetCompactCbor,
      referenceScriptUtxo: references[1]!,
      certificateReferenceScriptUtxo: certificateReference,
    }),
  );
  let current = authenticated.nextThreadOutRef;
  let scans = 0;
  for (;;) {
    const scanned = await measured(
      registered,
      () =>
        submitTransactionOutputNonCanonicalStep03({
          ...step,
          threadOutRef: current,
          evidence,
          nativeTxCompactCbor,
          witnessSetCompactCbor,
          referenceScriptUtxo: references[2]!,
        }),
      true,
    );
    scans += 1;
    current = scanned.nextThreadOutRef;
    if (scanned.terminal) {
      // The datum step only follows a canonical scan that found a datum;
      // these outputs carry none.
      expect(scanned.nextStage).not.toBe("step03Datum");
      // One scan per control the trace records, the last control of a
      // canonical trace being the terminal itself.
      expect(scans).toBe(
        evidence.scanControls.length - (evidence.canonical ? 1 : 0),
      );
      return { step, threadOutRef: current };
    }
  }
};

/**
 * One block accepting a transaction per output. Each output whose value
 * repeats a key is convicted from Init; each distinct ascending output is
 * refused at step 04. The block is then removed.
 */
export const runAcceptedBlock = async (
  shapes: readonly {
    readonly label: string;
    readonly output: Buffer;
    readonly repeated: boolean;
  }[],
) => {
  const { log, measured } = measurementLog();
  const harness = await makeFaultProofEmulatorHarness(harnessOptions);
  const registered = await registeredContracts(harness);
  const txs = shapes.map(({ output }, index) =>
    makeNativeTx({
      spendInputCbors: [],
      fee: BigInt(index + 1),
      outputCbors: [output],
    }),
  );
  const committed = await committedInclusions(txs);
  const setup = await setupFraudulentBlock({
    funderLucid: harness.funderLucid,
    emulator: harness.emulator,
    contracts: harness.contracts,
    catalogue: registered.catalogue,
    fixture: {
      transactionsRoot: committed.transactionsRoot,
      l2TransactionCount: BigInt(txs.length),
    },
  });
  const certificateReference = await registered.publishReferences();
  const removeBlock = await prepareRemoval(registered, measured);
  let fraudulentHeaderHash = "";
  for (const [index, { label, output, repeated }] of shapes.entries()) {
    const inclusion = committed.inclusions[index]!;
    const evidence = evidenceOf(
      acceptedVerdictSubject(inclusion.nativeTxId),
      txs[index]!,
      0,
    );
    expect(evidence.itemLength, label).toBe(output.length);
    expect(evidence.canonical, label).toBe(!repeated);
    const init = await measured(registered, () =>
      registered.init(setup.fraudulentBlockOutRef),
    );
    fraudulentHeaderHash = init.result.fraudulentHeaderHash;
    const bound = await measured(registered, async () =>
      submitTransactionOutputNonCanonicalStep01Accepted({
        lucid: harness.proverLucid,
        blueprint: harness.realBlueprint,
        network,
        contracts: registered.contracts,
        signer: harness.proverSigner,
        finding: evidence,
        threadUtxo: await registered.threadUtxoOf(
          init.result.txHash,
          init.result.firstStepOutputIndex,
        ),
        threadToken: {
          unit: init.result.computationThreadUnit,
          fraudulentHeaderHash: init.result.fraudulentHeaderHash,
        },
        stateQueueBlockOutRef: setup.fraudulentBlockOutRef,
        txInclusion: inclusion,
        referenceScriptUtxo: registered.references[0]!,
        witnessReferenceScripts: harness.witnessReferenceScripts,
      }),
    );
    const terminal = await openAndScan({
      registered,
      threadOutRef: bound.nextThreadOutRef,
      evidence,
      nativeTxCompactCbor: inclusion.nativeTxCompactCbor,
      witnessSetCompactCbor: witnessSetCborOf(txs[index]!),
      certificateReference,
      measured,
    });
    if (repeated) {
      const minted = await measured(registered, () =>
        submitTransactionOutputNonCanonicalStep04({
          ...terminal.step,
          threadOutRef: terminal.threadOutRef,
          evidence,
          referenceScriptUtxo: registered.references[4]!,
          witnessReferenceScripts: harness.witnessReferenceScripts,
        }),
      );
      expect(minted.fraudProofUnit, label).toBeTruthy();
    } else {
      await expectStep04Refused(registered, terminal.threadOutRef);
    }
  }
  await removeBlock(setup.fraudulentBlockOutRef, fraudulentHeaderHash);
  return log;
};

/**
 * A forced transaction spending one held input into `output`: what the L1
 * admission screen and the node's classifier make of it.
 */
export const forcedOutputTransaction = (
  output: Buffer,
  heldLovelace: bigint,
) => {
  const { transaction, ledger } = nodeTransactionSpendingHeldInput(
    [output],
    heldLovelace,
  );
  const forced = materializeMidgardForcedTxFromCanonical(transaction);
  return {
    transaction,
    admission: screenMidgardForcedTxAdmission(transaction),
    verdict: async () =>
      await nodeForcedVerdict({
        transactionId: computeMidgardNativeTxId(forced),
        forcedCanonicalCbor: encodeMidgardForcedTxCanonical(forced),
        ledger,
      }),
  };
};

/**
 * A block whose forced leaf records `transaction` as rejected for
 * `OutputNonCanonical { 0 }` (`accepted` false) or as valid (`accepted`
 * true), driven from Init to step 04. The rejection is refused at step 04;
 * the acceptance is convicted and the block removed.
 */
export const runForcedLeaf = async ({
  transaction,
  accepted,
}: {
  readonly transaction: ReturnType<
    typeof forcedOutputTransaction
  >["transaction"];
  readonly accepted: boolean;
}) => {
  const { log, measured } = measurementLog();
  const harness = await makeFaultProofEmulatorHarness(harnessOptions);
  const registered = await registeredContracts(harness);
  const funderCredential = getAddressDetails(
    await harness.funderLucid.wallet().address(),
  ).paymentCredential;
  if (funderCredential?.type !== "Key")
    throw new Error("forced fixture funder key absent");
  const fixture = await buildForcedOutputFixture({
    operatorVkey: funderCredential.hash,
    now:
      alignUnixTimeToEmulatorSlotBoundary(
        harness.funderLucid,
        harness.emulator.now() + 120_000,
      ) - 1,
    transaction,
    accepted,
  });
  const setup = await submitSetupTx({
    lucid: harness.funderLucid,
    contracts: harness.contracts,
    nonceUtxo: harness.nonceUtxo,
    catalogue: registered.catalogue,
    header: fixture.header,
  });
  const certificateReference = await registered.publishReferences();
  const removeBlock = accepted
    ? await prepareRemoval(registered, measured)
    : undefined;
  const membership = await buildForcedTransactionLeafMembershipProof({
    reconstruction: fixture.reconstruction,
    eventKey: fixture.eventKey,
  });
  const evidence = evidenceOf(
    forcedVerdictSubject({
      transactionId: fixture.transaction.tx_id,
      sourceKey: membership.key,
      rejectionReason: fixture.rejectionReason,
    }),
    fixture.nativeTx,
    0,
  );
  expect(evidence.canonical).toBe(false);
  const init = await measured(registered, () =>
    registered.init(setup.fraudulentBlockOutRef),
  );
  const bound = await measured(registered, () =>
    submitTransactionOutputNonCanonicalStep01Forced({
      lucid: harness.proverLucid,
      contracts: registered.contracts,
      categoryId: registered.category.categoryId,
      signer: harness.proverSigner,
      threadOutRef: `${init.result.txHash}#${init.result.firstStepOutputIndex.toString()}`,
      finding: evidence,
      forcedSource: { header: fixture.header, membership },
      referenceScriptUtxo: registered.references[0]!,
    }),
  );
  const terminal = await openAndScan({
    registered,
    threadOutRef: bound.nextThreadOutRef,
    evidence,
    nativeTxCompactCbor: fixture.transaction.submitted_source.compact_cbor,
    witnessSetCompactCbor:
      fixture.transaction.submitted_source.witness_set_compact_cbor,
    certificateReference,
    measured,
  });
  if (removeBlock === undefined) {
    await expectStep04Refused(registered, terminal.threadOutRef);
    return log;
  }
  const minted = await measured(registered, () =>
    submitTransactionOutputNonCanonicalStep04({
      ...terminal.step,
      threadOutRef: terminal.threadOutRef,
      evidence,
      referenceScriptUtxo: registered.references[4]!,
      witnessReferenceScripts: harness.witnessReferenceScripts,
    }),
  );
  expect(minted.fraudProofUnit).toBeTruthy();
  await removeBlock(setup.fraudulentBlockOutRef, setup.headerHash);
  return log;
};
