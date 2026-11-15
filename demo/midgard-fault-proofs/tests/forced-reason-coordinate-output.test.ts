import {
  computeMidgardNativeTxId,
  decodeMidgardFieldPreimage,
  encodeCbor,
  encodeMidgardTxOutput,
  initialMidgardLedgerOutputScanControl,
} from "@al-ft/midgard-core";
import {
  encodeMidgardForcedTxCanonical,
  materializeMidgardForcedTxFromCanonical,
} from "@al-ft/midgard-core/codec/forced";
import { forcedVerdictSubject } from "@al-ft/midgard-sdk";
import { getAddressDetails } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import { FIELD_GRAMMAR_WALKED_COUNT_LIMIT } from "../src/field-grammar-certificate.js";
import {
  VAN_ROSSEM_MAX_CPU_UNITS,
  VAN_ROSSEM_MAX_MEMORY_UNITS,
} from "../src/proof-fit/van-rossem.js";
import {
  submitTransactionOutputNonCanonicalStep01Forced,
  submitTransactionOutputNonCanonicalStep02,
  submitTransactionOutputNonCanonicalStep03,
  submitTransactionOutputNonCanonicalStep04,
  TRANSACTION_OUTPUT_OUTCOME_ABSENT,
} from "../src/transaction-output-non-canonical/index.js";
import { buildForcedTransactionLeafMembershipProof } from "../src/transition-trace/witnesses.js";
import { expectOnchainRefusal } from "./support/emulator/expect-onchain-refusal.js";
import { makeFaultProofEmulatorHarness } from "./support/emulator/harness.js";
import { captureEmulatorSubmission } from "./support/emulator/measurement.js";
import { makeNativeTx } from "./support/emulator/native-tx.js";
import { nodeForcedVerdict } from "./support/emulator/node-forced-verdict.js";
import { deployHarnessFieldGrammarCertificate } from "./support/field-grammar-certificate-deployment-fixture.js";
import {
  alignUnixTimeToEmulatorSlotBoundary,
  submitSetupTx,
} from "./support/submit-init-emulator-shared.js";
import {
  buildForcedOutputFixture,
  MALFORMED_OUTPUT,
  outputFieldOpening,
  publishOutputFieldCarriage,
  scanStateOf,
  submitOutputStep02Raw,
  submitOutputStep04Raw,
} from "./support/transaction-output-non-canonical-emulator.js";
import { nodeTransactionSpendingHeldInput } from "./transaction-output-non-canonical-datum-lifecycle.scenario.js";
import {
  evidenceOf,
  registeredContracts,
} from "./transaction-output-non-canonical-lifecycle.registered-contracts.js";

/**
 * A forced OutputNonCanonical reason names an output by its field position,
 * and transactionOutputNonCanonical reopens exactly that output item. The
 * fixture puts a canonical output before the malformed one. The verdict is
 * the one the node's classifier writes, so the suite fails if the writer and
 * the proof disagree on how outputs are counted: one position early names the
 * canonical output and convicts; the written position is refused on chain.
 * One position late names an output field 2 does not have: step 02 closes it
 * by the field's count and convicts, at the two-output shape and at the
 * 761-output tier-3 maximum; a claim that a held output is absent is refused.
 */

const canonicalOutput = encodeMidgardTxOutput({
  address: Buffer.concat([Buffer.from([0x60]), Buffer.alloc(28, 0x47)]),
  value: { lovelace: 2_000_000n, assets: new Map() },
});

/**
 * A canonical output, then a malformed one, spent from a held input carrying
 * both outputs' 7_000_000 lovelace: the node refuses an output only once the
 * inputs resolve.
 */
const { transaction, ledger } = nodeTransactionSpendingHeldInput(
  [canonicalOutput, MALFORMED_OUTPUT],
  7_000_000n,
);

const writtenOutputIndex = async (): Promise<bigint> => {
  const forced = materializeMidgardForcedTxFromCanonical(transaction);
  const verdict = await nodeForcedVerdict({
    transactionId: computeMidgardNativeTxId(forced),
    forcedCanonicalCbor: encodeMidgardForcedTxCanonical(forced),
    ledger,
  });
  expect(verdict).toStrictEqual({
    ForcedTxInvalid: { reason: { OutputNonCanonical: { output_index: 1n } } },
  });
  return 1n;
};

/**
 * A block whose forced leaf rejects `forcedTransaction` with
 * `OutputNonCanonical { outputIndex }`, with a thread bound to that output.
 * `authenticate` runs step 02, through the field grammar certificate when one
 * is given.
 */
const setupForcedThread = async ({
  forcedTransaction,
  outputIndex,
  withFieldGrammar = false,
}: {
  readonly forcedTransaction: typeof transaction;
  readonly outputIndex: bigint;
  readonly withFieldGrammar?: boolean;
}) => {
  const harness = await makeFaultProofEmulatorHarness({
    contractOptions: {
      realTransactionOutputNonCanonical: true,
      alwaysFraudProofCatalogue: true,
    },
  });
  const registered = await registeredContracts(harness);
  const { contracts, category, references } = registered;
  const funderCredential = getAddressDetails(
    await harness.funderLucid.wallet().address(),
  ).paymentCredential;
  if (funderCredential?.type !== "Key")
    throw new Error("forced fixture funder key absent");
  const forced = await buildForcedOutputFixture({
    operatorVkey: funderCredential.hash,
    now:
      alignUnixTimeToEmulatorSlotBoundary(
        harness.funderLucid,
        harness.emulator.now() + 120_000,
      ) - 1,
    transaction: forcedTransaction,
    outputIndex,
  });
  const setup = await submitSetupTx({
    lucid: harness.funderLucid,
    contracts: harness.contracts,
    nonceUtxo: harness.nonceUtxo,
    catalogue: registered.catalogue,
    header: forced.header,
  });
  const certificateReference = await registered.publishReferences();
  const grammar = withFieldGrammar
    ? await deployHarnessFieldGrammarCertificate(
        harness,
        "transactionOutputNonCanonical",
      )
    : undefined;
  const membership = await buildForcedTransactionLeafMembershipProof({
    reconstruction: forced.reconstruction,
    eventKey: forced.eventKey,
  });
  const evidence = evidenceOf(
    forcedVerdictSubject({
      transactionId: forced.transaction.tx_id,
      sourceKey: membership.key,
      rejectionReason: forced.rejectionReason,
    }),
    forced.nativeTx,
    Number(outputIndex),
  );
  const source = {
    nativeTxCompactCbor: forced.transaction.submitted_source.compact_cbor,
    witnessSetCompactCbor:
      forced.transaction.submitted_source.witness_set_compact_cbor,
  };
  const step = {
    lucid: harness.proverLucid,
    contracts,
    categoryId: category.categoryId,
    signer: harness.proverSigner,
  };
  const init = await registered.init(setup.fraudulentBlockOutRef);
  const bound = await captureEmulatorSubmission(harness.emulator, () =>
    submitTransactionOutputNonCanonicalStep01Forced({
      ...step,
      threadOutRef: `${init.result.txHash}#${init.result.firstStepOutputIndex.toString()}`,
      finding: evidence,
      forcedSource: {
        header: forced.header,
        membership,
      },
      referenceScriptUtxo: references[0]!,
    }),
  );
  const authenticate = async () =>
    await captureEmulatorSubmission(harness.emulator, () =>
      submitTransactionOutputNonCanonicalStep02({
        ...step,
        ...source,
        threadOutRef: bound.result.nextThreadOutRef,
        evidence,
        referenceScriptUtxo: references[1]!,
        certificateReferenceScriptUtxo: certificateReference,
        ...(grammar === undefined
          ? {}
          : { fieldGrammar: grammar.fieldGrammar }),
      }),
    );
  const mint = async (threadOutRef: string) =>
    await captureEmulatorSubmission(harness.emulator, () =>
      submitTransactionOutputNonCanonicalStep04({
        ...step,
        threadOutRef,
        evidence,
        referenceScriptUtxo: references[4]!,
        witnessReferenceScripts: harness.witnessReferenceScripts,
      }),
    );
  /** The fraud proof removes the block: its state-queue node is spent. */
  const expectBlockRemoved = async () => {
    await registered.removal(setup.headerHash);
    const [txHash, index] = setup.fraudulentBlockOutRef.split("#");
    expect(
      await harness.proverLucid.utxosByOutRef([
        { txHash: txHash!, outputIndex: Number(index) },
      ]),
    ).toHaveLength(0);
  };
  return {
    harness,
    registered,
    setup,
    forced,
    evidence,
    step,
    source,
    references,
    certificateReference,
    boundThreadOutRef: bound.result.nextThreadOutRef,
    authenticate,
    mint,
    expectBlockRemoved,
  };
};

/**
 * The two-output transaction rejected at `written + offset`, authenticated
 * through step 02 and scanned to its terminal checkpoint by `scanToTerminal`.
 */
const setupScenario = async (offset: bigint) => {
  const s = await setupForcedThread({
    forcedTransaction: transaction,
    outputIndex: (await writtenOutputIndex()) + offset,
  });
  const authenticated = (await s.authenticate()).result;
  /** Step 03 run to its terminal checkpoint. */
  const scanToTerminal = async (): Promise<string> => {
    let threadOutRef = authenticated.nextThreadOutRef;
    for (;;) {
      const scanned = await submitTransactionOutputNonCanonicalStep03({
        ...s.step,
        ...s.source,
        threadOutRef,
        evidence: s.evidence,
        referenceScriptUtxo: s.references[2]!,
      });
      threadOutRef = scanned.nextThreadOutRef;
      if (scanned.terminal) return threadOutRef;
    }
  };
  return { ...s, authenticated, scanToTerminal };
};

/** 761 outputs of 43 preimage bytes fill field 2 to the tier-3 maximum. */
const MAX_OUTPUT_COUNT = 761;

/** One canonical output: enterprise pubkey address, lovelace only. */
const maxShapeOutputCbor = (index: number): Buffer =>
  Buffer.concat([
    Buffer.from([0xa2, 0x00, 0x58, 0x1d, 0x60]),
    Buffer.alloc(28, (0x40 + index) % 0x100),
    Buffer.from([0x01, 0x82]),
    encodeCbor(5_000_000n + BigInt(index)),
    Buffer.from([0xa0]),
  ]);

describe("forced OutputNonCanonical coordinate the node writes", () => {
  it("convicts a coordinate one position early, where the output is canonical", async () => {
    const s = await setupScenario(-1n);
    expect(s.evidence.canonical).toBe(true);
    const minted = await s.mint(await s.scanToTerminal());
    expect(minted.result.fraudProofUnit).toBeTruthy();
    await s.expectBlockRemoved();
  }, 900_000);

  it("refuses the written coordinate on chain", async () => {
    const s = await setupScenario(0n);
    // The written output is the malformed one: the rejection holds.
    expect(s.evidence.canonical).toBe(false);
    expect(s.evidence.decisiveFaultHolds).toBe(true);
    const terminal = await s.scanToTerminal();
    await expect(
      submitTransactionOutputNonCanonicalStep04({
        ...s.step,
        threadOutRef: terminal,
        evidence: s.evidence,
        referenceScriptUtxo: s.references[4]!,
        witnessReferenceScripts: s.harness.witnessReferenceScripts,
      }),
    ).rejects.toThrow(/does not contradict/u);
    // The step authenticates the non-canonical terminal and the rule, which
    // convicts only an outcome the reason contradicts, returns false.
    await expectOnchainRefusal(
      async () =>
        await submitOutputStep04Raw({
          ...s.registered.common(terminal, 4),
          witnessReferenceScripts: s.harness.witnessReferenceScripts,
        }),
      {
        refusedBy: "fraud_proofs/transaction_output_non_canonical/step_04",
        check: /^Validator returned false$/u,
      },
    );
  }, 900_000);

  it("convicts a coordinate past the last output, which field 2 does not have", async () => {
    const s = await setupScenario(1n);
    expect(s.evidence.absent).toBe(true);
    // Step 02 closes the coordinate by field 2's count and hands the thread
    // straight to step 04, with no output to scan.
    expect(s.authenticated.nextStage).toBe("step04");
    const minted = await s.mint(s.authenticated.nextThreadOutRef);
    expect(minted.result.fraudProofUnit).toBeTruthy();
    await s.expectBlockRemoved();
  }, 900_000);

  it("refuses on chain a claim that the written coordinate is absent", async () => {
    const s = await setupForcedThread({
      forcedTransaction: transaction,
      outputIndex: await writtenOutputIndex(),
    });
    expect(s.evidence.absent).toBe(false);
    const carriage = await publishOutputFieldCarriage({
      lucid: s.harness.proverLucid,
      network: "Custom",
      signer: s.harness.proverSigner,
      contracts: s.step.contracts,
      anchorTxId: s.forced.transaction.tx_id,
      nativeTxCompactCbor: s.source.nativeTxCompactCbor,
      witnessSetCompactCbor: s.source.witnessSetCompactCbor,
      items: decodeMidgardFieldPreimage(
        Buffer.from(s.evidence.fieldPreimageHex, "hex"),
      ),
      certificateReferenceScriptUtxo: s.certificateReference,
      anchorSourceKind: 1n,
    });
    // The absent terminal state an output past the field's end would carry,
    // claimed for output 1, which field 2 holds.
    const absentState = scanStateOf(
      {
        ...s.evidence,
        itemLength: 0,
        itemHash: "",
        chunkHashes: [],
        scanControls: [initialMidgardLedgerOutputScanControl()],
      },
      0,
      TRANSACTION_OUTPUT_OUTCOME_ABSENT,
    );
    await expectOnchainRefusal(
      async () =>
        await submitOutputStep02Raw({
          ...s.registered.common(s.boundThreadOutRef, 1),
          opening: outputFieldOpening({
            anchorCompactCbor: s.source.nativeTxCompactCbor,
            carriage,
            stepReference: s.references[1]!,
            certificatePolicyId:
              s.step.contracts.fieldPreimageCertificatePolicyId,
          }),
          nextState: absentState,
          nextStepIndex: 4,
          coordinateAbsent: true,
          carriageUtxos: carriage.carriageUtxos,
          extraReferenceInputs:
            carriage.certificateUtxo === undefined
              ? []
              : [carriage.certificateUtxo],
        }),
      {
        refusedBy: "fraud_proofs/transaction_output_non_canonical/step_02",
        check: /^Validator returned false$/u,
      },
    );
  }, 900_000);

  it("convicts an absent coordinate past a 761-output field through the field grammar certificate", async () => {
    const outputs = Array.from({ length: MAX_OUTPUT_COUNT }, (_, index) =>
      maxShapeOutputCbor(index),
    );
    const s = await setupForcedThread({
      forcedTransaction: makeNativeTx({
        spendInputCbors: [],
        fee: 0n,
        referenceByte: "b1",
        outputCbors: outputs,
        witnessByte: "b8",
      }),
      outputIndex: BigInt(MAX_OUTPUT_COUNT),
      withFieldGrammar: true,
    });
    expect(s.evidence.absent).toBe(true);
    expect(s.evidence.carriage).toBe("Certified");
    expect(MAX_OUTPUT_COUNT).toBeGreaterThan(FIELD_GRAMMAR_WALKED_COUNT_LIMIT);
    const authenticated = await s.authenticate();
    expect(authenticated.result.nextStage).toBe("step04");
    const minted = await s.mint(authenticated.result.nextThreadOutRef);
    expect(minted.result.fraudProofUnit).toBeTruthy();
    for (const { measurement } of [authenticated, minted]) {
      expect(measurement.executionMemory).toBeLessThanOrEqual(
        VAN_ROSSEM_MAX_MEMORY_UNITS,
      );
      expect(measurement.executionSteps).toBeLessThanOrEqual(
        VAN_ROSSEM_MAX_CPU_UNITS,
      );
    }
    console.info(
      `max-shape absent output: step-02 mem=${authenticated.measurement.executionMemory.toString()} cpu=${authenticated.measurement.executionSteps.toString()}; step-04 mem=${minted.measurement.executionMemory.toString()} cpu=${minted.measurement.executionSteps.toString()}`,
    );
    await s.expectBlockRemoved();
  }, 1_800_000);
});
