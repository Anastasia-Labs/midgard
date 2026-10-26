import { encodeMidgardNativeTxCanonical } from "@al-ft/midgard-core";
import {
  encodeMidgardTxOutput,
  type MidgardTxOutput,
  midgardValueToCmlValue,
} from "@al-ft/midgard-core/codec";
import { acceptedVerdictSubject } from "@al-ft/midgard-sdk";
import { describe, expect, it } from "vitest";

import { canonicalBlockEvidenceFromVerifiedPayload } from "../src/evidence/canonical-block-evidence.js";
import {
  submitTransactionOutputNonCanonicalStep01Accepted,
  submitTransactionOutputNonCanonicalStep02,
  submitTransactionOutputNonCanonicalStep04,
} from "../src/transaction-output-non-canonical/index.js";
import { classifyCanonicalBlockViolations } from "../src/workflow/classification.js";
import {
  CANONICAL_DECODABILITY_COMPLETE_CANONICAL_REPLAY,
  COMMITTED_FIELD_SHAPE_COMPLETE_CANONICAL_REPLAY,
  createCompleteCanonicalReplayUnion,
  FIELD_ITEM_WIDTH_ILLEGAL_COMPLETE_CANONICAL_REPLAY,
  FIELD_PREIMAGE_LENGTH_MISMATCH_COMPLETE_CANONICAL_REPLAY,
  MINT_ITEM_NON_CANONICAL_COMPLETE_CANONICAL_REPLAY,
  OUTPUT_REFERENCE_SCRIPT_DECODING_COMPLETE_CANONICAL_REPLAY,
  requireCompleteCanonicalReplayDecision,
  TRANSACTION_OUTPUT_NON_CANONICAL_COMPLETE_CANONICAL_REPLAY,
} from "../src/workflow/complete-replay.js";
import {
  authenticatedHeaderObservation,
  buildCanonicalBlockFixture,
  fixtureTransactionFromCanonicalCbor,
} from "./helpers/canonical-block-evidence-fixture.js";
import { expectOnchainRefusal } from "./support/emulator/expect-onchain-refusal.js";
import { makeFaultProofEmulatorHarness } from "./support/emulator/harness.js";
import { captureEmulatorSubmission } from "./support/emulator/measurement.js";
import { makeNativeTx } from "./support/emulator/native-tx.js";
import { setupFraudulentBlock } from "./support/submit-init-emulator-fixtures.js";
import {
  publishOutputFieldCarriage,
  readOutputScanState,
  submitOutputStep04Raw,
} from "./support/transaction-output-non-canonical-emulator.js";
import { record } from "./transaction-output-non-canonical-lifecycle.record-fit.js";
import {
  committedInclusions,
  type Evidence,
  evidenceOf,
  type Inclusion,
  type NativeTx,
  network,
  registeredContracts,
  witnessSetCborOf,
} from "./transaction-output-non-canonical-lifecycle.registered-contracts.js";
import { scanToTerminal } from "./transaction-output-non-canonical-lifecycle.scan-to-terminal.js";

/**
 * The output `{0: address, 1: value}` whose Cardano value is one policy of
 * 141 32-byte names and one 23-byte tail name: 5,000 bytes with tail
 * quantity 24 and 5,001 with 256. The value runs across the 4,095-byte chunk
 * boundary.
 */
const longNameOutput = (tailQuantity: 24n | 256n): MidgardTxOutput => ({
  address: Buffer.concat([Buffer.from([0x60]), Buffer.alloc(28, 0x11)]),
  value: {
    lovelace: 2_000_000n,
    assets: new Map([
      [
        "55".repeat(28),
        new Map([
          ...Array.from(
            { length: 141 },
            (_, index) =>
              [
                Buffer.concat([
                  Buffer.from([index]),
                  Buffer.alloc(31, 0xaa),
                ]).toString("hex"),
                1n,
              ] as const,
          ),
          [`ff${"bb".repeat(22)}`, tailQuantity] as const,
        ]),
      ],
    ]),
  },
});

const overCapOutput = longNameOutput(256n);
const atCapOutput = longNameOutput(24n);

/** A normal transaction committing `output` as its only output. */
const transactionWith = (output: MidgardTxOutput, fee: bigint): NativeTx =>
  makeNativeTx({
    spendInputCbors: [],
    fee,
    outputCbors: [encodeMidgardTxOutput(output)],
  });

describe("transactionOutputNonCanonical at the 5,000-byte Cardano value cap", () => {
  it("convicts an accepted output whose value is 5,001 bytes from Init to the proof mint and removes the block, and refuses the 5,000-byte twin", async () => {
    expect(
      midgardValueToCmlValue(overCapOutput.value).to_cbor_bytes(),
    ).toHaveLength(5_001);
    expect(
      midgardValueToCmlValue(atCapOutput.value).to_cbor_bytes(),
    ).toHaveLength(5_000);
    const harness = await makeFaultProofEmulatorHarness({
      contractOptions: {
        realTransactionOutputNonCanonical: true,
        alwaysFraudProofCatalogue: true,
      },
    });
    const registered = await registeredContracts(harness);
    const { contracts, category, references, common } = registered;

    const overCapTx = transactionWith(overCapOutput, 11n);
    const atCapTx = transactionWith(atCapOutput, 12n);
    const committed = await committedInclusions([overCapTx, atCapTx]);
    const overCapInclusion = committed.inclusions[0]!;
    const atCapInclusion = committed.inclusions[1]!;
    const setup = await setupFraudulentBlock({
      funderLucid: harness.funderLucid,
      emulator: harness.emulator,
      contracts: harness.contracts,
      catalogue: registered.catalogue,
      fixture: {
        transactionsRoot: committed.transactionsRoot,
        l2TransactionCount: 2n,
      },
    });
    const certificateReference = await registered.publishReferences();

    const overCapEvidence = evidenceOf(
      acceptedVerdictSubject(overCapInclusion.nativeTxId),
      overCapTx,
      0,
    );
    expect(overCapEvidence.canonical).toBe(false);
    expect(overCapEvidence.decisiveFaultHolds).toBe(true);
    const atCapEvidence = evidenceOf(
      acceptedVerdictSubject(atCapInclusion.nativeTxId),
      atCapTx,
      0,
    );
    expect(atCapEvidence.canonical).toBe(true);
    expect(atCapEvidence.decisiveFaultHolds).toBe(false);

    // Init, step 01 and step 02: the thread binds the accepted transaction's
    // output and opens its certified field-2 bytes.
    const authenticate = async (
      inclusion: Inclusion,
      nativeTx: NativeTx,
      evidence: Evidence,
      label: string,
    ) => {
      const carriage = await publishOutputFieldCarriage({
        lucid: harness.proverLucid,
        network,
        signer: harness.proverSigner,
        contracts,
        anchorTxId: inclusion.nativeTxId,
        nativeTxCompactCbor: inclusion.nativeTxCompactCbor,
        witnessSetCompactCbor: witnessSetCborOf(nativeTx),
        items: [Buffer.from(evidence.itemHex, "hex")],
        certificateReferenceScriptUtxo: certificateReference,
      });
      const init = await registered.init(setup.fraudulentBlockOutRef);
      record(`${label}-init`, init.measurement);
      const threadUtxo = await registered.threadUtxoOf(
        init.result.txHash,
        init.result.firstStepOutputIndex,
      );
      const bound = await captureEmulatorSubmission(harness.emulator, () =>
        submitTransactionOutputNonCanonicalStep01Accepted({
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
          txInclusion: inclusion,
          referenceScriptUtxo: references[0]!,
          witnessReferenceScripts: harness.witnessReferenceScripts,
        }),
      );
      record(`${label}-step01`, bound.measurement);
      const opened = await captureEmulatorSubmission(harness.emulator, () =>
        submitTransactionOutputNonCanonicalStep02({
          lucid: harness.proverLucid,
          contracts,
          categoryId: category.categoryId,
          signer: harness.proverSigner,
          threadOutRef: bound.result.nextThreadOutRef,
          evidence,
          nativeTxCompactCbor: inclusion.nativeTxCompactCbor,
          witnessSetCompactCbor: witnessSetCborOf(nativeTx),
          publishedCarriageUtxos: carriage.carriageUtxos,
          certificateUtxo: carriage.certificateUtxo,
          referenceScriptUtxo: references[1]!,
          certificateReferenceScriptUtxo: certificateReference,
        }),
      );
      record(`${label}-step02`, opened.measurement);
      return opened.result.nextThreadOutRef;
    };
    const mint = async (threadOutRef: string, evidence: Evidence) =>
      await captureEmulatorSubmission(harness.emulator, () =>
        submitTransactionOutputNonCanonicalStep04({
          lucid: harness.proverLucid,
          contracts,
          categoryId: category.categoryId,
          signer: harness.proverSigner,
          threadOutRef,
          evidence,
          referenceScriptUtxo: references[4]!,
          witnessReferenceScripts: harness.witnessReferenceScripts,
        }),
      );

    // ## Wrongful acceptance: the scan refuses the asset that takes the value
    // past 5,000 bytes, in the output's second chunk.
    const overCapScan = await scanToTerminal({
      registered,
      threadOutRef: await authenticate(
        overCapInclusion,
        overCapTx,
        overCapEvidence,
        "over-cap",
      ),
      evidence: overCapEvidence,
      rederive: () =>
        evidenceOf(
          acceptedVerdictSubject(overCapInclusion.nativeTxId),
          overCapTx,
          0,
        ),
      label: "over-cap-step03",
      nativeTxCompactCbor: "",
      witnessSetCompactCbor: "",
    });
    expect(overCapScan.scans).toBeGreaterThan(1);
    const overCapTerminal = await readOutputScanState(
      common(overCapScan.threadOutRef, 4),
      4,
    );
    expect(overCapTerminal.outcome).toBe(2n);
    expect(overCapTerminal.control.cursor).toBeGreaterThan(4_095n);
    const minted = await mint(overCapScan.threadOutRef, overCapEvidence);
    expect(minted.result.fraudProofUnit).toBeTruthy();
    record("over-cap-step04-proof-mint", minted.measurement);

    // ## The 5,000-byte twin scans to its canonical terminal and the proof
    // mint refuses, through the builder and on chain.
    const atCapScan = await scanToTerminal({
      registered,
      threadOutRef: await authenticate(
        atCapInclusion,
        atCapTx,
        atCapEvidence,
        "at-cap",
      ),
      evidence: atCapEvidence,
      rederive: () =>
        evidenceOf(
          acceptedVerdictSubject(atCapInclusion.nativeTxId),
          atCapTx,
          0,
        ),
      label: "at-cap-step03",
      nativeTxCompactCbor: "",
      witnessSetCompactCbor: "",
    });
    expect(atCapScan.scans).toBe(atCapEvidence.scanControls.length - 1);
    const atCapTerminal = await readOutputScanState(
      common(atCapScan.threadOutRef, 4),
      4,
    );
    expect(atCapTerminal.outcome).toBe(1n);
    await expect(mint(atCapScan.threadOutRef, atCapEvidence)).rejects.toThrow(
      /does not contradict/u,
    );
    await expectOnchainRefusal(
      async () =>
        await submitOutputStep04Raw({
          ...common(atCapScan.threadOutRef, 4),
          witnessReferenceScripts: harness.witnessReferenceScripts,
        }),
    );
    await registered.cancel(atCapScan.threadOutRef, 4);

    // ## The permanent proof token removes the block.
    const removed = await registered.removal(setup.headerHash);
    record("over-cap-remove-fraudulent-block", removed.measurement);
  }, 900_000);

  it("finds the 5,001-byte value in the block's replay union, and nothing in the 5,000-byte twin's", async () => {
    // The families that judge a transaction's own committed bytes, in
    // catalogue order.
    const union = createCompleteCanonicalReplayUnion([
      CANONICAL_DECODABILITY_COMPLETE_CANONICAL_REPLAY,
      COMMITTED_FIELD_SHAPE_COMPLETE_CANONICAL_REPLAY,
      FIELD_PREIMAGE_LENGTH_MISMATCH_COMPLETE_CANONICAL_REPLAY,
      FIELD_ITEM_WIDTH_ILLEGAL_COMPLETE_CANONICAL_REPLAY,
      TRANSACTION_OUTPUT_NON_CANONICAL_COMPLETE_CANONICAL_REPLAY,
      OUTPUT_REFERENCE_SCRIPT_DECODING_COMPLETE_CANONICAL_REPLAY,
      MINT_ITEM_NON_CANONICAL_COMPLETE_CANONICAL_REPLAY,
    ]);
    const classify = async (nativeTx: NativeTx) => {
      const canonicalCbor = encodeMidgardNativeTxCanonical(nativeTx);
      const [inclusion] = (await committedInclusions([nativeTx])).inclusions;
      const block = await buildCanonicalBlockFixture({
        transactions: [
          fixtureTransactionFromCanonicalCbor({
            txId: inclusion!.nativeTxId,
            canonicalCbor,
          }),
        ],
      });
      const evidence = await canonicalBlockEvidenceFromVerifiedPayload({
        observation: authenticatedHeaderObservation(block),
        payloadEnvelopeCbor: block.payloadEnvelopeCbor,
        daProvenance: {
          trustClass: "public_or_permissionless_da",
          sourceId: "retained-fixture/value-cap",
          grade: "security",
        },
      });
      const decision = await union.replay(evidence);
      return {
        txId: inclusion!.nativeTxId,
        classification: await classifyCanonicalBlockViolations({
          evidence,
          detections: requireCompleteCanonicalReplayDecision({
            evidence,
            replayer: union,
            decision,
          }),
        }),
      };
    };

    const overCap = await classify(transactionWith(overCapOutput, 11n));
    expect(overCap.classification).toMatchObject({
      decision: "fault_detected",
      category: "transactionOutputNonCanonical",
    });
    if (overCap.classification.decision !== "fault_detected") return;
    expect(overCap.classification.detections).toHaveLength(1);
    expect(overCap.classification.selected.detectionId).toContain(overCap.txId);

    const atCap = await classify(transactionWith(atCapOutput, 12n));
    expect(atCap.classification).toMatchObject({
      decision: "no_fault_detected",
    });
  });
});
