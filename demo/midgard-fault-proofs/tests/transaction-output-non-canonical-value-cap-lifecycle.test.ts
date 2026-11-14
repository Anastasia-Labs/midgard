import {
  encodeMidgardTxOutput,
  type MidgardTxOutput,
  midgardValueToCmlValue,
} from "@al-ft/midgard-core/codec";
import { acceptedVerdictSubject } from "@al-ft/midgard-sdk";
import {
  MIDGARD_COINS_PER_UTXO_BYTE,
  minAdaLovelace,
} from "@al-ft/midgard-validation";
import { describe, expect, it } from "vitest";

import {
  submitTransactionOutputNonCanonicalStep01Accepted,
  submitTransactionOutputNonCanonicalStep02,
  submitTransactionOutputNonCanonicalStep04,
} from "../src/transaction-output-non-canonical/index.js";
import { expectOnchainRefusal } from "./support/emulator/expect-onchain-refusal.js";
import { makeFaultProofEmulatorHarness } from "./support/emulator/harness.js";
import { captureEmulatorSubmission } from "./support/emulator/measurement.js";
import { makeNativeTx } from "./support/emulator/native-tx.js";
import { longAssetNameOutput } from "./support/long-asset-name-value.js";
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

// 25 ADA clears the min-Ada floor, so the twins differ only in the cap.
const overCapOutput = longAssetNameOutput(256n);
const atCapOutput = longAssetNameOutput(24n);

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
    for (const output of [overCapOutput, atCapOutput])
      expect(output.value.lovelace).toBeGreaterThanOrEqual(
        minAdaLovelace(
          MIDGARD_COINS_PER_UTXO_BYTE,
          BigInt(encodeMidgardTxOutput(output).length),
        ),
      );
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
});
