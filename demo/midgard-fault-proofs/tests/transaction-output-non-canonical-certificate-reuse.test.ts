/**
 * Step 02 at a Certified-tier opening, driven turn by turn through the
 * workflow capture boundary: once the field certificate is on chain, the
 * next captured transaction is the step itself, and only one certificate is
 * ever minted.
 */
import { forcedVerdictSubject } from "@al-ft/midgard-sdk";
import { getAddressDetails } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  submitTransactionOutputNonCanonicalStep01Forced,
  submitTransactionOutputNonCanonicalStep02,
} from "../src/transaction-output-non-canonical/index.js";
import { buildForcedTransactionLeafMembershipProof } from "../src/transition-trace/witnesses.js";
import {
  driveCapturedStep,
  fieldPreimageCertificateCount,
} from "./support/captured-step-turns.js";
import { makeFaultProofEmulatorHarness } from "./support/emulator/harness.js";
import {
  alignUnixTimeToEmulatorSlotBoundary,
  submitSetupTx,
} from "./support/submit-init-emulator-shared.js";
import {
  buildForcedOutputFixture,
  canonicalOutputOfLength,
} from "./support/transaction-output-non-canonical-emulator.js";
import {
  evidenceOf,
  MAXIMUM_OUTPUT_BYTES,
  registeredContracts,
} from "./transaction-output-non-canonical-lifecycle.registered-contracts.js";

describe("transactionOutputNonCanonical step 02 under the workflow capture boundary", () => {
  it("reuses the field certificate already on chain, so the next captured transaction is the step", async () => {
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
      outputCbor: canonicalOutputOfLength(MAXIMUM_OUTPUT_BYTES),
    });
    const setup = await submitSetupTx({
      lucid: harness.funderLucid,
      contracts: harness.contracts,
      nonceUtxo: harness.nonceUtxo,
      catalogue: registered.catalogue,
      header: forced.header,
    });
    const certificateReference = await registered.publishReferences();
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
      0,
    );
    expect(evidence.carriage).toBe("Certified");
    const init = await registered.init(setup.fraudulentBlockOutRef);
    const bound = await submitTransactionOutputNonCanonicalStep01Forced({
      lucid: harness.proverLucid,
      contracts,
      categoryId: category.categoryId,
      signer: harness.proverSigner,
      threadOutRef: `${init.result.txHash}#${init.result.firstStepOutputIndex.toString()}`,
      finding: evidence,
      forcedSource: { header: forced.header, membership },
      referenceScriptUtxo: references[0]!,
    });
    const drive = await driveCapturedStep({
      emulator: harness.emulator,
      threadOutRef: bound.nextThreadOutRef,
      maxTurns: 6,
      invoke: async (boundary) =>
        await submitTransactionOutputNonCanonicalStep02({
          lucid: harness.proverLucid,
          contracts,
          categoryId: category.categoryId,
          signer: harness.proverSigner,
          threadOutRef: bound.nextThreadOutRef,
          evidence,
          nativeTxCompactCbor: forced.transaction.submitted_source.compact_cbor,
          witnessSetCompactCbor:
            forced.transaction.submitted_source.witness_set_compact_cbor,
          publishCarriage: evidence.carriage === "RawUtxo",
          referenceScriptUtxo: references[1]!,
          certificateReferenceScriptUtxo: certificateReference,
          publicationPreSubmitBoundary: boundary,
          certificatePreSubmitBoundary: boundary,
          preSubmitBoundary: boundary,
        }),
    });
    expect(drive.prerequisites.length).toBeGreaterThan(1);
    expect(
      await fieldPreimageCertificateCount({
        lucid: harness.proverLucid,
        certificatePolicyId: contracts.fieldPreimageCertificatePolicyId,
      }),
    ).toBe(1n);
  }, 900_000);
});
