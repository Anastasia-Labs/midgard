/**
 * Step 02 at a Certified-tier opening, driven turn by turn through the
 * workflow capture boundary: once the field certificate is on chain, the
 * next captured transaction is the step itself, and only one certificate is
 * ever minted.
 */
import { decodeMidgardFieldPreimage } from "@al-ft/midgard-core";
import {
  FIELD_PREIMAGE_CERTIFICATE_ASSET_NAME_HEX,
  forcedVerdictSubject,
} from "@al-ft/midgard-sdk";
import { getAddressDetails } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import { planFaultProofFieldOpening } from "../src/field-opening.js";
import {
  submitTransactionOutputNonCanonicalStep01Forced,
  submitTransactionOutputNonCanonicalStep02,
} from "../src/transaction-output-non-canonical/index.js";
import { buildForcedTransactionLeafMembershipProof } from "../src/transition-trace/witnesses.js";
import { createAuthenticatedFieldCarriagePrerequisitePort } from "../src/workflow/field-carriage-prerequisite.js";
import { workflowFieldCarriageOwnOutputs } from "../src/workflow/field-carriage-prerequisite.own-carriage-claims.js";
import {
  createOwnOutputJournal,
  type OwnOutputJournal,
} from "../src/workflow/own-outputs.js";
import {
  FRAUD_PROOF_AUTHENTICATED_PUBLICATION_OBSERVER,
  type FraudProofAuthenticatedPublicationObserver,
} from "../src/workflow/raw-l1-publication-observation.js";
import {
  type FraudProofPreSubmitBoundary,
  submitCapturedTransaction,
} from "../src/workflow/transaction-boundary.js";
import {
  capturedOutputHolding,
  driveCapturedStep,
  fieldPreimageCertificateCount,
} from "./support/captured-step-turns.js";
import { createConfirmedActionJournal } from "./support/confirmed-action-journal.js";
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

/** Step 02's thread input, one step past forced binding at Certified tier. */
const stepTwoReady = async () => {
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
  return {
    harness,
    contracts,
    category,
    references,
    certificateReference,
    forced,
    evidence,
    threadOutRef: bound.nextThreadOutRef,
  };
};

const submitStepTwo = (
  ready: Awaited<ReturnType<typeof stepTwoReady>>,
  boundary: FraudProofPreSubmitBoundary,
  ownOutputs: OwnOutputJournal,
) =>
  submitTransactionOutputNonCanonicalStep02({
    ownOutputs,
    lucid: ready.harness.proverLucid,
    contracts: ready.contracts,
    categoryId: ready.category.categoryId,
    signer: ready.harness.proverSigner,
    threadOutRef: ready.threadOutRef,
    evidence: ready.evidence,
    nativeTxCompactCbor: ready.forced.transaction.submitted_source.compact_cbor,
    witnessSetCompactCbor:
      ready.forced.transaction.submitted_source.witness_set_compact_cbor,
    publishCarriage: ready.evidence.carriage === "RawUtxo",
    referenceScriptUtxo: ready.references[1]!,
    certificateReferenceScriptUtxo: ready.certificateReference,
    publicationPreSubmitBoundary: boundary,
    certificatePreSubmitBoundary: boundary,
    preSubmitBoundary: boundary,
  });

describe("transactionOutputNonCanonical step 02 under the workflow capture boundary", () => {
  it("reuses the field certificate already on chain, so the next captured transaction is the step", async () => {
    const ready = await stepTwoReady();
    // One run of the step: the carriage it publishes on one turn is what it
    // reads back on the next.
    const ownOutputs = createOwnOutputJournal();
    const drive = await driveCapturedStep({
      emulator: ready.harness.emulator,
      threadOutRef: ready.threadOutRef,
      maxTurns: 6,
      invoke: async (boundary) =>
        await submitStepTwo(ready, boundary, ownOutputs),
    });
    expect(drive.prerequisites.length).toBeGreaterThan(1);
    expect(
      await fieldPreimageCertificateCount({
        lucid: ready.harness.proverLucid,
        certificatePolicyId: ready.contracts.fieldPreimageCertificatePolicyId,
      }),
    ).toBe(1n);
  }, 900_000);

  it("references the certificate the field-carriage prerequisite landed, without minting another", async () => {
    const ready = await stepTwoReady();
    const lucid = ready.harness.proverLucid;
    const network = lucid.config().network!;
    // The production step-02 requirement, from the same authenticated source.
    const requirement = {
      planned: planFaultProofFieldOpening({
        anchorSourceKind: ready.evidence.subject.source_kind === 1n ? 1n : 0n,
        fieldIndex: ready.evidence.fieldIndex,
        anchorTxId: ready.evidence.subject.transaction_id,
        nativeTxCompactCbor:
          ready.forced.transaction.submitted_source.compact_cbor,
        itemCbors: decodeMidgardFieldPreimage(
          Buffer.from(ready.evidence.fieldPreimageHex, "hex"),
        ),
        owner: ready.harness.proverSigner.paymentKeyHash,
        publish: true,
        label: "transactionOutputNonCanonical field opening",
      }),
      compactCbor: ready.forced.transaction.submitted_source.compact_cbor,
      witnessSetCompactCbor:
        ready.forced.transaction.submitted_source.witness_set_compact_cbor,
      certificate: {
        policyId: ready.contracts.fieldPreimageCertificatePolicyId,
        mintingScript: ready.contracts.fieldPreimageCertificateMintingScript,
        referenceScriptUtxo: ready.certificateReference,
      },
    };
    const publications: FraudProofAuthenticatedPublicationObserver = {
      observerVersion: FRAUD_PROOF_AUTHENTICATED_PUBLICATION_OBSERVER,
      observeExact: async (input) => {
        const found = (await lucid.utxosAt(input.address)).find(
          (utxo) =>
            `${utxo.txHash}#${utxo.outputIndex}` === input.expectedOutRef &&
            utxo.datum === input.expectedDatumCbor &&
            (input.expectedUnit === undefined ||
              utxo.assets[input.expectedUnit] === 1n),
        );
        return found === undefined
          ? { kind: "not_found" as const }
          : { kind: "confirmed" as const, outRef: input.expectedOutRef };
      },
    };
    const headerHash = "00".repeat(28);
    const journal = createConfirmedActionJournal({
      category: "transactionOutputNonCanonical",
      headerHash,
    });
    const port = createAuthenticatedFieldCarriagePrerequisitePort({
      category: "transactionOutputNonCanonical",
      lucid,
      network,
      signer: ready.harness.proverSigner,
      publications,
      requirementForAction: () => requirement,
      transactionConfirmed: async () => false,
    });
    const baseAction = {
      actionId: "transactionOutputNonCanonical:step_02",
      input: { stage: "step_02", threadOutRef: ready.threadOutRef },
    };
    const certificateUnit = `${requirement.certificate.policyId}${FIELD_PREIMAGE_CERTIFICATE_ASSET_NAME_HEX}`;
    let certification: string | undefined;
    for (let turn = 0; turn < 8; turn += 1) {
      const inspected = await port.inspect({
        headerHash,
        baseAction,
        artifact: {},
        entries: journal.entries(),
      });
      if (inspected.kind === "satisfied") break;
      if (inspected.kind !== "required")
        throw new Error(`field prerequisite is ${inspected.kind}`);
      const { transaction, durableRecovery } = await port.capture({
        headerHash,
        action: inspected.action,
        artifact: {},
        entries: journal.entries(),
      });
      await submitCapturedTransaction(transaction);
      ready.harness.emulator.awaitBlock();
      await expect(
        port.reconcile({
          headerHash,
          action: inspected.action,
          artifact: {},
          txHash: transaction.txHash,
          durableRecovery,
        }),
      ).resolves.toEqual({ kind: "confirmed", txHash: transaction.txHash });
      journal.confirm({
        action: inspected.action,
        txHash: transaction.txHash,
        durableRecovery,
      });
      if (inspected.action.input.stage === "certify_field_carriage")
        certification = capturedOutputHolding(transaction, certificateUnit);
    }
    if (certification === undefined)
      throw new Error("the prerequisite never certified the field");

    const drive = await driveCapturedStep({
      emulator: ready.harness.emulator,
      threadOutRef: ready.threadOutRef,
      maxTurns: 1,
      invoke: async (boundary) =>
        await submitStepTwo(
          ready,
          boundary,
          workflowFieldCarriageOwnOutputs({
            category: "transactionOutputNonCanonical",
            entries: journal.entries(),
          }),
        ),
    });
    expect(drive.prerequisites).toEqual([]);
    const referenceInputs = drive.step.signed
      .toTransaction()
      .body()
      .reference_inputs();
    const referenced = Array.from(
      { length: referenceInputs?.len() ?? 0 },
      (_, index) => {
        const input = referenceInputs!.get(index);
        return `${input.transaction_id().to_hex()}#${input.index().toString()}`;
      },
    );
    expect(referenced).toContain(certification);
    expect(
      await fieldPreimageCertificateCount({
        lucid,
        certificatePolicyId: ready.contracts.fieldPreimageCertificatePolicyId,
      }),
    ).toBe(1n);
  }, 900_000);
});
