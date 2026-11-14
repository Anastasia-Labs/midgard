/**
 * The item-reader end to end, through the production workflow entry: an
 * accepted transaction whose outputs field carries 761 items under tier 3, the
 * malformed one at 751. The config is built from a deployment that names the
 * field grammar certificate's members, so step 02 reads the accused output
 * through the field grammar certificate, and its cost no longer grows with the
 * field's item count; the scan and step 04 then convict. A direct step-02 call
 * without the certificate walks, and at this size the walk fails on chain.
 */
import {
  acceptedVerdictSubject,
  fieldGrammarCertificateUnit,
} from "@al-ft/midgard-sdk";
import { describe, expect, it } from "vitest";

import {
  FIELD_GRAMMAR_WALKED_ITEM_LIMIT,
  fieldGrammarCertificateAddress,
} from "../src/field-grammar-certificate.js";
import {
  VAN_ROSSEM_MAX_CPU_UNITS,
  VAN_ROSSEM_MAX_MEMORY_UNITS,
} from "../src/proof-fit/van-rossem.js";
import { submitTransactionOutputNonCanonicalStep02 } from "../src/transaction-output-non-canonical/index.js";
import { createManifestBoundTransactionOutputNonCanonicalSubmission } from "../src/transaction-output-non-canonical/workflow.create-manifest-bound-transaction-output-non-canonical-submission.js";
import {
  transactionOutputNonCanonicalConfigFromBinding,
  type TransactionOutputNonCanonicalReferenceScripts,
  type TransactionOutputNonCanonicalStage,
} from "../src/transaction-output-non-canonical/workflow.transaction-output-non-canonical-config-from-binding.js";
import type { FraudProofWorkflowDeploymentBinding } from "../src/workflow/deployment-manifest-binding.js";
import { createOwnOutputJournal } from "../src/workflow/own-outputs.js";
import { makeFaultProofEmulatorHarness } from "./support/emulator/harness.js";
import { captureEmulatorSubmission } from "./support/emulator/measurement.js";
import { makeNativeTx } from "./support/emulator/native-tx.js";
import { deployFieldGrammarCertificate } from "./support/field-grammar-certificate-deployment-fixture.js";
import { setupFraudulentBlock } from "./support/submit-init-emulator-fixtures.js";
import { MALFORMED_OUTPUT } from "./support/transaction-output-non-canonical-emulator.js";
import {
  committedInclusions,
  evidenceOf,
  network,
  registeredContracts,
  witnessSetCborOf,
} from "./transaction-output-non-canonical-lifecycle.registered-contracts.js";

/** 761 outputs fill field 2 to the tier-3 maximum; the accused one is 751. */
const OUTPUT_COUNT = 761;
const ACCUSED_INDEX = 751;
const RESERVED_MEMORY = (VAN_ROSSEM_MAX_MEMORY_UNITS * 4n) / 5n;
const RESERVED_CPU = (VAN_ROSSEM_MAX_CPU_UNITS * 4n) / 5n;

const outputCbors = (): Buffer[] =>
  Array.from({ length: OUTPUT_COUNT }, (_, index) => {
    if (index === ACCUSED_INDEX) return MALFORMED_OUTPUT;
    const filler = Buffer.alloc(41, 0xa1);
    filler.writeUInt16BE(index, 0);
    return filler;
  });

/**
 * One thread, accused and bound through step 01 by the production entry,
 * against a deployment that publishes and names the field grammar
 * certificate.
 */
const buildScenario = async () => {
  const harness = await makeFaultProofEmulatorHarness({
    contractOptions: {
      realTransactionOutputNonCanonical: true,
      alwaysFraudProofCatalogue: true,
    },
  });
  const registered = await registeredContracts(harness);
  const { category, references } = registered;
  const nativeTx = makeNativeTx({
    spendInputCbors: [],
    fee: 11n,
    outputCbors: outputCbors(),
  });
  const committed = await committedInclusions([nativeTx]);
  const inclusion = committed.inclusions[0]!;
  const setup = await setupFraudulentBlock({
    funderLucid: harness.funderLucid,
    emulator: harness.emulator,
    contracts: harness.contracts,
    catalogue: registered.catalogue,
    fixture: {
      transactionsRoot: committed.transactionsRoot,
      l2TransactionCount: 1n,
    },
  });
  const certificateReference = await registered.publishReferences();
  const preimageCertificate = harness.contracts.fieldPreimageCertificate;
  const grammar = await deployFieldGrammarCertificate({
    lucid: harness.funderLucid,
    blueprint: harness.realBlueprint,
    network,
    fieldPreimageCertificatePolicyId: preimageCertificate.policyId,
    label: "transactionOutputNonCanonical",
  });

  const evidence = evidenceOf(
    acceptedVerdictSubject(inclusion.nativeTxId),
    nativeTx,
    ACCUSED_INDEX,
  );
  expect(evidence.canonical).toBe(false);
  expect(evidence.carriage).toBe("Certified");
  expect(OUTPUT_COUNT).toBeGreaterThan(FIELD_GRAMMAR_WALKED_ITEM_LIMIT);

  const init = await registered.init(setup.fraudulentBlockOutRef);
  // The deployment the production config is built from: the registered
  // chain, and a manifest naming both field grammar certificate members.
  const binding = {
    blueprint: harness.realBlueprint,
    network,
    referenceScriptsByContract: grammar.referenceScriptsByContract,
    resolvedContracts: {
      category,
      contracts: {
        transactionOutputNonCanonical:
          harness.contracts.fraudProofContracts.transactionOutputNonCanonical,
        computationThread: harness.contracts.computationThread,
        fraudProof: harness.contracts.fraudProof,
      },
    },
    contractEntries: {
      hubOracleMint: { scriptHash: harness.contracts.hubOracle.policyId },
    },
    definition: {
      headerHash: init.result.fraudulentHeaderHash,
      stateQueue: { policyId: harness.contracts.stateQueue.policyId },
    },
    fieldPreimageCertificate: {
      policyId: preimageCertificate.policyId,
      mintingScript: preimageCertificate.mintingScript,
    },
  } as unknown as FraudProofWorkflowDeploymentBinding<"transactionOutputNonCanonical">;
  const referenceScripts: TransactionOutputNonCanonicalReferenceScripts = {
    step01: references[0]!,
    step02: references[1]!,
    step03: references[2]!,
    step03Datum: references[3]!,
    step04: references[4]!,
    fieldPreimageCertificateMint: certificateReference,
    fieldGrammarCertificateSpend: grammar.referenceScriptUtxo,
    witnesses:
      harness.witnessReferenceScripts as TransactionOutputNonCanonicalReferenceScripts["witnesses"],
  };
  const config = transactionOutputNonCanonicalConfigFromBinding({
    binding,
    lucid: harness.proverLucid,
    signer: harness.proverSigner,
    referenceScripts,
  });
  expect(config.fieldGrammar.certificateReferenceScriptUtxo).toBe(
    grammar.referenceScriptUtxo,
  );

  let stage: TransactionOutputNonCanonicalStage = {
    fraudulentBlockOutRef: setup.fraudulentBlockOutRef,
  };
  const ownOutputs = createOwnOutputJournal();
  const submission = createManifestBoundTransactionOutputNonCanonicalSubmission(
    {
      config,
      observe: () => Promise.resolve("step01"),
      resolveStage: () => Promise.resolve(stage as never),
      ownOutputs,
    },
  );
  stage = {
    ...stage,
    threadUtxo: await registered.threadUtxoOf(
      init.result.txHash,
      init.result.firstStepOutputIndex,
    ),
    threadToken: {
      unit: init.result.computationThreadUnit,
      fraudulentHeaderHash: init.result.fraudulentHeaderHash,
    },
    stateQueueBlockOutRef: setup.fraudulentBlockOutRef,
    acceptedInclusion: inclusion,
  };
  const bound = await submission.submit("submitStep01", evidence);
  expect(bound.stage).toBe("step02");
  stage = {
    fraudulentBlockOutRef: setup.fraudulentBlockOutRef,
    threadOutRef: bound.outputReference!,
    nativeTxCompactCbor: inclusion.nativeTxCompactCbor,
    witnessSetCompactCbor: witnessSetCborOf(nativeTx),
  };
  return {
    harness,
    config,
    grammar,
    evidence,
    ownOutputs,
    submission,
    step02Stage: stage,
    /** The next step's thread and step material, on the same block. */
    advanceStage: (
      next: Omit<TransactionOutputNonCanonicalStage, "fraudulentBlockOutRef">,
    ) => {
      stage = { fraudulentBlockOutRef: setup.fraudulentBlockOutRef, ...next };
    },
  };
};

let scenario: ReturnType<typeof buildScenario> | undefined;
const scenarioOf = () => (scenario ??= buildScenario());

describe("transactionOutputNonCanonical step 02 through the field grammar certificate", () => {
  it("fails on chain when step 02 walks a 761-output field without the certificate", async () => {
    const { config, evidence, ownOutputs, step02Stage } = await scenarioOf();
    // The submitter's walked route, reached only by a direct call that passes
    // no certificate deployment: at this size the walk runs the step over its
    // execution budget.
    await expect(
      submitTransactionOutputNonCanonicalStep02({
        ownOutputs,
        lucid: config.lucid,
        contracts: config.contracts,
        categoryId: config.binding.resolvedContracts.category.categoryId,
        signer: config.signer,
        threadOutRef: step02Stage.threadOutRef!,
        evidence,
        nativeTxCompactCbor: step02Stage.nativeTxCompactCbor!,
        witnessSetCompactCbor: step02Stage.witnessSetCompactCbor!,
        referenceScriptUtxo: config.referenceScripts.step02,
        certificateReferenceScriptUtxo:
          config.referenceScripts.fieldPreimageCertificateMint,
      }),
    ).rejects.toThrow(/went over budget/u);
  }, 1_800_000);

  it("certifies a 761-output field through the production entry, reads the accused output through the certificate and convicts", async () => {
    const { harness, config, grammar, evidence, submission, advanceStage } =
      await scenarioOf();
    const authenticated = await captureEmulatorSubmission(
      harness.emulator,
      () => submission.submit("submitStep02", evidence),
    );
    expect(authenticated.result.stage).toBe("step03");
    expect(authenticated.measurement.executionMemory).toBeLessThanOrEqual(
      RESERVED_MEMORY,
    );
    expect(authenticated.measurement.executionSteps).toBeLessThanOrEqual(
      RESERVED_CPU,
    );
    const certificates = await harness.proverLucid.utxosAt(
      fieldGrammarCertificateAddress({
        network,
        certificatePolicyId: grammar.policyId,
      }),
    );
    expect(
      certificates.filter(
        (utxo) =>
          (utxo.assets[fieldGrammarCertificateUnit(grammar.policyId)] ?? 0n) ===
          1n,
      ),
    ).toHaveLength(1);

    advanceStage({
      threadOutRef: authenticated.result.outputReference!,
      nativeTxCompactCbor: "",
      witnessSetCompactCbor: "",
    });
    const scanned = await submission.submit("submitStep03", evidence);
    expect(scanned.stage).toBe("step04");
    advanceStage({
      threadOutRef: scanned.outputReference!,
    });
    const minted = await submission.submit("submitStep04", evidence);
    expect(minted.stage).toBe("proven");
    const fraudProofs = await harness.proverLucid.utxosAt(
      config.contracts.fraudProof.spendingScriptAddress,
    );
    expect(
      fraudProofs.some((utxo) =>
        Object.keys(utxo.assets).some((unit) =>
          unit.startsWith(config.contracts.fraudProof.policyId),
        ),
      ),
    ).toBe(true);
  }, 1_800_000);
});
