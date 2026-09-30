import { Data, getAddressDetails, type UTxO } from "@lucid-evolution/lucid";
import { expect } from "vitest";

import {
  applyMissingRedeemerScripts,
  type MissingRedeemerContracts,
} from "../src/missing-redeemer/contracts.js";
import { MISSING_REDEEMER_CATEGORY_ID } from "../src/missing-redeemer/family.js";
import { buildCatalogueDeploymentInfo } from "./support/emulator/catalogue.js";
import { type CompleteSignedTransactionMeasurement } from "./support/emulator/measurement.js";
import { createMeasuredFitRecorder } from "./support/measured-fit-ledger.js";
import {
  makeFaultProofEmulatorHarness,
  network,
  publishPlainReferenceScriptUtxo,
} from "./support/submit-init-emulator-shared.js";

const AddressDataLocal = Data.Object({
  paymentCredential: Data.Enum([
    Data.Object({ PublicKeyCredential: Data.Tuple([Data.Bytes()]) }),
    Data.Object({ ScriptCredential: Data.Tuple([Data.Bytes()]) }),
  ]),
  stakeCredential: Data.Nullable(Data.Any()),
});

export const PURPOSE_KINDS = [0, 1, 2, 3] as const;

export const PHYSICAL_STEPS = [
  "step01",
  "step02",
  "step02a",
  "step02b",
  "step03",
  "step04",
  "step05",
] as const;

export const AUTHENTICATION_SEAMS = [
  "reference-script",
  "trace-root",
  "trace-state",
  "purpose-selection",
  "source-selection",
  "field-commitment",
  "walk-checkpoint",
] as const;

export const measuredFit = createMeasuredFitRecorder(
  "missing-redeemer",
  "lifecycle",
  "all purpose kinds and source locations in both directions; exact 32,768-byte certified field with 17 redeemers and resumed grammar/walk",
);

export const FAMILY = "missing-redeemer";

export type Row = {
  readonly label: string;
  readonly measurement: CompleteSignedTransactionMeasurement;
};

export const printedFit = (rows: readonly Row[]): string =>
  JSON.stringify(rows, (_key, value: unknown) =>
    typeof value === "bigint" ? value.toString() : value,
  );

export const outRefUtxo = async (
  lucid: Harness["harness"]["proverLucid"],
  outRef: string,
) =>
  (
    await lucid.utxosByOutRef([
      { txHash: outRef.slice(0, 64), outputIndex: Number(outRef.slice(65)) },
    ])
  )[0];

export const makeHarness = async () => {
  const harness = await makeFaultProofEmulatorHarness({
    contractOptions: { alwaysFraudProofCatalogue: true },
  });
  const paymentCredential = getAddressDetails(
    harness.contracts.fraudProof.spendingScriptAddress,
  ).paymentCredential!;
  const addressData = Data.from(
    Data.to(
      {
        paymentCredential:
          paymentCredential.type === "Key"
            ? { PublicKeyCredential: [paymentCredential.hash] }
            : { ScriptCredential: [paymentCredential.hash] },
        stakeCredential: null,
      } as never,
      AddressDataLocal as never,
    ),
  );
  const steps = applyMissingRedeemerScripts({
    blueprint: harness.realBlueprint,
    network,
    computationThreadPolicyId: harness.contracts.computationThread.policyId,
    fraudProofPolicyId: harness.contracts.fraudProof.policyId,
    fraudProofTokenAddressData: addressData,
    fieldPreimageCertificatePolicyId:
      harness.contracts.fieldPreimageCertificate.policyId,
    hubOracleScriptHash: harness.contracts.hubOracle.policyId,
  });
  const contracts: MissingRedeemerContracts = {
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
  const catalogue = await buildCatalogueDeploymentInfo({
    ...harness.contracts.fraudProofs,
    missingRedeemer: {
      ...harness.contracts.fraudProofs.missingRedeemer,
      spendingScriptHash: steps[0].spendingScriptHash,
    },
  });
  const category = catalogue.categories.missingRedeemer;
  expect(category.categoryId).toBe(MISSING_REDEEMER_CATEGORY_ID);
  const publications: Row[] = [];
  const references: UTxO[] = [];
  // Published from the prover wallet: the funder's first UTxO is the
  // deployment nonce the block setup spends later.
  for (const [index, step] of steps.entries()) {
    const published = await publishPlainReferenceScriptUtxo({
      lucid: harness.proverLucid,
      script: step.spendingScript,
      label: `missing-redeemer-${PHYSICAL_STEPS[index]!}`,
    });
    references.push(published.utxo);
    publications.push({
      label: `reference-${PHYSICAL_STEPS[index]!}`,
      measurement: published.publicationMeasurement,
    });
    expect(
      published.publicationMeasurement.completeSignedBytes,
    ).toBeLessThanOrEqual(15_872);
  }
  return {
    harness,
    steps,
    contracts,
    catalogue,
    category,
    references,
    publications,
  };
};

export type Harness = Awaited<ReturnType<typeof makeHarness>>;
