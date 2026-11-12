/** Field carriage for the direct-walk emulator driver: every planned field a
 * walk opens that does not travel inline is published (and, at tier 3,
 * certified) ahead of the actions that open it. */
import type { UTxO } from "@lucid-evolution/lucid";

import type { DirectPurposeFieldPlan } from "../../src/direct-purpose-family/index.js";
import type { DirectScriptMaterial } from "../../src/direct-script-derivation/material.js";
import {
  certifyFaultProofFieldCarriage,
  publishFaultProofFieldCarriage,
} from "../../src/field-opening.js";
import { createOwnOutputJournal } from "../../src/workflow/own-outputs.js";
import type { FaultProofEmulatorHarness } from "./emulator/harness.js";
import {
  captureEmulatorSubmission,
  type CompleteSignedTransactionMeasurement,
  network,
  publishPlainReferenceScriptUtxo,
} from "./submit-init-emulator-shared.js";

/** Publishes (and, at tier 3, certifies) every planned field of `fields`
 * that does not travel inline. */
export const publishDirectWalkCarriage = async ({
  harness,
  label,
  fields,
  scenario,
  plan,
}: {
  readonly harness: FaultProofEmulatorHarness;
  readonly label: string;
  readonly fields: readonly number[];
  readonly scenario: Readonly<{ material: DirectScriptMaterial }>;
  readonly plan: (field: number) => DirectPurposeFieldPlan;
}) => {
  const carriage = new Map<number, readonly UTxO[]>();
  const measurements: {
    field: number;
    tier: string;
    measurement: CompleteSignedTransactionMeasurement;
  }[] = [];
  const ownOutputs = createOwnOutputJournal();
  let certificateReference: UTxO | undefined;
  for (const field of fields) {
    const planned = plan(field).openingPlan;
    const tier = planned.plan.tier;
    if (tier === "Inline") continue;
    const published = await captureEmulatorSubmission(harness.emulator, () =>
      publishFaultProofFieldCarriage({
        ownOutputs,
        lucid: harness.proverLucid,
        signer: harness.proverSigner,
        planned,
        publisherAddress: harness.proverSigner.address,
        label: `${label} field ${field.toString()}`,
      }),
    );
    for (const measurement of published.measurements)
      measurements.push({ field, tier, measurement });
    const references = [...published.result];
    if (tier === "Certified") {
      certificateReference ??= (
        await publishPlainReferenceScriptUtxo({
          lucid: harness.proverLucid,
          script: harness.contracts.fieldPreimageCertificate.mintingScript,
          label: `${label} field certificate`,
        })
      ).utxo;
      const certified = await captureEmulatorSubmission(harness.emulator, () =>
        certifyFaultProofFieldCarriage({
          ownOutputs,
          lucid: harness.proverLucid,
          network,
          signer: harness.proverSigner,
          planned,
          certificatePolicyId:
            harness.contracts.fieldPreimageCertificate.policyId,
          certificateMintingScript:
            harness.contracts.fieldPreimageCertificate.mintingScript,
          certificateReferenceScriptUtxo: certificateReference!,
          chunkUtxos: references,
          compactCbor: scenario.material.bound.compact_cbor,
          witnessSetCompactCbor:
            scenario.material.material.proofSource.witnessSetCompactCbor.toString(
              "hex",
            ),
        }),
      );
      measurements.push({
        field,
        tier: "certificate",
        measurement: certified.measurement,
      });
      references.push(certified.result.certificateUtxo);
    }
    carriage.set(field, references);
  }
  return { carriage, measurements };
};
