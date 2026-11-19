import { expect } from "vitest";

import { planFaultProofFieldOpening } from "../src/field-opening.js";
import { admitNativeScriptInvalidForcedArtifact } from "../src/native-script-invalid/forced-artifact.js";
import { buildNativeScriptInvalidStep02 } from "../src/native-script-invalid/submit-step-02.js";
import { buildNativeScriptInvalidStep03 } from "../src/native-script-invalid/submit-step-03.js";
import { buildNativeScriptInvalidStep03StartSignerScan } from "../src/native-script-invalid/submit-step-03-staged.js";
import { buildNativeScriptInvalidStep04 } from "../src/native-script-invalid/submit-step-04.js";
import { buildNativeScriptInvalidStep05 } from "../src/native-script-invalid/submit-step-05.js";
import { submitRemoveFraudulentBlock } from "../src/remove-fraudulent-block.js";
import {
  createOwnOutputJournal,
  type OwnOutputJournal,
} from "../src/workflow/own-outputs.js";
import { setup } from "./native-script-invalid-wrongful-rejection-lifecycle.setup.js";
import { submitFamilyCancel } from "./support/linear-fault-cancel.js";
import {
  buildRemovalDeploymentInfo,
  network,
  publishPlainReferenceScriptUtxo,
  publishRemovalReferenceScripts,
} from "./support/submit-init-emulator-shared.js";
import {
  buildAndSubmitUnjournaled,
  certifyCarriageUnjournaled,
  publishCarriageUnjournaled,
  submitTurnsUnjournaled,
} from "./support/submit-unjournaled.js";

/**
 * Publishes field `fieldIndex` of the setup's forced transaction and, when
 * its tier is certified, mints the field's certificate.
 */
export const publishNativeScriptFieldCarriage = async (
  f: Awaited<ReturnType<typeof setup>>,
  ownOutputs: OwnOutputJournal,
  fieldIndex: number,
  items: readonly Uint8Array[],
) => {
  const planned = planFaultProofFieldOpening({
    anchorSourceKind: 1n,
    fieldIndex,
    anchorTxId: f.state.bad_tx_id,
    nativeTxCompactCbor: f.common.nativeTxCompactCbor,
    itemCbors: items,
    owner: f.h.proverSigner.paymentKeyHash,
    publish: true,
    witnessSet: f.common.witnessSet,
    anchorWitnessSetHash: f.state.bad_tx_witness_set_hash,
    label: "native-script maximum field",
  });
  const chunks = await publishCarriageUnjournaled({
    ownOutputs,
    lucid: f.h.proverLucid,
    signer: f.h.proverSigner,
    planned,
    publisherAddress: f.h.proverSigner.proofMaterialsAddress,
    label: "native-script field",
  });
  if (planned.plan.tier !== "Certified")
    return { publishedCarriageUtxos: chunks };
  const ref = await publishPlainReferenceScriptUtxo({
    lucid: f.h.funderLucid,
    script: f.h.contracts.fieldPreimageCertificate.mintingScript,
    label: "native-script field certificate",
  });
  const certified = await certifyCarriageUnjournaled({
    ownOutputs,
    lucid: f.h.proverLucid,
    network,
    signer: f.h.proverSigner,
    planned,
    certificatePolicyId: f.h.contracts.fieldPreimageCertificate.policyId,
    certificateMintingScript:
      f.h.contracts.fieldPreimageCertificate.mintingScript,
    certificateReferenceScriptUtxo: ref.utxo,
    chunkUtxos: chunks,
    compactCbor: f.common.nativeTxCompactCbor,
    witnessSetCompactCbor: f.proofSource.witnessSetCompactCbor.toString("hex"),
  });
  return {
    publishedCarriageUtxos: chunks,
    certificateUtxo: certified.certificateUtxo,
  };
};

/**
 * Removes the seeded fraudulent block with the minted proof and checks the
 * proof token stays at the fraud-proof address.
 */
export const removeBlockKeepingProof = async (
  f: Awaited<ReturnType<typeof setup>>,
  fraudProofUnit: string,
): Promise<void> => {
  const removal = await publishRemovalReferenceScripts({
    lucid: f.h.proverLucid,
    contracts: f.h.contracts,
  });
  const now = BigInt(f.h.emulator.now());
  await submitRemoveFraudulentBlock({
    lucid: f.h.proverLucid,
    blueprint: f.h.realBlueprint,
    deploymentInfo: buildRemovalDeploymentInfo(f.h.contracts, f.h.catalogue, {
      removalReferenceScripts: removal.published,
    }),
    network,
    signer: f.h.proverSigner,
    fraudCategory: "nativeScriptInvalid",
    fraudulentHeaderHash: f.seeded.headerHash,
    requireReferenceScripts: true,
    validFrom: now > 120000n ? now - 120000n : 0n,
    validTo: now + 300000n,
  });
  expect(
    await f.h.proverLucid.utxosAtWithUnit(
      f.h.family.fraudProof.spendingScriptAddress,
      fraudProofUnit,
    ),
  ).toHaveLength(1);
};

export const run = async (
  f: Awaited<ReturnType<typeof setup>>,
  unsafeSkipLocalViolationCheckForTest = false,
  cancelAt?: "grammar" | "signer",
): Promise<void> => {
  const ownOutputs = createOwnOutputJournal();
  const initial = await f.init();
  const reopened = unsafeSkipLocalViolationCheckForTest
    ? undefined
    : await admitNativeScriptInvalidForcedArtifact(
        JSON.parse(JSON.stringify(f.artifact)),
      );
  const bound = await f.bind(
    `${initial.txHash}#${initial.firstStepOutputIndex}`,
    reopened
      ? { state: reopened.evidence.state, forcedSource: reopened.forcedSource }
      : {},
  );
  const cancel = async (threadOutRef: string, index: number) => {
    await submitFamilyCancel("native-script-invalid", {
      ...f.common,
      threadOutRef,
      referenceScriptUtxo: f.refs[index]!,
      witnessReferenceScripts: f.h.witnessReferenceScripts,
    });
    expect(
      await f.h.proverLucid.utxosAt(
        f.h.family.fraudProof.spendingScriptAddress,
      ),
    ).toHaveLength(0);
  };
  let threadOutRef = bound.nextThreadOutRef;
  const scriptCarriage = await publishNativeScriptFieldCarriage(
    f,
    ownOutputs,
    6,
    f.scripts,
  );
  const signerCarriage = await publishNativeScriptFieldCarriage(
    f,
    ownOutputs,
    7,
    f.witnesses.map((w) => w.item),
  );
  for (let i = 0; i < 1000; i++) {
    const next = await submitTurnsUnjournaled(buildNativeScriptInvalidStep02, {
      ownOutputs,
      ...f.common,
      ...scriptCarriage,
      threadOutRef,
      scriptWitnessItems: f.scripts,
      scriptIndex: f.scriptIndex,
      referenceScriptUtxo: f.refs[1]!,
    });
    threadOutRef = next.nextThreadOutRef;
    if (cancelAt === "grammar" && next.nextStepIndex === 1) {
      await cancel(threadOutRef, 1);
      await run(f);
      return;
    }
    if (next.nextStepIndex === 2) break;
  }
  let proof;
  if (f.witnesses.length <= 28) {
    proof = await submitTurnsUnjournaled(buildNativeScriptInvalidStep03, {
      ownOutputs,
      ...f.common,
      ...signerCarriage,
      unsafeSkipLocalViolationCheckForTest,
      threadOutRef,
      scriptItemCbor: f.scriptItem,
      addressWitnessItems: f.witnesses.map((w) => w.item),
      addressWitnessVerificationKeys: f.witnesses.map((w) =>
        w.key.to_public().to_raw_bytes(),
      ),
      referenceScriptUtxo: f.refs[2]!,
      witnessReferenceScripts: f.h.witnessReferenceScripts,
    });
  } else {
    const started = await submitTurnsUnjournaled(
      buildNativeScriptInvalidStep03StartSignerScan,
      {
        ownOutputs,
        ...f.common,
        ...signerCarriage,
        threadOutRef,
        scriptItemCbor: f.scriptItem,
        addressWitnessItems: f.witnesses.map((w) => w.item),
        referenceScriptUtxo: f.refs[2]!,
      },
    );
    threadOutRef = started.nextThreadOutRef;
    if (cancelAt === "signer") {
      await cancel(threadOutRef, 3);
      await run(f);
      return;
    }
    for (let i = 0; i < 1000; i++) {
      const advanced = await submitTurnsUnjournaled(
        buildNativeScriptInvalidStep04,
        {
          ownOutputs,
          ...f.common,
          ...signerCarriage,
          threadOutRef,
          addressWitnessItems: f.witnesses.map((w) => w.item),
          referenceScriptUtxo: f.refs[3]!,
        },
      );
      threadOutRef = advanced.nextThreadOutRef;
      if (advanced.action === "FinalizeSignerScan") break;
    }
    for (let i = 0; i < 1000; i++) {
      const advanced = await buildAndSubmitUnjournaled(
        buildNativeScriptInvalidStep05,
        {
          ...f.common,
          unsafeSkipLocalViolationCheckForTest,
          threadOutRef,
          scriptItemCbor: f.scriptItem,
          addressWitnessItems: f.witnesses.map((w) => w.item),
          referenceScriptUtxo: f.refs[4]!,
          witnessReferenceScripts: f.h.witnessReferenceScripts,
        },
      );
      if ("fraudProofUnit" in advanced) {
        proof = advanced;
        break;
      }
      threadOutRef = advanced.nextThreadOutRef;
    }
    if (proof === undefined)
      throw new Error("script lifecycle did not finalize");
  }
  await removeBlockKeepingProof(f, proof.fraudProofUnit);
};
