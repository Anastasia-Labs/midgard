import { expect } from "vitest";

import {
  certifyFaultProofFieldCarriage,
  planFaultProofFieldOpening,
  publishFaultProofFieldCarriage,
} from "../src/field-opening.js";
import { admitNativeScriptInvalidForcedArtifact } from "../src/native-script-invalid/forced-artifact.js";
import { submitNativeScriptInvalidCancel } from "../src/native-script-invalid/submit-cancel.js";
import { submitNativeScriptInvalidStep02 } from "../src/native-script-invalid/submit-step-02.js";
import { submitNativeScriptInvalidStep03 } from "../src/native-script-invalid/submit-step-03.js";
import { submitNativeScriptInvalidStep03StartSignerScan } from "../src/native-script-invalid/submit-step-03-staged.js";
import { submitNativeScriptInvalidStep04 } from "../src/native-script-invalid/submit-step-04.js";
import { submitNativeScriptInvalidStep05 } from "../src/native-script-invalid/submit-step-05.js";
import { submitRemoveFraudulentBlock } from "../src/remove-fraudulent-block.js";
import { setup } from "./native-script-invalid-wrongful-rejection-lifecycle.setup.js";
import {
  buildRemovalDeploymentInfo,
  network,
  publishPlainReferenceScriptUtxo,
  publishRemovalReferenceScripts,
} from "./support/submit-init-emulator-shared.js";

export const run = async (
  f: Awaited<ReturnType<typeof setup>>,
  unsafeSkipLocalViolationCheckForTest = false,
  cancelAt?: "grammar" | "signer",
): Promise<void> => {
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
    await submitNativeScriptInvalidCancel({
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
  const prepareField = async (
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
    const chunks = await publishFaultProofFieldCarriage({
      lucid: f.h.proverLucid,
      signer: f.h.proverSigner,
      planned,
      publisherAddress: f.h.proverSigner.address,
      label: "native-script field",
    });
    if (planned.plan.tier !== "Certified")
      return { publishedCarriageUtxos: chunks };
    const ref = await publishPlainReferenceScriptUtxo({
      lucid: f.h.funderLucid,
      script: f.h.contracts.fieldPreimageCertificate.mintingScript,
      label: "native-script field certificate",
    });
    const certified = await certifyFaultProofFieldCarriage({
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
      witnessSetCompactCbor:
        f.proofSource.witnessSetCompactCbor.toString("hex"),
    });
    return {
      publishedCarriageUtxos: chunks,
      certificateUtxo: certified.certificateUtxo,
    };
  };
  const scriptCarriage = await prepareField(6, f.scripts);
  const signerCarriage = await prepareField(
    7,
    f.witnesses.map((w) => w.item),
  );
  for (let i = 0; i < 1000; i++) {
    const next = await submitNativeScriptInvalidStep02({
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
    proof = await submitNativeScriptInvalidStep03({
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
    const started = await submitNativeScriptInvalidStep03StartSignerScan({
      ...f.common,
      ...signerCarriage,
      threadOutRef,
      scriptItemCbor: f.scriptItem,
      addressWitnessItems: f.witnesses.map((w) => w.item),
      referenceScriptUtxo: f.refs[2]!,
    });
    threadOutRef = started.nextThreadOutRef;
    if (cancelAt === "signer") {
      await cancel(threadOutRef, 3);
      await run(f);
      return;
    }
    for (let i = 0; i < 1000; i++) {
      const advanced = await submitNativeScriptInvalidStep04({
        ...f.common,
        ...signerCarriage,
        threadOutRef,
        addressWitnessItems: f.witnesses.map((w) => w.item),
        referenceScriptUtxo: f.refs[3]!,
      });
      threadOutRef = advanced.nextThreadOutRef;
      if (advanced.action === "FinalizeSignerScan") break;
    }
    for (let i = 0; i < 1000; i++) {
      const advanced = await submitNativeScriptInvalidStep05({
        ...f.common,
        unsafeSkipLocalViolationCheckForTest,
        threadOutRef,
        scriptItemCbor: f.scriptItem,
        addressWitnessItems: f.witnesses.map((w) => w.item),
        referenceScriptUtxo: f.refs[4]!,
        witnessReferenceScripts: f.h.witnessReferenceScripts,
      });
      if ("fraudProofUnit" in advanced) {
        proof = advanced;
        break;
      }
      threadOutRef = advanced.nextThreadOutRef;
    }
    if (proof === undefined)
      throw new Error("script lifecycle did not finalize");
  }
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
      proof.fraudProofUnit,
    ),
  ).toHaveLength(1);
};
