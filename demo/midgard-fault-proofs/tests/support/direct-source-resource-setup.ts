import { computeHash28 } from "@al-ft/midgard-core";
import * as SDK from "@al-ft/midgard-sdk";
import {
  Data,
  Emulator,
  generateEmulatorAccount,
  getAddressDetails,
  Lucid,
  type UTxO,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { expect } from "vitest";

import { createDirectFieldPlan } from "../../src/direct-script-derivation/index.js";
import {
  directCarriagePublisherFunding,
  publishDirectReceiveFieldCarriage,
} from "./direct-receive-field-carriage.js";
import type { DirectSourceResourceFixture } from "./direct-source-resource-fixture.js";
import {
  captureEmulatorSubmission,
  EMULATOR_PROTOCOL_PARAMETERS,
  network,
  publishPlainReferenceScriptUtxo,
  readBlueprint,
  realBlueprintPath,
} from "./submit-init-emulator-shared.js";

/** A fresh emulator holding the seeded source thread at the start script, the
 * four source scripts published as references, and the field-0 carriage. The
 * owner is the thread's prover: it seeds the thread and signs every
 * continuation, which the shared `continue` requires. */
export const setupDirectSourceResourceThread = async (
  fixture: DirectSourceResourceFixture,
) => {
  const { material } = fixture;
  const blueprint = readBlueprint(realBlueprintPath);
  const owner = generateEmulatorAccount({ lovelace: 40_000_000_000n });
  const prover = getAddressDetails(owner.address).paymentCredential!.hash;
  const policy = "cc".repeat(28);
  const certificate = SDK.makeMintingPolicy(
    SDK.getUnappliedScript(
      SDK.parseFaultProofBlueprint(blueprint),
      "field_preimage_certificate.field_preimage_certificate.mint",
    ),
  );
  const roles = await Effect.runPromise(
    SDK.buildDirectSourceContracts({
      blueprint,
      network,
      computationThreadPolicyId: policy,
      fieldPreimageCertificatePolicyId: certificate.policyId,
      nextStepScriptHash: "ee".repeat(28),
    }),
  );
  const unit =
    policy +
    "0000002b" +
    computeHash28(SDK.encodeHeaderCbor(material.sourceBound.header)).toString(
      "hex",
    );
  const initialDatum = Data.to(
    { fraud_prover: prover, data: material.bound },
    SDK.DirectScriptBoundDatum,
  );
  const assets = { lovelace: 30_000_000n, [unit]: 1n };
  const emulator = new Emulator(
    [
      owner,
      ...directCarriagePublisherFunding(owner),
      {
        ...owner,
        address: roles.start.spendingScriptAddress,
        assets,
        outputData: { inline: initialDatum },
      },
    ],
    EMULATOR_PROTOCOL_PARAMETERS,
  );
  const ownerLucid = await Lucid(emulator, network);
  ownerLucid.selectWallet.fromSeed(owner.seedPhrase);
  const references = new Map<string, UTxO>();
  const publications: {
    role: string;
    bytes: number;
    memory: string;
    steps: string;
    signedCbors: readonly string[];
    utxo: UTxO;
  }[] = [];
  for (const [name, role] of Object.entries({
    start: roles.start,
    resolved: roles.resolved,
    authenticate: roles.authenticate,
    map: roles.map,
  })) {
    const captured = await captureEmulatorSubmission(emulator, () =>
      publishPlainReferenceScriptUtxo({
        lucid: ownerLucid,
        script: role.spendingScript,
        label: `N6 F2 ${name}`,
      }),
    );
    const published = captured.result;
    references.set(role.spendingScriptHash, published.utxo);
    publications.push({
      role: name,
      bytes: published.publicationMeasurement.completeSignedBytes,
      memory: published.publicationMeasurement.executionMemory.toString(),
      steps: published.publicationMeasurement.executionSteps.toString(),
      signedCbors: captured.transactionCbors,
      utxo: published.utxo,
    });
  }
  const plan = createDirectFieldPlan(material, 0, prover);
  const publishCarriage = () =>
    publishDirectReceiveFieldCarriage({
      lucid: ownerLucid,
      emulator,
      account: owner,
      prover,
      plan: plan.openingPlan,
      certificate,
      compactCbor: material.bound.compact_cbor,
      witnessSetCompactCbor:
        material.material.proofSource.witnessSetCompactCbor.toString("hex"),
      walletAddressType: "Base",
    });
  const capturedCarriage =
    plan.openingPlan.plan.tier === "Inline"
      ? { result: await publishCarriage(), transactionCbors: [] }
      : await captureEmulatorSubmission(emulator, publishCarriage);
  const carriage = new Map([[0, capturedCarriage.result]]);
  const thread = (
    await ownerLucid.utxosAt(roles.start.spendingScriptAddress)
  ).find((item) => item.assets[unit] === 1n)!;
  expect(thread.datum).toBe(initialDatum);
  return {
    prover,
    certificate,
    roles,
    unit,
    initialDatum,
    assets,
    ownerLucid,
    references,
    publications,
    plan,
    capturedCarriage,
    carriage,
    thread,
  };
};
