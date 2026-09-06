import { aikenSerialisedPlutusDataCborPreservingMapOrder } from "@al-ft/midgard-core/plutus-data-cbor";
import {
  Constr,
  Data,
  Emulator,
  generateEmulatorAccount,
  getAddressDetails,
  Lucid,
} from "@lucid-evolution/lucid";
import { expect, it } from "vitest";

import {
  createAuthenticatedRawDatumPreimagePrerequisitePort,
  createStructuredDataPreimageRequirement,
} from "../src/workflow/raw-datum-preimage-prerequisite.js";
import { FRAUD_PROOF_AUTHENTICATED_PUBLICATION_OBSERVER } from "../src/workflow/raw-l1-publication-observation.js";
import { structuredDataPublicationPlan } from "../src/workflow/structured-data-preimage.js";
import {
  captureEmulatorSubmission,
  EMULATOR_PROTOCOL_PARAMETERS,
  network,
} from "./support/submit-init-emulator-shared.js";

it("journals bounded structured Data publications and re-admits every exact output after restart", async () => {
  const account = generateEmulatorAccount({ lovelace: 5_000_000_000n });
  const emulator = new Emulator([account], EMULATOR_PROTOCOL_PARAMETERS);
  const lucid = await Lucid(emulator, network);
  const signer = {
    source: "structured-evidence-test",
    address: account.address,
    paymentKeyHash: getAddressDetails(account.address).paymentCredential!.hash,
    selectWallet: (target: typeof lucid) =>
      target.selectWallet.fromSeed(account.seedPhrase),
  };
  signer.selectWallet(lucid);
  const value = new Constr(7, [
    new Map(
      Array.from({ length: 1304 }, (_, i) => [
        i.toString(16).padStart(4, "0"),
        1n,
      ]),
    ),
    "ab".repeat(16384),
    Array.from({ length: 192 }, () => "cd".repeat(64)),
  ]);
  const preimageHex = aikenSerialisedPlutusDataCborPreservingMapOrder(
    Data.to(value),
  );
  const requirement = createStructuredDataPreimageRequirement({ preimageHex });
  expect(requirement.kind).toBe("structured_data_preimage");
  expect(
    requirement.publicationDatums.every((datum) => datum.length <= 28000),
  ).toBe(true);
  const action = {
    actionId: "structured-evidence-step",
    input: { category: "withdrawalMistag", stage: "step_03" },
  };
  const headerHash = "ab".repeat(28);
  const port = () =>
    createAuthenticatedRawDatumPreimagePrerequisitePort({
      category: "withdrawalMistag",
      lucid,
      network,
      signer,
      requirementForAction: () => requirement,
      transactionConfirmed: async () => false,
      publications: {
        observerVersion: FRAUD_PROOF_AUTHENTICATED_PUBLICATION_OBSERVER,
        observeExact: async (request) => {
          const output = (await lucid.utxosAt(request.address)).find(
            (utxo) =>
              `${utxo.txHash}#${utxo.outputIndex}` === request.expectedOutRef &&
              utxo.datum === request.expectedDatumCbor &&
              utxo.scriptRef == null,
          );
          return output === undefined
            ? { kind: "not_found" }
            : { kind: "confirmed", outRef: request.expectedOutRef };
        },
      },
    });
  for (let i = 0; i < requirement.publicationDatums.length; i++) {
    const inspected = await port().inspect({
      headerHash,
      baseAction: action,
      artifact: {},
      entries: [],
    });
    if (inspected.kind !== "required")
      throw new Error("missing structured publication action");
    const captured = await port().capture({
      headerHash,
      action: inspected.action,
      artifact: {},
    });
    const recovery = JSON.parse(JSON.stringify(captured.durableRecovery));
    const sent = await captureEmulatorSubmission(emulator, () =>
      captured.transaction.signed.submit(),
    );
    emulator.awaitBlock();
    expect(sent.measurement.completeSignedBytes).toBeLessThanOrEqual(15872);
    expect(
      await port().reconcile({
        headerHash,
        action: inspected.action,
        artifact: {},
        txHash: captured.transaction.txHash,
        durableRecovery: recovery,
      }),
    ).toEqual({ kind: "confirmed", txHash: captured.transaction.txHash });
  }
  const resolved = await port().resolveAuthenticated({
    headerHash,
    action,
    artifact: {},
  });
  expect(resolved.publications.map((output) => output.datum)).toEqual(
    requirement.publicationDatums,
  );
  expect(() => structuredDataPublicationPlan(`${preimageHex}00`)).toThrow();
  const changed = {
    ...requirement,
    publicationDatums: [...requirement.publicationDatums].reverse(),
  };
  const altered = createAuthenticatedRawDatumPreimagePrerequisitePort({
    category: "withdrawalMistag",
    lucid,
    network,
    signer,
    requirementForAction: () => changed,
    transactionConfirmed: async () => false,
    publications: {
      observerVersion: FRAUD_PROOF_AUTHENTICATED_PUBLICATION_OBSERVER,
      observeExact: async () => ({ kind: "not_found" }),
    },
  });
  await expect(
    altered.inspect({
      headerHash,
      baseAction: action,
      artifact: {},
      entries: [],
    }),
  ).rejects.toThrow("publication identities changed");
});

it("groups wide constructor fields into bounded publications", () => {
  const value = new Constr(
    11,
    Array.from({ length: 20000 }, (_, i) => BigInt(i)),
  );
  const preimageHex = aikenSerialisedPlutusDataCborPreservingMapOrder(
    Data.to(value),
  );
  const plan = structuredDataPublicationPlan(preimageHex);
  expect(plan.tree.kind).toBe("constructor");
  expect(plan.publicationDatums.length).toBeLessThanOrEqual(8);
  expect(plan.publicationDatums.every((datum) => datum.length <= 28000)).toBe(
    true,
  );
});
