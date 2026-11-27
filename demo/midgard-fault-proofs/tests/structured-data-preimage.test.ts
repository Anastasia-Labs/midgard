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

import { proofMaterialsAddress } from "../src/funding/proof-materials-address.js";
import {
  createAuthenticatedRawDatumPreimagePrerequisitePort,
  createStructuredDataPreimageRequirement,
} from "../src/workflow/raw-datum-preimage-prerequisite.js";
import { FRAUD_PROOF_AUTHENTICATED_PUBLICATION_OBSERVER } from "../src/workflow/raw-l1-publication-observation.js";
import {
  structuredDataPublicationPlan,
  structuredDataTreeData,
} from "../src/workflow/structured-data-preimage.js";
import { network } from "./support/emulator/blueprints.js";
import { captureEmulatorSubmission } from "./support/emulator/measurement.js";
import { EMULATOR_PROTOCOL_PARAMETERS } from "./support/emulator/protocol-parameters.js";
import { TEST_PROOF_MATERIALS_HOLDER } from "./support/proof-materials-holder.js";

it("publishes bounded structured Data and re-admits every exact output after restart", async () => {
  const account = generateEmulatorAccount({ lovelace: 5_000_000_000n });
  const emulator = new Emulator([account], EMULATOR_PROTOCOL_PARAMETERS);
  const lucid = await Lucid(emulator, network);
  const signer = {
    source: "structured-evidence-test",
    address: account.address,
    paymentKeyHash: getAddressDetails(account.address).paymentCredential!.hash,
    proofMaterialsAddress: proofMaterialsAddress(
      network,
      getAddressDetails(account.address).paymentCredential!.hash,
    ),
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
      holder: TEST_PROOF_MATERIALS_HOLDER,
      headerHash,
      baseAction: action,
      artifact: {},
    });
    if (inspected.kind !== "required")
      throw new Error("missing structured publication action");
    const captured = await port().capture({
      holder: TEST_PROOF_MATERIALS_HOLDER,
      headerHash,
      action: inspected.action,
      artifact: {},
    });
    const sent = await captureEmulatorSubmission(emulator, () =>
      captured.transaction.signed.submit(),
    );
    emulator.awaitBlock();
    expect(sent.measurement.completeSignedBytes).toBeLessThanOrEqual(15872);
  }
  // A restarted port finds every publication by content and builds none.
  expect(
    (
      await port().inspect({
        holder: TEST_PROOF_MATERIALS_HOLDER,
        headerHash,
        baseAction: action,
        artifact: {},
      })
    ).kind,
  ).toBe("satisfied");
  const resolved = await port().resolveAuthenticated({
    holder: TEST_PROOF_MATERIALS_HOLDER,
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
    publications: {
      observerVersion: FRAUD_PROOF_AUTHENTICATED_PUBLICATION_OBSERVER,
      observeExact: async () => ({ kind: "not_found" }),
    },
  });
  await expect(
    altered.inspect({
      holder: TEST_PROOF_MATERIALS_HOLDER,
      headerHash,
      baseAction: action,
      artifact: {},
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

it("encodes oversized map entries as the Aiken List<Pair> wire map", () => {
  const tree = structuredDataTreeData(
    {
      kind: "map",
      items: [
        [
          { kind: "reference", index: 0 },
          { kind: "reference", index: 1 },
        ],
      ],
    },
    BigInt,
  );
  expect(Data.to(tree)).toBe("d87c9fbfd8799f00ffd8799f01ffffff");
  const plan = structuredDataPublicationPlan(
    aikenSerialisedPlutusDataCborPreservingMapOrder(
      Data.to(new Map([["ab", "cd".repeat(16384)]])),
    ),
  );
  expect(plan.tree.kind).toBe("map_parts");
  expect(plan.publicationDatums.every((datum) => datum.length <= 28000)).toBe(
    true,
  );
});
