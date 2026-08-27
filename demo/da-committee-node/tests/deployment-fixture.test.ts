import { readFile } from "node:fs/promises";

import {
  DEPLOYMENT_MANIFEST_V1_CONTRACT_NAMES,
  DEPLOYMENT_MANIFEST_V1_REFERENCE_SCRIPT_CONTRACT_BY_ROLE,
  verifyFinalizedDeploymentManifestV1,
} from "@al-ft/midgard-core/deployment-manifest-identity-v1";
import { describe, expect, it } from "vitest";

import {
  buildDaDeploymentFixture,
  readDaDeploymentFixture,
} from "./helpers/deployment-fixture.js";

const FIXTURE_URL = new URL(
  "./fixtures/da-contract-deployment-info.json",
  import.meta.url,
);

const readRawFixture = async (): Promise<Record<string, unknown>> =>
  JSON.parse(await readFile(FIXTURE_URL, "utf8")) as Record<string, unknown>;

describe("DA deployment fixture", () => {
  it("contains exactly every canonical V1 contract record", async () => {
    const fixture = await readRawFixture();
    const contracts = fixture.contracts as Record<string, unknown>;

    expect(Object.keys(contracts).sort()).toEqual(
      [...DEPLOYMENT_MANIFEST_V1_CONTRACT_NAMES].sort(),
    );
  });

  it("rejects a named missing contract before construction", async () => {
    const fixture = await readRawFixture();
    const contracts = {
      ...(fixture.contracts as Record<string, unknown>),
    };
    delete contracts.payoutMint;

    await expect(
      buildDaDeploymentFixture({ ...fixture, contracts }),
    ).rejects.toThrow(/payoutMint/u);
  });

  it("derives each manifest contract from its named source record", async () => {
    const fixture = await readRawFixture();
    const built = await buildDaDeploymentFixture(fixture);
    const sourceContracts = fixture.contracts as Record<string, unknown>;
    const manifestContracts = built.contracts as Record<string, unknown>;

    for (const contractName of DEPLOYMENT_MANIFEST_V1_CONTRACT_NAMES) {
      const source = sourceContracts[contractName] as Record<string, unknown>;
      const manifest = manifestContracts[contractName] as Record<
        string,
        unknown
      >;

      expect({
        refScriptUTxO: manifest.refScriptUTxO,
        contract: manifest.contract,
        scriptHash: manifest.scriptHash,
      }).toEqual(source);
    }
  });

  it("pins the appended fraud-proof category identities to their first-step contracts", async () => {
    const manifest = await readDaDeploymentFixture();
    const contracts = manifest.contracts as Record<
      string,
      Record<string, unknown>
    >;
    const catalogue = contracts.fraudProofCatalogueMint!
      .fraudProofCatalogue as {
      readonly categories: Readonly<
        Record<
          string,
          { readonly categoryId: string; readonly scriptHash: string }
        >
      >;
    };
    const appendedCategories = [
      ["fabricatedDeposit", "0000000b", "fraudProofFabricatedDeposit"],
      ["fabricatedWithdrawal", "0000000c", "fraudProofFabricatedWithdrawal"],
      ["nativeScriptDecoding", "0000000d", "fraudProofNativeScriptDecoding"],
      ["missingSignature", "0000000e", "fraudProofMissingSignature"],
      ["missingNativeScriptTx", "0000000f", "fraudProofMissingNativeScriptTx"],
      [
        "withdrawnReferenceInput",
        "00000010",
        "fraudProofWithdrawnReferenceInput",
      ],
      ["canonicalDecodability", "00000011", "fraudProofCanonicalDecodability"],
      ["committedFieldShape", "00000012", "fraudProofCommittedFieldShape"],
      ["minFee", "00000013", "fraudProofMinFee"],
      ["withdrawalMistag", "00000014", "fraudProofWithdrawalMistag"],
      ["doubleWithdraw", "00000015", "fraudProofDoubleWithdraw"],
      [
        "crossBlockDuplicateEvent",
        "00000016",
        "fraudProofCrossBlockDuplicateEvent",
      ],
      ["l2TxMistag", "00000017", "fraudProofL2TxMistag"],
      ["withdrawnInput", "00000018", "fraudProofWithdrawnInput"],
    ] as const;

    for (const [categoryName, categoryId, contractName] of appendedCategories) {
      expect(catalogue.categories[categoryName]).toMatchObject({
        categoryId,
        scriptHash: contracts[contractName]!.scriptHash,
      });
    }
  });

  it("gives every canonical reference-script role a source UTxO", async () => {
    const fixture = await readRawFixture();
    const contracts = fixture.contracts as Record<
      string,
      Record<string, unknown>
    >;

    for (const contractName of Object.values(
      DEPLOYMENT_MANIFEST_V1_REFERENCE_SCRIPT_CONTRACT_BY_ROLE,
    )) {
      expect(contracts[contractName]!.refScriptUTxO).toMatchObject({
        txHash: expect.stringMatching(/^[0-9a-f]{64}$/u),
        outputIndex: expect.any(Number),
      });
    }
  });

  it("produces a finalized manifest that passes canonical verification", async () => {
    const manifest = await readDaDeploymentFixture();
    expect(() => verifyFinalizedDeploymentManifestV1(manifest)).not.toThrow();
  });
});
