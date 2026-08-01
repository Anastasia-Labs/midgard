import { readFile } from "node:fs/promises";

import {
  DEPLOYMENT_MANIFEST_V1_CONTRACT_NAMES,
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

  it("produces a finalized manifest that passes canonical verification", async () => {
    const manifest = await readDaDeploymentFixture();
    expect(() => verifyFinalizedDeploymentManifestV1(manifest)).not.toThrow();
  });
});
