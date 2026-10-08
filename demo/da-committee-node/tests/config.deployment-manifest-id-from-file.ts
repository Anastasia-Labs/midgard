import { readFile, writeFile } from "node:fs/promises";
import { join } from "node:path";

import { computeDeploymentManifestId } from "@al-ft/midgard-core/deployment-manifest-identity";

import { readDaDeploymentFixture } from "./helpers/deployment-fixture.js";

export const withRecomputedDeploymentManifestId = (
  manifest: Record<string, unknown>,
): Record<string, unknown> => {
  const { manifestId: _manifestId, ...identityInput } = manifest;
  return {
    ...identityInput,
    manifestId: computeDeploymentManifestId(identityInput),
  };
};

export const deploymentManifestIdFromFile = async (
  path: string,
): Promise<string> => {
  const parsed = JSON.parse(await readFile(path, "utf8")) as Record<
    string,
    unknown
  >;
  if (typeof parsed.manifestId !== "string") {
    throw new Error(`${path} is missing manifestId`);
  }
  return parsed.manifestId;
};

export const writeDaContractDeploymentFixture = async (
  dir: string,
): Promise<string> => {
  const fixturePath = join(dir, "contract-deployment-info.with-refs.json");
  await writeFile(fixturePath, JSON.stringify(await readDaDeploymentFixture()));
  return fixturePath;
};
