import { readFile, writeFile } from "node:fs/promises";

import {
  type MidgardNodeDeployment,
  parseMidgardNodeDeploymentInfo,
} from "../../src/l1/deployment.js";

const FIXTURE_URL = new URL(
  "../fixtures/da-contract-deployment-info.json",
  import.meta.url,
);

export const readDaDeploymentFixture = async (): Promise<
  Record<string, unknown>
> => JSON.parse(await readFile(FIXTURE_URL, "utf8")) as Record<string, unknown>;

export const loadDaDeploymentFixture = async (
  network: string,
): Promise<MidgardNodeDeployment> => {
  const deployment = parseMidgardNodeDeploymentInfo(
    await readDaDeploymentFixture(),
    network,
  );
  if (deployment === undefined) {
    throw new Error("DA contract deployment fixture did not parse");
  }
  return deployment;
};

export const writeDaDeploymentFixture = async (path: string): Promise<void> => {
  await writeFile(path, JSON.stringify(await readDaDeploymentFixture()));
};
