import { readFile } from "node:fs/promises";

import {
  type MidgardNodeDeployment,
  parseMidgardNodeDeploymentInfo,
} from "../../src/l1/deployment.js";
import { FIXTURE_URL } from "./deployment-fixture.build-canonical-fraud-proof-catalogue-fixture.js";
import { buildDaDeploymentFixture } from "./deployment-fixture.build-da-deployment-fixture.js";

export const readDaDeploymentFixture = async (): Promise<
  Record<string, unknown>
> => {
  const fixture = JSON.parse(await readFile(FIXTURE_URL, "utf8")) as Record<
    string,
    unknown
  >;
  return buildDaDeploymentFixture(fixture);
};

export const loadDaDeploymentFixture = async (
  network: string,
): Promise<MidgardNodeDeployment> => {
  const deployment = parseMidgardNodeDeploymentInfo(
    await readDaDeploymentFixture(),
    network,
  );
  return deployment;
};
