import { deriveDaDeploymentInfo } from "midgard-node/tests/helpers/derived-da-deployment-info";

import {
  type MidgardNodeDeployment,
  parseMidgardNodeDeploymentInfo,
} from "../../src/l1/deployment.js";
import { buildDaDeploymentFixture } from "./deployment-fixture.build-da-deployment-fixture.js";

export const readDaDeploymentFixture = async (): Promise<
  Record<string, unknown>
> => {
  const fixture = (await deriveDaDeploymentInfo()) as unknown as Record<
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
