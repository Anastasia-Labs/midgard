import { writeFile } from "node:fs/promises";
import { join } from "node:path";

import { readDaDeploymentFixture } from "./helpers/deployment-fixture.js";

export const writeDaContractDeploymentFixture = async (
  dir: string,
): Promise<string> => {
  const fixturePath = join(dir, "contract-deployment-info.with-refs.json");
  await writeFile(fixturePath, JSON.stringify(await readDaDeploymentFixture()));
  return fixturePath;
};
