import { Effect } from "effect";

import { inspectContracts } from "./inspect-contracts.inspect-contracts.js";
import {
  DEFAULT_FAULT_PROOF_NETWORK,
  type InspectContractsFromFilesParams,
  type InspectContractsOutput,
} from "./inspect-contracts.inspect-contracts-output.js";
import { readJsonFile } from "./json-file.js";

export const inspectContractsFromFiles = async ({
  blueprintPath,
  deploymentInfoPath,
  network = DEFAULT_FAULT_PROOF_NETWORK,
}: InspectContractsFromFilesParams): Promise<InspectContractsOutput> => {
  const [blueprint, deploymentInfo] = await Promise.all([
    readJsonFile(blueprintPath),
    readJsonFile(deploymentInfoPath),
  ]);

  return await Effect.runPromise(
    inspectContracts({ blueprint, deploymentInfo, network }),
  );
};
