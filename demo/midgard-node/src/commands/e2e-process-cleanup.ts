import { resolve } from "node:path";

import {
  cleanupOwnedProcessGroupAndRecord,
  type OwnedProcessGroupCleanupResult,
} from "@/e2e/process-ownership.js";

export const cleanupOwnedProcessGroupFromEnv = async ({
  recordPath,
  runTokenEnv,
  env = process.env,
}: {
  readonly recordPath: string;
  readonly runTokenEnv: string;
  readonly env?: Readonly<NodeJS.ProcessEnv>;
}): Promise<OwnedProcessGroupCleanupResult> => {
  const runToken = env[runTokenEnv]?.trim();
  if (runToken === undefined || runToken.length === 0) {
    throw new Error(`Missing owned process-group run token env ${runTokenEnv}`);
  }
  const spec = { recordPath: resolve(recordPath), runToken };
  return cleanupOwnedProcessGroupAndRecord({ spec });
};
