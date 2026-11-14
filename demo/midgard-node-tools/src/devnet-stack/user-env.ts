import { join } from "node:path";

import type { DeployContext } from "./deploy.js";

/**
 * What `midgard-user` reads: the run's deployment manifest and its own view
 * of L1 through the devnet's Kupo (which matches every address) and Ogmios.
 * It never reads the node's database or configuration. Its records stay
 * inside the run (`runKey`): its default is machine-wide, and submission ids
 * repeat across runs.
 */
export const userEnvironment = (
  { run, layout }: Pick<DeployContext, "run" | "layout">,
  runKey: string,
): Record<string, string> => ({
  MIDGARD_DEPLOYMENT_MANIFEST: layout.contractManifest,
  L1_ACCESS: "kupmios",
  L1_KUPO_URL: `http://127.0.0.1:${run.kupoPort}`,
  L1_OGMIOS_URL: `http://127.0.0.1:${run.ogmiosPort}`,
  MIDGARD_USER_RECORD_DIR: join(runKey, "user-submissions"),
});
