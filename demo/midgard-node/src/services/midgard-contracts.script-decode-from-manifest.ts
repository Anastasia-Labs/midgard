import type { DeploymentManifest } from "@al-ft/midgard-core/deployment-manifest-identity";
import * as SDK from "@al-ft/midgard-sdk";
import type { Network } from "@lucid-evolution/lucid";

import {
  authenticatedValidatorFromManifest,
  withdrawalValidatorFromManifest,
} from "./midgard-contracts.assert-deployment-manifest-matches-config.js";

/** The script-decode validator and its class stakers, from the manifest. */
export const scriptDecodeFromManifest = (
  network: Network,
  manifest: DeploymentManifest,
  sourcePath: string,
): SDK.ScriptDecodeValidator => ({
  ...authenticatedValidatorFromManifest(
    network,
    manifest,
    sourcePath,
    "scriptDecodeSpend",
    "scriptDecodeMint",
  ),
  stakers: Object.fromEntries(
    SDK.SCRIPT_DECODE_STAKER_CLASS_NAMES.map((name) => [
      name,
      withdrawalValidatorFromManifest(
        manifest,
        sourcePath,
        SDK.SCRIPT_DECODE_STAKER_MEMBERS[name],
      ),
    ]),
  ) as SDK.ScriptDecodeStakerValidators,
});
