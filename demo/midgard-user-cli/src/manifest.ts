/**
 * The deployment a submission targets: a manifest whose identity checks
 * (canonical profile, economics, network slot mapping, manifest id), whose
 * user-history contracts come from its own script CBOR, and whose network and
 * slot mapping pick the L1 the tool talks to.
 */
import { readFile } from "node:fs/promises";

import {
  type DeploymentManifest,
  verifyDeploymentManifestIdentity,
} from "@al-ft/midgard-core/deployment-manifest-identity";
import {
  type L1SlotConfig,
  parseL1SlotConfig,
} from "@al-ft/midgard-core/l1-slot-config";
import {
  type UserHistoryContracts,
  userHistoryContractsFromDeploymentManifest,
  type UserHistoryReferenceScripts,
} from "@al-ft/midgard-sdk";
import { type Network } from "@lucid-evolution/lucid";

import { refuse } from "./options.js";

export type UserDeployment = {
  readonly manifestId: string;
  readonly network: Network;
  readonly slotConfig: L1SlotConfig;
  readonly contracts: UserHistoryContracts;
  readonly referenceScripts: UserHistoryReferenceScripts;
};

const NETWORKS: readonly Network[] = [
  "Mainnet",
  "Preprod",
  "Preview",
  "Custom",
];

const errorText = (cause: unknown) =>
  cause instanceof Error ? cause.message : String(cause);

export const loadUserDeployment = async (
  path: string,
): Promise<UserDeployment> => {
  let value: unknown;
  try {
    value = JSON.parse(await readFile(path, "utf8"));
  } catch (cause) {
    return refuse(
      `Cannot read the deployment manifest ${path}: ${errorText(cause)}`,
    );
  }
  try {
    const manifest = verifyDeploymentManifestIdentity(
      value,
    ) as DeploymentManifest;
    const network = manifest.network as Network;
    if (!NETWORKS.includes(network))
      throw new Error(`unknown network ${JSON.stringify(manifest.network)}`);
    const { contracts, referenceScripts } =
      userHistoryContractsFromDeploymentManifest(manifest, {
        sourcePath: path,
      });
    return {
      manifestId: manifest.manifestId,
      network,
      slotConfig: parseL1SlotConfig(
        (value as { l1SlotConfig?: unknown }).l1SlotConfig,
        "Deployment manifest l1SlotConfig",
      ),
      contracts,
      referenceScripts,
    };
  } catch (cause) {
    return refuse(
      `The deployment manifest ${path} is not usable: ${errorText(cause)}`,
    );
  }
};
