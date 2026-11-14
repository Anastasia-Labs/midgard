/**
 * The deployment a submission targets: a manifest whose identity checks
 * (canonical profile, economics, network slot mapping, manifest id), whose
 * user-history contracts come from its own script CBOR, and whose network and
 * slot mapping pick the L1 the tool talks to. A forced order also reads the
 * tx-order contract.
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
  type AuthenticatedValidator,
  authenticatedValidatorFromManifest,
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

type VerifiedManifest = {
  readonly value: unknown;
  readonly manifest: DeploymentManifest;
  readonly network: Network;
};

const readVerifiedManifest = async (
  path: string,
): Promise<VerifiedManifest> => {
  let value: unknown;
  try {
    value = JSON.parse(await readFile(path, "utf8"));
  } catch (cause) {
    return refuse(
      `Cannot read the deployment manifest ${path}: ${errorText(cause)}`,
    );
  }
  return usable(path, () => {
    const manifest = verifyDeploymentManifestIdentity(
      value,
    ) as DeploymentManifest;
    const network = manifest.network as Network;
    if (!NETWORKS.includes(network))
      throw new Error(`unknown network ${JSON.stringify(manifest.network)}`);
    return { value, manifest, network };
  });
};

const usable = <T>(path: string, read: () => T): T => {
  try {
    return read();
  } catch (cause) {
    return refuse(
      `The deployment manifest ${path} is not usable: ${errorText(cause)}`,
    );
  }
};

const userDeploymentOf = (
  path: string,
  { value, manifest, network }: VerifiedManifest,
): UserDeployment =>
  usable(path, () => {
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
  });

export const loadUserDeployment = async (
  path: string,
): Promise<UserDeployment> =>
  userDeploymentOf(path, await readVerifiedManifest(path));

/** A user deployment and its tx-order contract, for forced orders. */
export type TxOrderDeployment = UserDeployment & {
  readonly txOrder: AuthenticatedValidator;
};

export const loadTxOrderDeployment = async (
  path: string,
): Promise<TxOrderDeployment> => {
  const verified = await readVerifiedManifest(path);
  const deployment = userDeploymentOf(path, verified);
  return {
    ...deployment,
    txOrder: usable(path, () =>
      authenticatedValidatorFromManifest(
        verified.network,
        verified.manifest,
        path,
        "txOrderSpend",
        "txOrderMint",
      ),
    ),
  };
};
