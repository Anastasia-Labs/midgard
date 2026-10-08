// What the committee's follower runs on, read from the committee's
// configuration: the run plan, the tracked set and the own wallets.
import { type OriginConfig } from "@al-ft/midgard-l1-follower";
import {
  CML,
  getAddressDetails,
  type Network,
  walletFromSeed,
} from "@lucid-evolution/lucid";

import type { LoadedCommitteeConfig } from "../../config.js";
import { readL1SubmitterKeySource } from "../submitter.js";
import type { CommitteeTracked } from "./projection.js";

export type CommitteeL1FollowerPlan =
  | Readonly<{ kind: "unconfigured"; reason: string }>
  | Readonly<{
      kind: "run";
      databaseUrl: string;
      socketPath: string;
      binaryPath: string;
      networkMagic: number;
      origin: OriginConfig;
    }>;

const HEX_32 = /^[0-9a-f]{64}$/u;

const hubOracleOneShot = (
  info: Record<string, unknown>,
): OriginConfig["hubOracleOneShot"] | null => {
  const entry = info.hubOracleOneShot;
  if (typeof entry !== "object" || entry === null) return null;
  const { txHash, outputIndex } = entry as Record<string, unknown>;
  if (
    typeof txHash !== "string" ||
    !HEX_32.test(txHash) ||
    typeof outputIndex !== "number" ||
    !Number.isSafeInteger(outputIndex) ||
    outputIndex < 0
  )
    return null;
  return { txHash: Buffer.from(txHash, "hex"), index: outputIndex };
};

/**
 * How the committee's follower runs. It needs the configured origin
 * (`L1_ORIGIN`), the local node (the native ledger's socket and transport
 * binary) and the manifest's `hubOracleOneShot`. Its tables share the
 * committee store's database. Anything missing leaves the committee unready
 * with `l1_follower_not_configured`; it never fails startup.
 */
export const committeeL1FollowerPlan = (
  config: LoadedCommitteeConfig,
): CommitteeL1FollowerPlan => {
  const missing = (reason: string): CommitteeL1FollowerPlan => ({
    kind: "unconfigured",
    reason,
  });
  if (config.l1Origin === undefined) return missing("L1_ORIGIN is not set");
  if (config.nativeLedger === undefined)
    return missing("no local node ledger is configured");
  const oneShot = hubOracleOneShot(config.contractDeploymentInfo);
  if (oneShot === null)
    return missing("the deployment info has no hubOracleOneShot outref");
  return {
    kind: "run",
    databaseUrl: config.localState.url,
    socketPath: config.nativeLedger.socketPath,
    binaryPath: config.nativeLedger.binaryPath,
    networkMagic: config.cardanoL1Source.networkMagic,
    origin: {
      origin: {
        slot: config.l1Origin.slot,
        hash: Buffer.from(config.l1Origin.blockHash, "hex"),
      },
      hubOracleOneShot: oneShot,
    },
  };
};

export const addressBytes = (bech32: string): Buffer =>
  Buffer.from(getAddressDetails(bech32).address.hex, "hex");

/**
 * Everything the committee reads on L1 beside the state queue (plan §4.4,
 * committee column): the hub oracle, the DA params governor, the DA
 * attestation, the pooled DA bond, the availability challenge, the
 * correction lock and the reference-script auth policy. Script outputs are
 * tracked by their spending script's payment credential, so a staked
 * variant of the address is tracked too. The hub oracle output sits at the
 * hub policy's own script address and is never spent: tracking it keeps the
 * protocol-init tx that created it stored (R1b), which is the follower's
 * evidence that it started before protocol init (R3).
 */
export const committeeTracked = (
  config: Pick<
    LoadedCommitteeConfig,
    | "hubOraclePolicyId"
    | "daParamsGovernorAddress"
    | "daParamsGovernorPolicyId"
    | "daAttestationAddress"
    | "daAttestationPolicyId"
    | "correctionLockAddress"
    | "midgardNodeDeployment"
  >,
): CommitteeTracked => {
  const deployment = config.midgardNodeDeployment;
  return {
    addresses: [
      config.daParamsGovernorAddress,
      config.daAttestationAddress,
      config.correctionLockAddress,
    ].map(addressBytes),
    paymentCredentials: [
      config.hubOraclePolicyId,
      deployment.daParamsGovernor.spend.scriptHash,
      deployment.daAttestation.spend.scriptHash,
      deployment.daBondPool.spend.scriptHash,
      deployment.availabilityChallenge.spend.scriptHash,
    ],
    policies: [
      config.hubOraclePolicyId,
      config.daParamsGovernorPolicyId,
      config.daAttestationPolicyId,
      deployment.daBondPool.mint.scriptHash,
      deployment.availabilityChallenge.mint.scriptHash,
      deployment.referenceScriptAuthPolicyId,
    ],
  };
};

export const lucidNetwork = (network: string): Network => {
  switch (network.trim().toLowerCase()) {
    case "mainnet":
      return "Mainnet";
    case "preprod":
    case "pre-production":
    case "preproduction":
      return "Preprod";
    case "preview":
      return "Preview";
    case "custom":
      return "Custom";
    default:
      throw new Error(`unsupported Cardano network ${network}`);
  }
};

/** The address Lucid's `selectWallet` selects for `keySource`. */
export const ownWalletAddress = async (
  keySource: string,
  network: string,
): Promise<string> => {
  const credential = await readL1SubmitterKeySource(keySource);
  const lucid = lucidNetwork(network);
  if (credential.kind === "seed")
    return walletFromSeed(credential.value, {
      addressType: "Base",
      accountIndex: 0,
      network: lucid,
    }).address;
  return CML.EnterpriseAddress.new(
    lucid === "Mainnet" ? 1 : 0,
    CML.Credential.new_pub_key(
      CML.PrivateKey.from_bech32(credential.value).to_public().hash(),
    ),
  )
    .to_address()
    .to_bech32();
};

/** The committee's own wallets: the L1 submitter's and the availability submitter's. */
export const ownWallets = async (
  config: LoadedCommitteeConfig,
): Promise<Buffer[]> => {
  const sources = [
    config.l1SubmitterKeySource,
    config.availabilitySubmitterKeySource,
  ].filter((source): source is string => source !== undefined);
  const addresses = await Promise.all(
    sources.map((source) => ownWalletAddress(source, config.network)),
  );
  return [
    ...new Set(addresses.map((a) => getAddressDetails(a).address.hex)),
  ].map((hex) => Buffer.from(hex, "hex"));
};
