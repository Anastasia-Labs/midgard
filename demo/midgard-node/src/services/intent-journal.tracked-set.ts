/**
 * The addresses and credentials the node's follower tracks for the intent
 * journal's invariant (plan §8.2): the node's own wallets, the
 * reference-script addresses, and the protocol validators' payment
 * credentials.
 */
import type * as SDK from "@al-ft/midgard-sdk";
import { getAddressDetails } from "@lucid-evolution/lucid";

import {
  type NodeConfigDep,
  seedPhraseAddress,
} from "./config.node-config-dep.js";

const addressBytes = (bech32: string): Buffer =>
  Buffer.from(getAddressDetails(bech32).address.hex, "hex");

const distinct = (addresses: readonly Buffer[]): Buffer[] => [
  ...new Map(addresses.map((a) => [a.toString("hex"), a])).values(),
];

/** The node's own wallets: every seed it signs with (operator, merge, settlement, reference scripts). */
export const nodeOwnWallets = (
  config: Pick<
    NodeConfigDep,
    | "NETWORK"
    | "L1_OPERATOR_SEED_PHRASE"
    | "L1_OPERATOR_SEED_PHRASE_FOR_MERGE_TX"
    | "L1_SETTLEMENT_SEED_PHRASE"
    | "L1_REFERENCE_SCRIPT_SEED_PHRASE"
  >,
): Buffer[] =>
  distinct(
    [
      config.L1_OPERATOR_SEED_PHRASE,
      config.L1_OPERATOR_SEED_PHRASE_FOR_MERGE_TX,
      config.L1_SETTLEMENT_SEED_PHRASE,
      config.L1_REFERENCE_SCRIPT_SEED_PHRASE,
    ]
      .filter(
        (seed): seed is string => seed !== undefined && seed.trim() !== "",
      )
      .map((seed) => addressBytes(seedPhraseAddress(seed, config.NETWORK))),
  );

/**
 * The addresses the node's follower seeds and tracks for the journal's
 * invariant: its own wallets and the reference-script addresses every
 * family reads its scripts from (outputs there may predate the origin).
 */
export const nodeSeededAddresses = (
  config: Parameters<typeof nodeOwnWallets>[0] &
    Pick<
      NodeConfigDep,
      "L1_REFERENCE_SCRIPT_ADDRESS" | "L1_REFERENCE_SCRIPT_DEPLOY_ADDRESS"
    >,
): Buffer[] =>
  distinct([
    ...nodeOwnWallets(config),
    ...[
      config.L1_REFERENCE_SCRIPT_ADDRESS,
      config.L1_REFERENCE_SCRIPT_DEPLOY_ADDRESS,
    ]
      .filter((address) => address.trim() !== "")
      .map(addressBytes),
  ]);

/** The protocol validators whose outputs node families spend or reference. */
const PROTOCOL_SPENDING_VALIDATORS = [
  "hubOracle",
  "daParamsGovernor",
  "daAttestation",
  "daBondPool",
  "availabilityChallenge",
  "correctionLock",
  "stateQueue",
  "scheduler",
  "registeredOperators",
  "activeOperators",
  "retiredOperators",
  "escapeHatch",
  "fraudProofCatalogue",
  "fraudProof",
  "deposit",
  "withdrawal",
  "txOrder",
  "settlement",
  "reserve",
  "payout",
] as const satisfies readonly (keyof SDK.MidgardValidators)[];

/**
 * The payment credentials of the protocol addresses (§8.2: protocol
 * addresses are always tracked). Every output there is created after the
 * protocol-init transaction, which is after the follower's origin.
 */
export const protocolPaymentCredentials = (
  contracts: Pick<
    SDK.MidgardValidators,
    (typeof PROTOCOL_SPENDING_VALIDATORS)[number]
  >,
): string[] => [
  ...new Set(
    PROTOCOL_SPENDING_VALIDATORS.map((name) =>
      contracts[name].spendingScriptHash.toLowerCase(),
    ),
  ),
];
