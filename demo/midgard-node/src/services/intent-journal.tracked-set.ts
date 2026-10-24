/**
 * The addresses and credentials the node's follower tracks for the intent
 * journal's invariant (plan §8.2): the node's own wallets, the
 * reference-script addresses, and the protocol validators' payment
 * credentials.
 */
import type * as SDK from "@al-ft/midgard-sdk";
import { credentialToAddress, getAddressDetails } from "@lucid-evolution/lucid";

import {
  type NodeConfigDep,
  seedPhraseAddress,
} from "./config.node-config-dep.js";

const addressBytes = (bech32: string): Buffer =>
  Buffer.from(getAddressDetails(bech32).address.hex, "hex");

const distinct = (addresses: readonly Buffer[]): Buffer[] => [
  ...new Map(addresses.map((a) => [a.toString("hex"), a])).values(),
];

/**
 * The defense pocket: the Enterprise address of the operator's payment key.
 * The node's wallet is the key's Base address, while a validation-trace
 * thread's leftover and expiry pay the key's Enterprise address; the pocket
 * is tracked as an own wallet so that value stays in the node's view.
 */
export const defensePocketAddress = (
  config: Pick<NodeConfigDep, "NETWORK" | "L1_OPERATOR_SEED_PHRASE">,
): string => {
  const { paymentCredential } = getAddressDetails(
    seedPhraseAddress(config.L1_OPERATOR_SEED_PHRASE, config.NETWORK),
  );
  if (paymentCredential === undefined)
    throw new Error("the operator seed's address has no payment credential");
  return credentialToAddress(config.NETWORK, paymentCredential);
};

/**
 * The node's own wallets: every seed it signs with (operator, merge,
 * settlement, reference scripts), and the operator's defense pocket.
 */
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
      .map((seed) => seedPhraseAddress(seed, config.NETWORK))
      .concat(
        config.L1_OPERATOR_SEED_PHRASE.trim() === ""
          ? []
          : [defensePocketAddress(config)],
      )
      .map(addressBytes),
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
  "fraudProofCatalogue",
  "fraudProof",
  "deposit",
  "withdrawal",
  "txOrder",
  "settlement",
  "reserve",
  "payout",
] as const satisfies readonly (keyof SDK.MidgardValidators)[];

/** The validation-trace dispute's holders a defending operator follows:
 * its cases and the threads it defends, wherever they sit. */
const VALIDATION_TRACE_DISPUTE_HOLDERS = [
  "case",
  "game",
  "timeout",
  "boundary",
  "award",
] as const;

/**
 * The payment credentials of the protocol addresses (§8.2: protocol
 * addresses are always tracked), the validation-trace dispute's holders
 * among them, and the case configuration's address, which a defense reads.
 * Every output there is created after the protocol-init transaction, which
 * is after the follower's origin.
 */
export const protocolPaymentCredentials = (
  contracts: Readonly<
    Record<
      (typeof PROTOCOL_SPENDING_VALIDATORS)[number],
      Readonly<{ spendingScriptHash: string }>
    >
  > & {
    readonly fraudProofContracts: {
      readonly validationTraceDispute: Readonly<
        Record<
          (typeof VALIDATION_TRACE_DISPUTE_HOLDERS)[number],
          Readonly<{ spendingScriptHash: string }>
        >
      > & { readonly caseConfig: Readonly<{ policyId: string }> };
    };
  },
): string[] => {
  const dispute = contracts.fraudProofContracts.validationTraceDispute;
  return [
    ...new Set(
      [
        ...PROTOCOL_SPENDING_VALIDATORS.map((name) => contracts[name]),
        ...VALIDATION_TRACE_DISPUTE_HOLDERS.map((name) => dispute[name]),
      ]
        .map(({ spendingScriptHash }) => spendingScriptHash)
        // The case configuration's address is its minting policy's own.
        .concat(dispute.caseConfig.policyId)
        .map((hash) => hash.toLowerCase()),
    ),
  ];
};
