import { mkdtemp } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";

import { SELECTED_DEPLOYMENT_PROFILE } from "@al-ft/midgard-core/deployment-profile";
import {
  Emulator,
  Lucid,
  type Network,
  PROTOCOL_PARAMETERS_DEFAULT,
  SLOT_CONFIG_NETWORK,
  unixTimeToEnclosingSlot,
} from "@lucid-evolution/lucid";

import { TEST_CARDANO_PROTOCOL_PARAMETERS } from "./cardano-protocol-parameters.js";
import { publishWorkflowDeploymentOnChain } from "./published-workflow-deployment.publish-workflow-deployment-on-chain.js";
import {
  createPublishedWorkflowDeploymentAccounts,
  type PublishedWorkflowDeploymentAccounts,
} from "./published-workflow-deployment.submit-published-initialization.js";
import { DEFAULT_PUBLICATION_SCHEDULE } from "./reference-publication-chain.js";

// A parameter keeps this fixture typecheckable for either compiled profile.
const isPreprodNetwork = (network: Network): boolean => network === "Preprod";

/** Emulator wrapper around the same deployment builders used by live journeys. */
export const publishWorkflowDeployment = async (
  options: Readonly<{
    accounts?: PublishedWorkflowDeploymentAccounts;
    publicationMaxTargetsPerBatch?: number;
  }> = {},
) => {
  // The manifest binding admits only the compiled profile's network.
  const network: "Custom" | "Preprod" = SELECTED_DEPLOYMENT_PROFILE.network;
  const accounts =
    options.accounts ?? createPublishedWorkflowDeploymentAccounts();
  const emulator = new Emulator([accounts.operator, accounts.publisher], {
    ...PROTOCOL_PARAMETERS_DEFAULT,
    maxTxSize: 16_384,
    maxTxExMem: 16_500_000n,
    maxTxExSteps: 10_000_000_000n,
  });
  emulator.time = 1_788_739_200_000;
  if (isPreprodNetwork(network)) {
    emulator.slot = unixTimeToEnclosingSlot(
      emulator.time,
      SLOT_CONFIG_NETWORK.Preprod,
    );
    emulator.blockHeight = Math.floor(emulator.slot / 20);
  }
  const operatorLucid = await Lucid(emulator, network);
  const publisherLucid = await Lucid(emulator, network);
  operatorLucid.selectWallet.fromSeed(accounts.operator.seedPhrase);
  publisherLucid.selectWallet.fromSeed(accounts.publisher.seedPhrase);
  const deployment = await publishWorkflowDeploymentOnChain({
    network,
    accounts,
    operatorLucid,
    publisherLucid,
    chain: {
      now: () => emulator.now(),
      delaySlots: (slots) => emulator.awaitSlot(slots),
      awaitLedgerTime: (targetUnixTimeMs) => {
        const slots = Math.ceil((targetUnixTimeMs - emulator.now()) / 1000);
        if (slots > 0) emulator.awaitSlot(slots);
      },
    },
    protocolParameters: TEST_CARDANO_PROTOCOL_PARAMETERS,
    publicationMaxTargetsPerBatch: options.publicationMaxTargetsPerBatch,
    publicationJournalPath: join(
      await mkdtemp(join(tmpdir(), "midgard-publication-")),
      "transactions.ndjson",
    ),
    publicationSynchronize: async () => emulator.slot,
    publicationSchedule: DEFAULT_PUBLICATION_SCHEDULE,
  });
  return { ...deployment, emulator };
};
