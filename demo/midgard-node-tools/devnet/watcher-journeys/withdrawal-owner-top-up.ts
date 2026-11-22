import { existsSync } from "node:fs";
import { mkdir, rm } from "node:fs/promises";
import { join } from "node:path";
import { setTimeout as delay } from "node:timers/promises";

import { midgardAddressFromText } from "@al-ft/midgard-core/codec";
import { deploymentManifestHubOraclePolicyId } from "@al-ft/midgard-core/deployment-manifest-identity";
import * as SDK from "@al-ft/midgard-sdk/users";
import { credentialToAddress } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import {
  buildPastProtectedPredecessor,
  ledgerClock,
} from "midgard-watcher/tests/support/history-predecessor-hold";
import {
  type PublishedDepositTraceCheckpoint,
  stagePublishedDepositTrace,
} from "midgard-watcher/tests/support/published-deposit-trace";

import { readJourneyArtifact, writeJourneyArtifact } from "./artifacts.js";
import { familyOwnerRequirement } from "./family-owner-requirement.js";
import type { JourneySuccessor } from "./fixture.js";
import {
  journeyLedgerOwnerKey,
  journeyWithdrawalQuote,
} from "./history-event-staging.js";
import { journeyDaSignerConfig, journeyNetwork } from "./journey-network.js";
import type {
  AttachedJourneySession,
  JourneySession,
} from "./journey-session.js";
import { readJourneyTiming } from "./journey-timing.js";
import { JOURNEY_ACTION_DEPTH } from "./live-context.js";
import { stackLedgerOwnerFunding } from "./stack-ledger-owner.js";
import { DECISION_POLL_MS } from "./stack-verified-predecessor.js";
import { decodeJourneyRetainedBlock } from "./staging.decode-journey-retained-block.js";
import { createJourneyVerifiedHeaders } from "./verified-headers.js";
import {
  ensureWithdrawalOwnerFunding,
  type OwnerFundingRequirement,
} from "./withdrawal-owner-funding.js";
import { stackWithdrawalOwnerFundingPorts } from "./withdrawal-owner-top-up.stack.js";

type DevnetHead = Readonly<{
  hubOraclePolicyId: string;
  block: JourneySuccessor;
}>;

const devnetTopUpDirectory = (session: JourneySession, category: string) =>
  join(session.context.runDirectory, "work/journeys/owner-top-up", category);

/**
 * Reads a devnet top-up's checkpoint when it can still resume on `head`,
 * removing its records otherwise: a top-up that completed but stopped before
 * removing them, or one stopped before its trace began.
 */
const resumableDevnetTopUp = async (directory: string, head: DevnetHead) => {
  const checkpointPath = join(directory, "honest-deposit.json");
  const checkpoint = existsSync(checkpointPath)
    ? await readJourneyArtifact<PublishedDepositTraceCheckpoint>(checkpointPath)
    : undefined;
  if (
    checkpoint !== undefined &&
    checkpoint.predecessor.header.prevHeaderHash === head.block.headerHash
  )
    return checkpoint;
  await rm(directory, { recursive: true, force: true });
  return undefined;
};

/**
 * A devnet's top-up of its ledger owner: one deposit of exactly the
 * requirement to the owner's key address, then an honest deposit trace on
 * the run's head that absorbs it. The trace commits its own deposit as well,
 * as every devnet deposit trace does. Once the watcher has verified the
 * trace's blocks, its last block becomes the run's head, with the record the
 * watcher verified it by, and the top-up's own records are removed. A
 * stopped top-up resumes its trace; one stopped after its deposit but before
 * its trace began deposits again, and the trace absorbs both.
 */
const devnetTopUp = async (
  session: JourneySession,
  input: Readonly<{
    category: string;
    head: DevnetHead;
    address: string;
    requirement: OwnerFundingRequirement;
    onStage(name: string): void;
  }>,
) => {
  const { context, ledgerAt, retain } = session;
  const { deployment, accounts } = context;
  const { onStage } = input;
  const headPath = join(context.runDirectory, "work/journeys/head.json");
  const directory = devnetTopUpDirectory(session, input.category);
  const checkpointPath = join(directory, "honest-deposit.json");
  const resume = await resumableDevnetTopUp(directory, input.head);
  await mkdir(directory, { recursive: true });
  if (resume === undefined) {
    const depositMinting = deployment.references.get("depositMint");
    if (depositMinting === undefined)
      throw new Error("Missing published deposit minting reference");
    const built = await buildPastProtectedPredecessor(
      () =>
        Effect.runPromise(
          SDK.buildUnsignedDepositTxWithMetadataProgram(
            deployment.operatorLucid,
            deployment.contracts,
            {
              l2Address: input.address,
              l2Datum: null,
              lovelace: input.requirement.topUpLovelace,
              additionalAssets: {},
              referenceScripts: { depositMinting },
            },
          ),
        ),
      ledgerClock(deployment.chain),
    );
    onStage("ledger owner top-up deposit publication");
    const signed = await built.tx.sign.withWallet().complete();
    await deployment.operatorLucid.awaitTx(await signed.submit());
  }
  const trace = await stagePublishedDepositTrace(deployment, {
    daSignerConfig: journeyDaSignerConfig(accounts),
    honest: true,
    onStage,
    resume,
    onCheckpoint: (checkpoint) =>
      writeJourneyArtifact(checkpointPath, checkpoint),
    finalityDepth: JOURNEY_ACTION_DEPTH,
    liveTail: { ledgerAt },
  });
  if (
    trace.checkpoint.predecessor.header.prevHeaderHash !==
    input.head.block.headerHash
  )
    throw new Error(
      `The ledger owner's top-up extended a tail other than the run's head ${input.head.block.headerHash}`,
    );
  await retain(trace.predecessor, trace.commits[0]!);
  await retain(trace.current, trace.commits[1]!);
  // The next family's healthy decision needs the watcher's record of its
  // predecessor, which a restarted watcher never records again.
  const running = await session.ensureWatcher();
  const verified = createJourneyVerifiedHeaders(running, directory, headPath);
  const { allowances } = await readJourneyTiming(
    context.runDirectory,
    input.category,
    { actionDepth: JOURNEY_ACTION_DEPTH },
  );
  const deadline = Date.now() + allowances.automaticDecisionMs;
  for (;;) {
    running.requireLive();
    onStage("awaiting the watcher's verification of the top-up's blocks");
    const records = await verified.collect([trace.predecessor, trace.current]);
    if (records !== undefined) {
      await writeJourneyArtifact(headPath, {
        hubOraclePolicyId: input.head.hubOraclePolicyId,
        block: { ...trace.current, commitTxHash: trace.commits[1]! },
        verified: records.at(-1),
      });
      break;
    }
    if (Date.now() > deadline)
      throw new Error(
        `The watcher did not verify the ledger owner's top-up within ${allowances.automaticDecisionMs.toString()} ms`,
      );
    await delay(DECISION_POLL_MS);
  }
  await rm(directory, { recursive: true, force: true });
};

/**
 * Before the timer of a family that withdraws or spends an output of the
 * ledger owner's (`familyOwnerRequirement`), outside its limit: tops the
 * ledger owner up when no output of the owner's on the tail the family
 * builds on meets what the family needs (`ensureWithdrawalOwnerFunding`).
 * A devnet tops up through an honest deposit trace on its head; before the
 * run's first block it has no head and tops nothing up, and only the first
 * family's deposit trace funds the owner. A stack deposits to the owner's wallet address and waits
 * for our node to absorb it and its committee to attest the block, as the
 * run's own funding does. A family that neither withdraws nor spends an
 * output of the owner's needs nothing.
 */
export const withdrawalOwnerTopUpBeforeFamily = async (
  session: JourneySession | AttachedJourneySession,
  input: Readonly<{ category: string; onStage(name: string): void }>,
) => {
  const { onStage } = input;
  const { deployment } = session.context;
  const requirementFor = (seedPhrase: string) =>
    familyOwnerRequirement(input.category, {
      ledgerOwnerSeedPhrase: seedPhrase,
      quote: journeyWithdrawalQuote({
        deployment,
        ownerKey: journeyLedgerOwnerKey(seedPhrase),
      }),
    });
  if (session.mode === "attached") {
    const requirement = requirementFor(
      session.context.accounts.ledgerOwner.seedPhrase,
    );
    if (requirement === undefined) return "spends-no-owner-output" as const;
    const { owner, ourLedger, readTail } = await stackLedgerOwnerFunding(
      session,
      input,
    );
    return ensureWithdrawalOwnerFunding(
      stackWithdrawalOwnerFundingPorts({
        owner: owner.keyHash,
        readTail,
        readOurOutputs: async () =>
          (await ourLedger.utxos(owner.address)).map((utxo) => ({
            outRef: utxo.outrefCbor,
            output: utxo.outputCbor,
          })),
        requirement: (tail) =>
          requirement(tail, midgardAddressFromText(owner.address)),
        // The run's own funding, with a deposit of the top-up's amount in
        // the family's own record.
        fundingPorts: async (lovelace) =>
          (
            await stackLedgerOwnerFunding(session, {
              ...input,
              deposit: {
                lovelace,
                pendingName: `ledger-owner-top-up.${input.category}.json`,
              },
            })
          ).ports,
        onStage,
      }),
    );
  }
  const { seedPhrase } = session.context.accounts.operator;
  const requirement = requirementFor(seedPhrase);
  if (requirement === undefined) return "spends-no-owner-output" as const;
  const owner = journeyLedgerOwnerKey(seedPhrase).to_public().hash().to_hex();
  const address = credentialToAddress(journeyNetwork().network, {
    type: "Key",
    hash: owner,
  });
  const headPath = join(
    session.context.runDirectory,
    "work/journeys/head.json",
  );
  const readHead = async () => {
    if (!existsSync(headPath)) return undefined;
    const head = await readJourneyArtifact<DevnetHead>(headPath);
    if (
      head.hubOraclePolicyId !==
      deploymentManifestHubOraclePolicyId(deployment.manifest)
    )
      throw new Error("Journey head belongs to a different deployment");
    return head;
  };
  return ensureWithdrawalOwnerFunding({
    owner,
    readTail: async () => {
      const head = await readHead();
      return head === undefined
        ? undefined
        : decodeJourneyRetainedBlock(head.block, session.ledgerAt);
    },
    requirement: (tail) => requirement(tail, midgardAddressFromText(address)),
    topUp: async (requirement) => {
      const head = await readHead();
      if (head === undefined)
        throw new Error("The ledger owner's top-up has no head to extend");
      await devnetTopUp(session, {
        category: input.category,
        head,
        address,
        requirement,
        onStage,
      });
    },
    settle: async () => {
      const head = await readHead();
      if (head !== undefined)
        await resumableDevnetTopUp(
          devnetTopUpDirectory(session, input.category),
          head,
        );
    },
    onStage,
  });
};
