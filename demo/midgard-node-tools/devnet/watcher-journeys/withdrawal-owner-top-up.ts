import { existsSync } from "node:fs";
import { mkdir, rm } from "node:fs/promises";
import { join } from "node:path";
import { setTimeout as delay } from "node:timers/promises";

import { midgardAddressFromText } from "@al-ft/midgard-core/codec";
import * as SDK from "@al-ft/midgard-sdk";
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
import {
  awaitStackLedgerOwnerFunded,
  stackLedgerOwnerFunding,
} from "./stack-ledger-owner.js";
import { DECISION_POLL_MS } from "./stack-verified-predecessor.js";
import { decodeJourneyRetainedBlock } from "./staging.decode-journey-retained-block.js";
import { createJourneyVerifiedHeaders } from "./verified-headers.js";
import {
  ensureWithdrawalOwnerFunding,
  type OwnerFundingRequirement,
  qualifyingOwnedOutput,
  withdrawalFundingShape,
  withdrawalOwnerRequirement,
} from "./withdrawal-owner-funding.js";

type DevnetHead = Readonly<{
  deploymentFingerprint: string;
  block: JourneySuccessor;
}>;

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
  const directory = join(
    context.runDirectory,
    "work/journeys/owner-top-up",
    input.category,
  );
  const checkpointPath = join(directory, "honest-deposit.json");
  const extendsHead = (checkpoint: PublishedDepositTraceCheckpoint) =>
    checkpoint.predecessor.header.prevHeaderHash ===
    input.head.block.headerHash;
  let resume = existsSync(checkpointPath)
    ? await readJourneyArtifact<PublishedDepositTraceCheckpoint>(checkpointPath)
    : undefined;
  // A top-up that completed but stopped before removing its records.
  if (resume !== undefined && !extendsHead(resume)) {
    await rm(directory, { recursive: true, force: true });
    resume = undefined;
  }
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
    timeoutCorrectionJournalPath: join(directory, "timeout-correction.json"),
    finalityDepth: JOURNEY_ACTION_DEPTH,
    liveTail: { ledgerAt },
  });
  if (!extendsHead(trace.checkpoint))
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
        deploymentFingerprint: input.head.deploymentFingerprint,
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
 * Before a withdrawal family's timer starts, outside its limit: tops the
 * ledger owner up when no output of the owner's on the tail the family
 * builds on meets what the family needs (`ensureWithdrawalOwnerFunding`).
 * A devnet tops up through an honest deposit trace on its head; before the
 * run's first block it has no head, and the first family's deposit trace
 * funds the owner. A stack deposits to the owner's wallet address and waits
 * for our node to absorb it and its committee to attest the block, as the
 * run's own funding does. A family that withdraws nothing needs nothing.
 */
export const withdrawalOwnerTopUpBeforeFamily = async (
  session: JourneySession | AttachedJourneySession,
  input: Readonly<{ category: string; onStage(name: string): void }>,
) => {
  const shape = withdrawalFundingShape(input.category);
  if (shape === undefined) return "not-withdrawal" as const;
  const { onStage } = input;
  const { deployment } = session.context;
  if (session.mode === "attached") {
    const key = journeyLedgerOwnerKey(
      session.context.accounts.ledgerOwner.seedPhrase,
    );
    const funding = await stackLedgerOwnerFunding(session, input);
    return ensureWithdrawalOwnerFunding({
      owner: funding.owner.keyHash,
      readTail: funding.readTail,
      requirement: (tail) =>
        withdrawalOwnerRequirement({
          shape,
          quote: journeyWithdrawalQuote({ deployment, ownerKey: key }),
          key,
          header: tail.header,
          topUpAddress: midgardAddressFromText(funding.owner.address),
        }),
      // The run's own funding, with a deposit of the top-up's amount and
      // its own record, waiting for an output that funds the family.
      topUp: async (requirement) => {
        const topUp = await stackLedgerOwnerFunding(session, {
          ...input,
          deposit: {
            lovelace: requirement.topUpLovelace,
            pendingName: "ledger-owner-top-up.json",
          },
        });
        const { owner, ourLedger } = topUp;
        await awaitStackLedgerOwnerFunded({
          ...topUp.ports,
          ourLedgerHoldsOutput: async () => {
            for (const utxo of await ourLedger.utxos(owner.address))
              if (
                utxo.assets.lovelace !== undefined &&
                utxo.assets.lovelace >=
                  (await requirement.requiredOf({
                    outRef: utxo.outrefCbor,
                    output: utxo.outputCbor,
                  }))
              )
                return true;
            return false;
          },
          ownerHoldsOutput: async () =>
            (await qualifyingOwnedOutput(
              await topUp.readTail(),
              owner.keyHash,
              requirement,
            )) !== undefined,
        });
      },
      onStage,
    });
  }
  const key = journeyLedgerOwnerKey(
    session.context.accounts.operator.seedPhrase,
  );
  const owner = key.to_public().hash().to_hex();
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
    if (head.deploymentFingerprint !== deployment.manifest.manifestId)
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
    requirement: (tail) =>
      withdrawalOwnerRequirement({
        shape,
        quote: journeyWithdrawalQuote({ deployment, ownerKey: key }),
        key,
        header: tail.header,
        topUpAddress: midgardAddressFromText(address),
      }),
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
    onStage,
  });
};
