import { existsSync } from "node:fs";
import { rm } from "node:fs/promises";
import { join } from "node:path";
import { setTimeout as delay } from "node:timers/promises";

import * as SDK from "@al-ft/midgard-sdk";
import { credentialToAddress } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import { readJourneyArtifact, writeJourneyArtifact } from "./artifacts.js";
import { ownedHistoryLedgerEntries } from "./history-cases.js";
import type { JourneyLedgerPredecessor } from "./journey-ledger.js";
import type { StackJourneyContext } from "./stack-context.js";

/** What funding the drill's L2 ledger owner reads and does. */
export type StackLedgerOwnerFundingPorts = {
  /** Whether the stack's attested queue tail holds an output of the owner's. */
  ownerHoldsOutput(): Promise<boolean>;
  /** The inclusion time of a deposit to the owner made by an earlier run. */
  readPendingDeposit(): Promise<number | undefined>;
  /** Deposit to the owner's L2 address, recording it before it is awaited. */
  deposit(): Promise<number>;
  /** Forget a recorded deposit once the owner holds an output. */
  clearPendingDeposit(): Promise<void>;
  now(): number;
  pause(): Promise<void>;
  /** How long after its inclusion time the stack absorbs and attests a deposit. */
  absorbWithinMs: number;
  onStage(name: string): void;
};

/**
 * Fund the drill's L2 ledger owner on an attached stack: while the stack's
 * attested tail holds no output of the owner's, the drill deposits to the
 * owner's L2 address once and waits for the stack's own node to absorb the
 * deposit and its committee to attest that block. A deposit an earlier run
 * made is awaited rather than repeated, and forgotten once absorbed.
 */
export const awaitStackLedgerOwnerFunded = async (
  ports: StackLedgerOwnerFundingPorts,
): Promise<void> => {
  if (await ports.ownerHoldsOutput()) return ports.clearPendingDeposit();
  const inclusionTime =
    (await ports.readPendingDeposit()) ?? (await ports.deposit());
  const deadline = inclusionTime + ports.absorbWithinMs;
  for (;;) {
    ports.onStage(
      `awaiting the stack's absorption of the ledger owner's deposit, due at ${inclusionTime.toString()}`,
    );
    await ports.pause();
    if (await ports.ownerHoldsOutput()) return ports.clearPendingDeposit();
    if (ports.now() > deadline)
      throw new Error(
        `The stack did not absorb the ledger owner's deposit, due at ${inclusionTime.toString()}, within ${ports.absorbWithinMs.toString()} ms`,
      );
  }
};

/** Polls the stack's tail no faster than this while the deposit is pending. */
const LEDGER_OWNER_POLL_MS = 30_000;

/**
 * The drill's own L2 ledger owner funding on an attached stack: its deposit
 * of the stack journey's deposit amount is paid by the drill's funding wallet.
 * The stack's node absorbs it in its own shift, which may follow operator-2's
 * whole shift, and its committee attests the block within its timeout.
 */
export const fundStackLedgerOwner = (
  context: StackJourneyContext,
  input: {
    readAttestedTail(): Promise<JourneyLedgerPredecessor>;
    onStage(name: string): void;
  },
) => {
  const { contracts, chain, references, publisherLucid } = context.deployment;
  const owner = context.layout.wallets.ledgerOwner.keyHash;
  const fingerprint = context.deployment.manifest.manifestId;
  const pendingPath = join(
    context.runDirectory,
    "work/journeys/ledger-owner-deposit.json",
  );
  return awaitStackLedgerOwnerFunded({
    ownerHoldsOutput: async () =>
      ownedHistoryLedgerEntries(await input.readAttestedTail(), owner).length >
      0,
    readPendingDeposit: async () => {
      if (!existsSync(pendingPath)) return undefined;
      const pending = await readJourneyArtifact<{
        deploymentFingerprint: string;
        inclusionTime: number;
      }>(pendingPath);
      if (pending.deploymentFingerprint !== fingerprint)
        throw new Error(
          "The ledger owner's pending deposit belongs to a different deployment",
        );
      return pending.inclusionTime;
    },
    clearPendingDeposit: () => rm(pendingPath, { force: true }),
    deposit: async () => {
      const depositMinting = references.get("depositMint");
      if (depositMinting === undefined)
        throw new Error("Missing the stack's deposit minting reference");
      const deposit = await Effect.runPromise(
        SDK.buildUnsignedDepositTxWithMetadataProgram(
          publisherLucid,
          contracts,
          {
            l2Address: credentialToAddress("Preprod", {
              type: "Key",
              hash: owner,
            }),
            l2Datum: null,
            lovelace: BigInt(context.stackConfig.journey.depositLovelace),
            additionalAssets: {},
            referenceScripts: { depositMinting },
          },
        ),
      );
      input.onStage("ledger owner deposit");
      const signed = await deposit.tx.sign.withWallet().complete();
      await writeJourneyArtifact(pendingPath, {
        deploymentFingerprint: fingerprint,
        inclusionTime: deposit.metadata.inclusionTime,
      });
      await publisherLucid.awaitTx(await signed.submit(), 500);
      return deposit.metadata.inclusionTime;
    },
    now: () => chain.now(),
    pause: () => delay(LEDGER_OWNER_POLL_MS),
    absorbWithinMs:
      2 * Number(SDK.SHIFT_DURATION_MS) + Number(SDK.DA_ATTESTATION_TIMEOUT_MS),
    onStage: input.onStage,
  });
};
