import { existsSync } from "node:fs";
import { mkdir, rename, rm } from "node:fs/promises";
import { dirname, join } from "node:path";
import { setTimeout as delay } from "node:timers/promises";

import * as SDK from "@al-ft/midgard-sdk";
import { credentialToAddress } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import { readJourneyArtifact, writeJourneyArtifact } from "./artifacts.js";
import { ownedHistoryLedgerEntries } from "./history-cases.js";
import type { JourneyLedgerPredecessor } from "./journey-ledger.js";
import type { StackJourneyContext } from "./stack-context.js";
import { readKupoTransactionInclusion } from "./stack-transactions.js";

/** A deposit to the drill's L2 ledger owner, recorded before it is submitted. */
export type StackLedgerOwnerDeposit = Readonly<{
  txHash: string;
  /** When the stack's node is due to absorb it (POSIX ms). */
  inclusionTime: number;
  /** The end of its validity interval (POSIX ms): it cannot land after. */
  validTo: number;
}>;

/** Where a recorded deposit stands on the stack's chain. */
export type StackLedgerOwnerDepositStatus = "landed" | "expired" | "pending";

/** What funding the drill's L2 ledger owner reads and does. */
export type StackLedgerOwnerFundingPorts = {
  /** Whether the stack's attested queue tail holds an output of the owner's. */
  ownerHoldsOutput(): Promise<boolean>;
  /** A deposit to the owner recorded by an earlier run, or by this one. */
  readPendingDeposit(): Promise<StackLedgerOwnerDeposit | undefined>;
  depositStatus(
    deposit: StackLedgerOwnerDeposit,
  ): Promise<StackLedgerOwnerDepositStatus>;
  /** Set aside a recorded deposit that can no longer land. */
  archivePendingDeposit(): Promise<void>;
  /** Deposit to the owner's L2 address, recording it before submitting it. */
  deposit(): Promise<StackLedgerOwnerDeposit>;
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
 * owner's L2 address, waits for the deposit to land on L1, then waits for the
 * stack's own node to absorb it and its committee to attest that block. A
 * deposit an earlier run recorded is awaited while it can still land, set
 * aside and replaced once its validity passed without it landing, and
 * forgotten once absorbed. A deposit this run made that expires unlanded
 * fails the run, keeping its record for the next.
 */
export const awaitStackLedgerOwnerFunded = async (
  ports: StackLedgerOwnerFundingPorts,
): Promise<void> => {
  if (await ports.ownerHoldsOutput()) return ports.clearPendingDeposit();
  let pending = await ports.readPendingDeposit();
  let ours = false;
  for (;;) {
    if (pending === undefined) {
      pending = await ports.deposit();
      ours = true;
    }
    const status = await ports.depositStatus(pending);
    if (status === "landed") break;
    if (status === "expired") {
      if (ours)
        throw new Error(
          `The ledger owner's deposit ${pending.txHash} expired without landing (valid to ${pending.validTo.toString()})`,
        );
      await ports.archivePendingDeposit();
      pending = undefined;
      continue;
    }
    ports.onStage(
      `awaiting the ledger owner's deposit ${pending.txHash} on L1, valid to ${pending.validTo.toString()}`,
    );
    await ports.pause();
  }
  const { inclusionTime } = pending;
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

/**
 * A recorded deposit's standing on the stack's chain: landed once the
 * stack's Kupo indexed an output of it, expired once Kupo indexed the slot
 * its validity ends at without it (no later block can include it), and
 * pending otherwise.
 */
export const stackLedgerOwnerDepositStatus =
  (
    input: Readonly<{
      kupoUrl: string;
      unixTimeToSlot(posixMs: number): number;
      fetchImpl?: typeof fetch;
    }>,
  ) =>
  async (
    deposit: StackLedgerOwnerDeposit,
  ): Promise<StackLedgerOwnerDepositStatus> => {
    const kupo = await readKupoTransactionInclusion(
      input.kupoUrl,
      deposit.txHash,
      input.fetchImpl,
    );
    if (kupo.included) return "landed";
    return kupo.indexedThroughSlot >= input.unixTimeToSlot(deposit.validTo)
      ? "expired"
      : "pending";
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
      const pending = await readJourneyArtifact<
        Partial<StackLedgerOwnerDeposit> & { deploymentFingerprint?: string }
      >(pendingPath);
      if (pending.deploymentFingerprint !== fingerprint)
        throw new Error(
          "The ledger owner's pending deposit belongs to a different deployment",
        );
      const { txHash, inclusionTime, validTo } = pending;
      if (
        typeof txHash !== "string" ||
        !Number.isSafeInteger(inclusionTime) ||
        !Number.isSafeInteger(validTo)
      )
        throw new Error(
          `The ledger owner's pending deposit ${pendingPath} has no transaction hash, inclusion time or validity end`,
        );
      return { txHash, inclusionTime: inclusionTime!, validTo: validTo! };
    },
    depositStatus: stackLedgerOwnerDepositStatus({
      kupoUrl: context.kupoUrl,
      unixTimeToSlot: (posixMs) => publisherLucid.unixTimeToSlot(posixMs),
    }),
    archivePendingDeposit: async () => {
      const archived = join(
        dirname(pendingPath),
        "archive",
        `ledger-owner-deposit.${Date.now().toString()}.json`,
      );
      await mkdir(dirname(archived), { recursive: true });
      await rename(pendingPath, archived);
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
      const pending: StackLedgerOwnerDeposit = {
        txHash: signed.toHash(),
        inclusionTime: deposit.metadata.inclusionTime,
        validTo: deposit.metadata.validTo,
      };
      await writeJourneyArtifact(pendingPath, {
        deploymentFingerprint: fingerprint,
        ...pending,
      });
      await signed.submit();
      return pending;
    },
    now: () => chain.now(),
    pause: () => delay(LEDGER_OWNER_POLL_MS),
    absorbWithinMs:
      2 * Number(SDK.SHIFT_DURATION_MS) + Number(SDK.DA_ATTESTATION_TIMEOUT_MS),
    onStage: input.onStage,
  });
};
