import { existsSync } from "node:fs";
import { mkdir, rename, rm } from "node:fs/promises";
import { dirname, join } from "node:path";
import { setTimeout as delay } from "node:timers/promises";

import {
  decodeMidgardTxOutput,
  encodeMidgardAddressText,
} from "@al-ft/midgard-core/codec";
import type { LedgerAt } from "@al-ft/midgard-fault-proofs";
import * as SDK from "@al-ft/midgard-sdk";
import { paymentCredentialOf } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import {
  type JourneyLedger,
  nodeLedger,
} from "../../src/full-stack/journey.js";
import { readJourneyArtifact, writeJourneyArtifact } from "./artifacts.js";
import type { JourneyLedgerPredecessor } from "./journey-ledger.js";
import { readStackJourneyTiming } from "./journey-timing.js";
import { JOURNEY_ACTION_DEPTH } from "./live-context.js";
import type { StackJourneyContext } from "./stack-context.js";
import { stackJourneyDa } from "./stack-journey-da.js";
import { stackUserEventPublicationGate } from "./stack-late-shift.js";
import { readKupoTransactionInclusion } from "./stack-transactions.js";
import { decodeJourneyRetainedBlock } from "./staging.decode-journey-retained-block.js";

/** A deposit to the drill's L2 ledger owner, recorded before it is submitted. */
export type StackLedgerOwnerDeposit = Readonly<{
  txHash: string;
  /** When the stack's node is due to absorb it (POSIX ms). */
  inclusionTime: number;
  /** The end of its validity interval (POSIX ms): it cannot land after. */
  validTo: number;
}>;

/** A deposit built and signed but not yet recorded or submitted. */
export type StackLedgerOwnerBuiltDeposit = StackLedgerOwnerDeposit &
  Readonly<{
    /** Record the deposit, then submit it. */
    submit(): Promise<void>;
  }>;

/** Where a recorded deposit stands on the stack's chain. */
export type StackLedgerOwnerDepositStatus = "landed" | "expired" | "pending";

/** What funding the drill's L2 ledger owner reads and does. */
export type StackLedgerOwnerFundingPorts = {
  /** Whether our node's own ledger holds an output of the owner's. */
  ourLedgerHoldsOutput(): Promise<boolean>;
  /** Whether the stack's attested queue tail holds an output of the owner's. */
  ownerHoldsOutput(): Promise<boolean>;
  /** A deposit to the owner recorded by an earlier run, or by this one. */
  readPendingDeposit(): Promise<StackLedgerOwnerDeposit | undefined>;
  depositStatus(
    deposit: StackLedgerOwnerDeposit,
  ): Promise<StackLedgerOwnerDepositStatus>;
  /** Set aside a recorded deposit that can no longer land. */
  archivePendingDeposit(): Promise<void>;
  /**
   * Before a deposit is built (`stackUserEventGate`'s `beforeFunding`):
   * late in another operator's shift, the shift's end, and in operator-2's
   * shift, which our node cannot absorb it in, the publication floor. It
   * fails rather than wait past `notAfterMs`.
   */
  beforeDeposit(notAfterMs: number): Promise<void>;
  /** Build and sign a deposit to the owner's L2 address. */
  buildDeposit(): Promise<StackLedgerOwnerBuiltDeposit>;
  /**
   * Whether a built deposit, by the time it actually falls due, is safe to
   * submit (`stackUserEventGate`'s `admitsFunding`).
   */
  admitsDeposit(inclusionTimeMs: number): Promise<boolean>;
  /** Forget a recorded deposit once the owner holds an output. */
  clearPendingDeposit(): Promise<void>;
  now(): number;
  pause(): Promise<void>;
  /** How long after its inclusion time the stack absorbs and attests a deposit. */
  absorbWithinMs: number;
  onStage(name: string): void;
};

/** The funding's own deadline for a deposit due at `dueMs`. */
const absorbDeadlineMs = (ports: StackLedgerOwnerFundingPorts, dueMs: number) =>
  dueMs + ports.absorbWithinMs;

/**
 * Waits until our node's ledger and the stack's attested tail both hold an
 * output of the owner's, failing once ledger time passes `dueMs` by the
 * funding's own bound.
 */
const awaitAbsorbed = async (
  ports: StackLedgerOwnerFundingPorts,
  dueMs: number,
): Promise<void> => {
  const deadline = absorbDeadlineMs(ports, dueMs);
  for (;;) {
    ports.onStage(
      `awaiting the stack's absorption of the ledger owner's deposit, due at ${dueMs.toString()}`,
    );
    await ports.pause();
    if (
      (await ports.ourLedgerHoldsOutput()) &&
      (await ports.ownerHoldsOutput())
    )
      return ports.clearPendingDeposit();
    if (ports.now() > deadline)
      throw new Error(
        `The stack did not absorb the ledger owner's deposit, due at ${dueMs.toString()}, within ${ports.absorbWithinMs.toString()} ms`,
      );
  }
};

/**
 * Builds a deposit once the gate lets it, and submits it only when its
 * actual due time is safe: one that falls due too late, because building
 * took longer than the gate foresaw, is dropped unsubmitted and built
 * again. Neither the gate nor a rebuild runs past `notAfterMs`.
 */
const submitAdmittedDeposit = async (
  ports: StackLedgerOwnerFundingPorts,
  notAfterMs: number,
): Promise<StackLedgerOwnerDeposit> => {
  for (;;) {
    await ports.beforeDeposit(notAfterMs);
    const { submit, ...deposit } = await ports.buildDeposit();
    if (await ports.admitsDeposit(deposit.inclusionTime)) {
      await submit();
      return deposit;
    }
    ports.onStage(
      `the ledger owner's deposit, built to fall due at ${deposit.inclusionTime.toString()}, falls due too late for the shift it was built in; building it again`,
    );
    if (ports.now() > notAfterMs)
      throw new Error(
        `The ledger owner's deposit could not be built to fall due safely by ${notAfterMs.toString()}`,
      );
  }
};

/**
 * Fund the drill's L2 ledger owner on an attached stack. Whether the owner
 * is funded is read from our node's own ledger, the one the run's honest
 * work spends from: while it holds no output of the owner's, the drill
 * deposits to the owner's L2 address, waits for the deposit to land on L1,
 * then waits for our node to absorb it and its committee to attest that
 * block, which the families' blocks extend. When our node already holds
 * one, nothing is deposited and only the attestation is awaited. A deposit
 * an earlier run recorded is awaited while it can still land, set aside and
 * replaced once its validity passed without it landing, and forgotten once
 * absorbed. A deposit this run made that expires unlanded fails the run,
 * keeping its record for the next. Before its deposit, it waits no longer
 * than the funding's own bound from its start; for the deposit to land, no
 * longer than the deadline its absorption is held to.
 */
export const awaitStackLedgerOwnerFunded = async (
  ports: StackLedgerOwnerFundingPorts,
): Promise<void> => {
  const notAfterMs = ports.now() + ports.absorbWithinMs;
  if (await ports.ourLedgerHoldsOutput()) {
    if (await ports.ownerHoldsOutput()) return ports.clearPendingDeposit();
    // Our node absorbed it and has yet to commit the block or have it
    // attested: within the same bound as a deposit due now.
    return awaitAbsorbed(ports, ports.now());
  }
  let pending = await ports.readPendingDeposit();
  let ours = false;
  for (;;) {
    if (pending === undefined) {
      pending = await submitAdmittedDeposit(ports, notAfterMs);
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
    // A Kupo that answers while its checkpoint stalls short of the
    // deposit's validity end shows it pending for ever. Healthy, it shows it
    // landed or expired well before this deadline, which falls an event
    // wait and the absorption bound after that end.
    const deadline = absorbDeadlineMs(ports, pending.inclusionTime);
    if (ports.now() > deadline)
      throw new Error(
        `The ledger owner's deposit ${pending.txHash} neither landed nor expired on L1, as the stack's Kupo shows it, by ${deadline.toString()}, the deadline for its absorption`,
      );
    ports.onStage(
      `awaiting the ledger owner's deposit ${pending.txHash} on L1, valid to ${pending.validTo.toString()}`,
    );
    await ports.pause();
  }
  return awaitAbsorbed(ports, pending.inclusionTime);
};

/** The ledger owner's funding failed, so the run refuses before any family. */
export class StackLedgerOwnerFundingRefusal extends Error {
  constructor(cause: unknown) {
    super(
      `The drill's ledger owner was not funded before any family started, so the run refuses: ${cause instanceof Error ? cause.message : String(cause)}`,
      { cause },
    );
    this.name = "StackLedgerOwnerFundingRefusal";
  }
}

/**
 * The run's step before any family's timer starts: funds the ledger owner
 * (`awaitStackLedgerOwnerFunded`), bounded only by the funding's own
 * deadline, and turns any failure into a refusal of the whole run.
 */
export const fundStackLedgerOwnerBeforeRun = async (
  ports: StackLedgerOwnerFundingPorts,
): Promise<void> => {
  try {
    await awaitStackLedgerOwnerFunded(ports);
  } catch (cause) {
    throw new StackLedgerOwnerFundingRefusal(cause);
  }
};

/** A family found the stack's ledger owner unfunded. */
export class StackLedgerOwnerUnfundedRefusal extends Error {
  constructor(readonly headerHash: string) {
    super(
      `The stack's attested tail ${headerHash} holds no output of the drill's ledger owner. The owner is funded before the run, ahead of every family's timer, and a family never funds it itself: start the run again so that its funding step runs first`,
    );
    this.name = "StackLedgerOwnerUnfundedRefusal";
  }
}

/** The drill's L2 ledger owner: its payment key hash and wallet address. */
export type StackLedgerOwner = Readonly<{ keyHash: string; address: string }>;

/**
 * The L2 address the ledger owner's funding deposit pays: the owner's
 * wallet address, the one the honest successor's transfer spends from.
 * Our node's `/utxos` matches the address text exactly, so an output at the
 * owner's key-only address is not one the transfer can spend.
 */
export const stackLedgerOwnerDepositAddress = (owner: StackLedgerOwner) =>
  owner.address;

/** Whether our node's ledger holds an output the owner's transfer spends. */
export const ourLedgerHoldsLedgerOwnerOutput = async (
  ledger: Pick<JourneyLedger, "utxos">,
  owner: StackLedgerOwner,
) => (await ledger.utxos(stackLedgerOwnerDepositAddress(owner))).length > 0;

/**
 * Whether an attested tail holds an output the owner's transfer spends: at
 * the owner's wallet address exactly, as the successor reads its ledger.
 */
const tailHoldsLedgerOwnerOutput = (
  tail: JourneyLedgerPredecessor,
  owner: StackLedgerOwner,
) =>
  tail.ledger.entries.some(
    ({ output }) =>
      encodeMidgardAddressText(decodeMidgardTxOutput(output).address) ===
      stackLedgerOwnerDepositAddress(owner),
  );

/** Refuses a family whose attested tail holds no output of the owner's. */
export const requireStackLedgerOwnerFunded = (
  tail: JourneyLedgerPredecessor,
  owner: StackLedgerOwner,
): void => {
  if (!tailHoldsLedgerOwnerOutput(tail, owner))
    throw new StackLedgerOwnerUnfundedRefusal(tail.headerHash);
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
 * The drill's own L2 ledger owner funding on an attached stack, run once
 * before any family's timer starts (`fundStackLedgerOwnerBeforeRun`): its
 * deposit of the stack journey's deposit amount is paid by the drill's
 * funding wallet. Our node absorbs it in its own shift, which may follow
 * operator-2's whole shift, and its committee attests the block within its
 * timeout. The deposit pays the owner's wallet address, which the run's
 * honest transfers spend from and pay back to, and only an output there
 * counts as funded.
 */
export const fundStackLedgerOwner = async (
  session: Readonly<{ context: StackJourneyContext; ledgerAt: LedgerAt }>,
  input: Readonly<{
    /** A family of the run, whose commit allowance every family shares. */
    category: string;
    onStage(name: string): void;
  }>,
) => {
  const { onStage } = input;
  const { context } = session;
  const { contracts, chain, references, publisherLucid, operatorLucid } =
    context.deployment;
  const owner = context.layout.wallets.ledgerOwner;
  const fingerprint = context.deployment.manifest.manifestId;
  const pendingPath = join(
    context.runDirectory,
    "work/journeys/ledger-owner-deposit.json",
  );
  const ourLedger = nodeLedger(context.stackConfig.endpoint);
  const da = stackJourneyDa(context, { onStage });
  const operator2 = paymentCredentialOf(
    await operatorLucid.wallet().address(),
  ).hash;
  const { transactionAllowanceMs } = await readStackJourneyTiming(
    {
      nodeConfigPath: context.layout.node.nodeConfigPath,
      manifestPath: context.layout.manifestPath,
    },
    input.category,
    { actionDepth: JOURNEY_ACTION_DEPTH },
  );
  const gate = stackUserEventPublicationGate(context, {
    commitAllowanceMs: transactionAllowanceMs,
    onStage,
  });
  return fundStackLedgerOwnerBeforeRun({
    ourLedgerHoldsOutput: () =>
      ourLedgerHoldsLedgerOwnerOutput(ourLedger, owner),
    ownerHoldsOutput: async () =>
      tailHoldsLedgerOwnerOutput(
        await decodeJourneyRetainedBlock(
          await da.readAttestedAtOrAfterTail(),
          session.ledgerAt,
        ),
        owner,
      ),
    beforeDeposit: (notAfterMs) => gate.beforeFunding(operator2, notAfterMs),
    admitsDeposit: (inclusionTimeMs) =>
      gate.admitsFunding(operator2, inclusionTimeMs),
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
    buildDeposit: async () => {
      const depositMinting = references.get("depositMint");
      if (depositMinting === undefined)
        throw new Error("Missing the stack's deposit minting reference");
      const deposit = await Effect.runPromise(
        SDK.buildUnsignedDepositTxWithMetadataProgram(
          publisherLucid,
          contracts,
          {
            l2Address: stackLedgerOwnerDepositAddress(owner),
            l2Datum: null,
            lovelace: BigInt(context.stackConfig.journey.depositLovelace),
            additionalAssets: {},
            referenceScripts: { depositMinting },
          },
        ),
      );
      const signed = await deposit.tx.sign.withWallet().complete();
      const pending: StackLedgerOwnerDeposit = {
        txHash: signed.toHash(),
        inclusionTime: deposit.metadata.inclusionTime,
        validTo: deposit.metadata.validTo,
      };
      return {
        ...pending,
        submit: async () => {
          onStage("ledger owner deposit");
          await writeJourneyArtifact(pendingPath, {
            deploymentFingerprint: fingerprint,
            ...pending,
          });
          await signed.submit();
        },
      };
    },
    now: () => chain.now(),
    pause: () => delay(LEDGER_OWNER_POLL_MS),
    absorbWithinMs:
      2 * Number(SDK.SHIFT_DURATION_MS) + Number(SDK.DA_ATTESTATION_TIMEOUT_MS),
    onStage,
  });
};
