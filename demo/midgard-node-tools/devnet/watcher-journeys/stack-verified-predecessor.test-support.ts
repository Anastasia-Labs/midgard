/**
 * A scripted stack around the verified-predecessor check: its state queue,
 * read through the root-anchor module's settled view and the stack's own
 * attested-tail waits; its watcher, the production operations observability
 * served through its own HTTP handler; and our node, its ledger and its
 * `/tx-status` as the drill's transfer reads them. Time is a fake clock that
 * each pause moves on by a second, running the next scripted step.
 */
import { createHash } from "node:crypto";
import { mkdtemp } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";

import * as SDK from "@al-ft/midgard-sdk";
import {
  assetsToValue,
  CML,
  generateSeedPhrase,
  walletFromSeed,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";
import {
  makeMidgardTxOutput,
  makeOutRefCbor,
} from "midgard-node/tests/midgard-output-helpers";
import {
  createWatcherOperationsObservability,
  type WatcherFaultProofSupervisor,
  type WatcherVerificationDiagnostic,
} from "midgard-watcher";

import type { JourneyLedger } from "../../src/full-stack/journey.js";
import type { JourneyBlock } from "./fixture.js";
import { headerOn, honestOperator } from "./honest-successor.test-support.js";
import { successorTransfer } from "./honest-successor.transfer.js";
import {
  awaitAttestedAtOrAfterStackQueueTail,
  awaitAttestedStackQueueTail,
} from "./stack-journey-da.js";
import type { StackVerifiedPredecessorPorts } from "./stack-verified-predecessor.js";
import {
  type QueueRootState,
  settledStateQueue,
} from "./state-queue-root-anchor.js";

/** Every drill's directory, for the suite to remove. */
export const drillDirectories: string[] = [];

/** The scripted bounds; the real ones come from the drill's own names. */
export const ATTESTATION_TIMEOUT_MS = 20_000n;
export const ABSORB_WITHIN_MS = 30_000;
export const DECISION_WITHIN_MS = 40_000;
const PAUSE_MS = 1_000;

export const envelopeSha256 = (block: JourneyBlock) =>
  createHash("sha256").update(block.payloadEnvelopeCbor).digest("hex");

/** The production watcher observability, as a fresh watcher process has it. */
const watcherProcess = () =>
  createWatcherOperationsObservability({
    deploymentFingerprint: "11".repeat(32),
    supervisor: {
      status: () => ({
        phase: "accepting",
        recovered: true,
        queuedJobCount: 0,
        activeJob: null,
        blockedJob: null,
        deadlineHealth: "safe",
        earliestDeadlineJob: null,
        remainingSafeStartMs: "1000",
      }),
    } as unknown as WatcherFaultProofSupervisor,
    launchScopeStatus: () => ({
      installedCategoryCount: 54,
      requiredCategoryCount: 54,
    }),
    durableProofQueueStatus: () => ({
      queuedJobCount: 0,
      oldestQueuedAtMs: null,
    }),
    retainedDaTransportStatus: () => ({ state: "idle", failure: null }),
  });

type QueueEntry = { block: JourneyBlock; blockTime: bigint; attested: boolean };
type QueueNode = Omit<QueueEntry, "block"> & {
  headerHash: string;
  block: JourneyBlock;
};

export const verifiedPredecessorDrill = async () => {
  const directory = await mkdtemp(join(tmpdir(), "verified-predecessor-"));
  drillDirectories.push(directory);
  let now = 10_000_000;
  let slot = 1n;
  const built = new Map<string, JourneyBlock>();
  /**
   * A block on `parent` with its own payload envelope; each one built ends
   * after the one built before it, as the queue's blocks do.
   */
  const blockOn = async (parent: string): Promise<JourneyBlock> => {
    const { header } = await headerOn(parent, honestOperator, (slot += 1n));
    const timed = {
      ...header,
      startTime: slot * 1000n,
      endTime: slot * 1000n + 999n,
    };
    const headerHash = await Effect.runPromise(SDK.hashBlockHeader(timed));
    const block = {
      header: timed,
      headerHash,
      payloadEnvelopeCbor: Buffer.from(`envelope-${headerHash}`),
    };
    built.set(headerHash, block);
    return block;
  };
  // An idle stack: every block merged, so the queue holds only its root.
  const rootBlock = await blockOn("00".repeat(28));
  const l1 = {
    root: rootBlock,
    queue: [] as QueueEntry[],
    merged: new Set([rootBlock.headerHash]),
    removed: new Set<string>(),
    /** How long a committed block stays out of the queue as it is read. */
    queueLagMs: 0,
    /** How many queue reads next lose their connection. */
    lostQueueReads: 0,
  };
  const queued = (hash: string) =>
    l1.queue.find(({ block }) => block.headerHash === hash);
  const rootState = (): QueueRootState => ({
    headerHash: l1.root.headerHash,
    prevHeaderHash: l1.root.header.prevHeaderHash,
    utxoRoot: l1.root.header.utxosRoot,
    endTime: l1.root.header.endTime,
  });

  // Each pause moves time on and runs the next scripted step.
  const steps: (() => void | Promise<void>)[] = [];
  let pauses = 0;
  const pause = async () => {
    pauses += 1;
    now += PAUSE_MS;
    await steps.shift()?.();
  };
  const stages: string[] = [];
  const onStage = (name: string) => {
    stages.push(name);
  };
  const queuePorts = {
    ...settledStateQueue<{ headerHash: string; entry: QueueEntry }, QueueNode>({
      readQueue: () => {
        if (l1.lostQueueReads > 0) {
          l1.lostQueueReads -= 1;
          // As fetch rejects a request whose connection the server reset.
          return Promise.reject(
            new TypeError("fetch failed", {
              cause: Object.assign(new Error("read ECONNRESET"), {
                code: "ECONNRESET",
              }),
            }),
          );
        }
        return Promise.resolve({
          root: rootState(),
          blocks: l1.queue
            .filter(({ blockTime }) => blockTime + BigInt(l1.queueLagMs) <= now)
            .map((entry) => ({ headerHash: entry.block.headerHash, entry })),
        });
      },
      decodeBlock: ({ entry }: { entry: QueueEntry }) =>
        Promise.resolve({
          headerHash: entry.block.headerHash,
          blockTime: entry.blockTime,
          attested: entry.attested,
          block: entry.block,
        }),
      mergedRoot: (_root, node) => ({ ...node, block: l1.root }),
    }),
    now: () => now,
    pause,
    attestationTimeoutMs: ATTESTATION_TIMEOUT_MS,
    onStage,
  };

  let process = watcherProcess();
  let sequence = 0;
  const diagnosticsPaths: string[] = [];
  const watcher = {
    record: (
      block: JourneyBlock,
      outcome: WatcherVerificationDiagnostic["outcome"],
      payloadEnvelopeSha256: string | undefined = outcome === "verified"
        ? envelopeSha256(block)
        : undefined,
    ) => {
      sequence += 1;
      process.sink.recordVerification({
        subjectDigest: sequence.toString(16).padStart(64, "0"),
        headerHash: block.headerHash,
        ...(payloadEnvelopeSha256 === undefined
          ? {}
          : { payloadEnvelopeSha256 }),
        queuedAtMs: "1",
        startedAtMs: "2",
        completedAtMs: "3",
        elapsedMs: "1",
        outcome,
      });
    },
    /** A restarted watcher holds no record of what it decided before. */
    restart: () => {
      process = watcherProcess();
    },
    operations: async (path: string) => {
      diagnosticsPaths.push(path);
      const response = await process.handleHttpRequest(
        new Request(`http://127.0.0.1${path}`),
      );
      if (response.status !== 200)
        throw new Error(`Operations HTTP returned ${response.status}`);
      return await response.json();
    },
  };

  // Our node: its ledger shows the owner's one output, and /tx-status
  // answers an unknown transaction with 404 and `not_found`.
  const seedPhrase = generateSeedPhrase();
  const owner = walletFromSeed(seedPhrase, { network: "Preprod" }).address;
  const node = {
    statuses: new Map<string, Record<string, unknown>>(),
    /** Every transaction handed to our node's /submit, in order. */
    submitted: [] as string[],
    loseNextResponse: false,
    /** How many next /tx-status reads its ingress answers 502 instead. */
    unavailableStatusReads: 0,
    ownerHolds: true,
    onSubmit: undefined as ((txId: string) => void) | undefined,
  };
  const ownerOutput = "44".repeat(32);
  const ledger: JourneyLedger = {
    utxos: (address) =>
      Promise.resolve(
        address !== owner || !node.ownerHolds
          ? []
          : [
              {
                txHash: ownerOutput,
                outputIndex: 0,
                outrefCbor: makeOutRefCbor(ownerOutput, 0),
                outputCbor: Buffer.from(
                  makeMidgardTxOutput(
                    CML.Address.from_bech32(address),
                    assetsToValue({ lovelace: 50_000_000n }),
                  ).to_cbor_bytes(),
                ),
                address,
                assets: { lovelace: 50_000_000n },
              },
            ],
      ),
    submitTransfer: (_cbor, txId) => {
      node.submitted.push(txId);
      if (node.loseNextResponse) {
        node.loseNextResponse = false;
        return Promise.reject(
          new Error("Our node's /submit response was lost"),
        );
      }
      node.statuses.set(txId, { status: "queued" });
      node.onSubmit?.(txId);
      return Promise.resolve({ txId, status: "queued" });
    },
  };
  const fetchImpl = ((input: Parameters<typeof fetch>[0]) => {
    const url = new URL(input instanceof Request ? input.url : input);
    if (url.pathname !== "/tx-status")
      return Promise.resolve(Response.json({}, { status: 404 }));
    if (node.unavailableStatusReads > 0) {
      node.unavailableStatusReads -= 1;
      return Promise.resolve(
        new Response("<html>502 Bad Gateway</html>", { status: 502 }),
      );
    }
    const txId = url.searchParams.get("tx_hash");
    const status = node.statuses.get(txId ?? "") ?? { status: "not_found" };
    return Promise.resolve(
      Response.json(
        { txId, ...status },
        { status: status["status"] === "not_found" ? 404 : 200 },
      ),
    );
  }) as typeof fetch;

  const standings: string[] = [];
  const ports = (): StackVerifiedPredecessorPorts => ({
    directory,
    readAttestedTail: async () =>
      (await awaitAttestedStackQueueTail(queuePorts)).block,
    readAttestedAtOrAfterTail: async () =>
      (await awaitAttestedAtOrAfterStackQueueTail(queuePorts)).block,
    standing: (hash) => {
      standings.push(hash);
      return Promise.resolve(
        queued(hash) !== undefined
          ? "queued"
          : l1.merged.has(hash)
            ? "merged"
            : "removed",
      );
    },
    headerOnL1: (hash) =>
      Promise.resolve(
        queued(hash)?.block.header ??
          (l1.merged.has(hash) || l1.removed.has(hash)
            ? built.get(hash)?.header
            : undefined),
      ),
    operations: watcher.operations,
    transfer: (transferDirectory) =>
      successorTransfer({
        directory: transferDirectory,
        nodeEndpoint: "http://our-node.test",
        ledger,
        ledgerOwner: { seedPhrase, address: owner },
        network: "Preprod",
        transferLovelace: 5_000_000n,
        fetchImpl,
      }),
    now: () => now,
    pause,
    absorbWithinMs: ABSORB_WITHIN_MS,
    decisionWithinMs: DECISION_WITHIN_MS,
    onStage,
  });

  const l1Ops = {
    /** A block committed on the queue's tail at the current time. */
    commit: (block: JourneyBlock, attested = false) => {
      l1.queue.push({ block, blockTime: BigInt(now), attested });
    },
    /** Each queue read leaves out the blocks committed within `ms`. */
    lag: (ms: number) => {
      l1.queueLagMs = ms;
    },
    attest: (hash: string) => {
      queued(hash)!.attested = true;
    },
    /** The next `reads` queue reads lose their connection. */
    loseQueueReads: (reads: number) => {
      l1.lostQueueReads = reads;
    },
    /** The queue's first block merges into the root. */
    merge: (hash: string) => {
      const entry = queued(hash)!;
      l1.queue.splice(l1.queue.indexOf(entry), 1);
      l1.root = entry.block;
      l1.merged.add(hash);
    },
  };

  /**
   * Our node commits each submitted transfer in a block on `parent` one
   * pause later, its committee attests it the pause after, unless
   * `attests` is false, and the watcher then records `outcome` for it,
   * unless `outcome` is absent.
   */
  const ourNodeCommits = (
    parent: () => string,
    outcome?: WatcherVerificationDiagnostic["outcome"],
    { attests = true }: { attests?: boolean } = {},
  ) => {
    const committed: JourneyBlock[] = [];
    node.onSubmit = (txId) => {
      steps.push(
        async () => {
          const block = await blockOn(parent());
          committed.push(block);
          l1Ops.commit(block);
          node.statuses.set(txId, {
            status: "committed",
            headerHash: block.headerHash,
          });
        },
        () => {
          if (attests) l1Ops.attest(committed.at(-1)!.headerHash);
        },
        () => {
          if (attests && outcome !== undefined)
            watcher.record(committed.at(-1)!, outcome);
        },
      );
    };
    return committed;
  };

  return {
    directory,
    rootBlock,
    blockOn,
    l1: l1Ops,
    watcher,
    node,
    ports,
    steps,
    stages,
    standings,
    diagnosticsPaths,
    ourNodeCommits,
    now: () => now,
    pauses: () => pauses,
  };
};
