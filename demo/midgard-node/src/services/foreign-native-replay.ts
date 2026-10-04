import { Effect, Exit } from "effect";

import type { NativeMpfReplayInput } from "../database/pendingBlockFinalizations.js";
import {
  assertNativeMpfHashHex,
  NATIVE_MPF_OWNER_DEFAULT_CAPS,
  type NativeMpfGenerationHandle,
  type NativeMpfOwnerService,
  type PersistedNativeMpfReplay,
} from "./mpf-native-owner/protocol.js";
import { encodeNativeMpfEventLog } from "./mpf-native-owner/service.js";
import {
  digest,
  EVENT_LOG_DIGEST_DOMAIN,
  EVENT_LOG_HEADER_BYTES,
  EVENT_STREAM_DIGEST_DOMAIN,
  type NativeMpfEventOp,
} from "./mpf-native-owner/service.normalize-owner-options.js";

export type PreparedForeignNativeReplay = Readonly<{
  handle: NativeMpfGenerationHandle;
  replay: PersistedNativeMpfReplay;
}>;

export type ForeignNativeReplayBlock = Readonly<{
  kind: "local" | "foreign";
  nativeMpfReplay?: NativeMpfReplayInput;
  headerHash: string;
  parentHeaderHash: string;
  parentUtxosRoot: string;
  root: string;
  events: readonly (readonly Readonly<{
    key: string;
    output: Buffer | null;
  }>[])[];
  eventRoots: readonly string[];
}>;
type ForeignReplayBase = Readonly<{
  root: string;
  headerHash: string;
  importedBlocks: readonly ForeignNativeReplayBlock[];
}>;

const replayInput = (
  base: ForeignReplayBase,
  durableRoot: string,
  ownerBinarySha256: string,
) => {
  assertNativeMpfHashHex(base.root, "verified foreign root");
  assertNativeMpfHashHex(durableRoot, "durable root");
  const blocks = base.importedBlocks;
  for (let index = 0; index < blocks.length; index++) {
    const block = blocks[index]!;
    assertNativeMpfHashHex(block.parentUtxosRoot, "foreign parent root");
    assertNativeMpfHashHex(block.root, "foreign post root");
    const prior = blocks[index - 1];
    if (
      prior !== undefined &&
      (block.parentHeaderHash !== prior.headerHash ||
        block.parentUtxosRoot !== prior.root)
    )
      throw new Error(
        "Foreign native replay has a gap in its verified ancestry",
      );
    if (
      block.kind === "foreign" &&
      block.events.length !== block.eventRoots.length
    )
      throw new Error("Foreign native replay has incomplete event roots");
    for (const root of block.eventRoots)
      assertNativeMpfHashHex(root, "foreign event root");
    if ((block.eventRoots.at(-1) ?? block.parentUtxosRoot) !== block.root)
      throw new Error(
        "Foreign native replay does not reproduce its block root",
      );
  }
  if (durableRoot === base.root) return undefined;
  const last = blocks.at(-1);
  if (last?.headerHash !== base.headerHash || last.root !== base.root)
    throw new Error("Foreign native replay does not end at the verified base");
  const start = blocks.findIndex(
    (block) => block.parentUtxosRoot === durableRoot,
  );
  if (start < 0)
    throw new Error(
      "Native durable root is not a parent of the verified replay",
    );
  const suffix = blocks.slice(start);
  const logs = suffix.map((block) => {
    if (block.kind === "local") {
      const replay = block.nativeMpfReplay;
      if (
        replay === undefined ||
        replay.ownerBinarySha256.toString("hex") !== ownerBinarySha256 ||
        replay.baseRoot.toString("hex") !== block.parentUtxosRoot ||
        replay.candidateRoot.toString("hex") !== block.root ||
        replay.eventCount !== block.eventRoots.length ||
        replay.eventRoots.toString("hex") !== block.eventRoots.join("") ||
        digest(EVENT_LOG_DIGEST_DOMAIN, replay.eventLog).toString("hex") !==
          replay.eventLogDigest.toString("hex")
      )
        throw new Error(
          "Local ancestor lacks exact pinned native replay material",
        );
      return Buffer.from(replay.eventLog);
    }
    const events = block.events.map((event) =>
      event.map((mutation): NativeMpfEventOp => {
        if (!/^(?:[0-9a-f]{2})+$/.test(mutation.key))
          throw new Error(
            "Foreign native replay contains a noncanonical ledger key",
          );
        const key = Buffer.from(mutation.key, "hex");
        return mutation.output === null
          ? { type: "delete", key }
          : { type: "insert", key, value: Buffer.from(mutation.output) };
      }),
    );
    return encodeNativeMpfEventLog(block.parentUtxosRoot, events);
  });
  const roots = suffix.flatMap((block) => [...block.eventRoots]);
  let opCount = 0;
  for (const [index, log] of logs.entries()) {
    if (
      log.length < EVENT_LOG_HEADER_BYTES ||
      log.subarray(0, 4).toString("ascii") !== "MEGO" ||
      log.readUInt16LE(4) !== 1 ||
      log.readUInt16LE(6) !== 0 ||
      log.subarray(28, 60).toString("hex") !== suffix[index]!.parentUtxosRoot ||
      log.readUInt32LE(8) !== suffix[index]!.eventRoots.length ||
      !digest(
        EVENT_STREAM_DIGEST_DOMAIN,
        log.subarray(8, 28),
        log.subarray(28, 60),
        log.subarray(EVENT_LOG_HEADER_BYTES),
      ).equals(log.subarray(60, EVENT_LOG_HEADER_BYTES))
    )
      throw new Error(
        "Ancestor native event log differs from its verified segment",
      );
    opCount += log.readUInt32LE(12);
  }
  if (
    roots.length > NATIVE_MPF_OWNER_DEFAULT_CAPS.maxEvents ||
    opCount > NATIVE_MPF_OWNER_DEFAULT_CAPS.maxOps ||
    EVENT_LOG_HEADER_BYTES +
      logs.reduce(
        (total, log) => total + log.length - EVENT_LOG_HEADER_BYTES,
        0,
      ) >
      NATIVE_MPF_OWNER_DEFAULT_CAPS.maxFrameBytes
  )
    throw new Error("Foreign native replay exceeds the owner input caps");
  const eventLog = Buffer.concat([
    encodeNativeMpfEventLog(durableRoot, []),
    ...logs.map((log) => log.subarray(EVENT_LOG_HEADER_BYTES)),
  ]);
  eventLog.writeUInt32LE(roots.length, 8);
  eventLog.writeUInt32LE(opCount, 12);
  digest(
    EVENT_STREAM_DIGEST_DOMAIN,
    eventLog.subarray(8, 28),
    eventLog.subarray(28, 60),
    eventLog.subarray(EVENT_LOG_HEADER_BYTES),
  ).copy(eventLog, 60);
  return { eventLog, roots };
};

/** Materialize an already verified canonical prefix in a private generation.
 * The parent must persist the replay under source authority before promotion.
 * No worker or root-equality shortcut can grant durable ownership here.
 */
export const prepareForeignNativeReplay = ({
  owner,
  ownerBinarySha256,
  base,
}: {
  readonly owner: Pick<
    NativeMpfOwnerService,
    "diagnostics" | "fork" | "applyEvents" | "discard"
  >;
  readonly ownerBinarySha256: string;
  readonly base: ForeignReplayBase;
}): Effect.Effect<PreparedForeignNativeReplay | undefined, unknown> => {
  let handle: NativeMpfGenerationHandle | undefined;
  const promise = <A>(work: () => Promise<A>) =>
    Effect.tryPromise({ try: work, catch: (cause) => cause });
  return Effect.gen(function* () {
    const input = yield* Effect.try({
      try: () => {
        assertNativeMpfHashHex(ownerBinarySha256, "owner binary SHA256");
        return base;
      },
      catch: (cause) => cause,
    });
    const { durableRoot } = yield* promise(() => owner.diagnostics());
    const replay = yield* Effect.try({
      try: () => replayInput(input, durableRoot, ownerBinarySha256),
      catch: (cause) => cause,
    });
    if (replay === undefined) return undefined;
    const acquired = yield* promise(() => owner.fork(durableRoot));
    handle = acquired;
    const applied = yield* promise(() =>
      owner.applyEvents(acquired, replay.eventLog),
    );
    const eventLogDigest = digest(
      EVENT_LOG_DIGEST_DOMAIN,
      replay.eventLog,
    ).toString("hex");
    if (
      applied.candidateRoot !== base.root ||
      applied.eventLogDigest !== eventLogDigest ||
      applied.eventRoots.length !== replay.roots.length ||
      applied.eventRoots.some((root, index) => root !== replay.roots[index])
    )
      return yield* Effect.fail(
        new Error(
          "Native foreign replay differs from independently verified E1 roots",
        ),
      );
    return {
      handle: acquired,
      replay: {
        schema: 1,
        ownerBinarySha256,
        baseRoot: durableRoot,
        candidateRoot: applied.candidateRoot,
        eventLog: replay.eventLog,
        eventLogDigest,
        eventRoots: Buffer.from(replay.roots.join(""), "hex"),
        eventCount: replay.roots.length,
      },
    } satisfies PreparedForeignNativeReplay;
  }).pipe(
    // Native calls have owner-enforced deadlines. Join them before cleanup so
    // cancellation cannot strand an unjournalled generation mid-application.
    Effect.uninterruptible,
    Effect.onExit((exit) =>
      Exit.isFailure(exit) && handle !== undefined
        ? promise(() => owner.discard(handle!)).pipe(
            Effect.catchAll(() => Effect.void),
          )
        : Effect.void,
    ),
  );
};
