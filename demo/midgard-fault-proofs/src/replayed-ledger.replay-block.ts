/**
 * One block's replay for the replayed ledger (`replayed-ledger.ts`): its
 * authenticated header and payload, and the replay of that payload over its
 * parent's ledger, with every failure sorted into the block's refusal, a
 * wait for a bound copy, or a local fault.
 */
import * as SDK from "@al-ft/midgard-sdk/core";
import type * as SDKDa from "@al-ft/midgard-sdk/da";
import { Cause, Effect, Exit } from "effect";

import { isReplayLocalFault, ReplayLocalFault } from "./block-replay.faults.js";
import { replayBlockEvents } from "./block-replay.js";
import type { Ledger } from "./ledger-store/index.js";
import type {
  ReplayedLedgerRefusal,
  ReplayedLedgerSource,
} from "./replayed-ledger.js";
import { RetainedDaPayloadUnavailableError } from "./transition-trace/fetch.retained-da-payload-unavailable.js";

/** Whether `error` is a script run left without a verdict
 * (`ScriptEvaluationIndeterminateError`), matched by its tag. */
const isScriptEvaluationIndeterminate = (error: unknown): error is Error =>
  error instanceof Error &&
  (error as { readonly _tag?: unknown })._tag ===
    "ScriptEvaluationIndeterminate";

/**
 * A payload no commitment binds yet that does not replay says nothing of
 * its block: the copy is released and the read waits for another.
 */
export const unboundWait = (
  source: ReplayedLedgerSource,
  headerHash: string,
  detail: string,
) => {
  if (source.binding?.unbound(headerHash) !== true) return;
  source.binding.release(headerHash);
  throw new RetainedDaPayloadUnavailableError(
    headerHash,
    `the payload of ${headerHash} is not bound to an attested commitment yet, and this copy does not replay: ${detail}`,
    "unreachable",
  );
};

const hashOf = (header: SDK.Header) =>
  Effect.runPromise(SDK.hashBlockHeader(header));

/** The header of `headerHash`, or `undefined` when the source has none. */
export const authenticHeader = async (
  source: ReplayedLedgerSource,
  headerHash: string,
): Promise<SDK.Header | undefined> => {
  const header = await source.headerAt(headerHash);
  if (header === undefined) return undefined;
  if ((await hashOf(header)) !== headerHash)
    throw new Error(`the header served for ${headerHash} is another block's`);
  return header;
};

/** The retained payload of `headerHash`, or `undefined`. */
export const authenticPayload = async (
  source: ReplayedLedgerSource,
  headerHash: string,
): Promise<SDKDa.DaPayload | undefined> => {
  const payload = await source.payloadAt(headerHash);
  if (payload === undefined) return undefined;
  const hash = await hashOf(payload.block_body.header);
  if (hash !== headerHash || payload.block_body.header_hash !== headerHash)
    throw new Error(
      `the retained payload served for ${headerHash} is another block's`,
    );
  return payload;
};

/**
 * The ledger after replaying `payload` over `parent`, or `undefined` when
 * the replay is refused (reported through `refuse`). A local fault is
 * raised, never refused: it says nothing about the block, so the block is
 * replayed again.
 */
export const replayBlock = async (input: {
  readonly headerHash: string;
  readonly payload: SDKDa.DaPayload;
  readonly parent: Ledger;
  readonly source: ReplayedLedgerSource;
  readonly refuse: (refusal: ReplayedLedgerRefusal) => void;
}): Promise<Ledger | undefined> => {
  const { headerHash, payload, source } = input;
  const header = payload.block_body.header;
  const material = await source.materialFor(payload);
  const exit = await Effect.runPromiseExit(
    replayBlockEvents({
      header,
      parent: input.parent,
      expectedNetworkId: header.expectedNetworkId,
      minFeeA: header.minFeeA,
      minFeeB: header.minFeeB,
      blockSlot: header.blockSlot,
      l1SlotConfig: source.l1SlotConfig,
      payload,
      ...material,
    }),
  );
  if (Exit.isSuccess(exit)) return exit.value;
  // A script run left without a verdict is a local fault too: verdict mode
  // leaves that only when the evaluator itself failed.
  const causes = [...Cause.defects(exit.cause), ...Cause.failures(exit.cause)];
  const local = causes.find(isReplayLocalFault);
  if (local !== undefined) throw local;
  const noVerdict = causes.find(isScriptEvaluationIndeterminate);
  if (noVerdict !== undefined)
    throw new ReplayLocalFault({
      message: `the replay of ${headerHash} has no script verdict: ${noVerdict.message}`,
      cause: noVerdict,
    });
  const detail = String(exit.cause);
  unboundWait(source, headerHash, detail);
  input.refuse({ headerHash, kind: "replay-refused", detail });
  return undefined;
};
