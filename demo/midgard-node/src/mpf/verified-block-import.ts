import { MidgardCekProgramMaterialMissingRootError } from "@al-ft/midgard-core/cek-proof";
import * as SDK from "@al-ft/midgard-sdk";
import { Data, Effect } from "effect";

import {
  computeDaPayloadRoots,
  headerCounts,
  headerRoots,
  rootMismatches,
} from "../workers/commit-block-header/da-payload.compute-da-payload-roots.js";
import { computeLedgerMpfRootFromLedgerEntries } from "./ledger-hydration.js";
import {
  type ImportedBlockReplayContext,
  replayImportedBlockEvents,
} from "./verified-block-import.replay-events.js";

export class ForeignBlockVerificationError extends Data.TaggedError(
  "ForeignBlockVerificationError",
)<{
  readonly foreignHeaderHash: string;
  readonly reason: "missing" | "invalid";
  readonly detail: string;
}> {}

/** Full import authentication. A caller must supply the exact canonical parent
 * and deployment observation; matching ledger roots or DA signatures do not
 * substitute for replay. No successful result is cached across observations. */
export const verifyAndImportBlock = (
  input: ImportedBlockReplayContext & {
    readonly headerHash: string;
    readonly parentHeaderHash: string;
    readonly parentUtxosRoot: string;
    readonly payload?: SDK.DaPayload;
  },
) =>
  Effect.gen(function* () {
    const fail = (reason: "missing" | "invalid", detail: string) =>
      Effect.fail(
        new ForeignBlockVerificationError({
          foreignHeaderHash: input.headerHash,
          reason,
          detail,
        }),
      );
    if (
      input.header.prevHeaderHash !== input.parentHeaderHash ||
      input.header.prevUtxosRoot !== input.parentUtxosRoot
    ) {
      return yield* fail(
        "invalid",
        "foreign header does not extend the verified parent",
      );
    }
    if (
      input.header.protocolVersion !== 1n ||
      input.header.expectedNetworkId !== input.expectedNetworkId ||
      input.header.minFeeA !== input.minFeeA ||
      input.header.minFeeB !== input.minFeeB ||
      input.header.blockSlot !== input.blockSlot
    )
      return yield* fail(
        "invalid",
        "foreign header validation context differs from deployment",
      );
    if (input.payload === undefined)
      return yield* fail("missing", "foreign DA payload is unavailable");
    const payload = input.payload;
    if (
      (yield* SDK.hashBlockHeader(input.header)) !== input.headerHash ||
      payload.block_body.header_hash !== input.headerHash ||
      (yield* SDK.hashBlockHeader(payload.block_body.header)) !==
        input.headerHash ||
      payload.version !== SDK.DA_PAYLOAD_VERSION
    )
      return yield* fail(
        "invalid",
        "foreign DA/header identity differs from the canonical observation",
      );
    const baseRoot = yield* computeLedgerMpfRootFromLedgerEntries(
      input.parentEntries,
    );
    if (baseRoot !== input.parentUtxosRoot)
      return yield* fail(
        "invalid",
        "foreign replay parent snapshot root mismatch",
      );
    const roots = yield* computeDaPayloadRoots(payload);
    const mismatches = [...rootMismatches(headerRoots(input.header), roots)];
    const body = payload.block_body;
    const memberCounts: SDK.DaPayloadCounts = {
      depositCount: BigInt(body.deposits.length),
      withdrawalCount: BigInt(body.withdrawals.length),
      forcedTransactionCount: BigInt(body.forced_transactions.length),
      l2TransactionCount: BigInt(body.transactions.length),
      totalEventCount: BigInt(
        body.deposits.length +
          body.withdrawals.length +
          body.forced_transactions.length +
          body.transactions.length,
      ),
      transitionStepCount: BigInt(body.transition_trace.length),
      validationTraceCount: BigInt(body.validation_traces.length),
    };
    for (const key of Object.keys(
      memberCounts,
    ) as (keyof SDK.DaPayloadCounts)[]) {
      if (
        memberCounts[key] !== headerCounts(input.header)[key] ||
        body.counts[key] !== memberCounts[key]
      )
        mismatches.push(key);
    }
    if (body.event_to_step.length !== body.transition_trace.length)
      mismatches.push("event_to_step_count");
    if (body.transition_trace.length !== Number(memberCounts.totalEventCount))
      mismatches.push("transition_step_count");
    if (mismatches.length > 0)
      return yield* fail(
        "invalid",
        `foreign DA commitment mismatch: ${mismatches.join(",")}`,
      );
    const entries = yield* replayImportedBlockEvents({ ...input, payload });
    const replayRoot = yield* computeLedgerMpfRootFromLedgerEntries(entries);
    if (replayRoot !== input.header.utxosRoot)
      return yield* fail("invalid", "foreign replay UTxO root mismatch");
    // Replay compares every transition, mapping and validation descriptor by its
    // canonical bytes, independently of the payload's internally consistent roots.
    return { headerHash: input.headerHash, root: replayRoot, entries };
  }).pipe(
    Effect.catchAllDefect((cause) =>
      Effect.fail(
        new ForeignBlockVerificationError({
          foreignHeaderHash: input.headerHash,
          reason:
            cause instanceof MidgardCekProgramMaterialMissingRootError
              ? "missing"
              : "invalid",
          detail: String(cause),
        }),
      ),
    ),
    Effect.catchAll((cause) =>
      cause instanceof ForeignBlockVerificationError
        ? Effect.fail(cause)
        : Effect.fail(
            new ForeignBlockVerificationError({
              foreignHeaderHash: input.headerHash,
              reason:
                cause instanceof MidgardCekProgramMaterialMissingRootError
                  ? "missing"
                  : "invalid",
              detail: String(cause),
            }),
          ),
    ),
  );
