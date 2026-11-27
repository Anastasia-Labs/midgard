/** Honest re-execution of one committed normal transaction from DA replay, for
 * the validation-trace dispute (category 00000006). The challenger commits its
 * own trace, so nothing here reads a trace from the DA payload. */
import { MIDGARD_CONSENSUS_PROFILE } from "@al-ft/midgard-core/consensus-profile";
import type { L1SlotConfig } from "@al-ft/midgard-core/l1-slot-config";
import * as SDK from "@al-ft/midgard-sdk/core";
import {
  applyValidationLedgerMutations,
  canonicalTransitionEffectFromStatePatch,
  commitValidationTraceV1,
  DirectValidationTraceUnavailable,
  type PreStateLedger,
  replayValidationVerdictEvent,
  requiresRealValidationDescriptorV1,
  transitionEffectToLedgerOps,
  type ValidationMachineLedgerMutationStep,
  type ValidationMachineLedgerOp,
  type ValidationMachineReplayInput,
  ValidationTraceStopped,
  type ValidationVerdictEvent,
} from "@al-ft/midgard-validation";
import type { UTxOStatePatch } from "@al-ft/midgard-validation/phase-b";
import { RejectCodes } from "@al-ft/midgard-validation/types";
import { Data } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import { resolvableOutRefs } from "../block-replay.ledger.js";
import { readingLedger } from "../event-ledger.ledger.js";
import type { CanonicalBlockEvidence } from "../evidence/canonical-block-evidence.js";
import { ledgerEntryLeaf } from "../ledger-at.js";
import { type Ledger, ledgerTrie } from "../ledger-store/index.js";
import type { SourceEventRecord } from "../transition-trace/reconstruct.js";

export type CommittedL2Event = Extract<
  SourceEventRecord,
  { readonly phase: "L2Transaction" }
>;

/**
 * Rule R-DESC, read from the authenticated source bytes: only an accepted
 * normal transaction with a non-empty redeemer field runs Plutus.
 */
export const eventRunsPlutus = (
  event: SourceEventRecord,
): event is CommittedL2Event =>
  event.phase === "L2Transaction" &&
  event.entry.validity === "TxIsValid" &&
  requiresRealValidationDescriptorV1(event.entry.fullTransactionCbor, "normal");

/** The ledger mutation an accepted event makes to its pre-state ledger. */
export type AcceptedLedgerMutation = Readonly<{
  ledgerOps: readonly ValidationMachineLedgerOp[];
  ledgerMutationSteps: readonly ValidationMachineLedgerMutationStep[];
  /** The ledger root after the event, in the committed encoding. */
  postUtxosRoot: string;
}>;

/**
 * The mutation `statePatch` (an accepted verdict replay's patch) makes to
 * `ledger`: its consensus ledger operations in canonical order, and each
 * operation's step from the root before it to the root after it, built on a
 * trie of `ledger`'s own nodes. A ledger no longer readable is this
 * process's own fault (`ReplayLocalFault`).
 */
export const acceptedLedgerMutation = async (
  ledger: Ledger,
  statePatch: UTxOStatePatch,
): Promise<AcceptedLedgerMutation> => {
  const ledgerOps = transitionEffectToLedgerOps(
    canonicalTransitionEffectFromStatePatch(statePatch),
  );
  const ledgerMutationSteps = await readingLedger(
    "validation-trace ledger mutation",
    async () =>
      await applyValidationLedgerMutations(await ledgerTrie(ledger), ledgerOps),
  );
  return {
    ledgerOps,
    ledgerMutationSteps,
    postUtxosRoot:
      ledgerMutationSteps.at(-1)?.postRoot.toString("hex") ?? ledger.root,
  };
};

/** `ledger` as the pre-state input resolution proves against: each key's
 * proof against its root. */
const ledgerPreState = (ledger: Ledger): PreStateLedger => ({
  proveResolution: async (keys) =>
    await readingLedger("validation-trace input resolution", async () => {
      const read = await ledger.read(keys);
      return await Promise.all(
        keys.map(async (key) => await read.get(key.toString("hex"))!.proof()),
      );
    }),
});

/**
 * What `ledger` holds at the out-refs `event`'s transaction can resolve, its
 * spend and reference inputs, each entry with the descriptor its trie leaf
 * holds: every entry a replay of the event reads.
 */
const resolvedEntries = async (ledger: Ledger, event: CommittedL2Event) => {
  const read = await readingLedger(
    "validation-trace replay pre-state",
    async () =>
      await ledger.read(
        resolvableOutRefs(
          Buffer.from(event.entry.fullTransactionCbor),
          "normal",
        ),
      ),
  );
  return [...read].flatMap(([outRef, { output }]) =>
    output === undefined
      ? []
      : [
          ledgerEntryLeaf(
            { outRef: Buffer.from(outRef, "hex"), output },
            "validation-trace replay pre-state",
          ).entry,
        ],
  );
};

/** The honest verdict a replay input expects, with an accepted event's mutation. */
export type PlutusReplayVerdict =
  | Readonly<{ verdict: "rejected" }>
  | Readonly<{ verdict: "accepted"; mutation: AcceptedLedgerMutation }>;

/**
 * The replay input for `event` over its event pre-state ledger, expecting
 * `verdict`: the entries the transaction resolves, read from `ledger`, and
 * input resolution proven against it. The claim derives the trace context
 * from the header, so the context's time element is `header.end_time`;
 * validity is decided by `header.block_slot`. A rejected event leaves the
 * ledger unchanged; an accepted one makes its mutation. A ledger no longer
 * readable is this process's own fault (`ReplayLocalFault`).
 */
export const plutusReplayInput = async ({
  header,
  event,
  ledger,
  l1SlotConfig,
  verdict,
}: {
  readonly header: SDK.Header;
  readonly event: CommittedL2Event;
  readonly ledger: Ledger;
  readonly l1SlotConfig: L1SlotConfig;
  readonly verdict: PlutusReplayVerdict;
}): Promise<ValidationMachineReplayInput> => ({
  consensusProfile: MIDGARD_CONSENSUS_PROFILE,
  eventKeyCbor: Buffer.from(Data.to(event.eventKey, SDK.EventKey), "hex"),
  transactionId: Buffer.from(event.entry.txId, "hex"),
  canonicalTransactionCbor: Buffer.from(event.entry.fullTransactionCbor),
  sourceKind: "normal",
  priorUtxosRoot: ledger.root,
  ledgerWitnessEntries: await resolvedEntries(ledger, event),
  preStateLedger: ledgerPreState(ledger),
  ...(verdict.verdict === "rejected"
    ? {
        postUtxosRoot: ledger.root,
        expectedLedgerOps: [],
        ledgerMutationSteps: [],
        expectedVerdict: "rejected",
        expectedRejectionCode: RejectCodes.PlutusScriptInvalid,
      }
    : {
        postUtxosRoot: verdict.mutation.postUtxosRoot,
        expectedLedgerOps: verdict.mutation.ledgerOps,
        ledgerMutationSteps: verdict.mutation.ledgerMutationSteps,
        expectedVerdict: "accepted",
        expectedRejectionCode: null,
      }),
  blockTimeMs: Number(header.endTime),
  expectedNetworkId: header.expectedNetworkId,
  minFeeA: header.minFeeA,
  minFeeB: header.minFeeB,
  blockSlot: header.blockSlot,
  l1SlotConfig,
});

/** The verdict replay of a replay input's event: no trace is built. */
export const replayPlutusVerdict = async (
  input: ValidationMachineReplayInput,
): Promise<ValidationVerdictEvent> =>
  await Effect.runPromise(
    replayValidationVerdictEvent({
      consensusProfile: input.consensusProfile,
      canonicalTransactionCbor: input.canonicalTransactionCbor,
      ledgerWitnessEntries: input.ledgerWitnessEntries,
      blockTimeMs: input.blockTimeMs,
      expectedNetworkId: input.expectedNetworkId,
      minFeeA: input.minFeeA,
      minFeeB: input.minFeeB,
      blockSlot: input.blockSlot,
      l1SlotConfig: input.l1SlotConfig,
      sourceKind: "normal",
    }),
  );

export type PlutusExecutionReplay =
  | { readonly kind: "no_failure" }
  | { readonly kind: "fails"; readonly stepCount: number }
  /** The verdict replay found a failed script run, but the trace replay
   * deterministically did not reproduce it. No claim is built on it. */
  | { readonly kind: "trace_disagrees"; readonly detail: string };

/**
 * Replays `event` by verdict first and builds the trace only for a failed
 * script run, so a block of passing transactions runs no proof-mode replay.
 *
 * Only a deterministic outcome is returned: a verdict, or a trace that
 * disagrees with it. Anything else throws, including a trace build stopped
 * as `unavailable`, which wraps whatever the builder threw (an exhausted
 * heap among them), so the caller retries rather than deciding.
 */
export const replayPlutusExecution = async ({
  evidence,
  event,
  ledger,
  l1SlotConfig,
}: {
  readonly evidence: CanonicalBlockEvidence;
  readonly event: CommittedL2Event;
  readonly ledger: Ledger;
  readonly l1SlotConfig: L1SlotConfig;
}): Promise<PlutusExecutionReplay> => {
  const input = await plutusReplayInput({
    header: evidence.header,
    event,
    ledger,
    l1SlotConfig,
    verdict: { verdict: "rejected" },
  });
  const verdict = await replayPlutusVerdict(input);
  if (
    verdict.verdict !== "rejected" ||
    verdict.rejection?.subject?.arm !== "PlutusExecutionFailed"
  )
    return { kind: "no_failure" };
  const committed = await Effect.runPromise(
    Effect.either(commitValidationTraceV1(input)),
  );
  if (committed._tag === "Left") {
    const stop = committed.left;
    if (
      (stop instanceof ValidationTraceStopped &&
        stop.reason === "disagreement") ||
      stop instanceof DirectValidationTraceUnavailable
    )
      return { kind: "trace_disagrees", detail: stop.message };
    throw stop;
  }
  if (committed.right.descriptor.verdict !== "rejected")
    return {
      kind: "trace_disagrees",
      detail: "the trace replay did not end Rejected",
    };
  return { kind: "fails", stepCount: committed.right.descriptor.stepCount };
};
