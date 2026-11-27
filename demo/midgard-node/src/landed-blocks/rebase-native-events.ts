/**
 * The MPF events the native MPF applies for one step of the rebase target,
 * from point reads of the step's parent ledger in the native owner's ledger
 * store. Processing persists the ledger after every block before it records
 * the block, and each fold has the store hold the frontier's root, so a
 * parent's ledger is retained unless a retain dropped it; then it is rebuilt
 * from the newest root the store retains on the processed chain to it
 * (`ledgerAt`). A parent this node cannot give is its own fault: the rebase
 * fails and holds, and the native MPF does not move past the step.
 */
import { Effect } from "effect";

import { ledgerOutputToInsertBatchOp } from "../mpf/ledger-delta.js";
import type { NativeMpfOwnerService } from "../services/mpf-native-owner/protocol.js";
import type { NativeMpfEventOp } from "../services/mpf-native-owner/service.normalize-owner-options.js";
import { ledgerAt } from "./parent-ledger.js";
import type { RebaseTarget } from "./rebase-target.js";

/** A step's MPF mutation: its spends and replaced outputs, then its outputs. */
export type StepEvents = readonly (readonly NativeMpfEventOp[])[];

const promise = <A>(work: () => Promise<A>) =>
  Effect.tryPromise({ try: work, catch: (cause) => cause });

/**
 * The ledger after the first `count` steps of `target` (the frontier's at
 * 0) from `owner`'s ledger store, retained or rebuilt by the processed rows
 * before it.
 */
const ledgerAfter = (
  owner: NativeMpfOwnerService,
  target: RebaseTarget,
  count: number,
) =>
  Effect.gen(function* () {
    const root =
      count === 0
        ? target.landed.frontier.utxosRoot
        : target.steps[count - 1]!.utxosRoot;
    const at = yield* ledgerAt(
      { retainedLedger: (at: string) => promise(() => owner.ledger(at)) },
      root,
      Effect.succeed(target.landed.chain.slice(0, count)),
    );
    if (at.kind === "unavailable")
      return yield* Effect.fail(
        new Error(
          `the rebase cannot read the ledger at root ${root} its next landed block applies to: ${at.detail}`,
        ),
      );
    return at.ledger;
  });

/**
 * The events of `target.steps[index]` against the ledger before it: its
 * spends and every output it produces at an out-ref that ledger already
 * holds, deleted in key order, then its outputs inserted. Only a step that
 * produces outputs reads its parent's ledger, and only at those out-refs.
 */
export const stepEvents = (
  owner: NativeMpfOwnerService,
  target: RebaseTarget,
  index: number,
) =>
  Effect.gen(function* () {
    const step = target.steps[index]!;
    const replaced: string[] = [];
    if (step.produced.length > 0) {
      const parent = yield* ledgerAfter(owner, target, index);
      const read = yield* promise(() =>
        parent.read(step.produced.map((entry) => entry.outref)),
      );
      for (const [outRef, { output }] of read)
        if (output !== undefined) replaced.push(outRef);
    }
    const deletes = [
      ...new Set([
        ...step.spent.map((outRef) => outRef.toString("hex")),
        ...replaced,
      ]),
    ]
      .sort()
      .map((key) => ({
        type: "delete" as const,
        key: Buffer.from(key, "hex"),
      }));
    const inserts = step.produced.map((entry) =>
      ledgerOutputToInsertBatchOp({
        outRef: entry.outref,
        outputCbor: entry.output,
      }),
    );
    return [deletes, inserts].filter(
      (event) => event.length > 0,
    ) satisfies StepEvents;
  });
