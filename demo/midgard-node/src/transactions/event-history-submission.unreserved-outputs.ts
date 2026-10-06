import {
  compareOutRefs,
  outRefLabel,
  parseOutRefLabel,
} from "@al-ft/midgard-core/out-ref";
import type * as SDK from "@al-ft/midgard-sdk";
import type { LucidEvolution, UTxO } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import * as Journal from "../database/eventHistorySubmissions.js";
import type { Database } from "../services/database.js";
import { indexedL1Slot } from "./event-history-submission.indexed-l1-slot.js";

/** The wallet outputs a submission may take as its nonce or funding, in
 * out-ref order: plain outputs without history tokens that no other
 * submission holds in `reserved`. */
export const plainUnreservedOutputs = (
  utxos: readonly UTxO[],
  reserved: ReadonlySet<string>,
  historyPolicyIds: readonly string[],
): UTxO[] =>
  utxos
    .filter(
      (utxo) =>
        !reserved.has(outRefLabel(utxo)) &&
        utxo.datum == null &&
        utxo.datumHash == null &&
        utxo.scriptRef == null &&
        !Object.keys(utxo.assets).some((unit) =>
          historyPolicyIds.some((policyId) => unit.startsWith(policyId)),
        ),
    )
    .sort(compareOutRefs);

/** The first wallet output `plainUnreservedOutputs` admits as a fresh nonce,
 * only `nonceInput` when it is given. Run it within `Journal.choosingNonce`. */
export const unreservedNonce = <E>({
  lucid,
  walletAddress,
  historyPolicyIds,
  tipSlot,
  nonceInput,
  wrap,
}: {
  readonly lucid: LucidEvolution;
  readonly walletAddress: string;
  readonly historyPolicyIds: readonly string[];
  readonly tipSlot: number | undefined;
  readonly nonceInput: Pick<UTxO, "txHash" | "outputIndex"> | undefined;
  readonly wrap: (cause: unknown) => E;
}) =>
  Effect.gen(function* () {
    const reserved = yield* Journal.reservedInputs(walletAddress, tipSlot);
    const candidates = yield* Effect.tryPromise({
      try: () => lucid.utxosAt(walletAddress),
      catch: wrap,
    });
    return plainUnreservedOutputs(candidates, reserved, historyPolicyIds).find(
      (utxo) =>
        nonceInput === undefined ||
        outRefLabel(nonceInput) === outRefLabel(utxo),
    );
  });

/** The wallet outputs `plainUnreservedOutputs` offers as funding, and those
 * of them another submission held when read. The reservations are read
 * before the wallet, so an output a holder releases after this read counts as
 * held even though its holder's transaction may have spent it. */
export const fundingOutputs = async ({
  lucid,
  walletAddress,
  historyPolicyIds,
  run,
}: {
  readonly lucid: LucidEvolution;
  readonly walletAddress: string;
  readonly historyPolicyIds: readonly string[];
  readonly run: <A, E>(effect: Effect.Effect<A, E, Database>) => Promise<A>;
}) => {
  const reserved = await run(
    Journal.reservedInputs(walletAddress, await run(indexedL1Slot(lucid))),
  );
  const offered = plainUnreservedOutputs(
    await lucid.utxosAt(walletAddress),
    await run(Journal.reservedNonces(walletAddress)),
    historyPolicyIds,
  );
  const held: ReadonlySet<string> = new Set(
    offered.map(outRefLabel).filter((outRef) => reserved.has(outRef)),
  );
  return { offered, held };
};

/** Whether the provider still shows every input of a completed attempt,
 * collateral included, as unspent. */
export const inputsUnspent = async (
  lucid: LucidEvolution,
  attempt: SDK.EventHistorySubmissionAttempt,
) => {
  const { inputs } = Journal.attemptSpend(attempt);
  const unspent = new Set(
    (await lucid.utxosByOutRef(inputs.map(parseOutRefLabel))).map(outRefLabel),
  );
  return inputs.every((input) => unspent.has(input));
};
