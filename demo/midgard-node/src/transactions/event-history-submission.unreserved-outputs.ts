import { compareOutRefs, outRefLabel } from "@al-ft/midgard-core/out-ref";
import type { LucidEvolution, UTxO } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import * as Journal from "../database/eventHistorySubmissions.js";

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
