import { encodeMidgardSpendInputItem } from "@al-ft/midgard-core/codec";
import { plutusConstrFieldCbor } from "@al-ft/midgard-core/plutus-data-cbor";
import {
  canonicalCommittedWithdrawalTransitionEffect,
  deriveCanonicalOriginalDepositTransitionEffect,
} from "@al-ft/midgard-validation";

import type { WatcherBlockReplayEventAuthority } from "./block-replay.js";
import {
  bindWatcherOriginEventClaim,
  type WatcherCommittedEventClaim,
} from "./event-claims.js";
import {
  assertWatcherUserEventAuthorityCurrent,
  readWatcherUserEventAuthority,
  watcherOriginalDepositAssets,
  type WatcherUserEventAuthority,
} from "./user-event.js";

/** Adapts a follower-read user-event capability to W25's canonical replay
 * input. The caller acquires the capability at the authenticated header cutoff. This
 * helper grants no authority to the claim: W25 rereads the private
 * capability and independently binds the claim to its own DA reconstruction.
 */
export const deriveWatcherUserEventReplayAuthority = async (
  input: Readonly<{
    userEvent: WatcherUserEventAuthority;
    committedClaim: WatcherCommittedEventClaim;
  }>,
): Promise<WatcherBlockReplayEventAuthority> => {
  const userEvent = input.userEvent;
  const claim = structuredClone(input.committedClaim);
  const local = await readWatcherUserEventAuthority(userEvent);
  const bound = bindWatcherOriginEventClaim(local.event, claim);
  let authority: WatcherBlockReplayEventAuthority;
  if (bound.phase === "Deposit") {
    if (claim.canonicalNativeTxCborHex !== null)
      throw new Error("deposit claim carries forced native transaction bytes");
    authority = {
      userEvent,
      phase: "Deposit",
      eventKey: { DepositEventKey: { deposit_id: bound.origin.id } },
      transitionEffect: deriveCanonicalOriginalDepositTransitionEffect({
        configuredNetwork: local.network,
        eventId: bound.origin.id,
        l2NetworkId: bound.origin.info.l2_network_id,
        l2Address: bound.origin.info.l2_address,
        l2DatumCbor:
          bound.origin.info.l2_datum === null
            ? null
            : Buffer.from(
                plutusConstrFieldCbor(local.event.eventCborHex, [1, 2, 0]),
                "hex",
              ),
        originalAssets: watcherOriginalDepositAssets(local.event),
      }),
    };
  } else if (bound.phase === "Withdrawal") {
    if (claim.canonicalNativeTxCborHex !== null)
      throw new Error(
        "withdrawal claim carries forced native transaction bytes",
      );
    const outRef = bound.origin.info.body.l2_outref;
    authority = {
      userEvent,
      phase: "Withdrawal",
      eventKey: { WithdrawalEventKey: { withdrawal_id: bound.origin.id } },
      transitionEffect: canonicalCommittedWithdrawalTransitionEffect({
        committedValid: bound.committed.validity === "WithdrawalIsValid",
        outRefCbor: encodeMidgardSpendInputItem({
          txId: Buffer.from(outRef.transactionId, "hex"),
          outputIndex: Number(outRef.outputIndex),
        }),
      }),
    };
  } else {
    if (
      claim.canonicalNativeTxCborHex === null ||
      !/^(?:[0-9a-f]{2})+$/u.test(claim.canonicalNativeTxCborHex)
    )
      throw new Error("forced claim is missing exact native transaction bytes");
    const canonicalNativeTxCbor = Buffer.from(
      claim.canonicalNativeTxCborHex,
      "hex",
    );
    authority = {
      userEvent,
      phase: "ForcedTransaction",
      eventKey: { ForcedTransactionEventKey: { tx_order_id: bound.origin.id } },
      canonicalNativeTxCbor,
    };
  }
  assertWatcherUserEventAuthorityCurrent(userEvent);
  return Object.freeze(authority);
};
