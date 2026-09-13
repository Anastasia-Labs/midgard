import { encodeMidgardSpendInputItem } from "@al-ft/midgard-core/codec";
import {
  canonicalCommittedWithdrawalTransitionEffect,
  deriveCanonicalDepositTransitionEffect,
} from "@al-ft/midgard-validation";
import { CML, coreToTxOutput, Data } from "@lucid-evolution/lucid";

import {
  assertWatcherLocalUserEventAuthorityCurrent,
  readWatcherLocalUserEventAuthority,
  type WatcherLocalUserEventAuthority,
} from "../indexers/user-event-indexer.js";
import type { WatcherBlockReplayEventAuthority } from "./block-replay.js";
import {
  bindWatcherOriginEventClaim,
  type WatcherCommittedEventClaim,
} from "./event-claims.js";
import { watcherPhaseAQueuedTxs } from "./phase-a-verifier.js";

/** Adapts an actual local event capability to W25's canonical replay input.
 * The caller acquires the capability at the authenticated header cutoff. This
 * helper grants no authority to the claim or material: W25 rereads the private
 * capability and independently binds both to its own DA reconstruction.
 */
export const deriveWatcherLocalEventReplayAuthority = async (
  input: Readonly<{
    localUserEvent: WatcherLocalUserEventAuthority;
    committedClaim: WatcherCommittedEventClaim;
    programMaterial: readonly (readonly [string, string])[];
  }>,
): Promise<WatcherBlockReplayEventAuthority> => {
  const localUserEvent = input.localUserEvent;
  const claim = structuredClone(input.committedClaim);
  const programMaterial = structuredClone(input.programMaterial);
  const local = await readWatcherLocalUserEventAuthority(localUserEvent);
  const bound = bindWatcherOriginEventClaim(local.event, claim);
  let authority: WatcherBlockReplayEventAuthority;
  if (bound.phase === "Deposit") {
    if (claim.canonicalNativeTxCborHex !== null)
      throw new Error("deposit claim carries forced native transaction bytes");
    authority = {
      localUserEvent,
      phase: "Deposit",
      eventKey: { DepositEventKey: { deposit_id: bound.origin.id } },
      transitionEffect: deriveCanonicalDepositTransitionEffect({
        configuredNetwork: local.network,
        eventId: bound.origin.id,
        l2NetworkId: bound.origin.info.l2_network_id,
        l2Address: bound.origin.info.l2_address,
        l2DatumCbor:
          bound.origin.info.l2_datum === null
            ? null
            : Buffer.from(Data.to(bound.origin.info.l2_datum), "hex"),
        l1Assets: coreToTxOutput(
          CML.TransactionOutput.from_cbor_hex(local.event.outputCborHex),
        ).assets,
        depositPolicyId: local.event.policyId,
        depositAssetNameHex: local.event.assetNameHex,
      }),
    };
  } else if (bound.phase === "Withdrawal") {
    if (claim.canonicalNativeTxCborHex !== null)
      throw new Error(
        "withdrawal claim carries forced native transaction bytes",
      );
    const outRef = bound.origin.info.body.l2_outref;
    authority = {
      localUserEvent,
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
    // Reuse W24's canonical reachable-material projection. If canonical
    // verification rejects, that existing reshaper retains the block material
    // so W25 can adjudicate the forced-invalid outcome itself.
    const queued = watcherPhaseAQueuedTxs({
      transactions: [
        {
          txId: bound.origin.tx.tx_id,
          txCbor: canonicalNativeTxCbor,
          sourceKind: "forced",
        },
      ],
      programMaterial,
    })[0]!;
    authority = {
      localUserEvent,
      phase: "ForcedTransaction",
      eventKey: { ForcedTransactionEventKey: { tx_order_id: bound.origin.id } },
      canonicalNativeTxCbor,
      programMaterialSidecarCbor: Buffer.from(
        queued.programMaterialSidecarCbor!,
      ),
    };
  }
  assertWatcherLocalUserEventAuthorityCurrent(localUserEvent);
  return Object.freeze(authority);
};
