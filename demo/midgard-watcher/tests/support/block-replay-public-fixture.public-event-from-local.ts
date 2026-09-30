import { plutusConstrFieldCbor } from "@al-ft/midgard-core/plutus-data-cbor";
import * as SDK from "@al-ft/midgard-sdk";
import {
  canonicalCommittedWithdrawalTransitionEffect,
  type CanonicalTransitionEffect,
  deriveCanonicalOriginalDepositTransitionEffect,
} from "@al-ft/midgard-validation";
import {
  makeQueued,
  outRefFromTxId,
} from "@al-ft/midgard-validation/tests/validation-fixtures";
import { Data } from "@lucid-evolution/lucid";

import { type WatcherBlockReplayEventAuthority } from "../../src/verification/block-replay.js";
import { type LocalReplayEvent } from "./block-replay-public-fixture.build-public-replay-fixture.js";
import {
  cardanoOutputAssets,
  dataHex,
  type PublicFixtureEvent,
} from "./block-replay-public-fixture.committed-steps-for-effects.js";
import type { makeForcedTxFixture } from "./forced-submission-fixture.js";

/** The DA-committed claim for a locally published originating event. */
export const publicEventFromLocal = (
  local: LocalReplayEvent,
  options: Readonly<{
    forcedNative?: ReturnType<typeof makeForcedTxFixture>;
    withdrawalValidity?: SDK.WithdrawalValidity;
    forcedVerdict?: SDK.OperatorVerdict;
  }> = {},
): PublicFixtureEvent => {
  const event = local.event;
  const outputReference = Data.from(
    event.eventId,
    SDK.OutputReference as never,
  ) as SDK.OutputReference;
  if (event.kind === "deposit") {
    const decoded = Data.from(event.eventCborHex, SDK.DepositEvent) as {
      readonly info: SDK.DepositInfo;
    };
    return Object.freeze({
      eventKey: { DepositEventKey: { deposit_id: outputReference } },
      phase: "Deposit" as const,
      domain: "deposits" as const,
      entry: [
        event.eventId,
        dataHex(decoded.info, SDK.DepositInfoSchema),
      ] as SDK.DaPayloadEntry,
    });
  }
  if (event.kind === "withdrawal") {
    const decoded = Data.from(event.eventCborHex, SDK.WithdrawalEvent) as {
      readonly info: SDK.WithdrawalInfo;
    };
    return Object.freeze({
      eventKey: { WithdrawalEventKey: { withdrawal_id: outputReference } },
      phase: "Withdrawal" as const,
      domain: "withdrawals" as const,
      entry: [
        event.eventId,
        SDK.committedWithdrawalValueBytes({
          ...decoded.info,
          validity: options.withdrawalValidity ?? decoded.info.validity,
        }),
      ] as SDK.DaPayloadEntry,
    });
  }
  if (options.forcedNative === undefined) {
    throw new Error("forced public event requires canonical native bytes");
  }
  const decoded = Data.from(event.eventCborHex, SDK.TxOrderEvent) as {
    readonly tx: {
      readonly tx_id: string;
      readonly submitted_source: SDK.ForcedTxProofSource;
    };
  };
  return Object.freeze({
    eventKey: { ForcedTransactionEventKey: { tx_order_id: outputReference } },
    phase: "ForcedTransaction" as const,
    domain: "forced_transactions" as const,
    entry: [
      event.eventId,
      dataHex(
        {
          tx_id: decoded.tx.tx_id,
          submitted_source: {
            compact_cbor: decoded.tx.submitted_source.compact_cbor,
            witness_set_compact_cbor:
              decoded.tx.submitted_source.witness_set_compact_cbor,
            field_preimage_lengths_cbor:
              decoded.tx.submitted_source.field_preimage_lengths_cbor,
          },
          verdict: options.forcedVerdict ?? ("ForcedTxValid" as const),
        },
        SDK.ForcedInclusionTxV1Schema,
      ),
    ] as SDK.DaPayloadEntry,
    forcedPreimage: [
      event.eventId,
      options.forcedNative.txCbor.toString("hex"),
    ] as SDK.DaPayloadEntry,
  });
};

/** Header window containing the local event's inclusion time. */
export const localEventWindow = (
  local: LocalReplayEvent,
): Readonly<{ start: bigint; end: bigint }> => {
  const inclusion = BigInt(local.event.inclusionTime);
  return Object.freeze({ start: inclusion - 1n, end: inclusion });
};

export const depositEffectFromLocal = (
  local: LocalReplayEvent,
): CanonicalTransitionEffect => {
  const event = local.event;
  if (event.kind !== "deposit") {
    throw new Error("local authority is not a deposit");
  }
  const decoded = Data.from(event.eventCborHex, SDK.DepositEvent) as {
    readonly id: SDK.OutputReference;
    readonly info: SDK.DepositInfo;
  };
  return deriveCanonicalOriginalDepositTransitionEffect({
    configuredNetwork: local.network,
    eventId: decoded.id,
    l2NetworkId: decoded.info.l2_network_id,
    l2Address: decoded.info.l2_address,
    l2DatumCbor:
      decoded.info.l2_datum === null
        ? null
        : Buffer.from(
            plutusConstrFieldCbor(event.eventCborHex, [1, 2, 0]),
            "hex",
          ),
    // Structural list funds remain on L1 and must not enter the expected L2
    // transition. Derive the fixture independently from the authenticated Order.
    originalAssets: SDK.eventHistoryOriginalAssets(
      Data.from(event.datumCborHex, SDK.EventHistoryNode),
      cardanoOutputAssets(event.outputCborHex),
      event.policyId,
    ),
  });
};

export const withdrawalEffectFromLocal = (
  local: LocalReplayEvent,
  committedValid: boolean,
): CanonicalTransitionEffect => {
  if (local.event.kind !== "withdrawal") {
    throw new Error("local authority is not a withdrawal");
  }
  const decoded = Data.from(local.event.eventCborHex, SDK.WithdrawalEvent) as {
    readonly info: SDK.WithdrawalInfo;
  };
  const outRef = decoded.info.body.l2_outref;
  return canonicalCommittedWithdrawalTransitionEffect({
    committedValid,
    // The Plutus-Data `OutputReference` in the event datum is a *different*
    // encoding from the ledger out-ref; going from one to the other means
    // re-encoding through §5.3's fixed-index field-0/1 item, never CML's
    // minimal-index `TransactionInput` CBOR.
    outRefCbor: outRefFromTxId(
      Buffer.from(outRef.transactionId, "hex"),
      outRef.outputIndex,
    ),
  });
};

export const localEventAuthority = (input: {
  readonly event: PublicFixtureEvent;
  readonly local: LocalReplayEvent;
  readonly effect: CanonicalTransitionEffect;
  readonly forcedNative?: ReturnType<typeof makeForcedTxFixture>;
}): WatcherBlockReplayEventAuthority => {
  const common = {
    eventKey: input.event.eventKey,
    localUserEvent: input.local.localUserEvent,
  };
  if (input.event.phase === "ForcedTransaction") {
    if (input.forcedNative === undefined) {
      throw new Error("forced replay fixture requires canonical native bytes");
    }
    return {
      ...common,
      phase: input.event.phase,
      canonicalNativeTxCbor: input.forcedNative.txCbor,
      programMaterialSidecarCbor: makeQueued(
        input.forcedNative.txId,
        input.forcedNative.txCbor,
      ).programMaterialSidecarCbor,
    };
  }
  return {
    ...common,
    phase: input.event.phase,
    transitionEffect: input.effect,
  };
};
