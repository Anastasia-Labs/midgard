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

import { type FixtureEventAuthority } from "./block-replay-public-fixture.build-public-replay-fixture.js";
import {
  dataHex,
  type PublicFixtureEvent,
} from "./block-replay-public-fixture.committed-steps-for-effects.js";
import type { makeForcedTxFixture } from "./forced-submission-fixture.js";
import type { FixtureUserEvent } from "./user-event-authority-fixture.js";

/** The DA-committed claim for an originating user event. */
export const publicEventFromOrigin = (
  origin: FixtureUserEvent,
  options: Readonly<{
    forcedNative?: ReturnType<typeof makeForcedTxFixture>;
    withdrawalValidity?: SDK.WithdrawalValidity;
    forcedVerdict?: SDK.OperatorVerdict;
  }> = {},
): PublicFixtureEvent => {
  const event = origin.event;
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

/** Header window containing the originating event's inclusion time. */
export const originEventWindow = (
  origin: FixtureUserEvent,
): Readonly<{ start: bigint; end: bigint }> => {
  const inclusion = BigInt(origin.event.inclusionTime);
  return Object.freeze({ start: inclusion - 1n, end: inclusion });
};

export const depositEffectFromOrigin = (
  origin: FixtureUserEvent,
): CanonicalTransitionEffect => {
  const event = origin.event;
  if (event.kind !== "deposit" || origin.originalAssets === null) {
    throw new Error("originating event is not a deposit");
  }
  const decoded = Data.from(event.eventCborHex, SDK.DepositEvent) as {
    readonly id: SDK.OutputReference;
    readonly info: SDK.DepositInfo;
  };
  return deriveCanonicalOriginalDepositTransitionEffect({
    configuredNetwork: origin.network,
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
    // The assets the fixture deposited, held apart from the event's encoded
    // copy so the expected effect does not come from the bytes under test.
    originalAssets: origin.originalAssets,
  });
};

export const withdrawalEffectFromOrigin = (
  origin: FixtureUserEvent,
  committedValid: boolean,
): CanonicalTransitionEffect => {
  if (origin.event.kind !== "withdrawal") {
    throw new Error("originating event is not a withdrawal");
  }
  const decoded = Data.from(origin.event.eventCborHex, SDK.WithdrawalEvent) as {
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

/** The replay event authority for `origin`, before the header binds it. */
export const originEventAuthority = (input: {
  readonly event: PublicFixtureEvent;
  readonly origin: FixtureUserEvent;
  readonly effect: CanonicalTransitionEffect;
  readonly forcedNative?: ReturnType<typeof makeForcedTxFixture>;
}): FixtureEventAuthority => {
  const common = {
    eventKey: input.event.eventKey,
    origin: input.origin,
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
