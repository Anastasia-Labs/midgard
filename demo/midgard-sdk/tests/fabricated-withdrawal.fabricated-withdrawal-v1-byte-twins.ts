import { aikenSerialisedPlutusDataCbor } from "@al-ft/midgard-core/plutus-data-cbor";
import { aikenSerialisedPlutusDataCborPreservingMapOrder } from "@al-ft/midgard-core/plutus-data-cbor";
import { Data } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import { OutputReference } from "../src/common.js";
import {
  committedWithdrawalKeyBytes,
  committedWithdrawalValueBytes,
  FABRICATED_WITHDRAWAL_FRAUD_CATEGORY_ID,
  FabricatedWithdrawalStep02State as FabricatedWithdrawalStep02StateType,
  FabricatedWithdrawalStep03State as FabricatedWithdrawalStep03StateType,
  FabricatedWithdrawalStep04State as FabricatedWithdrawalStep04StateType,
  fabricatedWithdrawalThreadTokenAssetName,
  isFabricatedWithdrawalFault,
  withdrawalContentBytes,
  withdrawalContentCommitment,
  withdrawalEventDatumBytes,
  withdrawalEventDatumCommitment,
  withdrawalEventNonce,
} from "../src/fraud-proof/fabricated-withdrawal.js";
import { FabricatedWithdrawalAuthenticContentOpening } from "../src/fraud-proof/fabricated-withdrawal.js";
import { WithdrawalInfo as WithdrawalInfoType } from "../src/ledger-state.js";
import {
  commitCountedRootProgram,
  ROOT_DOMAINS,
} from "../src/transition-trace.js";
import { opensEventHistoryCommitment } from "../src/user-events/history-proof.js";
import {
  AU_WITHDRAWALS_PHAS_ROOT,
  AU_WITHDRAWALS_ROOT,
  AUTHENTIC_INCLUSION_TIME,
  AUTHENTIC_WITHDRAWAL_EVENT_DATUM,
  AUTHENTIC_WITHDRAWAL_ID,
  AUTHENTIC_WITHDRAWAL_INFO,
  DATUM_AUTHENTIC_WITHDRAWAL_EVENT,
  DIVERTED_WITHDRAWAL_INFO,
  FABRICATED_WITHDRAWAL_ID,
  FI_HEADER_HASH,
  FI_STEP_02_STATE_CBOR,
  FI_STEP_03_STATE_CBOR,
  FI_STEP_04_STATE_CBOR,
  FI_THREAD_TOKEN_ASSET_NAME,
  FI_WITHDRAWALS_PHAS_ROOT,
  FI_WITHDRAWALS_ROOT,
  fiStep02State,
  fiStep03State,
  fiStep04State,
  FORGED_SIGNATURE_WITHDRAWAL_INFO,
  HASH_AUTHENTIC_WITHDRAWAL_CONTENT,
  HASH_AUTHENTIC_WITHDRAWAL_EVENT_DATUM,
  HASH_DIVERTED_WITHDRAWAL_CONTENT,
  HASH_FORGED_SIGNATURE_WITHDRAWAL_CONTENT,
  HEADER_END_TIME,
  HEADER_START_TIME,
  HISTORY_COMMITMENT,
  HISTORY_OPENING_CBOR,
  HISTORY_PAYLOAD,
  KEY_AUTHENTIC_WITHDRAWAL_ID,
  KEY_FABRICATED_WITHDRAWAL_ID,
  MM_HEADER_HASH,
  MM_STEP_02_STATE_CBOR,
  MM_STEP_03_STATE_CBOR,
  MM_STEP_04_STATE_CBOR,
  MM_THREAD_TOKEN_ASSET_NAME,
  MM_WITHDRAWALS_PHAS_ROOT,
  MM_WITHDRAWALS_ROOT,
  mmStep02State,
  mmStep03State,
  mmStep04State,
  NONCE_AUTHENTIC_WITHDRAWAL_ID,
  ORIGINAL_ASSETS,
  REVALIDATED_WITHDRAWAL_INFO,
  VALUE_AUTHENTIC_WITHDRAWAL_INFO,
} from "./fabricated-withdrawal.authentic-withdrawal-info.js";

describe("fabricated-withdrawal v1 byte twins", () => {
  it("encodes the committed withdrawal leaf key exactly as Aiken serialises a WithdrawalId", () => {
    expect(committedWithdrawalKeyBytes(AUTHENTIC_WITHDRAWAL_ID)).toBe(
      KEY_AUTHENTIC_WITHDRAWAL_ID,
    );
    expect(committedWithdrawalKeyBytes(FABRICATED_WITHDRAWAL_ID)).toBe(
      KEY_FABRICATED_WITHDRAWAL_ID,
    );
    // A `WithdrawalId` carries no map, so the key is the one place where the raw
    // Lucid encoding already agrees with `serialiseData`.
    expect(Data.to(AUTHENTIC_WITHDRAWAL_ID, OutputReference)).toBe(
      KEY_AUTHENTIC_WITHDRAWAL_ID,
    );
  });

  it("encodes the committed withdrawal leaf value exactly as Aiken does, and only after normalising Lucid's indefinite maps", () => {
    expect(committedWithdrawalValueBytes(AUTHENTIC_WITHDRAWAL_INFO)).toBe(
      VALUE_AUTHENTIC_WITHDRAWAL_INFO,
    );
    // The reason the helper normalises: Lucid writes the `l2_value` map in
    // indefinite form, which is *not* what the on-chain `cbor.serialise` of the
    // same typed leaf produces, so the un-normalised bytes would be a leaf no
    // step could reproduce.
    const rawLucid = Data.to(AUTHENTIC_WITHDRAWAL_INFO, WithdrawalInfoType);
    expect(rawLucid).not.toBe(VALUE_AUTHENTIC_WITHDRAWAL_INFO);
    expect(rawLucid).toContain("bf");
    expect(aikenSerialisedPlutusDataCbor(rawLucid)).toBe(
      VALUE_AUTHENTIC_WITHDRAWAL_INFO,
    );
  });

  it("commits body and signature fidelity — and nothing else — in one 32-byte hash, exactly as Aiken does", () => {
    expect(
      Effect.runSync(withdrawalContentCommitment(AUTHENTIC_WITHDRAWAL_INFO)),
    ).toBe(HASH_AUTHENTIC_WITHDRAWAL_CONTENT);
    expect(
      Effect.runSync(withdrawalContentCommitment(DIVERTED_WITHDRAWAL_INFO)),
    ).toBe(HASH_DIVERTED_WITHDRAWAL_CONTENT);
    expect(
      Effect.runSync(
        withdrawalContentCommitment(FORGED_SIGNATURE_WITHDRAWAL_INFO),
      ),
    ).toBe(HASH_FORGED_SIGNATURE_WITHDRAWAL_CONTENT);
    // Each fabrication of an L1-owned field is distinguishable from the authentic
    // order and from the other, which is what lets one inequality settle both.
    expect(
      new Set([
        HASH_AUTHENTIC_WITHDRAWAL_CONTENT,
        HASH_DIVERTED_WITHDRAWAL_CONTENT,
        HASH_FORGED_SIGNATURE_WITHDRAWAL_CONTENT,
      ]).size,
    ).toBe(3);
    // Decision 0007: the committed `validity` verdict is the operator's own, so a
    // leaf that differs from the authentic order only in its verdict commits the
    // very same content and cannot be a fabrication.
    expect(
      Effect.runSync(withdrawalContentCommitment(REVALIDATED_WITHDRAWAL_INFO)),
    ).toBe(HASH_AUTHENTIC_WITHDRAWAL_CONTENT);
    expect(withdrawalContentBytes(REVALIDATED_WITHDRAWAL_INFO)).toBe(
      withdrawalContentBytes(AUTHENTIC_WITHDRAWAL_INFO),
    );
    // The leaf value the header commits still carries the verdict, so the two
    // blocks are distinguishable — this family simply does not judge that field.
    expect(committedWithdrawalValueBytes(REVALIDATED_WITHDRAWAL_INFO)).not.toBe(
      committedWithdrawalValueBytes(AUTHENTIC_WITHDRAWAL_INFO),
    );
  });

  it("derives the withdrawal event NFT nonce exactly as Aiken's out_ref_to_nonce does", () => {
    expect(Effect.runSync(withdrawalEventNonce(AUTHENTIC_WITHDRAWAL_ID))).toBe(
      NONCE_AUTHENTIC_WITHDRAWAL_ID,
    );
  });

  it("encodes the authentic withdrawal event datum and step-02's retained commitment exactly as Aiken does", () => {
    expect(withdrawalEventDatumBytes(AUTHENTIC_WITHDRAWAL_EVENT_DATUM)).toBe(
      DATUM_AUTHENTIC_WITHDRAWAL_EVENT,
    );
    expect(
      Effect.runSync(
        withdrawalEventDatumCommitment(AUTHENTIC_WITHDRAWAL_EVENT_DATUM),
      ),
    ).toBe(HASH_AUTHENTIC_WITHDRAWAL_EVENT_DATUM);
    // The event datum's five fields include the refund path, so the commitment
    // covers material the leaf value does not.
    expect(DATUM_AUTHENTIC_WITHDRAWAL_EVENT).toContain(
      VALUE_AUTHENTIC_WITHDRAWAL_INFO.slice(6),
    );
  });

  it("commits the challenged headers' counted withdrawals_root and thread token asset names exactly as Aiken does", () => {
    expect(
      Effect.runSync(
        commitCountedRootProgram({
          domain: ROOT_DOMAINS.withdrawals,
          phasRoot: FI_WITHDRAWALS_PHAS_ROOT,
          count: 1n,
        }),
      ),
    ).toBe(FI_WITHDRAWALS_ROOT);
    expect(
      Effect.runSync(
        commitCountedRootProgram({
          domain: ROOT_DOMAINS.withdrawals,
          phasRoot: MM_WITHDRAWALS_PHAS_ROOT,
          count: 1n,
        }),
      ),
    ).toBe(MM_WITHDRAWALS_ROOT);
    expect(
      Effect.runSync(
        commitCountedRootProgram({
          domain: ROOT_DOMAINS.withdrawals,
          phasRoot: AU_WITHDRAWALS_PHAS_ROOT,
          count: 1n,
        }),
      ),
    ).toBe(AU_WITHDRAWALS_ROOT);
    expect(FABRICATED_WITHDRAWAL_FRAUD_CATEGORY_ID).toBe("0000000c");
    expect(fabricatedWithdrawalThreadTokenAssetName(FI_HEADER_HASH)).toBe(
      FI_THREAD_TOKEN_ASSET_NAME,
    );
    expect(fabricatedWithdrawalThreadTokenAssetName(MM_HEADER_HASH)).toBe(
      MM_THREAD_TOKEN_ASSET_NAME,
    );
  });

  it("encodes the step-01 to step-02 and step-02 to step-03 handoffs exactly as the Aiken validators produce them", () => {
    expect(Data.to(fiStep02State, FabricatedWithdrawalStep02StateType)).toBe(
      FI_STEP_02_STATE_CBOR,
    );
    expect(Data.to(mmStep02State, FabricatedWithdrawalStep02StateType)).toBe(
      MM_STEP_02_STATE_CBOR,
    );
    expect(Data.to(fiStep03State, FabricatedWithdrawalStep03StateType)).toBe(
      FI_STEP_03_STATE_CBOR,
    );
    expect(Data.to(mmStep03State, FabricatedWithdrawalStep03StateType)).toBe(
      MM_STEP_03_STATE_CBOR,
    );
    // The two verdicts are different constructors of the same enum, so the
    // step-03 opening that pairs with one cannot be re-used against the other.
    expect(FI_STEP_03_STATE_CBOR).not.toBe(MM_STEP_03_STATE_CBOR);
    expect(FI_STEP_03_STATE_CBOR.endsWith("d87980ff")).toBe(true);
    expect(MM_STEP_03_STATE_CBOR).toContain("d87a9f");
  });

  it("encodes the step-03 to step-04 handoff and settles the fault rule exactly as the Aiken step-04 validator does", () => {
    expect(Data.to(fiStep04State, FabricatedWithdrawalStep04StateType)).toBe(
      FI_STEP_04_STATE_CBOR,
    );
    expect(Data.to(mmStep04State, FabricatedWithdrawalStep04StateType)).toBe(
      MM_STEP_04_STATE_CBOR,
    );
    // The rule twin of `fabricated_withdrawal_fault_is_established_v1`.
    expect(isFabricatedWithdrawalFault(fiStep04State)).toBe(true);
    expect(isFabricatedWithdrawalFault(mmStep04State)).toBe(true);
    // A header committing exactly the authentic order is not a fault, and an
    // authentic event outside the challenged block's window is not this block's
    // fault, whichever side of the window it falls on.
    expect(
      isFabricatedWithdrawalFault({
        ...mmStep04State,
        fault: {
          MismatchedWithdrawalContent: {
            committed_withdrawal_content_hash:
              HASH_AUTHENTIC_WITHDRAWAL_CONTENT,
            authentic_withdrawal_content_hash:
              HASH_AUTHENTIC_WITHDRAWAL_CONTENT,
            event_inclusion_time: AUTHENTIC_INCLUSION_TIME,
          },
        },
      }),
    ).toBe(false);
    expect(
      isFabricatedWithdrawalFault({
        ...mmStep04State,
        fault: {
          MismatchedWithdrawalContent: {
            committed_withdrawal_content_hash: HASH_DIVERTED_WITHDRAWAL_CONTENT,
            authentic_withdrawal_content_hash:
              HASH_AUTHENTIC_WITHDRAWAL_CONTENT,
            event_inclusion_time: HEADER_END_TIME + 1n,
          },
        },
      }),
    ).toBe(false);
    expect(
      isFabricatedWithdrawalFault({
        ...mmStep04State,
        fault: {
          MismatchedWithdrawalContent: {
            committed_withdrawal_content_hash: HASH_DIVERTED_WITHDRAWAL_CONTENT,
            authentic_withdrawal_content_hash:
              HASH_AUTHENTIC_WITHDRAWAL_CONTENT,
            event_inclusion_time: HEADER_START_TIME,
          },
        },
      }),
    ).toBe(false);
    // A forged signature convicts on the same rule as a diverted body.
    expect(
      isFabricatedWithdrawalFault({
        ...mmStep04State,
        fault: {
          MismatchedWithdrawalContent: {
            committed_withdrawal_content_hash:
              HASH_FORGED_SIGNATURE_WITHDRAWAL_CONTENT,
            authentic_withdrawal_content_hash:
              HASH_AUTHENTIC_WITHDRAWAL_CONTENT,
            event_inclusion_time: AUTHENTIC_INCLUSION_TIME,
          },
        },
      }),
    ).toBe(true);
    // Decision 0007: a leaf whose only difference from the authentic order is its
    // operator-owned `validity` verdict commits the same content, so this family
    // cannot convict it — the honest control survives, and a wrong verdict is
    // `withdrawalMistag`'s fault instead.
    expect(
      Effect.runSync(withdrawalContentCommitment(REVALIDATED_WITHDRAWAL_INFO)),
    ).toBe(HASH_AUTHENTIC_WITHDRAWAL_CONTENT);
    expect(
      isFabricatedWithdrawalFault({
        ...mmStep04State,
        fault: {
          MismatchedWithdrawalContent: {
            committed_withdrawal_content_hash:
              HASH_AUTHENTIC_WITHDRAWAL_CONTENT,
            authentic_withdrawal_content_hash:
              HASH_AUTHENTIC_WITHDRAWAL_CONTENT,
            event_inclusion_time: AUTHENTIC_INCLUSION_TIME,
          },
        },
      }),
    ).toBe(false);
    // The identity fault is a bare constructor and the content fault a
    // three-field one, so a conviction cannot be re-labelled on the wire.
    expect(FI_STEP_04_STATE_CBOR.endsWith("d87980ff")).toBe(true);
    expect(MM_STEP_04_STATE_CBOR).toContain("d87a9f");
  });
});

describe("withdrawal retained history opening", () => {
  it("matches Aiken's complete payload and original Value bytes", () => {
    const opening: FabricatedWithdrawalAuthenticContentOpening = {
      RetainedEventData: {
        payload: HISTORY_PAYLOAD,
        original_assets: ORIGINAL_ASSETS,
      },
    };
    expect(
      aikenSerialisedPlutusDataCborPreservingMapOrder(
        Data.to(opening, FabricatedWithdrawalAuthenticContentOpening),
      ),
    ).toBe(HISTORY_OPENING_CBOR);
    expect(
      opensEventHistoryCommitment(
        HISTORY_COMMITMENT,
        HISTORY_PAYLOAD,
        ORIGINAL_ASSETS,
      ),
    ).toBe(true);
    expect(
      opensEventHistoryCommitment(
        HISTORY_COMMITMENT,
        HISTORY_PAYLOAD,
        new Map([["", new Map([["", 1n]])]]),
      ),
    ).toBe(false);
  });
  it("classifies timing independently of content fidelity", () => {
    for (const time of [HEADER_START_TIME, HEADER_END_TIME + 1n]) {
      expect(
        isFabricatedWithdrawalFault({
          ...mmStep04State,
          fault: { IneligibleWithdrawalEvent: { event_inclusion_time: time } },
        }),
      ).toBe(true);
    }
    expect(
      isFabricatedWithdrawalFault({
        ...mmStep04State,
        fault: {
          IneligibleWithdrawalEvent: {
            event_inclusion_time: AUTHENTIC_INCLUSION_TIME,
          },
        },
      }),
    ).toBe(false);
  });
});
