/** A submitted L2 withdrawal of a chronology seed output. The seed's owner
 * signs the body, the public SDK user-event builder admits the order to the
 * applied withdrawal list, and the authenticated list read feeds the node's
 * own ingestion, classification, root-entry, event-key and transition-effect
 * code, so the committed event is exactly what block production derives. */
import * as SDK from "@al-ft/midgard-sdk";
import { canonicalCommittedWithdrawalTransitionEffect } from "@al-ft/midgard-validation";
import { Effect } from "effect";
import { withdrawalDataToEntry } from "midgard-node/database/user-event-entries";
import * as Ledger from "midgard-node/database/utils/ledger";
import * as WithdrawalsDB from "midgard-node/database/withdrawals";
import { withdrawalTraceEventKey } from "midgard-node/mpf/trace-events";
import {
  classifyWithdrawalsInOrder,
  indexSelectedLedgerOutputs,
} from "midgard-node/workers/utils/mpf/withdrawal-classification";
import { expect } from "vitest";

import type { LedgerEntry } from "../../../src/ledger-at.js";
import type { CommittedChronologyWithdrawal } from "../../helpers/input-existence-chronology-fixture.js";
import {
  CHRONOLOGY_OWNER_KEY_HASH,
  CHRONOLOGY_SEED_VALUE,
  signChronologyWithdrawal,
} from "../../helpers/input-existence-chronology-seed.js";
import type { prepareFamilyHistory } from "./family-history.js";
import type { makeFaultProofEmulatorHarness } from "./harness.js";

type Harness = Awaited<ReturnType<typeof makeFaultProofEmulatorHarness>>;
type FamilyHistory = Awaited<ReturnType<typeof prepareFamilyHistory>>;

/** The applied lists as the public user-event builders address them. */
const userHistoryContracts = (
  family: FamilyHistory,
): SDK.UserHistoryContracts => {
  const histories = family.applied.map(
    (a, i): SDK.EventHistoryContracts => ({
      recipe: family.recipes[i]!,
      list: {
        ...SDK.makeAuthenticatedValidator(
          "Custom",
          a.validator.script,
          a.validator.script,
        ),
        ...SDK.makeWithdrawalValidator(a.validator.script),
      },
      retention: SDK.makeSpendingValidator(
        "Custom",
        a.retention.validator.script,
      ),
      retirement: SDK.makeWithdrawalValidator(a.retirement.validator.script),
    }),
  );
  return {
    eventHistory: { deposit: histories[0]!, withdrawal: histories[1]! },
    deposit: histories[0]!.list,
    withdrawal: histories[1]!.list,
    hubOracle: family.contracts.hubOracle,
  };
};

/**
 * Submit the seed owner's withdrawal of `l2OutRef` through the public SDK
 * builder, valid until `validTo`, and read it back from the authenticated
 * list. The order's inclusion time is `validTo - 1 + EVENT_WAIT_DURATION_MS`.
 */
export const submitChronologyWithdrawal = async ({
  h,
  family,
  l2OutRef,
  validTo,
}: {
  readonly h: Harness;
  readonly family: FamilyHistory;
  readonly l2OutRef: SDK.OutputReference;
  readonly validTo: number;
}): Promise<SDK.WithdrawalUTxO> => {
  const owner: SDK.AddressData = {
    paymentCredential: { PublicKeyCredential: [CHRONOLOGY_OWNER_KEY_HASH] },
    stakeCredential: null,
  };
  const body: SDK.WithdrawalBody = {
    l2_outref: l2OutRef,
    l2_owner: CHRONOLOGY_OWNER_KEY_HASH,
    l2_value: new Map([["", new Map([["", CHRONOLOGY_SEED_VALUE]])]]),
    l1_address: owner,
    l1_datum: "NoDatum",
  };
  const contracts = userHistoryContracts(family);
  const nonce = family.nonce("Withdrawal");
  const built = await Effect.runPromise(
    SDK.buildUnsignedWithdrawalTxWithMetadataProgram(h.proverLucid, contracts, {
      body,
      signature: signChronologyWithdrawal(body),
      refundAddress: owner,
      refundDatum: "NoDatum",
      nonceInput: nonce,
      validity: { validFrom: h.emulator.now() - 60_000, validTo },
      referenceScripts: { withdrawalMinting: family.scripts[1]! },
    }),
  );
  const txHash = await family.submit("admit-chronology-withdrawal", built.tx);
  const orders = await Effect.runPromise(
    SDK.fetchWithdrawalUTxOsProgram(
      h.proverLucid,
      SDK.eventHistoryDeploymentFromContracts(
        contracts.eventHistory!.withdrawal,
      ),
    ),
  );
  expect(orders).toHaveLength(1);
  const [order] = orders;
  expect(order!.utxo.txHash).toBe(txHash);
  expect(order!.event.id).toEqual({
    transactionId: nonce.txHash,
    outputIndex: BigInt(nonce.outputIndex),
  });
  expect(order!.event.info.body).toEqual(body);
  expect(order!.inclusionTime.getTime()).toBe(built.metadata.inclusionTime);
  return order!;
};

/**
 * The committed form of an authenticated withdrawal order against the
 * block's base ledger: the node's ingestion row, its ordered classification,
 * withdrawals-root entry, transition event key and ledger effect.
 */
export const commitChronologyWithdrawal = (
  order: SDK.WithdrawalUTxO,
  baseLedger: readonly LedgerEntry[],
): Promise<
  CommittedChronologyWithdrawal &
    Readonly<{ validity: WithdrawalsDB.Validity; inclusionTime: Date }>
> =>
  Effect.runPromise(
    Effect.gen(function* () {
      const entry = yield* withdrawalDataToEntry({
        ...order,
        location: order.utxo,
        payloadCbor: order.history.payloadCbor,
      });
      const [classified] = yield* classifyWithdrawalsInOrder({
        entries: [entry],
        baseOutputs: yield* indexSelectedLedgerOutputs(
          baseLedger.map((ledgerEntry) => ({
            [Ledger.Columns.OUTREF]: ledgerEntry.outRef,
            [Ledger.Columns.OUTPUT]: ledgerEntry.output,
          })),
        ),
      });
      if (classified === undefined)
        throw new Error("withdrawal classification returned no row");
      const committed: WithdrawalsDB.Entry = {
        ...classified.entry,
        [WithdrawalsDB.Columns.SETTLEMENT_EVENT_INFO]:
          classified.settlementEventInfo,
        [WithdrawalsDB.Columns.VALIDITY]: classified.validity,
        [WithdrawalsDB.Columns.VALIDITY_DETAIL]: classified.validityDetail,
      };
      const root = yield* WithdrawalsDB.toRootKeyValue(committed);
      return {
        eventKey: yield* withdrawalTraceEventKey(committed),
        rootKey: root.key,
        rootValue: root.value,
        effect: canonicalCommittedWithdrawalTransitionEffect({
          committedValid:
            classified.validity === WithdrawalsDB.Validity.WithdrawalIsValid,
          outRefCbor: yield* WithdrawalsDB.toLedgerOutRef(committed),
        }),
        validity: classified.validity,
        inclusionTime: committed[WithdrawalsDB.Columns.INCLUSION_TIME],
      };
    }),
  );
