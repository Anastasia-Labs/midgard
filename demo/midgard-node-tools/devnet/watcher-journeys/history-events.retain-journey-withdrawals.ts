import { encodeMidgardSpendInputItem } from "@al-ft/midgard-core";
import { wrapDaPayload } from "@al-ft/midgard-core/da-payload-envelope";
import {
  aikenSerialisedPlutusDataCborPreservingMapOrder as canonical,
  plutusConstrFieldCbor,
  replacePlutusConstrFieldCbor,
} from "@al-ft/midgard-core/plutus-data-cbor";
import { buildCountedRoot } from "@al-ft/midgard-fault-proofs";
import { buildCanonicalBlockFixture } from "@al-ft/midgard-fault-proofs/test-support/canonical-block-evidence-fixture";
import { depositEventsRetainedBlock } from "@al-ft/midgard-fault-proofs/test-support/transition-trace-retained";
import * as SDK from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import { type HistoryPredecessor } from "./history-cases.js";

export type HistoryEventBlockInput = {
  predecessor: HistoryPredecessor;
  operatorVkey: string;
  endTime: bigint;
  blockSlot: bigint;
};

/** An event resolved by the shared authenticated history reader. */
export type StagedHistoryEvent = {
  order: SDK.DepositUTxO | SDK.WithdrawalUTxO;
  policyId: string;
};

/** Canonical bytes survive journey JSON persistence, including arbitrary Data maps. */
export type CapturedEventHistoryWitness = {
  commitmentCbor: string;
  openingCbor: string;
};

export const authenticateEvent = (input: StagedHistoryEvent) => {
  const captured = SDK.captureEventHistoryWitness(
    input.order.history,
    input.policyId,
    input.order.kind,
  );
  const canonicalEvent =
    "DepositPayload" in captured.payload
      ? captured.payload.DepositPayload.event
      : captured.payload.WithdrawalPayload.event;
  const schema =
    input.order.kind === "Deposit" ? SDK.DepositEvent : SDK.WithdrawalEvent;
  if (
    Data.to(canonicalEvent, schema) !== Data.to(input.order.event, schema) ||
    input.order.idCbor.toString("hex") !==
      canonical(plutusConstrFieldCbor(captured.payloadCbor, [0, 0])) ||
    input.order.infoCbor.toString("hex") !==
      canonical(plutusConstrFieldCbor(captured.payloadCbor, [0, 1])) ||
    input.order.history.anchor.utxo.datum == null ||
    captured.factsCbor !==
      canonical(
        plutusConstrFieldCbor(input.order.history.anchor.utxo.datum, [3, 0]),
      )
  )
    throw new Error(
      "Staged history event content differs from its authenticated Order",
    );
  if (
    !SDK.assetsEqual(
      SDK.valueToAssets(captured.originalAssets),
      input.order.originalAssets,
    ) ||
    Data.to(captured.commitment.event_id, SDK.OutputReference) !==
      Data.to(input.order.event.id, SDK.OutputReference) ||
    captured.commitment.inclusion_time !== input.order.facts.inclusion_time
  )
    throw new Error(
      "Staged history event differs from its authenticated Order",
    );
  return input.order;
};

/** Content capture only. It preserves facts already authenticated by the reader;
 * later reads of this archive do not create new L1 observation authority. */
export const captureStagedHistoryEvent = (
  input: StagedHistoryEvent,
): CapturedEventHistoryWitness => {
  authenticateEvent(input);
  const captured = SDK.captureEventHistoryWitness(
    input.order.history,
    input.policyId,
    input.order.kind,
  );
  return {
    commitmentCbor: Data.to(captured.commitment, SDK.EventHistoryCommitment),
    openingCbor: canonical(captured.openingCbor),
  };
};

export const requireEventWindow = (
  time: bigint,
  input: HistoryEventBlockInput,
) => {
  if (time <= input.predecessor.header.endTime || time > input.endTime)
    throw new Error(
      "Staged history event is outside the committed block interval",
    );
};

/** Divert the committed deposit, preserving its actual L1 identity and witness. */
export const buildJourneyFabricatedDeposit = async (
  input: HistoryEventBlockInput & {
    deposit: StagedHistoryEvent;
    honest?: boolean;
  },
) => {
  const datum = authenticateEvent(input.deposit);
  if (datum.kind !== "Deposit")
    throw new Error("Deposit journey requires an authenticated deposit Order");
  requireEventWindow(datum.facts.inclusion_time, input);
  const payment = datum.event.info.l2_address.paymentCredential;
  const hash =
    "PublicKeyCredential" in payment
      ? payment.PublicKeyCredential[0]
      : payment.ScriptCredential[0];
  const divertedHash = (hash.startsWith("00") ? "01" : "00") + hash.slice(2);
  const committed = input.honest
    ? datum
    : {
        ...datum,
        event: {
          ...datum.event,
          info: {
            ...datum.event.info,
            l2_address: {
              ...datum.event.info.l2_address,
              paymentCredential: {
                PublicKeyCredential: [divertedHash] as [string],
              },
            },
          },
        },
      };
  const block = await depositEventsRetainedBlock({
    operatorVkey: input.operatorVkey,
    startTime: input.predecessor.header.endTime,
    endTime: input.endTime,
    blockSlot: input.blockSlot,
    prevHeaderHash: input.predecessor.headerHash,
    prevUtxosRoot: input.predecessor.header.utxosRoot,
    priorLedger: input.predecessor.payload.block_body.utxos,
    events: [
      {
        eventCbor: canonical(
          replacePlutusConstrFieldCbor(
            plutusConstrFieldCbor(datum.history.payloadCbor, [0]),
            [1, 0],
            Data.to(committed.event.info.l2_address, SDK.AddressData),
          ),
        ),
        originalAssets: datum.originalAssets,
        honest: true,
      },
    ],
  });
  return { ...block, actualDeposit: input.deposit };
};

type WithdrawalClaim = { id: SDK.OutputReference; infoCbor: string };

/**
 * Commit withdrawal events and their operator-selected verdicts against the
 * supplied prior ledger. Classification records the honest result independently
 * of the malicious claim, including sequential duplicate withdrawal refusal.
 */
export const retainJourneyWithdrawals = async (
  input: HistoryEventBlockInput & { claims: readonly WithdrawalClaim[] },
) => {
  const ledger = new Map(input.predecessor.payload.block_body.utxos);
  const ledgerBlock = () =>
    buildCanonicalBlockFixture({
      transactions: [],
      utxos: [...ledger].map(([key, value]) => ({
        key: Buffer.from(key, "hex"),
        value: Buffer.from(value, "hex"),
      })),
      startTime: input.predecessor.header.endTime,
      endTime: input.endTime,
      prevHeaderHash: input.predecessor.headerHash,
      prevUtxosRoot: input.predecessor.header.utxosRoot,
      minFeeA: input.predecessor.header.minFeeA,
      minFeeB: input.predecessor.header.minFeeB,
    });
  let base = await ledgerBlock();
  if (base.header.utxosRoot !== input.predecessor.header.utxosRoot)
    throw new Error(
      "Withdrawal history does not open the predecessor ledger root",
    );
  const withdrawals: SDK.DaPayloadEntry[] = [];
  const mappings: SDK.DaPayloadEntry[] = [];
  const transitions: SDK.DaPayloadEntry[] = [];
  const classifications: SDK.WithdrawalLedgerClassification[] = [];
  const claims = [...input.claims].sort((left, right) =>
    SDK.committedWithdrawalKeyBytes(left.id).localeCompare(
      SDK.committedWithdrawalKeyBytes(right.id),
    ),
  );
  for (const [index, claim] of claims.entries()) {
    const infoCbor = canonical(claim.infoCbor);
    const info = Data.from(infoCbor, SDK.WithdrawalInfo);
    const reference = encodeMidgardSpendInputItem({
      txId: Buffer.from(info.body.l2_outref.transactionId, "hex"),
      outputIndex: Number(info.body.l2_outref.outputIndex),
    });
    const output = ledger.get(reference.toString("hex"));
    const classification = await Effect.runPromise(
      SDK.classifyWithdrawalFromLedger({
        l2Owner: info.body.l2_owner,
        l2ValueCbor: canonical(plutusConstrFieldCbor(infoCbor, [0, 2])),
        eventInfoCbor: infoCbor,
        ledgerOutRef: reference,
        ledgerOutput: output === undefined ? null : Buffer.from(output, "hex"),
      }),
    );
    classifications.push(classification);
    const preRoot = base.header.utxosRoot;
    if (SDK.withdrawalClaimsValid(info))
      ledger.delete(reference.toString("hex"));
    base = await ledgerBlock();
    const eventKey: SDK.EventKey = {
      WithdrawalEventKey: { withdrawal_id: claim.id },
    };
    withdrawals.push([SDK.committedWithdrawalKeyBytes(claim.id), infoCbor]);
    mappings.push([
      Data.to(eventKey, SDK.EventKey),
      Data.to(
        { step_index: BigInt(index), phase: "Withdrawal" },
        SDK.EventToStepValue,
      ),
    ]);
    transitions.push([
      Data.to(BigInt(index)),
      Data.to(
        {
          schema_version: 1n,
          step_index: BigInt(index),
          event_key: eventKey,
          phase: "Withdrawal",
          pre_utxos_root: preRoot,
          post_utxos_root: base.header.utxosRoot,
        },
        SDK.TransitionStep,
      ),
    ]);
  }
  const root = (domain: SDK.RootDomain, entries: SDK.DaPayloadEntry[]) =>
    buildCountedRoot(
      domain,
      entries.map(([key, value]) => ({
        key: Buffer.from(key, "hex"),
        value: Buffer.from(value, "hex"),
      })),
    );
  const [withdrawalRoot, mappingRoot, transitionRoot] = await Promise.all([
    root(SDK.ROOT_DOMAINS.withdrawals, withdrawals),
    root(SDK.ROOT_DOMAINS.eventToStep, mappings),
    root(SDK.ROOT_DOMAINS.transitionTrace, transitions),
  ]);
  const counts = {
    ...base.payload.block_body.counts,
    withdrawalCount: BigInt(claims.length),
    totalEventCount: BigInt(claims.length),
    transitionStepCount: BigInt(claims.length),
  };
  const header: SDK.Header = {
    ...base.header,
    ...counts,
    operatorVkey: input.operatorVkey,
    blockSlot: input.blockSlot,
    expectedNetworkId: input.predecessor.header.expectedNetworkId,
    withdrawalsRoot: withdrawalRoot.root,
    eventToStepRoot: mappingRoot.root,
    transitionTraceRoot: transitionRoot.root,
  };
  const headerHash = await Effect.runPromise(SDK.hashBlockHeader(header));
  const sort = (entries: SDK.DaPayloadEntry[]) =>
    entries.sort(([left], [right]) => left.localeCompare(right));
  const payload: SDK.DaPayload = {
    ...base.payload,
    block_body: {
      ...base.payload.block_body,
      header,
      header_hash: headerHash,
      counts,
      withdrawals: sort(withdrawals),
      event_to_step: sort(mappings),
      transition_trace: sort(transitions),
    },
  };
  return {
    header,
    headerHash,
    payload,
    payloadEnvelopeCbor: await wrapDaPayload(SDK.encodeDaPayload(payload), {
      mode: "identity",
    }),
    classifications,
  };
};
