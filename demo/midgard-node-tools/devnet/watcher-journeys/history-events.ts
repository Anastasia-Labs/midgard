import {
  decodeMidgardSpendInputItem,
  decodeMidgardTxOutput,
  encodeMidgardSpendInputItem,
} from "@al-ft/midgard-core";
import { wrapDaPayload } from "@al-ft/midgard-core/da-payload-envelope";
import { buildCountedRoot } from "@al-ft/midgard-fault-proofs";
import { buildCanonicalBlockFixture } from "@al-ft/midgard-fault-proofs/test-support/canonical-block-evidence-fixture";
import { depositEventsRetainedBlock } from "@al-ft/midgard-fault-proofs/test-support/transition-trace-retained";
import * as SDK from "@al-ft/midgard-sdk";
import { CML, Data, type UTxO, walletFromSeed } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import {
  type HistoryPredecessor,
  ownedHistoryLedgerEntries,
  retainHistoryTransactions,
  signHistoryTransaction,
} from "./history-cases.js";

export type HistoryEventBlockInput = {
  predecessor: HistoryPredecessor;
  operatorVkey: string;
  endTime: bigint;
  blockSlot: bigint;
};

/** An exact event output supplied by the common real-chain staging owner. */
export type StagedHistoryEvent = {
  event: UTxO;
  policyId: string;
  assetName: string;
};

const authenticateEvent = (input: StagedHistoryEvent) => {
  if (!input.event.datum)
    throw new Error("Staged history event has no inline datum");
  if (input.event.assets[input.policyId + input.assetName] !== 1n)
    throw new Error("Staged history event lacks its exact role token");
  return input.event.datum;
};

const requireEventWindow = (time: bigint, input: HistoryEventBlockInput) => {
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
  const datum = Data.from(authenticateEvent(input.deposit), SDK.DepositDatum);
  requireEventWindow(datum.inclusion_time, input);
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
        event: {
          ...input.deposit.event,
          datum: Data.to(committed, SDK.DepositDatum),
        },
        depositPolicyId: input.deposit.policyId,
        assetName: input.deposit.assetName,
        honest: true,
      },
    ],
  });
  return { ...block, actualDeposit: input.deposit };
};

type WithdrawalClaim = { id: SDK.OutputReference; info: SDK.WithdrawalInfo };

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
    const reference = encodeMidgardSpendInputItem({
      txId: Buffer.from(claim.info.body.l2_outref.transactionId, "hex"),
      outputIndex: Number(claim.info.body.l2_outref.outputIndex),
    });
    const output = ledger.get(reference.toString("hex"));
    const classification = await Effect.runPromise(
      SDK.classifyWithdrawalFromLedger({
        l2Owner: claim.info.body.l2_owner,
        l2ValueCbor: Data.to(claim.info.body.l2_value, SDK.Value),
        eventInfoCbor: SDK.committedWithdrawalValueBytes(claim.info),
        ledgerOutRef: reference,
        ledgerOutput: output === undefined ? null : Buffer.from(output, "hex"),
      }),
    );
    classifications.push(classification);
    const preRoot = base.header.utxosRoot;
    if (SDK.withdrawalClaimsValid(claim.info))
      ledger.delete(reference.toString("hex"));
    base = await ledgerBlock();
    const eventKey: SDK.EventKey = {
      WithdrawalEventKey: { withdrawal_id: claim.id },
    };
    withdrawals.push([
      SDK.committedWithdrawalKeyBytes(claim.id),
      SDK.committedWithdrawalValueBytes(claim.info),
    ]);
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

export const buildJourneyWithdrawalEvent = async (
  input: HistoryEventBlockInput & {
    category: "fabricatedWithdrawal" | "withdrawalMistag" | "doubleWithdraw";
    withdrawals: readonly StagedHistoryEvent[];
    honest?: boolean;
  },
) => {
  const events = input.withdrawals.map((withdrawal) => {
    const datum = Data.from(
      authenticateEvent(withdrawal),
      SDK.WithdrawalOrderDatum,
    );
    requireEventWindow(datum.inclusion_time, input);
    return datum.event;
  });
  const first = events[0];
  if (first === undefined)
    throw new Error("Withdrawal journey requires a real staged event");
  if (input.category === "doubleWithdraw") {
    const second = events[1];
    if (
      events.length !== 2 ||
      second === undefined ||
      SDK.committedWithdrawalKeyBytes(first.id) ===
        SDK.committedWithdrawalKeyBytes(second.id)
    )
      throw new Error(
        "Double-withdraw journey needs two distinct real event identities",
      );
    if (
      Data.to(first.info.body.l2_outref, SDK.OutputReference) !==
      Data.to(second.info.body.l2_outref, SDK.OutputReference)
    )
      throw new Error(
        "Double-withdraw staged events must name the same L2 output",
      );
    const ordered = [...events].sort((left, right) =>
      SDK.committedWithdrawalKeyBytes(left.id).localeCompare(
        SDK.committedWithdrawalKeyBytes(right.id),
      ),
    );
    const claims = ordered.map((event, index) => ({
      ...event,
      info: {
        ...event.info,
        validity:
          input.honest && index === 1
            ? ("NonExistentWithdrawalUtxo" as const)
            : ("WithdrawalIsValid" as const),
      },
    }));
    return retainJourneyWithdrawals({ ...input, claims });
  }
  if (events.length !== 1)
    throw new Error("Withdrawal journey needs exactly one staged event");
  let info = first.info;
  if (!input.honest && input.category === "withdrawalMistag")
    info = { ...info, validity: "NonExistentWithdrawalUtxo" };
  if (!input.honest && input.category === "fabricatedWithdrawal") {
    const key = first.info.body.l2_owner;
    const diverted = (key.startsWith("00") ? "01" : "00") + key.slice(2);
    info = {
      ...info,
      body: {
        ...info.body,
        l1_address: {
          paymentCredential: { PublicKeyCredential: [diverted] },
          stakeCredential: null,
        },
      },
    };
  }
  return retainJourneyWithdrawals({
    ...input,
    claims: [{ id: first.id, info }],
  });
};

/** Body for the common L1 staging owner to sign and submit through the SDK. */
export const journeyWithdrawalBody = (input: {
  predecessor: HistoryPredecessor;
  owner: string;
}): SDK.WithdrawalBody => {
  const selected = ownedHistoryLedgerEntries(input.predecessor, input.owner)[0];
  const entry =
    selected === undefined
      ? undefined
      : [selected.outRef.toString("hex"), selected.output.toString("hex")];
  if (entry === undefined)
    throw new Error("Withdrawal journey has no deposited ledger output");
  const reference = decodeMidgardSpendInputItem(Buffer.from(entry[0], "hex"));
  const output = decodeMidgardTxOutput(Buffer.from(entry[1], "hex"));
  return {
    l2_outref: {
      transactionId: Buffer.from(reference.txId).toString("hex"),
      outputIndex: BigInt(reference.outputIndex),
    },
    l2_owner: input.owner,
    l2_value: new Map([
      ["", new Map([["", output.value.lovelace]])],
      ...Array.from(
        output.value.assets,
        ([policy, assets]) => [policy, new Map(assets)] as const,
      ),
    ]),
    l1_address: {
      paymentCredential: { PublicKeyCredential: [input.owner] },
      stakeCredential: null,
    },
    l1_datum: "NoDatum",
  };
};

/** A withdrawal followed by an accepted claim that still spends/reads its output. */
export const buildJourneyWithdrawnTransaction = async (
  input: HistoryEventBlockInput & {
    category: "withdrawnInput" | "withdrawnReferenceInput";
    withdrawal: StagedHistoryEvent;
    ledgerOwnerSeedPhrase: string;
    honest?: boolean;
  },
) => {
  const datum = Data.from(
    authenticateEvent(input.withdrawal),
    SDK.WithdrawalOrderDatum,
  );
  requireEventWindow(datum.inclusion_time, input);
  const withdrawn = encodeMidgardSpendInputItem({
    txId: Buffer.from(datum.event.info.body.l2_outref.transactionId, "hex"),
    outputIndex: Number(datum.event.info.body.l2_outref.outputIndex),
  });
  const withdrawnOutput = input.predecessor.payload.block_body.utxos.find(
    ([key]) => key === withdrawn.toString("hex"),
  );
  const owner = CML.PrivateKey.from_bech32(
    walletFromSeed(input.ledgerOwnerSeedPhrase, { network: "Custom" })
      .paymentKey,
  )
    .to_public()
    .hash()
    .to_hex();
  const unspent = ownedHistoryLedgerEntries(input.predecessor, owner).find(
    ({ outRef }) => !outRef.equals(withdrawn),
  );
  const remaining =
    unspent === undefined
      ? undefined
      : [unspent.outRef.toString("hex"), unspent.output.toString("hex")];
  if (withdrawnOutput === undefined || remaining === undefined)
    throw new Error(
      "Withdrawn-input journeys need two actual predecessor outputs, one withdrawn and one retained for the valid control",
    );
  const withdrawalBlock = await retainJourneyWithdrawals({
    ...input,
    claims: [datum.event],
  });
  if (withdrawalBlock.classifications[0]?.validity !== "WithdrawalIsValid")
    throw new Error(
      "Withdrawn-input journey requires an honestly valid staged withdrawal",
    );
  const key = CML.PrivateKey.from_bech32(
    walletFromSeed(input.ledgerOwnerSeedPhrase, { network: "Custom" })
      .paymentKey,
  );
  const selected =
    !input.honest && input.category === "withdrawnInput"
      ? withdrawnOutput
      : remaining;
  const transaction = signHistoryTransaction(
    {
      spendInputs: [Buffer.from(selected[0], "hex")],
      referenceInputs:
        !input.honest && input.category === "withdrawnReferenceInput"
          ? [withdrawn]
          : [],
      outputs: [Buffer.from(selected[1], "hex")],
      fee: 0n,
      networkId: input.predecessor.header.expectedNetworkId,
    },
    key,
  );
  const transactions = await retainHistoryTransactions({
    ...input,
    predecessor: {
      ...withdrawalBlock,
      header: {
        ...withdrawalBlock.header,
        endTime: input.predecessor.header.endTime,
      },
    },
    transactions: [transaction],
  });
  const mappings: SDK.DaPayloadEntry[] = [
    ...withdrawalBlock.payload.block_body.event_to_step,
    ...transactions.payload.block_body.event_to_step.map(
      ([key, value]): SDK.DaPayloadEntry => [
        key,
        Data.to(
          { ...Data.from(value, SDK.EventToStepValue), step_index: 1n },
          SDK.EventToStepValue,
        ),
      ],
    ),
  ];
  const transitions: SDK.DaPayloadEntry[] = [
    ...withdrawalBlock.payload.block_body.transition_trace,
    ...transactions.payload.block_body.transition_trace.map(
      ([, value]): SDK.DaPayloadEntry => [
        Data.to(1n),
        Data.to(
          { ...Data.from(value, SDK.TransitionStep), step_index: 1n },
          SDK.TransitionStep,
        ),
      ],
    ),
  ];
  const root = (domain: SDK.RootDomain, entries: SDK.DaPayloadEntry[]) =>
    buildCountedRoot(
      domain,
      entries.map(([key, value]) => ({
        key: Buffer.from(key, "hex"),
        value: Buffer.from(value, "hex"),
      })),
    );
  const [eventRoot, traceRoot] = await Promise.all([
    root(SDK.ROOT_DOMAINS.eventToStep, mappings),
    root(SDK.ROOT_DOMAINS.transitionTrace, transitions),
  ]);
  const counts = {
    ...transactions.payload.block_body.counts,
    withdrawalCount: 1n,
    totalEventCount: 2n,
    transitionStepCount: 2n,
  };
  const header: SDK.Header = {
    ...transactions.header,
    ...counts,
    prevHeaderHash: input.predecessor.headerHash,
    prevUtxosRoot: input.predecessor.header.utxosRoot,
    withdrawalsRoot: withdrawalBlock.header.withdrawalsRoot,
    eventToStepRoot: eventRoot.root,
    transitionTraceRoot: traceRoot.root,
  };
  const headerHash = await Effect.runPromise(SDK.hashBlockHeader(header));
  const sort = (entries: SDK.DaPayloadEntry[]) =>
    entries.sort(([left], [right]) => left.localeCompare(right));
  const payload: SDK.DaPayload = {
    ...transactions.payload,
    block_body: {
      ...transactions.payload.block_body,
      header,
      header_hash: headerHash,
      counts,
      withdrawals: withdrawalBlock.payload.block_body.withdrawals,
      event_to_step: sort(mappings),
      transition_trace: sort(transitions),
    },
  };
  return {
    ...transactions,
    header,
    headerHash,
    payload,
    payloadEnvelopeCbor: await wrapDaPayload(SDK.encodeDaPayload(payload), {
      mode: "identity",
    }),
    actualWithdrawal: input.withdrawal,
  };
};

/**
 * Repeat a deposit from an actual settled ancestor. The caller must stage and
 * retain the real settlement NFT separately; retained DA alone proves no
 * settlement. The honest control commits an empty continuation.
 */
export const buildJourneyRepeatedDeposit = async (
  input: HistoryEventBlockInput & {
    settled: HistoryPredecessor;
    honest?: boolean;
  },
) => {
  const entry = input.settled.payload.block_body.deposits[0];
  if (entry === undefined)
    throw new Error("Settled ancestor has no retained deposit event");
  if (input.settled.header.endTime > input.predecessor.header.endTime)
    throw new Error("Duplicate-event history is newer than the predecessor");
  const base = await buildCanonicalBlockFixture({
    transactions: [],
    utxos: input.predecessor.payload.block_body.utxos.map(([key, value]) => ({
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
  const eventKey: SDK.EventKey = {
    DepositEventKey: { deposit_id: Data.from(entry[0], SDK.OutputReference) },
  };
  const deposits: SDK.DaPayloadEntry[] = input.honest ? [] : [entry];
  const mappings: SDK.DaPayloadEntry[] = input.honest
    ? []
    : [
        [
          Data.to(eventKey, SDK.EventKey),
          Data.to({ step_index: 0n, phase: "Deposit" }, SDK.EventToStepValue),
        ],
      ];
  const transitions: SDK.DaPayloadEntry[] = input.honest
    ? []
    : [
        [
          Data.to(0n),
          Data.to(
            {
              schema_version: 1n,
              step_index: 0n,
              event_key: eventKey,
              phase: "Deposit",
              pre_utxos_root: base.header.prevUtxosRoot,
              post_utxos_root: base.header.utxosRoot,
            },
            SDK.TransitionStep,
          ),
        ],
      ];
  const root = (domain: SDK.RootDomain, entries: SDK.DaPayloadEntry[]) =>
    buildCountedRoot(
      domain,
      entries.map(([key, value]) => ({
        key: Buffer.from(key, "hex"),
        value: Buffer.from(value, "hex"),
      })),
    );
  const [depositRoot, eventRoot, traceRoot] = await Promise.all([
    root(SDK.ROOT_DOMAINS.deposits, deposits),
    root(SDK.ROOT_DOMAINS.eventToStep, mappings),
    root(SDK.ROOT_DOMAINS.transitionTrace, transitions),
  ]);
  const counts = {
    ...base.payload.block_body.counts,
    depositCount: BigInt(deposits.length),
    totalEventCount: BigInt(deposits.length),
    transitionStepCount: BigInt(deposits.length),
  };
  const header: SDK.Header = {
    ...base.header,
    ...counts,
    operatorVkey: input.operatorVkey,
    blockSlot: input.blockSlot,
    expectedNetworkId: input.predecessor.header.expectedNetworkId,
    depositsRoot: depositRoot.root,
    eventToStepRoot: eventRoot.root,
    transitionTraceRoot: traceRoot.root,
  };
  const headerHash = await Effect.runPromise(SDK.hashBlockHeader(header));
  const payload: SDK.DaPayload = {
    ...base.payload,
    block_body: {
      ...base.payload.block_body,
      header,
      header_hash: headerHash,
      counts,
      deposits,
      event_to_step: mappings,
      transition_trace: transitions,
    },
  };
  return {
    header,
    headerHash,
    payload,
    payloadEnvelopeCbor: await wrapDaPayload(SDK.encodeDaPayload(payload), {
      mode: "identity",
    }),
  };
};
