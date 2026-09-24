import { computeHash28 } from "@al-ft/midgard-core";
import { wrapDaPayload } from "@al-ft/midgard-core/da-payload-envelope";
import {
  aikenSerialisedPlutusDataCborPreservingMapOrder as serialise,
  compactPlutusDataCarriageCbor,
  plutusConstrFieldCbor,
  replacePlutusConstrFieldCbor,
} from "@al-ft/midgard-core/plutus-data-cbor";
import * as SDK from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";
import { beforeAll, describe, expect, it } from "vitest";

import { buildCountedRoot } from "../src/transition-trace/phas.js";
import { transitionTraceProofChunks } from "../src/transition-trace/proof-carriage.js";
import {
  makeTransitionProofMaterial,
  readTransitionProof,
  transitionProofHistorySource,
} from "../src/transition-trace/proof-material.js";
import { reconstructDaPayload } from "../src/transition-trace/reconstruct.js";
import { buildSourceMembershipProof } from "../src/transition-trace/witnesses.js";
import { buildCanonicalBlockFixture } from "./helpers/canonical-block-evidence-fixture.js";

const id = { transactionId: "11".repeat(32), outputIndex: 0n };
const address: SDK.AddressData = {
  paymentCredential: { PublicKeyCredential: ["22".repeat(28)] },
  stakeCredential: null,
};
const rawDatum = "a3020a010b020c";
let reconstruction: Awaited<ReturnType<typeof reconstructDaPayload>>;
let deposit: SDK.DepositSourceMembershipProof;
let withdrawal: SDK.WithdrawalSourceMembershipProof;
let cases: {
  name: string;
  fault: SDK.TransitionFault;
  path: number[];
  raw: string;
}[];

beforeAll(async () => {
  const base = await buildCanonicalBlockFixture({ transactions: [] });
  const rawDeposit = serialise(
    replacePlutusConstrFieldCbor(
      Data.to(
        { l2_address: address, l2_network_id: 0n, l2_datum: 42n },
        SDK.DepositInfo,
      ),
      [2, 0],
      rawDatum,
    ),
  );
  const rawWithdrawal = serialise(
    replacePlutusConstrFieldCbor(
      Data.to(
        {
          body: {
            l2_outref: id,
            l2_owner: "22".repeat(28),
            l2_value: new Map([["", new Map([["", 5_000_000n]])]]),
            l1_address: address,
            l1_datum: { InlineDatum: { data: 42n } },
          },
          signature: ["33".repeat(32), "44".repeat(64)],
          validity: "WithdrawalIsValid",
        },
        SDK.WithdrawalInfo,
      ),
      [0, 4, 0],
      rawDatum,
    ),
  );
  const entries = (raw: string): SDK.DaPayloadEntry[] => [
    [Data.to(id, SDK.OutputReference), raw],
  ];
  const root = (domain: SDK.RootDomain, raw: string) =>
    buildCountedRoot(
      domain,
      entries(raw).map(([key, value]) => ({
        key: Buffer.from(key, "hex"),
        value: Buffer.from(value, "hex"),
      })),
    );
  const deposits = await root(SDK.ROOT_DOMAINS.deposits, rawDeposit);
  const withdrawals = await root(SDK.ROOT_DOMAINS.withdrawals, rawWithdrawal);
  const eventKeys: SDK.EventKey[] = [
    { WithdrawalEventKey: { withdrawal_id: id } },
    { DepositEventKey: { deposit_id: id } },
  ];
  const event_to_step: SDK.DaPayloadEntry[] = eventKeys.map((key, index) => [
    Data.to(key, SDK.EventKey),
    Data.to(
      {
        step_index: BigInt(index),
        phase: index === 0 ? "Withdrawal" : "Deposit",
      },
      SDK.EventToStepValue,
    ),
  ]);
  event_to_step.sort(([a], [b]) => a.localeCompare(b));
  const transition_trace: SDK.DaPayloadEntry[] = eventKeys.map((key, index) => [
    Data.to(BigInt(index)),
    Data.to(
      {
        schema_version: 1n,
        step_index: BigInt(index),
        event_key: key,
        phase: index === 0 ? "Withdrawal" : "Deposit",
        pre_utxos_root: base.header.utxosRoot,
        post_utxos_root: base.header.utxosRoot,
      },
      SDK.TransitionStep,
    ),
  ]);
  const commitment = async (
    domain: SDK.RootDomain,
    values: SDK.DaPayloadEntry[],
  ) =>
    (
      await buildCountedRoot(
        domain,
        values.map(([key, value]) => ({
          key: Buffer.from(key, "hex"),
          value: Buffer.from(value, "hex"),
        })),
      )
    ).root;
  const counts = {
    ...base.payload.block_body.counts,
    depositCount: 1n,
    withdrawalCount: 1n,
    totalEventCount: 2n,
    transitionStepCount: 2n,
  };
  const header = {
    ...base.header,
    ...counts,
    depositsRoot: deposits.root,
    withdrawalsRoot: withdrawals.root,
    eventToStepRoot: await commitment(
      SDK.ROOT_DOMAINS.eventToStep,
      event_to_step,
    ),
    transitionTraceRoot: await commitment(
      SDK.ROOT_DOMAINS.transitionTrace,
      transition_trace,
    ),
  };
  const headerHash = computeHash28(SDK.encodeHeaderCbor(header)).toString(
    "hex",
  );
  const payload: SDK.DaPayload = {
    ...base.payload,
    block_body: {
      ...base.payload.block_body,
      header,
      header_hash: headerHash,
      counts,
      event_to_step,
      transition_trace,
      deposits: entries(rawDeposit),
      withdrawals: entries(rawWithdrawal),
    },
  };
  reconstruction = await reconstructDaPayload({
    payloadEnvelopeCbor: await wrapDaPayload(SDK.encodeDaPayload(payload), {
      mode: "identity",
    }),
    expectedHeaderHash: headerHash,
    committedHeader: header,
  });
  const d = await buildSourceMembershipProof({
    reconstruction,
    eventKey: { DepositEventKey: { deposit_id: id } },
  });
  const w = await buildSourceMembershipProof({
    reconstruction,
    eventKey: { WithdrawalEventKey: { withdrawal_id: id } },
  });
  if (!("DepositSourceMembership" in d) || !("WithdrawalSourceMembership" in w))
    throw new Error("Missing fixture memberships");
  deposit = d.DepositSourceMembership.membership;
  withdrawal = w.WithdrawalSourceMembership.membership;
  // These are serialization cases, not valid fault claims: source membership
  // is genuine; unrelated trace/mutation witnesses are schema-valid placeholders.
  const trace: SDK.IndexedTraceProof = {
    domain: SDK.ROOT_DOMAINS.transitionTrace,
    root: SDK.EMPTY_MERKLE_TREE_ROOT,
    phas_root: SDK.EMPTY_MERKLE_TREE_ROOT,
    count: 0n,
    key: 0n,
    value: {
      schema_version: 1n,
      step_index: 0n,
      event_key: { DepositEventKey: { deposit_id: id } },
      phase: "Deposit",
      pre_utxos_root: SDK.EMPTY_MERKLE_TREE_ROOT,
      post_utxos_root: SDK.EMPTY_MERKLE_TREE_ROOT,
    },
    proof: [],
  };
  const eventToStep: SDK.EventToStepMembershipProof = {
    domain: SDK.ROOT_DOMAINS.eventToStep,
    root: SDK.EMPTY_MERKLE_TREE_ROOT,
    phas_root: SDK.EMPTY_MERKLE_TREE_ROOT,
    count: 0n,
    key: trace.value.event_key,
    value: { step_index: 0n, phase: "Deposit" },
    proof: [],
  };
  const absent: SDK.EventToStepNonMembershipProof = {
    domain: eventToStep.domain,
    root: eventToStep.root,
    phas_root: eventToStep.phas_root,
    count: eventToStep.count,
    key: eventToStep.key,
    proof: eventToStep.proof,
  };
  const shared = { trace_proof: trace, event_to_step: eventToStep };
  cases = [
    {
      name: "deposit transition",
      fault: {
        InvalidOneStepTransition: {
          witness: {
            ValidDepositTransition: {
              ...shared,
              source_membership: deposit,
              projected_utxo: {
                key: "",
                value: "",
                non_membership_proof: [],
                insert_proof: [],
              },
            },
          },
        },
      },
      path: [2, 0, 2, 5],
      raw: rawDeposit,
    },
    {
      name: "valid withdrawal transition",
      fault: {
        InvalidOneStepTransition: {
          witness: {
            ValidWithdrawalTransition: {
              ...shared,
              source_membership: withdrawal,
              spent_utxo: {
                key: "",
                value: "",
                membership_proof: [],
                delete_proof: [],
              },
            },
          },
        },
      },
      path: [2, 0, 2, 5],
      raw: rawWithdrawal,
    },
    {
      name: "invalid withdrawal noop",
      fault: {
        InvalidOneStepTransition: {
          witness: {
            InvalidWithdrawalNoOpTransition: {
              ...shared,
              source_membership: withdrawal,
            },
          },
        },
      },
      path: [2, 0, 2, 5],
      raw: rawWithdrawal,
    },
  ];
  for (const [kind, wrapped, member, raw] of [
    ["Deposit", d, deposit, rawDeposit],
    ["Withdrawal", w, withdrawal, rawWithdrawal],
  ] as const) {
    const window: SDK.OutOfWindowSourceEventWitness =
      kind === "Deposit"
        ? {
            OutOfWindowDeposit: {
              source_membership: member as SDK.DepositSourceMembershipProof,
            },
          }
        : {
            OutOfWindowWithdrawal: {
              source_membership: member as SDK.WithdrawalSourceMembershipProof,
            },
          };
    cases.push(
      {
        name: `${kind} window`,
        fault: { OutOfWindowSourceEvent: { witness: window } },
        path: [2, 0, 0, 5],
        raw,
      },
      {
        name: `${kind} missing trace`,
        fault: {
          SourceMembershipMismatch: {
            witness: {
              SourceEventMissingTrace: {
                source_membership: wrapped,
                event_to_step_non_membership: absent,
              },
            },
          },
        },
        path: [2, 0, 0, 0, 5],
        raw,
      },
      {
        name: `${kind} phase`,
        fault: {
          SourceMembershipMismatch: {
            witness: {
              SourcePhaseMismatch: {
                trace_proof: trace,
                source_membership: wrapped,
              },
            },
          },
        },
        path: [2, 0, 1, 0, 5],
        raw,
      },
    );
  }
});

describe("transition raw proof material", () => {
  it("retains all nine history source branches across decoding, JSON and compact carriage", () => {
    expect(cases).toHaveLength(9);
    for (const item of cases) {
      const proof = SDK.makeTransitionFaultProof({
        challengedHeaderHash: reconstruction.headerHash,
        header: reconstruction.header,
        fault: item.fault,
      });
      const material = makeTransitionProofMaterial(reconstruction, proof);
      expect(
        plutusConstrFieldCbor(material.proofCbor, item.path),
        item.name,
      ).toBe(item.raw);
      expect(transitionProofHistorySource(material)?.valueCbor, item.name).toBe(
        item.raw,
      );
      expect(serialise(material.proofCbor), item.name).toBe(material.proofCbor);
      const persisted = JSON.parse(JSON.stringify(material)) as typeof material;
      const chunks = transitionTraceProofChunks(persisted);
      expect(chunks.chunks.join(""), item.name).toBe(
        compactPlutusDataCarriageCbor(material.proofCbor),
      );
      expect(
        serialise(plutusConstrFieldCbor(chunks.chunks.join(""), item.path)),
        item.name,
      ).toBe(item.raw);
      expect(
        transitionTraceProofChunks(readTransitionProof(material)).hash,
        item.name,
      ).not.toBe(chunks.hash);
      const detached = readTransitionProof(material);
      detached.header.endTime += 1n;
      expect(transitionTraceProofChunks(material)).toEqual(chunks);
    }
  });

  it("refuses an altered raw leaf even when its collapsed typed view agrees", () => {
    const proof = SDK.makeTransitionFaultProof({
      challengedHeaderHash: reconstruction.headerHash,
      header: reconstruction.header,
      fault: cases[0]!.fault,
    });
    const entry = reconstruction.deposits[0]!;
    const valueBytes = Buffer.from(
      replacePlutusConstrFieldCbor(
        entry.valueBytes.toString("hex"),
        [2, 0],
        "a3010b020a020c",
      ),
      "hex",
    );
    expect(Data.from(valueBytes.toString("hex"), SDK.DepositInfo)).toEqual(
      entry.value,
    );
    expect(() =>
      makeTransitionProofMaterial(
        { ...reconstruction, deposits: [{ ...entry, valueBytes }] },
        proof,
      ),
    ).toThrow(/membership/);
    const changed = structuredClone(proof);
    if (
      !("InvalidOneStepTransition" in changed.fault) ||
      !(
        "ValidDepositTransition" in
        changed.fault.InvalidOneStepTransition.witness
      )
    )
      throw new Error("Missing deposit fixture");
    const witness =
      changed.fault.InvalidOneStepTransition.witness.ValidDepositTransition;
    const changedCount = {
      ...changed,
      fault: {
        InvalidOneStepTransition: {
          witness: {
            ValidDepositTransition: {
              ...witness,
              source_membership: {
                ...witness.source_membership,
                count: witness.source_membership.count + 1n,
              },
            },
          },
        },
      },
    };
    expect(() =>
      makeTransitionProofMaterial(reconstruction, changedCount),
    ).toThrow(/counted root/);
  });
});
