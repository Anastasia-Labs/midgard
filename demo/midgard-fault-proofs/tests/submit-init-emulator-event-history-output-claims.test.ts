/** Both genuine list policies share one fixture-issued hub. No per-order certificates. */
import { createHash } from "node:crypto";
import { mkdirSync, readFileSync, writeFileSync } from "node:fs";
import { join } from "node:path";

import { aikenSerialisedPlutusDataCborPreservingMapOrder } from "@al-ft/midgard-core/plutus-data-cbor";
import {
  ConfirmedState,
  DepositInfo,
  EMPTY_MERKLE_TREE_ROOT,
  EventHistoryNode,
  EventHistoryObserve,
  type EventHistoryOperation,
  type EventHistoryPayload,
  eventHistoryRetirementOperation,
  EventHistoryRetirementWitness,
  OutputReference,
  parseFaultProofBlueprint,
  SettlementDatum,
  WithdrawalInfo,
} from "@al-ft/midgard-sdk";
import {
  Constr,
  credentialToAddress,
  Data,
  fromText,
  validatorToScriptHash,
} from "@lucid-evolution/lucid";
import { afterAll, describe, expect, it } from "vitest";

import {
  buildCountedRoot,
  keyValuePhasProof,
} from "../src/transition-trace/phas.js";
import { realBlueprintPath } from "./support/emulator/blueprints.js";
import {
  index,
  promoteHistoryPair,
  setupHistoryPair,
} from "./support/emulator/history-pair.js";
import { EMULATOR_PROTOCOL_PARAMETERS } from "./support/emulator/protocol-parameters.js";

const blueprintBytes = readFileSync(realBlueprintPath);
const blueprint = parseFaultProofBlueprint(
  JSON.parse(blueprintBytes.toString()),
);
const records: unknown[] = [];
const setup = () => setupHistoryPair({ blueprint, records });

type Attack =
  | "shared-refund"
  | "continuation-payment"
  | "omitted-other-withdrawal"
  | "wrong-own-input"
  | "hidden-filler";
const reclaimBoth = async (
  h: Awaited<ReturnType<typeof setup>>,
  attack?: Attack,
) => {
  const lists = await Promise.all(
    h.applied.map((a) => h.lucid.utxosAt(a.address)),
  );
  const roots = lists.map(
    (nodes, i) => nodes.find((u) => u.assets[h.applied[i]!.policyId] === 1n)!,
  );
  const fillers = lists.map(
    (nodes, i) =>
      nodes.find((u) => u.assets[h.applied[i]!.policyId + h.keys[i]!] === 1n)!,
  );
  const funding = await h.funding();
  const inputs = [...funding, ...roots, ...fillers];
  const refs = [h.hub, ...h.scripts];
  const b = h.bounds();
  let tx = h.lucid
    .newTx()
    .collectFrom(funding)
    .readFrom(refs)
    .validFrom(b.lower)
    .validTo(b.validTo)
    .addSignerKey(h.owner);
  for (let i = 0; i < 2; i++) {
    const a = h.applied[i]!;
    const root = roots[i]!;
    const filler = fillers[i]!;
    const operation: EventHistoryOperation = {
      ReclaimFiller: {
        predecessor_input_index: index(inputs, root),
        filler_input_index:
          attack === "hidden-filler" && i === 1
            ? index(inputs, root)
            : index(inputs, filler),
        predecessor_output_index: BigInt(i),
        refund_output_index:
          attack === "shared-refund" && i === 1
            ? 2n
            : attack === "continuation-payment" && i === 1
              ? 0n
              : BigInt(2 + i),
      },
    };
    tx = tx
      .collectFrom(
        [root],
        Data.to(
          attack === "wrong-own-input" && i === 1
            ? index(inputs, filler)
            : index(inputs, root),
        ),
      )
      .collectFrom([filler], Data.to(index(inputs, filler)))
      .mintAssets({ [a.policyId + h.keys[i]!]: -1n }, Data.void());
    if (!(attack === "omitted-other-withdrawal" && i === 1))
      tx = tx.withdraw(
        a.rewardAddress,
        0n,
        Data.to(
          { Apply: { hub_reference_index: index(refs, h.hub), operation } },
          EventHistoryObserve,
        ),
      );
    const node = Data.from(root.datum!, EventHistoryNode);
    tx = tx.pay.ToContract(
      a.address,
      {
        kind: "inline",
        value: Data.to(
          { ...node, next: null, protected_until: b.protectedUntil },
          EventHistoryNode,
        ),
      },
      root.assets,
    );
  }
  tx = tx.pay.ToAddress(
    credentialToAddress("Custom", { type: "Key", hash: h.owner }),
    { lovelace: 3_000_000n },
  );
  if (attack !== "shared-refund")
    tx = tx.pay.ToAddress(
      credentialToAddress("Custom", { type: "Key", hash: h.owner }),
      { lovelace: 3_000_000n },
    );
  return tx.complete({ coinSelection: false, localUPLCEval: true });
};

afterAll(() => {
  const dir = join(process.cwd(), "../../artifacts/event-history");
  mkdirSync(dir, { recursive: true });
  writeFileSync(
    join(dir, "history-output-claims-applied.json"),
    JSON.stringify(
      {
        scope:
          "Two real list policies with native fixture hub; output claims and filler reclamation, not final settlement acceptance",
        blueprintSha256: createHash("sha256")
          .update(blueprintBytes)
          .digest("hex"),
        protocolParameters: EMULATOR_PROTOCOL_PARAMETERS,
        records,
      },
      (_, v) => (typeof v === "bigint" ? v.toString() : v),
      2,
    ) + "\n",
  );
});

describe("cross-kind history output ownership", () => {
  it("reclaims equal-value fillers with distinct refund outputs and wallet change", async () => {
    const h = await setup();
    await h.submit("reclaim-distinct-cross-kind-refunds", await reclaimBoth(h));
    for (const a of h.applied) {
      const nodes = await h.lucid.utxosAt(a.address);
      expect(nodes).toHaveLength(1);
      expect(Data.from(nodes[0]!.datum!, EventHistoryNode).next).toBeNull();
    }
  });
  for (const attack of [
    "shared-refund",
    "continuation-payment",
    "omitted-other-withdrawal",
    "wrong-own-input",
    "hidden-filler",
  ] as const)
    it(`rejects ${attack}`, async () => {
      const h = await setup();
      await expect(reclaimBoth(h, attack)).rejects.toThrow();
      for (const a of h.applied)
        expect(await h.lucid.utxosAt(a.address)).toHaveLength(2);
    });
});

const promoteBoth = promoteHistoryPair;

const settlementForBoth = async (
  h: Awaited<ReturnType<typeof setup>>,
  payloads: EventHistoryPayload[],
) => {
  const memberships = [];
  const roots = [];
  for (let i = 0; i < 2; i++) {
    const payload = payloads[i]!;
    const event =
      "DepositPayload" in payload
        ? payload.DepositPayload.event
        : payload.WithdrawalPayload.event;
    const key = Buffer.from(Data.to(event.id, OutputReference), "hex");
    const info =
      "DepositPayload" in payload
        ? Data.to(payload.DepositPayload.event.info, DepositInfo)
        : Data.to(
            {
              ...payload.WithdrawalPayload.event.info,
              validity: "IncorrectWithdrawalSignature",
            },
            WithdrawalInfo,
          );
    const value = Buffer.from(
      aikenSerialisedPlutusDataCborPreservingMapOrder(info),
      "hex",
    );
    const tree = await buildCountedRoot(
      i === 0 ? "DepositsRootDomain" : "WithdrawalsRootDomain",
      [{ key, value }],
    );
    const proof = await keyValuePhasProof(
      { ...tree, root: tree.phasRoot },
      key,
      value,
    );
    memberships.push({ phas_root: tree.phasRoot, count: tree.count, proof });
    roots.push(tree.root);
  }
  h.emulator.awaitBlock(10);
  const confirmedUnit = h.hubPolicy + fromText("MIDGARD_CONFIRMED_STATE");
  const settlementUnit = h.hubPolicy + fromText("event-history-settlement");
  const confirmed = Data.from(
    Data.to(
      {
        headerHash: "01".repeat(28),
        prevHeaderHash: "02".repeat(28),
        utxoRoot: EMPTY_MERKLE_TREE_ROOT,
        startTime: 0n,
        endTime: BigInt(h.emulator.now()),
        protocolVersion: 1n,
      },
      ConfirmedState,
    ),
  );
  const datum = Data.to(
    new Constr(0, [new Constr(0, [confirmed]), new Constr(1, [])]),
  );
  let publish = h.lucid
    .newTx()
    .collectFrom(await h.funding())
    .mintAssets({ [confirmedUnit]: 1n, [settlementUnit]: 1n })
    .attach.MintingPolicy(h.issuer)
    .pay.ToContract(
      h.hubAddress,
      { kind: "inline", value: datum },
      { lovelace: 5_000_000n, [confirmedUnit]: 1n },
    )
    .pay.ToContract(
      h.hubAddress,
      {
        kind: "inline",
        value: Data.to(
          {
            deposits_root: roots[0]!,
            withdrawals_root: roots[1]!,
            forced_transactions_root: EMPTY_MERKLE_TREE_ROOT,
            transactions_root: EMPTY_MERKLE_TREE_ROOT,
            resolution_claim: null,
          },
          SettlementDatum,
        ),
      },
      { lovelace: 5_000_000n, [settlementUnit]: 1n },
    );
  for (const a of h.applied)
    publish = publish.register.Stake(a.retirement.rewardAddress);
  await h.submit(
    "publish-finalized-settlement-fixture",
    await publish.complete({ coinSelection: false, localUPLCEval: true }),
  );
  for (const a of h.applied)
    await h.submit(
      "publish-retirement-observer",
      await h.lucid
        .newTx()
        .collectFrom(await h.funding())
        .pay.ToAddressWithData(
          h.hubAddress,
          undefined,
          { lovelace: 50_000_000n },
          a.retirement.validator,
        )
        .complete({ coinSelection: false, localUPLCEval: true }),
    );
  const utxos = await h.lucid.utxosAt(h.hubAddress);
  return {
    memberships,
    confirmed: utxos.find((u) => u.assets[confirmedUnit] === 1n)!,
    settlement: utxos.find((u) => u.assets[settlementUnit] === 1n)!,
    retirementScripts: h.applied.map(
      (a) =>
        utxos.find(
          (u) =>
            u.scriptRef != null &&
            validatorToScriptHash(u.scriptRef) ===
              validatorToScriptHash(a.retirement.validator),
        )!,
    ),
  };
};

const retireBoth = async (
  h: Awaited<ReturnType<typeof setup>>,
  authority: Awaited<ReturnType<typeof settlementForBoth>>,
  attack?: "shared-funds" | "declared-subset" | "structural-payment-reuse",
) => {
  const lists = await Promise.all(
    h.applied.map((a) => h.lucid.utxosAt(a.address)),
  );
  const predecessors = lists.map(
    (nodes, i) => nodes.find((u) => u.assets[h.applied[i]!.policyId] === 1n)!,
  );
  const orders = lists.map(
    (nodes, i) =>
      nodes.find((u) => u.assets[h.applied[i]!.policyId + h.keys[i]!] === 1n)!,
  );
  const funding = await h.funding();
  const inputs = [...funding, ...predecessors, ...orders];
  const refs = [
    h.hub,
    ...h.scripts,
    authority.confirmed,
    authority.settlement,
    ...authority.retirementScripts,
  ];
  const b = h.bounds();
  let tx = h.lucid
    .newTx()
    .collectFrom(funding)
    .readFrom(refs)
    .validFrom(b.lower)
    .validTo(b.validTo);
  for (let i = 0; i < 2; i++) {
    const a = h.applied[i]!;
    const predecessor = predecessors[i]!;
    const order = orders[i]!;
    const witness: EventHistoryRetirementWitness = {
      predecessor_input_index: index(inputs, predecessor),
      order_input_index: index(inputs, order),
      predecessor_output_index: BigInt(i),
      funds_output_index:
        attack === "shared-funds" && i === 1 ? 2n : BigInt(2 + i),
      structural_refund_output_index:
        i === 0 ? (attack === "structural-payment-reuse" ? 2n : 4n) : null,
      confirmed_reference_index: index(refs, authority.confirmed),
      settlement_reference_index: index(refs, authority.settlement),
      external_reference_index: null,
      membership: authority.memberships[i]!,
      purpose:
        i === 0
          ? "AbsorbDeposit"
          : {
              RefundInvalidWithdrawal: {
                validity: "IncorrectWithdrawalSignature",
              },
            },
    };
    const operation = eventHistoryRetirementOperation(witness);
    if (attack === "declared-subset" && i === 0 && "RetireOrder" in operation)
      operation.RetireOrder.structural_refund_output_index = null;
    tx = tx
      .collectFrom([predecessor], Data.to(index(inputs, predecessor)))
      .collectFrom([order], Data.to(index(inputs, order)))
      .mintAssets({ [a.policyId + h.keys[i]!]: -1n }, Data.void())
      .withdraw(
        a.rewardAddress,
        0n,
        Data.to(
          { Apply: { hub_reference_index: index(refs, h.hub), operation } },
          EventHistoryObserve,
        ),
      )
      .withdraw(
        a.retirement.rewardAddress,
        0n,
        Data.to(
          new Constr(0, [
            index(refs, h.hub),
            Data.from(Data.to(witness, EventHistoryRetirementWitness)),
          ]),
        ),
      )
      .pay.ToContract(
        a.address,
        {
          kind: "inline",
          value: Data.to(
            {
              ...Data.from(predecessor.datum!, EventHistoryNode),
              next: null,
              protected_until: b.protectedUntil,
            },
            EventHistoryNode,
          ),
        },
        predecessor.assets,
      );
  }
  tx = tx.pay.ToAddress(h.hubAddress, { lovelace: 20_000_000n });
  if (attack !== "shared-funds")
    tx = tx.pay.ToAddress(h.hubAddress, { lovelace: 20_000_000n });
  tx = tx.pay.ToAddress(
    credentialToAddress("Custom", { type: "Key", hash: h.owner }),
    { lovelace: 3_000_000n },
  );
  return tx.complete({ coinSelection: false, localUPLCEval: true });
};

describe("cross-kind promotion and finalized fund release", () => {
  it("promotes both fillers without owner approval and retires to identical destinations using distinct outputs", async () => {
    const h = await setup();
    const payloads = await promoteBoth(h);
    const authority = await settlementForBoth(h, payloads);
    await h.submit(
      "retire-distinct-deposit-reserve-and-withdrawal-refund",
      await retireBoth(h, authority),
    );
    for (const a of h.applied)
      expect(await h.lucid.utxosAt(a.address)).toHaveLength(1);
  });
  it("rejects a shared promotion refund", async () => {
    const h = await setup();
    await expect(promoteBoth(h, true)).rejects.toThrow();
  });
  for (const attack of [
    "shared-funds",
    "declared-subset",
    "structural-payment-reuse",
  ] as const)
    it(`rejects retirement ${attack}`, async () => {
      const h = await setup();
      const payloads = await promoteBoth(h);
      const authority = await settlementForBoth(h, payloads);
      await expect(retireBoth(h, authority, attack)).rejects.toThrow();
      for (const a of h.applied)
        expect(await h.lucid.utxosAt(a.address)).toHaveLength(2);
    });
});
