import { createHash } from "node:crypto";
import { mkdirSync, writeFileSync } from "node:fs";
import { mkdtemp } from "node:fs/promises";
import { tmpdir } from "node:os";
import { dirname, join } from "node:path";
import { inspect } from "node:util";

import { decodeMidgardTxOutput } from "@al-ft/midgard-core/codec";
import { compareOutRefs, outRefLabel } from "@al-ft/midgard-core/out-ref";
import {
  aikenSerialisedPlutusDataCborPreservingMapOrder as ordered,
  plutusConstrFieldCbor,
  replacePlutusConstrFieldCbor,
} from "@al-ft/midgard-core/plutus-data-cbor";
import * as SDK from "@al-ft/midgard-sdk";
import {
  CML,
  Data,
  datumToHash,
  Emulator,
  fromText,
  generateEmulatorAccount,
  type UTxO,
  validatorToScriptHash,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { expect, it, vi } from "vitest";

import { DepositsDB } from "../src/database/index.js";
import { depositUTxOToEntry } from "../src/fibers/fetch-and-insert-deposit-utxos.js";
import { projectEventHistoryBlock } from "../src/l1-event-history-projection.js";
import {
  decodeBoundEventHistoryLedgerSnapshot,
  eventHistoryGenesisLosslessSha256,
  makeEventHistorySourceBinding,
} from "../src/l1-event-history-source.js";
import { TEST_CARDANO_PROTOCOL_PARAMETERS } from "./helpers/cardano-protocol-parameters.js";
import {
  type AcceptedHistoryObservation,
  historyOutputObservation,
  submitHistoryObservation,
} from "./helpers/history-projection-observations.js";
import {
  createMainnetEmulatorLucid,
  MAINNET_PROTOCOL_PARAMETERS,
  MAINNET_PROTOCOL_PARAMETERS_SOURCE,
} from "./helpers/mainnet-protocol-parameters.js";
import {
  createPublishedWorkflowDeploymentAccounts,
  publishWorkflowDeploymentOnChain,
} from "./helpers/published-workflow-deployment.js";
import { DEFAULT_PUBLICATION_SCHEDULE } from "./helpers/reference-publication-chain.js";

const hash = (value: string) =>
  createHash("sha256").update(value).digest("hex");
const field = (cbor: string, path: readonly number[]) =>
  ordered(plutusConstrFieldCbor(cbor, path));
const indexOf = (inputs: readonly UTxO[], target: UTxO) => {
  const index = [...inputs]
    .sort(compareOutRefs)
    .findIndex((input) => outRefLabel(input) === outRefLabel(target));
  if (index < 0) throw new Error("Missing actual raw admission input");
  return BigInt(index);
};
const distinct = (inputs: readonly UTxO[]) =>
  expect(new Set(inputs.map(outRefLabel)).size).toBe(inputs.length);
const plain = (utxo: UTxO) =>
  utxo.datum == null &&
  utxo.datumHash == null &&
  utxo.scriptRef == null &&
  Object.keys(utxo.assets).every((unit) => unit === "lovelace");
type Prepared = Effect.Effect.Success<
  ReturnType<typeof SDK.prepareDepositSubmissionProgram>
>;

/** Test-only admission assembly mirrors history-build's InsertOrder input,
 * reference, funding and observer rules. Raw bytes replace only datum encoding
 * BEFORE ordinary complete/local evaluation; no signed transaction is patched.
 * The fixture uses new sorted nonce keys, so no filler promotion is involved. */
const admitRaw = async (
  prepared: Prepared,
  payloadCbor: string,
  externalData: UTxO | undefined,
  validFrom: number,
  validTo: number,
) => {
  const { context, request } = prepared;
  const payload = Data.from(request.payloadCbor, SDK.EventHistoryPayload);
  const { lucid, recipe, applied, hubReference, scriptReference } = context;
  expect(recipe.kind).toBe("Deposit");
  expect(
    hubReference.assets[recipe.hubPolicyId + fromText("MIDGARD_HUB_ORACLE")],
  ).toBe(1n);
  const hub = Data.from(hubReference.datum!, SDK.HubOracleDatum);
  expect(hub.deposit).toBe(applied.policyId);
  expect(Data.to(hub.deposit_addr, SDK.AddressData)).toBe(
    Data.to(
      Effect.runSync(SDK.addressDataFromBech32(applied.address)),
      SDK.AddressData,
    ),
  );
  if (scriptReference !== undefined)
    expect(validatorToScriptHash(scriptReference.scriptRef!)).toBe(
      applied.policyId,
    );
  if (!("DepositPayload" in payload))
    throw new Error("Expected prepared Deposit payload");
  const id = payload.DepositPayload.event.id;
  expect(id).toEqual({
    transactionId: request.nonce.txHash,
    outputIndex: BigInt(request.nonce.outputIndex),
  });
  const key = datumToHash(Data.to(id, SDK.OutputReference));
  const deployment = {
    policyId: applied.policyId,
    address: applied.address,
    retentionAddress: applied.retention.address,
    inlineLimitBytes: recipe.inlineLimitBytes,
  };
  const witness = await SDK.fetchEventHistoryWitness(lucid, deployment, id);
  if (witness.kind !== "Absent")
    throw new Error("Raw admission requires authentic unused nonce gap");
  const { anchor } = witness;
  expect(anchor.key).not.toBe(key);
  const lower = BigInt(lucid.slotToUnixTime(lucid.unixTimeToSlot(validFrom)));
  const upper =
    BigInt(lucid.slotToUnixTime(lucid.unixTimeToSlot(validTo))) - 1n;
  expect(lower).toBeGreaterThanOrEqual(anchor.node.protected_until);
  expect(upper - lower).toBeLessThanOrEqual(
    BigInt(SDK.MAX_VALIDITY_RANGE_LENGTH_MS),
  );
  const typedPayload = Data.to(payload, SDK.EventHistoryPayload);
  const inline = BigInt(payloadCbor.length / 2) <= recipe.inlineLimitBytes;
  expect(inline).toBe(externalData === undefined);
  let location: SDK.EventHistoryFacts["location"];
  if (externalData === undefined) location = { Inline: { payload } };
  else {
    const [actual] = await lucid.utxosByOutRef([externalData]);
    expect(actual).toEqual(externalData);
    expect(actual!.address).toBe(applied.retention.address);
    expect(actual!.scriptRef).toBeUndefined();
    expect(field(actual!.datum!, [1])).toBe(payloadCbor);
    location = {
      External: { storage_datum_hash: datumToHash(ordered(actual!.datum!)) },
    };
  }
  const inputs = [...context.fundingInputs, request.nonce, anchor.utxo];
  const references = [
    hubReference,
    ...(scriptReference === undefined ? [] : [scriptReference]),
    ...(externalData === undefined ? [] : [externalData]),
  ];
  distinct(inputs);
  distinct(references);
  expect(
    inputs.some((input) =>
      references.some((ref) => outRefLabel(ref) === outRefLabel(input)),
    ),
  ).toBe(false);
  const protectedUntil = upper + recipe.protectionDurationMs;
  const node: SDK.EventHistoryNode = {
    position: { Key: [key] },
    next: anchor.node.next,
    protected_until: protectedUntil,
    payload: {
      Order: {
        facts: {
          event_id: id,
          inclusion_time: upper + BigInt(SDK.EVENT_WAIT_DURATION_MS),
          location,
          structural_lovelace: request.structuralLovelace,
          structural_refund_key: request.structuralRefundKey,
        },
      },
    },
  };
  SDK.assertEventHistoryAdmissionFunding(
    node,
    { ...request.assets, [applied.policyId + key]: 1n },
    applied.policyId,
    payload,
    request.structuralLovelace,
  );
  let nodeCbor = Data.to(node, SDK.EventHistoryNode);
  if (inline) {
    expect(field(nodeCbor, [3, 0, 2, 0])).toBe(ordered(typedPayload));
    nodeCbor = replacePlutusConstrFieldCbor(
      nodeCbor,
      [3, 0, 2, 0],
      payloadCbor,
    );
  }
  // Pointer/protection continuation preserves the previous raw facts verbatim.
  const nextSkeleton = Data.to(
    { ...anchor.node, next: key, protected_until: protectedUntil },
    SDK.EventHistoryNode,
  );
  const predecessorCbor = replacePlutusConstrFieldCbor(
    replacePlutusConstrFieldCbor(
      anchor.utxo.datum!,
      [1],
      plutusConstrFieldCbor(nextSkeleton, [1]),
    ),
    [2],
    Data.to(protectedUntil),
  );
  const operation: SDK.EventHistoryOperation = {
    InsertOrder: {
      predecessor_input_index: indexOf(inputs, anchor.utxo),
      predecessor_output_index: 0n,
      order_output_index: 1n,
      nonce_input_index: indexOf(inputs, request.nonce),
      external_reference_index:
        externalData === undefined ? null : indexOf(references, externalData),
    },
  };
  const observerCbor = Data.to(
    {
      Apply: {
        hub_reference_index: indexOf(references, hubReference),
        operation,
      },
    },
    SDK.EventHistoryObserve,
  );
  let tx = lucid
    .newTx()
    .collectFrom([...context.fundingInputs, request.nonce])
    .collectFrom([anchor.utxo], Data.to(indexOf(inputs, anchor.utxo)))
    .readFrom(references)
    .withdraw(applied.rewardAddress, 0n, observerCbor)
    .validFrom(validFrom)
    .validTo(validTo)
    .mintAssets({ [applied.policyId + key]: 1n }, Data.void())
    .pay.ToContract(
      applied.address,
      { kind: "inline", value: predecessorCbor },
      anchor.utxo.assets,
    )
    .pay.ToContract(
      applied.address,
      { kind: "inline", value: nodeCbor },
      { ...request.assets, [applied.policyId + key]: 1n },
    );
  if (scriptReference === undefined) tx = tx.attach.Script(applied.validator);
  const completed = await tx.complete({
    coinSelection: false,
    localUPLCEval: true,
    presetWalletInputs: [...context.fundingInputs, request.nonce],
  });
  const body = CML.Transaction.from_cbor_hex(completed.toCBOR()).body();
  const approved = new Set(
    [...context.fundingInputs, request.nonce].map(outRefLabel),
  );
  const collateral = body.collateral_inputs();
  for (let i = 0; i < (collateral?.len() ?? 0); i++) {
    const input = collateral!.get(i);
    expect(
      approved.has(`${input.transaction_id().to_hex()}#${input.index()}`),
    ).toBe(true);
  }
  return {
    completed,
    key,
    nodeCbor,
    predecessorCbor,
    anchor,
    inputs,
    references,
    observerCbor,
  };
};

it("preserves actual inline and external Deposit raw map pair order and duplicates through continuation, capture and node conversion", async () => {
  const accounts = createPublishedWorkflowDeploymentAccounts();
  const user = generateEmulatorAccount({ lovelace: 2_000_000_000n });
  const p = MAINNET_PROTOCOL_PARAMETERS;
  const emulator = new Emulator(
    [accounts.operator, accounts.publisher, user],
    p,
  );
  const operator = await createMainnetEmulatorLucid(emulator, "Custom");
  const publisher = await createMainnetEmulatorLucid(emulator, "Custom");
  const lucid = await createMainnetEmulatorLucid(emulator, "Custom");
  operator.selectWallet.fromSeed(accounts.operator.seedPhrase);
  publisher.selectWallet.fromSeed(accounts.publisher.seedPhrase);
  lucid.selectWallet.fromSeed(user.seedPhrase);
  const snapshot = {
    ...TEST_CARDANO_PROTOCOL_PARAMETERS,
    minFeeA: String(p.minFeeA),
    minFeeB: String(p.minFeeB),
    coinsPerUtxoByte: String(p.coinsPerUtxoByte),
    collateralPercentage: String(p.collateralPercentage),
    maxCollateralInputs: String(p.maxCollateralInputs),
    maxTxSize: String(p.maxTxSize),
    maxValueSize: String(p.maxValSize),
    maxTxExUnits: {
      memory: String(p.maxTxExMem),
      steps: String(p.maxTxExSteps),
    },
  };
  const deployment = await publishWorkflowDeploymentOnChain({
    network: "Custom",
    accounts,
    operatorLucid: operator,
    publisherLucid: publisher,
    chain: {
      now: () => emulator.now(),
      delaySlots: (n) => emulator.awaitSlot(n),
      awaitLedgerTime: (time) => {
        const slots = Math.ceil((time - emulator.now()) / 1000);
        if (slots > 0) emulator.awaitSlot(slots);
      },
    },
    protocolParameters: snapshot,
    publicationJournalPath: join(
      await mkdtemp(join(tmpdir(), "midgard-raw-deposit-")),
      "transactions.ndjson",
    ),
    publicationSchedule: DEFAULT_PUBLICATION_SCHEDULE,
    publicationSynchronize: async () => emulator.slot,
  });
  const { contracts, manifest } = deployment;
  const history = SDK.requireEventHistoryContracts(contracts).deposit;
  const historyDeployment = SDK.eventHistoryDeploymentFromContracts(history);
  const receipts: AcceptedHistoryObservation[] = [];
  const stages: unknown[] = [];
  let evidence: Record<string, unknown> = {
    status: "started",
    manifestId: manifest.manifestId,
    blueprintSha256: manifest.artifacts.blueprintHash,
    protocolParameters: p,
    protocolParametersSource: MAINNET_PROTOCOL_PARAMETERS_SOURCE,
  };
  vi.useFakeTimers({ toFake: ["Date"] });
  try {
    vi.setSystemTime(emulator.now());
    const blob = `590258${"ab".repeat(600)}`;
    const cases = [
      { name: "inline-2-1", datum: ordered("a2020a010b"), storage: "Inline" },
      {
        name: "external-2-1",
        datum: ordered(`a202${blob}010b`),
        storage: "External",
      },
      {
        name: "inline-2-1-2",
        datum: ordered("a3020a010b020c"),
        storage: "Inline",
      },
      {
        name: "external-2-1-2",
        datum: ordered(`a302${blob}010b020c`),
        storage: "External",
      },
      { name: "tail", datum: "00", storage: "Inline" },
    ] as const;
    for (const specimen of cases) {
      expect(
        ordered(CML.PlutusData.from_cbor_hex(specimen.datum).to_cbor_hex()),
      ).toBe(specimen.datum);
      if (specimen.name !== "tail")
        expect(
          ordered(
            CML.PlutusData.from_cbor_hex(
              specimen.datum,
            ).to_canonical_cbor_hex(),
          ),
        ).not.toBe(specimen.datum);
    }
    let split = lucid.newTx();
    for (let index = 0; index < cases.length; index++)
      split = split.pay.ToAddress(user.address, { lovelace: 30_000_000n });
    split = split.pay.ToAddress(user.address, { lovelace: 10_000_000n });
    const splitReceipt = await submitHistoryObservation(
      lucid,
      await split.complete({ localUPLCEval: true }),
    );
    receipts.push(splitReceipt);
    const nonces = (await lucid.utxosAt(user.address)).filter(
      (u) =>
        u.txHash === splitReceipt.transaction.txHash &&
        u.assets.lovelace === 30_000_000n,
    );
    expect(nonces).toHaveLength(cases.length);
    const key = (u: UTxO) =>
      Effect.runSync(
        SDK.eventHistoryKey({
          transactionId: u.txHash,
          outputIndex: BigInt(u.outputIndex),
        }),
      );
    nonces.sort((a, b) => key(a).localeCompare(key(b)));
    const reserved = new Set(nonces.map(outRefLabel));
    const selectFunding = async (nonce: UTxO) =>
      lucid.overrideUTxOs(
        (await lucid.utxosAt(user.address)).filter(
          (u) =>
            plain(u) &&
            (!reserved.has(outRefLabel(u)) ||
              outRefLabel(u) === outRefLabel(nonce)),
        ),
      );
    const binding = await Effect.runPromise(
      makeEventHistorySourceBinding({
        contracts,
        identity: {
          kind: "manifest",
          manifest,
          manifestId: manifest.manifestId,
          consensusProfile: manifest.consensusProfile,
        },
        network: "Custom",
        ogmiosUrl: "http://raw-deposit-emulator.invalid:1337",
        expectedGenesisLosslessSha256: eventHistoryGenesisLosslessSha256({
          scope: "synthetic raw datum fixture",
          initialization: deployment.initialization.txHash,
        }),
      }),
    );
    const addresses = [
      binding.hubAddress,
      ...Object.values(binding.deployments).flatMap((d) => [
        d.address,
        d.retentionAddress,
      ]),
    ];
    let height = 0;
    const providerCapture = async (point: { slot: number; id: string }) =>
      Effect.runPromise(
        decodeBoundEventHistoryLedgerSnapshot(
          {
            point: { slot: point.slot, id: point.id },
            addresses,
            outputs: (
              await Promise.all(
                addresses.map((address) => lucid.utxosAt(address)),
              )
            )
              .flat()
              .map(historyOutputObservation),
          },
          binding,
        ),
      );
    let capture = await providerCapture({
      slot: emulator.slot,
      id: hash(`raw-initial:${splitReceipt.transaction.txHash}`),
    });
    const project = async (receipt: AcceptedHistoryObservation) => {
      const point = {
        slot: emulator.slot,
        height: ++height,
        id: hash(`raw-block:${receipt.transaction.txHash}`),
      };
      const archive = new Map(
        receipt.historical.map((output) => [outRefLabel(output), output]),
      );
      const projected = await projectEventHistoryBlock({
        previous: capture,
        block: {
          point,
          parent: capture.history.ledger.point.id,
          transactions: [receipt.transaction],
        },
        binding,
        histories: SDK.requireEventHistoryContracts(contracts),
        slotToUnixTime: lucid.slotToUnixTime,
        resolveReference: (_tx, ref) => archive.get(outRefLabel(ref)),
      });
      const actual = await providerCapture(point);
      expect(projected.capture.snapshotDigest).toBe(actual.snapshotDigest);
      expect(projected.capture.history.deposits).toEqual(
        actual.history.deposits,
      );
      capture = projected.capture;
      return {
        point,
        projectedSnapshotDigest: projected.capture.snapshotDigest,
        providerSnapshotDigest: actual.snapshotDigest,
        transitions: projected.transitions,
      };
    };
    const admitted = new Map<
      string,
      {
        payloadCbor: string;
        datum: string;
        factsCbor: string;
        capture: ReturnType<typeof SDK.captureEventHistoryWitness>;
        order: SDK.DepositUTxO;
      }
    >();
    for (const [ordinal, specimen] of cases.entries()) {
      const nonce = nonces[ordinal]!;
      const nodes = SDK.authenticateHistoryNodes(
        await lucid.utxosAt(history.list.spendingScriptAddress),
        historyDeployment,
      );
      const protectedUntil = nodes.reduce(
        (n, { node }) => (node.protected_until > n ? node.protected_until : n),
        0n,
      );
      await deployment.chain.awaitLedgerTime(Number(protectedUntil) + 60_000);
      vi.setSystemTime(emulator.now());
      const prepare = async () => {
        await selectFunding(nonce);
        return Effect.runPromise(
          SDK.prepareDepositSubmissionProgram(lucid, contracts, {
            nonceInput: nonce,
            l2Address: user.address,
            l2Datum: "00",
            lovelace: 5_000_000n,
            additionalAssets: {},
            structuralLovelace: 3_000_000n,
            referenceScripts: {
              depositMinting: deployment.references.get("depositMint")!,
            },
          }),
        );
      };
      let prepared = await prepare();
      const payloadCbor = ordered(
        replacePlutusConstrFieldCbor(
          prepared.request.payloadCbor,
          [0, 1, 2, 0],
          specimen.datum,
        ),
      );
      expect(field(payloadCbor, [0, 1, 2, 0])).toBe(specimen.datum);
      expect(BigInt(payloadCbor.length / 2)).toBeLessThanOrEqual(
        history.recipe.maxPayloadBytes,
      );
      expect(
        BigInt(payloadCbor.length / 2) <= history.recipe.inlineLimitBytes,
      ).toBe(specimen.storage === "Inline");
      let retained: UTxO | undefined;
      if (specimen.storage === "External") {
        const retainedCbor = ordered(
          replacePlutusConstrFieldCbor(
            Data.to(
              {
                event_key: key(nonce),
                event_payload: Data.from(prepared.request.payloadCbor),
                reclaim_auth: prepared.request.reclaimAuth,
              },
              SDK.EventHistoryData,
            ),
            [1],
            payloadCbor,
          ),
        );
        const publication = await prepared.context.lucid
          .newTx()
          .collectFrom([...prepared.context.fundingInputs])
          .pay.ToContract(
            prepared.context.applied.retention.address,
            { kind: "inline", value: retainedCbor },
            {},
          )
          .complete({ coinSelection: false, localUPLCEval: true });
        const receipt = await submitHistoryObservation(lucid, publication);
        receipts.push(receipt);
        expect(
          receipt.transaction.inputs.some(
            (input) => outRefLabel(input) === outRefLabel(nonce),
          ),
        ).toBe(false);
        [retained] = await lucid.utxosByOutRef([
          { txHash: receipt.transaction.txHash, outputIndex: 0 },
        ]);
        expect(ordered(retained!.datum!)).toBe(retainedCbor);
        stages.push({
          name: specimen.name,
          publication: await project(receipt),
          retained,
        });
        prepared = await prepare();
      }
      vi.setSystemTime(emulator.now());
      const validFrom = emulator.now() - 60_000;
      const validTo = SDK.resolveUserEventValidTo(lucid);
      const built = await admitRaw(
        prepared,
        payloadCbor,
        retained,
        validFrom,
        validTo,
      );
      const accepted = await submitHistoryObservation(lucid, built.completed);
      receipts.push(accepted);
      reserved.delete(outRefLabel(nonce));
      expect(accepted.transaction.inputs).toEqual(
        [...built.inputs]
          .sort(compareOutRefs)
          .map(({ txHash, outputIndex }) => ({ txHash, outputIndex })),
      );
      expect(accepted.transaction.references).toEqual(
        [...built.references]
          .sort(compareOutRefs)
          .map(({ txHash, outputIndex }) => ({ txHash, outputIndex })),
      );
      expect(ordered(accepted.transaction.outputs[1]!.datum!)).toBe(
        ordered(built.nodeCbor),
      );
      expect(ordered(accepted.transaction.outputs[0]!.datum!)).toBe(
        ordered(built.predecessorCbor),
      );
      expect(
        accepted.transaction.redeemers.some(
          (redeemer) =>
            redeemer.purpose === "withdraw" &&
            ordered(redeemer.cbor) === ordered(built.observerCbor),
        ),
      ).toBe(true);
      const projected = await project(accepted);
      const orders = await Effect.runPromise(
        SDK.fetchDepositUTxOsProgram(lucid, historyDeployment),
      );
      const order = orders.find((order) => order.assetName === built.key)!;
      expect(order).toBeDefined();
      const captured = SDK.captureEventHistoryWitness(
        order.history,
        history.list.policyId,
        "Deposit",
      );
      expect(order.originalAssets).toEqual({ lovelace: 5_000_000n });
      expect(order.utxo.assets[history.list.policyId + built.key]).toBe(1n);
      expect(order.utxo.assets.lovelace).toBe(8_000_000n);
      expect(order.history.payloadCbor).toBe(payloadCbor);
      expect(order.infoCbor.toString("hex")).toBe(field(payloadCbor, [0, 1]));
      expect(captured.payloadCbor).toBe(payloadCbor);
      expect(captured.factsCbor).toBe(field(order.utxo.datum!, [3, 0]));
      expect(captured.commitment.payload_hash).toBe(datumToHash(payloadCbor));
      expect(field(captured.openingCbor, [0])).toBe(payloadCbor);
      expect(field(captured.openingCbor, [1])).toBe(
        ordered(Data.to(SDK.assetsToValue(order.originalAssets), SDK.Value)),
      );
      expect(
        SDK.opensEventHistoryCommitmentCbor(
          captured.commitment,
          payloadCbor,
          field(captured.openingCbor, [1]),
        ),
      ).toBe(true);
      const entry = await Effect.runPromise(
        depositUTxOToEntry(order, "Custom"),
      );
      const converted = decodeMidgardTxOutput(
        entry[DepositsDB.Columns.LEDGER_OUTPUT],
      );
      expect(converted.datum?.cbor.toString("hex")).toBe(specimen.datum);
      expect(entry[DepositsDB.Columns.INFO]).toEqual(order.infoCbor);
      expect(converted.value).toEqual({
        lovelace: 5_000_000n,
        assets: new Map(),
      });
      admitted.set(built.key, {
        payloadCbor,
        datum: specimen.datum,
        factsCbor: captured.factsCbor,
        capture: captured,
        order,
      });
      let continuation: unknown;
      if (ordinal > 0) {
        const previous = admitted.get(built.anchor.key!)!;
        const refreshed = orders.find(
          (order) => order.assetName === built.anchor.key,
        )!;
        expect(refreshed.utxo.txHash).not.toBe(previous.order.utxo.txHash);
        expect(refreshed.utxo.assets).toEqual(previous.order.utxo.assets);
        const after = SDK.captureEventHistoryWitness(
          refreshed.history,
          history.list.policyId,
          "Deposit",
        );
        expect(after).toEqual(previous.capture);
        expect(field(refreshed.utxo.datum!, [3, 0])).toBe(previous.factsCbor);
        expect(refreshed.infoCbor).toEqual(previous.order.infoCbor);
        expect(refreshed.history.anchor.node.next).toBe(built.key);
        expect(refreshed.history.anchor.node.protected_until).toBeGreaterThan(
          previous.order.history.anchor.node.protected_until,
        );
        const reprojected = await Effect.runPromise(
          depositUTxOToEntry(refreshed, "Custom"),
        );
        expect(
          decodeMidgardTxOutput(
            reprojected[DepositsDB.Columns.LEDGER_OUTPUT],
          ).datum?.cbor.toString("hex"),
        ).toBe(previous.datum);
        continuation = {
          before: previous.order.utxo,
          after: refreshed.utxo,
          capture: after,
          nodeEntry: reprojected,
        };
      }
      stages.push({
        name: specimen.name,
        rawDatum: specimen.datum,
        payloadCbor,
        projected,
        order: order.utxo,
        captured,
        nodeEntry: entry,
        converted,
        continuation,
      });
    }
    expect(admitted.size).toBe(5);
    expect(capture.history.deposits).toHaveLength(5);
    expect(capture.history.withdrawals).toHaveLength(0);
    for (const receipt of receipts) {
      expect(receipt.measurement.completeSignedBytes).toBeLessThanOrEqual(
        p.maxTxSize,
      );
      expect(receipt.measurement.executionMemory).toBeLessThanOrEqual(
        p.maxTxExMem,
      );
      expect(receipt.measurement.executionSteps).toBeLessThanOrEqual(
        p.maxTxExSteps,
      );
    }
    evidence = {
      ...evidence,
      status: "passed",
      binding,
      capture,
      scope:
        "Actual raw Deposit admissions/retention publications and continuation; synthetic transport point hashes/heights/genesis; pure node converter, no DB persistence or L2 settlement claim",
    };
  } catch (cause) {
    evidence = {
      ...evidence,
      status: "failed",
      cause: inspect(cause, { depth: 12 }),
    };
    throw cause;
  } finally {
    vi.useRealTimers();
    const path = process.env.MIDGARD_RAW_DEPOSIT_EVIDENCE_PATH;
    if (path !== undefined) {
      mkdirSync(dirname(path), { recursive: true });
      writeFileSync(
        path,
        JSON.stringify(
          { ...evidence, receipts, stages },
          (_key, value) =>
            typeof value === "bigint" ? value.toString() : value,
          2,
        ) + "\n",
      );
    }
  }
});
