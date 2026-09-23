/** Actual list mutations and fabricated proof stages 02/03. The hub and initial
 * CT issuer are native fixtures: this does not claim full installed-family acceptance. */
import { createHash } from "node:crypto";
import { mkdirSync, readFileSync, writeFileSync } from "node:fs";
import { join } from "node:path";

import * as SDK from "@al-ft/midgard-sdk";
import {
  CML,
  Data,
  type Script,
  type UTxO,
  validatorToAddress,
  validatorToScriptHash,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { afterAll, describe, expect, it, vi } from "vitest";

import { submitFabricatedDepositStep02 } from "../src/submit-fabricated-deposit-step-02.js";
import { submitFabricatedDepositStep03 } from "../src/submit-fabricated-deposit-step-03.js";
import { submitFabricatedWithdrawalStep02 } from "../src/submit-fabricated-withdrawal-step-02.js";
import { submitFabricatedWithdrawalStep03 } from "../src/submit-fabricated-withdrawal-step-03.js";
import { workflowTransactionReferenceInputOutRefs } from "../src/workflow/transaction-boundary.js";
import { realBlueprintPath } from "./support/emulator/blueprints.js";
import {
  historyPairPayloads,
  index,
  insertHistoryFillerAfter as buildChurn,
  promoteHistoryPair,
  setupHistoryPair,
} from "./support/emulator/history-pair.js";
import { measureCompleteSignedTransaction } from "./support/emulator/measurement.js";
import { EMULATOR_PROTOCOL_PARAMETERS } from "./support/emulator/protocol-parameters.js";

const blueprintBytes = readFileSync(realBlueprintPath);
const blueprint = SDK.parseFaultProofBlueprint(
  JSON.parse(blueprintBytes.toString()),
);
const records: unknown[] = [];
const protectionDurationMs = 120_000n;
const timing = {
  visibilityBudgetMs: 20_000n,
  submissionBudgetMs: 40_000n,
  remainingProofBudgetMs: 80_000n,
  slotLengthMs: 1000n,
};
type Harness = Awaited<ReturnType<typeof setupHistoryPair>>;
type Kind = "Deposit" | "Withdrawal";
const familyIndex = (kind: Kind) => (kind === "Deposit" ? 0 : 1);
const deployment = (h: Harness, kind: Kind) => {
  const a = h.applied[familyIndex(kind)]!;
  return {
    policyId: a.policyId,
    address: a.address,
    retentionAddress: a.retention.address,
    inlineLimitBytes: 512n,
  };
};
const fetchWitness = (h: Harness, kind: Kind, id: SDK.OutputReference) =>
  SDK.fetchEventHistoryWitness(
    { utxosAt: (address) => h.lucid.utxosAt(address) },
    deployment(h, kind),
    id,
  );

// Divide precisely the gap containing the selected identity, or change an
// Order's own successor. Both cases consume the challenger's exact reference.
const nextChurnKey = async (
  w: SDK.EventHistoryWitness,
  id: SDK.OutputReference,
) => {
  const target = BigInt(
    "0x" + (await Effect.runPromise(SDK.eventHistoryKey(id))),
  );
  const lower = BigInt("0x" + (w.anchor.key ?? "00".repeat(32)));
  const upper =
    w.kind === "Present"
      ? BigInt("0x" + (w.anchor.node.next ?? "ff".repeat(32)))
      : target;
  const key = (lower + upper) / 2n;
  if (key <= lower || key >= upper)
    throw new Error("Fixture exhausted its insertion gap");
  return key.toString(16).padStart(64, "0");
};
const waitForMutation = (h: Harness, w: SDK.EventHistoryWitness) => {
  const remaining = w.anchor.node.protected_until - BigInt(h.emulator.now());
  if (remaining > 0n) h.emulator.awaitSlot(Number((remaining + 999n) / 1000n));
};

const setupProof = async (
  h: Harness,
  kind: Kind,
  id: SDK.OutputReference,
  headerEnd: bigint,
  committedHash = "00".repeat(32),
) => {
  const title =
    kind === "Deposit" ? "fabricated_deposit" : "fabricated_withdrawal";
  const step03: Script = {
    type: "PlutusV3",
    script: SDK.applyBlueprintParams(
      blueprint,
      `fraud_proofs/${title}/step_03.main.spend`,
      [h.hubPolicy, h.hubPolicy],
    ),
  };
  const step02: Script = {
    type: "PlutusV3",
    script: SDK.applyBlueprintParams(
      blueprint,
      `fraud_proofs/${title}/step_02.main.spend`,
      [
        validatorToScriptHash(step03),
        h.hubPolicy,
        h.hubPolicy,
        Data.from(
          Data.to(
            Effect.runSync(
              SDK.addressDataFromBech32(deployment(h, kind).retentionAddress),
            ),
            SDK.AddressData,
          ),
        ),
        512n,
        5000n,
        512n,
      ],
    ),
  };
  const scripts = [step02, step03];
  const refs: UTxO[] = [];
  for (const script of scripts) {
    const hash = await h.submit(
      `publish-${kind}-capture-script`,
      await h.lucid
        .newTx()
        .collectFrom(await h.funding())
        .pay.ToAddressWithData(
          h.hubAddress,
          undefined,
          { lovelace: 80_000_000n },
          script,
        )
        .complete({ coinSelection: false, localUPLCEval: true }),
    );
    refs.push(
      (await h.lucid.utxosByOutRef([{ txHash: hash, outputIndex: 0 }]))[0]!,
    );
  }
  const common = {
    state_queue_policy: h.hubPolicy,
    challenged_header_hash: "ab".repeat(28),
    header_start_time: headerEnd - 100_000n,
    header_end_time: headerEnd,
  };
  const state02 =
    kind === "Deposit"
      ? Data.to(
          {
            fraud_prover: h.owner,
            data: {
              ...common,
              committed_deposit_id: id,
              committed_deposit_info_hash: committedHash,
            },
          },
          SDK.FabricatedDepositStep02Datum,
        )
      : Data.to(
          {
            fraud_prover: h.owner,
            data: {
              ...common,
              committed_withdrawal_id: id,
              committed_withdrawal_content_hash: committedHash,
            },
          },
          SDK.FabricatedWithdrawalStep02Datum,
        );
  const unit =
    h.hubPolicy +
    (kind === "Deposit"
      ? SDK.FABRICATED_DEPOSIT_FRAUD_CATEGORY_ID
      : SDK.FABRICATED_WITHDRAWAL_FRAUD_CATEGORY_ID) +
    common.challenged_header_hash;
  const txHash = await h.submit(
    `fixture-issue-${kind}-step02`,
    await h.lucid
      .newTx()
      .collectFrom(await h.funding())
      .mintAssets({ [unit]: 1n })
      .attach.MintingPolicy(h.issuer)
      .pay.ToContract(
        validatorToAddress("Custom", step02),
        { kind: "inline", value: state02 },
        { lovelace: 4_000_000n, [unit]: 1n },
      )
      .pay.ToAddress(h.wallet.address, { lovelace: 20_000_000n })
      .complete({ coinSelection: false, localUPLCEval: true }),
  );
  const [thread, fee] = await h.lucid.utxosByOutRef([
    { txHash, outputIndex: 0 },
    { txHash, outputIndex: 1 },
  ]);
  return {
    kind,
    id,
    headerEnd,
    common,
    committedHash,
    scripts,
    refs,
    thread: thread!,
    fee: fee!,
    unit,
  };
};
type Proof = Awaited<ReturnType<typeof setupProof>>;
const buildCapture = async (
  h: Harness,
  p: Proof,
  w: SDK.EventHistoryWitness,
  window: SDK.EventHistoryCaptureWindow,
) => {
  const inputs = [p.thread, p.fee];
  const refs = [
    h.hub,
    p.refs[0]!,
    w.anchor.utxo,
    ...(w.kind === "Present" && w.retainedDataUtxo ? [w.retainedDataUtxo] : []),
  ];
  const captured =
    w.kind === "Present"
      ? SDK.captureEventHistoryWitness(
          w,
          deployment(h, p.kind).policyId,
          p.kind,
        )
      : undefined;
  let datum: string;
  let redeemer: string;
  const commonArgs = { input_index: index(inputs, p.thread), output_index: 0n };
  const evidenceIndices = {
    hub_ref_input_index: index(refs, h.hub),
    event_ref_input_index: index(refs, w.anchor.utxo),
    external_ref_input_index:
      w.kind === "Present" && w.retainedDataUtxo
        ? index(refs, w.retainedDataUtxo)
        : null,
  };
  if (p.kind === "Deposit") {
    const state = Data.from(
      p.thread.datum!,
      SDK.FabricatedDepositStep02Datum,
    ).data!;
    datum = Data.to(
      {
        fraud_prover: h.owner,
        data: SDK.fabricatedDepositStep03State(
          state,
          captured
            ? { DepositEventObserved: { commitment: captured.commitment } }
            : "DepositIdentityAbsent",
        ),
      },
      SDK.FabricatedDepositStep03Datum,
    );
    redeemer = Data.to(
      {
        Continue: [
          {
            ...commonArgs,
            evidence: captured
              ? { PresentDepositEvent: evidenceIndices }
              : {
                  AbsentDepositIdentity: {
                    hub_ref_input_index: index(refs, h.hub),
                    history_ref_input_index: index(refs, w.anchor.utxo),
                  },
                },
          },
        ],
      },
      SDK.FabricatedDepositStep02SpendRedeemer,
    );
  } else {
    const state = Data.from(
      p.thread.datum!,
      SDK.FabricatedWithdrawalStep02Datum,
    ).data!;
    datum = Data.to(
      {
        fraud_prover: h.owner,
        data: SDK.fabricatedWithdrawalStep03State(
          state,
          captured
            ? { WithdrawalEventObserved: { commitment: captured.commitment } }
            : "WithdrawalIdentityAbsent",
        ),
      },
      SDK.FabricatedWithdrawalStep03Datum,
    );
    redeemer = Data.to(
      {
        Continue: [
          {
            ...commonArgs,
            evidence: captured
              ? { PresentWithdrawalEvent: evidenceIndices }
              : {
                  AbsentWithdrawalIdentity: {
                    hub_ref_input_index: index(refs, h.hub),
                    history_ref_input_index: index(refs, w.anchor.utxo),
                  },
                },
          },
        ],
      },
      SDK.FabricatedWithdrawalStep02SpendRedeemer,
    );
  }
  const tx = await h.lucid
    .newTx()
    .collectFrom([p.fee])
    .collectFrom([p.thread], redeemer)
    .readFrom(refs)
    .validFrom(Number(window.validFrom))
    .validTo(Number(window.validTo))
    .pay.ToContract(
      validatorToAddress("Custom", p.scripts[1]!),
      { kind: "inline", value: datum },
      p.thread.assets,
    )
    .complete({ coinSelection: false, localUPLCEval: true });
  return { tx, captured, datum };
};

const continueCaptured = async (
  h: Harness,
  p: Proof,
  thread: UTxO,
  captured: ReturnType<typeof SDK.captureEventHistoryWitness> | undefined,
  badOpening = false,
) => {
  const funding = await h.funding();
  const inputs = [...funding, thread];
  const commonArgs = { input_index: index(inputs, thread), output_index: 0n };
  // Serialize and reopen preimage bytes without retrieving a historical L1 node.
  // Full process restart/persistence acceptance remains a separate gate.
  const opening = captured
    ? {
        RetainedEventData: {
          payload: captured.payload,
          original_assets: badOpening
            ? new Map([["", new Map([["", 1n]])]])
            : captured.originalAssets,
        },
      }
    : "NoAuthenticContent";
  let datum: string;
  let redeemer: string;
  if (p.kind === "Deposit") {
    const state = Data.from(
      thread.datum!,
      SDK.FabricatedDepositStep03Datum,
    ).data!;
    const fault: SDK.FabricatedDepositFault = captured
      ? {
          IneligibleDepositEvent: {
            event_inclusion_time: captured.commitment.inclusion_time,
          },
        }
      : "NonexistentDepositIdentity";
    datum = Data.to(
      {
        fraud_prover: h.owner,
        data: SDK.fabricatedDepositStep04State(state, fault),
      },
      SDK.FabricatedDepositStep04Datum,
    );
    const bytes = Data.to(
      opening,
      SDK.FabricatedDepositAuthenticContentOpening,
    );
    redeemer = Data.to(
      {
        Continue: [
          {
            ...commonArgs,
            authentic_content: Data.from(
              bytes,
              SDK.FabricatedDepositAuthenticContentOpening,
            ),
          },
        ],
      },
      SDK.FabricatedDepositStep03SpendRedeemer,
    );
  } else {
    const state = Data.from(
      thread.datum!,
      SDK.FabricatedWithdrawalStep03Datum,
    ).data!;
    const fault: SDK.FabricatedWithdrawalFault = captured
      ? {
          IneligibleWithdrawalEvent: {
            event_inclusion_time: captured.commitment.inclusion_time,
          },
        }
      : "NonexistentWithdrawalIdentity";
    datum = Data.to(
      {
        fraud_prover: h.owner,
        data: SDK.fabricatedWithdrawalStep04State(state, fault),
      },
      SDK.FabricatedWithdrawalStep04Datum,
    );
    const bytes = Data.to(
      opening,
      SDK.FabricatedWithdrawalAuthenticContentOpening,
    );
    redeemer = Data.to(
      {
        Continue: [
          {
            ...commonArgs,
            authentic_content: Data.from(
              bytes,
              SDK.FabricatedWithdrawalAuthenticContentOpening,
            ),
          },
        ],
      },
      SDK.FabricatedWithdrawalStep03SpendRedeemer,
    );
  }
  return h.lucid
    .newTx()
    .collectFrom(funding)
    .collectFrom([thread], redeemer)
    .readFrom([p.refs[1]!])
    .validFrom(Math.max(Number(p.headerEnd) + 1000, h.emulator.now() - 60_000))
    .validTo(h.emulator.now() + 40_000)
    .pay.ToContract(
      h.hubAddress,
      { kind: "inline", value: datum },
      thread.assets,
    )
    .complete({ coinSelection: false, localUPLCEval: true });
};

const productionContracts = (h: Harness, p: Proof) => {
  const step = (script: Script) => ({
    spendingScript: script,
    spendingScriptHash: validatorToScriptHash(script),
    spendingScriptAddress: validatorToAddress("Custom", script),
  });
  return {
    history: {
      inlineLimitBytes: 512n,
      maxPayloadBytes: 5000n,
      maxPayloadNodes: 512n,
      retentionAddress: deployment(h, p.kind).retentionAddress,
    },
    steps: [
      step(p.scripts[0]!),
      step(p.scripts[0]!),
      step(p.scripts[1]!),
      step(h.issuer),
    ] as const,
    computationThread: { policyId: h.hubPolicy, mintingScript: h.issuer },
    fraudProof: {
      policyId: h.hubPolicy,
      mintingScript: h.issuer,
      spendingScriptAddress: h.hubAddress,
    },
    hubOraclePolicyId: h.hubPolicy,
    stateQueuePolicyId: h.hubPolicy,
    categoryId:
      p.kind === "Deposit"
        ? SDK.FABRICATED_DEPOSIT_FRAUD_CATEGORY_ID
        : SDK.FABRICATED_WITHDRAWAL_FRAUD_CATEGORY_ID,
  };
};

const submitProductionClassification = async (
  h: Harness,
  p: Proof,
  thread: UTxO,
  captured: ReturnType<typeof SDK.captureEventHistoryWitness> | undefined,
) => {
  const contracts = productionContracts(h, p);
  const submit =
    p.kind === "Deposit"
      ? submitFabricatedDepositStep03
      : submitFabricatedWithdrawalStep03;
  return submit({
    lucid: h.lucid,
    contracts,
    signer: {
      source: "emulator fixture",
      address: h.wallet.address,
      paymentKeyHash: h.owner,
      selectWallet: (lucid) => lucid.selectWallet.fromSeed(h.wallet.seedPhrase),
    },
    threadOutRef: `${thread.txHash}#${thread.outputIndex}`,
    openingCbor: captured
      ? Data.to(
          {
            RetainedEventData: {
              payload: captured.payload,
              original_assets: captured.originalAssets,
            },
          },
          SDK.FabricatedDepositAuthenticContentOpening,
        )
      : undefined,
    referenceScriptUtxo: p.refs[1]!,
    now: () => h.emulator.now(),
    preSubmitBoundary: ({ signed, txHash }) => {
      const transactionCbor = signed.toCBOR();
      const measurement = measureCompleteSignedTransaction(transactionCbor);
      expect(measurement.completeSignedBytes).toBeLessThanOrEqual(
        EMULATOR_PROTOCOL_PARAMETERS.maxTxSize,
      );
      expect(measurement.executionMemory).toBeLessThanOrEqual(
        EMULATOR_PROTOCOL_PARAMETERS.maxTxExMem,
      );
      expect(measurement.executionSteps).toBeLessThanOrEqual(
        EMULATOR_PROTOCOL_PARAMETERS.maxTxExSteps,
      );
      records.push({
        label: `${p.kind}-production-retained-classification`,
        txHash,
        transactionCbor,
        measurement,
        fee: CML.Transaction.from_cbor_hex(transactionCbor).body().fee(),
      });
    },
  });
};

afterAll(() => {
  const dir = join(process.cwd(), "../../artifacts/event-history");
  mkdirSync(dir, { recursive: true });
  writeFileSync(
    join(dir, "history-capture-applied.json"),
    JSON.stringify(
      {
        scope:
          "Actual list policies and fabricated steps 02/03; native fixture hub and initial CT issuer. Simulated bounded inclusion, not live or full-family acceptance.",
        blueprintSha256: createHash("sha256")
          .update(blueprintBytes)
          .digest("hex"),
        protectionDurationMs,
        timing,
        protocolParameters: EMULATOR_PROTOCOL_PARAMETERS,
        records,
      },
      (_, v) => (typeof v === "bigint" ? v.toString() : v),
      2,
    ) + "\n",
  );
});

for (const kind of ["Deposit", "Withdrawal"] as const)
  describe(`${kind} history proof capture`, () => {
    for (const mode of ["absent", "inline", "external"] as const)
      it(`production capture discovers current ${mode} history and ignores stale pointer hints`, async () => {
        const presence = mode !== "absent";
        const h = await setupHistoryPair({
          blueprint,
          records,
          protectionDurationMs,
        });
        if (presence) {
          const payloads = historyPairPayloads(h);
          if (mode === "external")
            for (const payload of payloads) {
              if ("DepositPayload" in payload)
                payload.DepositPayload.event.info.l2_datum = "ab".repeat(2000);
              else
                payload.WithdrawalPayload.event.info.body.l1_datum = {
                  InlineDatum: { data: "cd".repeat(2000) },
                };
            }
          await promoteHistoryPair(h, false, payloads);
        }
        const nonce = h.eventNonces[familyIndex(kind)]!;
        const id = presence
          ? {
              transactionId: nonce.txHash,
              outputIndex: BigInt(nonce.outputIndex),
            }
          : { transactionId: "fe".repeat(32), outputIndex: 987654321n };
        const headerEnd =
          BigInt(h.emulator.now()) - SDK.MATURITY_DURATION_MS + 700_000n;
        const p = await setupProof(h, kind, id, headerEnd);
        const old = await fetchWitness(h, kind, id);
        waitForMutation(h, old);
        await h.submit(
          `${kind}-production-pointer-churn`,
          await buildChurn(h, kind, old, await nextChurnKey(old, id), [p.fee]),
        );
        const current = await fetchWitness(h, kind, id);
        const captured =
          current.kind === "Present"
            ? SDK.captureEventHistoryWitness(
                current,
                deployment(h, kind).policyId,
                kind,
              )
            : undefined;
        const openingCbor = captured
          ? Data.to(
              {
                RetainedEventData: {
                  payload: captured.payload,
                  original_assets: captured.originalAssets,
                },
              },
              SDK.FabricatedDepositAuthenticContentOpening,
            )
          : null;
        const submit =
          kind === "Deposit"
            ? submitFabricatedDepositStep02
            : submitFabricatedWithdrawalStep02;
        const args = {
          lucid: h.lucid,
          contracts: productionContracts(h, p),
          network: "Custom" as const,
          signer: {
            source: "emulator fixture",
            address: h.wallet.address,
            paymentKeyHash: h.owner,
            selectWallet: (lucid: typeof h.lucid) =>
              lucid.selectWallet.fromSeed(h.wallet.seedPhrase),
          },
          threadOutRef: `${p.thread.txHash}#${p.thread.outputIndex}`,
          evidence: presence
            ? {
                kind: "present_event" as const,
                eventOutRef: `${old.anchor.utxo.txHash}#${old.anchor.utxo.outputIndex}`,
              }
            : { kind: "absent_identity" as const },
          referenceScriptUtxo: p.refs[0]!,
          expectedOpeningCbor: openingCbor,
          now: () => h.emulator.now(),
        };
        await expect(
          submit({
            ...args,
            evidence: presence
              ? { kind: "absent_identity" }
              : { kind: "present_event" },
          }),
        ).rejects.toThrow("History facts changed");
        if (presence)
          await expect(
            submit({ ...args, expectedOpeningCbor: null }),
          ).rejects.toThrow("History facts changed");
        await expect(
          submit({
            ...args,
            contracts: {
              ...args.contracts,
              stateQueuePolicyId: "ab".repeat(28),
            },
          }),
        ).rejects.toThrow("state queue policy");
        await expect(
          submit({
            ...args,
            now: () => Number(headerEnd + SDK.MATURITY_DURATION_MS),
          }),
        ).rejects.toThrow("no usable validity window");
        if (mode === "external") {
          const lookup = h.lucid.utxosAt.bind(h.lucid);
          const unavailable = vi
            .spyOn(h.lucid, "utxosAt")
            .mockImplementation((address) =>
              address === deployment(h, kind).retentionAddress
                ? Promise.resolve([])
                : lookup(address),
            );
          try {
            await expect(submit(args)).rejects.toThrow(
              "retained event data is unavailable on L1",
            );
          } finally {
            unavailable.mockRestore();
          }
        }
        const result = await submit({
          ...args,
          preSubmitBoundary: ({ signed, txHash }) => {
            if (mode === "external") {
              expect(current.kind).toBe("Present");
              if (current.kind !== "Present" || !current.retainedDataUtxo)
                throw new Error("Missing external fixture");
              expect(
                workflowTransactionReferenceInputOutRefs(signed),
              ).toContain(
                `${current.retainedDataUtxo.txHash}#${current.retainedDataUtxo.outputIndex}`,
              );
            }
            const transactionCbor = signed.toCBOR();
            const measurement =
              measureCompleteSignedTransaction(transactionCbor);
            expect(measurement.completeSignedBytes).toBeLessThanOrEqual(
              EMULATOR_PROTOCOL_PARAMETERS.maxTxSize,
            );
            expect(measurement.executionMemory).toBeLessThanOrEqual(
              EMULATOR_PROTOCOL_PARAMETERS.maxTxExMem,
            );
            expect(measurement.executionSteps).toBeLessThanOrEqual(
              EMULATOR_PROTOCOL_PARAMETERS.maxTxExSteps,
            );
            records.push({
              label: `${kind}-production-capture-${mode}`,
              txHash,
              transactionCbor,
              measurement,
              fee: CML.Transaction.from_cbor_hex(transactionCbor).body().fee(),
            });
          },
        });
        expect(result.historyOutRef).toBe(
          `${current.anchor.utxo.txHash}#${current.anchor.utxo.outputIndex}`,
        );
        expect(result.openingCbor).toBe(openingCbor);
        expect(result.historyOutRef).not.toBe(
          `${old.anchor.utxo.txHash}#${old.anchor.utxo.outputIndex}`,
        );
        const [txHash, index] = result.nextThreadOutRef.split("#");
        const thread = (
          await h.lucid.utxosByOutRef([
            { txHash: txHash!, outputIndex: Number(index) },
          ])
        )[0]!;
        await submitProductionClassification(h, p, thread, captured);
      });
    for (const presence of [false, true])
      it(`captures ${presence ? "Order facts" : "arbitrary-ID absence"} after targeted churn and continues without a live history pointer`, async () => {
        SDK.requireEventHistoryCaptureProtection(protectionDurationMs, timing);
        const h = await setupHistoryPair({
          blueprint,
          records,
          protectionDurationMs,
        });
        if (presence) await promoteHistoryPair(h);
        const nonce = h.eventNonces[familyIndex(kind)]!;
        const id = presence
          ? {
              transactionId: nonce.txHash,
              outputIndex: BigInt(nonce.outputIndex),
            }
          : { transactionId: "ea".repeat(32), outputIndex: 987_654n };
        // The header predates these events. Its last ten minutes remain; later
        // admission cannot manufacture an eligible event for this accused interval.
        const headerEnd =
          BigInt(h.emulator.now()) - SDK.MATURITY_DURATION_MS + 700_000n;
        const p = await setupProof(h, kind, id, headerEnd);
        const mergeDeadline = headerEnd + SDK.MATURITY_DURATION_MS;
        let conflicts = 0;
        const successful = await SDK.captureEventHistoryWithRetry<{
          thread: UTxO;
          captured:
            | ReturnType<typeof SDK.captureEventHistoryWitness>
            | undefined;
          witness: SDK.EventHistoryWitness;
        }>({
          now: () => BigInt(h.emulator.now()),
          headerEnd,
          mergeDeadline,
          timing,
          maxAttempts: 4,
          fetch: () => fetchWitness(h, kind, id),
          submit: async (w, window, attempt) => {
            const built = await buildCapture(h, p, w, window);
            if (attempt <= 3) {
              // Deliberately violate the declared inclusion bound on attempts 2/3:
              // allow protection to expire, then submit the attack first.
              waitForMutation(h, w);
              const key = await nextChurnKey(w, id);
              await h.submit(
                `${kind}-targeted-pointer-churn-${attempt}`,
                await buildChurn(h, kind, w, key, [p.fee]),
              );
              expect(await h.lucid.utxosByOutRef([w.anchor.utxo])).toHaveLength(
                0,
              );
              const signed = await built.tx.sign.withWallet().complete();
              await expect(signed.submit()).rejects.toThrow(
                attempt === 1
                  ? /does not exist or was already spent/
                  : /Upper bound .* not in slot range/,
              );
              expect(await h.lucid.utxosByOutRef([p.fee])).toHaveLength(1);
              expect(await h.lucid.utxosByOutRef([p.thread])).toHaveLength(1);
              records.push({
                label: `${kind}-stale-capture-${attempt}`,
                witness: w.anchor.utxo,
                window,
                at: h.emulator.now(),
                transactionCbor: signed.toCBOR(),
              });
              conflicts++;
              return { kind: "ReferenceConflict" };
            }
            expect(window.protected).toBe(true);
            // A second immediate mutation cannot take the refreshed reference away.
            await expect(
              buildChurn(h, kind, w, await nextChurnKey(w, id), [p.fee]),
            ).rejects.toThrow();
            const hash = await h.submit(
              `${kind}-capture-protected-${presence ? "present" : "absent"}`,
              built.tx,
            );
            const thread = (
              await h.lucid.utxosByOutRef([{ txHash: hash, outputIndex: 0 }])
            )[0]!;
            expect(thread.datum).toBe(built.datum);
            return {
              kind: "Captured",
              value: { thread, captured: built.captured, witness: w },
            };
          },
        });
        expect(conflicts).toBe(3);
        // Churn after capture is no longer a dependency for content classification.
        waitForMutation(h, successful.witness);
        await h.submit(
          `${kind}-churn-after-capture`,
          await buildChurn(
            h,
            kind,
            successful.witness,
            await nextChurnKey(successful.witness, id),
            [],
          ),
        );
        expect(
          await h.lucid.utxosByOutRef([successful.witness.anchor.utxo]),
        ).toHaveLength(0);
        if (presence)
          await expect(
            continueCaptured(
              h,
              p,
              successful.thread,
              successful.captured,
              true,
            ),
          ).rejects.toThrow();
        const classified = await submitProductionClassification(
          h,
          p,
          successful.thread,
          successful.captured,
        );
        expect(classified.awaitedConfirmation).toBe(true);
        expect(BigInt(h.emulator.now())).toBeLessThan(mergeDeadline);
      });
  });

for (const kind of ["Deposit", "Withdrawal"] as const) {
  it(`${kind}: an honest eligible event remains unchallengeable after pointer mutation`, async () => {
    const h = await setupHistoryPair({
      blueprint,
      records,
      protectionDurationMs,
    });
    const payloads = await promoteHistoryPair(h);
    const payload = payloads[familyIndex(kind)]!;
    const nonce = h.eventNonces[familyIndex(kind)]!;
    const id = {
      transactionId: nonce.txHash,
      outputIndex: BigInt(nonce.outputIndex),
    };
    const w = await fetchWitness(h, kind, id);
    expect(w.kind).toBe("Present");
    if (w.kind !== "Present") throw new Error("Missing admitted Order");
    const facts = SDK.captureEventHistoryWitness(
      w,
      deployment(h, kind).policyId,
      kind,
    );
    const committedHash =
      "DepositPayload" in payload
        ? Effect.runSync(
            SDK.depositInfoCommitment(payload.DepositPayload.event.info),
          )
        : Effect.runSync(
            SDK.withdrawalContentCommitment({
              ...payload.WithdrawalPayload.event.info,
              validity: "NonExistentWithdrawalUtxo",
            }),
          );
    const p = await setupProof(
      h,
      kind,
      id,
      facts.commitment.inclusion_time,
      committedHash,
    );
    const window = SDK.eventHistoryCaptureWindow({
      now: BigInt(h.emulator.now()),
      headerEnd: p.headerEnd,
      mergeDeadline: p.headerEnd + SDK.MATURITY_DURATION_MS,
      protectedUntil: w.anchor.node.protected_until,
      timing,
    });
    const built = await buildCapture(h, p, w, window);
    const hash = await h.submit(`${kind}-capture-honest-eligible`, built.tx);
    const thread = (
      await h.lucid.utxosByOutRef([{ txHash: hash, outputIndex: 0 }])
    )[0]!;
    waitForMutation(h, w);
    await h.submit(
      `${kind}-honest-pointer-continuation`,
      await buildChurn(h, kind, w, await nextChurnKey(w, id), []),
    );
    await expect(
      continueCaptured(h, p, thread, built.captured),
    ).rejects.toThrow();
    await expect(
      submitProductionClassification(h, p, thread, built.captured),
    ).rejects.toThrow(/matches/);
    expect(await h.lucid.utxosByOutRef([thread])).toHaveLength(1);
    const refreshed = await fetchWitness(h, kind, id);
    if (refreshed.kind !== "Present")
      throw new Error("Pointer continuation lost the Order");
    expect(
      SDK.captureEventHistoryWitness(
        refreshed,
        deployment(h, kind).policyId,
        kind,
      ),
    ).toEqual(facts);
  });

  it(`${kind}: capture and classification both refuse merge-deadline overlap`, async () => {
    const h = await setupHistoryPair({
      blueprint,
      records,
      protectionDurationMs,
    });
    const id = { transactionId: "fe".repeat(32), outputIndex: 123n };
    const headerEnd =
      BigInt(h.emulator.now()) - SDK.MATURITY_DURATION_MS + 250_000n;
    const p = await setupProof(h, kind, id, headerEnd);
    const mergeDeadline = headerEnd + SDK.MATURITY_DURATION_MS;
    const w = await fetchWitness(h, kind, id);
    await expect(
      buildCapture(h, p, w, {
        validFrom: BigInt(h.emulator.now()) - 60_000n,
        validTo: mergeDeadline + 1_000n,
        protected: false,
      }),
    ).rejects.toThrow();
    const window = SDK.eventHistoryCaptureWindow({
      now: BigInt(h.emulator.now()),
      headerEnd,
      mergeDeadline,
      protectedUntil: w.anchor.node.protected_until,
      timing,
    });
    const built = await buildCapture(h, p, w, window);
    const hash = await h.submit(`${kind}-capture-before-deadline`, built.tx);
    const thread = (
      await h.lucid.utxosByOutRef([{ txHash: hash, outputIndex: 0 }])
    )[0]!;
    h.emulator.awaitSlot(
      Number((mergeDeadline - BigInt(h.emulator.now()) + 999n) / 1000n),
    );
    await expect(continueCaptured(h, p, thread, undefined)).rejects.toThrow();
    await expect(
      submitProductionClassification(h, p, thread, undefined),
    ).rejects.toThrow(/window/);
    expect(await h.lucid.utxosByOutRef([thread])).toHaveLength(1);
  });
}
