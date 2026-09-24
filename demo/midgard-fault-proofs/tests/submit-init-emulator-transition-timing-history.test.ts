import { createHash } from "node:crypto";
import { mkdirSync, readFileSync, writeFileSync } from "node:fs";
import { join } from "node:path";
import { inspect } from "node:util";

import { outRefLabel } from "@al-ft/midgard-core";
import * as SDK from "@al-ft/midgard-sdk";
import {
  CML,
  Data,
  type UTxO,
  validatorToRewardAddress,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { afterAll, describe, expect, it, vi } from "vitest";

import { submitInit } from "../src/submit-init.js";
import { buildCountedRoot } from "../src/transition-trace/phas.js";
import {
  submitTransitionTraceFinal,
  submitTransitionTraceRoute,
} from "../src/transition-trace/submit.js";
import { prepareFamilyHistory } from "./support/emulator/family-history.js";
import { makeHeader } from "./support/emulator/header-fixtures.js";
import { insertHistoryFillerAfter } from "./support/emulator/history-pair.js";
import { measureCompleteSignedTransaction } from "./support/emulator/measurement.js";
import { EMULATOR_PROTOCOL_PARAMETERS } from "./support/emulator/protocol-parameters.js";
import {
  alignUnixTimeToEmulatorSlotBoundary,
  buildRemovalDeploymentInfo,
  funderPaymentKeyHash,
  makeFaultProofEmulatorHarness,
  network,
  publishRemovalReferenceScripts,
  submitSetupTx,
} from "./support/submit-init-emulator-shared.js";
import { publishTransitionTraceYields } from "./support/transition-trace-yields.js";

const records: unknown[] = [];
afterAll(() => {
  const directory = process.env.MIDGARD_EVENT_HISTORY_EVIDENCE_DIR;
  if (directory === undefined) return;
  mkdirSync(directory, { recursive: true });
  writeFileSync(
    join(directory, "transition-timing-history.json"),
    JSON.stringify(
      {
        scope:
          "Applied timed history final, actual admission/external publication and pointer churn; fixture catalogue governance",
        blueprintSha256: createHash("sha256")
          .update(
            readFileSync(
              new URL("../../../onchain/aiken/plutus.json", import.meta.url),
            ),
          )
          .digest("hex"),
        records,
      },
      (_key, value: unknown) =>
        typeof value === "bigint" ? value.toString() : value,
      2,
    ) + "\n",
  );
});

const cases = [
  ...(["Deposit", "Withdrawal"] as const).flatMap((kind) =>
    (["inline", "external"] as const).flatMap((mode) =>
      (["omitted", "outside", "eligible"] as const).map((scenario) => ({
        kind,
        mode,
        scenario,
        payloadBytes: mode === "external" ? 2000 : 0,
      })),
    ),
  ),
  ...(["Deposit", "Withdrawal"] as const).map((kind) => ({
    kind,
    mode: "external" as const,
    scenario: "outside" as const,
    payloadBytes: 12000,
  })),
];

describe("applied timed history final", () => {
  it.each(cases)(
    "$kind $mode $scenario $payloadBytes-byte datum survives pointer churn with exact terminal marking",
    async ({ kind, mode, scenario, payloadBytes }) => {
      const bounds = {
        inlineLimitBytes: 512n,
        maxPayloadBytes: payloadBytes === 12000 ? 14000n : 5000n,
        maxPayloadNodes: 512n,
      };
      const h = await makeFaultProofEmulatorHarness({
        contractOptions: {
          realTransitionTrace: true,
          alwaysFraudProofCatalogue: true,
          eventHistoryBounds: bounds,
        },
      });
      const submit = h.emulator.submitTx.bind(h.emulator);
      h.emulator.submitTx = async (cbor) => {
        const measurement = measureCompleteSignedTransaction(cbor);
        expect(measurement.completeSignedBytes).toBeLessThanOrEqual(
          EMULATOR_PROTOCOL_PARAMETERS.maxTxSize,
        );
        expect(measurement.executionMemory).toBeLessThanOrEqual(
          EMULATOR_PROTOCOL_PARAMETERS.maxTxExMem,
        );
        expect(measurement.executionSteps).toBeLessThanOrEqual(
          EMULATOR_PROTOCOL_PARAMETERS.maxTxExSteps,
        );
        const txHash = await submit(cbor);
        records.push({
          kind,
          mode,
          scenario,
          payloadBytes,
          txHash,
          transactionCbor: cbor,
          measurement,
          fee: CML.Transaction.from_cbor_hex(cbor).body().fee(),
        });
        return txHash;
      };
      const history = await prepareFamilyHistory(h, records, bounds);
      const contracts = history.contracts;
      const yields = await publishTransitionTraceYields(
        h.proverLucid,
        contracts,
      );
      const removal = await publishRemovalReferenceScripts({
        lucid: h.proverLucid,
        contracts,
      });
      const deploymentInfo = buildRemovalDeploymentInfo(
        contracts,
        h.catalogue,
        {
          removalReferenceScripts: removal.published,
          fraudProofReferenceScripts: {
            ...h.faultProofReferenceScripts,
            ...yields,
          },
        },
      );
      records.push({
        kind,
        mode,
        scenario,
        payloadBytes,
        deploymentInfo,
        appliedTransition: contracts.fraudProofContracts.transitionTrace,
      });
      const nonce = history.nonce(kind);
      const id: SDK.OutputReference = {
        transactionId: nonce.txHash,
        outputIndex: BigInt(nonce.outputIndex),
      };
      const depositInfo: SDK.DepositInfo = {
        l2_address: {
          paymentCredential: { PublicKeyCredential: ["aa".repeat(28)] },
          stakeCredential: null,
        },
        l2_network_id: 0n,
        l2_datum: mode === "external" ? "ab".repeat(payloadBytes) : null,
      };
      const withdrawalInfo: SDK.WithdrawalInfo = {
        body: {
          l2_outref: { transactionId: "aa".repeat(32), outputIndex: 0n },
          l2_owner: "bb".repeat(28),
          l2_value: new Map([
            [
              "",
              new Map([
                ["", payloadBytes === 12000 ? 100_000_000n : 50_000_000n],
              ]),
            ],
          ]),
          l1_address: depositInfo.l2_address,
          l1_datum:
            mode === "external"
              ? { InlineDatum: { data: "ab".repeat(payloadBytes) } }
              : "NoDatum",
        },
        signature: ["aa".repeat(32), "bb".repeat(64)],
        validity: "WithdrawalIsValid",
      };
      const payload: SDK.EventHistoryPayload =
        kind === "Deposit"
          ? { DepositPayload: { event: { id, info: depositInfo } } }
          : {
              WithdrawalPayload: {
                event: { id, info: withdrawalInfo },
                refund_address: depositInfo.l2_address,
                refund_datum: "NoDatum",
              },
            };
      // A withdrawal verdict is authenticated in the L2 leaf, independently of its submitted body.
      const committedWithdrawal: SDK.WithdrawalInfo = {
        ...withdrawalInfo,
        validity: "IncorrectWithdrawalSignature",
      };
      const domain =
        kind === "Deposit"
          ? SDK.ROOT_DOMAINS.deposits
          : SDK.ROOT_DOMAINS.withdrawals;
      const source = await buildCountedRoot(
        domain,
        scenario === "omitted"
          ? []
          : [
              {
                key: Buffer.from(Data.to(id, SDK.OutputReference), "hex"),
                value: Buffer.from(
                  kind === "Deposit"
                    ? Data.to(depositInfo, SDK.DepositInfo)
                    : SDK.committedWithdrawalValueBytes(committedWithdrawal),
                  "hex",
                ),
              },
            ],
      );
      const now =
        alignUnixTimeToEmulatorSlotBoundary(
          h.funderLucid,
          h.emulator.now() + 240_000,
        ) - 1;
      const header: SDK.Header = {
        ...makeHeader(await funderPaymentKeyHash(h.funderLucid), now),
        ...(kind === "Deposit"
          ? { depositsRoot: source.root, depositCount: source.count }
          : { withdrawalsRoot: source.root, withdrawalCount: source.count }),
        totalEventCount: source.count,
        transitionStepCount: source.count,
        transitionTraceRoot: source.root,
        eventToStepRoot: source.root,
      };
      const sourceBase = {
        domain,
        root: source.root,
        phas_root: source.phasRoot,
        count: source.count,
        key: id,
        proof: [],
      };
      const fault: SDK.TransitionFault =
        scenario === "omitted"
          ? {
              OmittedDueL1Event: {
                witness:
                  kind === "Deposit"
                    ? {
                        OmittedDueDeposit: {
                          source_non_membership: sourceBase,
                        },
                      }
                    : {
                        OmittedDueWithdrawal: {
                          source_non_membership: sourceBase,
                        },
                      },
              },
            }
          : {
              OutOfWindowSourceEvent: {
                witness:
                  kind === "Deposit"
                    ? {
                        OutOfWindowDeposit: {
                          source_membership: {
                            ...sourceBase,
                            value: depositInfo,
                          },
                        },
                      }
                    : {
                        OutOfWindowWithdrawal: {
                          source_membership: {
                            ...sourceBase,
                            value: committedWithdrawal,
                          },
                        },
                      },
              },
            };
      const proof: SDK.TransitionFaultProof = {
        header,
        challenged_header_hash: Effect.runSync(SDK.hashBlockHeader(header)),
        fault,
      };
      let admitted: Awaited<ReturnType<typeof history.admit>> | undefined;
      let hub: UTxO | undefined;
      const setup = await submitSetupTx({
        lucid: h.funderLucid,
        contracts,
        nonceUtxo: h.nonceUtxo,
        catalogue: h.catalogue,
        header,
        beforeHeaderCommit: async (oracle) => {
          hub = oracle;
          admitted = await history.admit(
            oracle,
            payload,
            scenario === "outside"
              ? { ...header, endTime: header.startTime }
              : header,
            { lovelace: payloadBytes === 12000 ? 100_000_000n : 25_000_000n },
          );
        },
      });
      if (admitted === undefined || hub === undefined)
        throw new Error("Admission did not run");
      expect(admitted.witness.retainedDataUtxo !== undefined).toBe(
        mode === "external",
      );
      const common = {
        lucid: h.proverLucid,
        blueprint: h.realBlueprint,
        deploymentInfo,
        network,
        signer: h.proverSigner,
        witnessReferenceScripts: h.witnessReferenceScripts,
      };
      const init = await submitInit({
        ...common,
        fraudCategory: "transitionTrace",
        fraudulentBlockOutRef: setup.fraudulentBlockOutRef,
      });
      if (payloadBytes === 12000) {
        const encodeRoute = Data.to;
        let changedChunk = 0;
        const corruptChunk = vi
          .spyOn(Data, "to")
          .mockImplementation((data, schema, options) => {
            const cbor = encodeRoute(data, schema, options);
            if (schema !== SDK.TransitionTraceRouteSpendRedeemer) return cbor;
            const redeemer = Data.from(
              cbor,
              SDK.TransitionTraceRouteSpendRedeemer,
            );
            if (!("Continue" in redeemer)) return cbor;
            const args = redeemer.Continue[0];
            expect(args.proof).toBeNull();
            expect(args.proof_ref_indices.length).toBeGreaterThan(1);
            args.proof_ref_indices[0] = 999n;
            changedChunk++;
            return encodeRoute(redeemer, SDK.TransitionTraceRouteSpendRedeemer);
          });
        try {
          await expect(
            submitTransitionTraceRoute({
              ...common,
              threadOutRef: `${init.txHash}#${init.firstStepOutputIndex}`,
              proof,
            }),
          ).rejects.toThrow(/failed script execution/u);
        } finally {
          corruptChunk.mockRestore();
        }
        expect(changedChunk).toBeGreaterThan(0);
        records.push({
          kind,
          mode,
          scenario,
          payloadBytes,
          rejectedMutation: "missing-proof-chunk",
        });
      }
      const route = await submitTransitionTraceRoute({
        ...common,
        threadOutRef: `${init.txHash}#${init.firstStepOutputIndex}`,
        proof,
      }).catch((error) => {
        records.push({
          kind,
          mode,
          scenario,
          payloadBytes,
          failureStage: "route",
          error: inspect(error, { depth: 10 }),
        });
        throw error;
      });
      const staleOrder = admitted.witness.anchor.utxo;
      const targetTime =
        Number(admitted.witness.anchor.node.protected_until) + 61_000;
      if (h.emulator.now() < targetTime)
        h.emulator.awaitSlot(Math.ceil((targetTime - h.emulator.now()) / 1000));
      const churn = await insertHistoryFillerAfter(
        {
          applied: history.applied,
          scripts: history.scripts,
          lucid: h.proverLucid,
          hub,
          owner: h.proverSigner.paymentKeyHash,
          funding: async () =>
            (await h.proverLucid.wallet().getUtxos()).filter(
              (u) => u.datum == null && u.scriptRef == null,
            ),
          bounds: () => {
            const lower = h.emulator.now() - 60_000;
            const validTo = h.emulator.now() + 10_000;
            return {
              lower,
              validTo,
              protectedUntil:
                BigInt(validTo - 1) + history.recipes[0]!.protectionDurationMs,
            };
          },
        },
        kind,
        admitted.witness,
        "ff".repeat(32),
        [],
      );
      await history.submit("timed-order-pointer-churn", churn);
      expect(
        await h.proverLucid.utxosByOutRef([
          { txHash: staleOrder.txHash, outputIndex: staleOrder.outputIndex },
        ]),
      ).toHaveLength(0);
      const routed = (
        await h.proverLucid.utxosByOutRef([
          {
            txHash: route.routeOutRef.split("#")[0]!,
            outputIndex: Number(route.routeOutRef.split("#")[1]),
          },
        ])
      )[0]!;
      records.push({
        kind,
        mode,
        scenario,
        payloadBytes,
        label: "routed-proof-identity",
        actualDatum: routed.datum,
        expectedDatum: Data.to(
          { fraud_prover: h.proverSigner.paymentKeyHash, data: proof },
          SDK.TransitionTraceStepDatum,
        ),
      });
      const finish = () =>
        submitTransitionTraceFinal({
          ...common,
          threadOutRef: route.routeOutRef,
          proof,
          additionalReferenceInputs: [staleOrder],
        });
      if (scenario === "eligible") {
        await expect(finish()).rejects.toThrow(/failed script execution/u);
        expect(
          await h.proverLucid.utxosAtWithUnit(
            contracts.fraudProof.spendingScriptAddress,
            contracts.fraudProof.policyId + init.computationThreadAssetName,
          ),
        ).toHaveLength(0);
        return;
      }
      const encode = Data.to;
      for (const mutation of [
        "event-reference",
        "yield-reference",
        "terminal-witness",
      ] as const) {
        let changed = 0;
        const corrupt = vi
          .spyOn(Data, "to")
          .mockImplementation((data, schema, options) => {
            const cbor = encode(data, schema, options);
            if (schema !== SDK.TransitionTraceL1EventFinalSpendRedeemer)
              return cbor;
            const redeemer = Data.from(
              cbor,
              SDK.TransitionTraceL1EventFinalSpendRedeemer,
            );
            if (!("Continue" in redeemer)) return cbor;
            const args = redeemer.Continue[0];
            if (mutation === "event-reference")
              args.event_reference.order_index = 999n;
            else if (mutation === "yield-reference")
              args.yield_ref_input_index = args.event_reference.order_index;
            else args.completed_fraud_witness = 0n;
            changed++;
            return encode(
              redeemer,
              SDK.TransitionTraceL1EventFinalSpendRedeemer,
            );
          });
        try {
          await expect(finish()).rejects.toThrow(/failed script execution/u);
        } finally {
          corrupt.mockRestore();
        }
        expect(changed).toBeGreaterThan(0);
        records.push({
          kind,
          mode,
          scenario,
          payloadBytes,
          rejectedMutation: mutation,
        });
      }
      const reward = validatorToRewardAddress(
        network,
        contracts.fraudProofContracts.transitionTrace.yields.l1Event
          .withdrawalScript,
      );
      const newTx = h.proverLucid.newTx;
      let omitted = 0;
      const omitWithdrawal = vi
        .spyOn(h.proverLucid, "newTx")
        .mockImplementation(() => {
          const tx = newTx();
          const withdraw = tx.withdraw;
          tx.withdraw = (...args) => {
            if (args[0] !== reward) return withdraw(...args);
            omitted++;
            return tx;
          };
          return tx;
        });
      try {
        await expect(finish()).rejects.toThrow(/failed script execution/u);
      } finally {
        omitWithdrawal.mockRestore();
      }
      expect(omitted).toBeGreaterThan(0);
      records.push({
        kind,
        mode,
        scenario,
        payloadBytes,
        rejectedMutation: "missing-zero-withdrawal",
      });
      const result = await finish();
      expect(result.fraudProofOutRef).toMatch(/^[0-9a-f]{64}#[0-9]+$/u);
      const queue = await h.proverLucid.utxosAt(
        contracts.stateQueue.spendingScriptAddress,
      );
      const marked = queue.filter(
        (u) =>
          u.assets[
            contracts.stateQueue.policyId +
              "4d424c43" +
              proof.challenged_header_hash
          ] === 1n,
      );
      expect(marked).toHaveLength(1);
      const view = await Effect.runPromise(
        SDK.getLinkedListNodeViewFromUTxO(marked[0]!),
      );
      const node = Effect.runSync(
        SDK.getStateQueueNodeFromStateQueueDatum(view),
      );
      expect(node.proven_fraud).toBe(init.computationThreadAssetName);
      records.push({
        kind,
        mode,
        scenario,
        payloadBytes,
        routeOutRef: route.routeOutRef,
        finalOutRef: result.fraudProofOutRef,
        replacedOrder: outRefLabel(staleOrder),
      });
    },
    120_000,
  );
});
