import "node:crypto";
import "node:fs/promises";
import "node:path";
import "node:url";
import "@al-ft/midgard-core";
import "@al-ft/midgard-core/codec/forced";
import "@al-ft/midgard-core/da-payload-envelope";
import "@al-ft/midgard-sdk";
import "@lucid-evolution/lucid";
import "effect";
import "vitest";
import "../src/index.js";
import "../src/proof-fit/van-rossem-fit-ledger.js";
import "./support/emulator/blueprints.js";
import "./support/emulator/family-history.js";
import "./support/emulator/measurement.js";
import "./support/emulator/native-tx.js";
import "./support/legacy-submit-emulator.js";
import "./support/pinned-fit-ledger.js";
import "./support/submit-init-emulator-fixtures.js";
import "./support/submit-init-emulator-shared.js";
import "./support/transition-trace-yields.js";
import "./submit-init-emulator-transition-trace-subvariants.setup-challenge.js";
import "./submit-init-emulator-transition-trace-subvariants.remove-and-assert-permanent-proof.js";

import { createHash } from "node:crypto";
import { mkdir, readFile, writeFile } from "node:fs/promises";
import { join } from "node:path";
import { fileURLToPath } from "node:url";

import {
  computeMidgardForcedTxProofCommitment,
  computeMidgardNativeTxId,
} from "@al-ft/midgard-core";
import { outRefLabel } from "@al-ft/midgard-core";
import { deriveMidgardForcedTxProofSource } from "@al-ft/midgard-core/codec/forced";
import { materializeMidgardForcedTxFromCanonical } from "@al-ft/midgard-core/codec/forced";
import * as SDK from "@al-ft/midgard-sdk";
import { CML, Data, Emulator, toUnit } from "@lucid-evolution/lucid";
import { afterAll, describe, expect, it, vi } from "vitest";

import {
  buildCountedRoot,
  buildOmittedDueL1EventFault,
  buildOutOfWindowSourceEventFault,
  buildTransitionFaultProof,
  resolveTransitionTraceDeploymentContracts,
  submitTransitionTraceProof,
  transitionTraceFinalIndex,
} from "../src/index.js";
import {
  buildVanRossemFitLedger,
  type VanRossemFitMeasurement,
} from "../src/proof-fit/van-rossem-fit-ledger.js";
import {
  alignedHeaderStart,
  removeAndAssertPermanentProof,
} from "./submit-init-emulator-transition-trace-subvariants.remove-and-assert-permanent-proof.js";
import {
  firstThreadUtxo,
  historyRecords,
  makeHarness,
  reconstruct,
  setupChallenge,
  setupWithdrawalChallenge,
  withdrawalIdFor,
  withdrawalInfo,
} from "./submit-init-emulator-transition-trace-subvariants.setup-challenge.js";
import { realBlueprintPath } from "./support/emulator/blueprints.js";
import { FAMILY_HISTORY_HEADER_LEAD_MS } from "./support/emulator/family-history.js";
import { measureCompleteSignedTransaction } from "./support/emulator/measurement.js";
import { makeNativeTx } from "./support/emulator/native-tx.js";
import {
  readBlueprintIdentity,
  writeOrVerifyPinnedFitLedger,
} from "./support/pinned-fit-ledger.js";
import {
  expectSingleUtxoWithUnit,
  funderPaymentKeyHash,
  ledgerOrderedIndex,
  makeHeader,
  network,
  transitionTraceDaEntry,
  transitionTraceOutRef,
} from "./support/submit-init-emulator-shared.js";

afterAll(async () => {
  const directory = process.env.MIDGARD_EVENT_HISTORY_EVIDENCE_DIR;
  if (directory === undefined) return;
  await mkdir(directory, { recursive: true });
  await writeFile(
    join(directory, "transition-subvariant-history.json"),
    JSON.stringify(
      {
        scope:
          "Applied semantic subvariant lifecycle, actual withdrawal history admission/removal and forced timing; fixture catalogue governance",
        blueprintSha256: createHash("sha256")
          .update(await readFile(realBlueprintPath))
          .digest("hex"),
        records: historyRecords,
      },
      (_key, value: unknown) =>
        typeof value === "bigint" ? value.toString() : value,
      2,
    ) + "\n",
  );
});

const forcedWindowMeasurements: VanRossemFitMeasurement[] = [];

const forcedWindowCases = new Set<boolean>();

afterAll(async () => {
  expect(forcedWindowCases.size).toBe(2);
  const ledger = buildVanRossemFitLedger({
    category: "transitionTrace",
    ...(await readBlueprintIdentity(realBlueprintPath)),
    measurements: forcedWindowMeasurements,
  });
  const ledgerPath = fileURLToPath(
    new URL(
      "../../../docs/fault-proofs/size-plans/transition-trace-forced-window-fit-ledger.json",
      import.meta.url,
    ),
  );
  // Fresh execution budgets may differ; identity and row coverage must not.
  await writeOrVerifyPinnedFitLedger(ledgerPath, ledger);
});

describe("transition-trace omitted/out-of-window/count subvariant lifecycle", () => {
  it("routes an omitted due withdrawal to final 6 and removes the block", async () => {
    const { harness, history, publications, transitionTraceReferenceScripts } =
      await makeHarness();
    const header = makeHeader(
      await funderPaymentKeyHash(harness.funderLucid),
      await alignedHeaderStart(harness, FAMILY_HISTORY_HEADER_LEAD_MS),
    );
    const { lifecycle, event, withdrawalId } = await setupWithdrawalChallenge({
      harness,
      history,
      publications,
      transitionTraceReferenceScripts,
      header,
      inclusionTime: header.endTime,
    });

    const reconstruction = await reconstruct({ header });
    const proof = buildTransitionFaultProof({
      reconstruction,
      fault: await buildOmittedDueL1EventFault({
        reconstruction,
        evidence: {
          kind: "withdrawal",
          withdrawalId,
        },
      }),
    });
    expect(transitionTraceFinalIndex(proof)).toBe(6);
    const proofResult = await submitTransitionTraceProof({
      lucid: harness.proverLucid,
      blueprint: harness.realBlueprint,
      deploymentInfo: lifecycle.deploymentInfo,
      network,
      signer: harness.proverSigner,
      threadOutRef: outRefLabel(
        await firstThreadUtxo({ harness, init: lifecycle.init }),
      ),
      proof,
      additionalReferenceInputs: [event.utxo],
      witnessReferenceScripts: harness.witnessReferenceScripts,
      awaitConfirmation: true,
    });
    await removeAndAssertPermanentProof({
      harness,
      setup: lifecycle.setup,
      deploymentInfo: lifecycle.deploymentInfo,
      proofResult,
    });
  }, 180_000);

  it.each([false, true])(
    "authenticates a late rejected order from immutable submitted bytes (wrong reason %s)",
    async (wrongReason) => {
      const original = Emulator.prototype.submitTx;
      let index = 0;
      const capture = vi
        .spyOn(Emulator.prototype, "submitTx")
        .mockImplementation(async function (this: Emulator, cbor) {
          const result = await original.call(this, cbor);
          const m = measureCompleteSignedTransaction(cbor);
          const outputs = CML.Transaction.from_cbor_hex(cbor).body().outputs();
          forcedWindowMeasurements.push({
            name: `${wrongReason ? "wrong-reason" : "rejected-source"}/${index++}`,
            kind: Array.from({ length: outputs.len() }, (_, i) =>
              outputs.get(i),
            ).some((output) => output.script_ref() !== undefined)
              ? "publication"
              : "lifecycle",
            maximumShape: "immutable-submission-rejected-verdict",
            signedBytes: m.completeSignedBytes,
            memoryUnits: m.executionMemory,
            cpuUnits: m.executionSteps,
          });
          return result;
        });
      try {
        const { harness, publications, transitionTraceReferenceScripts } =
          await makeHarness();
        const operator = await funderPaymentKeyHash(harness.funderLucid);
        const startTime = await alignedHeaderStart(harness);
        const id = transitionTraceOutRef("91");
        const submitted = makeNativeTx({
          spendInputCbors: [],
          outputCbors: [],
          fee: 0n,
        });
        const rawSource = deriveMidgardForcedTxProofSource(
          materializeMidgardForcedTxFromCanonical(submitted),
        );
        const rejectedSource = deriveMidgardForcedTxProofSource(
          materializeMidgardForcedTxFromCanonical(submitted),
        );
        const sourceData = (
          source: typeof rawSource,
        ): SDK.ForcedTxProofSource => ({
          compact_cbor: source.compactCbor.toString("hex"),
          witness_set_compact_cbor:
            source.witnessSetCompactCbor.toString("hex"),
          field_preimage_lengths_cbor:
            source.fieldPreimageLengthsCbor.toString("hex"),
        });
        expect(sourceData(rawSource).compact_cbor).toBe(
          sourceData(rejectedSource).compact_cbor,
        );
        const committed: SDK.ForcedInclusionTxV1 = {
          tx_id: computeMidgardNativeTxId(submitted).toString("hex"),
          submitted_source: sourceData(rejectedSource),
          verdict: {
            ForcedTxInvalid: {
              reason: { PlutusExecutionFailed: { execution_index: 0n } },
            },
          },
        };
        const root = await buildCountedRoot(
          SDK.ROOT_DOMAINS.forcedTransactionsV1,
          [
            {
              key: Buffer.from(Data.to(id, SDK.OutputReference), "hex"),
              value: Buffer.from(
                Data.to(committed, SDK.ForcedInclusionTxV1),
                "hex",
              ),
            },
          ],
        );
        const header: SDK.Header = {
          ...makeHeader(operator, startTime),
          forcedTransactionsRoot: root.root,
          forcedTransactionCount: 1n,
          totalEventCount: 1n,
          transitionStepCount: 1n,
          validationTraceCount: 1n,
          transitionTraceRoot: "ab".repeat(32),
          eventToStepRoot: "bc".repeat(32),
          validationTracesRoot: "cd".repeat(32),
        };
        const lifecycle = await setupChallenge({
          harness,
          publications,
          transitionTraceReferenceScripts,
          header,
        });
        const assetName = "98",
          unit = toUnit(harness.contracts.txOrder.policyId, assetName);
        const datum: SDK.TxOrderDatum = {
          event: {
            id,
            tx: {
              tx_id: committed.tx_id,
              submitted_source: sourceData(rawSource),
              transaction_commitment: Buffer.from(
                computeMidgardForcedTxProofCommitment(rawSource),
              ).toString("hex"),
            },
          },
          inclusion_time: header.endTime + 1n,
          witness: "76".repeat(28),
          refund_address: {
            paymentCredential: { PublicKeyCredential: ["77".repeat(28)] },
            stakeCredential: null,
          },
          refund_datum: "NoDatum",
        };
        const signed = await (
          await harness.funderLucid
            .newTx()
            .mintAssets({ [unit]: 1n }, Data.void())
            .pay.ToContract(
              harness.contracts.txOrder.spendingScriptAddress,
              { kind: "inline", value: Data.to(datum, SDK.TxOrderDatum) },
              { lovelace: 5000000n, [unit]: 1n },
            )
            .attach.MintingPolicy(harness.contracts.txOrder.mintingScript)
            .complete()
        ).sign
          .withWallet()
          .complete();
        await harness.funderLucid.awaitTx(await signed.submit());
        const event = await expectSingleUtxoWithUnit(
          harness.funderLucid,
          harness.contracts.txOrder.spendingScriptAddress,
          unit,
        );
        const refs = [
          lifecycle.setup.hubOracle,
          transitionTraceReferenceScripts.fraudProofTransitionTraceL1Event!
            .utxo,
          harness.witnessReferenceScripts.computationThreadMint!,
          harness.witnessReferenceScripts.fraudProofMint!,
          event,
        ];
        const proof: SDK.TransitionFaultProof = {
          challenged_header_hash: lifecycle.setup.headerHash,
          header,
          fault: {
            OutOfWindowSourceEvent: {
              witness: {
                OutOfWindowForcedTransaction: {
                  event_ref_input_index: ledgerOrderedIndex(
                    refs,
                    event,
                    "late rejected order",
                  ),
                  event_asset_name: assetName,
                  validity_override: wrongReason
                    ? {
                        ForcedTxInvalid: {
                          reason: {
                            PlutusExecutionFailed: { execution_index: 1n },
                          },
                        },
                      }
                    : committed.verdict,
                  source_membership: {
                    domain: root.domain,
                    root: root.root,
                    phas_root: root.phasRoot,
                    count: root.count,
                    key: id,
                    value: committed,
                    proof: [],
                  },
                },
              },
            },
          },
        };
        const thread = await firstThreadUtxo({ harness, init: lifecycle.init });
        const run = () =>
          submitTransitionTraceProof({
            lucid: harness.proverLucid,
            blueprint: harness.realBlueprint,
            deploymentInfo: lifecycle.deploymentInfo,
            network,
            signer: harness.proverSigner,
            threadOutRef: outRefLabel(thread),
            proof,
            additionalReferenceInputs: [event],
            witnessReferenceScripts: harness.witnessReferenceScripts,
            awaitConfirmation: true,
          });
        if (wrongReason) await expect(run()).rejects.toThrow();
        else
          await removeAndAssertPermanentProof({
            harness,
            setup: lifecycle.setup,
            deploymentInfo: lifecycle.deploymentInfo,
            proofResult: await run(),
          });
        forcedWindowCases.add(wrongReason);
      } finally {
        capture.mockRestore();
      }
    },
    180000,
  );

  it("routes an out-of-window withdrawal to final 6 and removes the block", async () => {
    const { harness, history, publications, transitionTraceReferenceScripts } =
      await makeHarness();
    const operator = await funderPaymentKeyHash(harness.funderLucid);
    const startTime = await alignedHeaderStart(
      harness,
      FAMILY_HISTORY_HEADER_LEAD_MS,
    );
    const withdrawalId = withdrawalIdFor(history);
    const eventKey: SDK.EventKey = {
      WithdrawalEventKey: { withdrawal_id: withdrawalId },
    };
    const committedInfo = withdrawalInfo("IncorrectWithdrawalSignature");
    const step: SDK.TransitionStep = {
      schema_version: 1n,
      step_index: 0n,
      event_key: eventKey,
      phase: "Withdrawal",
      pre_utxos_root: SDK.EMPTY_MERKLE_TREE_ROOT,
      post_utxos_root: SDK.EMPTY_MERKLE_TREE_ROOT,
    };
    const mapping: SDK.EventToStepValue = {
      step_index: 0n,
      phase: "Withdrawal",
    };
    const withdrawals: SDK.DaPayloadEntry[] = [
      [
        Data.to(withdrawalId, SDK.OutputReference),
        SDK.committedWithdrawalValueBytes(committedInfo),
      ],
    ];
    const transitionTrace = [
      transitionTraceDaEntry({
        key: 0n,
        keySchema: Data.Integer() as never,
        value: step,
        valueSchema: SDK.TransitionStepSchema,
      }),
    ];
    const eventToStep = [
      transitionTraceDaEntry({
        key: eventKey,
        keySchema: SDK.EventKeySchema,
        value: mapping,
        valueSchema: SDK.EventToStepValueSchema,
      }),
    ];
    const [withdrawalsRoot, traceRoot, mappingRoot] = await Promise.all([
      buildCountedRoot(
        SDK.ROOT_DOMAINS.withdrawals,
        withdrawals.map(([key, value]) => ({
          key: Buffer.from(key, "hex"),
          value: Buffer.from(value, "hex"),
        })),
      ),
      buildCountedRoot(
        SDK.ROOT_DOMAINS.transitionTrace,
        transitionTrace.map(([key, value]) => ({
          key: Buffer.from(key, "hex"),
          value: Buffer.from(value, "hex"),
        })),
      ),
      buildCountedRoot(
        SDK.ROOT_DOMAINS.eventToStep,
        eventToStep.map(([key, value]) => ({
          key: Buffer.from(key, "hex"),
          value: Buffer.from(value, "hex"),
        })),
      ),
    ]);
    const header: SDK.Header = {
      ...makeHeader(operator, startTime),
      withdrawalsRoot: withdrawalsRoot.root,
      transitionTraceRoot: traceRoot.root,
      eventToStepRoot: mappingRoot.root,
      withdrawalCount: 1n,
      totalEventCount: 1n,
      transitionStepCount: 1n,
    };
    const { lifecycle, event } = await setupWithdrawalChallenge({
      harness,
      history,
      publications,
      transitionTraceReferenceScripts,
      header,
      inclusionTime: header.endTime + 1000n,
    });

    const reconstruction = await reconstruct({
      header,
      withdrawals,
      transitionTrace,
      eventToStep,
    });
    const proof = buildTransitionFaultProof({
      reconstruction,
      fault: await buildOutOfWindowSourceEventFault({
        reconstruction,
        evidence: {
          kind: "withdrawal",
          withdrawalId,
        },
      }),
    });
    expect(transitionTraceFinalIndex(proof)).toBe(6);
    const proofResult = await submitTransitionTraceProof({
      lucid: harness.proverLucid,
      blueprint: harness.realBlueprint,
      deploymentInfo: lifecycle.deploymentInfo,
      network,
      signer: harness.proverSigner,
      threadOutRef: outRefLabel(
        await firstThreadUtxo({ harness, init: lifecycle.init }),
      ),
      proof,
      additionalReferenceInputs: [event.utxo],
      witnessReferenceScripts: harness.witnessReferenceScripts,
      awaitConfirmation: true,
    });
    await removeAndAssertPermanentProof({
      harness,
      setup: lifecycle.setup,
      deploymentInfo: lifecycle.deploymentInfo,
      proofResult,
    });
  }, 180_000);

  it("routes a transition-step count mismatch to final 0 and removes the block", async () => {
    const { harness, publications, transitionTraceReferenceScripts } =
      await makeHarness({
        // The production state queue rejects this malformed header before a
        // fault proof can observe it. Bypass admission only; the registered
        // transition-trace chain and removal transaction remain real.
        alwaysStateQueue: true,
      });
    const header: SDK.Header = {
      ...makeHeader(
        await funderPaymentKeyHash(harness.funderLucid),
        await alignedHeaderStart(harness),
      ),
      transitionStepCount: 1n,
    };
    const lifecycle = await setupChallenge({
      harness,
      publications,
      transitionTraceReferenceScripts,
      header,
    });
    const proof = SDK.makeTransitionFaultProof({
      challengedHeaderHash: lifecycle.setup.headerHash,
      header,
      fault: SDK.countFault("HeaderTransitionStepCountMismatch"),
    });
    expect(transitionTraceFinalIndex(proof)).toBe(0);
    const proofResult = await submitTransitionTraceProof({
      lucid: harness.proverLucid,
      blueprint: harness.realBlueprint,
      deploymentInfo: lifecycle.deploymentInfo,
      network,
      signer: harness.proverSigner,
      threadOutRef: outRefLabel(
        await firstThreadUtxo({ harness, init: lifecycle.init }),
      ),
      proof,
      witnessReferenceScripts: harness.witnessReferenceScripts,
      awaitConfirmation: true,
    });
    await removeAndAssertPermanentProof({
      harness,
      setup: lifecycle.setup,
      deploymentInfo: lifecycle.deploymentInfo,
      proofResult,
    });
  }, 180_000);

  it("rejects an honest late withdrawal accused as omitted at final 6", async () => {
    const { harness, history, publications, transitionTraceReferenceScripts } =
      await makeHarness();
    const header = makeHeader(
      await funderPaymentKeyHash(harness.funderLucid),
      await alignedHeaderStart(harness, FAMILY_HISTORY_HEADER_LEAD_MS),
    );
    const { lifecycle, event, withdrawalId } = await setupWithdrawalChallenge({
      harness,
      history,
      publications,
      transitionTraceReferenceScripts,
      header,
      inclusionTime: header.endTime + 1000n,
    });

    const reconstruction = await reconstruct({ header });
    const proof = buildTransitionFaultProof({
      reconstruction,
      fault: await buildOmittedDueL1EventFault({
        reconstruction,
        evidence: {
          kind: "withdrawal",
          withdrawalId,
        },
      }),
    });
    expect(transitionTraceFinalIndex(proof)).toBe(6);
    const resolved = await resolveTransitionTraceDeploymentContracts({
      blueprint: harness.realBlueprint,
      deploymentInfo: lifecycle.deploymentInfo,
      network,
      requireFraudProofSpend: true,
    });
    await expect(
      submitTransitionTraceProof({
        lucid: harness.proverLucid,
        blueprint: harness.realBlueprint,
        deploymentInfo: lifecycle.deploymentInfo,
        network,
        signer: harness.proverSigner,
        threadOutRef: outRefLabel(
          await firstThreadUtxo({ harness, init: lifecycle.init }),
        ),
        proof,
        additionalReferenceInputs: [event.utxo],
        witnessReferenceScripts: harness.witnessReferenceScripts,
        awaitConfirmation: true,
      }),
    ).rejects.toThrow();
    await expect(
      harness.proverLucid.utxosAtWithUnit(
        resolved.contracts.transitionTrace.finals[6]!.spendingScriptAddress,
        lifecycle.init.computationThreadUnit,
      ),
    ).resolves.toHaveLength(1);
    await expect(
      harness.proverLucid.utxosAtWithUnit(
        resolved.contracts.fraudProof.spendingScriptAddress,
        toUnit(
          resolved.contracts.fraudProof.policyId,
          lifecycle.init.computationThreadAssetName,
        ),
      ),
    ).resolves.toHaveLength(0);
  }, 180_000);
});
