import { outRefLabel } from "@al-ft/midgard-core";
import * as SDK from "@al-ft/midgard-sdk";
import { Data, toUnit } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  resumeCrossBlockDuplicateEvent,
  submitCrossBlockDuplicateEventCancel,
  submitCrossBlockDuplicateEventInit,
  submitCrossBlockDuplicateEventStep01,
} from "../src/cross-block-duplicate-event/index.js";
import { submitRemoveFraudulentBlock } from "../src/remove-fraudulent-block.js";
import {
  buildCountedRoot,
  keyValuePhasProof,
} from "../src/transition-trace/phas.js";
import {
  alignUnixTimeToEmulatorSlotBoundary,
  buildRemovalDeploymentInfo,
  expectSingleUtxoWithUnit,
  funderPaymentKeyHash,
  makeFaultProofEmulatorHarness,
  makeHeader,
  network,
  publishCrossBlockDuplicateEventReferenceScripts,
  publishRemovalReferenceScripts,
  submitSetupTx,
} from "./support/submit-init-emulator-shared.js";

const DEPOSIT_KEY_CBOR =
  "d8799f58207a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a03ff";
const DEPOSIT_VALUE_CBOR =
  "d8799fd8799fd8799f581c2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2dffd87a80ff00d87a80ff";
const WITHDRAWAL_KEY_CBOR = `d8799f5820${"8b".repeat(32)}02ff`;
const WITHDRAWAL_VALUE_CBOR =
  "d8799fd8799fd8799f58207e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e01ff581c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9ca1581c4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4ba14d6d6964676172642d746f6b656e182ad8799fd8799f581c5d5d5d5d5d5d5d5d5d5d5d5d5d5d5d5d5d5d5d5d5d5d5d5d5d5d5d5dffd87a80ffd87980ff9f5820adadadadadadadadadadadadadadadadadadadadadadadadadadadadadadadad5840bebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebeffd87980ff";

const FORCED_KEY_CBOR = `d8799f5820${"9d".repeat(32)}03ff`;
const FORCED_VALUE_CBOR = Data.to(
  {
    tx_id: "ae".repeat(32),
    source: {
      compact_cbor: "80",
      witness_set_compact_cbor: "80",
      field_preimage_lengths_cbor: "80",
    },
    verdict: "ForcedTxValid",
  },
  SDK.ForcedInclusionTxV1,
);

type Variant = "deposit" | "withdrawal" | "forced-transaction";

const makeProof = async (variant: Variant) => {
  const keyCbor =
    variant === "deposit"
      ? DEPOSIT_KEY_CBOR
      : variant === "withdrawal"
        ? WITHDRAWAL_KEY_CBOR
        : FORCED_KEY_CBOR;
  const valueCbor =
    variant === "deposit"
      ? DEPOSIT_VALUE_CBOR
      : variant === "withdrawal"
        ? WITHDRAWAL_VALUE_CBOR
        : FORCED_VALUE_CBOR;
  const domain =
    variant === "deposit"
      ? SDK.ROOT_DOMAINS.deposits
      : variant === "withdrawal"
        ? SDK.ROOT_DOMAINS.withdrawals
        : SDK.ROOT_DOMAINS.forcedTransactionsV1;
  const counted = await buildCountedRoot(domain, [
    {
      key: Buffer.from(keyCbor, "hex"),
      value: Buffer.from(valueCbor, "hex"),
    },
  ]);
  const proof = await keyValuePhasProof(
    { root: counted.phasRoot, count: counted.count, entries: counted.entries },
    Buffer.from(keyCbor, "hex"),
    Buffer.from(valueCbor, "hex"),
  );
  const key = Data.from(keyCbor, SDK.OutputReference);
  const committedEvent: SDK.CommittedDuplicateEventProof =
    variant === "deposit"
      ? {
          CommittedDuplicateDepositV1: {
            membership: {
              domain,
              root: counted.root,
              phas_root: counted.phasRoot,
              count: counted.count,
              key,
              value: Data.from(valueCbor, SDK.DepositInfo),
              proof,
            },
          },
        }
      : variant === "withdrawal"
        ? {
            CommittedDuplicateWithdrawalV1: {
              membership: {
                domain,
                root: counted.root,
                phas_root: counted.phasRoot,
                count: counted.count,
                key,
                value: Data.from(valueCbor, SDK.WithdrawalInfo),
                proof,
              },
            },
          }
        : {
            CommittedDuplicateForcedTransactionV1: {
              membership: {
                domain,
                root: counted.root,
                phas_root: counted.phasRoot,
                count: counted.count,
                key,
                value: Data.from(valueCbor, SDK.ForcedInclusionTxV1),
                proof,
              },
            },
          };
  return { counted, committedEvent };
};

const setupLifecycle = async (variant: Variant) => {
  const harness = await makeFaultProofEmulatorHarness({
    contractOptions: {
      realCrossBlockDuplicateEvent: true,
      alwaysFraudProofCatalogue: true,
    },
  });
  const family = harness.contracts.crossBlockDuplicateEvent;
  const category = harness.catalogue.categories.crossBlockDuplicateEvent;
  if (family === undefined || category === undefined) {
    throw new Error(
      "cross-block-duplicate-event emulator deployment is absent",
    );
  }
  const { counted, committedEvent } = await makeProof(variant);
  const operator = await funderPaymentKeyHash(harness.funderLucid);
  const start =
    alignUnixTimeToEmulatorSlotBoundary(
      harness.funderLucid,
      harness.emulator.now() + 120_000,
    ) - 1;
  const base = makeHeader(operator, start);
  const header: SDK.Header = {
    ...base,
    ...(variant === "deposit"
      ? { depositsRoot: counted.root, depositCount: counted.count }
      : variant === "withdrawal"
        ? { withdrawalsRoot: counted.root, withdrawalCount: counted.count }
        : {
            forcedTransactionsRoot: counted.root,
            forcedTransactionCount: counted.count,
            // `header_transition_commitments_v1_are_valid` requires
            // `validation_trace_count == forced_transaction_count +
            // l2_transaction_count`, and the traces root to match that count.
            // Only the forced-transaction variant moves that sum off zero.
            validationTracesRoot: counted.root,
            validationTraceCount: counted.count,
          }),
    totalEventCount: counted.count,
    transitionStepCount: counted.count,
    transitionTraceRoot: counted.root,
    eventToStepRoot: counted.root,
  };
  const setup = await submitSetupTx({
    lucid: harness.funderLucid,
    contracts: harness.contracts,
    nonceUtxo: harness.nonceUtxo,
    catalogue: harness.catalogue,
    header,
  });

  const settledHeaderHash = (
    variant === "deposit" ? "41" : variant === "withdrawal" ? "42" : "43"
  ).repeat(28);
  const settlementUnit = toUnit(
    harness.contracts.settlement.policyId,
    settledHeaderHash,
  );
  const settlementDatum: SDK.SettlementDatum = {
    deposits_root:
      variant === "deposit" ? counted.root : SDK.EMPTY_MERKLE_TREE_ROOT,
    withdrawals_root:
      variant === "withdrawal" ? counted.root : SDK.EMPTY_MERKLE_TREE_ROOT,
    forced_transactions_root:
      variant === "forced-transaction"
        ? counted.root
        : SDK.EMPTY_MERKLE_TREE_ROOT,
    transactions_root: SDK.EMPTY_MERKLE_TREE_ROOT,
    resolution_claim: null,
  };
  const settlementTx = await harness.funderLucid
    .newTx()
    .mintAssets({ [settlementUnit]: 1n }, Data.void())
    .pay.ToContract(
      harness.contracts.settlement.spendingScriptAddress,
      { kind: "inline", value: Data.to(settlementDatum, SDK.SettlementDatum) },
      { lovelace: 5_000_000n, [settlementUnit]: 1n },
    )
    .attach.MintingPolicy(harness.contracts.settlement.mintingScript)
    .complete({ localUPLCEval: true });
  const settlementHash = await (
    await settlementTx.sign.withWallet().complete()
  ).submit();
  await harness.funderLucid.awaitTx(settlementHash);
  const settlementUtxo = await expectSingleUtxoWithUnit(
    harness.funderLucid,
    harness.contracts.settlement.spendingScriptAddress,
    settlementUnit,
  );
  const references = await publishCrossBlockDuplicateEventReferenceScripts({
    lucid: harness.funderLucid,
    contracts: family,
  });
  return {
    ...harness,
    family,
    category: {
      ...category,
      categoryId: SDK.CROSS_BLOCK_DUPLICATE_EVENT_FRAUD_CATEGORY_ID,
    },
    committedEvent,
    setup,
    settledHeaderHash,
    settlementUtxo,
    references,
  };
};

const init = async (scenario: Awaited<ReturnType<typeof setupLifecycle>>) =>
  await submitCrossBlockDuplicateEventInit({
    lucid: scenario.proverLucid,
    blueprint: scenario.realBlueprint,
    network,
    contracts: scenario.family,
    category: scenario.category,
    catalogue: {
      policyId: scenario.contracts.fraudProofCatalogue.policyId,
      spendingScriptAddress:
        scenario.contracts.fraudProofCatalogue.spendingScriptAddress,
      root: scenario.catalogue.root,
    },
    signer: scenario.proverSigner,
    fraudulentBlockOutRef: scenario.setup.fraudulentBlockOutRef,
    witnessReferenceScripts: scenario.witnessReferenceScripts,
  });

describe.each(["deposit", "withdrawal", "forced-transaction"] as const)(
  "cross-block duplicate %s lifecycle",
  (variant) => {
    it("mints permanent evidence and removes the fraudulent block", async () => {
      const scenario = await setupLifecycle(variant);
      const initialized = await init(scenario);
      const resumedStep01 = await resumeCrossBlockDuplicateEvent({
        lucid: scenario.proverLucid,
        network,
        contracts: scenario.family,
        signer: scenario.proverSigner,
        threadOutRef: initialized.nextThreadOutRef,
        stateQueueBlockOutRef: scenario.setup.fraudulentBlockOutRef,
        challengedEvent: scenario.committedEvent,
        settlementOutRef: outRefLabel(scenario.settlementUtxo),
        settledHeaderHash: scenario.settledHeaderHash,
        settledEvent: scenario.committedEvent,
        referenceScriptUtxos: scenario.references,
        witnessReferenceScripts: scenario.witnessReferenceScripts,
      });
      expect(resumedStep01.resumedStep).toBe("step-01");
      if (resumedStep01.resumedStep !== "step-01") {
        throw new Error("expected step-01 resume");
      }
      const resumedStep02 = await resumeCrossBlockDuplicateEvent({
        lucid: scenario.proverLucid,
        network,
        contracts: scenario.family,
        signer: scenario.proverSigner,
        threadOutRef: resumedStep01.result.nextThreadOutRef,
        stateQueueBlockOutRef: scenario.setup.fraudulentBlockOutRef,
        challengedEvent: scenario.committedEvent,
        settlementOutRef: outRefLabel(scenario.settlementUtxo),
        settledHeaderHash: scenario.settledHeaderHash,
        settledEvent: scenario.committedEvent,
        referenceScriptUtxos: scenario.references,
        witnessReferenceScripts: scenario.witnessReferenceScripts,
      });
      expect(resumedStep02.resumedStep).toBe("step-02");
      if (resumedStep02.resumedStep !== "step-02") {
        throw new Error("expected step-02 resume");
      }
      const step02 = resumedStep02.result;
      const fraudProof = await expectSingleUtxoWithUnit(
        scenario.proverLucid,
        scenario.family.fraudProof.spendingScriptAddress,
        step02.fraudProofUnit,
      );
      expect(outRefLabel(fraudProof)).toBe(step02.fraudProofOutRef);

      const removalRefs = await publishRemovalReferenceScripts({
        lucid: scenario.proverLucid,
        contracts: scenario.contracts,
      });
      const deployment = buildRemovalDeploymentInfo(
        scenario.contracts,
        scenario.catalogue,
        { removalReferenceScripts: removalRefs.published },
      );
      const now = BigInt(scenario.emulator.now());
      const removal = await submitRemoveFraudulentBlock({
        lucid: scenario.proverLucid,
        blueprint: scenario.realBlueprint,
        deploymentInfo: deployment,
        network,
        signer: scenario.proverSigner,
        fraudCategory: "crossBlockDuplicateEvent",
        fraudulentHeaderHash: scenario.setup.headerHash,
        requireReferenceScripts: true,
        validFrom: now > 120_000n ? now - 120_000n : 0n,
        validTo: now + 300_000n,
      });
      expect(removal.transactions.at(-1)?.kind).toBe("remove-target");
      await expect(
        scenario.proverLucid.utxosAtWithUnit(
          scenario.contracts.stateQueue.spendingScriptAddress,
          scenario.setup.stateQueueBlockUnit,
        ),
      ).resolves.toHaveLength(0);
      const retained = await expectSingleUtxoWithUnit(
        scenario.proverLucid,
        scenario.family.fraudProof.spendingScriptAddress,
        step02.fraudProofUnit,
      );
      expect(outRefLabel(retained)).toBe(step02.fraudProofOutRef);
    }, 600_000);
  },
);

describe("cross-block duplicate cancellation/resume", () => {
  it("cancels at either step and can initialize a fresh thread", async () => {
    const scenario = await setupLifecycle("deposit");
    const first = await init(scenario);
    const cancelled = await submitCrossBlockDuplicateEventCancel({
      lucid: scenario.proverLucid,
      contracts: scenario.family,
      signer: scenario.proverSigner,
      threadOutRef: first.nextThreadOutRef,
      referenceScriptUtxo: scenario.references[0],
      witnessReferenceScripts: scenario.witnessReferenceScripts,
    });
    expect(cancelled.cancelledStepIndex).toBe(0);
    const resumed = await init(scenario);
    expect(resumed.nextThreadOutRef).not.toBe(first.nextThreadOutRef);
    const advanced = await submitCrossBlockDuplicateEventStep01({
      lucid: scenario.proverLucid,
      network,
      contracts: scenario.family,
      signer: scenario.proverSigner,
      threadOutRef: resumed.nextThreadOutRef,
      stateQueueBlockOutRef: scenario.setup.fraudulentBlockOutRef,
      committedEvent: scenario.committedEvent,
      referenceScriptUtxo: scenario.references[0],
    });
    const cancelledAfterHandoff = await submitCrossBlockDuplicateEventCancel({
      lucid: scenario.proverLucid,
      contracts: scenario.family,
      signer: scenario.proverSigner,
      threadOutRef: advanced.nextThreadOutRef,
      referenceScriptUtxo: scenario.references[1],
      witnessReferenceScripts: scenario.witnessReferenceScripts,
    });
    expect(cancelledAfterHandoff.cancelledStepIndex).toBe(1);
    const resumedAgain = await init(scenario);
    expect(resumedAgain.nextThreadOutRef).not.toBe(resumed.nextThreadOutRef);
  }, 300_000);
});
