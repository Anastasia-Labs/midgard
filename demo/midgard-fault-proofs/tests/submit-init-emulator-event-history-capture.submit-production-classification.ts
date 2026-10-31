import * as SDK from "@al-ft/midgard-sdk";
import { CML, type UTxO } from "@lucid-evolution/lucid";
import { expect } from "vitest";

import { fabricatedHistoryOpeningCbor } from "../src/fabricated-history-witness.js";
import { submitFabricatedDepositStep03 } from "../src/submit-fabricated-deposit-step-03.js";
import { submitFabricatedWithdrawalStep03 } from "../src/submit-fabricated-withdrawal-step-03.js";
import { productionContracts } from "./submit-init-emulator-event-history-capture.build-capture.js";
import {
  type Harness,
  type Proof,
  records,
} from "./submit-init-emulator-event-history-capture.setup-proof.js";
import { measureCompleteSignedTransaction } from "./support/emulator/measurement.js";
import { EMULATOR_PROTOCOL_PARAMETERS } from "./support/emulator/protocol-parameters.js";

export const submitProductionClassification = async (
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
    openingCbor: captured ? fabricatedHistoryOpeningCbor(captured) : undefined,
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
