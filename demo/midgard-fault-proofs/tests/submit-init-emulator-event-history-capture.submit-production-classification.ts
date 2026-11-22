import type * as SDK from "@al-ft/midgard-sdk/users";
import { CML, type UTxO } from "@lucid-evolution/lucid";
import { expect } from "vitest";

import { fabricatedHistoryOpeningCbor } from "../src/fabricated-history-witness.js";
import { proofMaterialsAddress } from "../src/funding/proof-materials-address.js";
import { buildFabricatedDepositStep03 } from "../src/submit-fabricated-deposit-step-03.js";
import { buildFabricatedWithdrawalStep03 } from "../src/submit-fabricated-withdrawal-step-03.js";
import { productionContracts } from "./submit-init-emulator-event-history-capture.build-capture.js";
import {
  type Harness,
  type Proof,
  records,
} from "./submit-init-emulator-event-history-capture.setup-proof.js";
import { measureCompleteSignedTransaction } from "./support/emulator/measurement.js";
import { EMULATOR_PROTOCOL_PARAMETERS } from "./support/emulator/protocol-parameters.js";
import { buildAndSubmit } from "./support/submit-steps.js";

export const submitProductionClassification = async (
  h: Harness,
  p: Proof,
  thread: UTxO,
  captured: ReturnType<typeof SDK.captureEventHistoryWitness> | undefined,
) => {
  const contracts = productionContracts(h, p);
  const submit = async (
    input: Parameters<typeof buildFabricatedDepositStep03>[0] &
      Parameters<typeof buildFabricatedWithdrawalStep03>[0],
  ) =>
    p.kind === "Deposit"
      ? await buildFabricatedDepositStep03(input)
      : await buildFabricatedWithdrawalStep03(input);
  return buildAndSubmit(submit, {
    lucid: h.lucid,
    contracts,
    // Built from the shared fields: a retirement journey's list harness
    // carries no signer of its own.
    signer: {
      source: "emulator fixture",
      address: h.wallet.address,
      proofMaterialsAddress: proofMaterialsAddress("Custom", h.owner),
      paymentKeyHash: h.owner,
      selectWallet: (lucid) => lucid.selectWallet.fromSeed(h.wallet.seedPhrase),
    },
    threadOutRef: `${thread.txHash}#${thread.outputIndex}`,
    openingCbor: captured ? fabricatedHistoryOpeningCbor(captured) : undefined,
    referenceScriptUtxo: p.refs[1]!,
    now: () => h.emulator.now(),
    beforeSubmit: ({ signed, txHash }) => {
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
