import * as SDK from "@al-ft/midgard-sdk";
import { Data, type LucidEvolution, type UTxO } from "@lucid-evolution/lucid";
import { expect } from "vitest";

import { type DoubleWithdrawContracts } from "../src/double-withdraw/contracts.js";
import {
  parseSubmitDoubleWithdrawInclusion,
  type SubmitDoubleWithdrawInclusion,
} from "../src/double-withdraw/submit-double-withdraw-step-01.js";
import {
  buildCountedRoot,
  keyValuePhasProof,
} from "../src/transition-trace/phas.js";
import {
  alignUnixTimeToEmulatorSlotBoundary,
  funderPaymentKeyHash,
  makeFaultProofEmulatorHarness,
  makeHeader,
  publishPlainReferenceScriptUtxo,
  submitSetupTx,
} from "./support/submit-init-emulator-shared.js";

export const FIRST_ID: SDK.OutputReference = {
  transactionId: "8b".repeat(32),
  outputIndex: 2n,
};

export const SECOND_ID: SDK.OutputReference = {
  transactionId: "c4".repeat(32),
  outputIndex: 1n,
};

const SHARED_OUTREF: SDK.OutputReference = {
  transactionId: "7e".repeat(32),
  outputIndex: 1n,
};

export const PAYABLE_INFO: SDK.WithdrawalInfo = {
  body: {
    l2_outref: SHARED_OUTREF,
    l2_owner: "9c".repeat(28),
    l2_value: new Map([["4b".repeat(28), new Map([["6d696467617264", 42n]])]]),
    l1_address: {
      paymentCredential: { PublicKeyCredential: ["2b".repeat(28)] },
      stakeCredential: null,
    },
    l1_datum: "NoDatum",
  },
  signature: ["ad".repeat(32), "be".repeat(64)],
  validity: "WithdrawalIsValid",
};

export const HONEST_DUPLICATE_INFO: SDK.WithdrawalInfo = {
  ...PAYABLE_INFO,
  validity: { SpentWithdrawalUtxo: { l2_tx_id: "5a".repeat(32) } },
};

const entry = (
  id: SDK.OutputReference,
  info: SDK.WithdrawalInfo,
): readonly [string, string] => [
  SDK.committedWithdrawalKeyBytes(id),
  SDK.committedWithdrawalValueBytes(info),
];

export const makeHarness = async () => {
  const harness = await makeFaultProofEmulatorHarness({
    contractOptions: {
      realDoubleWithdraw: true,
      alwaysFraudProofCatalogue: true,
    },
  });
  const doubleWithdraw = harness.contracts.doubleWithdraw;
  const category = harness.catalogue.categories.doubleWithdraw;
  if (doubleWithdraw === undefined || category === undefined) {
    throw new Error("double-withdraw harness contracts/category missing");
  }
  expect(category.categoryId).toBe(
    SDK.FRAUD_PROOF_CATALOGUE_CATEGORY_IDS.doubleWithdraw,
  );
  expect(category.scriptHash).toBe(doubleWithdraw.steps[0].spendingScriptHash);
  return { ...harness, doubleWithdraw, category };
};

export const setupBlock = async ({
  harness,
  secondInfo,
}: {
  readonly harness: Awaited<ReturnType<typeof makeHarness>>;
  readonly secondInfo: SDK.WithdrawalInfo;
}) => {
  const entries = [entry(FIRST_ID, PAYABLE_INFO), entry(SECOND_ID, secondInfo)];
  const counted = await buildCountedRoot(
    SDK.ROOT_DOMAINS.withdrawals,
    entries.map(([key, value]) => ({
      key: Buffer.from(key, "hex"),
      value: Buffer.from(value, "hex"),
    })),
  );
  const operatorVkey = await funderPaymentKeyHash(harness.funderLucid);
  const start =
    alignUnixTimeToEmulatorSlotBoundary(
      harness.funderLucid,
      harness.emulator.now() + 120_000,
    ) - 1;
  const header: SDK.Header = {
    ...makeHeader(operatorVkey, start),
    withdrawalsRoot: counted.root,
    withdrawalCount: counted.count,
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
  return { entries, counted, header, setup };
};

export const publishStepReferences = async ({
  lucid,
  contracts,
}: {
  readonly lucid: LucidEvolution;
  readonly contracts: DoubleWithdrawContracts;
}): Promise<readonly [UTxO, UTxO]> => {
  const first = await publishPlainReferenceScriptUtxo({
    lucid,
    script: contracts.steps[0].spendingScript,
    label: "double-withdraw step-01",
  });
  const second = await publishPlainReferenceScriptUtxo({
    lucid,
    script: contracts.steps[1].spendingScript,
    label: "double-withdraw step-02",
  });
  return [first.utxo, second.utxo];
};

export const inclusionFor = async ({
  counted,
  leaf,
}: {
  readonly counted: Awaited<ReturnType<typeof buildCountedRoot>>;
  readonly leaf: readonly [string, string];
}): Promise<SubmitDoubleWithdrawInclusion> => {
  const proof = await keyValuePhasProof(
    { ...counted, root: counted.phasRoot },
    Buffer.from(leaf[0], "hex"),
    Buffer.from(leaf[1], "hex"),
  );
  return parseSubmitDoubleWithdrawInclusion({
    withdrawalIdCbor: leaf[0],
    withdrawalInfoCbor: leaf[1],
    withdrawalsPhasRoot: counted.phasRoot,
    withdrawalMembershipProofCbor: Data.to(proof, SDK.Proof),
  });
};
