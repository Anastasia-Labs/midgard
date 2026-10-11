/**
 * A block whose two withdrawal orders name one L2 outref, with the chained
 * trace an honest operator commits: step 0 drains the outref, step 1 is a
 * no-op over the ledger step 0 leaves. The challenged leaf is the second one.
 */
import { computeHash32 } from "@al-ft/midgard-core";
import {
  encodeMidgardSpendInputItem,
  encodeMidgardTxOutput,
} from "@al-ft/midgard-core/codec";
import * as SDK from "@al-ft/midgard-sdk";
import { buildCanonicalMidgardLedgerOutputMaterial } from "@al-ft/midgard-validation";
import { CML, Data } from "@lucid-evolution/lucid";

import {
  buildCountedRoot,
  type CountedRoot,
  keyValuePhasNonMembershipProof,
  keyValuePhasProof,
  keyValuePhasRootWithCount,
} from "../../src/transition-trace/phas.js";
import { prepareWithdrawalMistag } from "../../src/withdrawal-mistag/index.js";
import {
  alignUnixTimeToEmulatorSlotBoundary,
  funderPaymentKeyHash,
  makeHeader,
  submitSetupTx,
} from "./submit-init-emulator-shared.js";
import { type makeWithdrawalMistagEmulatorHarness } from "./withdrawal-mistag-emulator.js";

type Harness = Awaited<ReturnType<typeof makeWithdrawalMistagEmulatorHarness>>;

const FIRST_ID: SDK.OutputReference = {
  transactionId: "51".repeat(32),
  outputIndex: 0n,
};
const SECOND_ID: SDK.OutputReference = {
  transactionId: "52".repeat(32),
  outputIndex: 0n,
};
const SHARED_OUTREF: SDK.OutputReference = {
  transactionId: "53".repeat(32),
  outputIndex: 0n,
};
const OTHER_OUTREF: SDK.OutputReference = {
  transactionId: "54".repeat(32),
  outputIndex: 0n,
};

const ledgerKey = (outRef: SDK.OutputReference) =>
  encodeMidgardSpendInputItem({
    txId: Buffer.from(outRef.transactionId, "hex"),
    outputIndex: Number(outRef.outputIndex),
  });

const membership = async (counted: CountedRoot, key: Buffer, value: Buffer) =>
  await keyValuePhasProof({ ...counted, root: counted.phasRoot }, key, value);

/**
 * The evidence for the second order. `secondValidity` is its committed tag:
 * the honest operator's `NonExistentWithdrawalUtxo`, or a dishonest
 * `WithdrawalIsValid`.
 */
export const buildDuplicateOutRefMistagMaterial = async (
  secondValidity: SDK.WithdrawalInfo["validity"],
) => {
  const privateKey = CML.PrivateKey.generate_ed25519();
  const publicKey = privateKey.to_public();
  const owner = publicKey.hash().to_hex();
  const lovelace = 1_000_000n;
  const body: SDK.WithdrawalBody = {
    l2_outref: SHARED_OUTREF,
    l2_owner: owner,
    l2_value: new Map([["", new Map([["", lovelace]])]]),
    l1_address: {
      paymentCredential: { PublicKeyCredential: [owner] },
      stakeCredential: null,
    },
    l1_datum: "NoDatum",
  };
  const signature: SDK.WithdrawalInfo["signature"] = [
    Buffer.from(publicKey.to_raw_bytes()).toString("hex"),
    privateKey
      .sign(
        computeHash32(
          Buffer.concat([
            Buffer.from("MidgardWithdrawalV1", "utf8"),
            Buffer.from(SDK.withdrawalBodyBytes(body), "hex"),
          ]),
        ),
      )
      .to_hex(),
  ];
  const first: SDK.WithdrawalInfo = {
    body,
    signature,
    validity: "WithdrawalIsValid",
  };
  const second: SDK.WithdrawalInfo = {
    body,
    signature,
    validity: secondValidity,
  };

  const descriptor = (lovelaceAmount: bigint) =>
    buildCanonicalMidgardLedgerOutputMaterial({
      outputIndex: 0,
      outputCbor: encodeMidgardTxOutput({
        address: Buffer.concat([
          Buffer.from([0x60]),
          Buffer.from(owner, "hex"),
        ]),
        value: { lovelace: lovelaceAmount, assets: new Map() },
      }),
    }).descriptorCbor;
  const shared = { key: ledgerKey(SHARED_OUTREF), value: descriptor(lovelace) };
  const other = { key: ledgerKey(OTHER_OUTREF), value: descriptor(2_000_000n) };
  // Step 0 drains the shared outref; step 1 sees the ledger it leaves.
  const before = await keyValuePhasRootWithCount([shared, other]);
  const after = await keyValuePhasRootWithCount([other]);

  const withdrawalEntry = (
    id: SDK.OutputReference,
    info: SDK.WithdrawalInfo,
  ) => ({
    key: Buffer.from(SDK.committedWithdrawalKeyBytes(id), "hex"),
    value: Buffer.from(SDK.committedWithdrawalValueBytes(info), "hex"),
  });
  const withdrawalEntries = [
    withdrawalEntry(FIRST_ID, first),
    withdrawalEntry(SECOND_ID, second),
  ];
  const source = await buildCountedRoot(
    SDK.ROOT_DOMAINS.withdrawals,
    withdrawalEntries,
  );

  const eventKeyOf = (id: SDK.OutputReference): SDK.EventKey => ({
    WithdrawalEventKey: { withdrawal_id: id },
  });
  const eventValueOf = (index: bigint): SDK.EventToStepValue => ({
    step_index: index,
    phase: "Withdrawal",
  });
  const eventEntry = (id: SDK.OutputReference, index: bigint) => ({
    key: Buffer.from(Data.to(eventKeyOf(id), SDK.EventKey), "hex"),
    value: Buffer.from(
      Data.to(eventValueOf(index), SDK.EventToStepValue),
      "hex",
    ),
  });
  const eventEntries = [eventEntry(FIRST_ID, 0n), eventEntry(SECOND_ID, 1n)];
  const event = await buildCountedRoot(
    SDK.ROOT_DOMAINS.eventToStep,
    eventEntries,
  );

  const stepOf = (
    index: bigint,
    id: SDK.OutputReference,
    pre: string,
    post: string,
  ): SDK.TransitionStep => ({
    schema_version: SDK.TRANSITION_STEP_SCHEMA_VERSION,
    step_index: index,
    event_key: eventKeyOf(id),
    phase: "Withdrawal",
    pre_utxos_root: pre,
    post_utxos_root: post,
  });
  const steps = [
    stepOf(0n, FIRST_ID, before.root, after.root),
    stepOf(1n, SECOND_ID, after.root, after.root),
  ];
  const stepEntries = steps.map((step) => ({
    key: Buffer.from(Data.to(step.step_index), "hex"),
    value: Buffer.from(Data.to(step, SDK.TransitionStep), "hex"),
  }));
  const trace = await buildCountedRoot(
    SDK.ROOT_DOMAINS.transitionTrace,
    stepEntries,
  );

  return {
    source,
    event,
    trace,
    steps,
    args: {
      committedWithdrawal: {
        domain: SDK.ROOT_DOMAINS.withdrawals,
        root: source.root,
        phas_root: source.phasRoot,
        count: source.count,
        key: SECOND_ID,
        value: second,
        proof: await membership(
          source,
          withdrawalEntries[1]!.key,
          withdrawalEntries[1]!.value,
        ),
      },
      eventToStep: {
        domain: SDK.ROOT_DOMAINS.eventToStep,
        root: event.root,
        phas_root: event.phasRoot,
        count: event.count,
        key: eventKeyOf(SECOND_ID),
        value: eventValueOf(1n),
        proof: await membership(
          event,
          eventEntries[1]!.key,
          eventEntries[1]!.value,
        ),
      },
      transitionStep: {
        domain: SDK.ROOT_DOMAINS.transitionTrace,
        root: trace.root,
        phas_root: trace.phasRoot,
        count: trace.count,
        key: 1n,
        value: steps[1]!,
        proof: await membership(
          trace,
          stepEntries[1]!.key,
          stepEntries[1]!.value,
        ),
      },
      ledgerEvidence: {
        AbsentLedgerOutput: {
          non_membership_proof: await keyValuePhasNonMembershipProof(
            after,
            shared.key,
          ),
        },
      },
    },
  };
};

/** Puts the block on the state queue and prepares the second order's evidence. */
export const setupDuplicateOutRefMistagScenario = async ({
  harness,
  secondValidity,
}: {
  readonly harness: Harness;
  readonly secondValidity: SDK.WithdrawalInfo["validity"];
}) => {
  const material = await buildDuplicateOutRefMistagMaterial(secondValidity);
  const operatorVkey = await funderPaymentKeyHash(harness.funderLucid);
  const startTime =
    alignUnixTimeToEmulatorSlotBoundary(
      harness.funderLucid,
      harness.emulator.now() + 120_000,
    ) - 1;
  const header: SDK.Header = {
    ...makeHeader(operatorVkey, startTime),
    withdrawalsRoot: material.source.root,
    withdrawalCount: material.source.count,
    totalEventCount: material.source.count,
    transitionStepCount: material.trace.count,
    eventToStepRoot: material.event.root,
    transitionTraceRoot: material.trace.root,
  };
  const setup = await submitSetupTx({
    lucid: harness.funderLucid,
    contracts: harness.contracts,
    nonceUtxo: harness.nonceUtxo,
    catalogue: harness.catalogue,
    header,
  });
  const prepared = await prepareWithdrawalMistag({
    challengedHeaderHash: setup.headerHash,
    ...material.args,
  });
  return { material, header, setup, prepared };
};
