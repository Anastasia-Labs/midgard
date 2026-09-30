import { randomUUID } from "node:crypto";

import type {
  AvailabilityOperationIntent,
  AvailabilityOperationLease,
} from "@al-ft/midgard-core/availability-operation-journal";
import {
  calculateMinLovelaceFromUTxO,
  CML,
  type LucidEvolution,
  type TxSignBuilder,
  type UTxO,
} from "@lucid-evolution/lucid";

import { type DaAvailabilityOperationContext } from "./availability-challenge-operation.inspect-da-availability-signed-intent.js";
import { STATE_QUEUE_NODE_ASSET_NAME_PREFIX } from "./linked-list.js";

/**
 * The deployed Open predicate requires an exact
 * `challenger_bond_lovelace + challenge_record_lovelace + fee` input.
 */
export const buildDaAvailabilityFundingPreparationTx = async (
  lucid: LucidEvolution,
  input: Readonly<{
    fundingInput: UTxO;
    outputLovelace: bigint;
    feeLovelace: bigint;
    validFrom: bigint;
    validTo: bigint;
  }>,
): Promise<TxSignBuilder> => {
  const walletAddress = await lucid.wallet().address();
  const funding = input.fundingInput;
  if (
    funding.address !== walletAddress ||
    funding.datum !== undefined ||
    funding.datumHash !== undefined ||
    funding.scriptRef !== undefined ||
    Object.keys(funding.assets).length !== 1 ||
    input.outputLovelace <= 0n ||
    input.feeLovelace <= 0n ||
    input.validFrom < 0n ||
    input.validTo <= input.validFrom ||
    input.validTo - input.validFrom > 120_000n ||
    input.validTo > BigInt(Number.MAX_SAFE_INTEGER)
  ) {
    throw new Error("Invalid availability funding preparation resources");
  }
  const change =
    (funding.assets.lovelace ?? 0n) - input.outputLovelace - input.feeLovelace;
  const coinsPerByte = lucid.config().protocolParameters?.coinsPerUtxoByte;
  if (!coinsPerByte)
    throw new Error(
      "Availability funding preparation needs live protocol parameters",
    );
  for (const lovelace of [input.outputLovelace, change]) {
    if (
      lovelace !== 0n &&
      lovelace <
        calculateMinLovelaceFromUTxO(coinsPerByte, {
          address: walletAddress,
          assets: { lovelace },
          txHash: "00".repeat(32),
          outputIndex: 0,
        })
    )
      throw new Error(
        "Availability funding preparation has insufficient working capital or min-ADA change",
      );
  }
  const live = await lucid.utxosByOutRef([funding]);
  if (
    live.length !== 1 ||
    live[0]!.address !== funding.address ||
    live[0]!.assets.lovelace !== funding.assets.lovelace ||
    Object.keys(live[0]!.assets).length !== 1
  ) {
    throw new Error("Availability funding preparation input is stale");
  }
  let tx = lucid
    .newTx()
    .collectFrom([funding])
    .pay.ToAddress(walletAddress, { lovelace: input.outputLovelace });
  if (change > 0n) tx = tx.pay.ToAddress(walletAddress, { lovelace: change });
  const built = await tx
    .setMinFee(input.feeLovelace)
    .validFrom(Number(input.validFrom))
    .validTo(Number(input.validTo))
    .complete({ localUPLCEval: true, coinSelection: false });
  const body = built.toTransaction().body();
  if (
    body.fee() !== input.feeLovelace ||
    body.inputs().len() !== 1 ||
    body.outputs().len() !== (change > 0n ? 2 : 1)
  ) {
    throw new Error(
      "Availability funding preparation changed the reserved layout",
    );
  }
  return built;
};

const assertContext = (context: DaAvailabilityOperationContext): void => {
  if (
    !/^[0-9a-f]{64}$/u.test(context.deploymentIdentity) ||
    !/^[0-9a-f]{56}$/u.test(context.actor) ||
    !/^[0-9a-f]{56}$/u.test(context.stateQueuePolicyId) ||
    !Number.isSafeInteger(context.minimumConfirmationDepth) ||
    context.minimumConfirmationDepth <= 0
  ) {
    throw new Error(
      "Invalid availability operation deployment/finality authority",
    );
  }
};

export const assertTerminalIntent = (
  context: DaAvailabilityOperationContext,
  intent: AvailabilityOperationIntent,
): void => {
  if (intent.action !== "timeout" && intent.action !== "remove") return;
  const mint = CML.Transaction.from_cbor_hex(intent.signedCbor).body().mint();
  const removesTarget =
    mint?.get(
      CML.ScriptHash.from_hex(context.stateQueuePolicyId),
      CML.AssetName.from_hex(
        STATE_QUEUE_NODE_ASSET_NAME_PREFIX + intent.headerHash,
      ),
    ) === -1n;
  if (intent.completesWorkflow !== removesTarget) {
    throw new Error(
      "Availability terminal metadata does not match the signed target-header burn",
    );
  }
};

export const withLease = async <T>(
  context: DaAvailabilityOperationContext,
  run: (
    lease: AvailabilityOperationLease,
    assertCurrent: () => Promise<void>,
  ) => Promise<T>,
): Promise<T> => {
  assertContext(context);
  const now = context.nowMs ?? Date.now;
  const lease = context.journal.acquire(
    context.actor,
    randomUUID(),
    now(),
    context.leaseDurationMs ?? 300_000,
  );
  const assertCurrent = async (): Promise<void> => {
    context.journal.assertLease(lease, now());
    await context.assertActuationCurrent();
    context.journal.assertLease(lease, now());
  };
  try {
    await assertCurrent();
    return await run(lease, assertCurrent);
  } finally {
    context.journal.release(lease);
  }
};
