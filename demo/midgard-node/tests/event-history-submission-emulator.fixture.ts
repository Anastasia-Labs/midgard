import { readFileSync } from "node:fs";

import {
  historyPairPayloads,
  setupHistoryPair,
} from "@al-ft/midgard-fault-proofs/test-support/history-pair";
import * as SDK from "@al-ft/midgard-sdk";
import { type UTxO } from "@lucid-evolution/lucid";
import { Clock, Duration, Effect } from "effect";

export type Fixture = Awaited<ReturnType<typeof setupHistoryPair>>;

/** Genuine history policies; the shared emulator fixture uses a native hub.
 * This combines the production node journal/driver with applied scripts, not
 * the deployed state-queue/frontier or live provider acceptance gates. */
export const setupHistoryContracts = async () => {
  const blueprint = SDK.parseFaultProofBlueprint(
    JSON.parse(
      readFileSync(
        process.env.MIDGARD_REAL_BLUEPRINT_PATH ??
          new URL("../../../onchain/aiken/plutus.json", import.meta.url),
        "utf8",
      ),
    ),
  );
  const h = await setupHistoryPair({ blueprint, records: [] });
  const history = (index: number): SDK.EventHistoryContracts => {
    const applied = h.applied[index]!;
    return {
      recipe: h.recipes[index]!,
      list: {
        ...SDK.makeAuthenticatedValidator(
          "Custom",
          applied.validator.script,
          applied.validator.script,
        ),
        ...SDK.makeWithdrawalValidator(applied.validator.script),
      },
      retention: SDK.makeSpendingValidator(
        "Custom",
        applied.retention.validator.script,
      ),
      retirement: SDK.makeWithdrawalValidator(
        applied.retirement.validator.script,
      ),
    };
  };
  const pair = { deposit: history(0), withdrawal: history(1) };
  const contracts: SDK.UserHistoryContracts = {
    hubOracle: {
      policyId: h.hubPolicy,
      mintingScript: h.issuer,
      mintingScriptCBOR: h.issuer.script,
      spendingScript: h.issuer,
      spendingScriptCBOR: h.issuer.script,
      spendingScriptHash: h.hubPolicy,
      spendingScriptAddress: h.hubAddress,
    },
    eventHistory: pair,
    deposit: pair.deposit.list,
    withdrawal: pair.withdrawal.list,
  };
  return { h, contracts };
};

/** Program sleeps advance the emulator instead of wall time, then run the
 * adversary, so a protection wait is virtual and can be contended. */
export const emulatorClock = (
  h: Fixture,
  onSleep: () => Promise<void> = async () => {},
): Clock.Clock => ({
  [Clock.ClockTypeId]: Clock.ClockTypeId,
  unsafeCurrentTimeMillis: () => h.emulator.now(),
  currentTimeMillis: Effect.sync(() => h.emulator.now()),
  unsafeCurrentTimeNanos: () => BigInt(h.emulator.now()) * 1_000_000n,
  currentTimeNanos: Effect.sync(() => BigInt(h.emulator.now()) * 1_000_000n),
  sleep: (duration) =>
    Effect.promise(async () => {
      h.emulator.awaitSlot(
        Math.max(1, Math.ceil(Duration.toMillis(duration) / 1_000)),
      );
      await onSleep();
    }),
});

export const keyValue = async (id: Pick<UTxO, "txHash" | "outputIndex">) =>
  BigInt(
    `0x${await Effect.runPromise(
      SDK.eventHistoryKey({
        transactionId: id.txHash,
        outputIndex: BigInt(id.outputIndex),
      }),
    )}`,
  );

export const depositRequest = (
  h: Fixture,
  nonce: UTxO,
): SDK.EventHistorySubmissionRequest => {
  const template = historyPairPayloads(h)[0]!;
  if (!("DepositPayload" in template))
    throw new Error("Fixture deposit payload is missing");
  return {
    payload: {
      DepositPayload: {
        event: {
          ...template.DepositPayload.event,
          id: {
            transactionId: nonce.txHash,
            outputIndex: BigInt(nonce.outputIndex),
          },
        },
      },
    },
    nonce,
    assets: { lovelace: 25_000_000n },
    structuralLovelace: 5_000_000n,
    structuralRefundKey: h.owner,
    reclaimAuth: { PublicKeyCredential: [h.owner] },
  };
};

/** Two fresh wallet nonces whose keys fall on the same side of the deposit
 * list's fixture filler, so both admissions spend the same predecessor. Of
 * eight outputs, at least four share a side. */
export const sharedPredecessorNonces = async (h: Fixture) => {
  const filler = BigInt(`0x${h.keys[0]!}`);
  h.lucid.clearUTxOOverride();
  let tx = h.lucid.newTx();
  for (let i = 0; i < 8; i++)
    tx = tx.pay.ToAddress(h.wallet.address, { lovelace: 5_000_000n });
  const txHash = await h.submit(
    "concurrent-deposit-nonces",
    await tx.complete({ localUPLCEval: true }),
  );
  const sides: [UTxO[], UTxO[]] = [[], []];
  for (const utxo of await h.lucid.utxosByOutRef(
    Array.from({ length: 8 }, (_, outputIndex) => ({ txHash, outputIndex })),
  ))
    sides[(await keyValue(utxo)) > filler ? 1 : 0].push(utxo);
  return (sides[0].length >= 2 ? sides[0] : sides[1]).slice(0, 2);
};
