import { randomUUID } from "node:crypto";
import { readFileSync } from "node:fs";

import {
  historyPairPayloads,
  insertHistoryFillerAfter,
  setupHistoryPair,
} from "@al-ft/midgard-fault-proofs/test-support/history-pair";
import * as SDK from "@al-ft/midgard-sdk";
import { CML, type UTxO } from "@lucid-evolution/lucid";
import { Clock, Duration, Effect, Logger, Option } from "effect";
import { expect, it, vi } from "vitest";

import * as Journal from "../src/database/eventHistorySubmissions.js";
import { submitDurableEventHistoryProgram } from "../src/transactions/event-history-submission.js";
import { provideDatabaseLayers } from "./utils.js";

type Fixture = Awaited<ReturnType<typeof setupHistoryPair>>;

/** Genuine history policies; the shared emulator fixture uses a native hub.
 * This combines the production node journal/driver with applied scripts, not
 * the deployed state-queue/frontier or live provider acceptance gates. */
const setupHistoryContracts = async () => {
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
const emulatorClock = (
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

it.each(["Deposit", "Withdrawal"] as const)(
  "recovers %s publication after a signing crash without preparing a new nonce",
  async (kind) => {
    const { h, contracts } = await setupHistoryContracts();
    const index = kind === "Deposit" ? 0 : 1;
    const payload = historyPairPayloads(h)[index]!;
    if ("DepositPayload" in payload)
      payload.DepositPayload.event.info.l2_datum = "ab".repeat(600);
    else
      payload.WithdrawalPayload.refund_datum = {
        InlineDatum: { data: "ab".repeat(600) },
      };
    const request: SDK.EventHistorySubmissionRequest = {
      payload,
      nonce: h.eventNonces[index]!,
      assets: { lovelace: kind === "Deposit" ? 25_000_000n : 20_000_000n },
      structuralLovelace: kind === "Deposit" ? 5_000_000n : 0n,
      structuralRefundKey: h.owner,
      reclaimAuth: { PublicKeyCredential: [h.owner] },
    };
    const prepare = vi.fn(() => Effect.succeed({ request }));
    const submissionId = `emulator-${randomUUID()}`;
    const run = () =>
      Effect.runPromise(
        provideDatabaseLayers(
          submitDurableEventHistoryProgram({
            lucid: h.lucid,
            contracts,
            kind,
            submissionId,
            intentHash: "ab".repeat(32),
            nonceInput: request.nonce,
            scriptReference: h.scripts[index]!,
            prepare,
          }).pipe(Effect.withClock(emulatorClock(h))),
        ),
      );
    const signing = vi
      .spyOn(h.lucid.wallet(), "signTx")
      .mockRejectedValueOnce(new Error("simulated crash before signature"));
    try {
      await expect(run()).rejects.toThrow("requires reconciliation");
      const saved = await Effect.runPromise(
        provideDatabaseLayers(Journal.retrieve(submissionId)),
      );
      if (Option.isNone(saved))
        throw new Error("Crash lost its durable request");
      expect(saved.value.checkpoint.pending?.phase).toBe("Publication");
      expect(saved.value.request.nonce.txHash).toBe(request.nonce.txHash);
      const result = await run();
      expect(prepare).toHaveBeenCalledTimes(1);
      expect(result.checkpoint.publicationAttempt).toEqual(
        saved.value.checkpoint.pending,
      );
      expect(result.request.nonce).toEqual(request.nonce);
      expect(result.checkpoint.pending).toBeUndefined();
      expect(
        (await h.lucid.transactionStatus(result.admission.txHash)).status,
      ).toBe("confirmed");
      const resumed = await run();
      expect(resumed.admission).toEqual(result.admission);
      expect(prepare).toHaveBeenCalledTimes(1);
    } finally {
      signing.mockRestore();
    }
  },
  180_000,
);

const keyValue = async (id: Pick<UTxO, "txHash" | "outputIndex">) =>
  BigInt(
    `0x${await Effect.runPromise(
      SDK.eventHistoryKey({
        transactionId: id.txHash,
        outputIndex: BigInt(id.outputIndex),
      }),
    )}`,
  );

const depositRequest = (
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

/** Lands a first deposit, then returns a fresh nonce whose list predecessor
 * that deposit just protected. Nonce keys are hashes, so candidates are drawn
 * on the larger side of the fixture filler: both nodes that side's insertion
 * mutates stay protected, whichever of them precedes the second key. */
const backToBackDeposits = async () => {
  const { h, contracts } = await setupHistoryContracts();
  const deployment = {
    policyId: h.applied[0]!.policyId,
    address: h.applied[0]!.address,
    retentionAddress: h.applied[0]!.retention.address,
    inlineLimitBytes: h.recipes[0]!.inlineLimitBytes,
  };
  const filler = BigInt(`0x${h.keys[0]!}`);
  const upperSide = filler < 2n ** 255n;
  // The node pins the wallet's view after its own transactions; the fixture's
  // wallet-funded transactions must read the emulator instead.
  const draw = async () => {
    h.lucid.clearUTxOOverride();
    for (let round = 0; round < 10; round++) {
      let tx = h.lucid.newTx();
      for (let i = 0; i < 8; i++)
        tx = tx.pay.ToAddress(h.wallet.address, { lovelace: 5_000_000n });
      const txHash = await h.submit(
        "fresh-deposit-nonces",
        await tx.complete({ localUPLCEval: true }),
      );
      for (const utxo of await h.lucid.utxosByOutRef(
        Array.from({ length: 8 }, (_, outputIndex) => ({
          txHash,
          outputIndex,
        })),
      ))
        if ((await keyValue(utxo)) > filler === upperSide) return utxo;
    }
    throw new Error("No fresh nonce on the protected side of the filler");
  };
  const submit = (
    nonce: UTxO,
    submissionId: string,
    clock: Clock.Clock,
    {
      prepare = vi.fn(() =>
        Effect.succeed({ request: depositRequest(h, nonce) }),
      ),
      timeoutMs,
      log = () => {},
    }: {
      readonly prepare?: () => Effect.Effect<{
        readonly request: SDK.EventHistorySubmissionRequest;
      }>;
      readonly timeoutMs?: number;
      readonly log?: (message: string) => void;
    } = {},
  ) =>
    Effect.runPromise(
      provideDatabaseLayers(
        submitDurableEventHistoryProgram({
          lucid: h.lucid,
          contracts,
          kind: "Deposit",
          submissionId,
          intentHash: "cd".repeat(32),
          nonceInput: nonce,
          scriptReference: h.scripts[0]!,
          prepare,
          timeoutMs,
        }).pipe(
          Effect.withClock(clock),
          Effect.provide(
            Logger.add(Logger.make(({ message }) => log(String(message)))),
          ),
        ),
      ),
    );
  await submit(await draw(), `first-${randomUUID()}`, emulatorClock(h));
  const second = await draw();
  const witness = () =>
    SDK.fetchEventHistoryWitness(h.lucid, deployment, {
      transactionId: second.txHash,
      outputIndex: BigInt(second.outputIndex),
    });
  // Non-vacuous: the second admission's first build is refused as protected.
  const initial = await witness();
  expect(initial.kind).toBe("Absent");
  expect(initial.anchor.node.protected_until).toBeGreaterThan(
    BigInt(h.emulator.now() - 60_000),
  );
  /** Once the current protection allows, a list mutation re-protects the
   * second key's predecessor. The maximum validity range, as a reserve payout
   * retirement uses, protects it the longest. Returns the protection's slot
   * end. */
  const reprotect = async (
    validityMs = Number(SDK.MAX_VALIDITY_RANGE_LENGTH_MS),
  ) => {
    const protectedFor =
      Number((await witness()).anchor.node.protected_until) - h.emulator.now();
    if (protectedFor >= 0)
      h.emulator.awaitSlot(Math.ceil(protectedFor / 1_000) + 1);
    const current = await witness();
    const lower = h.emulator.now();
    h.lucid.clearUTxOOverride();
    const validTo = lower + validityMs;
    const anchorKey =
      current.anchor.node.position === "Root"
        ? 0n
        : BigInt(`0x${current.anchor.node.position.Key[0]}`);
    const key = ((anchorKey + (await keyValue(second))) / 2n)
      .toString(16)
      .padStart(64, "0");
    await h.submit(
      "maximum-validity-predecessor-mutation",
      await insertHistoryFillerAfter(
        {
          ...h,
          bounds: () => ({
            lower,
            validTo,
            protectedUntil: BigInt(validTo - 1) + h.protectionDurationMs,
          }),
        },
        "Deposit",
        current,
        key,
        [second],
      ),
    );
    return validTo + Number(h.protectionDurationMs);
  };
  return { h, second, witness, reprotect, submit };
};

it.each([
  { predecessor: "the previous deposit", reprotect: false },
  { predecessor: "a maximum-validity mutation", reprotect: true },
])(
  "waits out protection from $predecessor, then admits the next deposit on the list",
  async ({ reprotect }) => {
    const s = await backToBackDeposits();
    const protectedUntil = reprotect
      ? BigInt(await s.reprotect())
      : (await s.witness()).anchor.node.protected_until;
    const submissionId = `second-${randomUUID()}`;
    const events: string[] = [];
    const result = await s.submit(
      s.second,
      submissionId,
      emulatorClock(s.h, async () => {
        events.push("sleep");
      }),
      {
        // No budget slack: the deadline must carry the whole protection wait.
        timeoutMs: 1_000,
        log: (message) => events.push(message),
      },
    );
    // The operator learns the wait target and the rerun path before it starts.
    expect(events.indexOf("sleep")).toBeGreaterThan(0);
    expect(events[events.indexOf("sleep") - 1]).toMatch(
      new RegExp(
        `^History submission ${submissionId} is waiting until \\S+Z for its list predecessor's protection to end; if interrupted, rerun the same submission ID$`,
        "u",
      ),
    );
    expect(
      (await s.h.lucid.transactionStatus(result.admission.txHash)).status,
    ).toBe("confirmed");
    const start = CML.Transaction.from_cbor_hex(
      result.admission.transactionCbor,
    )
      .body()
      .validity_interval_start();
    if (start === undefined) throw new Error("Admission has no lower bound");
    expect(
      BigInt(s.h.lucid.slotToUnixTime(Number(start))),
    ).toBeGreaterThanOrEqual(protectedUntil);
    expect((await s.witness()).kind).not.toBe("Absent");
  },
  300_000,
);

it.each([
  {
    stop: "predecessor protection keeps moving past the deadline",
    contend: "reprotect",
  },
  {
    stop: "every admission attempt meets a protected predecessor",
    contend: "short-reprotect",
  },
  { stop: "the deadline passes during a wait", contend: "stall" },
] as const)(
  "names the rerun time when $stop with no transaction in flight",
  async ({ contend }) => {
    const s = await backToBackDeposits();
    const submissionId = `contended-${randomUUID()}`;
    const prepare = vi.fn(() =>
      Effect.succeed({ request: depositRequest(s.h, s.second) }),
    );
    let resumeAfter = 0;
    const contended = emulatorClock(s.h, async () => {
      if (contend === "stall") {
        // A stalled wait: the deadline check after it is the only stop.
        s.h.emulator.awaitSlot(3_600);
        resumeAfter = s.h.emulator.now();
      } else
        resumeAfter =
          (await s.reprotect(contend === "reprotect" ? undefined : 10_000)) +
          60_000;
    });
    const failure = await s
      .submit(s.second, submissionId, contended, { prepare })
      .then(
        () => undefined,
        (error: unknown) => error,
      );
    expect(String(failure)).toContain(
      `History submission ${submissionId} has no transaction in flight; rerun the same submission ID after ${new Date(resumeAfter).toISOString()}: `,
    );
    const saved = await Effect.runPromise(
      provideDatabaseLayers(Journal.retrieve(submissionId)),
    );
    if (Option.isNone(saved)) throw new Error("Deferral lost its request");
    expect(saved.value.checkpoint.pending).toBeUndefined();
    expect(saved.value.checkpoint.admission).toBeUndefined();
    s.h.emulator.awaitSlot(
      Math.max(1, Math.ceil((resumeAfter - s.h.emulator.now()) / 1_000) + 1),
    );
    const result = await s.submit(s.second, submissionId, emulatorClock(s.h), {
      prepare,
    });
    expect(prepare).toHaveBeenCalledTimes(1);
    expect(
      (await s.h.lucid.transactionStatus(result.admission.txHash)).status,
    ).toBe("confirmed");
  },
  300_000,
);
