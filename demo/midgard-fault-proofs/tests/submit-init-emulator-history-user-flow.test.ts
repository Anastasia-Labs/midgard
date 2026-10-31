import { createHash } from "node:crypto";
import { mkdirSync, readFileSync, writeFileSync } from "node:fs";
import { join } from "node:path";

import * as SDK from "@al-ft/midgard-sdk";
import { type TxSignBuilder } from "@lucid-evolution/lucid";
import { afterAll, expect, it } from "vitest";

import { realBlueprintPath } from "./support/emulator/blueprints.js";
import {
  historyPairPayloads,
  insertHistoryFillerAfter,
  setupHistoryPair,
} from "./support/emulator/history-pair.js";
import { EMULATOR_PROTOCOL_PARAMETERS } from "./support/emulator/protocol-parameters.js";

const bytes = readFileSync(realBlueprintPath);
const blueprint = SDK.parseFaultProofBlueprint(JSON.parse(bytes.toString()));
const records: unknown[] = [];
afterAll(() => {
  const directory = process.env.MIDGARD_EVENT_HISTORY_EVIDENCE_DIR;
  if (directory === undefined) return;
  mkdirSync(directory, { recursive: true });
  writeFileSync(
    join(directory, "history-user-flow.json"),
    JSON.stringify(
      {
        scope:
          "Applied two-list SDK automatic publication, admission, contention and restart; native fixture hub authority",
        blueprintSha256: createHash("sha256").update(bytes).digest("hex"),
        protocolParameters: EMULATOR_PROTOCOL_PARAMETERS,
        records,
      },
      (_key, value: unknown) =>
        typeof value === "bigint" ? value.toString() : value,
      2,
    ) + "\n",
  );
});

const setup = async (kind: "Deposit" | "Withdrawal", external: boolean) => {
  const h = await setupHistoryPair({ blueprint, records });
  const i = kind === "Deposit" ? 0 : 1;
  const applied = h.applied[i]!;
  const recipe = h.recipes[i]!;
  const payload = historyPairPayloads(h)[i]!;
  if (external) {
    if ("DepositPayload" in payload)
      payload.DepositPayload.event.info.l2_datum = "ab".repeat(600);
    else
      payload.WithdrawalPayload.refund_datum = {
        InlineDatum: { data: "ab".repeat(600) },
      };
  }
  const id =
    "DepositPayload" in payload
      ? payload.DepositPayload.event.id
      : payload.WithdrawalPayload.event.id;
  let checkpoint: SDK.EventHistorySubmissionCheckpoint | undefined;
  const submissions: SDK.EventHistorySubmissionAttempt[] = [];
  const driver: SDK.EventHistorySubmissionDriver = {
    save: async (next) => {
      checkpoint = structuredClone(next);
    },
    submit: async (tx, attempt) => {
      expect(checkpoint?.pending).toEqual(attempt);
      submissions.push(attempt);
      const hash = await h.submit(`${kind}-${attempt.phase}`, tx);
      expect(hash).toBe(attempt.txHash);
      return { kind: "Confirmed" };
    },
    reconcile: async (attempt) => {
      expect(await h.lucid.awaitTx(attempt.txHash)).toBe(true);
      return { kind: "Confirmed" };
    },
    funding: h.funding,
    now: () => h.emulator.now(),
    waitUntil: async (target) => {
      h.emulator.awaitSlot(
        Math.max(1, Math.ceil((target - h.emulator.now()) / 1000)),
      );
    },
  };
  const args = {
    context: {
      lucid: h.lucid,
      applied,
      recipe,
      hubReference: h.hub,
      scriptReference: h.scripts[i]!,
    },
    request: {
      payload,
      reclaimAuth: { PublicKeyCredential: [h.owner] as [string] },
      nonce: h.eventNonces[i]!,
      assets: { lovelace: kind === "Deposit" ? 25_000_000n : 20_000_000n },
      structuralLovelace: kind === "Deposit" ? 5_000_000n : 0n,
      structuralRefundKey: h.owner,
    },
    driver,
    maxAttempts: 5,
    deadlineMs: h.emulator.now() + 600_000,
    validityDurationMs: 120_000,
    outputVisibilityAttempts: 3,
    retryDelayMs: 1000,
  };
  const fetch = () =>
    SDK.fetchEventHistoryWitness(
      h.lucid,
      {
        policyId: applied.policyId,
        address: applied.address,
        retentionAddress: applied.retention.address,
        inlineLimitBytes: recipe.inlineLimitBytes,
      },
      id,
    );
  return { h, args, submissions, fetch, checkpoint: () => checkpoint };
};

it.each([
  { kind: "Deposit" as const, external: false },
  { kind: "Deposit" as const, external: true },
  { kind: "Withdrawal" as const, external: false },
  { kind: "Withdrawal" as const, external: true },
])(
  "automatically admits $kind with external=$external",
  async ({ kind, external }) => {
    const s = await setup(kind, external);
    const result = await SDK.submitEventHistory(s.args);
    expect(s.submissions.map((entry) => entry.phase)).toEqual(
      external ? ["Publication", "Admission"] : ["Admission"],
    );
    expect(result.pending).toBeUndefined();
    const witness = await s.fetch();
    if (witness.kind !== "Present") throw new Error("Missing admitted event");
    expect(witness.anchor.utxo.txHash).toBe(result.admission.txHash);
    expect(witness.payload).toEqual(s.args.request.payload);
    expect(witness.retainedDataUtxo !== undefined).toBe(external);
    const captured = SDK.captureEventHistoryWitness(
      witness,
      s.args.context.applied.policyId,
      kind,
    );
    expect(captured.originalAssets.get("")?.get("")).toBe(20_000_000n);
    expect(await s.h.lucid.utxosByOutRef([s.args.request.nonce])).toHaveLength(
      0,
    );
    const resumed = await SDK.submitEventHistory({
      ...s.args,
      checkpoint: result,
    });
    expect(resumed).toEqual(result);
    expect(s.submissions).toHaveLength(external ? 2 : 1);
  },
  180_000,
);

it.each(["Deposit", "Withdrawal"] as const)(
  "rebuilds %s after genuine filler continuation without republishing",
  async (kind) => {
    const s = await setup(kind, true);
    const submit = s.args.driver.submit;
    let conflicted = false;
    let rejectedHash: string | undefined;
    const driver: SDK.EventHistorySubmissionDriver = {
      ...s.args.driver,
      submit: async (tx, attempt) => {
        if (attempt.phase === "Admission" && !conflicted) {
          conflicted = true;
          rejectedHash = attempt.txHash;
          const witness = await s.fetch();
          const churn = await insertHistoryFillerAfter(
            s.h,
            kind,
            witness,
            "ff".repeat(32),
            [s.args.request.nonce],
          );
          await s.h.submit(`${kind}-concurrent-pointer-change`, churn);
          const signed = await tx.sign.withWallet().complete();
          await expect(signed.submit()).rejects.toThrow();
          expect(
            await s.h.lucid.utxosByOutRef([witness.anchor.utxo]),
          ).toHaveLength(0);
          expect(
            await s.h.lucid.utxosByOutRef([s.args.request.nonce]),
          ).toHaveLength(1);
          records.push({
            label: `${kind}-definitive-input-conflict`,
            accepted: false,
            txHash: attempt.txHash,
            transactionCbor: signed.toCBOR(),
          });
          return { kind: "InputConflict" };
        }
        return submit(tx, attempt);
      },
    };
    const result = await SDK.submitEventHistory({ ...s.args, driver });
    expect(conflicted).toBe(true);
    expect(result.admission.txHash).not.toBe(rejectedHash);
    expect(s.submissions.map((entry) => entry.phase)).toEqual([
      "Publication",
      "Admission",
    ]);
    const witness = await s.fetch();
    if (witness.kind !== "Present") throw new Error("Missing admitted event");
    expect(witness.anchor.node.next).toBe("ff".repeat(32));
    expect(witness.retainedDataUtxo?.txHash).toBe(result.publication?.txHash);
  },
  180_000,
);

it.each(["Publication", "Admission"] as const)(
  "reconciles an ambiguous %s after restart before building or submitting again",
  async (phase) => {
    const s = await setup("Deposit", true);
    const submit = s.args.driver.submit;
    const driver: SDK.EventHistorySubmissionDriver = {
      ...s.args.driver,
      submit: async (tx, attempt) => {
        const outcome = await submit(tx, attempt);
        if (attempt.phase === phase)
          throw new Error("Transport response lost after ledger confirmation");
        return outcome;
      },
    };
    await expect(
      SDK.submitEventHistory({ ...s.args, driver }),
    ).rejects.toBeInstanceOf(SDK.EventHistorySubmissionPendingError);
    const saved = s.checkpoint()!;
    expect(saved.pending?.phase).toBe(phase);
    const reconciled: SDK.EventHistorySubmissionAttempt[] = [];
    const result = await SDK.submitEventHistory({
      ...s.args,
      checkpoint: saved,
      driver: {
        ...s.args.driver,
        reconcile: async (attempt) => {
          expect(attempt).toEqual(
            attempt.phase === phase ? saved.pending : saved.publicationAttempt,
          );
          expect(await s.h.lucid.awaitTx(attempt.txHash)).toBe(true);
          reconciled.push(attempt);
          return { kind: "Confirmed" };
        },
      },
    });
    expect(reconciled.map((attempt) => attempt.phase)).toEqual(
      phase === "Admission" ? ["Publication", "Admission"] : ["Publication"],
    );
    expect(result.pending).toBeUndefined();
    expect(s.submissions.map((entry) => entry.phase)).toEqual([
      "Publication",
      "Admission",
    ]);
  },
  180_000,
);

it("retains an exact publication across delayed provider visibility and restart", async () => {
  const s = await setup("Withdrawal", true);
  const utxosByOutRef = s.h.lucid.utxosByOutRef.bind(s.h.lucid);
  let hide = true;
  s.h.lucid.utxosByOutRef = async (refs) => {
    const publication = s.checkpoint()?.publication;
    if (
      hide &&
      publication !== undefined &&
      refs.some(
        (ref) =>
          ref.txHash === publication.txHash &&
          ref.outputIndex === publication.outputIndex,
      )
    )
      return [];
    return utxosByOutRef(refs);
  };
  await expect(SDK.submitEventHistory(s.args)).rejects.toThrow("not visible");
  const saved = s.checkpoint()!;
  expect(saved.pending).toBeUndefined();
  expect(saved.publication).toBeDefined();
  expect(s.submissions.map((entry) => entry.phase)).toEqual(["Publication"]);
  hide = false;
  await SDK.submitEventHistory({ ...s.args, checkpoint: saved });
  expect(s.submissions.map((entry) => entry.phase)).toEqual([
    "Publication",
    "Admission",
  ]);
}, 180_000);

it("refuses a checkpoint for changed funds or payload without broadcasting", async () => {
  const s = await setup("Deposit", true);
  const result = await SDK.submitEventHistory(s.args);
  const calls = s.submissions.length;
  await expect(
    SDK.submitEventHistory({
      ...s.args,
      checkpoint: result,
      request: {
        ...s.args.request,
        assets: { lovelace: 26_000_000n },
      },
    }),
  ).rejects.toThrow("different request or deployment");
  expect(s.submissions).toHaveLength(calls);
}, 180_000);

it("does not broadcast when durable checkpoint storage fails", async () => {
  const s = await setup("Deposit", true);
  let submitCalls = 0;
  await expect(
    SDK.submitEventHistory({
      ...s.args,
      driver: {
        ...s.args.driver,
        save: async () => {
          throw new Error("checkpoint storage unavailable");
        },
        submit: async (_tx: TxSignBuilder) => {
          submitCalls++;
          return { kind: "Confirmed" };
        },
      },
    }),
  ).rejects.toThrow("checkpoint storage unavailable");
  expect(submitCalls).toBe(0);
  expect(await s.h.lucid.utxosByOutRef([s.args.request.nonce])).toHaveLength(1);
}, 180_000);

it("keeps an unresolved restart pending without rebuilding or broadcasting", async () => {
  const s = await setup("Deposit", true);
  await expect(
    SDK.submitEventHistory({
      ...s.args,
      driver: {
        ...s.args.driver,
        submit: async () => ({ kind: "Pending" }),
      },
    }),
  ).rejects.toThrow("confirmation is unresolved");
  const saved = s.checkpoint()!;
  let fundingCalls = 0;
  await expect(
    SDK.submitEventHistory({
      ...s.args,
      checkpoint: saved,
      driver: {
        ...s.args.driver,
        reconcile: async () => ({ kind: "Pending" }),
        funding: async () => {
          fundingCalls++;
          return s.h.funding();
        },
      },
    }),
  ).rejects.toThrow("confirmation is unresolved");
  expect(fundingCalls).toBe(0);
  expect(s.submissions).toHaveLength(0);
  expect(s.checkpoint()).toEqual(saved);
}, 180_000);

it("revalidates a completed receipt and stops if its current L1 status is unresolved", async () => {
  const s = await setup("Deposit", false);
  const completed = await SDK.submitEventHistory(s.args);
  let reconciliations = 0;
  await expect(
    SDK.submitEventHistory({
      ...s.args,
      checkpoint: completed,
      driver: {
        ...s.args.driver,
        reconcile: async (attempt) => {
          expect(attempt).toEqual(completed.admission);
          reconciliations++;
          return { kind: "Pending" };
        },
      },
    }),
  ).rejects.toThrow("confirmation is unresolved");
  expect(reconciliations).toBe(1);
  expect(s.submissions).toHaveLength(1);
  expect(s.checkpoint()?.admission).toBeUndefined();
  expect(s.checkpoint()?.pending?.txHash).toBe(completed.admission.txHash);
}, 180_000);

it.each(["Deposit", "Withdrawal"] as const)(
  "retains the %s publication body and reconciles it before a completed admission on restart",
  async (kind) => {
    const s = await setup(kind, true);
    const completed = await SDK.submitEventHistory(s.args);
    expect(completed.publicationAttempt).toEqual(s.submissions[0]);
    const calls: string[] = [];
    await expect(
      SDK.submitEventHistory({
        ...s.args,
        checkpoint: completed,
        driver: {
          ...s.args.driver,
          reconcile: async (attempt) => {
            calls.push(attempt.phase);
            expect(attempt.transactionCbor).toBe(
              s.submissions[0]!.transactionCbor,
            );
            return { kind: "Pending" };
          },
        },
      }),
    ).rejects.toThrow("publication confirmation is unresolved");
    expect(calls).toEqual(["Publication"]);
    expect(s.submissions).toHaveLength(2);
    expect(s.checkpoint()).toEqual(completed);
    const resumed = await SDK.submitEventHistory({
      ...s.args,
      checkpoint: completed,
    });
    expect(resumed).toEqual(completed);
    expect(s.submissions).toHaveLength(2);
  },
  180_000,
);
