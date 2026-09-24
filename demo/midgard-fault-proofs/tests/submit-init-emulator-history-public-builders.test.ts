import { createHash } from "node:crypto";
import { mkdirSync, readFileSync, writeFileSync } from "node:fs";
import { join } from "node:path";

import * as SDK from "@al-ft/midgard-sdk";
import { CML, Data, type UTxO } from "@lucid-evolution/lucid";
import { Effect } from "effect";
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
    join(directory, "history-public-builders.json"),
    JSON.stringify(
      {
        scope:
          "Public deposit/withdrawal config preparation and unsigned builders; actual applied lists and retention, native fixture hub",
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

const publicContracts = (
  h: Awaited<ReturnType<typeof setupHistoryPair>>,
): SDK.UserHistoryContracts => {
  const histories = h.applied.map(
    (a, i): SDK.EventHistoryContracts => ({
      recipe: h.recipes[i]!,
      list: {
        ...SDK.makeAuthenticatedValidator(
          "Custom",
          a.validator.script,
          a.validator.script,
        ),
        ...SDK.makeWithdrawalValidator(a.validator.script),
      },
      retention: SDK.makeSpendingValidator(
        "Custom",
        a.retention.validator.script,
      ),
      retirement: SDK.makeWithdrawalValidator(a.retirement.validator.script),
    }),
  );
  return {
    eventHistory: { deposit: histories[0]!, withdrawal: histories[1]! },
    deposit: histories[0]!.list,
    withdrawal: histories[1]!.list,
    hubOracle: {
      policyId: h.hubPolicy,
      mintingScript: h.issuer,
      mintingScriptCBOR: h.issuer.script,
      spendingScript: h.issuer,
      spendingScriptCBOR: h.issuer.script,
      spendingScriptHash: h.hubPolicy,
      spendingScriptAddress: h.hubAddress,
    },
  };
};

it.each([
  { kind: "Deposit" as const, external: false, reference: true },
  { kind: "Deposit" as const, external: true, reference: true },
  { kind: "Withdrawal" as const, external: false, reference: true },
  { kind: "Withdrawal" as const, external: true, reference: true },
  { kind: "Deposit" as const, external: false, reference: false },
  { kind: "Withdrawal" as const, external: false, reference: false },
])(
  "builds public $kind external=$external reference=$reference",
  async ({ kind, external, reference }) => {
    const h = await setupHistoryPair({ blueprint, records });
    h.emulator.awaitSlot(100);
    const contracts = publicContracts(h);
    const index = kind === "Deposit" ? 0 : 1;
    const nonce = h.eventNonces[index]!;
    const validity = {
      validFrom: h.emulator.now() - 60_000,
      validTo: h.emulator.now() + 60_000,
    };
    const withdrawal = historyPairPayloads(h)[1]!;
    if (!("WithdrawalPayload" in withdrawal))
      throw new Error("Expected withdrawal");
    const depositConfig: SDK.SubmitDepositConfig = {
      l2Address: h.wallet.address,
      l2Datum: external ? Data.to("ab".repeat(600)) : null,
      lovelace: 20_000_000n,
      additionalAssets: {},
      nonceInput: nonce,
      validity,
      referenceScripts: reference
        ? { depositMinting: h.scripts[0]! }
        : undefined,
    };
    const withdrawalConfig: SDK.SubmitWithdrawalConfig = {
      body: withdrawal.WithdrawalPayload.event.info.body,
      signature: withdrawal.WithdrawalPayload.event.info.signature,
      refundAddress: withdrawal.WithdrawalPayload.refund_address,
      refundDatum: external
        ? { InlineDatum: { data: "ab".repeat(600) } }
        : "NoDatum",
      nonceInput: nonce,
      validity,
      referenceScripts: reference
        ? { withdrawalMinting: h.scripts[1]! }
        : undefined,
    };
    const prepared =
      kind === "Deposit"
        ? await Effect.runPromise(
            SDK.prepareDepositSubmissionProgram(
              h.lucid,
              contracts,
              depositConfig,
            ),
          )
        : await Effect.runPromise(
            SDK.prepareWithdrawalSubmissionProgram(
              h.lucid,
              contracts,
              withdrawalConfig,
            ),
          );
    const payload = Data.from(
      prepared.request.payloadCbor,
      SDK.EventHistoryPayload,
    );
    expect(prepared.plan.kind).toBe(external ? "External" : "Inline");
    let externalData: UTxO | undefined;
    if (external) {
      const publication = await SDK.buildEventHistoryPublication(
        prepared.context,
        prepared.request.payloadCbor,
        prepared.request.reclaimAuth,
      );
      const hash = await h.submit(`${kind}-public-publication`, publication.tx);
      [externalData] = await h.lucid.utxosByOutRef([
        { txHash: hash, outputIndex: publication.publicationOutputIndex },
      ]);
      expect(await h.lucid.utxosByOutRef([nonce])).toHaveLength(1);
    }
    const built =
      kind === "Deposit"
        ? await Effect.runPromise(
            SDK.buildUnsignedDepositTxWithMetadataProgram(h.lucid, contracts, {
              ...depositConfig,
              externalData,
            }),
          )
        : await Effect.runPromise(
            SDK.buildUnsignedWithdrawalTxWithMetadataProgram(
              h.lucid,
              contracts,
              { ...withdrawalConfig, externalData },
            ),
          );
    const cml = CML.Transaction.from_cbor_hex(built.tx.toCBOR());
    expect(cml.body().certs()?.len() ?? 0).toBe(0);
    expect(cml.body().withdrawals()?.len()).toBe(1);
    expect(built.metadata.nonceInput.txHash).toBe(nonce.txHash);
    const txHash = await h.submit(`${kind}-public-admission`, built.tx);
    const witness = await SDK.fetchEventHistoryWitness(
      h.lucid,
      {
        policyId: h.applied[index]!.policyId,
        address: h.applied[index]!.address,
        retentionAddress: h.applied[index]!.retention.address,
        inlineLimitBytes: h.recipes[index]!.inlineLimitBytes,
      },
      { transactionId: nonce.txHash, outputIndex: BigInt(nonce.outputIndex) },
    );
    if (witness.kind !== "Present")
      throw new Error("Missing admitted public event");
    expect(witness.anchor.utxo.txHash).toBe(txHash);
    expect(witness.anchor.utxo.outputIndex).toBe(
      built.metadata.orderOutputIndex,
    );
    expect(witness.payload).toEqual(payload);
    expect(witness.retainedDataUtxo !== undefined).toBe(external);
    const captured = SDK.captureEventHistoryWitness(
      witness,
      h.applied[index]!.policyId,
      kind,
    );
    expect(captured.originalAssets.get("")?.get("")).toBe(
      kind === "Deposit" ? 20_000_000n : prepared.request.assets.lovelace,
    );
    const deployment = SDK.eventHistoryDeploymentFromContracts(
      kind === "Deposit"
        ? contracts.eventHistory!.deposit
        : contracts.eventHistory!.withdrawal,
    );
    const read = () =>
      kind === "Deposit"
        ? Effect.runPromise(SDK.fetchDepositUTxOsProgram(h.lucid, deployment))
        : Effect.runPromise(
            SDK.fetchWithdrawalUTxOsProgram(h.lucid, deployment),
          );
    const before = await read();
    expect(before).toHaveLength(1);
    expect(before[0]!.event).toEqual(
      "DepositPayload" in payload
        ? payload.DepositPayload.event
        : payload.WithdrawalPayload.event,
    );
    expect(before[0]!.originalAssets.lovelace).toBe(
      captured.originalAssets.get("")?.get(""),
    );
    expect(before[0]!.utxo.txHash).toBe(txHash);
    // A real successor insertion consumes the Order and changes only its
    // location/link/protection. It must remain the same single user event.
    h.emulator.awaitSlot(100);
    const continuation = await insertHistoryFillerAfter(
      h,
      kind,
      witness,
      "ff".repeat(32),
      [],
    );
    const continuationHash = await h.submit(
      `${kind}-public-reader-pointer-continuation`,
      continuation,
    );
    expect(await h.lucid.utxosByOutRef([witness.anchor.utxo])).toHaveLength(0);
    const after = await read();
    expect(after).toHaveLength(1);
    expect(after[0]!.utxo.txHash).toBe(continuationHash);
    expect(after[0]!.event).toEqual(before[0]!.event);
    expect(after[0]!.facts).toEqual(before[0]!.facts);
    expect(after[0]!.idCbor).toEqual(before[0]!.idCbor);
    expect(after[0]!.infoCbor).toEqual(before[0]!.infoCbor);
    expect(after[0]!.originalAssets).toEqual(before[0]!.originalAssets);
    expect(after[0]!.history.retainedDataUtxo).toEqual(
      before[0]!.history.retainedDataUtxo,
    );
  },
  180_000,
);

it("quotes structural ADA without adding it to the requested deposit Value", async () => {
  const h = await setupHistoryPair({ blueprint, records });
  h.emulator.awaitSlot(100);
  const config: SDK.SubmitDepositConfig = {
    l2Address: h.wallet.address,
    l2Datum: Data.to("ab".repeat(300)),
    lovelace: 1_500_000n,
    additionalAssets: {},
    nonceInput: h.eventNonces[0]!,
    referenceScripts: { depositMinting: h.scripts[0]! },
    validity: {
      validFrom: h.emulator.now() - 60_000,
      validTo: h.emulator.now() + 60_000,
    },
  };
  const prepared = await Effect.runPromise(
    SDK.prepareDepositSubmissionProgram(h.lucid, publicContracts(h), config),
  );
  expect(prepared.request.structuralLovelace).toBeGreaterThan(0n);
  const built = await Effect.runPromise(
    SDK.buildUnsignedDepositTxWithMetadataProgram(
      h.lucid,
      publicContracts(h),
      config,
    ),
  );
  await h.submit("public-deposit-automatic-structural-funding", built.tx);
  expect(built.metadata.structuralLovelace).toBe(
    prepared.request.structuralLovelace,
  );
  expect(
    prepared.request.assets.lovelace - prepared.request.structuralLovelace,
  ).toBe(config.lovelace);
  const deposits = await Effect.runPromise(
    SDK.fetchDepositUTxOsProgram(
      h.lucid,
      SDK.eventHistoryDeploymentFromContracts(
        publicContracts(h).eventHistory!.deposit,
      ),
    ),
  );
  expect(deposits).toHaveLength(1);
  expect(deposits[0]!.originalAssets).toEqual({ lovelace: config.lovelace });
  expect(deposits[0]!.utxo.assets.lovelace).toBe(
    config.lovelace + built.metadata.structuralLovelace,
  );
}, 180_000);

it("refuses external admission before actual publication and preserves the nonce", async () => {
  const h = await setupHistoryPair({ blueprint, records });
  const nonce = h.eventNonces[0]!;
  const config: SDK.SubmitDepositConfig = {
    l2Address: h.wallet.address,
    l2Datum: Data.to("ab".repeat(600)),
    lovelace: 20_000_000n,
    additionalAssets: {},
    nonceInput: nonce,
    validity: {
      validFrom: h.emulator.now() - 10_000,
      validTo: h.emulator.now() + 60_000,
    },
    referenceScripts: { depositMinting: h.scripts[0]! },
  };
  await expect(
    Effect.runPromise(
      SDK.buildUnsignedDepositTxWithMetadataProgram(
        h.lucid,
        publicContracts(h),
        config,
      ),
    ),
  ).rejects.toThrow("requires confirmed prepublished data");
  expect(await h.lucid.utxosByOutRef([nonce])).toHaveLength(1);
}, 180_000);

it.each(["Deposit", "Withdrawal"] as const)(
  "runs automatic external %s from the public configuration",
  async (kind) => {
    const h = await setupHistoryPair({ blueprint, records });
    const contracts = publicContracts(h);
    const withdrawal = historyPairPayloads(h)[1]!;
    if (!("WithdrawalPayload" in withdrawal))
      throw new Error("Expected withdrawal");
    const prepared =
      kind === "Deposit"
        ? await Effect.runPromise(
            SDK.prepareDepositSubmissionProgram(h.lucid, contracts, {
              l2Address: h.wallet.address,
              l2Datum: Data.to("ab".repeat(600)),
              lovelace: 20_000_000n,
              additionalAssets: {},
              nonceInput: h.eventNonces[0]!,
              referenceScripts: { depositMinting: h.scripts[0]! },
            }),
          )
        : await Effect.runPromise(
            SDK.prepareWithdrawalSubmissionProgram(h.lucid, contracts, {
              body: withdrawal.WithdrawalPayload.event.info.body,
              signature: withdrawal.WithdrawalPayload.event.info.signature,
              refundAddress: withdrawal.WithdrawalPayload.refund_address,
              refundDatum: { InlineDatum: { data: "ab".repeat(600) } },
              nonceInput: h.eventNonces[1]!,
              referenceScripts: { withdrawalMinting: h.scripts[1]! },
            }),
          );
    let saved: SDK.EventHistorySubmissionCheckpoint | undefined;
    const phases: string[] = [];
    const result = await SDK.submitEventHistory({
      context: prepared.context,
      request: prepared.request,
      driver: {
        save: async (checkpoint) => {
          saved = checkpoint;
        },
        submit: async (tx, attempt) => {
          expect(saved?.pending).toEqual(attempt);
          const hash = await h.submit(`${kind}-prepared-${attempt.phase}`, tx);
          expect(hash).toBe(attempt.txHash);
          phases.push(attempt.phase);
          return { kind: "Confirmed" };
        },
        reconcile: async () => {
          throw new Error("Unexpected pending transaction");
        },
        funding: h.funding,
        now: () => h.emulator.now(),
        waitUntil: async (time) => {
          h.emulator.awaitSlot(
            Math.max(1, Math.ceil((time - h.emulator.now()) / 1000)),
          );
        },
      },
      maxAttempts: 5,
      deadlineMs: h.emulator.now() + 600_000,
      validityDurationMs: 120_000,
      outputVisibilityAttempts: 3,
      retryDelayMs: 1000,
    });
    expect(phases).toEqual(["Publication", "Admission"]);
    expect(result.pending).toBeUndefined();
    expect(await h.lucid.utxosByOutRef([prepared.request.nonce])).toHaveLength(
      0,
    );
    expect(await h.lucid.utxosByOutRef([result.admission])).toHaveLength(1);
  },
  180_000,
);

it("refuses a public deposit that cannot fund future pointer changes with the requested structural ADA", async () => {
  const h = await setupHistoryPair({ blueprint, records });
  await expect(
    Effect.runPromise(
      SDK.prepareDepositSubmissionProgram(h.lucid, publicContracts(h), {
        l2Address: h.wallet.address,
        l2Datum: Data.to("ab".repeat(300)),
        lovelace: 1_500_000n,
        additionalAssets: {},
        structuralLovelace: 0n,
        nonceInput: h.eventNonces[0]!,
      }),
    ),
  ).rejects.toThrow("future pointer changes");
  expect(await h.lucid.utxosByOutRef([h.eventNonces[0]!])).toHaveLength(1);
}, 180_000);
